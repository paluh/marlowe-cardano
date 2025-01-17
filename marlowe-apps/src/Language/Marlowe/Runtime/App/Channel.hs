{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Runtime.App.Channel
  ( LastSeen(..)
  , runContractAction
  , runDetection
  , runDiscovery
  ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Monad (join, unless, void)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as M (Map, adjust, delete, insert, lookup)
import qualified Data.Set as S (Set, insert, member)
import Data.Text (Text)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , EOF(EOF)
  , TChanEOF
  , contractFromStream
  , streamAllContractIds
  , streamContractSteps
  , transactionIdFromStream
  )
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector(..), DynamicField)
import Observe.Event.Explicit (Event, EventBackend, addField, withEvent)
import Observe.Event.Syntax ((≔))

runDiscovery
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> Int
  -> Bool
  -> IO (TChanEOF ContractId)
runDiscovery eventBackend config pollingFrequency endOnWait =
  do
    channel <- newTChanIO
    void . forkIO
      . withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DiscoveryProcess")
      $ \event ->
        addField event
          . either (("failure" :: Text) ≔) (const $ ("success" :: Text) ≔ True)
          =<< runClientWithConfig config (streamAllContractIds eventBackend pollingFrequency endOnWait channel)
    pure channel


runDetection
  :: (Either (CreateStep 'V1) (ContractStep 'V1) -> Bool)
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> Int
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection accept eventBackend config pollingFrequency inChannel =
  do
    outChannel <- newTChanIO
    let
      -- FIXME: If `MarloweSyncClient` were a `Monad`, then we could run
      --        multiple actions sequentially in a single connection.
      threadAction = join
        . withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DetectionProcess")
        $ \event ->
          do
            liftIO (atomically $ readTChan inChannel) >>= \case
              Left _ -> do
                liftIO . atomically $ writeTChan outChannel $ Left EOF
                pure $ pure ()
              Right contractId -> do
                addField event $ ("contractId" :: Text) ≔ contractId
                -- FIXME: If there were concurrency combinators for `MarloweSyncClient`, then we
                --        could follow multiple contracts in parallel using the same connection.
                let
                  finishOnClose = True
                  finishOnWait = True
                streamContractSteps eventBackend pollingFrequency finishOnClose finishOnWait accept contractId outChannel
                pure threadAction
    void $ forkIO $ runClientWithConfig config threadAction
    pure outChannel


data LastSeen =
  LastSeen
  {
    thisContractId :: ContractId
  , lastContract :: Contract
  , lastTxId :: TxId
  , ignoredTxIds :: S.Set TxId
  }
    deriving (Show)


runContractAction
  :: forall r
  .  Text
  -> EventBackend IO r DynamicEventSelector
  -> (Event IO r DynamicField -> LastSeen -> IO ())
  -> Int
  -> Bool
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runContractAction selectorName eventBackend runInput pollingFrequency endOnWait inChannel outChannel =
  let
    -- Nothing needs updating.
    rollback :: ContractStream 'V1 -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    rollback = const id
    -- Remove the contract from tracking.
    delete :: ContractId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    delete = M.delete
    -- Update the contract and its latest transaction.
    update :: Event IO r DynamicField -> ContractStream 'V1 -> M.Map ContractId LastSeen -> IO (M.Map ContractId LastSeen)
    update event cs lastSeen =
      let
        contractId = csContractId cs
      in
        case (contractId `M.lookup` lastSeen, contractFromStream cs, transactionIdFromStream cs) of
          (Nothing  , Just contract, Just txId) -> pure $ M.insert contractId (LastSeen contractId contract txId mempty) lastSeen
          (Just seen, Just contract, Just txId) -> pure $ M.insert contractId (seen {lastContract = contract, lastTxId = txId}) lastSeen
          (Just _   , Nothing      , Just _   ) -> pure $ M.delete contractId lastSeen
          (seen     , _            , _        ) -> do  -- FIXME: Diagnose and remedy situations if this ever occurs.
                                                     addField event
                                                       $ ("invalidContractStream" :: Text) ≔
                                                         object
                                                             [
                                                               "lastContract" .= fmap lastContract seen
                                                             , "lastTxId" .= fmap lastTxId seen
                                                             , "contractStream" .= cs
                                                             ]
                                                     pure lastSeen
    -- Ignore the transaction in the future.
    ignore :: ContractId -> TxId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    ignore contractId txId lastSeen =
      M.adjust (\seen -> seen {ignoredTxIds = txId `S.insert` ignoredTxIds seen}) contractId lastSeen
    -- Revisit a contract later.
    revisit :: ContractId -> IO ()
    revisit contractId
      | endOnWait = pure ()
      -- FIXME: This is a workaround for contract discovery not tailing past the tip of the blockchain.
      | otherwise = void . forkIO
        $ threadDelay pollingFrequency
        >> atomically (writeTChan outChannel $ Right contractId)
    go :: M.Map ContractId LastSeen -> IO ()
    go lastSeen =
      do
        lastSeen' <-
          withEvent eventBackend (DynamicEventSelector selectorName)
            $ \event -> runExceptT $
              do
                cs <- ExceptT . atomically $ readTChan inChannel
                liftIO . addField event $ ("contractId" :: Text) ≔ csContractId cs
                liftIO $ case cs of
                  ContractStreamStart{}      -> do
                                                  addField event $ ("action" :: Text) ≔ ("start" :: String)
                                                  update event cs lastSeen
                  ContractStreamContinued{}  -> do
                                                  addField event $ ("action" :: Text) ≔ ("continued" :: String)
                                                  update event cs lastSeen
                  ContractStreamRolledBack{} -> do
                                                  addField event $ ("action" :: Text) ≔ ("rollback" :: String)
                                                  pure $ rollback cs lastSeen
                  ContractStreamWait{..}     -> do
                                                  addField event $ ("action" :: Text) ≔ ("wait" :: String)
                                                  case csContractId `M.lookup` lastSeen of
                                                    Just seen@LastSeen{lastTxId} ->
                                                      do
                                                        unless (lastTxId `S.member` ignoredTxIds seen)
                                                          $ runInput event seen
                                                        revisit csContractId
                                                        pure $ ignore csContractId lastTxId lastSeen
                                                    _ ->
                                                      do  -- FIXME: Diagnose and remedy situations if this ever occurs.
                                                        addField event
                                                          $ ("invalidContractStream" :: Text) ≔
                                                            object ["contractStream" .= cs]
                                                        pure lastSeen
                  ContractStreamFinish{..}   -> do
                                                  addField event $ ("action" :: Text) ≔ ("finish" :: String)
                                                  pure $ delete csContractId lastSeen
        either (const $ pure ()) go lastSeen'
  in
    go mempty
