-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Extra Plutus contract functions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Plutus.Contract.Extra (
-- * Queries
  continuingHistoryAt
-- * Filtering
, FilterTxOut
, filterContinuingHistory
, filterTxOutRef
) where


import           Control.Lens            ((^.))
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import           Data.List               (nub)
import           Data.Maybe              (catMaybes)
import           Ledger                  (Address, TxIn (txInRef), TxOut (..), TxOutRef (..))
import           Plutus.ChainIndex       (ChainIndexTx, ChainIndexTxOutputs (InvalidTx, ValidTx), citxInputs,
                                          citxOutputs)
import           Plutus.Contract.Request (txFromTxId, utxosTxOutTxAt)
import           Plutus.Contract.Types   (AsContractError, Contract)


-- TODO: Consider replacing this reference implentation with a stack-based search.


-- | Find all transactions that are UTxOs at the address, or predecessors of those
--   transactions and also to the address.
--
--   This function does not find all transactions at the address. Instead it
--   only finds the continuous series of transactions to the address, ending
--   with at one of the UTxOs.
continuingHistoryAt :: forall w s e
                    .  AsContractError e
                    => Address                        -- ^ The address of interest.
                    -> Contract w s e [ChainIndexTx]  -- ^ The action for finding the transactions.
continuingHistoryAt address =
  do
    let
      p TxOut{..} = txOutAddress == address
    citxs <- fmap snd . toList <$> utxosTxOutTxAt address
    nub . concat <$> filterContinuingHistory p `mapM` citxs


-- | Test a property of a transaction output.
type FilterTxOut =  TxOut  -- ^ The transaction output of interest.
                 -> Bool   -- ^ Whether the transaction possess the property.


-- | Find all transactions that are precessors of a given transaction and that
--   satisfy a specified criterion.
filterContinuingHistory :: forall w s e
                        .  AsContractError e
                        => FilterTxOut                    -- ^ The filtering criterion.
                        -> ChainIndexTx                   -- ^ The starting transaction of the history.
                        -> Contract w s e [ChainIndexTx]  -- ^ Action for finding the transaction history.
filterContinuingHistory p citx =
  do
    let
      txOutRefs = fmap txInRef . toList $ citx ^. citxInputs
    citxs <- catMaybes <$> filterTxOutRef p `mapM` txOutRefs
    nub . (citx :) . concat <$> filterContinuingHistory p `mapM` citxs


-- | Return the transaction for a given transaction output, if that output
--   satisfies a specified criterion.
filterTxOutRef :: forall w s e
               .  AsContractError e
               => FilterTxOut                          -- ^ The filtering criterion.
               -> TxOutRef                             -- ^ The transaction output under consideration.
               -> Contract w s e (Maybe ChainIndexTx)  -- ^ Action for checking the satisfying transaction.
filterTxOutRef p TxOutRef{..} =
  do
    let
      validTx citx =
        case citx ^. citxOutputs of
          InvalidTx      -> Nothing
          ValidTx txOuts -> Just txOuts
    citx <- txFromTxId txOutRefId
    pure
      $ do
        txOuts <- validTx =<< citx
        let
          i = fromIntegral txOutRefIdx
          txOut = txOuts !! i
        guard
          $ i < length txOuts && p txOut
        citx
