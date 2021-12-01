-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test use of chain-index.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module TestChainIndex.History (
  historyContract
) where


import           Control.Lens                ((^.))
import           Data.Text                   (Text)
import           Plutus.ChainIndex.Tx        (citxTxId)
import           Plutus.Contract             (Contract, EmptySchema, logInfo)
import           Plutus.Contract.Extra       (txHistoryAt)
import           Plutus.V1.Ledger.Address    (Address (..))
import           Plutus.V1.Ledger.Api        (toBuiltin)
import           Plutus.V1.Ledger.Credential (Credential (..), StakingCredential (..))
import           Plutus.V1.Ledger.Crypto     (PubKeyHash (..))

import qualified Data.ByteString.Base16      as Base16 (decode)


historyContract :: Contract () EmptySchema Text ()
historyContract =
  do
    let
      Right ownPubKey = PubKeyHash . toBuiltin <$> Base16.decode "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"
      Right ownStaKey = PubKeyHash . toBuiltin <$> Base16.decode "0cff2d99c693c4a7c787995a69262d796d8597a9b4b24e57d81cbe9f"
      address = Address (PubKeyCredential ownPubKey) (Just . StakingHash $ PubKeyCredential ownStaKey)
    logInfo @String "Started history contract."
    citxs <- txHistoryAt address
    sequence_
      [
        logInfo $ "  " <> show (citx ^. citxTxId)
      |
        citx <- citxs
      ]
    logInfo @String "Finished history contract."
