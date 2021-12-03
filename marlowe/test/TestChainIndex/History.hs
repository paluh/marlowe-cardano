-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test extract functions for querying the chain index.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module TestChainIndex.History (
-- * Testing
  historyContract
) where


import           Control.Lens                ((^.))
import           Control.Monad               (forM_)
import           Control.Monad.Except        (liftEither)
import           Data.Either                 (rights)
import           Data.List                   (sort)
import           Data.Text                   (Text)
import           Ledger                      (TxId (..))
import           Plutus.ChainIndex.Tx        (citxTxId)
import           Plutus.Contract             (Contract, EmptySchema, logDebug, logError, logInfo, mapError)
import           Plutus.Contract.Extra       (continuingHistoryAt)
import           Plutus.V1.Ledger.Address    (Address (..))
import           Plutus.V1.Ledger.Api        (toBuiltin)
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))

import qualified Data.ByteString.Base16      as Base16 (decode)
import qualified Data.Text                   as T (pack)


-- | Test `Plutus.Contract.Extra.continuingHistoryAt` using eUTxOs on the public testnet.
--
-- Run this locally using an API call like the following:
--
-- > curl -H "Content-Type: application/json" \
-- >   -d "{\"caID\":{\"tag\":\"HistoryContract\"},\"caWallet\":{\"getWalletId\":\"$WALLET_ID\"}}" \
-- >   localhost:9980/api/contract/activate"
historyContract :: Contract () EmptySchema Text Bool
historyContract =
  do
    -- The UTxOs for the following validator are left undisturbed on the public testnet.
    hash <-
      mapError T.pack $ liftEither
        $ ValidatorHash . toBuiltin
        <$> Base16.decode "6d74caa38cd740215f27a9f365ea1e3b7144a1ec2d6563344c85bbf4"
    let
      address = Address (ScriptCredential hash) Nothing
      -- The expected results were determined by db-sync queries.
      expected =
        rights
          $ fmap (TxId . toBuiltin) . Base16.decode
          <$> sort
          [
            "1101256fa295830c74891a143c07593aaff3e69245e935ea8113e857d55dbddb"
          , "26ddf432668a049fd2c991902b61fccfb16e4999cfdcb202e63e8e2238eb7f49"
          , "3e1ae5f2d705094f2a00566f023d3ac70091271ffe9c7c826a864b2fe6e61693"
          , "440fd38dc39fd8e22f605864189c278f3b0f712d0403b631b73aa0b382079d2a"
          , "5cedaeb1748f796a751deefa36a1f19067ad30ec71b1bc1ea9fc330f41ed640d"
          , "8536c207443a00bf634a6d66eaf1263387228e80c8edd3e3dcf492f44819e010"
          , "8bbec91e485225715fbf0fa8889d5474c42e7b84478161ac6ba0eaec3d421bf3"
          , "94c8238c58452ca9ae0f2f5e35af4758e00d3cd02bd4b43eb540295d806aa646"
          , "98a854c6af9d2bf83826238d130e067145e18ea433d47776c51792c0d6a98aeb"
          , "aad4b80f5fe2919a88b95c2e5764abfd134299e387ee518bcb8861dbb07f4bb8"
          , "cd8ad4fb8490bc1de2e19d6ab8e7e8f3c4fac17e8afca37c79e3b4cb732109c3"
          , "f427faa2e84f847badb5530445c7a0dc8d680fe641ebc6503a089e039352be14"
          , "f5aaeacece57576a6b3b14d0d78bed8ab4b384094c1fdac910f6599f7c5c65a3"
          ]
    logInfo @String "Started test for `continuingHistoryAt` . . ."
    -- | Perform the query and check the results.
    citxs <- continuingHistoryAt address
    forM_ citxs $ logDebug . ("  " <>) . show . (^. citxTxId)
    if sort (fmap (^. citxTxId) citxs) == expected
      then logInfo  @String "  . . . passed." >> pure True
      else logError @String "  . . . failed." >> pure False
