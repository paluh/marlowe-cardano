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
import           Data.ByteString             (ByteString)
import           Data.Either                 (rights)
import           Data.List                   (sort)
import           Data.Text                   (Text)
import           Ledger                      (TxId (..))
import           Plutus.ChainIndex.Tx        (ChainIndexTx, citxTxId)
import           Plutus.Contract             (Contract, EmptySchema, logDebug, logError, logInfo, mapError)
import           Plutus.Contract.Extra       (continuingHistoryAt)
import           Plutus.Contract.Request     (txsAt)
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


    logInfo @String "Started test for `continuingHistoryAt` . . ."
    citxs <- continuingHistoryAt address
    -- The expected results were determined by db-sync queries.
    passed <-
      checkResult citxs
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

    logInfo @String "Started test for `txsAt` . . ."
    citxs' <- txsAt address
    -- The expected results were determined by db-sync queries.
    passed' <-
      checkResult citxs'
        [
          "058e602d89a0c4d84d395726a6cea7f97c29fa066ea42c5ff37651160dec6369"
        , "1101256fa295830c74891a143c07593aaff3e69245e935ea8113e857d55dbddb"
        , "110b341636b48a495b2f66c9db3e4af39429fd48b042b68ff130b11bbaa0df7e"
        , "1bf24234bb6309b3c1c6e50722a398a062c6f655a35d6c9d3dc12d9db2be7195"
        , "2029cdb5f7fb8414452c189ed2c6092cbe374e81e9cb502a5b0f3edf6df79457"
        , "26ddf432668a049fd2c991902b61fccfb16e4999cfdcb202e63e8e2238eb7f49"
        , "3e1ae5f2d705094f2a00566f023d3ac70091271ffe9c7c826a864b2fe6e61693"
        , "3e51354ed13c23f1d7117de8e21c6a2790f4c577ee491991d4aac06efb83277d"
        , "3ed9cbe11b6308c5ede3ca8c9eb3a7ba1d7fe00a958dceb029f6c6219180235f"
        , "42ab9ed95781a8149abca655e16f09a5fec2bc9616e26dae144f451df662dc41"
        , "440fd38dc39fd8e22f605864189c278f3b0f712d0403b631b73aa0b382079d2a"
        , "50d4f06db0a521ea41a4dad4b146fa7fe18062048564c5d34e3c390e3a21cfc0"
        , "55a688ad74e60170619ee7dbfca759bb305ae6e85a280d30c5f057b6b11ff4b6"
        , "5b9e2b9b0b801323abf836068ecf4e6c22e5002362c4c5913f0d7f9888db39cd"
        , "5cedaeb1748f796a751deefa36a1f19067ad30ec71b1bc1ea9fc330f41ed640d"
        , "601a163e2e9e0ce3ad87005ad2a8070408dc01bb7ebe01cd67cbb342c3b55578"
        , "8191b74f3e52cfb8ead6017eb67d9c723acca3a8e18b075f91985827faf2dfba"
        , "8536c207443a00bf634a6d66eaf1263387228e80c8edd3e3dcf492f44819e010"
        , "8bbec91e485225715fbf0fa8889d5474c42e7b84478161ac6ba0eaec3d421bf3"
        , "8e89a695fd4da8f1b446b8ef6cbf855b009665021566c09f299bcf0f877c16d5"
        , "94c8238c58452ca9ae0f2f5e35af4758e00d3cd02bd4b43eb540295d806aa646"
        , "98a854c6af9d2bf83826238d130e067145e18ea433d47776c51792c0d6a98aeb"
        , "996f640a8a643583fe083905193f122e300e6cf789c5d3285b773c0d23b1f201"
        , "aad4b80f5fe2919a88b95c2e5764abfd134299e387ee518bcb8861dbb07f4bb8"
        , "b5abf9127ecd036c46750571d8941bb8540c11e447162913f90996916ac1a358"
        , "bae203ebba80e3096c39fe2a031fe4ecedd739d960ba0aeedc647565aa248ae4"
        , "bb77cb2a644ef3186096d9f9104413c25e0efe112beb974e3936028502e3312d"
        , "be74a5ca9c665d9aec37f5038840847ed19bb1074809cf4ac7c825715b42c559"
        , "c4dda70edc083eadc86c88c24348a7371aa6be1c0dfaff8762b9d114171e0685"
        , "cd8ad4fb8490bc1de2e19d6ab8e7e8f3c4fac17e8afca37c79e3b4cb732109c3"
        , "dd20325a92a06132cd8359a59939909d0ffeb1d55c826fb13dc717c2b7a4772f"
        , "f427faa2e84f847badb5530445c7a0dc8d680fe641ebc6503a089e039352be14"
        , "f5aaeacece57576a6b3b14d0d78bed8ab4b384094c1fdac910f6599f7c5c65a3"
        , "fc6abdf609cf1e86f72d557ddce90e617ed8542649eca0fcde181f6d7261a412"
        ]

    pure $ passed && passed'


-- | Check actual vs expected results.
checkResult :: [ChainIndexTx]       -- ^ The actual results.
            -> [ByteString]         -- ^ The expected results.
            -> Contract w s e Bool  -- ^ Action for whether the results match.
checkResult actual expected =
  do
    let
      expected' =
        rights
          $ fmap (TxId . toBuiltin) . Base16.decode
          <$> sort expected
    forM_ actual $ logDebug . ("  " <>) . show . (^. citxTxId)
    if sort (fmap (^. citxTxId) actual) == expected'
      then logInfo  @String "  . . . passed." >> pure True
      else logError @String "  . . . failed." >> pure False

