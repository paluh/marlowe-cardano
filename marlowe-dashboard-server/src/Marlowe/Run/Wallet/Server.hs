{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Wallet.Server
 ( handlers
 )
 where

import Cardano.Prelude hiding (Handler)
import qualified Cardano.Wallet.Api.Client as WBE.Api
import qualified Cardano.Wallet.Api.Types as WBE
import Cardano.Wallet.Primitive.SyncProgress (SyncProgress (..))
import Cardano.Wallet.Primitive.Types.Hash (getHash)
import Cardano.Wallet.Primitive.Types.TokenMap (AssetId (..), TokenMap, toFlatList)
import Cardano.Wallet.Primitive.Types.TokenPolicy (unTokenName, unTokenPolicyId)
import Cardano.Wallet.Primitive.Types.TokenQuantity (TokenQuantity (..))
import Data.Quantity (getPercentage, getQuantity)
import Data.Text.Class (FromText (fromText))
import GHC.Natural (naturalToInteger)
import Marlowe.Run.Types (Env, valueToDto)
import Marlowe.Run.Wallet.API (API, GetTotalFundsResponse (..))
import qualified Marlowe.Run.Wallet.CentralizedTestnet.Server as CentralizedTestnet
import Marlowe.Run.Wallet.Client (callWBE)
import qualified Plutus.V1.Ledger.Ada as Ledger
import qualified Plutus.V1.Ledger.Value as Ledger
import Servant (ServerError, ServerT, err400, err404, (:<|>) ((:<|>)))

handlers ::
    MonadIO m =>
    MonadReader Env m =>
    MonadError ServerError m =>
    ServerT API m
handlers = getTotalFunds :<|> CentralizedTestnet.handlers

-- The Wallet BackEnd uses TokenMap while Marlowe Run uses the Ledger MultiAsset Value
wbeTokenMapToLedgerValue ::
    TokenMap -> Ledger.Value
wbeTokenMapToLedgerValue tokenMap =
    let
        convertTokenName = Ledger.tokenName . unTokenName
        convertCurrencySymbol = Ledger.currencySymbol . getHash . unTokenPolicyId
    in
        mconcat $ fmap
            (\(AssetId wbePolicyId wbeTokenName, TokenQuantity quantity) ->
                Ledger.singleton
                    (convertCurrencySymbol wbePolicyId)
                    (convertTokenName wbeTokenName)
                    (naturalToInteger quantity)
            )
            (toFlatList tokenMap)

getTotalFunds ::
    MonadIO m =>
    MonadError ServerError m =>
    MonadReader Env m =>
    Text ->
    m GetTotalFundsResponse
getTotalFunds walletIdText = do
    walletId <- either (const $ throwError err400) pure $ fromText walletIdText
    result <- callWBE $ WBE.Api.getWallet WBE.Api.walletClient (WBE.ApiT walletId)
    case result of
        Left _ -> throwError err404
        Right WBE.ApiWallet{WBE.id = WBE.ApiT walletId, WBE.balance = balance, WBE.state = WBE.ApiT state, WBE.assets = assets} ->
            let
                WBE.ApiT tokenMap = WBE.total (assets :: WBE.ApiWalletAssetsBalance)

                assetsAsValues = wbeTokenMapToLedgerValue tokenMap

                adaValue = Ledger.lovelaceValueOf $ naturalToInteger $ getQuantity $ WBE.total (balance :: WBE.ApiWalletBalance)

                syncStatus = case state of
                    Ready     -> 1
                    Syncing q -> fromRational $ getPercentage $ getQuantity q
                    _         -> 0
            in
                pure $ GetTotalFundsResponse (valueToDto $ adaValue <> assetsAsValues) syncStatus
