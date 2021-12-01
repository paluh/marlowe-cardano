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


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeApplications   #-}


module TestChainIndex(
    TestContracts(..)
    , handlers
    ) where


import           Control.Monad.Freer                 (interpret)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Default                        (Default (def))
import           Data.Row                            (Empty)
import           GHC.Generics                        (Generic)
import           Language.PureScript.Bridge          (argonaut, equal, genericShow, mkSumType)
import           Ledger                              (TxId)
import           Playground.Types                    (FunctionSchema)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..),
                                                      SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, mkSimulatorHandlers)
import           Prettyprinter                       (Pretty (pretty), viaShow)
import           Schema                              (FormSchema)
import           TestChainIndex.History              (historyContract)

import qualified Data.OpenApi.Schema                 as OpenApi (ToSchema)


data TestContracts =
    WaitForTx TxId -- FIXME: Why do API calls fail w/o this?
  | HistoryContract
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty TestContracts where
    pretty = viaShow

instance HasPSTypes TestContracts where
    psTypes = [equal . genericShow . argonaut $ mkSumType @TestContracts]

instance HasDefinitions TestContracts where
    getDefinitions = [HistoryContract]
    getContract = getTestContracts
    getSchema = getTestContractsSchema


getTestContractsSchema :: TestContracts
                       -> [FunctionSchema FormSchema]
getTestContractsSchema =
  \case
    HistoryContract{} -> endpointsToSchemas @Empty
    _                 -> undefined


getTestContracts :: TestContracts
                 -> SomeBuiltin
getTestContracts =
  \case
    HistoryContract -> SomeBuiltin historyContract
    _               -> undefined


handlers :: SimulatorEffectHandlers (Builtin TestContracts)
handlers =
  mkSimulatorHandlers def def
    $ interpret
    $ contractHandler handleBuiltin
