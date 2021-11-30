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


module Plutus.Contract.Extra (
-- * Queries
  txHistoryAt
, incomingTxs
-- * Filtering
, FilterChainIndexTx
, filterTxHistory
, anyOutputTo
) where


import           Control.Lens            ((^.))
import           Data.Foldable           (toList)
import           Data.List               (nub)
import           Data.Maybe              (catMaybes)
import           Ledger                  (Address, TxId, TxIn (txInRef), TxOut (txOutAddress), TxOutRef (txOutRefId))
import           Plutus.ChainIndex       (ChainIndexTx, ChainIndexTxOutputs (InvalidTx, ValidTx), citxInputs,
                                          citxOutputs)
import           Plutus.Contract.Request (txFromTxId, utxosTxOutTxAt)
import           Plutus.Contract.Types   (AsContractError, Contract)


-- FIXME: Use this as a reference implementation, but replace it with a stack-based one.


-- | Find all transactions that are UTxOs at the address, or predecessors
-- | transactions to the address.
-- |
-- | This function does not find all transactions at the address. Instead it
-- | only finds the continuous series of transactions to the address, ending
-- | with at one of the UTxOs.
txHistoryAt :: forall w s e
      .  AsContractError e
      => Address                        -- ^ The address of interest.
      -> Contract w s e [ChainIndexTx]  -- ^ The action for finding the transactions.
txHistoryAt address =
  do
    -- Find the UTxOs at the address.
    citxs <- fmap snd . toList <$> utxosTxOutTxAt address
    -- Filter the history for transactions that have output to the address.
    nub . concat <$> mapM (filterTxHistory $ anyOutputTo address) citxs


-- | Test whether a transaction has outputs to a given address.
anyOutputTo :: Address       -- ^ The address of interest.
            -> ChainIndexTx  -- ^ The transaction of interest.
            -> Bool          -- ^ Whether the transaction has output to the address.
anyOutputTo address citx =
  case citx ^. citxOutputs of
    ValidTx txOuts -> any ((== address) . txOutAddress) txOuts
    InvalidTx      -> False


-- | Test a property of a transaction.
type FilterChainIndexTx =  ChainIndexTx  -- ^ The transaction of interest.
                        -> Bool          -- ^ Whether the transaction possess the property.


-- | Filter transaction history based on a criterion.
-- |
-- | This function does not find all transactions satisfying the criterion.
-- | Instead it only finds the continuous series of transactions ending with
-- | the starting transaction.
filterTxHistory :: forall w s e
                .  AsContractError e
                => FilterChainIndexTx             -- ^ The filtering criterion.
                -> ChainIndexTx                   -- ^ The starting transaction of the history.
                -> Contract w s e [ChainIndexTx]  -- ^ Action for finding the transaction history.
filterTxHistory p citx =
  do
    citxs <- incomingTxs citx
    (citx :) . concat <$> mapM (filterTxHistory p) citxs


-- | Find the transactions with inputs to transaction of interest.
incomingTxs :: forall w s e
            .  AsContractError e
            => ChainIndexTx                   -- ^ The transaction of interest.
            -> Contract w s e [ChainIndexTx]  -- ^ Action returning the input transactions.
incomingTxs citx =
  let
    -- Find the outputs of transactions corresponding to the inputs to the
    -- transaction of interest.
    txOutRefs = fmap txInRef . toList $ citx ^. citxInputs :: [TxOutRef]
    -- Find the TxIds of those inputs.
    txIds = txOutRefId <$> txOutRefs :: [TxId]
  in
    -- Look up the whole transactions.
    catMaybes <$> mapM txFromTxId txIds
