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


{-# LANGUAGE TypeApplications #-}


module Main (
-- * Entry point
  main
) where


import           Plutus.PAB.Effects.Contract.Builtin (handleBuiltin)
import           Plutus.PAB.Run                      (runWith)
import           TestChainIndex                      (TestContracts)


-- | Run the tests.
main :: IO ()
main = runWith $ handleBuiltin @TestContracts
