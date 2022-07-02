module Test.Data.Address.Bech32.DataPart (spec) where

import Prologue

import Arbitrary (ArbT(..))
import Data.Address.Bech32.DataPart
  ( Bech32DataPart
  , fromString
  , isValid
  , toString
  )
import Data.Newtype (un)
import Effect.Class (liftEffect)
import Test.QuickCheck ((===))
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = do
  describe "Data.Address.Bech32.DataPart" do
    it "Satisfies Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbT Bech32DataPart))
    it "Satisfies Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbT Bech32DataPart))
    describe "Arbitrary instance" do
      it "generates valid data" do
        quickCheck $ isValid <<< un ArbT
    describe "String injectivity" do
      it "preserves round trips" do
        quickCheck \(ArbT hrp) -> fromString (toString hrp) === Just hrp