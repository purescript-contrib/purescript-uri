module Test.URI.Port where

import Prelude

import Data.Maybe (Maybe(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Util (TestEffects, forAll, testIso)
import URI.Port as Port
import URI.Port.Gen as Port.Gen

spec ∷ ∀ eff. Spec (TestEffects eff) Unit
spec =
  describe "Port parser/printer" do
    testIso Port.parser Port.print ":0" (Port.unsafeFromInt 0)
    testIso Port.parser Port.print ":1234" (Port.unsafeFromInt 1234)
    testIso Port.parser Port.print ":63174" (Port.unsafeFromInt 63174)

    it "should uphold toString / fromString property" do
      forAll do
        port ← Port.Gen.genPort
        pure $ Port.fromInt (Port.toInt port) === Just port
