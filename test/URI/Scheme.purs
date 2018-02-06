module Test.URI.Scheme where

import Prelude

import Data.URI.Scheme as Scheme
import Test.Spec (Spec, describe)
import Test.Util (testIso)

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "Scheme parser/printer" do
    testIso Scheme.parser Scheme.print "http:" (Scheme.unsafeFromString "http")
    testIso Scheme.parser Scheme.print "git+ssh:" (Scheme.unsafeFromString "git+ssh")
