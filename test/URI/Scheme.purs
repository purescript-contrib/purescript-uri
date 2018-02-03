module Test.URI.Scheme where

import Prelude

import Data.URI.Scheme (Scheme(..))
import Data.URI.Scheme as Scheme
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "Scheme parser/printer" do
    testIso Scheme.parser Scheme.print "http:" (Scheme "http")
    testIso Scheme.parser Scheme.print "git+ssh:" (Scheme "git+ssh")
