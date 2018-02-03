module Test.URI.Fragment where

import Prelude

import Data.URI.Fragment as Fragment
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "Fragment parser/printer" do
    testIso (Fragment.parser pure) (Fragment.print id) "#" (Fragment.fromString "")
    testIso (Fragment.parser pure) (Fragment.print id) "#foo" (Fragment.fromString "foo")
    testIso (Fragment.parser pure) (Fragment.print id) "#foo%23bar" (Fragment.fromString "foo#bar")
    testIso (Fragment.parser pure) (Fragment.print id) "#foo%23bar" (Fragment.unsafeFromString "foo%23bar")
