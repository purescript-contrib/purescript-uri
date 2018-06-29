module Test.URI.Fragment where

import Prelude

import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Fragment as Fragment

spec ∷ Spec Unit
spec =
  describe "Fragment parser/printer" do
    testIso Fragment.parser Fragment.print "#" (Fragment.fromString "")
    testIso Fragment.parser Fragment.print "#foo" (Fragment.fromString "foo")
    testIso Fragment.parser Fragment.print "#foo%23bar" (Fragment.fromString "foo#bar")
    testIso Fragment.parser Fragment.print "#foo%23bar" (Fragment.unsafeFromString "foo%23bar")
