module Test.URI.Port where

import Prelude

import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Port as Port

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "Port parser/printer" do
    testIso (Port.parser pure) Port.print ":0" (Port.unsafeFromInt 0)
    testIso (Port.parser pure) Port.print ":1234" (Port.unsafeFromInt 1234)
    testIso (Port.parser pure) Port.print ":63174" (Port.unsafeFromInt 63174)
