module Test.URI.Port where

import Prelude

import Data.URI.Port (Port(..))
import Data.URI.Port as Port
import Test.Spec (Spec, describe)
import Test.Util (testIso)

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "Port parser/printer" do
    testIso (Port.parser pure) (Port.print id) "0" (Port 0)
    testIso (Port.parser pure) (Port.print id) "1234" (Port 1234)
    testIso (Port.parser pure) (Port.print id) "63174" (Port 63174)
