module Test.URI.Path where

import Prelude

import Data.URI.Path (Path(..))
import Data.URI.Path as Path
import Data.URI.Path.Segment as PathSegment
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "Path parser/printer" do
    testIso (Path.parser pure) Path.print "/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23" (Path [PathSegment.segmentFromString "Пациенты# #"])
