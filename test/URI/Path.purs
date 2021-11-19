module Test.URI.Path where

import Prelude

import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Path (Path(..))
import URI.Path as Path
import URI.Path.Segment as PathSegment

spec :: Spec Unit
spec =
  describe "Path parser/printer" do
    testIso Path.parser Path.print "/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23" (Path [ PathSegment.segmentFromString "Пациенты# #" ])
