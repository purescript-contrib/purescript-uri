module Test.URI.Scheme where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Test.Spec (Spec, describe, it)
import Test.Util (testIso, equal)
import URI.Scheme as Scheme

spec ∷ Spec Unit
spec = do
  describe "Scheme parser/printer" do
    testIso Scheme.parser Scheme.print "http:" (Scheme.unsafeFromString "http")
    testIso Scheme.parser Scheme.print "git+ssh:" (Scheme.unsafeFromString "git+ssh")
  describe "Scheme fromString/toString" do
    it "http"
      let http = Scheme.unsafeFromString "http"
      in equal (Just http) $ Scheme.fromString $ NES.toString $ Scheme.toString http
    it "git+ssh"
      let git = Scheme.unsafeFromString "git+ssh"
      in equal (Just git) $ Scheme.fromString $ NES.toString $ Scheme.toString git
