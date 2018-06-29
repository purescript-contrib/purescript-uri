module Test.URI.UserInfo where

import Prelude

import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.UserInfo as UserInfo

spec ∷ Spec Unit
spec =
  describe "UserInfo parser/printer" do
    testIso UserInfo.parser UserInfo.print "user" (UserInfo.fromString (nes (SProxy :: SProxy "user")))
    testIso UserInfo.parser UserInfo.print "spaced%20user" (UserInfo.fromString (nes (SProxy :: SProxy "spaced user")))
    testIso UserInfo.parser UserInfo.print "user:password" (UserInfo.fromString (nes (SProxy :: SProxy "user:password")))
    testIso UserInfo.parser UserInfo.print "spaced%20user:password%25%C2%A3" (UserInfo.fromString (nes (SProxy :: SProxy "spaced user:password%£")))
    testIso UserInfo.parser UserInfo.print "a:b:c" (UserInfo.fromString (nes (SProxy :: SProxy "a:b:c")))
