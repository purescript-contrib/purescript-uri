module Test.URI.UserInfo where

import Prelude

import Test.Spec (Spec, describe)
import Test.Util (nes, testIso)
import URI.UserInfo as UserInfo

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "UserInfo parser/printer" do
    testIso UserInfo.parser UserInfo.print "user" (UserInfo.fromString (nes "user"))
    testIso UserInfo.parser UserInfo.print "spaced%20user" (UserInfo.fromString (nes "spaced user"))
    testIso UserInfo.parser UserInfo.print "user:password" (UserInfo.fromString (nes "user:password"))
    testIso UserInfo.parser UserInfo.print "spaced%20user:password%25%C2%A3" (UserInfo.fromString (nes "spaced user:password%£"))
    testIso UserInfo.parser UserInfo.print "a:b:c" (UserInfo.fromString (nes "a:b:c"))
