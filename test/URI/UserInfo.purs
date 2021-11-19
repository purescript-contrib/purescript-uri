module Test.URI.UserInfo where

import Prelude

import Data.String.NonEmpty (nes)
import Type.Proxy (Proxy(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.UserInfo as UserInfo

spec :: Spec Unit
spec =
  describe "UserInfo parser/printer" do
    testIso UserInfo.parser UserInfo.print "user" (UserInfo.fromString (nes (Proxy :: Proxy "user")))
    testIso UserInfo.parser UserInfo.print "spaced%20user" (UserInfo.fromString (nes (Proxy :: Proxy "spaced user")))
    testIso UserInfo.parser UserInfo.print "user:password" (UserInfo.fromString (nes (Proxy :: Proxy "user:password")))
    testIso UserInfo.parser UserInfo.print "spaced%20user:password%25%C2%A3" (UserInfo.fromString (nes (Proxy :: Proxy "spaced user:password%£")))
    testIso UserInfo.parser UserInfo.print "a:b:c" (UserInfo.fromString (nes (Proxy :: Proxy "a:b:c")))
