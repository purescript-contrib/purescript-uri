module Test.URI.UserInfo where

import Prelude

import Data.URI.UserInfo as UserInfo
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "UserInfo parser/printer" do
    testIso (UserInfo.parser pure) (UserInfo.print id) "user" (UserInfo.fromString "user")
    testIso (UserInfo.parser pure) (UserInfo.print id) "spaced%20user" (UserInfo.fromString "spaced user")
    testIso (UserInfo.parser pure) (UserInfo.print id) "user:password" (UserInfo.fromString "user:password")
    testIso (UserInfo.parser pure) (UserInfo.print id) "spaced%20user:password%25%C2%A3" (UserInfo.fromString "spaced user:password%£")
    testIso (UserInfo.parser pure) (UserInfo.print id) "a:b:c" (UserInfo.fromString "a:b:c")
