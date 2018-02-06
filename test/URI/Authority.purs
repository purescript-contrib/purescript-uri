module Test.URI.Authority where

import Prelude

import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Data.URI.Authority (Authority(..), AuthorityOptions, Host(..), Port, UserInfo)
import Data.URI.Authority as Authority
import Data.URI.Host.RegName as RegName
import Data.URI.HostPortPair (HostPortPair)
import Data.URI.HostPortPair as HostPortPair
import Data.URI.Port as Port
import Data.URI.UserInfo as UserInfo
import Test.Spec (Spec, describe)
import Test.Util (testIso)

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "Authority parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost"
      (Authority
        Nothing
        (Just (This (NameAddress (RegName.unsafeFromString "localhost")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost:3000"
      (Authority
        Nothing
        (Just (Both (NameAddress (RegName.unsafeFromString "localhost")) (Port.unsafeFromInt 3000))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@localhost:3000"
      (Authority
        (Just (UserInfo.unsafeFromString "user"))
        (Just (Both (NameAddress (RegName.unsafeFromString "localhost")) (Port.unsafeFromInt 3000))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//:8000"
      (Authority Nothing (Just (That (Port.unsafeFromInt 8000))))

options ∷ Record (AuthorityOptions UserInfo (HostPortPair Host Port))
options =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  }