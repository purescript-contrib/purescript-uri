module Test.URI.Authority where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Data.These (These(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Authority (Authority(..), AuthorityOptions, Host(..), Port, UserInfo)
import URI.Authority as Authority
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.UserInfo as UserInfo

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "Authority parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost"
      (Authority
        Nothing
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "localhost")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost:3000"
      (Authority
        Nothing
        (Just (Both (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "localhost")) (Port.unsafeFromInt 3000))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@localhost:3000"
      (Authority
        (Just (UserInfo.unsafeFromString "user"))
        (Just (Both (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "localhost")) (Port.unsafeFromInt 3000))))
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
