module Test.URI.Authority where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Type.Proxy (Proxy(..))
import Data.These (These(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Authority (Authority(..), AuthorityOptions, Host(..), Port, UserInfo)
import URI.Authority as Authority
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.UserInfo as UserInfo

spec :: Spec Unit
spec =
  describe "Authority parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost"
      ( Authority
          Nothing
          (Just (This (NameAddress (RegName.unsafeFromString (nes (Proxy :: Proxy "localhost"))))))
      )

    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost:3000"
      ( Authority
          Nothing
          (Just (Both (NameAddress (RegName.unsafeFromString (nes (Proxy :: Proxy "localhost")))) (Port.unsafeFromInt 3000)))
      )

    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@localhost:3000"
      ( Authority
          (Just (UserInfo.unsafeFromString (nes (Proxy :: Proxy "user"))))
          (Just (Both (NameAddress (RegName.unsafeFromString (nes (Proxy :: Proxy "localhost")))) (Port.unsafeFromInt 3000)))
      )

    testIso
      (Authority.parser options)
      (Authority.print options)
      "//:8000"
      (Authority Nothing (Just (That (Port.unsafeFromInt 8000))))

options :: Record (AuthorityOptions UserInfo (HostPortPair Host Port))
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  }
