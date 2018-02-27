module Test.URI.Extra.UserPassInfo where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Data.These (These(..))
import Data.URI.Authority (Authority(..), Host(..), Port)
import Data.URI.Authority as Authority
import Data.URI.Extra.UserPassInfo (UserPassInfo(..))
import Data.URI.Extra.UserPassInfo as UserPassInfo
import Data.URI.Host.RegName as RegName
import Data.URI.HostPortPair (HostPortPair)
import Data.URI.HostPortPair as HostPortPair
import Data.URI.URIRef (Fragment, HierPath, Path, Query, RelPath, URIRefOptions)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe)
import Test.Util (testIso)

spec ∷ ∀ eff. Spec eff Unit
spec = do
  describe "Authority+UserPassInfo parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@host"
      (Authority
        (Just (UserPassInfo { user: "user", password: Nothing }))
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "host")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user:pass@host"
      (Authority
        (Just (UserPassInfo { user: "user", password: Just "pass" }))
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "host")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user:pa%3Ass@host"
      (Authority
        (Just (UserPassInfo { user: "user", password: Just "pa:ss" }))
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "host")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//us%3Aer:pa%3Ass@host"
      (Authority
        (Just (UserPassInfo { user: "us:er", password: Just "pa:ss" }))
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "host")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//us%3Aer:pa%3Ass@host"
      (Authority
        (Just (UserPassInfo { user: "us:er", password: Just "pa:ss" }))
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "host")))))
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user:p%40ss@host"
      (Authority
        (Just (UserPassInfo { user: "user", password: Just "p@ss" }))
        (Just (This (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "host")))))

options ∷ Record (URIRefOptions UserPassInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: UserPassInfo.parse
  , printUserInfo: UserPassInfo.print
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseRelPath: pure
  , printRelPath: id
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }
