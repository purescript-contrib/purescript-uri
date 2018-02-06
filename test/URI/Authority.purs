module Test.URI.Authority where

import Prelude

import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Data.URI.Authority (Authority(..), AuthorityOptions, Host(..), Port(..), UserInfo)
import Data.URI.Authority as Authority
import Data.URI.Host.RegName as RegName
import Data.URI.HostPortPair (HostPortPair)
import Data.URI.HostPortPair as HostPortPair
import Data.URI.UserInfo as UserInfo
import Test.Spec (Spec, describe)
import Test.Util (testIso)

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "Authority parser/printer" do
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//localhost"
      (Authority
        Nothing
        (Just (This (NameAddress (RegName.unsafeFromString "localhost")))))
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//localhost:3000"
      (Authority
        Nothing
        (Just (Both (NameAddress (RegName.unsafeFromString "localhost")) (Port 3000))))
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//user@localhost:3000"
      (Authority
        (Just (UserInfo.unsafeFromString "user"))
        (Just (Both (NameAddress (RegName.unsafeFromString "localhost")) (Port 3000))))
    -- testIso
    --   (Authority.parser optionsMany)
    --   (Authority.print optionsMany)
    --   "//mongo-1,mongo-2"
    --   (Authority
    --     Nothing
    --     [ This (NameAddress (RegName.unsafeFromString "mongo-1"))
    --     , This (NameAddress (RegName.unsafeFromString "mongo-2"))
    --     ])
    -- testIso
    --   (Authority.parser optionsMany)
    --   (Authority.print optionsMany)
    --   "//mongo-1:2000,mongo-2:3000"
    --   (Authority
    --     Nothing
    --     [ Both (NameAddress (RegName.unsafeFromString "mongo-1")) (Port 2000)
    --     , Both (NameAddress (RegName.unsafeFromString "mongo-2")) (Port 3000)
    --     ])
    -- testIso
    --   (Authority.parser optionsMany)
    --   (Authority.print optionsMany)
    --   "//mongo-1:2000,mongo-2"
    --   (Authority
    --     Nothing
    --     [ Both (NameAddress (RegName.unsafeFromString "mongo-1")) (Port 2000)
    --     , This (NameAddress (RegName.unsafeFromString "mongo-2"))
    --     ])
    -- testIso
    --   (Authority.parser optionsMany)
    --   (Authority.print optionsMany)
    --   "//mongo-1,mongo-2:3000"
    --   (Authority
    --     Nothing
    --     [ This (NameAddress (RegName.unsafeFromString "mongo-1"))
    --     , Both (NameAddress (RegName.unsafeFromString "mongo-2")) (Port 3000)
    --     ])
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//:8000"
      (Authority Nothing (Just (That (Port 8000))))
    -- testIso
    --   (Authority.parser optionsMany)
    --   (Authority.print optionsMany)
    --   "//:2000,:3000"
    --   (Authority
    --     Nothing
    --     [ That (Port 2000)
    --     , That (Port 3000)
    --     ])

optionsSingle ∷ Record (AuthorityOptions UserInfo (HostPortPair Host Port))
optionsSingle =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  }

-- optionsMany ∷ Record (AuthorityOptions UserInfo (HostPortPair Host Port))
-- optionsMany =
--   { parseUserInfo: pure
--   , printUserInfo: id
--   , parseHosts: Right { split: void (PS.char ','), build: Array.fromFoldable }
--   , printHosts: String.joinWith ","
--   , parseHost: pure
--   , printHost: id
--   , parsePort: pure
--   , printPort: id
--   }
