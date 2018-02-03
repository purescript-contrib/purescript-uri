module Test.URI.Authority where

import Prelude

import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.URI.Authority (Authority(..), AuthorityOptions, Host(..), Port(..), UserInfo)
import Data.URI.Authority as Authority
import Data.URI.Host.RegName as RegName
import Data.URI.UserInfo as UserInfo
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)
import Text.Parsing.StringParser.Combinators (sepBy) as SP
import Text.Parsing.StringParser.String (string) as SP

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "Authority parser/printer" do
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//localhost"
      (Authority
        Nothing
        (Identity (Tuple (NameAddress (RegName.unsafeFromString "localhost")) Nothing)))
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//localhost:3000"
      (Authority
        Nothing
        (Identity (Tuple (NameAddress (RegName.unsafeFromString "localhost")) (Just (Port 3000)))))
    testIso
      (Authority.parser optionsSingle)
      (Authority.print optionsSingle)
      "//user@localhost:3000"
      (Authority
        (Just (UserInfo.unsafeFromString "user"))
        (Identity (Tuple (NameAddress (RegName.unsafeFromString "localhost")) (Just (Port 3000)))))
    testIso
      (Authority.parser optionsMany)
      (Authority.print optionsMany)
      "//mongo-1,mongo-2:3000"
      (Authority
        Nothing
        [ Tuple (NameAddress (RegName.unsafeFromString "mongo-1")) Nothing
        , Tuple (NameAddress (RegName.unsafeFromString "mongo-2")) (Just (Port 3000))
        ])

optionsSingle ∷ Record (AuthorityOptions UserInfo Identity Host Port)
optionsSingle =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: map Identity
  , printHosts: un Identity
  , parseHost: pure
  , printHost: id
  , parsePort: pure
  , printPort: id
  }

optionsMany ∷ Record (AuthorityOptions UserInfo Array Host Port)
optionsMany =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: map Array.fromFoldable <<< flip SP.sepBy (SP.string ",")
  , printHosts: String.joinWith ","
  , parseHost: pure
  , printHost: id
  , parsePort: pure
  , printPort: id
  }
