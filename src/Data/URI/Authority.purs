module Data.URI.Authority
  ( Authority(..)
  , AuthorityOptions
  , AuthorityParseOptions
  , AuthorityPrintOptions
  , HostsParseOptions
  , parser
  , print
  , _userInfo
  , _hosts
  , module Data.URI.Host
  , module Data.URI.Port
  , module Data.URI.UserInfo
  ) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.URI.Common (URIPartParseError)
import Data.URI.Host (Host(..), RegName, _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Port (Port(..))
import Data.URI.UserInfo (UserInfo)
import Data.URI.UserInfo as UserInfo
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe, try)
import Text.Parsing.Parser.String (char, string)

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority userInfo hosts = Authority (Maybe userInfo) hosts

derive instance eqAuthority ∷ (Eq userInfo, Eq hosts) ⇒ Eq (Authority userInfo hosts)
derive instance ordAuthority ∷ (Ord userInfo, Ord hosts) ⇒ Ord (Authority userInfo hosts)
derive instance genericAuthority ∷ Generic (Authority userInfo hosts) _
instance showAuthority ∷ (Show userInfo, Show hosts) ⇒ Show (Authority userInfo hosts) where show = genericShow

type AuthorityOptions userInfo hosts =
  AuthorityParseOptions userInfo hosts
    (AuthorityPrintOptions userInfo hosts ())

type AuthorityParseOptions userInfo hosts r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  | r
  )

type AuthorityPrintOptions userInfo hosts r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  | r
  )

type HostsParseOptions hosts
  = ∀ a
  . Either
      (Maybe a → hosts a)
      { split ∷ Parser String Unit, build ∷ List a → hosts a }

parser
  ∷ ∀ userInfo hosts r
  . Record (AuthorityParseOptions userInfo hosts r)
  → Parser String (Authority userInfo hosts)
parser opts = do
  _ ← string "//"
  ui ← optionMaybe $ try (UserInfo.parser opts.parseUserInfo <* char '@')
  hosts ← opts.parseHosts
  pure $ Authority ui hosts

print
  ∷ ∀ userInfo hosts r
  . Record (AuthorityPrintOptions userInfo hosts r)
  → Authority userInfo hosts
  → String
print opts (Authority mui hs) = case mui of
  Just ui → "//" <> UserInfo.print opts.printUserInfo ui <> "@" <> opts.printHosts hs
  Nothing → "//" <> opts.printHosts hs

_userInfo
  ∷ ∀ userInfo hosts
  . Lens'
      (Authority userInfo hosts)
      (Maybe userInfo)
_userInfo =
  lens
    (\(Authority ui _) → ui)
    (\(Authority _ hs) ui → Authority ui hs)

_hosts
  ∷ ∀ userInfo hosts
  . Lens'
      (Authority userInfo hosts)
      hosts
_hosts =
  lens
    (\(Authority _ hs) → hs)
    (\(Authority ui _) hs → Authority ui hs)
