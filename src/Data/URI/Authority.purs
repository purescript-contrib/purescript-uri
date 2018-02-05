module Data.URI.Authority
  ( Authority(..)
  , AuthorityOptions
  , AuthorityParseOptions
  , AuthorityPrintOptions
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
import Data.Eq (class Eq1, eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe, maybe)
import Data.Ord (class Ord1, compare1)
import Data.Tuple (Tuple(..))
import Data.URI.Common (URIPartParseError)
import Data.URI.Host (Host(..), RegName, _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Host as Host
import Data.URI.Port (Port(..))
import Data.URI.Port as Port
import Data.URI.UserInfo (UserInfo)
import Data.URI.UserInfo as UserInfo
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe, try)
import Text.Parsing.Parser.String (char, string)

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority userInfo hosts host port = Authority (Maybe userInfo) (hosts (Tuple host (Maybe port)))

instance eqAuthority ∷ (Eq userInfo, Eq1 hosts, Eq host, Eq port) ⇒ Eq (Authority userInfo hosts host port) where
  eq (Authority ui1 hs1) (Authority ui2 hs2) = eq ui1 ui2 && eq1 hs1 hs2

instance ordAuthority ∷ (Ord userInfo, Ord1 hosts, Ord host, Ord port) ⇒ Ord (Authority userInfo hosts host port) where
  compare (Authority ui1 hs1) (Authority ui2 hs2) =
    case compare ui1 ui2 of
      EQ → compare1 hs1 hs2
      o → o

derive instance genericAuthority ∷ Generic (Authority userInfo hosts host port) _

instance showAuthority ∷ (Show userInfo, Show (hosts (Tuple host (Maybe port))), Show port) ⇒ Show (Authority userInfo hosts host port) where show = genericShow

type AuthorityOptions userInfo hosts host port =
  AuthorityParseOptions userInfo hosts host port
    (AuthorityPrintOptions userInfo hosts host port ())

type AuthorityParseOptions userInfo hosts host port r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ ∀ a. Parser String a → Parser String (hosts a)
  , parseHost ∷ Host → Either URIPartParseError host
  , parsePort ∷ Port → Either URIPartParseError port
  | r
  )

type AuthorityPrintOptions userInfo hosts host port r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts String → String
  , printHost ∷ host → Host
  , printPort ∷ port → Port
  | r
  )

parser
  ∷ ∀ userInfo hosts host port r
  . Record (AuthorityParseOptions userInfo hosts host port r)
  → Parser String (Authority userInfo hosts host port)
parser opts = do
  _ ← string "//"
  ui ← optionMaybe $ try (UserInfo.parser opts.parseUserInfo <* char '@')
  hosts ← opts.parseHosts $
    Tuple
      <$> Host.parser opts.parseHost
      <*> optionMaybe (char ':' *> Port.parser opts.parsePort)
  pure $ Authority ui hosts

print
  ∷ ∀ userInfo hosts host port r
  . Functor hosts
  ⇒ Record (AuthorityPrintOptions userInfo hosts host port r)
  → Authority userInfo hosts host port
  → String
print opts (Authority ui hs) =
  "//" <> printUserInfo ui <> opts.printHosts (printHostAndPort <$> hs)
  where
  printUserInfo =
    maybe "" (\u → UserInfo.print opts.printUserInfo u <> "@")
  printHostAndPort (Tuple h p) =
    Host.print opts.printHost h
      <> maybe "" (\n → ":" <> Port.print opts.printPort n) p

_userInfo
  ∷ ∀ userInfo hosts host port
  . Lens'
      (Authority userInfo hosts host port)
      (Maybe userInfo)
_userInfo =
  lens
    (\(Authority ui _) → ui)
    (\(Authority _ hs) ui → Authority ui hs)

_hosts
  ∷ ∀ userInfo hosts host port
  . Lens'
      (Authority userInfo hosts host port)
      (hosts (Tuple host (Maybe port)))
_hosts =
  lens
    (\(Authority _ hs) → hs)
    (\(Authority ui _) hs → Authority ui hs)
