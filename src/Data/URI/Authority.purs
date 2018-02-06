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

import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1, compare1)
import Data.These (These(..))
import Data.URI.Common (URIPartParseError)
import Data.URI.Host (Host(..), RegName, _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Host as Host
import Data.URI.Port (Port(..))
import Data.URI.Port as Port
import Data.URI.UserInfo (UserInfo)
import Data.URI.UserInfo as UserInfo
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy1, try)
import Text.Parsing.Parser.String (char, string)

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority userInfo hosts host port = Authority (Maybe userInfo) (hosts (These host port))

instance eqAuthority ∷ (Eq userInfo, Eq1 hosts, Eq host, Eq port) ⇒ Eq (Authority userInfo hosts host port) where
  eq (Authority ui1 hs1) (Authority ui2 hs2) = eq ui1 ui2 && eq1 hs1 hs2

instance ordAuthority ∷ (Ord userInfo, Ord1 hosts, Ord host, Ord port) ⇒ Ord (Authority userInfo hosts host port) where
  compare (Authority ui1 hs1) (Authority ui2 hs2) =
    case compare ui1 ui2 of
      EQ → compare1 hs1 hs2
      o → o

derive instance genericAuthority ∷ Generic (Authority userInfo hosts host port) _

instance showAuthority ∷ (Show userInfo, Show (hosts (These host port)), Show port) ⇒ Show (Authority userInfo hosts host port) where show = genericShow

type AuthorityOptions userInfo hosts host port =
  AuthorityParseOptions userInfo hosts host port
    (AuthorityPrintOptions userInfo hosts host port ())

type AuthorityParseOptions userInfo hosts host port r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ HostsParseOptions hosts
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

type HostsParseOptions hosts
  = ∀ a
  . Either
      (Maybe a → hosts a)
      { split ∷ Parser String Unit, build ∷ List a → hosts a }

parser
  ∷ ∀ userInfo hosts host port r
  . Record (AuthorityParseOptions userInfo hosts host port r)
  → Parser String (Authority userInfo hosts host port)
parser opts = do
  _ ← string "//"
  ui ← optionMaybe $ try (UserInfo.parser opts.parseUserInfo <* char '@')
  hosts ← case opts.parseHosts of
    Left build → do
      host ← optionMaybe (Host.parser opts.parseHost)
      port ← optionMaybe (char ':' *> Port.parser opts.parsePort)
      pure $ build case host, port of
        Just h, Nothing → Just (This h)
        Nothing, Just p → Just (That p)
        Just h, Just p → Just (Both h p)
        Nothing, Nothing → Nothing
    Right { split, build } → do
      hosts ←
        flip sepBy1 split $
          Both
            <$> Host.parser opts.parseHost
            <*> (char ':' *> Port.parser opts.parsePort)
      pure $ build hosts
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
  printHostAndPort (This h) =
    Host.print opts.printHost h
  printHostAndPort (That p) =
    ":" <> Port.print opts.printPort p
  printHostAndPort (Both h p) =
    Host.print opts.printHost h <> ":" <> Port.print opts.printPort p

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
      (hosts (These host port))
_hosts =
  lens
    (\(Authority _ hs) → hs)
    (\(Authority ui _) hs → Authority ui hs)
