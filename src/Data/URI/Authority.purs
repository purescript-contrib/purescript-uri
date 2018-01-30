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
import Data.URI.Host (Host(..), _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Host as Host
import Data.URI.Port (Port(..))
import Data.URI.Port as Port
import Data.URI.UserInfo as UserInfo
import Text.Parsing.StringParser (ParseError, Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string)

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority userInfo hosts = Authority (Maybe userInfo) (hosts (Tuple Host (Maybe Port)))

instance eqAuthority ∷ (Eq userInfo, Eq1 hosts) ⇒ Eq (Authority userInfo hosts) where
  eq (Authority ui1 hs1) (Authority ui2 hs2) = eq ui1 ui2 && eq1 hs1 hs2

instance ordAuthority ∷ (Ord userInfo, Ord1 hosts) ⇒ Ord (Authority userInfo hosts) where
  compare (Authority ui1 hs1) (Authority ui2 hs2) =
    case compare ui1 ui2 of
      EQ → compare1 hs1 hs2
      o → o

derive instance genericAuthority ∷ Generic (Authority userInfo hosts) _

instance showAuthority ∷ (Show userInfo, Show (hosts (Tuple Host (Maybe Port)))) ⇒ Show (Authority userInfo hosts) where show = genericShow

type AuthorityOptions userInfo hosts =
  AuthorityParseOptions userInfo hosts
    (AuthorityPrintOptions userInfo hosts ())

type AuthorityParseOptions userInfo hosts r =
  ( parseUserInfo ∷ String → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  | r
  )

type AuthorityPrintOptions userInfo hosts r =
  ( printUserInfo ∷ userInfo → String
  , printHosts ∷ hosts String → String
  | r
  )

parser
  ∷ ∀ userInfo hosts r
  . Record (AuthorityParseOptions userInfo hosts r)
  → Parser (Authority userInfo hosts)
parser opts = do
  _ ← string "//"
  ui ← optionMaybe $ try (UserInfo.parser opts.parseUserInfo <* string "@")
  hosts ← opts.parseHosts $ Tuple <$> Host.parser <*> optionMaybe (string ":" *> Port.parser)
  pure $ Authority ui hosts

print
  ∷ ∀ userInfo hosts r
  . Functor hosts
  ⇒ Record (AuthorityPrintOptions userInfo hosts r)
  → Authority userInfo hosts
  → String
print opts (Authority ui hs) =
  "//" <> printUserInfo ui <> opts.printHosts (printHostAndPort <$> hs)
  where
  printUserInfo =
    maybe "" (\u → opts.printUserInfo u <> "@")
  printHostAndPort (Tuple h p) =
    Host.print h <> maybe "" (\n → ":" <> Port.print n) p

_userInfo ∷ ∀ userInfo hosts. Lens' (Authority userInfo hosts) (Maybe userInfo)
_userInfo =
  lens
    (\(Authority ui _) → ui)
    (\(Authority _ hs) ui → Authority ui hs)

_hosts ∷ ∀ userInfo hosts. Lens' (Authority userInfo hosts) (hosts (Tuple Host (Maybe Port)))
_hosts =
  lens
    (\(Authority _ hs) → hs)
    (\(Authority ui _) hs → Authority ui hs)
