module URI.Authority
  ( Authority(..)
  , AuthorityOptions
  , AuthorityParseOptions
  , AuthorityPrintOptions
  , parser
  , print
  , _userInfo
  , _hosts
  , module URI.Host
  , module URI.Port
  , module URI.UserInfo
  ) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe, try)
import Text.Parsing.Parser.String (char, string)
import URI.Common (URIPartParseError, wrapParser)
import URI.Host (Host(..), IPv4Address, IPv6Address, RegName, _IPv4Address, _IPv6Address, _NameAddress)
import URI.Port (Port)
import URI.UserInfo (UserInfo)
import URI.UserInfo as UserInfo

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`.
newtype Authority userInfo hosts =
  Authority { userInfo :: Maybe userInfo
            , hosts :: hosts
            }

derive instance eqAuthority ∷ (Eq userInfo, Eq hosts) ⇒ Eq (Authority userInfo hosts)
derive instance ordAuthority ∷ (Ord userInfo, Ord hosts) ⇒ Ord (Authority userInfo hosts)
derive instance genericAuthority ∷ Generic (Authority userInfo hosts) _
instance showAuthority ∷ (Show userInfo, Show hosts) ⇒ Show (Authority userInfo hosts) where show = genericShow

-- | A row type for describing the options fields used by the authority parser
-- | and printer.
-- |
-- | Used as `Record (AuthorityOptions userInfo hosts)` when type annotating an
-- | options record.
type AuthorityOptions userInfo hosts =
  AuthorityParseOptions userInfo hosts
    (AuthorityPrintOptions userInfo hosts ())

-- | A row type for describing the options fields used by the authority parser.
-- |
-- | Used as `Record (AuthorityParseOptions userInfo hosts ())` when type
-- | annotating an options record.
type AuthorityParseOptions userInfo hosts r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  | r
  )

-- | A row type for describing the options fields used by the authority printer.
-- |
-- | Used as `Record (AuthorityPrintOptions userInfo hosts ())` when type
-- | annotating an options record.
type AuthorityPrintOptions userInfo hosts r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  | r
  )

-- | A parser for the authority part of a URI. Expects values with a `"//"`
-- | prefix.
parser
  ∷ ∀ userInfo hosts r
  . Record (AuthorityParseOptions userInfo hosts r)
  → Parser String (Authority userInfo hosts)
parser opts = do
  _ ← string "//"
  ui ← optionMaybe $ try (wrapParser opts.parseUserInfo UserInfo.parser <* char '@')
  hosts ← opts.parseHosts
  pure $ Authority { userInfo: ui, hosts }

-- | A printer for the authority part of a URI. Will print the value with a
-- | `"//"` prefix.
print
  ∷ ∀ userInfo hosts r
  . Record (AuthorityPrintOptions userInfo hosts r)
  → Authority userInfo hosts
  → String
print opts (Authority { userInfo, hosts }) = case userInfo of
  Just ui → "//" <> UserInfo.print (opts.printUserInfo ui) <> "@" <> opts.printHosts hosts
  Nothing → "//" <> opts.printHosts hosts

-- | A lens for the user-info component of the authority.
_userInfo
  ∷ ∀ userInfo hosts
  . Lens'
      (Authority userInfo hosts)
      (Maybe userInfo)
_userInfo =
  lens
    (\(Authority rec) → rec.userInfo)
    (\(Authority rec) ui → Authority (rec { userInfo = ui }))

-- | A lens for the host(s) component of the authority.
_hosts
  ∷ ∀ userInfo hosts
  . Lens'
      (Authority userInfo hosts)
      hosts
_hosts =
  lens
    (\(Authority rec) → rec.hosts)
    (\(Authority rec) h → Authority (rec { hosts = h }))
