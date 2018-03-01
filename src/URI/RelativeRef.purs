module URI.RelativeRef
  ( RelativeRef(..)
  , RelativeRefOptions
  , RelativeRefParseOptions
  , RelativeRefPrintOptions
  , parser
  , print
  , _relPart
  , _query
  , _fragment
  , module URI.Fragment
  , module URI.Query
  , module URI.RelativePart
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as String
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (eof)
import URI.Common (URIPartParseError, wrapParser)
import URI.Fragment (Fragment)
import URI.Fragment as Fragment
import URI.Query (Query)
import URI.Query as Query
import URI.RelativePart (Authority(..), AuthorityOptions, AuthorityParseOptions, AuthorityPrintOptions, Host(..), HostsParseOptions, IPv4Address, IPv6Address, Path, PathAbsolute, PathNoScheme, Port, RegName, RelPath, RelativePart(..), RelativePartOptions, RelativePartParseOptions, RelativePartPrintOptions, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _relPath, _userInfo)
import URI.RelativePart as RPart

-- | A relative reference for a URI.
data RelativeRef userInfo hosts path relPath query fragment = RelativeRef (RelativePart userInfo hosts path relPath) (Maybe query) (Maybe fragment)

derive instance eqRelativeRef ∷ (Eq userInfo, Eq hosts, Eq path, Eq relPath, Eq query, Eq fragment) ⇒ Eq (RelativeRef userInfo hosts path relPath query fragment)
derive instance ordRelativeRef ∷ (Ord userInfo, Ord hosts, Ord path, Ord relPath, Ord query, Ord fragment) ⇒ Ord (RelativeRef userInfo hosts path relPath query fragment)
derive instance genericRelativeRef ∷ Generic (RelativeRef userInfo hosts path relPath query fragment) _
instance showRelativeRef ∷ (Show userInfo, Show hosts, Show path, Show relPath, Show query, Show fragment) ⇒ Show (RelativeRef userInfo hosts path relPath query fragment) where show = genericShow

type RelativeRefOptions userInfo hosts path relPath query fragment =
  RelativeRefParseOptions userInfo hosts path relPath query fragment
    (RelativeRefPrintOptions userInfo hosts path relPath query fragment ())

type RelativeRefParseOptions userInfo hosts path relPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseRelPath ∷ Either PathAbsolute PathNoScheme → Either URIPartParseError relPath
  , parseQuery ∷ Query → Either URIPartParseError query
  , parseFragment ∷ Fragment → Either URIPartParseError fragment
  | r
  )

type RelativeRefPrintOptions userInfo hosts path relPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printRelPath ∷ relPath → Either PathAbsolute PathNoScheme
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

parser
  ∷ ∀ userInfo hosts path relPath query fragment r
  . Record (RelativeRefParseOptions userInfo hosts path relPath query fragment r)
  → Parser String (RelativeRef userInfo hosts path relPath query fragment)
parser opts =
  RelativeRef
    <$> RPart.parser opts
    <*> optionMaybe (wrapParser opts.parseQuery Query.parser)
    <*> optionMaybe (wrapParser opts.parseFragment Fragment.parser)
    <* eof

print
  ∷ ∀ userInfo hosts path relPath query fragment r
  . Record (RelativeRefPrintOptions userInfo hosts path relPath query fragment r)
  → RelativeRef userInfo hosts path relPath query fragment
  → String
print opts (RelativeRef h q f) =
  String.joinWith "" $ Array.catMaybes
    [ Just (RPart.print opts h)
    , Query.print <<< opts.printQuery <$> q
    , Fragment.print <<< opts.printFragment <$> f
    ]

_relPart
  ∷ ∀ userInfo hosts path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts path relPath query fragment)
      (RelativePart userInfo hosts path relPath)
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query
  ∷ ∀ userInfo hosts path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts path relPath query fragment)
      (Maybe query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment
  ∷ ∀ userInfo hosts path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts path relPath query fragment)
      (Maybe fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
