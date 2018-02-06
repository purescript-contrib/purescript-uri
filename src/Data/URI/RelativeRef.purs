module Data.URI.RelativeRef
  ( RelativeRef(..)
  , RelativeRefOptions
  , RelativeRefParseOptions
  , RelativeRefPrintOptions
  , parser
  , print
  , _relPart
  , _query
  , _fragment
  , module Data.URI.Fragment
  , module Data.URI.Query
  , module Data.URI.RelativePart
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String as String
import Data.These (These)
import Data.Tuple (Tuple)
import Data.URI.Common (URIPartParseError)
import Data.URI.Fragment (Fragment)
import Data.URI.Fragment as Fragment
import Data.URI.Query (Query)
import Data.URI.Query as Query
import Data.URI.RelativePart (Authority(..), Host(..), HostsParseOptions, Path, PathAbsolute, PathNoScheme, Port(..), RelativePart(..), RelPath, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.RelativePart as RPart
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (eof)

-- | A relative reference for a URI.
data RelativeRef userInfo hosts host port path relPath query fragment = RelativeRef (RelativePart userInfo hosts host port path relPath) (Maybe query) (Maybe fragment)

derive instance eqRelativeRef ∷ (Eq userInfo, Eq1 hosts, Eq host, Eq port, Eq path, Eq relPath, Eq query, Eq fragment) ⇒ Eq (RelativeRef userInfo hosts host port path relPath query fragment)
derive instance ordRelativeRef ∷ (Ord userInfo, Ord1 hosts, Ord host, Ord port, Ord path, Ord relPath, Ord query, Ord fragment) ⇒ Ord (RelativeRef userInfo hosts host port path relPath query fragment)
derive instance genericRelativeRef ∷ Generic (RelativeRef userInfo hosts host port path relPath query fragment) _
instance showRelativeRef ∷ (Show userInfo, Show (hosts (These host port)), Show host, Show port, Show path, Show relPath, Show query, Show fragment) ⇒ Show (RelativeRef userInfo hosts host port path relPath query fragment) where show = genericShow

type RelativeRefOptions userInfo hosts host port path relPath query fragment =
  RelativeRefParseOptions userInfo hosts host port path relPath query fragment
    (RelativeRefPrintOptions userInfo hosts host port path relPath query fragment ())

type RelativeRefParseOptions userInfo hosts host port path relPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ HostsParseOptions hosts
  , parseHost ∷ Host → Either URIPartParseError host
  , parsePort ∷ Port → Either URIPartParseError port
  , parsePath ∷ Path → Either URIPartParseError path
  , parseRelPath ∷ Either PathAbsolute PathNoScheme → Either URIPartParseError relPath
  , parseQuery ∷ Query → Either URIPartParseError query
  , parseFragment ∷ Fragment → Either URIPartParseError fragment
  | r
  )

type RelativeRefPrintOptions userInfo hosts host port path relPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts String → String
  , printHost ∷ host → Host
  , printPort ∷ port → Port
  , printPath ∷ path → Path
  , printRelPath ∷ relPath → Either PathAbsolute PathNoScheme
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

parser
  ∷ ∀ userInfo hosts host port path relPath query fragment r
  . Record (RelativeRefParseOptions userInfo hosts host port path relPath query fragment r)
  → Parser String (RelativeRef userInfo hosts host port path relPath query fragment)
parser opts =
  RelativeRef
    <$> RPart.parser opts
    <*> optionMaybe (Query.parser opts.parseQuery)
    <*> optionMaybe (Fragment.parser opts.parseFragment)
    <* eof

print
  ∷ ∀ userInfo hosts host port path relPath query fragment r
  . Functor hosts
  ⇒ Record (RelativeRefPrintOptions userInfo hosts host port path relPath query fragment r)
  → RelativeRef userInfo hosts host port path relPath query fragment
  → String
print opts (RelativeRef h q f) =
  String.joinWith "" $ Array.catMaybes
    [ Just (RPart.print opts h)
    , Query.print opts.printQuery <$> q
    , Fragment.print opts.printFragment <$> f
    ]

_relPart
  ∷ ∀ userInfo hosts host port path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts host port path relPath query fragment)
      (RelativePart userInfo hosts host port path relPath)
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query
  ∷ ∀ userInfo hosts host port path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts host port path relPath query fragment)
      (Maybe query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment
  ∷ ∀ userInfo hosts host port path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts host port path relPath query fragment)
      (Maybe fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
