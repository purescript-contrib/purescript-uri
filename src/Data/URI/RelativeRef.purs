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
  , module Data.URI.RelativePart
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String as S
import Data.Tuple (Tuple)
import Data.URI.Fragment as Fragment
import Data.URI.Query as Query
import Data.URI.RelativePart (Authority(..), AuthorityParseOptions, AuthorityPrintOptions, Host(..), Port(..), RelativePart(..), RelativePartParseOptions, RelativePartPrintOptions, _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.RelativePart as RPart
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A relative reference for a URI.
data RelativeRef userInfo hosts relPath query fragment = RelativeRef (RelativePart userInfo hosts relPath) (Maybe query) (Maybe fragment)

derive instance eqRelativeRef ∷ (Eq userInfo, Eq1 hosts, Eq relPath, Eq query, Eq fragment) ⇒ Eq (RelativeRef userInfo hosts relPath query fragment)
derive instance ordRelativeRef ∷ (Ord userInfo, Ord1 hosts, Ord relPath, Ord query, Ord fragment) ⇒ Ord (RelativeRef userInfo hosts relPath query fragment)
derive instance genericRelativeRef ∷ Generic (RelativeRef userInfo hosts relPath query fragment) _
instance showRelativeRef ∷ (Show userInfo, Show (hosts (Tuple Host (Maybe Port))), Show relPath, Show query, Show fragment) ⇒ Show (RelativeRef userInfo hosts relPath query fragment) where show = genericShow

type RelativeRefOptions userInfo hosts relPath query fragment =
  RelativeRefParseOptions userInfo hosts relPath query fragment
    (RelativeRefPrintOptions userInfo hosts relPath query fragment ())

type RelativeRefParseOptions userInfo hosts relPath query fragment r =
  ( parseUserInfo ∷ String → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseRelPath ∷ String → Either ParseError relPath
  , parseQuery ∷ String → Either ParseError query
  , parseFragment ∷ String → Either ParseError fragment
  | r
  )

type RelativeRefPrintOptions userInfo hosts relPath query fragment r =
  ( printUserInfo ∷ userInfo → String
  , printHosts ∷ hosts String → String
  , printRelPath ∷ relPath → String
  , printQuery ∷ query → String
  , printFragment ∷ fragment → String
  | r
  )

parser
  ∷ ∀ userInfo hosts relPath query fragment r
  . Record (RelativeRefParseOptions userInfo hosts relPath query fragment r)
  → Parser (RelativeRef userInfo hosts relPath query fragment)
parser opts =
  RelativeRef
    <$> RPart.parser opts
    <*> optionMaybe (Query.parser opts.parseQuery)
    <*> optionMaybe (Fragment.parser opts.parseFragment)
    <* eof

print
  ∷ ∀ userInfo hosts relPath query fragment r
  . Functor hosts
  ⇒ Record (RelativeRefPrintOptions userInfo hosts relPath query fragment r)
  → RelativeRef userInfo hosts relPath query fragment
  → String
print opts (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (RPart.print opts h)
    , Query.print opts.printQuery <$> q
    , Fragment.print opts.printFragment <$> f
    ]

_relPart
  ∷ ∀ userInfo hosts relPath query fragment
  . Lens' (RelativeRef userInfo hosts relPath query fragment) (RelativePart userInfo hosts relPath)
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query
  ∷ ∀ userInfo hosts relPath query fragment
  . Lens' (RelativeRef userInfo hosts relPath query fragment) (Maybe query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment
  ∷ ∀ userInfo hosts relPath query fragment
  . Lens' (RelativeRef userInfo hosts relPath query fragment) (Maybe fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
