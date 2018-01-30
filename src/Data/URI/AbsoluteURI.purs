module Data.URI.AbsoluteURI
  ( AbsoluteURI(..)
  , AbsoluteURIOptions
  , AbsoluteURIParseOptions
  , parser
  , AbsoluteURIPrintOptions
  , print
  , _scheme
  , _hierPart
  , _query
  , module Data.URI.HierarchicalPart
  , module Data.URI.Scheme
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
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query as Query
import Data.URI.Scheme (Scheme(..))
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A generic AbsoluteURI
data AbsoluteURI userInfo hosts hierPath query = AbsoluteURI Scheme (HierarchicalPart userInfo hosts hierPath) (Maybe query)

derive instance eqAbsoluteURI ∷ (Eq userInfo, Eq1 hosts, Eq hierPath, Eq query) ⇒ Eq (AbsoluteURI userInfo hosts hierPath query)
derive instance ordAbsoluteURI ∷ (Ord userInfo, Ord1 hosts, Ord hierPath, Ord query) ⇒ Ord (AbsoluteURI userInfo hosts hierPath query)
derive instance genericAbsoluteURI ∷ Generic (AbsoluteURI userInfo hosts hierPath query) _
instance showAbsoluteURI ∷ (Show userInfo, Show (hosts (Tuple Host (Maybe Port))), Show hierPath, Show query) ⇒ Show (AbsoluteURI userInfo hosts hierPath query) where show = genericShow

type AbsoluteURIOptions userInfo hosts hierPath query =
  AbsoluteURIParseOptions userInfo hosts hierPath query
    (AbsoluteURIPrintOptions userInfo hosts hierPath query ())

type AbsoluteURIParseOptions userInfo hosts hierPath query r =
  ( parseUserInfo ∷ String → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseHierPath ∷ String → Either ParseError hierPath
  , parseQuery ∷ String → Either ParseError query
  | r
  )

type AbsoluteURIPrintOptions userInfo hosts hierPath query r =
  ( printUserInfo ∷ userInfo → String
  , printHosts ∷ hosts String → String
  , printHierPath ∷ hierPath → String
  , printQuery ∷ query → String
  | r
  )

parser
  ∷ ∀ userInfo hosts hierPath query r
  . Record (AbsoluteURIParseOptions userInfo hosts hierPath query r)
  → Parser (AbsoluteURI userInfo hosts hierPath query)
parser opts = AbsoluteURI
  <$> Scheme.parser
  <*> HPart.parser opts
  <*> optionMaybe (Query.parser opts.parseQuery)
  <* eof

print
  ∷ ∀ userInfo hosts hierPath query r
  . Functor hosts
  ⇒ Record (AbsoluteURIPrintOptions userInfo hosts hierPath query r)
  → AbsoluteURI userInfo hosts hierPath query
  → String
print opts (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ Just (Scheme.print s)
    , Just (HPart.print opts h)
    , Query.print opts.printQuery <$> q
    ]

_scheme
  ∷ ∀ userInfo hosts hierPath query
  . Lens' (AbsoluteURI userInfo hosts hierPath query) Scheme
_scheme =
  lens
    (\(AbsoluteURI s _ _) → s)
    (\(AbsoluteURI _ h q) s → AbsoluteURI s h q)

_hierPart
  ∷ ∀ userInfo hosts hierPath query
  . Lens' (AbsoluteURI userInfo hosts hierPath query) (HierarchicalPart userInfo hosts hierPath)
_hierPart =
  lens
    (\(AbsoluteURI _ h _) → h)
    (\(AbsoluteURI s _ q) h → AbsoluteURI s h q)

_query
  ∷ ∀ userInfo hosts hierPath query
  . Lens' (AbsoluteURI userInfo hosts hierPath query) (Maybe query)
_query =
  lens
    (\(AbsoluteURI _ _ q) → q)
    (\(AbsoluteURI s h _) q → AbsoluteURI s h q)
