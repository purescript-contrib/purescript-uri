module Data.URI.AbsoluteURI
  ( AbsoluteURI(..)
  , parse
  , parser
  , print
  , _scheme
  , _hierPart
  , _query
  , module Data.URI.HierarchicalPart
  , module Data.URI.Query
  , module Data.URI.Scheme
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.HierarchicalPart as HPart
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), URIPath, URIPathAbs, URIPathRel, UserInfo(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.Query as Query
import Data.URI.Query (Query(..))
import Data.URI.Scheme as Scheme
import Data.URI.Scheme (Scheme(..))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | An absolute URI.
data AbsoluteURI = AbsoluteURI (Maybe Scheme) HierarchicalPart (Maybe Query)

derive instance eqAbsoluteURI ∷ Eq AbsoluteURI
derive instance ordAbsoluteURI ∷ Ord AbsoluteURI
derive instance genericAbsoluteURI ∷ Generic AbsoluteURI _
instance showAbsoluteURI ∷ Show AbsoluteURI where show = genericShow

parse ∷ String → Either ParseError AbsoluteURI
parse = runParser parser

parser ∷ Parser AbsoluteURI
parser = AbsoluteURI
  <$> optionMaybe Scheme.parser
  <*> HPart.parser
  <*> optionMaybe Query.parser
  <* eof

print ∷ AbsoluteURI → String
print (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ Scheme.print <$> s
    , Just (HPart.print h)
    , Query.print <$> q
    ]

_scheme ∷ Lens' AbsoluteURI (Maybe Scheme)
_scheme =
  lens
    (\(AbsoluteURI s _ _) → s)
    (\(AbsoluteURI _ h q) s → AbsoluteURI s h q)

_hierPart ∷ Lens' AbsoluteURI HierarchicalPart
_hierPart =
  lens
    (\(AbsoluteURI _ h _) → h)
    (\(AbsoluteURI s _ q) h → AbsoluteURI s h q)

_query ∷ Lens' AbsoluteURI (Maybe Query)
_query =
  lens
    (\(AbsoluteURI _ _ q) → q)
    (\(AbsoluteURI s h _) q → AbsoluteURI s h q)
