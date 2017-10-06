module Data.URI.URI
  ( URI(..)
  , parse
  , parser
  , print
  , _scheme
  , _hierPart
  , _query
  , _fragment
  , module Data.URI.Fragment
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
import Data.URI.Fragment (Fragment(..))
import Data.URI.Fragment as Fragment
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), URIPath, URIPathAbs, URIPathRel, UserInfo(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query (Query(..))
import Data.URI.Query as Query
import Data.URI.Scheme (Scheme(..))
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (Parser, ParseError, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A generic URI
data URI = URI (Maybe Scheme) HierarchicalPart (Maybe Query) (Maybe Fragment)

derive instance eqURI ∷ Eq URI
derive instance ordURI ∷ Ord URI
derive instance genericURI ∷ Generic URI _
instance showURI ∷ Show URI where show = genericShow

parse ∷ String → Either ParseError URI
parse = runParser parser

parser ∷ Parser URI
parser = URI
  <$> optionMaybe Scheme.parser
  <*> HPart.parser
  <*> optionMaybe Query.parser
  <*> optionMaybe Fragment.parser
  <* eof

print ∷ URI → String
print (URI s h q f) =
  S.joinWith "" $ catMaybes
    [ Scheme.print <$> s
    , Just (HPart.print h)
    , Query.print <$> q
    , Fragment.print <$> f
    ]

_scheme ∷ Lens' URI (Maybe Scheme)
_scheme =
  lens
    (\(URI s _ _ _) → s)
    (\(URI _ h q f) s → URI s h q f)

_hierPart ∷ Lens' URI HierarchicalPart
_hierPart =
  lens
    (\(URI _ h _ _) → h)
    (\(URI s _ q f) h → URI s h q f)

_query ∷ Lens' URI (Maybe Query)
_query =
  lens
    (\(URI _ _ q _) → q)
    (\(URI s h _ f) q → URI s h q f)

_fragment ∷ Lens' URI (Maybe Fragment)
_fragment =
  lens
    (\(URI _ _ _ f) → f)
    (\(URI s h q _) f → URI s h q f)
