module Data.URI.AbsoluteURI where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (AbsoluteURI(..), HierarchicalPart, Query, Scheme)
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

parse ∷ String → Either ParseError AbsoluteURI
parse = runParser parser

parser ∷ Parser AbsoluteURI
parser = AbsoluteURI
  <$> (optionMaybe Scheme.parser)
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
