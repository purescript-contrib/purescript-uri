module Data.URI.URI where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (Fragment, HierarchicalPart, Query, Scheme, URI(..))
import Data.URI.Fragment as Fragment
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string, eof)

parse ∷ String → Either ParseError URI
parse = runParser parser

parser ∷ Parser URI
parser = URI
  <$> (optionMaybe Scheme.parser <* string ":")
  <*> (string "//" *> HPart.parser)
  <*> optionMaybe Query.parser
  <*> optionMaybe (string "#" *> try Fragment.parser)
  <* eof

print ∷ URI → String
print (URI s h q f) =
  S.joinWith "" $ catMaybes
    [ (\scheme → Scheme.print scheme <> "//") <$> s
    , Just (HPart.print h)
    , Query.print <$> q
    , (\frag → "#" <> Fragment.print frag) <$> f
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
