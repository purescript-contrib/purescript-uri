module Data.URI.URI where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (URI(..))
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
  <*> optionMaybe (string "?" *> Query.parser)
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
