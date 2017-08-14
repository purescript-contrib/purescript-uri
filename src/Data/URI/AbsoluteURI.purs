module Data.URI.AbsoluteURI where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (AbsoluteURI(..))
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string, eof)

parse ∷ String → Either ParseError AbsoluteURI
parse = runParser parser

parser ∷ Parser AbsoluteURI
parser = AbsoluteURI
  <$> (optionMaybe Scheme.parser <* string ":")
  <*> HPart.parser
  <*> optionMaybe (string "?" *> Query.parser)
  <* eof

print ∷ AbsoluteURI → String
print (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ Scheme.print <$> s
    , Just (HPart.print h)
    , Query.print <$> q
    ]
