module Data.URI.RelativeRef where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (RelativeRef(..))
import Data.URI.Fragment as Fragment
import Data.URI.Query as Query
import Data.URI.RelativePart as RPart
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string, eof)

parse ∷ String → Either ParseError RelativeRef
parse = runParser parser

parser ∷ Parser RelativeRef
parser = RelativeRef
  <$> RPart.parser
  <*> optionMaybe (string "?" *> Query.parser)
  <*> optionMaybe (string "#" *> try Fragment.parser)
  <* eof

print ∷ RelativeRef → String
print (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (RPart.print h)
    , Query.print <$> q
    , (\frag → "#" <> Fragment.print frag) <$> f
    ]
