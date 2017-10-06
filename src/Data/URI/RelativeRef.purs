module Data.URI.RelativeRef where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (Fragment, Query, RelativePart, RelativeRef(..))
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
  <*> optionMaybe Query.parser
  <*> optionMaybe (string "#" *> try Fragment.parser)
  <* eof

print ∷ RelativeRef → String
print (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (RPart.print h)
    , Query.print <$> q
    , (\frag → "#" <> Fragment.print frag) <$> f
    ]

_relPart ∷ Lens' RelativeRef RelativePart
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query ∷ Lens' RelativeRef (Maybe Query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment ∷ Lens' RelativeRef (Maybe Fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
