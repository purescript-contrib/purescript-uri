module Data.URI.Query (parser, print) where

import Prelude

import Control.Alt ((<|>))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.URI (Query(..))
import Data.URI.Common (joinWith, parseFragmentOrQuery, printFragmentOrQuery, rxPat, wrapParser)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy, many)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser Query
parser = Query <$> (wrapParser parseParts $ try (joinWith "" <$> many parseFragmentOrQuery))

parseParts ∷ Parser (List (Tuple String (Maybe String)))
parseParts = sepBy parsePart (string ";" <|> string "&")

parsePart ∷ Parser (Tuple String (Maybe String))
parsePart = do
  key ← rxPat "[^=;&]+"
  value ← optionMaybe $ string "=" *> rxPat "[^;&]*"
  pure $ Tuple key value

print ∷ Query → String
print (Query m) =
  case m of
    Nil → "?"
    items → "?" <> joinWith "&" (printPart <$> items)
  where
  printPart ∷ Tuple String (Maybe String) → String
  printPart (Tuple k Nothing) = printFragmentOrQuery k
  printPart (Tuple k (Just v)) = printFragmentOrQuery k <> "=" <> printFragmentOrQuery v
