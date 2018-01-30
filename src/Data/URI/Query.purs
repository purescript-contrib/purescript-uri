module Data.URI.Query where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.String as String
import Data.URI.Common (decodePCT, parsePChar, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ q. (String → Either ParseError q) → Parser q
parser parseQ = string "?" *> (wrapParser parseQ query)
  where
  query =
    String.joinWith ""
      <$> Array.many (parsePChar decodePCT <|> string "/" <|> string "?")

print ∷ ∀ q. (q → String) → q → String
print printQ q = "?" <> printQ q
