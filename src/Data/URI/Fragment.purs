module Data.URI.Fragment where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.URI.Common (decodePCTComponent, joinWith, parsePChar, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ f. (String → Either ParseError f) → Parser f
parser parseF = string "#" *>
  wrapParser parseF (joinWith ""
    <$> many (parsePChar decodePCTComponent <|> string "/" <|> string "?"))

print ∷ ∀ f. (f → String) → f → String
print printF f = "#" <> printF f
