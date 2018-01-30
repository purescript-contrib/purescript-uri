module Data.URI.Query where

import Prelude

import Data.Either (Either)
import Data.URI.Common (rxPat, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser, try)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ q. (String → Either ParseError q) → Parser q
parser parseQ = string "?" *> (wrapParser parseQ (try (rxPat "[^#]*")))

print ∷ ∀ q. (q → String) → q → String
print printQ q = "?" <> printQ q
