module Data.URI.Query where

import Prelude

import Data.URI.Common (rxPat, wrapParser)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ q. Parser q → Parser q
parser parseQ = string "?" *> (wrapParser parseQ (try (rxPat "[^#]*")))

print ∷ ∀ q. (q → String) → q → String
print printQ q = "?" <> printQ q
