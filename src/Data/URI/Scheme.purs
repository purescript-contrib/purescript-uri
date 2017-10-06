module Data.URI.Scheme where

import Prelude

import Data.URI (Scheme(..))
import Data.URI.Common (rxPat)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser Scheme
parser = Scheme <$> rxPat "[a-z][a-z0-9+\\.\\-]+" <* string ":"

print ∷ Scheme → String
print (Scheme s) = s <> ":"
