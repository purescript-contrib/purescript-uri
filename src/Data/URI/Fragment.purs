module Data.URI.Fragment where

import Prelude

import Control.Alt ((<|>))
import Data.URI (Fragment(..))
import Data.URI.Common (decodePCTComponent, joinWith, parsePChar)
import Global (encodeURIComponent)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser Fragment
parser = Fragment <<< joinWith "" <$> many p
  where
  p = parsePChar decodePCTComponent <|> string "/" <|> string "?"

print ∷ Fragment → String
print (Fragment f) = encodeURIComponent f
