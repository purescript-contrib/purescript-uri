module Data.URI.UserInfo where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.String as String
import Data.URI.Common (decodePCT, parsePCTEncoded, parseSubDelims, parseUnreserved, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ ui. (String → Either ParseError ui) → Parser ui
parser p = wrapParser p (String.joinWith "" <$> Array.some p')
  where
  p' = parseUnreserved
    <|> parsePCTEncoded decodePCT
    <|> parseSubDelims
    <|> string ":"

print ∷ ∀ ui. (ui → String) → ui → String
print = id
