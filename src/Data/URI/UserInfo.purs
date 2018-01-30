module Data.URI.UserInfo where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.URI.Common (decodePCT, joinWith, parsePCTEncoded, parseSubDelims, parseUnreserved, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ ui. (String → Either ParseError ui) → Parser ui
parser p = wrapParser p (joinWith "" <$> many1 p')
  where
  p' = parseUnreserved
    <|> parsePCTEncoded decodePCT
    <|> parseSubDelims
    <|> string ":"

print ∷ ∀ ui. (ui → String) → ui → String
print = id
