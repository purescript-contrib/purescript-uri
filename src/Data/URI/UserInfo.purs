module Data.URI.UserInfo where

import Prelude

import Control.Alt ((<|>))
import Data.URI.Common (decodePCT, joinWith, parsePCTEncoded, parseSubDelims, parseUnreserved, wrapParser)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

parser ∷ ∀ userInfo. Parser userInfo → Parser userInfo
parser p = wrapParser p (joinWith "" <$> many1 p')
  where
  p' = parseUnreserved
    <|> parsePCTEncoded decodePCT
    <|> parseSubDelims
    <|> string ":"

print ∷ ∀ userInfo. (userInfo → String) → userInfo → String
print = id
