module Data.URI.UserInfo where

import Prelude

import Control.Alt ((<|>))
import Data.URI (UserInfo(..))
import Data.URI.Common (decodePCT, joinWith, parsePCTEncoded, parseSubDelims, parseUnreserved)
import Global (encodeURI)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser UserInfo
parser = UserInfo <<< joinWith "" <$> many1 p
  where
  p = parseUnreserved
    <|> parsePCTEncoded decodePCT
    <|> parseSubDelims
    <|> string ":"

print ∷ UserInfo → String
print (UserInfo u) = encodeURI u
