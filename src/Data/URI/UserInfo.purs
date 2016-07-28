module Data.URI.UserInfo
  ( module Data.URI.UserInfo
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.URI.Common (parseSubDelims, parsePCTEncoded, parseUnreserved, joinWith)
import Data.URI.Types (UserInfo)

import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

parseUserInfo ∷ Parser UserInfo
parseUserInfo = try ((joinWith "" <$> many1 p) <* string "@")
  where
  p = parseUnreserved <|> parsePCTEncoded <|> parseSubDelims <|> string ":"
