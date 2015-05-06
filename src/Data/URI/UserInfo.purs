module Data.URI.UserInfo where

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Data.String (joinWith)
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser (Parser(), try)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

parseUserInfo :: Parser UserInfo
parseUserInfo = try ((joinWith "" <$> many1 (parseUnreserved
                                         <|> parsePCTEncoded
                                         <|> parseSubDelims
                                         <|> string ":")) <* string "@")
