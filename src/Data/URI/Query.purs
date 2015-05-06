module Data.URI.Query (parseQuery) where

import Control.Alt
import Control.Apply
import Data.String
import Data.StrMap (StrMap(), fromList)
import Data.Tuple (Tuple(..))
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

parseQuery :: Parser Query
parseQuery = Query <$> (wrapParser parseParts $
  try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?")))

parseParts :: Parser (StrMap String)
parseParts = fromList <$> sepBy parsePart (string ";" <|> string "&")

parsePart :: Parser (Tuple String String)
parsePart = do
  key <- rxPat "[^=]+"
  string "="
  value <- rxPat "[^;&]*"
  return $ Tuple key value
