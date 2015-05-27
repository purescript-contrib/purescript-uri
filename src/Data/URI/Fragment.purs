module Data.URI.Fragment where

import Control.Alt ((<|>))
import Data.String (joinWith)
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser (Parser(), try)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (string)

parseFragment :: Parser Fragment
parseFragment = try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?"))
