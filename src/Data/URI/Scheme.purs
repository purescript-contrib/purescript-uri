module Data.URI.Scheme where

import Data.Maybe (Maybe())
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser (Parser())
import Text.Parsing.StringParser.Combinators (optionMaybe)

parseScheme :: Parser (Maybe URIScheme)
parseScheme = optionMaybe (URIScheme <$> rxPat "[a-z][a-z0-9+\\.\\-]+")

printScheme :: URIScheme -> String
printScheme (URIScheme s) = s ++ ":"
