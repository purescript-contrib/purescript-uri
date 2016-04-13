module Data.URI.Fragment
  ( module Data.URI.Fragment
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.URI.Common (parsePChar, joinWith)
import Data.URI.Types (Fragment)

import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (string)

parseFragment ∷ Parser Fragment
parseFragment =
  try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?"))
