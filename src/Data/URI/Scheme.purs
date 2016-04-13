module Data.URI.Scheme
  ( module Data.URI.Scheme
  , module Data.URI.Types
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.URI.Common (rxPat)
import Data.URI.Types (URIScheme(..))

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)

parseScheme ∷ Parser (Maybe URIScheme)
parseScheme = optionMaybe (URIScheme <$> rxPat "[a-z][a-z0-9+\\.\\-]+")

printScheme ∷ URIScheme → String
printScheme (URIScheme s) = s ++ ":"
