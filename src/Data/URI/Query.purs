module Data.URI.Query
  ( parseQuery
  , printQuery
  ) where

import Prelude
import Control.Alt
import Control.Apply
import qualified Data.String as S
import Data.StrMap (StrMap(), fromList, toList)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

parseQuery :: Parser Query
parseQuery = Query <$> (wrapParser parseParts $
  try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?")))

parseParts :: Parser (StrMap (Maybe String))
parseParts = fromList <$> sepBy parsePart (string ";" <|> string "&")

parsePart :: Parser (Tuple String (Maybe String))
parsePart = do
  key <- rxPat "[^=]+"
  value <- optionMaybe $ string "=" *> rxPat "[^;&]*"
  return $ Tuple key value

printQuery :: Query -> String
printQuery (Query m) = "?" ++ joinWith "&" (printPart <$> toList m)
  where
  printPart :: Tuple String (Maybe String) -> String
  printPart (Tuple k Nothing) = k
  printPart (Tuple k (Just v)) = k ++ "=" ++ v
