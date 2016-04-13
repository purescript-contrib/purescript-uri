module Data.URI.Query
  ( parseQuery
  , printQuery
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex as Rgx
import Data.StrMap (StrMap, fromList, toList)
import Data.Tuple (Tuple(..))
import Data.URI.Common (joinWith, rxPat, parsePChar, wrapParser)
import Data.URI.Types (Query(..))

import Global (encodeURIComponent, decodeURIComponent)

import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy, many)
import Text.Parsing.StringParser.String (string)

parseQuery ∷ Parser Query
parseQuery = Query <$> (wrapParser parseParts $
  try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?")))

parseParts ∷ Parser (StrMap (Maybe String))
parseParts = fromList <$> sepBy parsePart (string ";" <|> string "&")

parsePart ∷ Parser (Tuple String (Maybe String))
parsePart = do
  key ← rxPat "[^=]+"
  value ← optionMaybe $ string "=" *> rxPat "[^;&]*"
  return $ Tuple (prettyDecodeURI key) (prettyDecodeURI <$> value)

printQuery ∷ Query → String
printQuery (Query m) =
  case toList m of
    Nil → ""
    items → "?" <> joinWith "&" (printPart <$> items)
  where
  printPart ∷ Tuple String (Maybe String) → String
  printPart (Tuple k Nothing) = prettyEncodeURI k
  printPart (Tuple k (Just v)) = prettyEncodeURI k <> "=" <> prettyEncodeURI v

prettyEncodeURI ∷ String → String
prettyEncodeURI = Rgx.replace rgxSpace "+" <<< encodeURIComponent

prettyDecodeURI ∷ String → String
prettyDecodeURI = decodeURIComponent <<< Rgx.replace rgxPlus " "

rgxSpace ∷ Rgx.Regex
rgxSpace = Rgx.regex "%20" (Rgx.noFlags { global = true })

rgxPlus ∷ Rgx.Regex
rgxPlus = Rgx.regex "\\+" (Rgx.noFlags { global = true })

