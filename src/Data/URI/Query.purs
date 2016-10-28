module Data.URI.Query
  ( parseQuery
  , printQuery
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Either (fromRight)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Tuple (Tuple(..))
import Data.URI.Common (joinWith, rxPat, parsePChar, wrapParser)
import Data.URI.Types (Query(..))

import Global (encodeURIComponent, decodeURIComponent)

import Partial.Unsafe (unsafePartial)

import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy, many)
import Text.Parsing.StringParser.String (string)

parseQuery ∷ Parser Query
parseQuery = Query <$> (wrapParser parseParts $
  try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?")))

parseParts ∷ Parser (List (Tuple String (Maybe String)))
parseParts = sepBy parsePart (string ";" <|> string "&")

parsePart ∷ Parser (Tuple String (Maybe String))
parsePart = do
  key ← rxPat "[^=;&]+"
  value ← optionMaybe $ string "=" *> rxPat "[^;&]*"
  pure $ Tuple (prettyDecodeURI key) (prettyDecodeURI <$> value)

printQuery ∷ Query → String
printQuery (Query m) =
  case m of
    Nil → ""
    items → "?" <> joinWith "&" (printPart <$> items)
  where
  printPart ∷ Tuple String (Maybe String) → String
  printPart (Tuple k Nothing) = prettyEncodeURI k
  printPart (Tuple k (Just v)) = prettyEncodeURI k <> "=" <> prettyEncodeURI v

prettyEncodeURI ∷ String → String
prettyEncodeURI = RX.replace rgxSpace "+" <<< encodeURIComponent

prettyDecodeURI ∷ String → String
prettyDecodeURI = decodeURIComponent <<< RX.replace rgxPlus " "

rgxSpace ∷ RX.Regex
rgxSpace = unsafePartial $ fromRight $ RX.regex "%20" RXF.global

rgxPlus ∷ RX.Regex
rgxPlus = unsafePartial $ fromRight $ RX.regex "\\+" RXF.global
