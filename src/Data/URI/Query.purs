module Data.URI.Query (parser, print) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (fromRight)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Tuple (Tuple(..))
import Data.URI (Query(..))
import Data.URI.Common (joinWith, rxPat, wrapParser)
import Global (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser Query
parser = string "?" *> (Query <$> wrapParser parseParts (try (rxPat "[^#]*")))

parseParts ∷ Parser (List (Tuple String (Maybe String)))
parseParts = sepBy parsePart (string ";" <|> string "&")

parsePart ∷ Parser (Tuple String (Maybe String))
parsePart = do
  key ← decodeURIComponent <$> rxPat "[^=;&]+"
  value ← optionMaybe $ decodeURIComponent <$> (string "=" *> rxPat "[^;&]*")
  pure $ Tuple key value

print ∷ Query → String
print (Query m) =
  case m of
    Nil → "?"
    items → "?" <> joinWith "&" (printPart <$> items)
  where
  printPart ∷ Tuple String (Maybe String) → String
  printPart (Tuple k Nothing) =
    printQueryPart k
  printPart (Tuple k (Just v)) =
    printQueryPart k <> "=" <> printQueryPart v

printQueryPart ∷ String → String
printQueryPart = S.joinWith "" <<< map printChar <<< S.split (S.Pattern "")
  where
  -- Fragments & queries have a bunch of characters that don't need escaping
  printChar ∷ String → String
  printChar s
    | RX.test rxPrintable s = s
    | otherwise = encodeURIComponent s

rxPrintable ∷ RX.Regex
rxPrintable = unsafePartial fromRight $ RX.regex "[$+=/?:@]" RXF.global
