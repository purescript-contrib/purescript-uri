module Data.URI.Query
  ( Query(..)
  , parser
  , parser'
  , print
  , print'
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Tuple (Tuple(..))
import Data.URI.Common (joinWith, rxPat, wrapParser)
import Global (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy)
import Text.Parsing.StringParser.String (string)

-- | The query component of a URI.
newtype Query = Query (List (Tuple String (Maybe String)))

derive newtype instance eqQuery ∷ Eq Query
derive newtype instance ordQuery ∷ Ord Query
derive instance genericQuery ∷ Generic Query _
derive instance newtypeQuery ∷ Newtype Query _
instance showQuery ∷ Show Query where show = genericShow
derive newtype instance semigroupQuery ∷ Semigroup Query
derive newtype instance monoidQuery ∷ Monoid Query

parser' ∷ ∀ q. Parser q → Parser q
parser' parseQ = string "?" *> (wrapParser parseQ (try (rxPat "[^#]*")))

parser ∷ Parser Query
parser = Query <$> parseParts

parseParts ∷ Parser (List (Tuple String (Maybe String)))
parseParts = sepBy parsePart (string ";" <|> string "&")

parsePart ∷ Parser (Tuple String (Maybe String))
parsePart = do
  key ← decodeURIComponent <$> rxPat "[^=;&]+"
  value ← optionMaybe $ decodeURIComponent <$> (string "=" *> rxPat "[^;&]*")
  pure $ Tuple key value

print' ∷ ∀ q. (q → String) → q → String
print' printQ q = "?" <> printQ q

print ∷ Query → String
print (Query m) = joinWith "&" (printPart <$> m)
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
rxPrintable = unsafePartial fromRight $ RX.regex "[$+/?:@]" RXF.global
