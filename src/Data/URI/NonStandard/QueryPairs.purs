module Data.URI.NonStandard.QueryPairs
  ( QueryPairs(..)
  , parse
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either, fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Tuple (Tuple(..))
import Data.URI.Query as Q
import Global (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy)
import Text.Parsing.StringParser.String (regex, string)

newtype QueryPairs = QueryPairs (List (Tuple String (Maybe String)))

derive newtype instance eqQueryPairs ∷ Eq QueryPairs
derive newtype instance ordQueryPairs ∷ Ord QueryPairs
derive instance genericQueryPairs ∷ Generic QueryPairs _
derive instance newtypeQueryPairs ∷ Newtype QueryPairs _
instance showQueryPairs ∷ Show QueryPairs where show = genericShow
derive newtype instance semigroupQueryPairs ∷ Semigroup QueryPairs
derive newtype instance monoidQueryPairs ∷ Monoid QueryPairs

parse ∷ Q.Query → Either ParseError QueryPairs
parse = runParser (QueryPairs <$> parseParts) <<< Q.unsafeToString

parseParts ∷ Parser (List (Tuple String (Maybe String)))
parseParts = sepBy parsePart (string ";" <|> string "&")

parsePart ∷ Parser (Tuple String (Maybe String))
parsePart = do
  key ← decodeURIComponent <$> regex "[^=;&]+"
  value ← optionMaybe $ decodeURIComponent <$> (string "=" *> regex "[^;&]*")
  pure $ Tuple key value

print ∷ QueryPairs → Q.Query
print (QueryPairs m) = Q.unsafeFromString $ String.joinWith "&" $ Array.fromFoldable (printPart <$> m)
  where
  printPart ∷ Tuple String (Maybe String) → String
  printPart (Tuple k Nothing) =
    printQueryPairsPart k
  printPart (Tuple k (Just v)) =
    printQueryPairsPart k <> "=" <> printQueryPairsPart v

printQueryPairsPart ∷ String → String
printQueryPairsPart = String.joinWith "" <<< map printChar <<< String.split (String.Pattern "")
  where
  -- Fragments & queries have a bunch of characters that don't need escaping
  printChar ∷ String → String
  printChar s
    | RX.test rxPrintable s = s
    | otherwise = encodeURIComponent s

rxPrintable ∷ RX.Regex
rxPrintable = unsafePartial fromRight $ RX.regex "[$+/?:@]" RXF.global
