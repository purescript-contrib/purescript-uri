module Data.URI.Query
  ( Query
  , fromString
  , toString
  , unsafeFromString
  , unsafeToString
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.String as String
import Data.URI.Common (newParsePCTEncoded, parseSubDelims, parseUnreserved, printEncoded, wrapParser)
import Global (decodeURIComponent)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.String (char, string)

newtype Query = Query String

derive newtype instance eqQuery ∷ Eq Query
derive newtype instance ordQuery ∷ Ord Query
derive newtype instance semigroupQuery ∷ Semigroup Query
derive newtype instance monoidQuery ∷ Monoid Query

instance showQuery ∷ Show Query where
  show (Query s) = "(Query.unsafeFromString " <> show s <> ")"

fromString ∷ String → Query
fromString = Query <<< printEncoded queryChar

toString ∷ Query → String
toString (Query s) = decodeURIComponent s

unsafeFromString ∷ String → Query
unsafeFromString = Query

unsafeToString ∷ Query → String
unsafeToString (Query s) = s

parser ∷ ∀ q. (Query → Either ParseError q) → Parser q
parser parseQ =
  string "?" *>
    wrapParser parseQ (Query <<< String.joinWith "" <$> Array.many p)
  where
    p = String.singleton <$> queryChar <|> newParsePCTEncoded

print ∷ ∀ q. (q → Query) → q → String
print printQ q = "?" <> unsafeToString (printQ q)

queryChar ∷ Parser Char
queryChar
  = parseUnreserved
  <|> parseSubDelims
  <|> char ':'
  <|> char '@'
  <|> char '/'
  <|> char '?'
