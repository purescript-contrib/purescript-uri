module URI.Query
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
import Data.Monoid (class Monoid)
import Data.String as String
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (parseSubDelims, parseUnreserved, pctEncoded, printEncoded)

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

parser ∷ Parser String Query
parser =
  char '?' *>
    (Query <<< String.joinWith ""
      <$> Array.many (String.singleton <$> queryChar <|> pctEncoded))

print ∷ Query → String
print (Query s) = "?" <> s

queryChar ∷ Parser String Char
queryChar
  = parseUnreserved
  <|> parseSubDelims
  <|> char ':'
  <|> char '@'
  <|> char '/'
  <|> char '?'
