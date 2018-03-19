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
import Data.String.NonEmpty as NES
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (subDelims, unreserved, pctEncoded, printEncoded)

-- | The query component of a URI.
-- |
-- | This type treats the entire string as an undifferentiated blob, if you
-- | would like to deal with the common `?key1=value1&key2=value2` format, take
-- | a look at `URI.Extra.QueryPairs`.
newtype Query = Query String

derive newtype instance eqQuery ∷ Eq Query
derive newtype instance ordQuery ∷ Ord Query
derive newtype instance semigroupQuery ∷ Semigroup Query
derive newtype instance monoidQuery ∷ Monoid Query

instance showQuery ∷ Show Query where
  show (Query s) = "(Query.unsafeFromString " <> show s <> ")"

-- | Constructs a query value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeFromString` instead.
-- |
-- | ``` purescript
-- | fromString "foo" = unsafeFromString "foo"
-- | fromString "foo#bar" = unsafeFromString "foo%23bar"
-- | fromString "foo%23bar" = unsafeFromString "foo%2523bar"
-- | ```
fromString ∷ String → Query
fromString = Query <<< printEncoded queryChar

-- | Returns the string value for a query, percent-decoding any characters
-- | that require it.
-- |
-- | ``` purescript
-- | toString (unsafeFromString "foo") = "foo"
-- | toString (unsafeFromString "foo%23bar") = "foo#bar"
-- | ```
toString ∷ Query → String
toString (Query s) = decodeURIComponent s

-- | Constructs a query value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the query, to prevent double-encoding.
unsafeFromString ∷ String → Query
unsafeFromString = Query

-- | Returns the string value for a query without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `fromString`/`unsafeFromString`
-- | pairing.
unsafeToString ∷ Query → String
unsafeToString (Query s) = s

-- | A parser for the query component of a URI. Expects values with a `'?'`
-- | prefix.
parser ∷ Parser String Query
parser =
  char '?' *>
    (Query <<< NES.joinWith ""
      <$> Array.many (NES.singleton <$> queryChar <|> pctEncoded))

-- | A printer for the query component of a URI. Will print the value with
-- | a `'?'` prefix.
print ∷ Query → String
print (Query s) = "?" <> s

-- | The supported query characters, excluding percent-encodings.
queryChar ∷ Parser String Char
queryChar
  = unreserved
  <|> subDelims
  <|> char ':'
  <|> char '@'
  <|> char '/'
  <|> char '?'
