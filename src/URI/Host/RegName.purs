module URI.Host.RegName
  ( RegName
  , fromString
  , toString
  , unsafeFromString
  , unsafeToString
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Text.Parsing.Parser (Parser)
import URI.Common (decodeURIComponent', subDelims, unreserved, pctEncoded, printEncoded')

-- | The reg-name variation of the host part of a URI. A reg-name is probably
-- | more commonly referred to as just a host name or domain name (but it is
-- | actually a name, rather than an IP address).
newtype RegName = RegName NonEmptyString

derive newtype instance eqRegName ∷ Eq RegName
derive newtype instance ordRegName ∷ Ord RegName
derive newtype instance semigroupRegName ∷ Semigroup RegName

instance showRegName ∷ Show RegName where
  show (RegName s) = "(RegName.unsafeFromString " <> show s <> ")"

-- | Constructs a reg-name value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeFromString` instead.
-- |
-- | ``` purescript
-- | fromString "foo.com" = unsafeFromString "foo.com"
-- | fromString "foo:bar" = unsafeFromString "foo%3Abar"
-- | fromString "foo%3Abar" = unsafeFromString "foo%253Abar"
-- | ```
fromString ∷ NonEmptyString → RegName
fromString = RegName <<< printEncoded' regNameChar

-- | Returns the string value for a reg-name, percent-decoding any characters
-- | that require it.
-- |
-- | ``` purescript
-- | toString (unsafeFromString "foo.com") = "foo.com"
-- | toString (unsafeFromString "foo%3Abar") = "foo:bar"
-- | ```
toString ∷ RegName → NonEmptyString
toString (RegName s) = decodeURIComponent' s

-- | Constructs a query value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the query, to prevent double-encoding.
unsafeFromString ∷ NonEmptyString → RegName
unsafeFromString = RegName

-- | Returns the string value for the reg-name without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `fromString`/`unsafeFromString`
-- | pairing.
unsafeToString ∷ RegName → NonEmptyString
unsafeToString (RegName s) = s

-- | A parser for reg-names.
parser ∷ Parser String RegName
parser = RegName <<< NES.join1With "" <$> NEA.some p
  where
  p = pctEncoded <|> NES.singleton <$> regNameChar

-- | A printer for reg-names.
print ∷ RegName → String
print = NES.toString <<< unsafeToString

-- | The supported reg-name characters, excluding percent-encodings.
regNameChar ∷ Parser String Char
regNameChar = unreserved <|> subDelims
