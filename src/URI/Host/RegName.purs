module URI.Host.RegName
  ( RegName
  , fromString
  , toString
  , unsafeFromString
  , unsafeToString
  , parser
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import URI.Common (decodeURIComponent', parseSubDelims, parseUnreserved, pctEncoded, printEncoded')

newtype RegName = RegName NonEmptyString

derive newtype instance eqRegName ∷ Eq RegName
derive newtype instance ordRegName ∷ Ord RegName
derive newtype instance semigroupRegName ∷ Semigroup RegName

instance showRegName ∷ Show RegName where
  show (RegName s) = "(RegName.unsafeFromString " <> show s <> ")"

-- | Constructs a `RegName` part safely: percent-encoding will be
-- | applied to any character that requires it for the user-info component of a
-- | URI.
fromString ∷ NonEmptyString → RegName
fromString = RegName <<< printEncoded' regNameChar

-- | Prints `RegName` as a string, decoding any percent-encoded
-- | characters contained within.
toString ∷ RegName → NonEmptyString
toString (RegName s) = decodeURIComponent' s

-- | Constructs a `RegName` part unsafely: no encoding will be applied
-- | to the value. If an incorrect value is provided, the URI will be invalid
-- | when printed back.
unsafeFromString ∷ NonEmptyString → RegName
unsafeFromString = RegName

-- | Prints `RegName` as a string without performing any decoding of
-- | percent-encoded octets. Only "unsafe" in the sense that values this
-- | produces will need further decoding, the name is more for symmetry with
-- | the `fromString`/`toString` and `unsafeFromString`/`unsafeToString`
-- | pairings.
unsafeToString ∷ RegName → NonEmptyString
unsafeToString (RegName s) = s

parser ∷ Parser String RegName
parser =
  RegName
    <<< unsafePartial NES.unsafeFromString
    <<< String.joinWith "" <$> Array.some p
  where
  p = pctEncoded <|> String.singleton <$> regNameChar

regNameChar ∷ Parser String Char
regNameChar = parseUnreserved <|> parseSubDelims
