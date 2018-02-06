module Data.URI.Host.RegName
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
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String as String
import Data.URI.Common (pctEncoded, parseSubDelims, parseUnreserved, printEncoded)
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)

newtype RegName = RegName String

derive newtype instance eqRegName ∷ Eq RegName
derive newtype instance ordRegName ∷ Ord RegName
derive newtype instance semigroupRegName ∷ Semigroup RegName
derive newtype instance monoidRegName ∷ Monoid RegName

instance showRegName ∷ Show RegName where
  show (RegName s) = "(RegName.unsafeFromString " <> show s <> ")"

-- | Constructs a `RegName` part safely: percent-encoding will be
-- | applied to any character that requires it for the user-info component of a
-- | URI.
fromString ∷ String → Maybe RegName
fromString = case _ of
  "" → Nothing
  s → Just $ RegName (printEncoded regNameChar s)

-- | Prints `RegName` as a string, decoding any percent-encoded
-- | characters contained within.
toString ∷ RegName → String
toString (RegName s) = decodeURIComponent s

-- | Constructs a `RegName` part unsafely: no encoding will be applied
-- | to the value. If an incorrect value is provided, the URI will be invalid
-- | when printed back.
unsafeFromString ∷ String → RegName
unsafeFromString = RegName

-- | Prints `RegName` as a string without performing any decoding of
-- | percent-encoded octets. Only "unsafe" in the sense that values this
-- | produces will need further decoding, the name is more for symmetry with
-- | the `fromString`/`toString` and `unsafeFromString`/`unsafeToString`
-- | pairings.
unsafeToString ∷ RegName → String
unsafeToString (RegName s) = s

parser ∷ Parser String RegName
parser = RegName <<< String.joinWith "" <$> Array.some p
  where
  p = pctEncoded <|> String.singleton <$> regNameChar

regNameChar ∷ Parser String Char
regNameChar = parseUnreserved <|> parseSubDelims
