module Data.URI.Host.IPv4Address
  ( IPv4Address
  , fromOctets
  , unsafeFromOctets
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.URI.Common (URIPartParseError(..), digit, wrapParser)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (char, satisfy)

data IPv4Address = IPv4Address Int Int Int Int

derive instance eqIPv4Address ∷ Eq IPv4Address
derive instance ordIPv4Address ∷ Ord IPv4Address

instance showIPv4Address ∷ Show IPv4Address where
  show (IPv4Address o1 o2 o3 o4) = "(IPv4Address.unsafeFromOctets " <> show o1 <> " " <> show o2 <> " " <> show o3 <> " " <> show o4 <> ")"

-- | Constructs a `IPv4Address` part safely: bounds-checks each octet to ensure
-- | it occurs within the range 0-255 (inclusive).
fromOctets ∷ Int → Int → Int → Int → Maybe IPv4Address
fromOctets o1 o2 o3 o4 =
  IPv4Address <$> check o1 <*> check o2 <*> check o3 <*> check o4
  where
    check ∷ Int → Maybe Int
    check i
      | i >= 0 && i <= 255 = Just i
      | otherwise = Nothing

-- | Constructs a `IPv4Address` part unsafely: no bounds-checking will be used
-- | on the passed integers, potentially allowing for invalid IP addresses.
unsafeFromOctets ∷ Int → Int → Int → Int → IPv4Address
unsafeFromOctets = IPv4Address

parser ∷ Parser String IPv4Address
parser = do
  o1 ← octet <* char '.'
  o2 ← octet <* char '.'
  o3 ← octet <* char '.'
  o4 ← octet
  pure $ IPv4Address o1 o2 o3 o4

print ∷ IPv4Address → String
print (IPv4Address o1 o2 o3 o4) =
  show o1 <> "." <> show o2 <> "." <> show o3 <> "." <> show o4

octet ∷ Parser String Int
octet = wrapParser toInt
  $ try ((\x y z → String.fromCharArray [x, y, z]) <$> nzDigit <*> digit <*> digit)
  <|> try ((\x y → String.fromCharArray [x, y]) <$> nzDigit <*> digit)
  <|> (String.singleton <$> digit)

nzDigit ∷ Parser String Char
nzDigit = satisfy (\c → c >= '1' && c <= '9')

toInt ∷ String → Either URIPartParseError Int
toInt s = case Int.fromString s of
  Just n | n >= 0 && n <= 255 → Right n
  _ → Left (URIPartParseError "Invalid IPv4 address octet")
