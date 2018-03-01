module URI.Host.IPv4Address
  ( IPv4Address
  , fromInts
  , unsafeFromInts
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (char, satisfy)
import URI.Common (URIPartParseError(..), digit, wrapParser)

-- | The IPv4 address variation of the host part of a URI.
data IPv4Address = IPv4Address Int Int Int Int

derive instance eqIPv4Address ∷ Eq IPv4Address
derive instance ordIPv4Address ∷ Ord IPv4Address

instance showIPv4Address ∷ Show IPv4Address where
  show (IPv4Address o1 o2 o3 o4) = "(IPv4Address.unsafeFromInts " <> show o1 <> " " <> show o2 <> " " <> show o3 <> " " <> show o4 <> ")"

-- | Constructs a `IPv4Address` part safely: bounds-checks each octet to ensure
-- | it occurs within the range 0-255 (inclusive).
fromInts ∷ Int → Int → Int → Int → Maybe IPv4Address
fromInts o1 o2 o3 o4 =
  IPv4Address <$> check o1 <*> check o2 <*> check o3 <*> check o4
  where
    check ∷ Int → Maybe Int
    check i
      | i >= 0 && i <= 255 = Just i
      | otherwise = Nothing

-- | Constructs a `IPv4Address` part unsafely: if any of the arguments are
-- | outside the allowable bounds, a runtime error will be thrown.
-- |
-- | This is intended as a convenience when describing `IPv4Address`es
-- | statically in PureScript code, in all other cases `fromInts` should be
-- | used.
unsafeFromInts ∷ Int → Int → Int → Int → IPv4Address
unsafeFromInts o1 o2 o3 o4 =
  case fromInts o1 o2 o3 o4 of
    Just addr → addr
    Nothing → unsafeCrashWith "IPv4Address octet was out of range"

-- | A parser for IPv4 addresses.
parser ∷ Parser String IPv4Address
parser = do
  o1 ← octet <* char '.'
  o2 ← octet <* char '.'
  o3 ← octet <* char '.'
  o4 ← octet
  pure $ IPv4Address o1 o2 o3 o4

-- | A printer for IPv4 adddresses.
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
