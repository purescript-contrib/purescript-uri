module URI.Host.IPv6Address
  ( IPv6Address
  , unsafeFromString
  , unsafeToString
  , parser
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.List as List
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.CodeUnits as NESCU
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (hexDigit)

-- | This type and parser are much too forgiving currently, allowing almost
-- | anything through that looks vaguely IPv6ish.
newtype IPv6Address = IPv6Address String

derive newtype instance eqIPv6Address :: Eq IPv6Address
derive newtype instance ordIPv6Address :: Ord IPv6Address

instance showIPv6Address :: Show IPv6Address where
  show (IPv6Address s) = "(IPv6Address.unsafeFromString " <> show s <> ")"

unsafeFromString :: String -> IPv6Address
unsafeFromString = IPv6Address

unsafeToString :: IPv6Address -> String
unsafeToString (IPv6Address s) = "[" <> s <> "]"

parser :: Parser String IPv6Address
parser =
  IPv6Address
    <$> (char '[' *> (NES.joinWith "" <$> List.someRec (NESCU.singleton <$> ipv6Char)) <* char ']')
    <?> "IPv6 address"
  where
  ipv6Char :: Parser String Char
  ipv6Char = hexDigit <|> char ':' <|> char '.'
