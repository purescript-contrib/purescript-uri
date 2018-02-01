module Data.URI.Host
  ( Host(..)
  , parser
  , print
  , _IPv6Address
  , _IPv4Address
  , _NameAddress
  , module Data.URI.Host.RegName
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.URI.Common (wrapParser)
import Data.URI.Host.RegName (RegName)
import Data.URI.Host.RegName as RegName
import Text.Parsing.StringParser (ParseError(..), Parser, try)
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.String (anyDigit, char, regex, satisfy, string)

-- | A host address.
data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress RegName

derive instance eqHost ∷ Eq Host
derive instance ordHost ∷ Ord Host
derive instance genericHost ∷ Generic Host _
instance showHost ∷ Show Host where show = genericShow

parser ∷ ∀ h. (Host → Either ParseError h) → Parser h
parser p = wrapParser p
  $ ipv6AddressParser
  <|> try ipv4AddressParser
  <|> (NameAddress <$> RegName.parser)

-- TODO: this is much too forgiving right now
ipv6AddressParser ∷ Parser Host
ipv6AddressParser = IPv6Address <$> (string "[" *> regex "[a-fA-F0-9\\.:]+" <* string "]") <?> "IPv6 address"

ipv4AddressParser ∷ Parser Host
ipv4AddressParser = IPv4Address <$> addr <?> "IPv4 address"
  where
  addr ∷ Parser String
  addr = do
    o1 <- octet
    _ <- char '.'
    o2 <- octet
    _ <- char '.'
    o3 <- octet
    _ <- char '.'
    o4 <- octet
    pure $ show o1 <> "." <> show o2 <> "." <> show o3 <> "." <> show o4
  octet ∷ Parser Int
  octet = wrapParser toInt
    $ try ((\x y z → String.fromCharArray [x, y, z]) <$> nzDigit <*> anyDigit <*> anyDigit)
    <|> try ((\x y → String.fromCharArray [x, y]) <$> nzDigit <*> anyDigit)
    <|> (String.singleton <$> anyDigit)
  nzDigit ∷ Parser Char
  nzDigit = satisfy (\c → c >= '1' && c <= '9')
  toInt ∷ String → Either ParseError Int
  toInt s = case Int.fromString s of
    Just n | n >= 0 && n <= 255 → Right n
    _ → Left (ParseError "Invalid IPv4 address octet")

print ∷ ∀ h. (h → Host) → h → String
print f = f >>> case _ of
  IPv6Address addr → "[" <> addr <> "]"
  IPv4Address addr → addr
  NameAddress addr → RegName.unsafeToString addr

_IPv6Address ∷ Prism' Host String
_IPv6Address = prism' IPv6Address case _ of
  IPv6Address addr → Just addr
  _ → Nothing

_IPv4Address ∷ Prism' Host String
_IPv4Address = prism' IPv4Address case _ of
  IPv4Address addr → Just addr
  _ → Nothing

_NameAddress ∷ Prism' Host RegName
_NameAddress = prism' NameAddress case _ of
  NameAddress addr → Just addr
  _ → Nothing
