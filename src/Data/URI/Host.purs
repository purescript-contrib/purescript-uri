module Data.URI.Host
  ( module Data.URI.Host
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))

import Data.String as S
import Data.URI.Common (parseSubDelims, parsePCTEncoded, parseUnreserved, joinWith, rxPat)
import Data.URI.Types (Host(..))

import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators ((<?>), many1)
import Text.Parsing.StringParser.String (string)

parseHost ∷ Parser Host
parseHost = parseIPv6Address <|> parseIPv4Address <|> parseRegName

-- TODO: this is much too forgiving right now
parseIPv6Address ∷ Parser Host
parseIPv6Address = IPv6Address <$> (string "[" *> rxPat "[a-f0-9\\.:]+" <* string "]") <?> "IPv6 address"

parseIPv4Address ∷ Parser Host
parseIPv4Address = IPv4Address <$> rxPat pattern <?> "IPv4 address"
  where
  pattern ∷ String
  pattern = S.joinWith "" ["(", octet, "\\.", octet, "\\.", octet, "\\.", octet, ")"]
  octet ∷ String
  octet = "(1[0-9]{2}|[1-9][0-9]|[0-9]|2[0-4][0-9]|25[0-5])"

parseRegName ∷ Parser Host
parseRegName =
  NameAddress
    <$> try (joinWith ""
      <$> many1 (parseUnreserved <|> parsePCTEncoded <|> parseSubDelims))

printHost ∷ Host → String
printHost (IPv6Address i) = "[" <> i <> "]"
printHost (IPv4Address i) = i
printHost (NameAddress i) = i
