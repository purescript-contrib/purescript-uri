module Data.URI.Host
  ( module Data.URI.Host
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.URI.Common (parseSubDelims, parsePCTEncoded, parseUnreserved, joinWith, rxPat)
import Data.URI.Types (Host(..))
import Text.Parsing.StringParser (Parser, try, fail)
import Text.Parsing.StringParser.Combinators ((<?>), many1)
import Text.Parsing.StringParser.String (string, char)

parseHost ∷ Parser Host
parseHost = parseIPv6Address <|> parseIPv4Address <|> parseRegName

-- TODO: this is much too forgiving right now
parseIPv6Address ∷ Parser Host
parseIPv6Address = IPv6Address <$> (string "[" *> rxPat "[a-f0-9\\.:]+" <* string "]") <?> "IPv6 address"

parseIPv4Address ∷ Parser Host
parseIPv4Address = IPv4Address <$> parse <?> "IPv4 address"
  where
  parse ∷ Parser String
  parse = do
    o1 <- octet
    _ <- char '.'
    o2 <- octet
    _ <- char '.'
    o3 <- octet
    _ <- char '.'
    o4 <- octet
    pure $ show o1 <> "." <> show o2 <> "." <> show o3 <> "." <> show o4
  octet ∷ Parser Int
  octet = do
    s <- rxPat "[0-9]{1,3}"
    case Int.fromString s of
      Just n | n >= 0 && n <= 255 -> pure n
      _ -> fail "Invalid IPv4 address octet"

parseRegName ∷ Parser Host
parseRegName =
  NameAddress
    <$> try (joinWith ""
      <$> many1 (parseUnreserved <|> parsePCTEncoded <|> parseSubDelims))

printHost ∷ Host → String
printHost (IPv6Address i) = "[" <> i <> "]"
printHost (IPv4Address i) = i
printHost (NameAddress i) = i
