module Data.URI.Host where

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Array (length)
import Data.String (joinWith)
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser (Parser(), try)
import Text.Parsing.StringParser.Combinators ((<?>), many1, sepBy1)
import Text.Parsing.StringParser.String (string)
import qualified Data.Array.Unsafe as U

parseHost :: Parser Host
parseHost = do
  hosts <- sepBy1 (parseIPv6Address <|> parseIPv4Address <|> parseRegName) (string ",")
  return $ if length hosts > 1
           then MultipleHosts hosts
           else U.head hosts

-- TODO: this is much too forgiving right now
parseIPv6Address :: Parser Host
parseIPv6Address = IPv6Address <$> (string "[" *> rxPat "[a-f0-9\\.:]+" <* string "]") <?> "IPv6 address"

parseIPv4Address :: Parser Host
parseIPv4Address = IPv4Address <$> rxPat pattern <?> "IPv4 address"
  where
  pattern :: String
  pattern = joinWith "" ["(", octet, "\\.", octet, "\\.", octet, "\\.", octet, ")"]
  octet :: String
  octet = "([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])"

parseRegName :: Parser Host
parseRegName = NameAddress <$> try (joinWith "" <$> many1 (parseUnreserved <|> parsePCTEncoded <|> parseSubDelims))
