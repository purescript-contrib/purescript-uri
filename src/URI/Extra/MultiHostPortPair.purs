module URI.Extra.MultiHostPortPair
  ( MultiHostPortPair
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty as NES
import Data.These (These(..))
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy, try)
import Text.Parsing.Parser.String (char, oneOf)
import URI.Common (URIPartParseError, unreserved, pctEncoded, wrapParser)
import URI.Host (Host(..), RegName)
import URI.Host.IPv4Address as IPv4Address
import URI.Host.IPv6Address as IPv6Address
import URI.Host.RegName as RegName
import URI.HostPortPair as HostPortPair
import URI.Port (Port)
import URI.Port as Port

-- | Multi-host/port pairs, where host & port combinations can be separated by
-- | `,`, as used by some connection URI schemes. This is not strictly
-- | compatible with RFC 3986, as in that spec `RegName`s can contain `,`, and
-- | only one port can be specified in the authority.
type MultiHostPortPair host port = Array (These host port)

parser
  ∷ ∀ host port
  . (Host → Either URIPartParseError host)
  → (Port → Either URIPartParseError port)
  → Parser String (MultiHostPortPair host port)
parser parseHost parsePort =
  Array.fromFoldable <$> sepBy (parsePair parseHost parsePort) (char ',')

parsePair
  ∷ ∀ host port
  . (Host → Either URIPartParseError host)
  → (Port → Either URIPartParseError port)
  → Parser String (These host port)
parsePair parseHost parsePort = do
  mh ← optionMaybe (parseHost' parseHost)
  mp ← optionMaybe (wrapParser parsePort Port.parser)
  case mh, mp of
    Just h, Nothing → pure (This h)
    Nothing, Just p → pure (That p)
    Just h, Just p → pure (Both h p)
    Nothing, Nothing → fail "Neither host nor port present"

parseHost' ∷ ∀ h. (Host → Either URIPartParseError h) → Parser String h
parseHost' p = wrapParser p
  $ (IPv6Address <$> IPv6Address.parser)
  <|> try (IPv4Address <$> IPv4Address.parser)
  <|> (NameAddress <$> parseRegName')

parseRegName' ∷ Parser String RegName
parseRegName' = do
  n ← Array.some p
  case NES.fromString $ String.joinWith "" n of
    Just x → pure $ RegName.unsafeFromString x
    Nothing → unsafeCrashWith "This must be unPathSegment.unsafeSegmentNZFromStringreachable as we shuold parse at least one char in `pctEncoded`"
  where
  p = pctEncoded <|> String.singleton <$> c
  c = unreserved <|> oneOf ['!', '$', '&', '\'', '(', ')', '*', '+', ';', '=']

print
  ∷ ∀ host port
  . (host → Host)
  → (port → Port)
  → MultiHostPortPair host port
  → String
print printHost printPort =
  String.joinWith "," <<< map (HostPortPair.print printHost printPort <<< Just)
