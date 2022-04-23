module URI.Extra.MultiHostPortPair
  ( MultiHostPortPair
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import Data.String.NonEmpty (join1With) as NES
import Data.These (These(..))
import Parsing (Parser, fail)
import Parsing.Combinators (optionMaybe, sepBy, try)
import Parsing.String (char)
import Parsing.String.Basic (oneOf)
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
-- |
-- | A motivating example for where this may be useful: dealing with mongodb
-- | connection strings.
type MultiHostPortPair host port = Array (These host port)

-- | A parser for multiple host/port pairs embedded in a URI.
-- |
-- | This function allows for the `Host` and `Port` components to be parsed into
-- | custom representations. If this is not necessary, use `pure` for both of
-- | these arguments.
parser
  :: forall host port
   . (Host -> Either URIPartParseError host)
  -> (Port -> Either URIPartParseError port)
  -> Parser String (MultiHostPortPair host port)
parser parseHost parsePort =
  Array.fromFoldable <$> sepBy (parsePair parseHost parsePort) (char ',')

parsePair
  :: forall host port
   . (Host -> Either URIPartParseError host)
  -> (Port -> Either URIPartParseError port)
  -> Parser String (These host port)
parsePair parseHost parsePort = do
  mh <- optionMaybe (parseHost' parseHost)
  mp <- optionMaybe (wrapParser parsePort Port.parser)
  case mh, mp of
    Just h, Nothing -> pure (This h)
    Nothing, Just p -> pure (That p)
    Just h, Just p -> pure (Both h p)
    Nothing, Nothing -> fail "Neither host nor port present"

parseHost' :: forall h. (Host -> Either URIPartParseError h) -> Parser String h
parseHost' p = wrapParser p do
  (IPv6Address <$> IPv6Address.parser)
    <|> try (IPv4Address <$> IPv4Address.parser)
    <|> (NameAddress <$> parseRegName')

parseRegName' :: Parser String RegName
parseRegName' = RegName.unsafeFromString <<< NES.join1With "" <$> NEA.some p
  where
  p = pctEncoded <|> NES.singleton <$> c
  c = unreserved <|> oneOf [ '!', '$', '&', '\'', '(', ')', '*', '+', ';', '=' ]

-- | A printer for multiple host/port pairs embedded in a URI.
-- |
-- | As a counterpart to the `parser` this function also requires the `Host`
-- | and `Port` components to be printed back from their custom representations.
-- | If no custom types are being used, pass `identity` for both of these arguments.
print
  :: forall host port
   . (host -> Host)
  -> (port -> Port)
  -> MultiHostPortPair host port
  -> String
print printHost printPort =
  String.joinWith "," <<< map (HostPortPair.print printHost printPort <<< Just)
