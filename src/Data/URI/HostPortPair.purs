module Data.URI.HostPortPair where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Data.URI.Common (URIPartParseError)
import Data.URI.Host (Host)
import Data.URI.Host as Host
import Data.URI.Port (Port)
import Data.URI.Port as Port
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)

type HostPortPair host port = Maybe (These host port)

parser
  ∷ ∀ host port
  . (Host → Either URIPartParseError host)
  → (Port → Either URIPartParseError port)
  → Parser String (HostPortPair host port)
parser parseHost parsePort = do
  mh ← optionMaybe (Host.parser parseHost)
  mp ← optionMaybe (Port.parser parsePort)
  pure case mh, mp of
    Just h, Nothing → Just (This h)
    Nothing, Just p → Just (That p)
    Just h, Just p → Just (Both h p)
    Nothing, Nothing → Nothing

print
  ∷ ∀ host port
  . (host → Host)
  → (port → Port)
  → HostPortPair host port
  → String
print printHost printPort = case _ of
  Nothing →
    ""
  Just (This host) →
    Host.print (printHost host)
  Just (That port) →
    Port.print (printPort port)
  Just (Both host port) →
    Host.print (printHost host) <> Port.print (printPort port)
