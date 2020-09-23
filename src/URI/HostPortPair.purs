module URI.HostPortPair where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import URI.Common (URIPartParseError, wrapParser)
import URI.Host (Host)
import URI.Host as Host
import URI.Port (Port)
import URI.Port as Port

-- | A spec-conformant host/port pair (may also be empty).
-- | For example: `purescript.org`, `localhost:3000`, `:9000`.
type HostPortPair host port = Maybe (These host port)

-- | A parser for a spec-conformant host/port pair.
-- |
-- | This function allows for the `Host` and `Port` components to be parsed into
-- | custom representations. If this is not necessary, use `pure` for both of
-- | these arguments.
-- |
-- | Host parsing is dealt with a little differently to all the other URI
-- | components, as for hosts the control is passed entirely to the component
-- | parser. This is to accomodate multi-host URIs that are used sometimes for
-- | connection strings and the like, but as these are not spec-conforming this
-- | part of parsing may need to bend the rules a little. See
-- | [`URI.Extra.MultiHostPortPair`](../URI.Extra.MultiHostPortPair) for an
-- | example of this.
parser
  ∷ ∀ host port
  . (Host → Either URIPartParseError host)
  → (Port → Either URIPartParseError port)
  → Parser String (HostPortPair host port)
parser parseHost parsePort = do
  mh ← optionMaybe (wrapParser parseHost Host.parser)
  mp ← optionMaybe (wrapParser parsePort Port.parser)
  pure case mh, mp of
    Just h, Nothing → Just (This h)
    Nothing, Just p → Just (That p)
    Just h, Just p → Just (Both h p)
    Nothing, Nothing → Nothing

-- | A printer for a spec-conformant host/port pair.
-- |
-- | As a counterpart to the `parser` this function also requires the `Host`
-- | and `Port` components to be printed back from their custom representations.
-- | If no custom types are being used, pass `identity` for both of these arguments.
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
