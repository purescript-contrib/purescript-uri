module URI.URIRef
  ( URIRef
  , URIRefOptions
  , URIRefParseOptions
  , URIRefPrintOptions
  , parser
  , print
  , module URI.Authority
  , module URI.Fragment
  , module URI.Host
  , module URI.Path
  , module URI.Path.Absolute
  , module URI.Path.NoScheme
  , module URI.Path.Rootless
  , module URI.Port
  , module URI.Query
  , module URI.RelativeRef
  , module URI.Scheme
  , module URI.URI
  , module URI.UserInfo
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import URI.Authority (Authority(..), HostsParseOptions)
import URI.Common (URIPartParseError)
import URI.Fragment (Fragment)
import URI.Host (Host(..), IPv4Address, IPv6Address, RegName, _IPv4Address, _IPv6Address, _NameAddress)
import URI.Path (Path(..))
import URI.Path.Absolute (PathAbsolute(..))
import URI.Path.NoScheme (PathNoScheme(..))
import URI.Path.Rootless (PathRootless(..))
import URI.Port (Port)
import URI.Query (Query)
import URI.RelativeRef (RelativeRef(..), RelativePart(..), RelPath)
import URI.RelativeRef as RelativeRef
import URI.Scheme (Scheme)
import URI.URI (URI(..), HierarchicalPart(..), HierPath)
import URI.URI as URI
import URI.UserInfo (UserInfo)

-- | An alias for the most common use case of resource identifiers.
type URIRef userInfo hosts path hierPath relPath query fragment =
  Either
    (URI.URI userInfo hosts path hierPath query fragment)
    (RelativeRef.RelativeRef userInfo hosts path relPath query fragment)

type URIRefOptions userInfo hosts path hierPath relPath query fragment =
  URIRefParseOptions userInfo hosts path hierPath relPath query fragment
    (URIRefPrintOptions userInfo hosts path hierPath relPath query fragment ())

type URIRefParseOptions userInfo hosts path hierPath relPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseHierPath ∷ Either PathAbsolute PathRootless → Either URIPartParseError hierPath
  , parseRelPath ∷ Either PathAbsolute PathNoScheme → Either URIPartParseError relPath
  , parseQuery ∷ Query → Either URIPartParseError query
  , parseFragment ∷ Fragment → Either URIPartParseError fragment
  | r
  )

type URIRefPrintOptions userInfo hosts path hierPath relPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → Either PathAbsolute PathRootless
  , printRelPath ∷ relPath → Either PathAbsolute PathNoScheme
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

parser
  ∷ ∀ userInfo hosts path hierPath relPath query fragment r
  . Record (URIRefParseOptions userInfo hosts path hierPath relPath query fragment r)
  → Parser String (URIRef userInfo hosts path hierPath relPath query fragment)
parser opts
  = (Left <$> try (URI.parser opts))
  <|> (Right <$> RelativeRef.parser opts)

print
  ∷ ∀ userInfo hosts path hierPath relPath query fragment r
  . Record (URIRefPrintOptions userInfo hosts path hierPath relPath query fragment r)
  → URIRef userInfo hosts path hierPath relPath query fragment
  → String
print opts =
  either
    (URI.print opts)
    (RelativeRef.print opts)
