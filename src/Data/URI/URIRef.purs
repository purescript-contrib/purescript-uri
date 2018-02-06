module Data.URI.URIRef
  ( URIRef
  , URIRefOptions
  , URIRefParseOptions
  , URIRefPrintOptions
  , parser
  , print
  , module Data.URI.Authority
  , module Data.URI.Fragment
  , module Data.URI.Host
  , module Data.URI.Path
  , module Data.URI.Path.Absolute
  , module Data.URI.Path.NoScheme
  , module Data.URI.Path.Rootless
  , module Data.URI.Port
  , module Data.URI.Query
  , module Data.URI.RelativeRef
  , module Data.URI.Scheme
  , module Data.URI.URI
  , module Data.URI.UserInfo
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.URI.Authority (Authority(..), HostsParseOptions)
import Data.URI.Common (URIPartParseError)
import Data.URI.Fragment (Fragment)
import Data.URI.Host (Host(..), IPv4Address, IPv6Address, RegName, _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Path (Path(..))
import Data.URI.Path.Absolute (PathAbsolute(..))
import Data.URI.Path.NoScheme (PathNoScheme(..))
import Data.URI.Path.Rootless (PathRootless(..))
import Data.URI.Port (Port)
import Data.URI.Query (Query)
import Data.URI.RelativeRef (RelativeRef(..), RelativePart(..), RelPath)
import Data.URI.RelativeRef as RelativeRef
import Data.URI.Scheme (Scheme)
import Data.URI.URI (URI(..), HierarchicalPart(..), HierPath)
import Data.URI.URI as URI
import Data.URI.UserInfo (UserInfo)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)

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
