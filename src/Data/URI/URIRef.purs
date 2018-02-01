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
  , module Data.URI.Port
  , module Data.URI.Query
  , module Data.URI.UserInfo
  , module Data.URI.RelativeRef
  , module Data.URI.URI
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.URI.Authority (Authority(..))
import Data.URI.Fragment (Fragment)
import Data.URI.Host (Host(..), RegName, _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Port (Port(..))
import Data.URI.Query (Query)
import Data.URI.RelativeRef (RelativeRef(..), RelativePart(..))
import Data.URI.RelativeRef as RelativeRef
import Data.URI.URI (URI(..), HierarchicalPart(..))
import Data.URI.URI as URI
import Data.URI.UserInfo (UserInfo)
import Text.Parsing.StringParser (ParseError, Parser, try)

-- | An alias for the most common use case of resource identifiers.
type URIRef userInfo hosts host port hierPath relPath query fragment =
  Either
    (URI.URI userInfo hosts host port hierPath query fragment)
    (RelativeRef.RelativeRef userInfo hosts host port relPath query fragment)

type URIRefOptions userInfo hosts host port hierPath relPath query fragment =
  URIRefParseOptions userInfo hosts host port hierPath relPath query fragment
    (URIRefPrintOptions userInfo hosts host port hierPath relPath query fragment ())

type URIRefParseOptions userInfo hosts host port hierPath relPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseHost ∷ Host → Either ParseError host
  , parsePort ∷ Port → Either ParseError port
  , parseRelPath ∷ String → Either ParseError relPath
  , parseHierPath ∷ String → Either ParseError hierPath
  , parseQuery ∷ Query → Either ParseError query
  , parseFragment ∷ Fragment → Either ParseError fragment
  | r
  )

type URIRefPrintOptions userInfo hosts host port hierPath relPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts String → String
  , printHost ∷ host → Host
  , printPort ∷ port → Port
  , printRelPath ∷ relPath → String
  , printHierPath ∷ hierPath → String
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

parser
  ∷ ∀ userInfo hosts host port hierPath relPath query fragment r
  . Record (URIRefParseOptions userInfo hosts host port hierPath relPath query fragment r)
  → Parser (URIRef userInfo hosts host port hierPath relPath query fragment)
parser opts
  = (Left <$> try (URI.parser opts))
  <|> (Right <$> RelativeRef.parser opts)

print
  ∷ ∀ userInfo hosts host port hierPath relPath query fragment r
  . Functor hosts
  ⇒ Record (URIRefPrintOptions userInfo hosts host port hierPath relPath query fragment r)
  → URIRef userInfo hosts host port hierPath relPath query fragment
  → String
print opts =
  either
    (URI.print opts)
    (RelativeRef.print opts)
