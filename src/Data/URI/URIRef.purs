module Data.URI.URIRef where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.URI.RelativeRef as RelativeRef
import Data.URI.URI as URI
import Text.Parsing.StringParser (ParseError, Parser, try)

-- | An alias for the most common use case of resource identifiers.
type URIRef userInfo hosts hierPath relPath query fragment =
  Either
    (URI.URI userInfo hosts hierPath query fragment)
    (RelativeRef.RelativeRef userInfo hosts relPath query fragment)

type URIRefOptions userInfo hosts hierPath relPath query fragment =
  URIRefParseOptions userInfo hosts hierPath relPath query fragment
    (URIRefPrintOptions userInfo hosts hierPath relPath query fragment ())

type URIRefParseOptions userInfo hosts hierPath relPath query fragment r =
  ( parseUserInfo ∷ String → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseRelPath ∷ String → Either ParseError relPath
  , parseHierPath ∷ String → Either ParseError hierPath
  , parseQuery ∷ String → Either ParseError query
  , parseFragment ∷ String → Either ParseError fragment
  | r
  )

type URIRefPrintOptions userInfo hosts hierPath relPath query fragment r =
  ( printUserInfo ∷ userInfo → String
  , printHosts ∷ hosts String → String
  , printRelPath ∷ relPath → String
  , printHierPath ∷ hierPath → String
  , printQuery ∷ query → String
  , printFragment ∷ fragment → String
  | r
  )

parser
  ∷ ∀ userInfo hosts hierPath relPath query fragment r
  . Record (URIRefParseOptions userInfo hosts hierPath relPath query fragment r)
  → Parser (URIRef userInfo hosts hierPath relPath query fragment)
parser opts
  = (Left <$> try (URI.parser opts))
  <|> (Right <$> RelativeRef.parser opts)

print
  ∷ ∀ userInfo hosts hierPath relPath query fragment r
  . Functor hosts
  ⇒ Record (URIRefPrintOptions userInfo hosts hierPath relPath query fragment r)
  → URIRef userInfo hosts hierPath relPath query fragment
  → String
print opts =
  either
    (URI.print opts)
    (RelativeRef.print opts)
