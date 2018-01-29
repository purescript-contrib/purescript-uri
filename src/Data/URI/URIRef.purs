module Data.URI.URIRef where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.URI.RelativeRef as RelativeRef
import Data.URI.URI as URI
import Text.Parsing.StringParser (Parser, try)

-- | An alias for the most common use case of resource identifiers.
type URIRef userInfo hierPath relPath query fragment =
  Either
    (URI.URI userInfo hierPath query fragment)
    (RelativeRef.RelativeRef userInfo relPath query fragment)

type URIRefOptions userInfo hierPath relPath query fragment =
  URIRefParseOptions userInfo hierPath relPath query fragment (URIRefPrintOptions userInfo hierPath relPath query fragment ())

type URIRefParseOptions userInfo hierPath relPath query fragment r =
  ( parseUserInfo ∷ Parser userInfo
  , parseHierPath ∷ Parser hierPath
  , parseRelPath ∷ Parser relPath
  , parseQuery ∷ Parser query
  , parseFragment ∷ Parser fragment
  | r
  )

parser
  ∷ ∀ userInfo hierPath relPath query fragment r
  . Record (URIRefParseOptions userInfo hierPath relPath query fragment r)
  → Parser (URIRef userInfo hierPath relPath query fragment)
parser opts
  = (Left <$> try (URI.parser opts))
  <|> (Right <$> RelativeRef.parser opts)

type URIRefPrintOptions userInfo hierPath relPath query fragment r =
  ( printUserInfo ∷ userInfo → String
  , printHierPath ∷ hierPath → String
  , printRelPath ∷ relPath → String
  , printQuery ∷ query → String
  , printFragment ∷ fragment → String
  | r
  )

print
  ∷ ∀ userInfo hierPath relPath query fragment r
  . Record (URIRefPrintOptions userInfo hierPath relPath query fragment r)
  → URIRef userInfo hierPath relPath query fragment
  → String
print opts =
  either
    (URI.print opts)
    (RelativeRef.print opts)
