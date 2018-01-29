module Data.URI.URIRef where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.URI.RelativeRef as RelativeRef
import Data.URI.URI as URI
import Text.Parsing.StringParser (Parser, try)

-- | An alias for the most common use case of resource identifiers.
type URIRef userInfo absPath relPath query fragment = Either (URI.URI userInfo absPath query fragment) (RelativeRef.RelativeRef userInfo relPath query fragment)

parser
  ∷ ∀ userInfo absPath relPath query fragment
  . Parser userInfo
  → Parser absPath
  → Parser relPath
  → Parser query
  → Parser fragment
  → Parser (URIRef userInfo absPath relPath query fragment)
parser parseUserInfo parseAbsPath parseRelPath parseQuery parseFragment
  = (Left <$> try (URI.parser parseUserInfo parseAbsPath parseQuery parseFragment))
  -- <|> (Right <$> RelativeRef.parser parseUserInfo parseRelPath parseQuery parseFragment)

print
  ∷ ∀ userInfo absPath relPath query fragment
  . (userInfo → String)
  → (absPath → String)
  → (relPath → String)
  → (query → String)
  → (fragment → String)
  → URIRef userInfo absPath relPath query fragment
  → String
print printUserInfo printAbsPath printRelPath printQuery printFragment =
  either
    (URI.print printUserInfo printAbsPath printQuery printFragment)
    (RelativeRef.print printUserInfo printRelPath printQuery printFragment)
