module Data.URI.URIRef where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.URI (URIRef)
import Data.URI.RelativeRef as RelativeRef
import Data.URI.URI as URI
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)

parse ∷ String → Either ParseError URIRef
parse = runParser parser

parser ∷ Parser URIRef
parser = (Left <$> try URI.parser) <|> (Right <$> RelativeRef.parser)

print ∷ URIRef → String
print = either URI.print RelativeRef.print
