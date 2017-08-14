module Data.URI
  ( module Data.URI
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.URI.Fragment (parseFragment)
import Data.URI.HierarchicalPart (parseHierarchicalPart)
import Data.URI.Query (parseQuery)
import Data.URI.RelativePart (parseRelativePart)
import Data.URI.Scheme (parseScheme)
import Data.URI.Types (Fragment, Port, URIPath, URIPathAbs, URIPathRel, URIRef, UserInfo, AbsoluteURI(..), Authority(..), HierarchicalPart(..), Host(..), Query(..), RelativePart(..), RelativeRef(..), URI(..), URIScheme(..))
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string, eof)

runParseURIRef ∷ String → Either ParseError URIRef
runParseURIRef = runParser parseURIRef

runParseURI ∷ String → Either ParseError URI
runParseURI = runParser parseURI

runParseAbsoluteURI ∷ String → Either ParseError AbsoluteURI
runParseAbsoluteURI = runParser parseAbsoluteURI

runParseRelativeRef ∷ String → Either ParseError RelativeRef
runParseRelativeRef = runParser parseRelativeRef

parseURIRef ∷ Parser URIRef
parseURIRef
  = (Left <$> try parseURI)
  <|> (Right <$> parseRelativeRef)

parseURI ∷ Parser URI
parseURI = URI
  <$> (parseScheme <* string ":")
  <*> parseHierarchicalPart
  <*> optionMaybe (string "?" *> parseQuery)
  <*> optionMaybe (string "#" *> parseFragment)
  <* eof

parseAbsoluteURI ∷ Parser AbsoluteURI
parseAbsoluteURI = AbsoluteURI
  <$> (parseScheme <* string ":")
  <*> parseHierarchicalPart
  <*> optionMaybe (string "?" *> parseQuery)
  <* eof

parseRelativeRef ∷ Parser RelativeRef
parseRelativeRef = RelativeRef
  <$> parseRelativePart
  <*> optionMaybe (string "?" *> parseQuery)
  <*> optionMaybe (string "#" *> parseFragment)
  <* eof
