module Data.URI
  ( module Data.URI
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Array (catMaybes)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Fragment (parseFragment)
import Data.URI.HierarchicalPart (printHierPart, parseHierarchicalPart)
import Data.URI.Query (printQuery, parseQuery)
import Data.URI.RelativePart (printRelativePart, parseRelativePart)
import Data.URI.Scheme (printScheme, parseScheme)
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

printURIRef ∷ URIRef → String
printURIRef = either printURI printRelativeRef

printURI ∷ URI → String
printURI (URI s h q f) =
  S.joinWith "" $ catMaybes
    [ printScheme <$> s
    , Just (printHierPart h)
    , printQuery <$> q
    , ("#" <> _) <$> f
    ]

printAbsoluteURI ∷ AbsoluteURI → String
printAbsoluteURI (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ printScheme <$> s
    , Just (printHierPart h)
    , printQuery <$> q
    ]

printRelativeRef ∷ RelativeRef → String
printRelativeRef (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (printRelativePart h)
    , printQuery <$> q
    , ("#" <> _) <$> f
    ]
