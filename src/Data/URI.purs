module Data.URI where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Array (catMaybes)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.URI.Authority
import Data.URI.Common
import Data.URI.Fragment
import Data.URI.RelativePart
import Data.URI.HierarchicalPart
import Data.URI.Host
import Data.URI.Path
import Data.URI.Query
import Data.URI.Scheme
import Data.URI.Types
import Data.URI.UserInfo
import Text.Parsing.StringParser (Parser(), ParseError(), runParser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string, eof)

runParseURIRef :: String -> Either ParseError URIRef
runParseURIRef = runParser parseURIRef

runParseURI :: String -> Either ParseError URI
runParseURI = runParser parseURI

runParseAbsoluteURI :: String -> Either ParseError AbsoluteURI
runParseAbsoluteURI = runParser parseAbsoluteURI

runParseRelativeRef :: String -> Either ParseError RelativeRef
runParseRelativeRef = runParser parseRelativeRef

parseURIRef :: Parser URIRef
parseURIRef = (Left <$> try parseURI)
          <|> (Right <$> parseRelativeRef)

parseURI :: Parser URI
parseURI = URI <$> (parseScheme <* string ":")
               <*> parseHierarchicalPart
               <*> optionMaybe (string "?" *> parseQuery)
               <*> optionMaybe (string "#" *> parseFragment)
               <* eof

parseAbsoluteURI :: Parser AbsoluteURI
parseAbsoluteURI = AbsoluteURI <$> (parseScheme <* string ":")
                               <*> parseHierarchicalPart
                               <*> optionMaybe (string "?" *> parseQuery)
                               <* eof

parseRelativeRef :: Parser RelativeRef
parseRelativeRef = RelativeRef <$> parseRelativePart
                               <*> optionMaybe (string "?" *> parseQuery)
                               <*> optionMaybe (string "#" *> parseFragment)
                               <* eof

printURIRef :: URIRef -> String
printURIRef = either printURI printRelativeRef

printURI :: URI -> String
printURI (URI s h q f) =
  joinWith "" $ catMaybes [ printScheme <$> s
                          , Just (printHierPart h)
                          , printQuery <$> q
                          , ("#" ++ ) <$> f
                          ]

printAbsoluteURI :: AbsoluteURI -> String
printAbsoluteURI (AbsoluteURI s h q) =
  joinWith "" $ catMaybes [ printScheme <$> s
                          , Just (printHierPart h)
                          , printQuery <$> q
                          ]

printRelativeRef :: RelativeRef -> String
printRelativeRef (RelativeRef h q f) =
  joinWith "" $ catMaybes [ Just (printRelativePart h)
                          , printQuery <$> q
                          , ("#" ++ ) <$> f
                          ]
