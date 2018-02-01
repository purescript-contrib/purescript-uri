module Data.URI.Path where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String as String
import Data.URI.Common (PCTEncoded, decodePCT, parsePCTEncoded, parsePChar, parseSubDelims, parseUnreserved, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string)

parsePath ∷ ∀ p. (String → Either ParseError p) → Parser (Maybe p)
parsePath p
  = parsePathAbEmpty p
  <|> (Just <$> parsePathAbsolute p)
  <|> (Just <$> parsePathNoScheme p)
  <|> (Just <$> parsePathRootless p)
  <|> pure Nothing

parsePathAbEmpty ∷ ∀ p. (String → Either ParseError p) → Parser (Maybe p)
parsePathAbEmpty p =
  optionMaybe $ wrapParser p do
    parts ← Array.some (string "/" *> parseSegment)
    pure ("/" <> String.joinWith "/" parts)

parsePathAbsolute ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathAbsolute p = wrapParser p $ do
  _ ← string "/"
  start ← parseSegmentNonZero
  rest ← String.joinWith "" <$> Array.many (append <$> string "/" <*> parseSegment)
  pure $ "/" <> start <> rest

parsePathNoScheme ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathNoScheme p = wrapParser p $
  append
    <$> parseSegmentNonZeroNoColon
    <*> (String.joinWith "" <$> Array.many (append <$> string "/" <*> parseSegment))

parsePathRootless ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathRootless p = wrapParser p $
  append
    <$> parseSegmentNonZero
    <*> (String.joinWith "" <$> Array.many (append <$> string "/" <*> parseSegment))

parseSegment ∷ Parser String
parseSegment = String.joinWith "" <$> Array.many (parsePChar decoder)

parseSegmentNonZero ∷ Parser String
parseSegmentNonZero = String.joinWith "" <$> Array.some (parsePChar decoder)

parseSegmentNonZeroNoColon ∷ Parser String
parseSegmentNonZeroNoColon =
  String.joinWith "" <$> Array.some
    (String.singleton <$> parseUnreserved <|> parsePCTEncoded decoder <|> String.singleton <$> parseSubDelims <|> string "@")

decoder ∷ PCTEncoded → String
decoder = String.replaceAll (String.Pattern "%23") (String.Replacement "#") <<< decodePCT

printPath ∷ ∀ p. (p → String) → p → String
printPath = id
