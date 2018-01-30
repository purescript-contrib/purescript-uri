module Data.URI.Path where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as Str
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
    pure ("/" <> Str.joinWith "/" parts)

parsePathAbsolute ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathAbsolute p = wrapParser p $ do
  _ ← string "/"
  start ← parseSegmentNonZero
  rest ← Str.joinWith "" <$> Array.many (append <$> string "/" <*> parseSegment)
  pure $ "/" <> start <> rest

parsePathNoScheme ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathNoScheme p = wrapParser p $
  append
    <$> parseSegmentNonZeroNoColon
    <*> (Str.joinWith "" <$> Array.many (append <$> string "/" <*> parseSegment))

parsePathRootless ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathRootless p = wrapParser p $
  append
    <$> parseSegmentNonZero
    <*> (Str.joinWith "" <$> Array.many (append <$> string "/" <*> parseSegment))

parseSegment ∷ Parser String
parseSegment = Str.joinWith "" <$> Array.many (parsePChar decoder)

parseSegmentNonZero ∷ Parser String
parseSegmentNonZero = Str.joinWith "" <$> Array.some (parsePChar decoder)

parseSegmentNonZeroNoColon ∷ Parser String
parseSegmentNonZeroNoColon =
  Str.joinWith "" <$> Array.some
    (parseUnreserved <|> parsePCTEncoded decoder <|> parseSubDelims <|> string "@")

decoder ∷ PCTEncoded → String
decoder = Str.replaceAll (Str.Pattern "%23") (Str.Replacement "#") <<< decodePCT

printPath ∷ ∀ p. (p → String) → p → String
printPath = id
