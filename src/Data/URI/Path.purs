module Data.URI.Path where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.URI.Common (PCTEncoded, decodePCT, joinWith, parsePCTEncoded, parsePChar, parseSubDelims, parseUnreserved, wrapParser)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (many, many1, optionMaybe)
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
    parts ← many1 (string "/" *> parseSegment)
    pure ("/" <> joinWith "/" parts)

parsePathAbsolute ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathAbsolute p = wrapParser p $ do
  _ ← string "/"
  start ← parseSegmentNonZero
  rest ← joinWith "" <$> many (append <$> string "/" <*> parseSegment)
  pure $ "/" <> start <> rest

parsePathNoScheme ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathNoScheme p = wrapParser p $
  append
    <$> parseSegmentNonZeroNoColon
    <*> (joinWith "" <$> many (append <$> string "/" <*> parseSegment))

parsePathRootless ∷ ∀ p. (String → Either ParseError p) → Parser p
parsePathRootless p = wrapParser p $
  append
    <$> parseSegmentNonZero
    <*> (joinWith "" <$> many (append <$> string "/" <*> parseSegment))

parseSegment ∷ Parser String
parseSegment = joinWith "" <$> many (parsePChar decoder)

parseSegmentNonZero ∷ Parser String
parseSegmentNonZero = joinWith "" <$> many1 (parsePChar decoder)

parseSegmentNonZeroNoColon ∷ Parser String
parseSegmentNonZeroNoColon =
  joinWith "" <$> many1
    (parseUnreserved <|> parsePCTEncoded decoder <|> parseSubDelims <|> string "@")

decoder ∷ PCTEncoded → String
decoder = Str.replaceAll (Str.Pattern "%23") (Str.Replacement "#") <<< decodePCT

printPath ∷ ∀ p. (p → String) → p → String
printPath = id
