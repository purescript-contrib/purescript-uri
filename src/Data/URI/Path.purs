module Data.URI.Path
  ( parsePath
  , parsePathAbEmpty
  , parsePathAbsolute
  , parsePathNoScheme
  , parsePathRootless
  , parseSegment
  , parseSegmentNonZero
  , parseSegmentNonZeroNoColon
  , parseURIPathAbs
  , parseURIPathRel
  , printPath
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (Path, parseAbsDir, parseRelDir, parseAbsFile, parseRelFile, sandbox, rootDir, (</>), unsafePrintPath)
import Data.String as Str
import Data.URI.Common (parseSubDelims, parsePCTEncoded, parseUnreserved, joinWith, parsePChar, wrapParser)
import Data.URI.Types (URIPath, URIPathRel, URIPathAbs)

import Text.Parsing.StringParser (Parser(..), ParseError(..), try)
import Text.Parsing.StringParser.Combinators (many, many1)
import Text.Parsing.StringParser.String (string)

parsePath ∷ ∀ p. Parser p → Parser (Maybe p)
parsePath p
  = parsePathAbEmpty p
  <|> (Just <$> parsePathAbsolute p)
  <|> (Just <$> parsePathNoScheme p)
  <|> (Just <$> parsePathRootless p)
  <|> pure Nothing

parsePathAbEmpty ∷ ∀ p. Parser p → Parser (Maybe p)
parsePathAbEmpty p
  = try (Just <$> wrapParser p
      (joinWith "" <$> many (append <$> string "/" <*> parseSegment)))
  <|> pure Nothing

parsePathAbsolute ∷ ∀ p. Parser p → Parser p
parsePathAbsolute p = wrapParser p $ do
  string "/"
  start ← parseSegmentNonZero
  rest ← joinWith "" <$> many (append <$> string "/" <*> parseSegment)
  return $ "/" ++ start ++ rest

parsePathNoScheme ∷ ∀ p. Parser p → Parser p
parsePathNoScheme p = wrapParser p $
  append
    <$> parseSegmentNonZeroNoColon
    <*> (joinWith "" <$> many (append <$> string "/" <*> parseSegment))

parsePathRootless ∷ ∀ p. Parser p → Parser p
parsePathRootless p = wrapParser p $
  append
    <$> parseSegmentNonZero
    <*> (joinWith "" <$> many (append <$> string "/" <*> parseSegment))

parseSegment ∷ Parser String
parseSegment = joinWith "" <$> many parsePChar

parseSegmentNonZero ∷ Parser String
parseSegmentNonZero = joinWith "" <$> many1 parsePChar

parseSegmentNonZeroNoColon ∷ Parser String
parseSegmentNonZeroNoColon =
  joinWith "" <$> many1
    (parseUnreserved <|> parsePCTEncoded <|> parseSubDelims <|> string "@")

parseURIPathAbs ∷ Parser URIPathAbs
parseURIPathAbs = Parser \{ str: str, pos: i } fc sc →
  case sandbox rootDir =<< parseAbsFile (Str.drop i str) of
    Just file → sc (Right $ rootDir </> file) { str: str, pos: Str.length str }
    Nothing → case sandbox rootDir =<< parseAbsDir (Str.drop i str) of
      Just dir → sc (Left $ rootDir </> dir) { str: str, pos: Str.length str }
      Nothing → fc i (ParseError $ "Expected a valid path")

parseURIPathRel ∷ Parser URIPathRel
parseURIPathRel = Parser \{ str: str, pos: i } fc sc →
  case parseRelFile (Str.drop i str) of
    Just file → sc (Right file) { str: str, pos: Str.length str }
    Nothing → case parseRelDir (Str.drop i str) of
      Just dir → sc (Left dir) { str: str, pos: Str.length str }
      Nothing → fc i (ParseError $ "Expected a valid path")

printPath ∷ ∀ a s. URIPath a s → String
printPath = either printPath' printPath'

printPath' ∷ ∀ a' b s'. Path a' b s' → String
printPath' path =
  let printed = unsafePrintPath path
  in fromMaybe printed $ Str.stripPrefix "./" printed
