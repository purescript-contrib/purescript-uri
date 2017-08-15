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
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (Path, Escaper(..), parseAbsDir, parseRelDir, parseAbsFile, parseRelFile, sandbox, rootDir, (</>), unsafePrintPath')
import Data.String as Str
import Data.URI (URIPath, URIPathRel, URIPathAbs)
import Data.URI.Common (PCTEncoded, decodePCT, joinWith, parsePCTEncoded, parsePChar, parseSubDelims, parseUnreserved, wrapParser)
import Global (encodeURI)
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
  _ <- string "/"
  start ← parseSegmentNonZero
  rest ← joinWith "" <$> many (append <$> string "/" <*> parseSegment)
  pure $ "/" <> start <> rest

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
parseSegment = joinWith "" <$> many (parsePChar decoder)

parseSegmentNonZero ∷ Parser String
parseSegmentNonZero = joinWith "" <$> many1 (parsePChar decoder)

parseSegmentNonZeroNoColon ∷ Parser String
parseSegmentNonZeroNoColon =
  joinWith "" <$> many1
    (parseUnreserved <|> parsePCTEncoded decoder <|> parseSubDelims <|> string "@")

parseURIPathAbs ∷ Parser URIPathAbs
parseURIPathAbs = Parser \{ str: str, pos: i } →
  case sandbox rootDir =<< parseAbsFile (Str.drop i str) of
    Just file → Right { result: (Right $ rootDir </> file), suffix: { str: str, pos: Str.length str }}
    Nothing → case sandbox rootDir =<< parseAbsDir (Str.drop i str) of
      Just dir → Right { result: (Left $ rootDir </> dir), suffix: { str: str, pos: Str.length str }}
      Nothing → Left { error: (ParseError $ "Expected a valid path"), pos: i }

parseURIPathRel ∷ Parser URIPathRel
parseURIPathRel = Parser \{ str: str, pos: i } →
  case parseRelFile (Str.drop i str) of
    Just file → Right { result : Right file, suffix: { str: str, pos: Str.length str }}
    Nothing → case parseRelDir (Str.drop i str) of
      Just dir →  Right { result : Left dir,  suffix: { str: str, pos: Str.length str }}
      Nothing → Left { error: (ParseError $ "Expected a valid path"), pos: i}

printPath ∷ ∀ a s. URIPath a s → String
printPath = either printPath' printPath'

printPath' ∷ ∀ a' b s'. Path a' b s' → String
printPath' path =
  let printed = unsafePrintPath' escaper path
  in fromMaybe printed $ Str.stripPrefix (Str.Pattern "./") printed

decoder ∷ PCTEncoded → String
decoder = Str.replaceAll (Str.Pattern "%23") (Str.Replacement "#") <<< decodePCT

escaper ∷ Escaper
escaper = Escaper $
  Str.replaceAll (Str.Pattern "#") (Str.Replacement "%23") <<< encodeURI
