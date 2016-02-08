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
import Control.Bind ((=<<))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (Path(), parseAbsDir, parseRelDir, parseAbsFile, parseRelFile, sandbox, rootDir, (</>), unsafePrintPath)
import Data.String as Str
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser (Parser(..), ParseError(..), try)
import Text.Parsing.StringParser.Combinators (many, many1)
import Text.Parsing.StringParser.String (string)

parsePath :: forall p. Parser p -> Parser (Maybe p)
parsePath p = parsePathAbEmpty p
         <|> (Just <$> parsePathAbsolute p)
         <|> (Just <$> parsePathNoScheme p)
         <|> (Just <$> parsePathRootless p)
         <|> pure Nothing

parsePathAbEmpty :: forall p. Parser p -> Parser (Maybe p)
parsePathAbEmpty p = try (Just <$> wrapParser p (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)))
                     <|> pure Nothing

parsePathAbsolute :: forall p. Parser p -> Parser p
parsePathAbsolute p = wrapParser p $ do
  string "/"
  start <- parseSegmentNonZero
  rest <- joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)
  return $ "/" ++ start ++ rest

parsePathNoScheme :: forall p. Parser p -> Parser p
parsePathNoScheme p = wrapParser p $
  ((++) <$> parseSegmentNonZeroNoColon
        <*> (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)))

parsePathRootless :: forall p. Parser p -> Parser p
parsePathRootless p = wrapParser p $
  ((++) <$> parseSegmentNonZero
        <*> (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)))

parseSegment :: Parser String
parseSegment = joinWith "" <$> many parsePChar

parseSegmentNonZero :: Parser String
parseSegmentNonZero = joinWith "" <$> many1 parsePChar

parseSegmentNonZeroNoColon :: Parser String
parseSegmentNonZeroNoColon = joinWith "" <$> many1 (parseUnreserved
                                         <|> parsePCTEncoded
                                         <|> parseSubDelims
                                         <|> string "@")

parseURIPathAbs :: Parser URIPathAbs
parseURIPathAbs = Parser \{ str: str, pos: i } fc sc ->
  case sandbox rootDir =<< parseAbsFile (Str.drop i str) of
    Just file -> sc (Left $ rootDir </> file) { str: str, pos: Str.length str }
    Nothing -> case sandbox rootDir =<< parseAbsDir (Str.drop i str) of
      Just dir -> sc (Right $ rootDir </> dir) { str: str, pos: Str.length str }
      Nothing -> fc i (ParseError $ "Expected a valid path")

parseURIPathRel :: Parser URIPathRel
parseURIPathRel = Parser \{ str: str, pos: i } fc sc ->
  case parseRelFile (Str.drop i str) of
    Just file -> sc (Left file) { str: str, pos: Str.length str }
    Nothing -> case parseRelDir (Str.drop i str) of
      Just dir -> sc (Right dir) { str: str, pos: Str.length str }
      Nothing -> fc i (ParseError $ "Expected a valid path")

printPath :: forall a s. URIPath a s -> String
printPath = either print print
  where
  print :: forall a' b s'. Path a' b s' -> String
  print path =
    let printed = unsafePrintPath path
    in if Str.take 2 printed == "./" then Str.drop 2 printed else printed
