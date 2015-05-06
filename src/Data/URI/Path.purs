module Data.URI.Path
  ( parsePath
  , parsePathAbEmpty
  , parsePathAbsolute
  , parsePathNoScheme
  , parsePathRootless
  , parseSegment
  , parseSegmentNonZero
  , parseSegmentNonZeroNoColon
  , parseFilePath
  , parseDirPath
  ) where

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (Dir(), File(), parseAbsDir, parseAbsFile)
import Data.String (joinWith, drop, length)
import Data.URI.Common
import Data.URI.Types
import Text.Parsing.StringParser (Parser(..), ParseError(..), try)
import Text.Parsing.StringParser.Combinators (many, many1)
import Text.Parsing.StringParser.String (string)

parsePath :: forall a. Parser (URIPath a) -> Parser (Maybe (URIPath a))
parsePath p = parsePathAbEmpty p
          <|> (Just <$> parsePathAbsolute p)
          <|> (Just <$> parsePathNoScheme p)
          <|> (Just <$> parsePathRootless p)
          <|> pure Nothing

parsePathAbEmpty :: forall a. Parser (URIPath a) -> Parser (Maybe (URIPath a))
parsePathAbEmpty p = try (Just <$> wrapParser p (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)))
                 <|> pure Nothing

parsePathAbsolute :: forall a. Parser (URIPath a) -> Parser (URIPath a)
parsePathAbsolute p = wrapParser p $ do
  string "/"
  start <- parseSegmentNonZero
  rest <- joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)
  return $ "/" ++ start ++ rest

parsePathNoScheme :: forall a. Parser (URIPath a) -> Parser (URIPath a)
parsePathNoScheme p =
  wrapParser p $ ((++) <$> parseSegmentNonZeroNoColon
                        <*> (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment)))

parsePathRootless :: forall a. Parser (URIPath a) -> Parser (URIPath a)
parsePathRootless p =
  wrapParser p $ ((++) <$> parseSegmentNonZero
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

parseFilePath :: Parser (URIPath File)
parseFilePath = Parser \{ str: str, pos: i } fc sc -> case parseAbsFile (drop i str) of
  Just path -> sc path { str: str, pos: length str }
  Nothing -> fc i (ParseError $ "Expected a valid file path")

parseDirPath :: Parser (URIPath Dir)
parseDirPath = Parser \{ str: str, pos: i } fc sc -> case parseAbsDir (drop i str) of
  Just path -> sc path { str: str, pos: length str }
  Nothing -> fc i (ParseError $ "Expected a valid directory path")
