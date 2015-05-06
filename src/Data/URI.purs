module Data.URI
  ( parseFileURI
  , parseDirURI
  , parserFileURI
  , parserDirURI
  , printURI
  ) where

import Data.URI.Authority
import Data.URI.Common
import Data.URI.Host
import Data.URI.Path
import Data.URI.Query
import Data.URI.Scheme
import Data.URI.Types
import Data.URI.UserInfo
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Either (Either())
import Data.Tuple (Tuple(..))
import Data.StrMap (toList)
import Data.String (joinWith)
import Text.Parsing.StringParser (Parser(), ParseError(), runParser, try)
import Text.Parsing.StringParser.Combinators (many, optionMaybe)
import Text.Parsing.StringParser.String (string, eof)
import Data.Path.Pathy (File(), Dir(), unsafePrintPath)

parseFileURI :: String -> Either ParseError (URI File)
parseFileURI = runParser parserFileURI

parseDirURI :: String -> Either ParseError (URI Dir)
parseDirURI = runParser parserDirURI

parserFileURI :: Parser (URI File)
parserFileURI = URI <$> (parseScheme <* string ":")
                    <*> parseHierarchicalPart parseFilePath
                    <*> optionMaybe (string "?" *> parseQuery)
                    <*> optionMaybe (string "#" *> parseFragment)
                    <* eof

parserDirURI :: Parser (URI Dir)
parserDirURI = URI <$> (parseScheme <* string ":")
                   <*> parseHierarchicalPart parseDirPath
                   <*> optionMaybe (string "?" *> parseQuery)
                   <*> optionMaybe (string "#" *> parseFragment)
                   <* eof

parseHierarchicalPart :: forall a. Parser (URIPath a) -> Parser (HierarchicalPart a)
parseHierarchicalPart p = (HierarchicalPart <$> optionMaybe (string "//" *> parseAuthority) <*> parsePathAbEmpty p)
                      <|> (HierarchicalPart Nothing <$> ((Just <$> parsePathAbsolute p)
                                                     <|> (Just <$> parsePathRootless p)
                                                     <|> pure Nothing))

-- URI-reference = URI / relative-ref
-- uriReference :: Parser _
-- uriReference = { uri: _, relativeRef: _ }
--            <$> parseURI
--            <*> parseRelativeRef

-- relative-ref = relative-part [ "?" query ] [ "#" fragment ]
-- parseRelativeRef :: Parser _
-- parseRelativeRef = { relativePart: _, query: _, fragment: _ }
--                <$> parseRelativePart
--                <*> optionMaybe (string "?" *> parseQuery)
--                <*> optionMaybe (string "#" *> parseFragment)

-- relative-part = "//" authority path-abempty
--               / path-absolute
--               / path-noscheme
--               / path-empty
-- parseRelativePart :: Parser _
-- parseRelativePart = ({ authority: _, path: _ } <$> optionMaybe (string "//" *> parseAuthority) <*> parsePathAbEmpty)
--                 <|> ({ authority: Nothing, path: _ } <$> (parsePathAbsolute
--                                                       <|> parsePathNoScheme
--                                                       <|> pure ""))

parseFragment :: Parser Fragment
parseFragment = try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?"))

printURI :: forall a. URI a -> String
printURI (URI s h q f) = combine [ printScheme <$> s
                                 , Just (printHierPart h)
                                 , printQuery <$> q
                                 , ("#" ++ ) <$> f
                                 ]
  where

  combine :: [Maybe String] -> String
  combine = joinWith "" <<< catMaybes

  printScheme :: URIScheme -> String
  printScheme (URIScheme s) = s ++ ":"

  printHierPart :: HierarchicalPart a -> String
  printHierPart (HierarchicalPart a p) = combine [ printAuthority <$> a
                                                 , printPath <$> p
                                                 ]

  printAuthority :: Authority -> String
  printAuthority (Authority u h p) = "//" ++ combine [ (++ "@") <$> u
                                                     , Just (printHost h)
                                                     , (":" ++) <$> p
                                                     ]

  printHost :: Host -> String
  printHost (IPv6Address i) = "[" ++ i ++ "]"
  printHost (IPv4Address i) = i
  printHost (NameAddress i) = i
  printHost (MultipleHosts hs) = joinWith "," $ printHost <$> hs

  printPath :: URIPath a -> String
  printPath = unsafePrintPath

  printQuery :: Query -> String
  printQuery (Query m) = "?" ++ joinWith "&" (printQueryPart <$> toList m)

  printQueryPart :: Tuple String String -> String
  printQueryPart (Tuple k v) = k ++ "=" ++ v
