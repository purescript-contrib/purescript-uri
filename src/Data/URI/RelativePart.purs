module Data.URI.RelativePart where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.URI.Authority
import Data.URI.Common
import Data.URI.Path
import Data.URI.Types
import Text.Parsing.StringParser (Parser())
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string)

parseRelativePart :: Parser RelativePart
parseRelativePart = (RelativePart <$> optionMaybe (string "//" *> parseAuthority) <*> parsePathAbEmpty parseURIPathRel)
                <|> (RelativePart Nothing <$> ((Just <$> parsePathAbsolute parseURIPathRel)
                                          <|> (Just <$> parsePathNoScheme parseURIPathRel)
                                          <|> pure Nothing))

printRelativePart :: RelativePart -> String
printRelativePart (RelativePart a p) =
  joinWith "" $ catMaybes [ printAuthority <$> a
                          , printPath <$> p
                          ]
