module Data.URI.HierarchicalPart where

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

parseHierarchicalPart :: Parser HierarchicalPart
parseHierarchicalPart = (HierarchicalPart <$> optionMaybe (string "//" *> parseAuthority) <*> parsePathAbEmpty parseURIPathAbs)
                    <|> (HierarchicalPart Nothing <$> ((Just <$> parsePathAbsolute parseURIPathAbs)
                                                  <|> (Just <$> parsePathRootless parseURIPathAbs)
                                                  <|> pure Nothing))

printHierPart :: HierarchicalPart -> String
printHierPart (HierarchicalPart a p) =
  joinWith "" $ catMaybes [ printAuthority <$> a
                          , printPath <$> p
                          ]
