module Data.URI.HierarchicalPart where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (HierarchicalPart(..))
import Data.URI.Authority as Authority
import Data.URI.Path (printPath, parseURIPathAbs, parsePathRootless, parsePathAbsolute, parsePathAbEmpty)
import Text.Parsing.StringParser (Parser)

parser ∷ Parser HierarchicalPart
parser = withAuth <|> withoutAuth
  where
  withAuth =
    HierarchicalPart <<< Just
      <$> Authority.parser
      <*> parsePathAbEmpty parseURIPathAbs

  withoutAuth = HierarchicalPart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute parseURIPathAbs)
    <|> (Just <$> parsePathRootless parseURIPathAbs)
    <|> pure Nothing

print ∷ HierarchicalPart → String
print (HierarchicalPart a p) =
  S.joinWith "" (catMaybes [Authority.print <$> a, printPath <$> p])
