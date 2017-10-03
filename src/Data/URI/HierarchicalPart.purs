module Data.URI.HierarchicalPart where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (Authority, HierarchicalPart(..), URIPathAbs)
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

_authority ∷ Lens' HierarchicalPart (Maybe Authority)
_authority =
  lens
    (\(HierarchicalPart a _) → a)
    (\(HierarchicalPart _ p) a → HierarchicalPart a p)

_path ∷ Lens' HierarchicalPart (Maybe URIPathAbs)
_path =
  lens
    (\(HierarchicalPart _ p) → p)
    (\(HierarchicalPart a _) p → HierarchicalPart a p)
