module Data.URI.HierarchicalPart
  ( module Data.URI.HierarchicalPart
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Authority (printAuthority, parseAuthority)
import Data.URI.Path (printPath, parseURIPathAbs, parsePathRootless, parsePathAbsolute, parsePathAbEmpty)
import Data.URI.Types (Authority(..), HierarchicalPart(..))

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)

parseHierarchicalPart ∷ Parser HierarchicalPart
parseHierarchicalPart = withAuth <|> withoutAuth
  where
  withAuth =
    HierarchicalPart
      <$> Just <$> (string "//" *> parseAuthority)
      <*> parsePathAbEmpty parseURIPathAbs

  withoutAuth = HierarchicalPart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute parseURIPathAbs)
    <|> (Just <$> parsePathRootless parseURIPathAbs)
    <|> pure Nothing

printHierPart ∷ HierarchicalPart → String
printHierPart (HierarchicalPart a p) =
  S.joinWith "" $ catMaybes
    [ printAuthority <$> a
    , printPath <$> p
    ]
