module Data.URI.RelativePart
  ( module Data.URI.RelativePart
  , module Data.URI.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Authority (printAuthority, parseAuthority)
import Data.URI.Path (printPath, parseURIPathRel, parsePathNoScheme, parsePathAbsolute, parsePathAbEmpty)
import Data.URI.Types (Authority(..), RelativePart(..))

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)

parseRelativePart ∷ Parser RelativePart
parseRelativePart = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> (string "//" *> parseAuthority)
      <*> parsePathAbEmpty parseURIPathRel

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute parseURIPathRel)
    <|> (Just <$> parsePathNoScheme parseURIPathRel)
    <|> pure Nothing

printRelativePart ∷ RelativePart → String
printRelativePart (RelativePart a p) =
  S.joinWith "" $ catMaybes
    [ printAuthority <$> a
    , printPath <$> p
    ]
