module Data.URI.RelativePart where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (RelativePart(..))
import Data.URI.Authority as Authority
import Data.URI.Path (printPath, parseURIPathRel, parsePathNoScheme, parsePathAbsolute, parsePathAbEmpty)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser RelativePart
parser = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> (string "//" *> Authority.parser)
      <*> parsePathAbEmpty parseURIPathRel

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute parseURIPathRel)
    <|> (Just <$> parsePathNoScheme parseURIPathRel)
    <|> pure Nothing

print ∷ RelativePart → String
print (RelativePart a p) =
  "//" <> S.joinWith "" (catMaybes [Authority.print <$> a, printPath <$> p])
