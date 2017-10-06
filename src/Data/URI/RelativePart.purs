module Data.URI.RelativePart where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (Authority, RelativePart(..), URIPathRel)
import Data.URI.Authority as Authority
import Data.URI.Path (printPath, parseURIPathRel, parsePathNoScheme, parsePathAbsolute, parsePathAbEmpty)
import Text.Parsing.StringParser (Parser)

parser ∷ Parser RelativePart
parser = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> Authority.parser
      <*> parsePathAbEmpty parseURIPathRel

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute parseURIPathRel)
    <|> (Just <$> parsePathNoScheme parseURIPathRel)
    <|> pure Nothing

print ∷ RelativePart → String
print (RelativePart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print <$> a
      , printPath <$> p
      ]

_authority ∷ Lens' RelativePart (Maybe Authority)
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path ∷ Lens' RelativePart (Maybe URIPathRel)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
