module Data.URI.HierarchicalPart
  ( HierarchicalPart(..)
  , parser
  , print
  , _authority
  , _path
  , module Data.URI.Authority
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Authority (Authority(..), Host(..), Port(..), _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Path as Path
import Text.Parsing.StringParser (Parser)

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart userInfo path = HierarchicalPart (Maybe (Authority userInfo)) (Maybe path)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq path) ⇒ Eq (HierarchicalPart userInfo path)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord path) ⇒ Ord (HierarchicalPart userInfo path)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo path) _
instance showHierarchicalPart ∷ (Show userInfo, Show path) ⇒ Show (HierarchicalPart userInfo path) where show = genericShow

parser ∷ ∀ userInfo path. Parser userInfo → Parser path → Parser (HierarchicalPart userInfo path)
parser parseUserInfo parsePath = withAuth <|> withoutAuth
  where
  withAuth =
    HierarchicalPart <<< Just
      <$> Authority.parser parseUserInfo
      <*> Path.parsePathAbEmpty parsePath

  withoutAuth = HierarchicalPart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> Path.parsePathAbsolute parsePath)
    <|> (Just <$> Path.parsePathRootless parsePath)
    <|> pure Nothing

print ∷ ∀ userInfo path. (userInfo → String) → (path → String) → HierarchicalPart userInfo path → String
print printUserInfo printPath (HierarchicalPart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print printUserInfo <$> a
      , printPath <$> p
      ]

_authority ∷ ∀ userInfo path. Lens' (HierarchicalPart userInfo path) (Maybe (Authority userInfo))
_authority =
  lens
    (\(HierarchicalPart a _) → a)
    (\(HierarchicalPart _ p) a → HierarchicalPart a p)

_path ∷ ∀ userInfo path. Lens' (HierarchicalPart userInfo path) (Maybe path)
_path =
  lens
    (\(HierarchicalPart _ p) → p)
    (\(HierarchicalPart a _) p → HierarchicalPart a p)
