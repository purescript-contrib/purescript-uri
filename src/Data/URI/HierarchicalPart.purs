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
data HierarchicalPart userInfo hierPath = HierarchicalPart (Maybe (Authority userInfo)) (Maybe hierPath)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq hierPath) ⇒ Eq (HierarchicalPart userInfo hierPath)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord hierPath) ⇒ Ord (HierarchicalPart userInfo hierPath)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo hierPath) _
instance showHierarchicalPart ∷ (Show userInfo, Show hierPath) ⇒ Show (HierarchicalPart userInfo hierPath) where show = genericShow

parser
  ∷ ∀ userInfo hierPath r
  . { parseUserInfo ∷ Parser userInfo
    , parseHierPath ∷ Parser hierPath
    | r
    }
  → Parser (HierarchicalPart userInfo hierPath)
parser opts = withAuth <|> withoutAuth
  where
  withAuth =
    HierarchicalPart <<< Just
      <$> Authority.parser opts
      <*> Path.parsePathAbEmpty opts.parseHierPath

  withoutAuth = HierarchicalPart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> Path.parsePathAbsolute opts.parseHierPath)
    <|> (Just <$> Path.parsePathRootless opts.parseHierPath)
    <|> pure Nothing

print
  ∷ ∀ userInfo hierPath r
  . { printUserInfo ∷ userInfo → String
    , printHierPath ∷ hierPath → String
    | r
    }
  → HierarchicalPart userInfo hierPath → String
print opts (HierarchicalPart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print opts <$> a
      , opts.printHierPath <$> p
      ]

_authority ∷ ∀ userInfo hierPath. Lens' (HierarchicalPart userInfo hierPath) (Maybe (Authority userInfo))
_authority =
  lens
    (\(HierarchicalPart a _) → a)
    (\(HierarchicalPart _ p) a → HierarchicalPart a p)

_path ∷ ∀ userInfo hierPath. Lens' (HierarchicalPart userInfo hierPath) (Maybe hierPath)
_path =
  lens
    (\(HierarchicalPart _ p) → p)
    (\(HierarchicalPart a _) p → HierarchicalPart a p)
