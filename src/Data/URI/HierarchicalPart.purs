module Data.URI.HierarchicalPart
  ( HierarchicalPart(..)
  , HierarchicalPartParseOptions
  , parser
  , HierarchicalPartPrintOptions
  , print
  , _authority
  , _path
  , module Data.URI.Authority
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String as S
import Data.Tuple (Tuple)
import Data.URI.Authority (Authority(..), AuthorityPrintOptions, Host(..), Port(..), AuthorityParseOptions, _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Path as Path
import Text.Parsing.StringParser (Parser)

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart userInfo hosts hierPath = HierarchicalPart (Maybe (Authority userInfo hosts)) (Maybe hierPath)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq1 hosts, Eq hierPath) ⇒ Eq (HierarchicalPart userInfo hosts hierPath)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord1 hosts, Ord hierPath) ⇒ Ord (HierarchicalPart userInfo hosts hierPath)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo hosts hierPath) _
instance showHierarchicalPart ∷ (Show userInfo, Show (hosts (Tuple Host (Maybe Port))), Show hierPath) ⇒ Show (HierarchicalPart userInfo hosts hierPath) where show = genericShow

type HierarchicalPartParseOptions userInfo hosts hierPath r =
  AuthorityParseOptions userInfo hosts
    ( parseHierPath ∷ Parser hierPath
    | r
    )

parser
  ∷ ∀ userInfo hosts hierPath r
  . Record (HierarchicalPartParseOptions userInfo hosts hierPath r)
  → Parser (HierarchicalPart userInfo hosts hierPath)
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

type HierarchicalPartPrintOptions userInfo hosts hierPath r =
  AuthorityPrintOptions userInfo hosts
    ( printHierPath ∷ hierPath → String
    | r
    )

print
  ∷ ∀ userInfo hosts hierPath r
  . Functor hosts
  ⇒ Record (HierarchicalPartPrintOptions userInfo hosts hierPath r)
  → HierarchicalPart userInfo hosts hierPath → String
print opts (HierarchicalPart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print opts <$> a
      , opts.printHierPath <$> p
      ]

_authority ∷ ∀ userInfo hosts hierPath. Lens' (HierarchicalPart userInfo hosts hierPath) (Maybe (Authority userInfo hosts))
_authority =
  lens
    (\(HierarchicalPart a _) → a)
    (\(HierarchicalPart _ p) a → HierarchicalPart a p)

_path ∷ ∀ userInfo hosts hierPath. Lens' (HierarchicalPart userInfo hosts hierPath) (Maybe hierPath)
_path =
  lens
    (\(HierarchicalPart _ p) → p)
    (\(HierarchicalPart a _) p → HierarchicalPart a p)
