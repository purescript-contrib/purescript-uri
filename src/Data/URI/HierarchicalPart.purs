module Data.URI.HierarchicalPart
  ( HierarchicalPart(..)
  , HierarchicalPartOptions
  , HierarchicalPartParseOptions
  , HierarchicalPartPrintOptions
  , parser
  , print
  , _authority
  , _path
  , module Data.URI.Authority
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String as String
import Data.Tuple (Tuple)
import Data.URI.Authority (Authority(..), Host(..), Port(..), RegName, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Path as Path
import Text.Parsing.StringParser (ParseError, Parser)

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart userInfo hosts host port hierPath = HierarchicalPart (Maybe (Authority userInfo hosts host port)) (Maybe hierPath)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq1 hosts, Eq host, Eq port, Eq hierPath) ⇒ Eq (HierarchicalPart userInfo hosts host port hierPath)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord1 hosts, Ord host, Ord port, Ord hierPath) ⇒ Ord (HierarchicalPart userInfo hosts host port hierPath)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo hosts host port hierPath) _
instance showHierarchicalPart ∷ (Show userInfo, Show (hosts (Tuple host (Maybe port))), Show host, Show port, Show hierPath) ⇒ Show (HierarchicalPart userInfo hosts host port hierPath) where show = genericShow

type HierarchicalPartOptions userInfo hosts host port hierPath =
  HierarchicalPartParseOptions userInfo hosts host port hierPath
    (HierarchicalPartPrintOptions userInfo hosts host port hierPath ())

type HierarchicalPartParseOptions userInfo hosts host port hierPath r =
  ( parseUserInfo ∷ UserInfo → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseHost ∷ Host → Either ParseError host
  , parsePort ∷ Port → Either ParseError port
  , parseHierPath ∷ String → Either ParseError hierPath
  | r
  )

type HierarchicalPartPrintOptions userInfo hosts host port hierPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts String → String
  , printHost ∷ host → Host
  , printPort ∷ port → Port
  , printHierPath ∷ hierPath → String
  | r
  )

parser
  ∷ ∀ userInfo hosts host port hierPath r
  . Record (HierarchicalPartParseOptions userInfo hosts host port hierPath r)
  → Parser (HierarchicalPart userInfo hosts host port hierPath)
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
  ∷ ∀ userInfo hosts host port hierPath r
  . Functor hosts
  ⇒ Record (HierarchicalPartPrintOptions userInfo hosts host port hierPath r)
  → HierarchicalPart userInfo hosts host port hierPath → String
print opts (HierarchicalPart a p) =
  String.joinWith "" $ Array.catMaybes
    [ Authority.print opts <$> a
    , opts.printHierPath <$> p
    ]

_authority
  ∷ ∀ userInfo hosts host port hierPath
  . Lens'
      (HierarchicalPart userInfo hosts host port hierPath)
      (Maybe (Authority userInfo hosts host port))
_authority =
  lens
    (\(HierarchicalPart a _) → a)
    (\(HierarchicalPart _ p) a → HierarchicalPart a p)

_path
  ∷ ∀ userInfo hosts host port hierPath
  . Lens'
      (HierarchicalPart userInfo hosts host port hierPath)
      (Maybe hierPath)
_path =
  lens
    (\(HierarchicalPart _ p) → p)
    (\(HierarchicalPart a _) p → HierarchicalPart a p)
