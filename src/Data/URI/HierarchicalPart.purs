module Data.URI.HierarchicalPart
  ( HierarchicalPart(..)
  , HierarchicalPartOptions
  , HierarchicalPartParseOptions
  , HierarchicalPartPrintOptions
  , HierPath
  , parser
  , print
  , _authority
  , _path
  , _hierPath
  , module Data.URI.Authority
  , module Data.URI.Path
  , module Data.URI.Path.Absolute
  , module Data.URI.Path.Rootless
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Traversal', wander)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.URI.Authority (Authority(..), AuthorityOptions, AuthorityParseOptions, AuthorityPrintOptions, Host(..), HostsParseOptions, IPv4Address, IPv6Address, Port, RegName, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Common (URIPartParseError)
import Data.URI.Path (Path(..))
import Data.URI.Path as Path
import Data.URI.Path.Absolute (PathAbsolute(..))
import Data.URI.Path.Absolute as PathAbs
import Data.URI.Path.Rootless (PathRootless(..))
import Data.URI.Path.Rootless as PathRootless
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart userInfo hosts path hierPath
  = HierarchicalPartAuth (Authority userInfo hosts) (Maybe path)
  | HierarchicalPartNoAuth (Maybe hierPath)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq hosts, Eq path, Eq hierPath) ⇒ Eq (HierarchicalPart userInfo hosts path hierPath)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord hosts, Ord path, Ord hierPath) ⇒ Ord (HierarchicalPart userInfo hosts path hierPath)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo hosts path hierPath) _
instance showHierarchicalPart ∷ (Show userInfo, Show hosts, Show path, Show hierPath) ⇒ Show (HierarchicalPart userInfo hosts path hierPath) where show = genericShow

type HierarchicalPartOptions userInfo hosts path hierPath =
  HierarchicalPartParseOptions userInfo hosts path hierPath
    (HierarchicalPartPrintOptions userInfo hosts path hierPath ())

type HierarchicalPartParseOptions userInfo hosts path hierPath r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseHierPath ∷ HierPath → Either URIPartParseError hierPath
  | r
  )

type HierarchicalPartPrintOptions userInfo hosts path hierPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → HierPath
  | r
  )

type HierPath = Either PathAbsolute PathRootless

parser
  ∷ ∀ userInfo hosts path hierPath r
  . Record (HierarchicalPartParseOptions userInfo hosts path hierPath r)
  → Parser String (HierarchicalPart userInfo hosts path hierPath)
parser opts = withAuth <|> withoutAuth
  where
  withAuth =
    HierarchicalPartAuth
      <$> Authority.parser opts
      <*> optionMaybe (Path.parser opts.parsePath)
  withoutAuth =
    HierarchicalPartNoAuth <$> noAuthPath
  noAuthPath
    = (Just <$> PathAbs.parse (opts.parseHierPath <<< Left))
    <|> (Just <$> PathRootless.parse (opts.parseHierPath <<< Right))
    <|> pure Nothing

print
  ∷ ∀ userInfo hosts path hierPath r
  . Record (HierarchicalPartPrintOptions userInfo hosts path hierPath r)
  → HierarchicalPart userInfo hosts path hierPath → String
print opts = case _ of
  HierarchicalPartAuth a p →
    String.joinWith "" $ Array.catMaybes
      [ pure $ Authority.print opts a
      , Path.print <<< opts.printPath <$> p
      ]
  HierarchicalPartNoAuth p →
    maybe "" (either PathAbs.print PathRootless.print <<< opts.printHierPath) p

_authority
  ∷ ∀ userInfo hosts path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts path hierPath)
      (Authority userInfo hosts)
_authority = wander \f → case _ of
  HierarchicalPartAuth a p → flip HierarchicalPartAuth p <$> f a
  a → pure a

_path
  ∷ ∀ userInfo hosts path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts path hierPath)
      (Maybe path)
_path = wander \f → case _ of
  HierarchicalPartAuth a p → HierarchicalPartAuth a <$> f p
  a → pure a

_hierPath
  ∷ ∀ userInfo hosts path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts path hierPath)
      (Maybe hierPath)
_hierPath = wander \f → case _ of
  HierarchicalPartNoAuth p → HierarchicalPartNoAuth <$> f p
  a → pure a
