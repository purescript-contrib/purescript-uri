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
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Traversal', wander)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)
import Data.String as String
import Data.Tuple (Tuple)
import Data.URI.Authority (Authority(..), Host(..), Port(..), RegName, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Path (Path(..))
import Data.URI.Path as Path
import Data.URI.Path.Absolute (PathAbsolute(..))
import Data.URI.Path.Absolute as PathAbs
import Data.URI.Path.Rootless (PathRootless(..))
import Data.URI.Path.Rootless as PathRootless
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart userInfo hosts host port path hierPath
  = HierarchicalPartAuth (Authority userInfo hosts host port) (Maybe path)
  | HierarchicalPartNoAuth (Maybe hierPath)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq1 hosts, Eq host, Eq port, Eq path, Eq hierPath) ⇒ Eq (HierarchicalPart userInfo hosts host port path hierPath)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord1 hosts, Ord host, Ord port, Ord path, Ord hierPath) ⇒ Ord (HierarchicalPart userInfo hosts host port path hierPath)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo hosts host port path hierPath) _
instance showHierarchicalPart ∷ (Show userInfo, Show (hosts (Tuple host (Maybe port))), Show host, Show port, Show path, Show hierPath) ⇒ Show (HierarchicalPart userInfo hosts host port path hierPath) where show = genericShow

type HierarchicalPartOptions userInfo hosts host port path hierPath =
  HierarchicalPartParseOptions userInfo hosts host port path hierPath
    (HierarchicalPartPrintOptions userInfo hosts host port path hierPath ())

type HierarchicalPartParseOptions userInfo hosts host port path hierPath r =
  ( parseUserInfo ∷ UserInfo → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseHost ∷ Host → Either ParseError host
  , parsePort ∷ Port → Either ParseError port
  , parsePath ∷ Path → Either ParseError path
  , parseHierPath ∷ HierPath → Either ParseError hierPath
  | r
  )

type HierarchicalPartPrintOptions userInfo hosts host port path hierPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts String → String
  , printHost ∷ host → Host
  , printPort ∷ port → Port
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → HierPath
  | r
  )

type HierPath = Either PathAbsolute PathRootless

parser
  ∷ ∀ userInfo hosts host port path hierPath r
  . Record (HierarchicalPartParseOptions userInfo hosts host port path hierPath r)
  → Parser (HierarchicalPart userInfo hosts host port path hierPath)
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
  ∷ ∀ userInfo hosts host port path hierPath r
  . Functor hosts
  ⇒ Record (HierarchicalPartPrintOptions userInfo hosts host port path hierPath r)
  → HierarchicalPart userInfo hosts host port path hierPath → String
print opts = case _ of
  HierarchicalPartAuth a p →
    String.joinWith "" $ Array.catMaybes
      [ pure $ Authority.print opts a
      , Path.print <<< opts.printPath <$> p
      ]
  HierarchicalPartNoAuth p →
    maybe "" (either PathAbs.print PathRootless.print <<< opts.printHierPath) p

_authority
  ∷ ∀ userInfo hosts host port path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts host port path hierPath)
      (Authority userInfo hosts host port)
_authority = wander \f a → case a of
  HierarchicalPartAuth a p → flip HierarchicalPartAuth p <$> f a
  _ → pure a

_path
  ∷ ∀ userInfo hosts host port path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts host port path hierPath)
      (Maybe path)
_path = wander \f a → case a of
  HierarchicalPartAuth a p → HierarchicalPartAuth a <$> f p
  _ → pure a

_hierPath
  ∷ ∀ userInfo hosts host port path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts host port path hierPath)
      (Maybe hierPath)
_hierPath = wander \f a → case a of
  HierarchicalPartNoAuth p → HierarchicalPartNoAuth <$> f p
  _ → pure a
