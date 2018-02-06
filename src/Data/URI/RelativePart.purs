module Data.URI.RelativePart
  ( RelativePart(..)
  , RelativePartOptions
  , RelativePartParseOptions
  , RelativePartPrintOptions
  , RelPath
  , parser
  , print
  , _authority
  , _path
  , _relPath
  , module Data.URI.Authority
  , module Data.URI.Path
  , module Data.URI.Path.Absolute
  , module Data.URI.Path.NoScheme
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
import Data.URI.Path (Path)
import Data.URI.Path as Path
import Data.URI.Path.Absolute (PathAbsolute)
import Data.URI.Path.Absolute as PathAbs
import Data.URI.Path.NoScheme (PathNoScheme)
import Data.URI.Path.NoScheme as PathNoScheme
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)

-- | The "relative part" of a relative reference.
data RelativePart userInfo hosts path relPath
  = RelativePartAuth (Authority userInfo hosts) (Maybe path)
  | RelativePartNoAuth (Maybe relPath)

derive instance eqRelativePart ∷ (Eq userInfo, Eq hosts, Eq path, Eq relPath) ⇒ Eq (RelativePart userInfo hosts path relPath)
derive instance ordRelativePart ∷ (Ord userInfo, Ord hosts, Ord path, Ord relPath) ⇒ Ord (RelativePart userInfo hosts path relPath)
derive instance genericRelativePart ∷ Generic (RelativePart userInfo hosts path relPath) _
instance showRelativePart ∷ (Show userInfo, Show hosts, Show path, Show relPath) ⇒ Show (RelativePart userInfo hosts path relPath) where show = genericShow

type RelativePartOptions userInfo hosts path relPath =
  RelativePartParseOptions userInfo hosts path relPath
    (RelativePartPrintOptions userInfo hosts path relPath ())

type RelativePartParseOptions userInfo hosts path relPath r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseRelPath ∷ RelPath → Either URIPartParseError relPath
  | r
  )

type RelativePartPrintOptions userInfo hosts path relPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printRelPath ∷ relPath → RelPath
  | r
  )

type RelPath = Either PathAbsolute PathNoScheme

parser
  ∷ ∀ userInfo hosts path relPath r
  . Record (RelativePartParseOptions userInfo hosts path relPath r)
  → Parser String (RelativePart userInfo hosts path relPath)
parser opts = withAuth <|> withoutAuth
  where
  withAuth =
    RelativePartAuth
      <$> Authority.parser opts
      <*> optionMaybe (Path.parser opts.parsePath)
  withoutAuth =
    RelativePartNoAuth <$> noAuthPath
  noAuthPath
    = (Just <$> PathAbs.parse (opts.parseRelPath <<< Left))
    <|> (Just <$> PathNoScheme.parse (opts.parseRelPath <<< Right))
    <|> pure Nothing

print
  ∷ ∀ userInfo hosts path relPath r
  . Record (RelativePartPrintOptions userInfo hosts path relPath r)
  → RelativePart userInfo hosts path relPath → String
print opts = case _ of
  RelativePartAuth a p →
    String.joinWith "" $ Array.catMaybes
      [ pure $ Authority.print opts a
      , Path.print <<< opts.printPath <$> p
      ]
  RelativePartNoAuth p →
    maybe "" (either PathAbs.print PathNoScheme.print <<< opts.printRelPath) p

_authority
  ∷ ∀ userInfo hosts path relPath
  . Traversal'
      (RelativePart userInfo hosts path relPath)
      (Authority userInfo hosts)
_authority = wander \f → case _ of
  RelativePartAuth a p → flip RelativePartAuth p <$> f a
  a → pure a

_path
  ∷ ∀ userInfo hosts path relPath
  . Traversal'
      (RelativePart userInfo hosts path relPath)
      (Maybe path)
_path = wander \f → case _ of
  RelativePartAuth a p → RelativePartAuth a <$> f p
  a → pure a

_relPath
  ∷ ∀ userInfo hosts path relPath
  . Traversal'
      (RelativePart userInfo hosts path relPath)
      (Maybe relPath)
_relPath = wander \f a → case a of
  RelativePartNoAuth p → RelativePartNoAuth <$> f p
  _ → pure a
