module Data.URI.RelativePart
  ( RelativePart(..)
  , RelativePartOptions
  , RelativePartParseOptions
  , RelativePartPrintOptions
  , parser
  , print
  , _authority
  , _path
  , module Data.URI.Authority
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Either (Either)
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
import Text.Parsing.StringParser (ParseError, Parser)

-- | The "relative part" of a relative reference.
data RelativePart userInfo hosts relPath = RelativePart (Maybe (Authority userInfo hosts)) (Maybe relPath)

derive instance eqRelativePart ∷ (Eq userInfo, Eq1 hosts, Eq relPath) ⇒ Eq (RelativePart userInfo hosts relPath)
derive instance ordRelativePart ∷ (Ord userInfo, Ord1 hosts, Ord relPath) ⇒ Ord (RelativePart userInfo hosts relPath)
derive instance genericRelativePart ∷ Generic (RelativePart userInfo hosts relPath) _
instance showRelativePart ∷ (Show userInfo, Show (hosts (Tuple Host (Maybe Port))), Show relPath) ⇒ Show (RelativePart userInfo hosts relPath) where show = genericShow

type RelativePartOptions userInfo hosts relPath =
  RelativePartParseOptions userInfo hosts relPath
    (RelativePartPrintOptions userInfo hosts relPath ())

type RelativePartParseOptions userInfo hosts relPath r =
  ( parseUserInfo ∷ String → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseRelPath ∷ String → Either ParseError relPath
  | r
  )

type RelativePartPrintOptions userInfo hosts relPath r =
  ( printUserInfo ∷ userInfo → String
  , printHosts ∷ hosts String → String
  , printRelPath ∷ relPath → String
  | r
  )

parser
  ∷ ∀ userInfo hosts relPath r
  . Record (RelativePartParseOptions userInfo hosts relPath r)
  → Parser (RelativePart userInfo hosts relPath)
parser opts = withAuth <|> withoutAuth
  where
  withAuth =
    RelativePart <<< Just
      <$> Authority.parser opts
      <*> Path.parsePathAbEmpty opts.parseRelPath

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> Path.parsePathAbsolute opts.parseRelPath)
    <|> (Just <$> Path.parsePathNoScheme opts.parseRelPath)
    <|> pure Nothing

print
  ∷ ∀ userInfo hosts relPath r
  . Functor hosts
  ⇒ Record (RelativePartPrintOptions userInfo hosts relPath r)
  → RelativePart userInfo hosts relPath → String
print opts (RelativePart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print opts <$> a
      , opts.printRelPath <$> p
      ]

_authority ∷ ∀ userInfo hosts relPath. Lens' (RelativePart userInfo hosts relPath) (Maybe (Authority userInfo hosts))
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path ∷ ∀ userInfo hosts relPath. Lens' (RelativePart userInfo hosts relPath) (Maybe relPath)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
