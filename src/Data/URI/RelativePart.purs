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

-- | The "relative part" of a relative reference.
data RelativePart userInfo hosts host port relPath = RelativePart (Maybe (Authority userInfo hosts host port)) (Maybe relPath)

derive instance eqRelativePart ∷ (Eq userInfo, Eq1 hosts, Eq host, Eq port, Eq relPath) ⇒ Eq (RelativePart userInfo hosts host port relPath)
derive instance ordRelativePart ∷ (Ord userInfo, Ord1 hosts, Ord host, Ord port, Ord relPath) ⇒ Ord (RelativePart userInfo hosts host port relPath)
derive instance genericRelativePart ∷ Generic (RelativePart userInfo hosts host port relPath) _
instance showRelativePart ∷ (Show userInfo, Show (hosts (Tuple host (Maybe port))), Show port, Show relPath) ⇒ Show (RelativePart userInfo hosts host port relPath) where show = genericShow

type RelativePartOptions userInfo hosts host port relPath =
  RelativePartParseOptions userInfo hosts host port relPath
    (RelativePartPrintOptions userInfo hosts host port relPath ())

type RelativePartParseOptions userInfo hosts host port relPath r =
  ( parseUserInfo ∷ UserInfo → Either ParseError userInfo
  , parseHosts ∷ ∀ a. Parser a → Parser (hosts a)
  , parseHost ∷ Host → Either ParseError host
  , parsePort ∷ Port → Either ParseError port
  , parseRelPath ∷ String → Either ParseError relPath
  | r
  )

type RelativePartPrintOptions userInfo hosts host port relPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts String → String
  , printHost ∷ host → Host
  , printPort ∷ port → Port
  , printRelPath ∷ relPath → String
  | r
  )

parser
  ∷ ∀ userInfo hosts host port relPath r
  . Record (RelativePartParseOptions userInfo hosts host port relPath r)
  → Parser (RelativePart userInfo hosts host port relPath)
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
  ∷ ∀ userInfo hosts host port relPath r
  . Functor hosts
  ⇒ Record (RelativePartPrintOptions userInfo hosts host port relPath r)
  → RelativePart userInfo hosts host port relPath → String
print opts (RelativePart a p) =
  String.joinWith "" $ Array.catMaybes
    [ Authority.print opts <$> a
    , opts.printRelPath <$> p
    ]

_authority
  ∷ ∀ userInfo hosts host port relPath
  . Lens'
      (RelativePart userInfo hosts host port relPath)
      (Maybe (Authority userInfo hosts host port))
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path
  ∷ ∀ userInfo hosts host port relPath
  . Lens'
      (RelativePart userInfo hosts host port relPath)
      (Maybe relPath)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
