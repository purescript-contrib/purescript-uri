module Data.URI.RelativePart
  ( RelativePart(..)
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
import Data.URI.Authority (Authority(..), Host(..), Port(..), UserInfo(..), _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Path as Path
import Text.Parsing.StringParser (Parser)

-- | The "relative part" of a relative reference.
data RelativePart userInfo path = RelativePart (Maybe (Authority userInfo)) (Maybe path)

derive instance eqRelativePart ∷ (Eq userInfo, Eq path) ⇒ Eq (RelativePart userInfo path)
derive instance ordRelativePart ∷ (Ord userInfo, Ord path) ⇒ Ord (RelativePart userInfo path)
derive instance genericRelativePart ∷ Generic (RelativePart userInfo path) _
instance showRelativePart ∷ (Show userInfo, Show path) ⇒ Show (RelativePart userInfo path) where show = genericShow

parser ∷ ∀ userInfo path. Parser userInfo → Parser path → Parser (RelativePart userInfo path)
parser parseUserInfo parsePath = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> Authority.parser parseUserInfo
      <*> Path.parsePathAbEmpty parsePath

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> Path.parsePathAbsolute parsePath)
    <|> (Just <$> Path.parsePathNoScheme parsePath)
    <|> pure Nothing

print ∷ ∀ userInfo path. (userInfo → String) → (path → String) → RelativePart userInfo path → String
print printUserInfo printPath (RelativePart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print printUserInfo <$> a
      , printPath <$> p
      ]

_authority ∷ ∀ userInfo path. Lens' (RelativePart userInfo path) (Maybe (Authority userInfo))
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path ∷ ∀ userInfo path. Lens' (RelativePart userInfo path) (Maybe path)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
