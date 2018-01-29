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
import Data.URI.Authority (Authority(..), Host(..), Port(..), _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import Data.URI.Authority as Authority
import Data.URI.Path as Path
import Text.Parsing.StringParser (Parser)

-- | The "relative part" of a relative reference.
data RelativePart userInfo relPath = RelativePart (Maybe (Authority userInfo)) (Maybe relPath)

derive instance eqRelativePart ∷ (Eq userInfo, Eq relPath) ⇒ Eq (RelativePart userInfo relPath)
derive instance ordRelativePart ∷ (Ord userInfo, Ord relPath) ⇒ Ord (RelativePart userInfo relPath)
derive instance genericRelativePart ∷ Generic (RelativePart userInfo relPath) _
instance showRelativePart ∷ (Show userInfo, Show relPath) ⇒ Show (RelativePart userInfo relPath) where show = genericShow

parser
  ∷ ∀ userInfo relPath r
  . { parseUserInfo ∷ Parser userInfo
    , parseRelPath ∷ Parser relPath
    | r
    }
  → Parser (RelativePart userInfo relPath)
parser opts = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> Authority.parser opts
      <*> Path.parsePathAbEmpty opts.parseRelPath

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> Path.parsePathAbsolute opts.parseRelPath)
    <|> (Just <$> Path.parsePathNoScheme opts.parseRelPath)
    <|> pure Nothing

print
  ∷ ∀ userInfo relPath r
  . { printUserInfo ∷ userInfo → String
    , printRelPath ∷ relPath → String
    | r
    }
  → RelativePart userInfo relPath → String
print opts (RelativePart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print opts <$> a
      , opts.printRelPath <$> p
      ]

_authority ∷ ∀ userInfo relPath. Lens' (RelativePart userInfo relPath) (Maybe (Authority userInfo))
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path ∷ ∀ userInfo relPath. Lens' (RelativePart userInfo relPath) (Maybe relPath)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
