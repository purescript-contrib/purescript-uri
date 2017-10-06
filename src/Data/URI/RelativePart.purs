module Data.URI.RelativePart
  ( RelativePart(..)
  , parser
  , print
  , _authority
  , _path
  , module Data.URI.Authority
  , module Data.URI.Path
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
import Data.URI.Path (URIPath, URIPathAbs, URIPathRel)
import Data.URI.Path as Path
import Text.Parsing.StringParser (Parser)

-- | The "relative part" of a relative reference.
data RelativePart = RelativePart (Maybe Authority) (Maybe URIPathRel)

derive instance eqRelativePart ∷ Eq RelativePart
derive instance ordRelativePart ∷ Ord RelativePart
derive instance genericRelativePart ∷ Generic RelativePart _
instance showRelativePart ∷ Show RelativePart where show = genericShow

parser ∷ Parser RelativePart
parser = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> Authority.parser
      <*> Path.parsePathAbEmpty Path.parseURIPathRel

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> Path.parsePathAbsolute Path.parseURIPathRel)
    <|> (Just <$> Path.parsePathNoScheme Path.parseURIPathRel)
    <|> pure Nothing

print ∷ RelativePart → String
print (RelativePart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print <$> a
      , Path.printPath <$> p
      ]

_authority ∷ Lens' RelativePart (Maybe Authority)
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path ∷ Lens' RelativePart (Maybe URIPathRel)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
