module Data.URI.AbsoluteURI
  ( AbsoluteURI(..)
  , parser
  , print
  , _scheme
  , _hierPart
  , _query
  , module Data.URI.HierarchicalPart
  , module Data.URI.Scheme
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.HierarchicalPart as HPart
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), UserInfo(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Data.URI.Scheme (Scheme(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | An absolute AbsoluteURI.
data AbsoluteURI userInfo path query = AbsoluteURI Scheme (HierarchicalPart userInfo path) (Maybe query)

derive instance eqAbsoluteURI ∷ (Eq userInfo, Eq path, Eq query) ⇒ Eq (AbsoluteURI userInfo path query)
derive instance ordAbsoluteURI ∷ (Ord userInfo, Ord path, Ord query) ⇒ Ord (AbsoluteURI userInfo path query)
derive instance genericAbsoluteURI ∷ Generic (AbsoluteURI userInfo path query) _
instance showAbsoluteURI ∷ (Show userInfo, Show path, Show query) ⇒ Show (AbsoluteURI userInfo path query) where show = genericShow

parser
  ∷ ∀ userInfo path query
  . Parser userInfo
  → Parser path
  → Parser query
  → Parser (AbsoluteURI userInfo path query)
parser parseUserInfo parsePath parseQuery = AbsoluteURI
  <$> Scheme.parser
  <*> HPart.parser parseUserInfo parsePath
  <*> optionMaybe (Query.parser' parseQuery)
  <* eof

print
  ∷ ∀ userInfo path query
  . (userInfo → String)
  → (path → String)
  → (query → String)
  → AbsoluteURI userInfo path query
  → String
print printUserInfo printPath printQuery (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ Just (Scheme.print s)
    , Just (HPart.print printUserInfo printPath h)
    , Query.print' printQuery <$> q
    ]

_scheme
  ∷ ∀ userInfo path query
  . Lens' (AbsoluteURI userInfo path query) Scheme
_scheme =
  lens
    (\(AbsoluteURI s _ _) → s)
    (\(AbsoluteURI _ h q) s → AbsoluteURI s h q)

_hierPart
  ∷ ∀ userInfo path query
  . Lens' (AbsoluteURI userInfo path query) (HierarchicalPart userInfo path)
_hierPart =
  lens
    (\(AbsoluteURI _ h _) → h)
    (\(AbsoluteURI s _ q) h → AbsoluteURI s h q)

_query
  ∷ ∀ userInfo path query
  . Lens' (AbsoluteURI userInfo path query) (Maybe query)
_query =
  lens
    (\(AbsoluteURI _ _ q) → q)
    (\(AbsoluteURI s h _) q → AbsoluteURI s h q)
