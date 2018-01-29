module Data.URI.URI
  ( URI(..)
  , parser
  , print
  , _scheme
  , _hierPart
  , _query
  , _fragment
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
import Data.URI.Fragment as Fragment
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), UserInfo(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query as Query
import Data.URI.Scheme (Scheme(..))
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A generic URI
data URI userInfo path query fragment = URI Scheme (HierarchicalPart userInfo path) (Maybe query) (Maybe fragment)

derive instance eqURI ∷ (Eq userInfo, Eq path, Eq query, Eq fragment) ⇒ Eq (URI userInfo path query fragment)
derive instance ordURI ∷ (Ord userInfo, Ord path, Ord query, Ord fragment) ⇒ Ord (URI userInfo path query fragment)
derive instance genericURI ∷ Generic (URI userInfo path query fragment) _
instance showURI ∷ (Show userInfo, Show path, Show query, Show fragment) ⇒ Show (URI userInfo path query fragment) where show = genericShow

parser
  ∷ ∀ userInfo path query fragment
  . Parser userInfo
  → Parser path
  → Parser query
  → Parser fragment
  → Parser (URI userInfo path query fragment)
parser parseUserInfo parsePath parseQuery parseFragment = URI
  <$> Scheme.parser
  <*> HPart.parser parseUserInfo parsePath
  <*> optionMaybe (Query.parser' parseQuery)
  <*> optionMaybe (Fragment.parser' parseFragment)
  <* eof

print
  ∷ ∀ userInfo path query fragment
  . (userInfo → String)
  → (path → String)
  → (query → String)
  → (fragment → String)
  → URI userInfo path query fragment
  → String
print printUserInfo printPath printQuery printFragment (URI s h q f) =
  S.joinWith "" $ catMaybes
    [ Just (Scheme.print s)
    , Just (HPart.print printUserInfo printPath h)
    , Query.print' printQuery <$> q
    , Fragment.print' printFragment <$> f
    ]

_scheme
  ∷ ∀ userInfo path query fragment
  . Lens' (URI userInfo path query fragment) Scheme
_scheme =
  lens
    (\(URI s _ _ _) → s)
    (\(URI _ h q f) s → URI s h q f)

_hierPart
  ∷ ∀ userInfo path query fragment
  . Lens' (URI userInfo path query fragment) (HierarchicalPart userInfo path)
_hierPart =
  lens
    (\(URI _ h _ _) → h)
    (\(URI s _ q f) h → URI s h q f)

_query
  ∷ ∀ userInfo path query fragment
  . Lens' (URI userInfo path query fragment) (Maybe query)
_query =
  lens
    (\(URI _ _ q _) → q)
    (\(URI s h _ f) q → URI s h q f)

_fragment
  ∷ ∀ userInfo path query fragment
  . Lens' (URI userInfo path query fragment) (Maybe fragment)
_fragment =
  lens
    (\(URI _ _ _ f) → f)
    (\(URI s h q _) f → URI s h q f)
