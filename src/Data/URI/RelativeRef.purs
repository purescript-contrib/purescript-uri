module Data.URI.RelativeRef
  ( RelativeRef(..)
  , parser
  , print
  , _relPart
  , _query
  , _fragment
  , module Data.URI.RelativePart
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Fragment as Fragment
import Data.URI.Query as Query
import Data.URI.RelativePart as RPart
import Data.URI.RelativePart (Authority(..), Host(..), Port(..), RelativePart(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A relative reference for a URI.
data RelativeRef userInfo path query fragment = RelativeRef (RelativePart userInfo path) (Maybe query) (Maybe fragment)

derive instance eqRelativeRef ∷ (Eq userInfo, Eq path, Eq query, Eq fragment) ⇒ Eq (RelativeRef userInfo path query fragment)
derive instance ordRelativeRef ∷ (Ord userInfo, Ord path, Ord query, Ord fragment) ⇒ Ord (RelativeRef userInfo path query fragment)
derive instance genericRelativeRef ∷ Generic (RelativeRef userInfo path query fragment) _
instance showRelativeRef ∷ (Show userInfo, Show path, Show query, Show fragment) ⇒ Show (RelativeRef userInfo path query fragment) where show = genericShow
--
-- parse ∷ String → Either ParseError RelativeRef
-- parse = runParser parser

parser
  ∷ ∀ userInfo path query fragment
  . Parser userInfo
  → Parser path
  → Parser query
  → Parser fragment
  → Parser (RelativeRef userInfo path query fragment)
parser parseUserInfo parsePath parseQuery parseFragment =
  RelativeRef
    <$> RPart.parser parseUserInfo parsePath
    <*> optionMaybe (Query.parser parseQuery)
    <*> optionMaybe (Fragment.parser parseFragment)
    <* eof

print
  ∷ ∀ userInfo path query fragment
  . (userInfo → String)
  → (path → String)
  → (query → String)
  → (fragment → String)
  → RelativeRef userInfo path query fragment
  → String
print printUserInfo printPath printQuery printFragment (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (RPart.print printUserInfo printPath h)
    , Query.print printQuery <$> q
    , Fragment.print printFragment <$> f
    ]

_relPart
  ∷ ∀ userInfo path query fragment
  . Lens' (RelativeRef userInfo path query fragment) (RelativePart userInfo path)
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query
  ∷ ∀ userInfo path query fragment
  . Lens' (RelativeRef userInfo path query fragment) (Maybe query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment
  ∷ ∀ userInfo path query fragment
  . Lens' (RelativeRef userInfo path query fragment) (Maybe fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
