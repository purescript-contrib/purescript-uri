module Data.URI.AbsoluteURI
  ( AbsoluteURI(..)
  , AbsoluteURIParseOptions
  , parser
  , AbsoluteURIPrintOptions
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
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Data.URI.Scheme (Scheme(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | An absolute AbsoluteURI.
data AbsoluteURI userInfo hierPath query = AbsoluteURI Scheme (HierarchicalPart userInfo hierPath) (Maybe query)

derive instance eqAbsoluteURI ∷ (Eq userInfo, Eq hierPath, Eq query) ⇒ Eq (AbsoluteURI userInfo hierPath query)
derive instance ordAbsoluteURI ∷ (Ord userInfo, Ord hierPath, Ord query) ⇒ Ord (AbsoluteURI userInfo hierPath query)
derive instance genericAbsoluteURI ∷ Generic (AbsoluteURI userInfo hierPath query) _
instance showAbsoluteURI ∷ (Show userInfo, Show hierPath, Show query) ⇒ Show (AbsoluteURI userInfo hierPath query) where show = genericShow

type AbsoluteURIParseOptions userInfo hierPath query r =
  ( parseUserInfo ∷ Parser userInfo
  , parseHierPath ∷ Parser hierPath
  , parseQuery ∷ Parser query
  | r
  )

parser
  ∷ ∀ userInfo hierPath query r
  . Record (AbsoluteURIParseOptions userInfo hierPath query r)
  → Parser (AbsoluteURI userInfo hierPath query)
parser opts = AbsoluteURI
  <$> Scheme.parser
  <*> HPart.parser opts
  <*> optionMaybe (Query.parser opts.parseQuery)
  <* eof

type AbsoluteURIPrintOptions userInfo hierPath query r =
  ( printUserInfo ∷ userInfo → String
  , printHierPath ∷ hierPath → String
  , printQuery ∷ query → String
  | r
  )

print
  ∷ ∀ userInfo hierPath query r
  . Record (AbsoluteURIPrintOptions userInfo hierPath query r)
  → AbsoluteURI userInfo hierPath query
  → String
print opts (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ Just (Scheme.print s)
    , Just (HPart.print opts h)
    , Query.print opts.printQuery <$> q
    ]

_scheme
  ∷ ∀ userInfo hierPath query
  . Lens' (AbsoluteURI userInfo hierPath query) Scheme
_scheme =
  lens
    (\(AbsoluteURI s _ _) → s)
    (\(AbsoluteURI _ h q) s → AbsoluteURI s h q)

_hierPart
  ∷ ∀ userInfo hierPath query
  . Lens' (AbsoluteURI userInfo hierPath query) (HierarchicalPart userInfo hierPath)
_hierPart =
  lens
    (\(AbsoluteURI _ h _) → h)
    (\(AbsoluteURI s _ q) h → AbsoluteURI s h q)

_query
  ∷ ∀ userInfo hierPath query
  . Lens' (AbsoluteURI userInfo hierPath query) (Maybe query)
_query =
  lens
    (\(AbsoluteURI _ _ q) → q)
    (\(AbsoluteURI s h _) q → AbsoluteURI s h q)
