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
data RelativeRef userInfo relPath query fragment = RelativeRef (RelativePart userInfo relPath) (Maybe query) (Maybe fragment)

derive instance eqRelativeRef ∷ (Eq userInfo, Eq relPath, Eq query, Eq fragment) ⇒ Eq (RelativeRef userInfo relPath query fragment)
derive instance ordRelativeRef ∷ (Ord userInfo, Ord relPath, Ord query, Ord fragment) ⇒ Ord (RelativeRef userInfo relPath query fragment)
derive instance genericRelativeRef ∷ Generic (RelativeRef userInfo relPath query fragment) _
instance showRelativeRef ∷ (Show userInfo, Show relPath, Show query, Show fragment) ⇒ Show (RelativeRef userInfo relPath query fragment) where show = genericShow

parser
  ∷ ∀ userInfo relPath query fragment r
  . { parseUserInfo ∷ Parser userInfo
    , parseRelPath ∷ Parser relPath
    , parseQuery ∷ Parser query
    , parseFragment ∷ Parser fragment
    | r
    }
  → Parser (RelativeRef userInfo relPath query fragment)
parser opts =
  RelativeRef
    <$> RPart.parser opts
    <*> optionMaybe (Query.parser opts.parseQuery)
    <*> optionMaybe (Fragment.parser opts.parseFragment)
    <* eof

print
  ∷ ∀ userInfo relPath query fragment r
  . { printUserInfo ∷ userInfo → String
    , printRelPath ∷ relPath → String
    , printQuery ∷ query → String
    , printFragment ∷ fragment → String
    | r
    }
  → RelativeRef userInfo relPath query fragment
  → String
print opts (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (RPart.print opts h)
    , Query.print opts.printQuery <$> q
    , Fragment.print opts.printFragment <$> f
    ]

_relPart
  ∷ ∀ userInfo relPath query fragment
  . Lens' (RelativeRef userInfo relPath query fragment) (RelativePart userInfo relPath)
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query
  ∷ ∀ userInfo relPath query fragment
  . Lens' (RelativeRef userInfo relPath query fragment) (Maybe query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment
  ∷ ∀ userInfo relPath query fragment
  . Lens' (RelativeRef userInfo relPath query fragment) (Maybe fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
