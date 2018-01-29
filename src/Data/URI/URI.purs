module Data.URI.URI
  ( URI(..)
  , URIOptions
  , URIParseOptions
  , parser
  , URIPrintOptions
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
import Data.URI.HierarchicalPart (Authority(..), HierarchicalPart(..), Host(..), Port(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Data.URI.HierarchicalPart as HPart
import Data.URI.Query as Query
import Data.URI.Scheme (Scheme(..))
import Data.URI.Scheme as Scheme
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A generic URI
data URI userInfo hierPath query fragment = URI Scheme (HierarchicalPart userInfo hierPath) (Maybe query) (Maybe fragment)

derive instance eqURI ∷ (Eq userInfo, Eq hierPath, Eq query, Eq fragment) ⇒ Eq (URI userInfo hierPath query fragment)
derive instance ordURI ∷ (Ord userInfo, Ord hierPath, Ord query, Ord fragment) ⇒ Ord (URI userInfo hierPath query fragment)
derive instance genericURI ∷ Generic (URI userInfo hierPath query fragment) _
instance showURI ∷ (Show userInfo, Show hierPath, Show query, Show fragment) ⇒ Show (URI userInfo hierPath query fragment) where show = genericShow

type URIOptions userInfo hierPath query fragment =
  URIParseOptions userInfo hierPath query fragment (URIPrintOptions userInfo hierPath query fragment ())

type URIParseOptions userInfo hierPath query fragment r =
  ( parseUserInfo ∷ Parser userInfo
  , parseHierPath ∷ Parser hierPath
  , parseQuery ∷ Parser query
  , parseFragment ∷ Parser fragment
  | r
  )

parser
  ∷ ∀ userInfo hierPath query fragment r
  . Record (URIParseOptions userInfo hierPath query fragment r)
  → Parser (URI userInfo hierPath query fragment)
parser opts = URI
  <$> Scheme.parser
  <*> HPart.parser opts
  <*> optionMaybe (Query.parser opts.parseQuery)
  <*> optionMaybe (Fragment.parser opts.parseFragment)
  <* eof

type URIPrintOptions userInfo hierPath query fragment r =
  ( printUserInfo ∷ userInfo → String
  , printHierPath ∷ hierPath → String
  , printQuery ∷ query → String
  , printFragment ∷ fragment → String
  | r
  )

print
  ∷ ∀ userInfo hierPath query fragment r
  . Record (URIPrintOptions userInfo hierPath query fragment r)
  → URI userInfo hierPath query fragment
  → String
print opts (URI s h q f) =
  S.joinWith "" $ catMaybes
    [ Just (Scheme.print s)
    , Just (HPart.print opts h)
    , Query.print opts.printQuery <$> q
    , Fragment.print opts.printFragment <$> f
    ]

_scheme
  ∷ ∀ userInfo hierPath query fragment
  . Lens' (URI userInfo hierPath query fragment) Scheme
_scheme =
  lens
    (\(URI s _ _ _) → s)
    (\(URI _ h q f) s → URI s h q f)

_hierPart
  ∷ ∀ userInfo hierPath query fragment
  . Lens' (URI userInfo hierPath query fragment) (HierarchicalPart userInfo hierPath)
_hierPart =
  lens
    (\(URI _ h _ _) → h)
    (\(URI s _ q f) h → URI s h q f)

_query
  ∷ ∀ userInfo hierPath query fragment
  . Lens' (URI userInfo hierPath query fragment) (Maybe query)
_query =
  lens
    (\(URI _ _ q _) → q)
    (\(URI s h _ f) q → URI s h q f)

_fragment
  ∷ ∀ userInfo hierPath query fragment
  . Lens' (URI userInfo hierPath query fragment) (Maybe fragment)
_fragment =
  lens
    (\(URI _ _ _ f) → f)
    (\(URI s h q _) f → URI s h q f)
