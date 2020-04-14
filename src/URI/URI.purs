module URI.URI
  ( URI(..)
  , URIOptions
  , URIParseOptions
  , URIPrintOptions
  , parser
  , print
  , _scheme
  , _hierPart
  , _query
  , _fragment
  , module URI.HierarchicalPart
  , module URI.Query
  , module URI.Scheme
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as String
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (eof)
import URI.Common (URIPartParseError, wrapParser)
import URI.Fragment (Fragment)
import URI.Fragment as Fragment
import URI.HierarchicalPart (Authority(..), AuthorityOptions, AuthorityParseOptions, AuthorityPrintOptions, HierPath, HierarchicalPart(..), HierarchicalPartOptions, HierarchicalPartParseOptions, HierarchicalPartPrintOptions, Host(..), IPv4Address, IPv6Address, Path(..), PathAbsolute(..), PathRootless(..), Port, RegName, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _authority, _hierPath, _hosts, _path, _userInfo)
import URI.HierarchicalPart as HPart
import URI.Query (Query)
import URI.Query as Query
import URI.Scheme (Scheme)
import URI.Scheme as Scheme

-- | A general purpose absolute URI - similar to `AbsoluteURI` but also admits
-- | a fragment component. An absolute URI can still contain relative paths
-- | but is required to have a `Scheme` component.
data URI userInfo hosts path hierPath query fragment = URI Scheme (HierarchicalPart userInfo hosts path hierPath) (Maybe query) (Maybe fragment)

derive instance eqURI ∷ (Eq userInfo, Eq hosts, Eq path, Eq hierPath, Eq query, Eq fragment) ⇒ Eq (URI userInfo hosts path hierPath query fragment)
derive instance ordURI ∷ (Ord userInfo, Ord hosts, Ord path, Ord hierPath, Ord query, Ord fragment) ⇒ Ord (URI userInfo hosts path hierPath query fragment)
derive instance genericURI ∷ Generic (URI userInfo hosts path hierPath query fragment) _
instance showURI ∷ (Show userInfo, Show hosts, Show path, Show hierPath, Show query, Show fragment) ⇒ Show (URI userInfo hosts path hierPath query fragment) where show = genericShow

-- | A row type for describing the options fields used by the URI parser and
-- | printer.
-- |
-- | Used as `Record (URIOptions userInfo hosts path hierPath query fragment)`
-- | when type anotating an options record.
-- |
-- | See below for details of how to use these configuration options.
type URIOptions userInfo hosts path hierPath query fragment =
  URIParseOptions userInfo hosts path hierPath query fragment
    (URIPrintOptions userInfo hosts path hierPath query fragment ())

-- | A row type for describing the options fields used by the URI parser.
-- |
-- | Used as `Record (URIParseOptions userInfo hosts path hierPath query fragment ())`
-- | when type anotating an options record.
-- |
-- | Having this options record allows custom representations to be used for
-- | the URI components. If this is not necessary, `pure` can be used for all
-- | the options aside from `parseHosts`, which will typically be
-- | `HostPortPair.parseHosts pure pure`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair parser.
type URIParseOptions userInfo hosts path hierPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseHierPath ∷ Either PathAbsolute PathRootless → Either URIPartParseError hierPath
  , parseQuery ∷ Query → Either URIPartParseError query
  , parseFragment ∷ Fragment → Either URIPartParseError fragment
  | r
  )

-- | A row type for describing the options fields used by the URI printer.
-- |
-- | Used as `Record (URIPrintOptions userInfo hosts path hierPath query fragment ())`
-- | when type anotating an options record.
-- |
-- | As a reverse of the parse options, this specifies how to print values back
-- | from custom representations. If this is not necessary, `identity` can be used for
-- | all the options aside from `printHosts`, which will typically be
-- | `HostPortPair.printHosts identity identity`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair printer.
type URIPrintOptions userInfo hosts path hierPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → Either PathAbsolute PathRootless
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

-- | A parser for a URI.
parser
  ∷ ∀ userInfo hosts path hierPath query fragment r
  . Record (URIParseOptions userInfo hosts path hierPath query fragment r)
  → Parser String (URI userInfo hosts path hierPath query fragment)
parser opts = URI
  <$> Scheme.parser
  <*> HPart.parser opts
  <*> optionMaybe (wrapParser opts.parseQuery Query.parser)
  <*> optionMaybe (wrapParser opts.parseFragment Fragment.parser)
  <* eof

-- | A printer for a URI.
print
  ∷ ∀ userInfo hosts path hierPath query fragment r
  . Record (URIPrintOptions userInfo hosts path hierPath query fragment r)
  → URI userInfo hosts path hierPath query fragment
  → String
print opts (URI s h q f) =
  String.joinWith "" $ Array.catMaybes
    [ Just (Scheme.print s)
    , Just (HPart.print opts h)
    , Query.print <<< opts.printQuery <$> q
    , Fragment.print <<< opts.printFragment <$> f
    ]

-- | The scheme component of a URI.
_scheme
  ∷ ∀ userInfo hosts path hierPath query fragment
  . Lens'
      (URI userInfo hosts path hierPath query fragment)
      Scheme
_scheme =
  lens
    (\(URI s _ _ _) → s)
    (\(URI _ h q f) s → URI s h q f)

-- | The hierarchical-part component of a URI.
_hierPart
  ∷ ∀ userInfo hosts path hierPath query fragment
  . Lens'
      (URI userInfo hosts path hierPath query fragment)
      (HierarchicalPart userInfo hosts path hierPath)
_hierPart =
  lens
    (\(URI _ h _ _) → h)
    (\(URI s _ q f) h → URI s h q f)

-- | The query component of a URI.
_query
  ∷ ∀ userInfo hosts path hierPath query fragment
  . Lens'
      (URI userInfo hosts path hierPath query fragment)
      (Maybe query)
_query =
  lens
    (\(URI _ _ q _) → q)
    (\(URI s h _ f) q → URI s h q f)

-- | The fragment component of a URI.
_fragment
  ∷ ∀ userInfo hosts path hierPath query fragment
  . Lens'
      (URI userInfo hosts path hierPath query fragment)
      (Maybe fragment)
_fragment =
  lens
    (\(URI _ _ _ f) → f)
    (\(URI s h q _) f → URI s h q f)
