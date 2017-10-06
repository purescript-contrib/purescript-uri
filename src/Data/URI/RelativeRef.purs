module Data.URI.RelativeRef
  ( RelativeRef(..)
  , parse
  , parser
  , print
  , _relPart
  , _query
  , _fragment
  , module Data.URI.Fragment
  , module Data.URI.Query
  , module Data.URI.RelativePart
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Fragment as Fragment
import Data.URI.Fragment (Fragment(..))
import Data.URI.Query as Query
import Data.URI.Query (Query(..))
import Data.URI.RelativePart as RPart
import Data.URI.RelativePart (Authority(..), Host(..), Port(..), RelativePart(..), URIPath, URIPathAbs, URIPathRel, UserInfo(..), _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _userInfo)
import Text.Parsing.StringParser (Parser, ParseError, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)

-- | A relative reference for a URI.
data RelativeRef = RelativeRef RelativePart (Maybe Query) (Maybe Fragment)

derive instance eqRelativeRef ∷ Eq RelativeRef
derive instance ordRelativeRef ∷ Ord RelativeRef
derive instance genericRelativeRef ∷ Generic RelativeRef _
instance showRelativeRef ∷ Show RelativeRef where show = genericShow

parse ∷ String → Either ParseError RelativeRef
parse = runParser parser

parser ∷ Parser RelativeRef
parser = RelativeRef
  <$> RPart.parser
  <*> optionMaybe Query.parser
  <*> optionMaybe Fragment.parser
  <* eof

print ∷ RelativeRef → String
print (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (RPart.print h)
    , Query.print <$> q
    , Fragment.print <$> f
    ]

_relPart ∷ Lens' RelativeRef RelativePart
_relPart =
  lens
    (\(RelativeRef r _ _) → r)
    (\(RelativeRef _ q f) r → RelativeRef r q f)

_query ∷ Lens' RelativeRef (Maybe Query)
_query =
  lens
    (\(RelativeRef _ q _) → q)
    (\(RelativeRef r _ f) q → RelativeRef r q f)

_fragment ∷ Lens' RelativeRef (Maybe Fragment)
_fragment =
  lens
    (\(RelativeRef _ _ f) → f)
    (\(RelativeRef r q _) f → RelativeRef r q f)
