module Data.URI.Path where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String as String
import Data.URI.Common (wrapParser)
import Data.URI.Path.Segment (PathSegment, parseSegment, unsafeSegmentToString)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.String (string)

newtype Path = Path (Array PathSegment)

derive newtype instance eqPath ∷ Eq Path
derive newtype instance ordPath ∷ Ord Path
derive instance genericPath ∷ Generic Path _
instance showPath ∷ Show Path where show = genericShow

parser ∷ ∀ p. (Path → Either ParseError p) → Parser p
parser p = wrapParser p $ Path <$> Array.some (string "/" *> parseSegment)

print ∷ Path → String
print (Path segs) = "/" <> String.joinWith "/" (map unsafeSegmentToString segs)
