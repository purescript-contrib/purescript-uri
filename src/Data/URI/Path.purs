module Data.URI.Path where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String as String
import Data.URI.Common (URIPartParseError, wrapParser)
import Data.URI.Path.Segment (PathSegment, parseSegment, unsafeSegmentToString)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)

newtype Path = Path (Array PathSegment)

derive newtype instance eqPath ∷ Eq Path
derive newtype instance ordPath ∷ Ord Path
derive instance genericPath ∷ Generic Path _
instance showPath ∷ Show Path where show = genericShow

parser ∷ ∀ p. (Path → Either URIPartParseError p) → Parser String p
parser p = wrapParser p $ Path <$> Array.some (char '/' *> parseSegment)

print ∷ Path → String
print (Path segs) = "/" <> String.joinWith "/" (map unsafeSegmentToString segs)
