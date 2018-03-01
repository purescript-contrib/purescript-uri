module URI.Path where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid (class Monoid)
import Data.String as String
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Path.Segment (PathSegment, parseSegment, unsafeSegmentToString)

newtype Path = Path (Array PathSegment)

derive newtype instance eqPath ∷ Eq Path
derive newtype instance ordPath ∷ Ord Path
derive newtype instance semigroupPath ∷ Semigroup Path
derive newtype instance monoidPath ∷ Monoid Path
derive instance genericPath ∷ Generic Path _
instance showPath ∷ Show Path where show = genericShow

parser ∷ Parser String Path
parser = Path <$> Array.some (char '/' *> parseSegment)

print ∷ Path → String
print (Path segs) = "/" <> String.joinWith "/" (map unsafeSegmentToString segs)
