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

-- | A generic absolute path. Used in both hierarchical-part and relative-parts
-- | when an authority component is present. Corresponds to path-abempty in
-- | the spec (although the empty part is tracked in a `Maybe` in
-- | `HierarchicalPart` or `RelativePart` in this library).
newtype Path = Path (Array PathSegment) -- TODO: this should use `NonEmptyArray`

derive newtype instance eqPath ∷ Eq Path
derive newtype instance ordPath ∷ Ord Path
derive newtype instance semigroupPath ∷ Semigroup Path
derive newtype instance monoidPath ∷ Monoid Path
derive instance genericPath ∷ Generic Path _
instance showPath ∷ Show Path where show = genericShow

-- | A parser for a generic absolute path URI component.
parser ∷ Parser String Path
parser = Path <$> Array.some (char '/' *> parseSegment)

-- | A printer for a generic absolute path URI component.
print ∷ Path → String
print (Path segs) = "/" <> String.joinWith "/" (map unsafeSegmentToString segs)
