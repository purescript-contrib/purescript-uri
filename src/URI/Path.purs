module URI.Path where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String as String
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Path.Segment (PathSegment, parseSegment, unsafeSegmentToString)

-- | A generic absolute-or-empty path, used in both hierarchical-part and
-- | relative-parts when an authority component is present. Corresponds to
-- | _path-abempty_ in the spec.
-- |
-- | A path value of `/` corresponds to `Path [""]`, an empty path is `Path []`.
newtype Path = Path (Array PathSegment)

derive newtype instance eqPath ∷ Eq Path
derive newtype instance ordPath ∷ Ord Path
derive newtype instance semigroupPath ∷ Semigroup Path
derive newtype instance monoidPath ∷ Monoid Path
derive instance genericPath ∷ Generic Path _
instance showPath ∷ Show Path where show = genericShow

-- | A parser for a _path-abempty_ URI component.
parser ∷ Parser String Path
parser = Path <$> Array.many (char '/' *> parseSegment)

-- | A printer for a _path-abempty_ URI component.
print ∷ Path → String
print (Path segs)
  | Array.null segs = ""
  | otherwise = "/" <> String.joinWith "/" (map unsafeSegmentToString segs)
