module URI.Path.Rootless where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Path.Segment (PathSegment, PathSegmentNZ, parseSegment, parseSegmentNonZero, unsafeSegmentNZToString, unsafeSegmentToString)

-- | A relative path, corresponding to _path-rootless_ in the spec. This path
-- | cannot start with the character `/` or be entirely empty. This type can
-- | appear in a hierarchical-part when there is no authority component.
newtype PathRootless = PathRootless (Tuple PathSegmentNZ (Array PathSegment))

derive instance eqPathRootless ∷ Eq PathRootless
derive instance ordPathRootless ∷ Ord PathRootless
derive instance genericPathRootless ∷ Generic PathRootless _
instance showPathRootless ∷ Show PathRootless where show = genericShow

-- | A parser for a _path-rootless_ URI component.
parse ∷ Parser String PathRootless
parse = do
  head ← parseSegmentNonZero
  tail ← Array.many (char '/' *> parseSegment)
  pure (PathRootless (Tuple head tail))

-- | A printer for a _path-rootless_ URI component.
print ∷ PathRootless → String
print = case _ of
  PathRootless (Tuple head []) →
    unsafeSegmentNZToString head
  PathRootless (Tuple head tail) →
    unsafeSegmentNZToString head
      <> "/"
      <> String.joinWith "/" (map unsafeSegmentToString tail)
