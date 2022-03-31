module URI.Path.Rootless where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Parsing (Parser)
import Parsing.String (char)
import URI.Path.Segment (PathSegment, PathSegmentNZ, parseSegment, parseSegmentNZ, printSegmentNZ, printSegment)

-- | A relative path, corresponding to _path-rootless_ in the spec. This path
-- | cannot start with the character `/` or be entirely empty. This type can
-- | appear in a hierarchical-part when there is no authority component.
newtype PathRootless = PathRootless (Tuple PathSegmentNZ (Array PathSegment))

derive instance eqPathRootless :: Eq PathRootless
derive instance ordPathRootless :: Ord PathRootless
derive instance genericPathRootless :: Generic PathRootless _

instance showPathRootless :: Show PathRootless where
  show = genericShow

-- | A parser for a _path-rootless_ URI component.
parse :: Parser String PathRootless
parse = do
  head <- parseSegmentNZ
  tail <- List.manyRec (char '/' *> parseSegment)
  pure (PathRootless (Tuple head (Array.fromFoldable tail)))

-- | A printer for a _path-rootless_ URI component.
print :: PathRootless -> String
print = case _ of
  PathRootless (Tuple head []) ->
    printSegmentNZ head
  PathRootless (Tuple head tail) ->
    printSegmentNZ head
      <> "/"
      <> String.joinWith "/" (map printSegment tail)
