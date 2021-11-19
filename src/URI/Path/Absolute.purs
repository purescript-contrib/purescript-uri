module URI.Path.Absolute where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (char)
import URI.Path.Segment (PathSegment, PathSegmentNZ, parseSegment, parseSegmentNZ, printSegmentNZ, printSegment)

-- | An absolute path, corresponding to _path-absolute_ in the spec. This path
-- | cannot represent the value `//` - it must either be `/`, or start with a
-- | segment that is not empty, for example: `/something`, `/.`, `/..`. This
-- | type can appear in both hierarchical-part and relative-parts to represent
-- | an absolute path when no authority component is present.
-- |
-- | This restriction exists as a value begining with `//` at this point in the
-- | grammar must be an authority, attempting to decide whether a value is an
-- | authority or a path would be ambiguous if `//` paths were allowed. The `//`
-- | path means the same thing as `/` anyway!
newtype PathAbsolute = PathAbsolute (Maybe (Tuple PathSegmentNZ (Array PathSegment)))

derive instance eqPathAbsolute :: Eq PathAbsolute
derive instance ordPathAbsolute :: Ord PathAbsolute
derive instance genericPathAbsolute :: Generic PathAbsolute _
instance showPathAbsolute :: Show PathAbsolute where
  show = genericShow

-- | A parser for a _path-absolute_ URI component.
parse :: Parser String PathAbsolute
parse = do
  _ <- char '/'
  optionMaybe parseSegmentNZ >>= case _ of
    Just head ->
      PathAbsolute
        <<< Just
        <<< Tuple head
        <<< Array.fromFoldable
        <$> List.manyRec (char '/' *> parseSegment)
    Nothing ->
      pure (PathAbsolute Nothing)

-- | A printer for a _path-absolute_ URI component.
print :: PathAbsolute -> String
print = case _ of
  PathAbsolute Nothing ->
    "/"
  PathAbsolute (Just (Tuple head [])) ->
    "/" <> printSegmentNZ head
  PathAbsolute (Just (Tuple head tail)) ->
    "/"
      <> printSegmentNZ head
      <> "/"
      <> String.joinWith "/" (map printSegment tail)
