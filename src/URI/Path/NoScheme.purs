module URI.Path.NoScheme where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Path.Segment (PathSegment, PathSegmentNZNC, parseSegment, parseSegmentNZNC, printSegmentNZNC, printSegment)

-- | A relative path that doesn't look like a URI scheme, corresponding to
-- | _path-noscheme_ in the spec. This path cannot start with the character
-- | `/`, contain the character `:` before the first `/`, or be entirely empty.
-- | This type can appear in a relative-part when there is no authority
-- | component.
newtype PathNoScheme = PathNoScheme (Tuple PathSegmentNZNC (Array PathSegment))

derive instance eqPathNoScheme :: Eq PathNoScheme
derive instance ordPathNoScheme :: Ord PathNoScheme
derive instance genericPathNoScheme :: Generic PathNoScheme _

instance showPathNoScheme :: Show PathNoScheme where
  show = genericShow

-- | A parser for a _path-noscheme_ URI component.
parse :: Parser String PathNoScheme
parse = do
  head <- parseSegmentNZNC
  tail <- List.manyRec (char '/' *> parseSegment)
  pure (PathNoScheme (Tuple head (Array.fromFoldable tail)))

-- | A printer for a _path-noscheme_ URI component.
print :: PathNoScheme -> String
print (PathNoScheme (Tuple head tail)) =
  case tail of
    [] -> printSegmentNZNC head
    _ -> printSegmentNZNC head <> "/" <> String.joinWith "/" (map printSegment tail)
