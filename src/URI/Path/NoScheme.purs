module URI.Path.NoScheme where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (URIPartParseError, wrapParser)
import URI.Path.Segment (PathSegment, PathSegmentNZNC, parseSegment, parseSegmentNonZeroNoColon, unsafeSegmentNZNCToString, unsafeSegmentToString)

newtype PathNoScheme = PathNoScheme (Tuple PathSegmentNZNC (Array PathSegment))

derive instance eqPathNoScheme ∷ Eq PathNoScheme
derive instance ordPathNoScheme ∷ Ord PathNoScheme
derive instance genericPathNoScheme ∷ Generic PathNoScheme _
instance showPathNoScheme ∷ Show PathNoScheme where show = genericShow

parse ∷ ∀ p. (PathNoScheme → Either URIPartParseError p) → Parser String p
parse p = wrapParser p do
  head ← parseSegmentNonZeroNoColon
  tail ← Array.many (char '/' *> parseSegment)
  pure (PathNoScheme (Tuple head tail))

print ∷ PathNoScheme → String
print (PathNoScheme (Tuple head tail)) =
  case tail of
    [] → unsafeSegmentNZNCToString head
    ps → unsafeSegmentNZNCToString head <> "/" <> String.joinWith "/" (map unsafeSegmentToString tail)
