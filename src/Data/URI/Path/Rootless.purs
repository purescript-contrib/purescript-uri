module Data.URI.Path.Rootless where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.URI.Common (URIPartParseError, wrapParser)
import Data.URI.Path.Segment (PathSegment, PathSegmentNZ, parseSegment, parseSegmentNonZero, unsafeSegmentNZToString, unsafeSegmentToString)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)

newtype PathRootless = PathRootless (Tuple PathSegmentNZ (Array PathSegment))

derive instance eqPathRootless ∷ Eq PathRootless
derive instance ordPathRootless ∷ Ord PathRootless
derive instance genericPathRootless ∷ Generic PathRootless _
instance showPathRootless ∷ Show PathRootless where show = genericShow

parse ∷ ∀ p. (PathRootless → Either URIPartParseError p) → Parser String p
parse p = wrapParser p do
  head ← parseSegmentNonZero
  tail ← Array.many (char '/' *> parseSegment)
  pure (PathRootless (Tuple head tail))

print ∷ PathRootless → String
print = case _ of
  PathRootless (Tuple head []) →
    unsafeSegmentNZToString head
  PathRootless (Tuple head tail) →
    unsafeSegmentNZToString head
      <> "/"
      <> String.joinWith "/" (map unsafeSegmentToString tail)
