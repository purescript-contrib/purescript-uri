module Data.URI.Path.Absolute where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.URI.Common (wrapParser)
import Data.URI.Path.Segment (PathSegment, PathSegmentNZ, parseSegment, parseSegmentNonZero, unsafeSegmentNZToString, unsafeSegmentToString)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string)

newtype PathAbsolute = PathAbsolute (Maybe (Tuple PathSegmentNZ (Array PathSegment)))

derive instance eqPathAbsolute ∷ Eq PathAbsolute
derive instance ordPathAbsolute ∷ Ord PathAbsolute
derive instance genericPathAbsolute ∷ Generic PathAbsolute _
instance showPathAbsolute ∷ Show PathAbsolute where show = genericShow

parse ∷ ∀ p. (PathAbsolute → Either ParseError p) → Parser p
parse p = wrapParser p do
  _ ← string "/"
  optionMaybe parseSegmentNonZero >>= case _ of
    Just head →
      PathAbsolute <<< Just <<< Tuple head <$> Array.many (string "/" *> parseSegment)
    Nothing →
      pure (PathAbsolute Nothing)

print ∷ PathAbsolute → String
print = case _ of
  PathAbsolute Nothing →
    "/"
  PathAbsolute (Just (Tuple head tail)) →
    "/"
      <> unsafeSegmentNZToString head
      <> String.joinWith "/" (map unsafeSegmentToString tail)
