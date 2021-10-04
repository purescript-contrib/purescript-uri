module URI.Path.Segment
  ( PathSegment
  , segmentFromString
  , segmentToString
  , unsafeSegmentFromString
  , unsafeSegmentToString
  , parseSegment
  , printSegment
  , PathSegmentNZ
  , segmentNZFromString
  , segmentNZToString
  , unsafeSegmentNZFromString
  , unsafeSegmentNZToString
  , parseSegmentNZ
  , printSegmentNZ
  , PathSegmentNZNC
  , segmentNZNCFromString
  , segmentNZNCToString
  , unsafeSegmentNZNCFromString
  , unsafeSegmentNZNCToString
  , parseSegmentNZNC
  , printSegmentNZNC
  , segmentChar
  , segmentNCChar
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty as NEA
import Data.List as List
import Data.Maybe (fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (join1With, joinWith, toString) as NES
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import JSURI (decodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (decodeURIComponent', pctEncoded, printEncoded, printEncoded', subDelims, unreserved)

-- | A path segment. Can be empty, as this is required to represent some paths -
-- | segments are joined together with slashes, so in cases where a path
-- | contains multiple contiguous slashes this is represented by a group of
-- | empty path segments. Corresponds to _segment_ in the spec.
newtype PathSegment = PathSegment String

derive newtype instance eqPathSegment ∷ Eq PathSegment
derive newtype instance ordPathSegment ∷ Ord PathSegment

instance showPathSegment ∷ Show PathSegment where
  show (PathSegment s) = "(Segment.unsafeSegmentToString " <> show s <> ")"

-- | Constructs a segment value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeSegmentFromString` instead.
segmentFromString ∷ String → PathSegment
segmentFromString = PathSegment <<< printEncoded segmentChar

-- | Returns the string value for a segment, percent-decoding any characters
-- | that require it.
segmentToString ∷ PathSegment → String
segmentToString (PathSegment s) = unsafePartial $ fromJust $ decodeURIComponent s

-- | Constructs a segment value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the segment, to prevent double-encoding.
unsafeSegmentFromString ∷ String → PathSegment
unsafeSegmentFromString = PathSegment

-- | Returns the string value for the segment without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `segmentFromString`/
-- | `unsafeSegmentFromString` pairing.
unsafeSegmentToString ∷ PathSegment → String
unsafeSegmentToString (PathSegment s) = s

-- | A parser for a _segment_ component of a URI.
parseSegment ∷ Parser String PathSegment
parseSegment =
  PathSegment
    <<< NES.joinWith ""
    <$> List.manyRec (pctEncoded <|> NES.singleton <$> segmentChar)

-- | A printer for a _segment_ component of a URI.
printSegment ∷ PathSegment → String
printSegment = unsafeSegmentToString

-- | A path segment that cannot be empty. Corresponds to _segment-nz_ in the
-- | spec.
newtype PathSegmentNZ = PathSegmentNZ NonEmptyString

derive newtype instance eqPathSegmentNZ ∷ Eq PathSegmentNZ
derive newtype instance ordPathSegmentNZ ∷ Ord PathSegmentNZ

instance showPathSegmentNZ ∷ Show PathSegmentNZ where
  show (PathSegmentNZ s) = "(Segment.unsafeSegmentNZFromString " <> show s <> ")"

-- | Constructs a non-empty segment value from a string, percent-encoding any
-- | characters that require it. Note that running this on a string that has
-- | already had percent-encoding applied will double-encode it, for those
-- | situations use `unsafeSegmentNZFromString` instead.
segmentNZFromString ∷ NonEmptyString → PathSegmentNZ
segmentNZFromString = PathSegmentNZ <<< printEncoded' segmentChar

-- | Returns the string value for a non-empty segment, percent-decoding any
-- | characters that require it.
segmentNZToString ∷ PathSegmentNZ → NonEmptyString
segmentNZToString (PathSegmentNZ s) = decodeURIComponent' s

-- | Constructs a non-empty segment value from a string directly - no
-- | percent-encoding will be applied. This is useful when using a custom
-- | encoding scheme for the segment, to prevent double-encoding.
unsafeSegmentNZFromString ∷ NonEmptyString → PathSegmentNZ
unsafeSegmentNZFromString = PathSegmentNZ

-- | Returns the string value for a non-empty segment without percent-decoding.
-- | Only "unsafe" in the sense that values this produces may need further
-- | decoding, the name is more for symmetry with the `segmentNZFromString`/
-- | `unsafeSegmentNZFromString` pairing.
unsafeSegmentNZToString ∷ PathSegmentNZ → NonEmptyString
unsafeSegmentNZToString (PathSegmentNZ s) = s

-- | A parser for a _segment-nz_ component of a URI.
parseSegmentNZ ∷ Parser String PathSegmentNZ
parseSegmentNZ =
  PathSegmentNZ
    <<< NES.join1With ""
    <$> NEA.some (pctEncoded <|> NES.singleton <$> segmentChar)

-- | A printer for a _segment-nz_ component of a URI.
printSegmentNZ ∷ PathSegmentNZ → String
printSegmentNZ = NES.toString <<< unsafeSegmentNZToString

-- | A path segment that cannot be empty or contain the `:` character.
-- | Corresponds to _segment-nz-nc_ in the spec.
newtype PathSegmentNZNC = PathSegmentNZNC NonEmptyString

derive newtype instance eqPathSegmentNZNC ∷ Eq PathSegmentNZNC
derive newtype instance ordPathSegmentNZNC ∷ Ord PathSegmentNZNC

instance showPathSegmentNZNC ∷ Show PathSegmentNZNC where
  show (PathSegmentNZNC s) = "(Segment.unsafeSegmentNZNCToString " <> show s <> ")"

-- | Constructs a non-empty-no-colon segment value from a string,
-- | percent-encoding any characters that require it. Note that running this on
-- | a string that has already had percent-encoding applied will double-encode
-- | it, for those situations use `unsafeSegmentNZNCFromString` instead.
segmentNZNCFromString ∷ NonEmptyString → PathSegmentNZNC
segmentNZNCFromString = PathSegmentNZNC <<< printEncoded' segmentNCChar

-- | Constructs a non-empty-no-colon segment value from a string directly - no
-- | percent-encoding will be applied. This is useful when using a custom
-- | encoding scheme for the segment, to prevent double-encoding.
segmentNZNCToString ∷ PathSegmentNZNC → NonEmptyString
segmentNZNCToString (PathSegmentNZNC s) = decodeURIComponent' s

-- | Returns the string value for a non-empty-no-colon segment, percent-decoding
-- | any characters that require it.
unsafeSegmentNZNCFromString ∷ NonEmptyString → PathSegmentNZNC
unsafeSegmentNZNCFromString = PathSegmentNZNC

-- | Returns the string value for the non-empty-no-colon segment without
-- | percent-decoding. Only "unsafe" in the sense that values this produces may
-- | need further decoding, the name is more for symmetry with the
-- | `segmentNZNCFromString`/`unsafeSegmentNZNCFromString` pairing.
unsafeSegmentNZNCToString ∷ PathSegmentNZNC → NonEmptyString
unsafeSegmentNZNCToString (PathSegmentNZNC s) = s

-- | A parser for a _segment-nz-nc_ component of a URI.
parseSegmentNZNC ∷ Parser String PathSegmentNZNC
parseSegmentNZNC =
  PathSegmentNZNC
    <<< NES.join1With ""
    <$> NEA.some (pctEncoded <|> NES.singleton <$> segmentNCChar)

-- | A printer for a _segment-nz-nc_ component of a URI.
printSegmentNZNC ∷ PathSegmentNZNC → String
printSegmentNZNC = NES.toString <<< unsafeSegmentNZNCToString

-- | The supported path segment characters, excluding percent-encodings.
segmentChar ∷ Parser String Char
segmentChar = segmentNCChar <|> char ':'

-- | The supported no-colon path segment characters, excluding
-- | percent-encodings.
segmentNCChar ∷ Parser String Char
segmentNCChar = unreserved <|> subDelims <|> char '@'
