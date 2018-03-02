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
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Global (decodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (decodeURIComponent', pctEncoded, printEncoded, printEncoded', subDelims, unreserved)

newtype PathSegment = PathSegment String

derive newtype instance eqPathSegment ∷ Eq PathSegment
derive newtype instance ordPathSegment ∷ Ord PathSegment

instance showPathSegment ∷ Show PathSegment where
  show (PathSegment s) = "(PathSegment.unsafeFromString " <> show s <> ")"

segmentFromString ∷ String → PathSegment
segmentFromString = PathSegment <<< printEncoded segmentChar

segmentToString ∷ PathSegment → String
segmentToString (PathSegment s) = decodeURIComponent s

unsafeSegmentFromString ∷ String → PathSegment
unsafeSegmentFromString = PathSegment

unsafeSegmentToString ∷ PathSegment → String
unsafeSegmentToString (PathSegment s) = s

parseSegment ∷ Parser String PathSegment
parseSegment =
  PathSegment
    <<< String.joinWith ""
    <$> Array.many (pctEncoded <|> String.singleton <$> segmentChar)

printSegment ∷ PathSegment → String
printSegment = unsafeSegmentToString

newtype PathSegmentNZ = PathSegmentNZ NonEmptyString

derive newtype instance eqPathSegmentNZ ∷ Eq PathSegmentNZ
derive newtype instance ordPathSegmentNZ ∷ Ord PathSegmentNZ

instance showPathSegmentNZ ∷ Show PathSegmentNZ where
  show (PathSegmentNZ s) = "(PathSegmentNZ.unsafeFromString " <> show s <> ")"

segmentNZFromString ∷ NonEmptyString → PathSegmentNZ
segmentNZFromString = PathSegmentNZ <<< printEncoded' segmentChar

segmentNZToString ∷ PathSegmentNZ → NonEmptyString
segmentNZToString (PathSegmentNZ s) = decodeURIComponent' s

unsafeSegmentNZFromString ∷ NonEmptyString → PathSegmentNZ
unsafeSegmentNZFromString = PathSegmentNZ

unsafeSegmentNZToString ∷ PathSegmentNZ → NonEmptyString
unsafeSegmentNZToString (PathSegmentNZ s) = s

parseSegmentNZ ∷ Parser String PathSegmentNZ
parseSegmentNZ =
  PathSegmentNZ
    <<< unsafePartial NES.unsafeFromString
    <<< String.joinWith ""
    <$> Array.some (pctEncoded <|> String.singleton <$> segmentChar)

printSegmentNZ ∷ PathSegmentNZ → String
printSegmentNZ = NES.toString <<< unsafeSegmentNZToString

newtype PathSegmentNZNC = PathSegmentNZNC NonEmptyString

derive newtype instance eqPathSegmentNZNC ∷ Eq PathSegmentNZNC
derive newtype instance ordPathSegmentNZNC ∷ Ord PathSegmentNZNC

instance showPathSegmentNZNC ∷ Show PathSegmentNZNC where
  show (PathSegmentNZNC s) = "(PathSegmentNZNC.unsafeFromString " <> show s <> ")"

segmentNZNCFromString ∷ NonEmptyString → PathSegmentNZNC
segmentNZNCFromString = PathSegmentNZNC <<< printEncoded' segmentNCChar

unsafeSegmentNZNCFromString ∷ NonEmptyString → PathSegmentNZNC
unsafeSegmentNZNCFromString = PathSegmentNZNC

segmentNZNCToString ∷ PathSegmentNZNC → NonEmptyString
segmentNZNCToString (PathSegmentNZNC s) = decodeURIComponent' s

unsafeSegmentNZNCToString ∷ PathSegmentNZNC → NonEmptyString
unsafeSegmentNZNCToString (PathSegmentNZNC s) = s

parseSegmentNZNC ∷ Parser String PathSegmentNZNC
parseSegmentNZNC =
  PathSegmentNZNC
    <<< unsafePartial NES.unsafeFromString
    <<< String.joinWith ""
    <$> Array.some (pctEncoded <|> String.singleton <$> segmentNCChar)

printSegmentNZNC ∷ PathSegmentNZNC → String
printSegmentNZNC = NES.toString <<< unsafeSegmentNZNCToString

segmentChar ∷ Parser String Char
segmentChar = segmentNCChar <|> char ':'

segmentNCChar ∷ Parser String Char
segmentNCChar = unreserved <|> subDelims <|> char '@'
