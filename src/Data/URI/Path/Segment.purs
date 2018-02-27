module Data.URI.Path.Segment
  ( PathSegment
  , parseSegment
  , segmentFromString
  , segmentToString
  , unsafeSegmentFromString
  , unsafeSegmentToString
  , PathSegmentNZ
  , parseSegmentNonZero
  , segmentNZFromString
  , segmentNZToString
  , unsafeSegmentNZFromString
  , unsafeSegmentNZToString
  , PathSegmentNZNC
  , parseSegmentNonZeroNoColon
  , segmentNZNCFromString
  , segmentNZNCToString
  , unsafeSegmentNZNCFromString
  , unsafeSegmentNZNCToString
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.URI.Common (pctEncoded, parseSubDelims, parseUnreserved, printEncoded)
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)

newtype PathSegment = PathSegment String

derive newtype instance eqPathSegment ∷ Eq PathSegment
derive newtype instance ordPathSegment ∷ Ord PathSegment

instance showPathSegment ∷ Show PathSegment where
  show (PathSegment s) = "(PathSegment.unsafeFromString " <> show s <> ")"

parseSegment ∷ Parser String PathSegment
parseSegment =
  PathSegment
    <<< String.joinWith ""
    <$> Array.many (pctEncoded <|> String.singleton <$> segmentChar)

segmentFromString ∷ String → PathSegment
segmentFromString = PathSegment <<< printEncoded segmentChar

segmentToString ∷ PathSegment → String
segmentToString (PathSegment s) = decodeURIComponent s

unsafeSegmentFromString ∷ String → PathSegment
unsafeSegmentFromString = PathSegment

unsafeSegmentToString ∷ PathSegment → String
unsafeSegmentToString (PathSegment s) = s

newtype PathSegmentNZ = PathSegmentNZ String

derive newtype instance eqPathSegmentNZ ∷ Eq PathSegmentNZ
derive newtype instance ordPathSegmentNZ ∷ Ord PathSegmentNZ

instance showPathSegmentNZ ∷ Show PathSegmentNZ where
  show (PathSegmentNZ s) = "(PathSegmentNZ.unsafeFromString " <> show s <> ")"

parseSegmentNonZero ∷ Parser String PathSegmentNZ
parseSegmentNonZero =
  PathSegmentNZ
    <<< String.joinWith ""
    <$> Array.some (pctEncoded <|> String.singleton <$> segmentChar)

segmentNZFromString ∷ NonEmptyString → PathSegmentNZ
segmentNZFromString s = PathSegmentNZ (printEncoded segmentChar $ NES.toString s)

segmentNZToString ∷ PathSegmentNZ → String
segmentNZToString (PathSegmentNZ s) = decodeURIComponent s

unsafeSegmentNZFromString ∷ String → PathSegmentNZ
unsafeSegmentNZFromString = PathSegmentNZ

unsafeSegmentNZToString ∷ PathSegmentNZ → String
unsafeSegmentNZToString (PathSegmentNZ s) = s

newtype PathSegmentNZNC = PathSegmentNZNC String

derive newtype instance eqPathSegmentNZNC ∷ Eq PathSegmentNZNC
derive newtype instance ordPathSegmentNZNC ∷ Ord PathSegmentNZNC

instance showPathSegmentNZNC ∷ Show PathSegmentNZNC where
  show (PathSegmentNZNC s) = "(PathSegmentNZNC.unsafeFromString " <> show s <> ")"

parseSegmentNonZeroNoColon ∷ Parser String PathSegmentNZNC
parseSegmentNonZeroNoColon =
  PathSegmentNZNC
    <<< String.joinWith ""
    <$> Array.some (pctEncoded <|> String.singleton <$> segmentNCChar)

segmentNZNCToString ∷ PathSegmentNZNC → String
segmentNZNCToString (PathSegmentNZNC s) = decodeURIComponent s

unsafeSegmentNZNCToString ∷ PathSegmentNZNC → String
unsafeSegmentNZNCToString (PathSegmentNZNC s) = s

segmentNZNCFromString ∷ String → Maybe PathSegmentNZNC
segmentNZNCFromString = case _ of
  "" → Nothing
  s → Just $ PathSegmentNZNC (printEncoded segmentNCChar s)

unsafeSegmentNZNCFromString ∷ String → PathSegmentNZNC
unsafeSegmentNZNCFromString = PathSegmentNZNC

segmentChar ∷ Parser String Char
segmentChar = segmentNCChar <|> char ':'

segmentNCChar ∷ Parser String Char
segmentNCChar = parseUnreserved <|> parseSubDelims <|> char '@'
