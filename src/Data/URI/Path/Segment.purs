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
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.URI.Common (newParsePCTEncoded, parseSubDelims, parseUnreserved, printEncoded)
import Global (decodeURIComponent)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (char)

newtype PathSegment = PathSegment String

derive newtype instance eqPathSegment ∷ Eq PathSegment
derive newtype instance ordPathSegment ∷ Ord PathSegment
derive instance genericPathSegment ∷ Generic PathSegment _
instance showPathSegment ∷ Show PathSegment where show = genericShow

parseSegment ∷ Parser PathSegment
parseSegment =
  PathSegment
    <<< String.joinWith ""
    <$> Array.many (newParsePCTEncoded <|> String.singleton <$> segmentChar)

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
derive instance genericPathSegmentNZ ∷ Generic PathSegmentNZ _
instance showPathSegmentNZ ∷ Show PathSegmentNZ where show = genericShow

parseSegmentNonZero ∷ Parser PathSegmentNZ
parseSegmentNonZero =
  PathSegmentNZ
    <<< String.joinWith ""
    <$> Array.some (newParsePCTEncoded <|> String.singleton <$> segmentChar)

segmentNZFromString ∷ String → Maybe PathSegmentNZ
segmentNZFromString = case _ of
  "" → Nothing
  s → Just $ PathSegmentNZ (printEncoded segmentChar s)

segmentNZToString ∷ PathSegmentNZ → String
segmentNZToString (PathSegmentNZ s) = decodeURIComponent s

unsafeSegmentNZFromString ∷ String → PathSegmentNZ
unsafeSegmentNZFromString = PathSegmentNZ

unsafeSegmentNZToString ∷ PathSegmentNZ → String
unsafeSegmentNZToString (PathSegmentNZ s) = s

newtype PathSegmentNZNC = PathSegmentNZNC String

derive newtype instance eqPathSegmentNZNC ∷ Eq PathSegmentNZNC
derive newtype instance ordPathSegmentNZNC ∷ Ord PathSegmentNZNC
derive instance genericPathSegmentNZNC ∷ Generic PathSegmentNZNC _
instance showPathSegmentNZNC ∷ Show PathSegmentNZNC where show = genericShow

parseSegmentNonZeroNoColon ∷ Parser PathSegmentNZNC
parseSegmentNonZeroNoColon =
  PathSegmentNZNC
    <<< String.joinWith ""
    <$> Array.some (newParsePCTEncoded <|> String.singleton <$> segmentNCChar)

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

segmentChar ∷ Parser Char
segmentChar = segmentNCChar <|> char ':'

segmentNCChar ∷ Parser Char
segmentNCChar = parseUnreserved <|> parseSubDelims <|> char '@'
