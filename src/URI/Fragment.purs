module URI.Fragment
  ( Fragment
  , fromString
  , toString
  , unsafeFromString
  , unsafeToString
  , parser
  , print
  , fragmentChar
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import Data.String.NonEmpty (joinWith) as NES
import Global.Unsafe (unsafeDecodeURIComponent)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (subDelims, unreserved, pctEncoded, printEncoded)

-- | The fragment component (hash) of a URI.
newtype Fragment = Fragment String

derive newtype instance eqFragment ∷ Eq Fragment
derive newtype instance ordFragment ∷ Ord Fragment
derive newtype instance semigroupFragment ∷ Semigroup Fragment
derive newtype instance monoidFragment ∷ Monoid Fragment

instance showFragment ∷ Show Fragment where
  show (Fragment s) = "(Fragment.unsafeFromString " <> show s <> ")"

-- | Constructs a fragment value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeFromString` instead.
-- |
-- | ``` purescript
-- | fromString "foo" = unsafeFromString "foo"
-- | fromString "foo#bar" = unsafeFromString "foo%23bar"
-- | fromString "foo%23bar" = unsafeFromString "foo%2523bar"
-- | ```
fromString ∷ String → Fragment
fromString = Fragment <<< printEncoded fragmentChar

-- | Returns the string value for a fragment, percent-decoding any characters
-- | that require it.
-- |
-- | ``` purescript
-- | toString (unsafeFromString "foo") = "foo"
-- | toString (unsafeFromString "foo%23bar") = "foo#bar"
-- | ```
toString ∷ Fragment → String
toString (Fragment s) = unsafeDecodeURIComponent s

-- | Constructs a fragment value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the fragment, to prevent double-encoding.
unsafeFromString ∷ String → Fragment
unsafeFromString = Fragment

-- | Returns the string value for the fragment without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `fromString`/`unsafeFromString`
-- | pairing.
unsafeToString ∷ Fragment → String
unsafeToString (Fragment s) = s

-- | A parser for the fragment component of a URI. Expects values with a `'#'`
-- | prefix.
parser ∷ Parser String Fragment
parser =
  char '#' *>
    (Fragment <<< NES.joinWith ""
      <$> Array.many (pctEncoded <|> NES.singleton <$> fragmentChar))

-- | A printer for the fragment component of a URI. Will print the value with
-- | a `'#'` prefix.
print ∷ Fragment → String
print (Fragment f) = "#" <> f

-- | The supported fragment characters, excluding percent-encodings.
fragmentChar ∷ Parser String Char
fragmentChar =
  unreserved <|> subDelims
    <|> char ':' <|> char '@' <|> char '/' <|> char '?'
