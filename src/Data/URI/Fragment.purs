module Data.URI.Fragment
  ( Fragment
  , fromString
  , toString
  , unsafeFromString
  , unsafeToString
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.String as String
import Data.URI.Common (newParsePCTEncoded, parseSubDelims, parseUnreserved, printEncoded, wrapParser)
import Global (decodeURIComponent)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.String (char, string)

-- | The hash fragment of a URI.
newtype Fragment = Fragment String

derive newtype instance eqFragment ∷ Eq Fragment
derive newtype instance ordFragment ∷ Ord Fragment
derive newtype instance semigroupFragment ∷ Semigroup Fragment
derive newtype instance monoidFragment ∷ Monoid Fragment

instance showFragment ∷ Show Fragment where
  show (Fragment s) = "(Fragment.unsafeFromString " <> show s <> ")"

fromString ∷ String → Fragment
fromString = Fragment <<< printEncoded fragmentChar

toString ∷ Fragment → String
toString (Fragment s) = decodeURIComponent s

unsafeFromString ∷ String → Fragment
unsafeFromString = Fragment

unsafeToString ∷ Fragment → String
unsafeToString (Fragment s) = s

parser ∷ ∀ f. (Fragment → Either ParseError f) → Parser f
parser parseF = string "#" *>
  wrapParser parseF (Fragment <<< String.joinWith ""
    <$> Array.many (newParsePCTEncoded <|> String.singleton <$> fragmentChar))

print ∷ ∀ f. (f → Fragment) → f → String
print printF f = "#" <> unsafeToString (printF f)

-- | The supported fragment characters, excluding percent-encodings.
fragmentChar ∷ Parser Char
fragmentChar =
  parseUnreserved <|> parseSubDelims
    <|> char ':' <|> char '@' <|> char '/' <|> char '?'
