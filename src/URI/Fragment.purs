module URI.Fragment
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
import Data.Monoid (class Monoid)
import Data.String as String
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (parseSubDelims, parseUnreserved, pctEncoded, printEncoded)

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

parser ∷ Parser String Fragment
parser =
  char '#' *>
    (Fragment <<< String.joinWith ""
      <$> Array.many (pctEncoded <|> String.singleton <$> fragmentChar))

print ∷ Fragment → String
print (Fragment f) = "#" <> f

-- | The supported fragment characters, excluding percent-encodings.
fragmentChar ∷ Parser String Char
fragmentChar =
  parseUnreserved <|> parseSubDelims
    <|> char ':' <|> char '@' <|> char '/' <|> char '?'
