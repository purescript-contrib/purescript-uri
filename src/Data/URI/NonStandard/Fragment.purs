module Data.URI.NonStandard.Fragment where

import Prelude

import Data.Either (Either, fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Global (encodeURIComponent)
import Partial.Unsafe (unsafePartial)

-- | The hash fragment of a URI.
newtype Fragment = Fragment String

derive newtype instance eqFragment ∷ Eq Fragment
derive newtype instance ordFragment ∷ Ord Fragment
derive instance genericFragment ∷ Generic Fragment _
derive instance newtypeFragment ∷ Newtype Fragment _
instance showFragment ∷ Show Fragment where show = genericShow

parse ∷ ∀ e. String → Either e Fragment
parse = pure <<< Fragment

print ∷ Fragment → String
print (Fragment f) = String.joinWith "" (map printChar $ String.split (String.Pattern "") f)
  where
  -- Fragments & queries have a bunch of characters that don't need escaping
  printChar ∷ String → String
  printChar s
    | RX.test rxPrintable s = s
    | otherwise = encodeURIComponent s

rxPrintable ∷ RX.Regex
rxPrintable = unsafePartial fromRight $ RX.regex "[&;$+=/?:@]" RXF.global
