module Data.URI.Fragment where

import Prelude

import Control.Alt ((<|>))
import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.URI.Common (anyString, decodePCTComponent, joinWith, parsePChar, wrapParser)
import Global (encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (string)

-- | The hash fragment of a URI.
newtype Fragment = Fragment String

derive newtype instance eqFragment ∷ Eq Fragment
derive newtype instance ordFragment ∷ Ord Fragment
derive instance genericFragment ∷ Generic Fragment _
derive instance newtypeFragment ∷ Newtype Fragment _
instance showFragment ∷ Show Fragment where show = genericShow

parser' ∷ ∀ f. Parser f → Parser f
parser' parseF = string "#" *>
  wrapParser parseF (joinWith ""
    <$> many (parsePChar decodePCTComponent <|> string "/" <|> string "?"))

parser ∷ Parser Fragment
parser = Fragment <$> anyString

print' ∷ ∀ f. (f → String) → f → String
print' printF f = "#" <> printF f

print ∷ Fragment → String
print (Fragment f) = S.joinWith "" (map printChar $ S.split (S.Pattern "") f)
  where
  -- Fragments & queries have a bunch of characters that don't need escaping
  printChar ∷ String → String
  printChar s
    | RX.test rxPrintable s = s
    | otherwise = encodeURIComponent s

rxPrintable ∷ RX.Regex
rxPrintable = unsafePartial fromRight $ RX.regex "[&;$+=/?:@]" RXF.global
