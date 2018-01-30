module Data.URI.Scheme where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.String as String
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (alphaNum, anyLetter, char, string)

-- | The scheme part of an absolute URI. For example: `http`, `ftp`, `git`.
newtype Scheme = Scheme String

derive newtype instance eqScheme ∷ Eq Scheme
derive newtype instance ordScheme ∷ Ord Scheme
derive instance genericScheme ∷ Generic Scheme _
derive instance newtypeScheme ∷ Newtype Scheme _
instance showScheme ∷ Show Scheme where show = genericShow

parser ∷ Parser Scheme
parser = Scheme <$> parseScheme <* string ":"
  where
  parseScheme = do
    init ← anyLetter
    rest ← Array.many (alphaNum <|> char '+' <|> char '-' <|> char '.')
    pure $ String.singleton init <> String.fromCharArray rest

print ∷ Scheme → String
print (Scheme s) = s <> ":"
