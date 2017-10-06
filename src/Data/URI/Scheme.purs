module Data.URI.Scheme where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.URI.Common (rxPat)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)

-- | The scheme part of an absolute URI. For example: `http`, `ftp`, `git`.
newtype Scheme = Scheme String

derive newtype instance eqScheme ∷ Eq Scheme
derive newtype instance ordScheme ∷ Ord Scheme
derive instance genericScheme ∷ Generic Scheme _
derive instance newtypeScheme ∷ Newtype Scheme _
instance showScheme ∷ Show Scheme where show = genericShow

parser ∷ Parser Scheme
parser = Scheme <$> rxPat "[a-z][a-z0-9+\\.\\-]+" <* string ":"

print ∷ Scheme → String
print (Scheme s) = s <> ":"
