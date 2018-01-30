module Data.URI.Port where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Global (readInt)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.String (anyDigit)

-- | A port number.
newtype Port = Port Int

derive newtype instance eqPort ∷ Eq Port
derive newtype instance ordPort ∷ Ord Port
derive instance genericPort ∷ Generic Port _
derive instance newtypePort ∷ Newtype Port _
instance showPort ∷ Show Port where show = genericShow

parser ∷ Parser Port
parser = do
  s ← String.fromCharArray <$> Array.some anyDigit
  case fromNumber $ readInt 10 s of
    Just x → pure (Port x)
    _ → fail "Expected valid port number"

print ∷ Port → String
print (Port p) = show p
