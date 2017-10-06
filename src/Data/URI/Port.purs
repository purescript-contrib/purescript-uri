module Data.URI.Port where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.URI.Common (rxPat)
import Global (readInt)
import Text.Parsing.StringParser (Parser, fail)

-- | A port number.
newtype Port = Port Int

derive newtype instance eqPort ∷ Eq Port
derive newtype instance ordPort ∷ Ord Port
derive instance genericPort ∷ Generic Port _
derive instance newtypePort ∷ Newtype Port _
instance showPort ∷ Show Port where show = genericShow

parser ∷ Parser Port
parser = do
 s ← rxPat "[0-9]+"
 case fromNumber $ readInt 10 s of
    Just x  → pure (Port x)
    _ → fail "Expected valid port number"

print ∷ Port → String
print (Port p) = show p
