module Data.URI.Port where

import Prelude

import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.URI (Port(..))
import Data.URI.Common (rxPat)
import Global (readInt)
import Text.Parsing.StringParser (Parser, fail)

parser ∷ Parser Port
parser = do
 s ← rxPat "[0-9]+"
 case fromNumber $ readInt 10 s of
    Just x  → pure (Port x)
    _ → fail "Expected valid port number"

print ∷ Port → String
print (Port p) = show p
