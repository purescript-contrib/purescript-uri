module URI.Port
  ( Port
  , toInt
  , fromInt
  , unsafeFromInt
  , parser
  , print
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.String as String
import Global (readInt)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (char)
import URI.Common (URIPartParseError, digit, wrapParser)

newtype Port = Port Int

derive newtype instance eqPort ∷ Eq Port
derive newtype instance ordPort ∷ Ord Port
derive instance genericPort ∷ Generic Port _
instance showPort ∷ Show Port where show = genericShow

toInt ∷ Port → Int
toInt (Port i) = i

-- | Constructs a `Port` safely: bounds-checks the value to ensure it occurs
-- | within the range 0-65535 (inclusive).
fromInt ∷ Int → Maybe Port
fromInt n
  | n >= 0 && n <= 65535 = Just (Port n)
  | otherwise = Nothing

-- | Constructs a `Port` unsafely: if the value is outside the allowable range,
-- | a runtime error will be thrown.
-- |
-- | This is intended as a convenience when describing `Port`s statically in
-- | PureScript code, in all other cases `fromInt` should be preferred.
unsafeFromInt ∷ Int → Port
unsafeFromInt n =
  case fromInt n of
    Just addr → addr
    Nothing → unsafeCrashWith "Port was out of range"

parser ∷ ∀ p. (Port → Either URIPartParseError p) → Parser String p
parser p = wrapParser p do
  s ← String.fromCharArray <$> (char ':' *> Array.some digit)
  case fromNumber $ readInt 10 s of
    Just x → pure (Port x)
    _ → fail "Expected valid port number"

print ∷ Port → String
print (Port x) = ":" <> show x
