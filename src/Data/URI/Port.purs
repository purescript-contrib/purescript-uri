module Data.URI.Port
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
import Data.URI.Common (URIPartParseError, digit, wrapParser)
import Global (readInt)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (char)

newtype Port = Port Int

derive newtype instance eqPort ∷ Eq Port
derive newtype instance ordPort ∷ Ord Port
derive instance genericPort ∷ Generic Port _
instance showPort ∷ Show Port where show = genericShow

toInt ∷ Port → Int
toInt (Port i) = i

fromInt ∷ Int → Maybe Port
fromInt n
  | n >= 0 && n <= 65535 = Just (Port n)
  | otherwise = Nothing

unsafeFromInt ∷ Int → Port
unsafeFromInt = Port

parser ∷ ∀ p. (Port → Either URIPartParseError p) → Parser String p
parser p = wrapParser p do
  s ← String.fromCharArray <$> (char ':' *> Array.some digit)
  case fromNumber $ readInt 10 s of
    Just x → pure (Port x)
    _ → fail "Expected valid port number"

print ∷ Port → String
print (Port x) = ":" <> show x
