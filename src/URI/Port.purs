module URI.Port
  ( Port
  , toInt
  , fromInt
  , unsafeFromInt
  , parser
  , print
  ) where

import Prelude

import Data.Int (decimal, fromStringAs)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (joinWith) as NES
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (digit)

-- | The port component of a host in a URI.
newtype Port = Port Int

derive newtype instance eqPort :: Eq Port
derive newtype instance ordPort :: Ord Port

instance showPort :: Show Port where
  show (Port i) = "(Port.unsafeFromInt " <> show i <> ")"

-- | Returns the port number as an integer.
toInt :: Port -> Int
toInt (Port i) = i

-- | Attempts to create a port from the passed integer. If the value falls
-- | outside of the range 0-65535 (inclusive) `Nothing` will be returned.
fromInt :: Int -> Maybe Port
fromInt i
  | i >= 0 && i <= 65535 = Just (Port i)
  | otherwise = Nothing

-- | Constructs a port from an integer directly: if the value is not an
-- | acceptable port number a runtime error will be thrown.
-- |
-- | This is intended as a convenience when describing `Port`s statically in
-- | PureScript code, in all other cases `fromInt` should be preferred.
unsafeFromInt :: Int -> Port
unsafeFromInt i =
  case fromInt i of
    Just addr -> addr
    Nothing -> unsafeCrashWith $ "Port value " <> show i <> " is out of range"

-- | A parser for the port component of a host in a URI. Expects values with a
-- | `':'` prefix.
parser :: Parser String Port
parser = do
  s <- NES.joinWith "" <$> (char ':' *> List.someRec (NES.singleton <$> digit))
  case fromStringAs decimal s of
    Just x -> pure (Port x)
    _ -> fail "Expected a valid port number"

-- | A printer for the port component of a host in a URI. Will print the value
-- | with a `':'` prefix.
print :: Port -> String
print (Port x) = ":" <> show x
