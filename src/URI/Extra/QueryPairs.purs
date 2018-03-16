module URI.Extra.QueryPairs
  ( Key
  , keyFromString
  , keyToString
  , unsafeKeyFromString
  , unsafeKeyToString
  , Value
  , valueFromString
  , valueToString
  , unsafeValueFromString
  , unsafeValueToString
  , QueryPairs(..)
  , parse
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Global (decodeURIComponent)
import Text.Parsing.Parser (ParseError(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy)
import Text.Parsing.Parser.String (char, oneOf)
import URI.Common (URIPartParseError(..), unreserved, pctEncoded, printEncoded, wrapParser)
import URI.Query as Q

newtype Key = Key String

derive newtype instance eqKey ∷ Eq Key
derive newtype instance ordKey ∷ Ord Key
derive newtype instance semigroupKey ∷ Semigroup Key
derive newtype instance monoidKey ∷ Monoid Key

instance showKey ∷ Show Key where
  show (Key s) = "(QueryPairs.unsafeKeyFromString " <> show s <> ")"

keyFromString ∷ String → Key
keyFromString = Key <<< printEncoded keyPartChar

keyToString ∷ Key → String
keyToString (Key s) = decodeURIComponent s

unsafeKeyFromString ∷ String → Key
unsafeKeyFromString = Key

unsafeKeyToString ∷ Key → String
unsafeKeyToString (Key s) = s

newtype Value = Value String

derive newtype instance eqValue ∷ Eq Value
derive newtype instance ordValue ∷ Ord Value
derive newtype instance semigroupValue ∷ Semigroup Value
derive newtype instance monoidValue ∷ Monoid Value

instance showValue ∷ Show Value where
  show (Value s) = "(QueryPairs.unsafeValueFromString " <> show s <> ")"

valueFromString ∷ String → Value
valueFromString =
  -- `keyPartChar` is used intentionally here. It only differs from
  -- `valuePartChar` by excluding `=`, and `=` should be encoded as `%3D`, but
  -- can be unambiguously decoded so we want to accept it when reading but do
  -- the right thing when printing
  Value <<< printEncoded keyPartChar

valueToString ∷ Value → String
valueToString (Value s) = decodeURIComponent s

unsafeValueFromString ∷ String → Value
unsafeValueFromString = Value

unsafeValueToString ∷ Value → String
unsafeValueToString (Value s) = s

newtype QueryPairs k v = QueryPairs (Array (Tuple k (Maybe v)))

derive newtype instance eqQueryPairs ∷ (Eq k, Eq v) ⇒ Eq (QueryPairs k v)
derive newtype instance ordQueryPairs ∷ (Ord k, Ord v) ⇒ Ord (QueryPairs k v)
derive instance genericQueryPairs ∷ Generic (QueryPairs k v) _
instance showQueryPairs ∷ (Show k, Show v) ⇒ Show (QueryPairs k v) where show = genericShow
derive newtype instance semigroupQueryPairs ∷ Semigroup (QueryPairs k v)
derive newtype instance monoidQueryPairs ∷ Monoid (QueryPairs k v)

parse
  ∷ ∀ k v
  . (Key → Either URIPartParseError k)
  → (Value → Either URIPartParseError v)
  → Q.Query
  → Either URIPartParseError (QueryPairs k v)
parse parseK parseV =
  bimap (\(ParseError err _) → URIPartParseError err) QueryPairs
    <<< flip runParser (Array.fromFoldable <$> sepBy (parsePart parseK parseV) (char '&'))
    <<< Q.unsafeToString

parsePart
  ∷ ∀ k v
  . (Key → Either URIPartParseError k)
  → (Value → Either URIPartParseError v)
  → Parser String (Tuple k (Maybe v))
parsePart parseK parseV = do
  key ← wrapParser (parseK <<< Key) $
    String.joinWith "" <$> Array.some (String.singleton <$> keyPartChar <|> pctEncoded)
  value ← wrapParser (traverse (parseV <<< Value)) $ optionMaybe do
    _ ← char '='
    String.joinWith "" <$> Array.many (String.singleton <$> valuePartChar <|> pctEncoded)
  pure $ Tuple key value


keyPartChar ∷ Parser String Char
keyPartChar
  = unreserved
  <|> oneOf ['!', '$', '\'', '(', ')', '*', '+', ',', ':', '@', '/', '?']

valuePartChar ∷ Parser String Char
valuePartChar = keyPartChar <|> char '='

print
  ∷ ∀ k v
  . (k → Key)
  → (v → Value)
  → QueryPairs k v
  → Q.Query
print printK printV (QueryPairs m) = Q.unsafeFromString $ String.joinWith "&" $ Array.fromFoldable (printPart <$> m)
  where
  printPart ∷ Tuple k (Maybe v) → String
  printPart (Tuple k Nothing) =
    unsafeKeyToString (printK k)
  printPart (Tuple k (Just v)) =
    unsafeKeyToString (printK k) <> "=" <> unsafeValueToString (printV v)
