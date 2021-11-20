module URI.Extra.QueryPairs
  ( QueryPairs(..)
  , parse
  , print
  , keyPartChar
  , valuePartChar
  , Key
  , keyFromString
  , keyToString
  , unsafeKeyFromString
  , unsafeKeyToString
  , Value
  , valueFromString
  , valueToString
  , unsafeValueFromString
  , unsafeValueToString
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.NonEmpty (joinWith) as NES
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JSURI (decodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy)
import Text.Parsing.Parser.String (char, oneOf)
import URI.Common (URIPartParseError(..), unreserved, pctEncoded, printEncoded, wrapParser)
import URI.Query as Q

-- | A query string split into an array of key/value pairs. There is no precise
-- | spec for this, but the format is commonly used, so this attempts to handle
-- | these strings in a sensible way.
-- |
-- | - The representation uses an array rather than a map, so duplicate keys
-- |   are supported.
-- | - Keys are not required to have a value associated.
-- | - `&` and `;` are both treated as pair delimiters.
newtype QueryPairs k v = QueryPairs (Array (Tuple k (Maybe v)))

derive instance genericQueryPairs :: Generic (QueryPairs k v) _
derive newtype instance eqQueryPairs :: (Eq k, Eq v) => Eq (QueryPairs k v)
derive newtype instance ordQueryPairs :: (Ord k, Ord v) => Ord (QueryPairs k v)
derive newtype instance semigroupQueryPairs :: Semigroup (QueryPairs k v)
derive newtype instance monoidQueryPairs :: Monoid (QueryPairs k v)

instance showQueryPairs :: (Show k, Show v) => Show (QueryPairs k v) where
  show = genericShow

-- | Parses a query into key/value pairs.
-- |
-- | This function allows for the `Key` and `Value` components to be parsed
-- | into custom representations. If this is not necessary, use `pure` for both
-- | these arguments.
parse
  :: forall k v
   . (Key -> Either URIPartParseError k)
  -> (Value -> Either URIPartParseError v)
  -> Q.Query
  -> Either URIPartParseError (QueryPairs k v)
parse parseK parseV =
  bimap (\(ParseError err _) -> URIPartParseError err) QueryPairs
    <<< flip runParser (Array.fromFoldable <$> sepBy (parsePart parseK parseV) (char '&'))
    <<< Q.unsafeToString

parsePart
  :: forall k v
   . (Key -> Either URIPartParseError k)
  -> (Value -> Either URIPartParseError v)
  -> Parser String (Tuple k (Maybe v))
parsePart parseK parseV = do
  key <- wrapParser (parseK <<< Key) $
    NES.joinWith "" <$> List.someRec (NES.singleton <$> keyPartChar <|> pctEncoded)
  value <- wrapParser (traverse (parseV <<< Value)) $ optionMaybe do
    _ <- char '='
    NES.joinWith "" <$> List.manyRec (NES.singleton <$> valuePartChar <|> pctEncoded)
  pure $ Tuple key value

-- | A printer for key/value pairs style query string.
-- |
-- | As a counterpart to the `parser` this function also requires the `Key`
-- | and `Value` components to be printed back from their custom representations.
-- | If no custom types are being used, pass `identity` for both of these arguments.
print
  :: forall k v
   . (k -> Key)
  -> (v -> Value)
  -> QueryPairs k v
  -> Q.Query
print printK printV (QueryPairs m) =
  Q.unsafeFromString $ String.joinWith "&" $ Array.fromFoldable (printPart <$> m)
  where
  printPart :: Tuple k (Maybe v) -> String
  printPart = case _ of
    Tuple k Nothing ->
      unsafeKeyToString (printK k)
    Tuple k (Just v) ->
      unsafeKeyToString (printK k) <> "=" <> unsafeValueToString (printV v)

-- | The default `Key` type used for `QueryPairs`.
newtype Key = Key String

derive newtype instance eqKey :: Eq Key
derive newtype instance ordKey :: Ord Key
derive newtype instance semigroupKey :: Semigroup Key
derive newtype instance monoidKey :: Monoid Key

instance showKey :: Show Key where
  show (Key s) = "(QueryPairs.unsafeKeyFromString " <> show s <> ")"

-- | Constructs a key value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeKeyFromString` instead.
-- |
-- | ``` purescript
-- | keyFromString "foo" = unsafeKeyFromString "foo"
-- | keyFromString "foo#bar" = unsafeKeyFromString "foo%23bar"
-- | keyFromString "foo%23bar" = unsafeKeyFromString "foo%2523bar"
-- | ```
keyFromString :: String -> Key
keyFromString = Key <<< printEncoded keyPartChar

-- | Returns the string value for a key, percent-decoding any characters
-- | that require it.
-- |
-- | ``` purescript
-- | keyToString (unsafeKeyFromString "foo") = "foo"
-- | keyToString (unsafeKeyFromString "foo%23bar") = "foo#bar"
-- | ```
keyToString :: Key -> String
keyToString (Key s) = unsafePartial $ fromJust $ decodeURIComponent s

-- | Constructs a key value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the key, to prevent double-encoding.
unsafeKeyFromString :: String -> Key
unsafeKeyFromString = Key

-- | Returns the string value for a key without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `fromString`/`unsafeFromString`
-- | pairing.
unsafeKeyToString :: Key -> String
unsafeKeyToString (Key s) = s

-- | The default `Value` type used for `QueryPairs`.
newtype Value = Value String

derive newtype instance eqValue :: Eq Value
derive newtype instance ordValue :: Ord Value
derive newtype instance semigroupValue :: Semigroup Value
derive newtype instance monoidValue :: Monoid Value

instance showValue :: Show Value where
  show (Value s) = "(QueryPairs.unsafeValueFromString " <> show s <> ")"

-- | Constructs a value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeValueFromString` instead.
-- |
-- | ``` purescript
-- | valueFromString "foo" = unsafeValueFromString "foo"
-- | valueFromString "foo#bar" = unsafeValueFromString "foo%23bar"
-- | valueFromString "foo%23bar" = unsafeValueFromString "foo%2523bar"
-- | ```
valueFromString :: String -> Value
valueFromString =
  -- `keyPartChar` is used intentionally here. It only differs from
  -- `valuePartChar` by excluding `=`, and `=` should be encoded as `%3D`, but
  -- can be unambiguously decoded so we want to accept it when reading but do
  -- the right thing when printing
  Value <<< printEncoded keyPartChar

-- | Returns the string value for a value, percent-decoding any characters
-- | that require it.
-- |
-- | ``` purescript
-- | valueToString (unsafeValueFromString "foo") = "foo"
-- | valueToString (unsafeValueFromString "foo%23bar") = "foo#bar"
-- | ```
valueToString :: Value -> String
valueToString (Value s) = unsafePartial $ fromJust $ decodeURIComponent s

-- | Constructs a value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the value, to prevent double-encoding.
unsafeValueFromString :: String -> Value
unsafeValueFromString = Value

-- | Returns the string value for a value without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `fromString`/`unsafeFromString`
-- | pairing.
unsafeValueToString :: Value -> String
unsafeValueToString (Value s) = s

-- | The supported key characters, excluding percent-encodings.
keyPartChar :: Parser String Char
keyPartChar =
  unreserved
    <|> oneOf [ '!', '$', '\'', '(', ')', '*', '+', ',', ':', '@', '/', '?' ]

-- | The supported value characters, excluding percent-encodings.
valuePartChar :: Parser String Char
valuePartChar = keyPartChar <|> char '='
