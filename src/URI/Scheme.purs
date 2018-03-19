module URI.Scheme
  ( Scheme
  , fromString
  , unsafeFromString
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char, eof)
import URI.Common (alpha, alphaNum)

-- | The scheme part of an absolute URI. For example: `http`, `ftp`, `git`.
newtype Scheme = Scheme NonEmptyString

derive newtype instance eqScheme ∷ Eq Scheme
derive newtype instance ordScheme ∷ Ord Scheme

instance showScheme ∷ Show Scheme where
  show (Scheme s) = "(Scheme.unsafeFromString " <> show (NES.toString s) <> ")"

-- | Attempts to create a `Scheme` from the passed string. The scheme component
-- | of a URI has no escape sequences, so this function will return `Nothing`
-- | if an invalid value is provided.
-- |
-- | ``` purescript
-- | fromString "http" == Just (Scheme.unsafeFromString "http")
-- | fromString "git+ssh" == Just (Scheme.unsafeFromString "git+ssh")
-- | fromString "!!!" == Nothing
-- | fromString "" == Nothing
-- | ```
fromString ∷ String → Maybe Scheme
fromString = map Scheme <<< hush <<< flip runParser (parseScheme <* eof)

-- | Constructs a `Scheme` part unsafely: if the value is not an acceptable
-- | scheme a runtime error will be thrown.
-- |
-- | This is intended as a convenience when describing `Scheme`s statically in
-- | PureScript code, in all other cases `fromString` should be used.
unsafeFromString ∷ String → Scheme
unsafeFromString s = case fromString s of
  Just s' → s'
  Nothing → unsafeCrashWith $ "Scheme value is invalid: `" <> show s <> "`"

-- | A parser for the scheme component of a URI. Expects a scheme string
-- | followed by `':'`.
parser ∷ Parser String Scheme
parser = Scheme <$> parseScheme <* char ':'

parseScheme ∷ Parser String NonEmptyString
parseScheme = do
  init ← alpha
  rest ← Array.many (alphaNum <|> char '+' <|> char '-' <|> char '.')
  pure $ NES.singleton init `NES.appendString` String.fromCharArray rest

-- | A printer for the scheme component of a URI. Prints a scheme value
-- | followed by a `':'`.
print ∷ Scheme → String
print (Scheme s) = NES.toString s <> ":"
