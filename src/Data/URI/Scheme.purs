module Data.URI.Scheme
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
import Data.URI.Common (alpha, alphaNum)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char)

-- | The scheme part of an absolute URI. For example: `http`, `ftp`, `git`.
newtype Scheme = Scheme String

derive newtype instance eqScheme ∷ Eq Scheme
derive newtype instance ordScheme ∷ Ord Scheme

instance showScheme ∷ Show Scheme where
  show (Scheme s) = "(Scheme.unsafeFromString " <> show s <> ")"

fromString ∷ String → Maybe Scheme
fromString = map Scheme <<< hush <<< flip runParser parseScheme

unsafeFromString ∷ String → Scheme
unsafeFromString s = case fromString s of
  Nothing → unsafeCrashWith $ "Got invalid scheme in unsafeFromString : `" <> show s <> "`"
  Just s' → s'

parser ∷ Parser String Scheme
parser = Scheme <$> parseScheme <* char ':'

parseScheme ∷ Parser String String
parseScheme = do
  init ← alpha
  rest ← Array.many (alphaNum <|> char '+' <|> char '-' <|> char '.')
  pure $ String.singleton init <> String.fromCharArray rest

print ∷ Scheme → String
print (Scheme s) = s <> ":"
