module URI.Common
  ( URIPartParseError(..)
  , wrapParser
  , alpha
  , digit
  , alphaNum
  , hexDigit
  , parseUnreserved
  , pctEncoded
  , parseSubDelims
  , printEncoded
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.State (get)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, un)
import Data.String as String
import Global (encodeURIComponent)
import Text.Parsing.Parser (ParseError(..), ParseState(..), Parser, ParserT(..), runParser)
import Text.Parsing.Parser.String (anyChar, char, eof, oneOf, satisfy)

newtype URIPartParseError = URIPartParseError String

derive newtype instance eqURIPartParseError :: Eq URIPartParseError
derive newtype instance ordURIPartParseError :: Ord URIPartParseError
derive instance newtypeURIPartParseError :: Newtype URIPartParseError _
derive instance genericURIPartParseError :: Generic URIPartParseError _
instance showURIPartParseError :: Show URIPartParseError where show = genericShow

wrapParser
  ∷ ∀ s m a b
  . Monad m
  ⇒ (a → Either URIPartParseError b)
  → ParserT s m a
  → ParserT s m b
wrapParser parseA p = ParserT do
  ParseState _ pos _ ← get
  a ← un ParserT p
  case parseA a of
    Left (URIPartParseError err) → throwError (ParseError err pos)
    Right b → pure b

alpha ∷ Parser String Char
alpha = satisfy \c → (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

digit ∷ Parser String Char
digit = satisfy \c → (c >= '0' && c <= '9')

alphaNum ∷ Parser String Char
alphaNum = alpha <|> digit

hexDigit ∷ Parser String Char
hexDigit = satisfy \c → (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

parseUnreserved ∷ Parser String Char
parseUnreserved = alphaNum <|> char '-' <|> char '.' <|> char '_' <|> char '~'

pctEncoded ∷ Parser String String
pctEncoded = do
  d0 ← char '%'
  d1 ← hexDigit
  d2 ← hexDigit
  pure $ String.fromCharArray [d0, d1, d2]

parseSubDelims ∷ Parser String Char
parseSubDelims =
  oneOf ['!', '$', '&', '\'', '(', ')', '*', '+', ';', '=', ',']

printEncoded ∷ Parser String Char → String → String
printEncoded p s = either (const s) id (runParser s parse)
  where
    parse ∷ Parser String String
    parse = (String.joinWith "" <$> Array.many (simpleChar <|> encodedChar)) <* eof
    simpleChar ∷ Parser String String
    simpleChar = String.singleton <$> p
    encodedChar ∷ Parser String String
    encodedChar = encodeURIComponent <<< String.singleton <$> anyChar
