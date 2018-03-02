module URI.Common
  ( URIPartParseError(..)
  , wrapParser
  , alpha
  , alphaNum
  , unreserved
  , pctEncoded
  , subDelims
  , printEncoded
  , printEncoded'
  , decodeURIComponent'
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
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Global (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError(..), ParseState(..), Parser, ParserT(..), runParser)
import Text.Parsing.Parser.String (anyChar, char, eof, oneOf, satisfy)
import Text.Parsing.Parser.Token (digit, hexDigit)

newtype URIPartParseError = URIPartParseError String

derive newtype instance eqURIPartParseError :: Eq URIPartParseError
derive newtype instance ordURIPartParseError :: Ord URIPartParseError
derive instance newtypeURIPartParseError :: Newtype URIPartParseError _
derive instance genericURIPartParseError :: Generic URIPartParseError _
instance showURIPartParseError :: Show URIPartParseError where show = genericShow

-- | Adapts a parser with a parser-esque function. First the original
-- | parser runs, then it attempts to refine the result with the function.
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

-- | Parser for ascii alphabetical characters (upper and lowercase).
alpha ∷ Parser String Char
alpha = satisfy \c → (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Parser for ascii alphanumeric characters (upper and lowercase for letters).
alphaNum ∷ Parser String Char
alphaNum = alpha <|> digit

-- | Parser for characters that are allowed in a URI but do not have a reserved
-- | purpose.
unreserved ∷ Parser String Char
unreserved = alphaNum <|> char '-' <|> char '.' <|> char '_' <|> char '~'

-- | Parser for the "sub-delims" group of reserved characters.
subDelims ∷ Parser String Char
subDelims =
  oneOf ['!', '$', '&', '\'', '(', ')', '*', '+', ';', '=', ',']

-- | Parser for a percent-encoded character.
pctEncoded ∷ Parser String String
pctEncoded = do
  d0 ← char '%'
  d1 ← hexDigit
  d2 ← hexDigit
  pure $ String.fromCharArray [d0, d1, d2]

-- | A helper function for printing URI components using percent-encoding for
-- | characters that require it.
-- |
-- | Accepts a parser that is used to determine whether a character is allowed
-- | to appear un-encoded in the URI component and the string to encode.
printEncoded ∷ Parser String Char → String → String
printEncoded p s = either (const s) id (runParser s parse)
  where
    parse ∷ Parser String String
    parse = (String.joinWith "" <$> Array.many (simpleChar <|> encodedChar)) <* eof
    simpleChar ∷ Parser String String
    simpleChar = String.singleton <$> p
    encodedChar ∷ Parser String String
    encodedChar = encodeURIComponent <<< String.singleton <$> anyChar

-- | A version of [`printEncoded`](#v:printEncoded) that operates on non-empty
-- | strings.
printEncoded' ∷ Parser String Char → NonEmptyString → NonEmptyString
printEncoded' p =
  unsafePartial NES.unsafeFromString <<< printEncoded p <<< NES.toString

-- | A version of [`decodeURIComponent`](https://pursuit.purescript.org/packages/purescript-globals/docs/Global#v:decodeURIComponent)
-- | that operates on non-empty strings.
decodeURIComponent' ∷ NonEmptyString → NonEmptyString
decodeURIComponent' =
  unsafePartial NES.unsafeFromString <<< decodeURIComponent <<< NES.toString
