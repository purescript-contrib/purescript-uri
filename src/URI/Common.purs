-- | Common functions used in parsing and printing URI components.
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
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (singleton) as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (joinWith, toString, unsafeFromString) as NES
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import JSURI (decodeURIComponent, encodeURIComponent)
import Parsing (ParseError(..), ParseState(..), Parser, ParserT, getParserT, initialPos, runParser)
import Parsing.String (anyChar, char, eof, satisfy)
import Parsing.String.Basic (oneOf)
import Parsing.Token (digit, hexDigit)
import Partial.Unsafe (unsafePartial)

-- | An error type used when a custom component parser fails to handle a value.
newtype URIPartParseError = URIPartParseError String

derive newtype instance eqURIPartParseError :: Eq URIPartParseError
derive newtype instance ordURIPartParseError :: Ord URIPartParseError
derive instance newtypeURIPartParseError :: Newtype URIPartParseError _
derive instance genericURIPartParseError :: Generic URIPartParseError _

instance showURIPartParseError :: Show URIPartParseError where
  show = genericShow

-- | Adapts a parser with a parser-esque function. First the original
-- | parser runs, then it attempts to refine the result with the function.
wrapParser
  :: forall s m a b
   . Monad m
  => (a -> Either URIPartParseError b)
  -> ParserT s m a
  -> ParserT s m b
wrapParser parseA p = do
  (ParseState _ pos _) <- getParserT
  a <- p
  case parseA a of
    Left (URIPartParseError err) -> throwError (ParseError err pos)
    Right b -> pure b

-- | Parser for ascii alphabetical characters (upper and lowercase).
alpha :: Parser String Char
alpha = satisfy \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Parser for ascii alphanumeric characters (upper and lowercase for letters).
alphaNum :: Parser String Char
alphaNum = alpha <|> digit

-- | Parser for characters that are allowed in a URI but do not have a reserved
-- | purpose.
unreserved :: Parser String Char
unreserved = alphaNum <|> char '-' <|> char '.' <|> char '_' <|> char '~'

-- | Parser for the "sub-delims" group of reserved characters.
subDelims :: Parser String Char
subDelims =
  oneOf [ '!', '$', '&', '\'', '(', ')', '*', '+', ';', '=', ',' ]

-- | Parser for a percent-encoded character.
pctEncoded :: Parser String NonEmptyString
pctEncoded = do
  d0 <- char '%'
  d1 <- hexDigit
  d2 <- hexDigit
  pure $ NES.singleton d0 <> NES.singleton d1 <> NES.singleton d2

-- | A helper function for printing URI components using percent-encoding for
-- | characters that require it.
-- |
-- | Accepts a parser that is used to determine whether a character is allowed
-- | to appear un-encoded in the URI component and the string to encode.
printEncoded :: Parser String Char -> String -> String
printEncoded p s = either (const s) identity (runParser s parse)
  where
  parse :: Parser String String
  parse = (NES.joinWith "" <$> List.manyRec (simpleChar <|> encodedChar)) <* eof

  simpleChar :: Parser String NonEmptyString
  simpleChar = NES.singleton <$> p

  encodedChar :: Parser String NonEmptyString
  encodedChar = handleURIEncodingResult =<< encodeURIComponent <<< String.singleton <$> anyChar

  handleURIEncodingResult :: Maybe String -> Parser String NonEmptyString
  handleURIEncodingResult Nothing =
    -- E.g. when there is a lone surrogate. See encodeURIComponent MDN documentation.
    throwError $ ParseError "Could not URI encode" initialPos
  handleURIEncodingResult (Just encoded) =
    pure $ unsafePartial (NES.unsafeFromString encoded)

-- | A version of [`printEncoded`](#v:printEncoded) that operates on non-empty
-- | strings.
printEncoded' :: Parser String Char -> NonEmptyString -> NonEmptyString
printEncoded' p =
  unsafePartial NES.unsafeFromString <<< printEncoded p <<< NES.toString

-- | A version of [`decodeURIComponent`](https://pursuit.purescript.org/packages/purescript-jsuri/docs/JSURI#v:decodeURIComponent)
-- | that operates on non-empty strings.
decodeURIComponent' :: NonEmptyString -> NonEmptyString
decodeURIComponent' =
  unsafePartial NES.unsafeFromString <<< unsafePartial fromJust <<< decodeURIComponent <<< NES.toString
