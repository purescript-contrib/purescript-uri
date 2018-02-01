module Data.URI.Common
  ( wrapParser
  , parsePChar
  , parseUnreserved
  , PCTEncoded
  , decodePCT
  , parsePCTEncoded
  , newParsePCTEncoded
  , parseSubDelims
  , printEncoded
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.String as String
import Global (decodeURI, encodeURIComponent)
import Text.Parsing.StringParser (ParseError, Parser(..), runParser, unParser)
import Text.Parsing.StringParser.String (alphaNum, anyChar, char, eof, oneOf, string)

wrapParser ∷ ∀ a b. (a → Either ParseError b) → Parser a → Parser b
wrapParser parseA p = Parser \ps → do
  pr ← unParser p ps
  case parseA pr.result of
    Left error → Left { error, pos: ps.pos }
    Right result → Right { result, suffix: pr.suffix }

parsePChar ∷ (PCTEncoded → String) → Parser String
parsePChar f
  = String.singleton <$> parseUnreserved
  <|> parsePCTEncoded f
  <|> String.singleton <$> parseSubDelims
  <|> string ":"
  <|> string "@"

parseUnreserved ∷ Parser Char
parseUnreserved = alphaNum <|> char '-' <|> char '.' <|> char '_' <|> char '~'

newParsePCTEncoded ∷ Parser String
newParsePCTEncoded = do
  d0 ← char '%'
  d1 ← alphaNum
  d2 ← alphaNum
  pure $ String.fromCharArray [d0, d1, d2]

parseSubDelims ∷ Parser Char
parseSubDelims =
  -- TODO: resolve the `,` situation
  oneOf ['!', '$', '&', '\'', '(', ')', '*', '+', {- ',', -} ';', '=']

newtype PCTEncoded = PCTEncoded String

decodePCT ∷ PCTEncoded → String
decodePCT (PCTEncoded s) = decodeURI s

parsePCTEncoded ∷ (PCTEncoded → String) → Parser String
parsePCTEncoded f = f <<< PCTEncoded <$> parseHex
  where
  parseHex = String.joinWith "" <$> Array.some do
    d0 ← char '%'
    d1 ← alphaNum
    d2 ← alphaNum
    pure $ String.fromCharArray [d0, d1, d2]

printEncoded ∷ Parser Char → String → String
printEncoded p s = either (const s) id (runParser parse s)
  where
  parse ∷ Parser String
  parse = (String.joinWith "" <$> Array.many (simpleChar <|> encodedChar)) <* eof
  simpleChar ∷ Parser String
  simpleChar = String.singleton <$> p
  encodedChar ∷ Parser String
  encodedChar = encodeURIComponent <<< String.singleton <$> anyChar
