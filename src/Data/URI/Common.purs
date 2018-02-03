module Data.URI.Common
  ( wrapParser
  , parseUnreserved
  , newParsePCTEncoded
  , parseSubDelims
  , printEncoded
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.String as String
import Global (encodeURIComponent)
import Text.Parsing.StringParser (ParseError, Parser(..), runParser, unParser)
import Text.Parsing.StringParser.String (alphaNum, anyChar, char, eof, oneOf)

wrapParser ∷ ∀ a b. (a → Either ParseError b) → Parser a → Parser b
wrapParser parseA p = Parser \ps → do
  pr ← unParser p ps
  case parseA pr.result of
    Left error → Left { error, pos: ps.pos }
    Right result → Right { result, suffix: pr.suffix }

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

printEncoded ∷ Parser Char → String → String
printEncoded p s = either (const s) id (runParser parse s)
  where
  parse ∷ Parser String
  parse = (String.joinWith "" <$> Array.many (simpleChar <|> encodedChar)) <* eof
  simpleChar ∷ Parser String
  simpleChar = String.singleton <$> p
  encodedChar ∷ Parser String
  encodedChar = encodeURIComponent <<< String.singleton <$> anyChar
