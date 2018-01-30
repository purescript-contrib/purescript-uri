module Data.URI.Common
  ( wrapParser
  , parsePChar
  , parseUnreserved
  , PCTEncoded
  , decodePCT
  , decodePCTComponent
  , parsePCTEncoded
  , parseSubDelims
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.String as String
import Global (decodeURI, decodeURIComponent)
import Text.Parsing.StringParser (ParseError, Parser(..), unParser)
import Text.Parsing.StringParser.String (alphaNum, char, oneOf, string)

wrapParser ∷ ∀ a. (String → Either ParseError a) → Parser String → Parser a
wrapParser parseA p = Parser \ps → do
  pr ← unParser p ps
  case parseA pr.result of
    Left error → Left { error, pos: ps.pos }
    Right result → Right { result, suffix: pr.suffix }

parsePChar ∷ (PCTEncoded → String) → Parser String
parsePChar f
  = parseUnreserved
  <|> parsePCTEncoded f
  <|> parseSubDelims
  <|> string ":"
  <|> string "@"

parseUnreserved ∷ Parser String
parseUnreserved =
  String.singleton <$> (alphaNum <|> char '-' <|> char '.' <|> char '_' <|> char '~')

newtype PCTEncoded = PCTEncoded String

decodePCT ∷ PCTEncoded → String
decodePCT (PCTEncoded s) = decodeURI s

decodePCTComponent ∷ PCTEncoded → String
decodePCTComponent (PCTEncoded s) = decodeURIComponent s

parsePCTEncoded ∷ (PCTEncoded → String) → Parser String
parsePCTEncoded f = f <<< PCTEncoded <$> parseHex
  where
  parseHex = String.joinWith "" <$> Array.some do
    d0 ← char '%'
    d1 ← alphaNum
    d2 ← alphaNum
    pure $ String.fromCharArray [d0, d1, d2]

parseSubDelims ∷ Parser String
parseSubDelims =
  -- TODO: resolve the `,` situation
  String.singleton <$> oneOf ['!', '$', '&', '\'', '(', ')', '*', '+', {- ',', -} ';', '=']
