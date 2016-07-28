module Data.URI.Common where

import Prelude

import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.Either (Either(..), fromRight)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as Rx
import Data.Unfoldable (replicateA)
import Partial.Unsafe (unsafePartial)

import Text.Parsing.StringParser (ParseError(ParseError), Parser(Parser), unParser)
import Text.Parsing.StringParser.String (string)

joinWith ∷ String → List String → String
joinWith x y = S.joinWith x $ fromFoldable y

rep ∷ Int → Parser String → Parser String
rep n p = S.joinWith "" <$> replicateA n p

rxPat ∷ String → Parser String
rxPat rx =
  unsafePartial $ fromRight $ anyMatch <$> Rx.regex rx (Rx.noFlags { ignoreCase = true })

wrapParser ∷ ∀ a. Parser a → Parser String → Parser a
wrapParser outer inner = Parser \ps → do
  r <- unParser inner ps
  r' <- unParser outer {str : r.result, pos: 0}
  pure { result: r'.result, suffix: r.suffix }

parsePChar ∷ Parser String
parsePChar
  = parseUnreserved
  <|> parsePCTEncoded
  <|> parseSubDelims
  <|> string ":"
  <|> string "@"

parseUnreserved ∷ Parser String
parseUnreserved = rxPat "[0-9a-z\\-\\._~]+"

parsePCTEncoded ∷ Parser String
parsePCTEncoded = rxPat "%[0-9a-f]{2}"

parseSubDelims ∷ Parser String
parseSubDelims = rxPat "[!$&'()*+;=]"

anyMatch ∷ Rx.Regex → Parser String
anyMatch rx = Parser \{ str: str, pos: i } → case match1From rx i str of
  Just m → pure { result : m, suffix: { str: str, pos: i + (S.length m) }}
  Nothing → Left { error: (ParseError $ "Expected " <> show rx), pos: i }

match1From ∷ Rx.Regex → Int → String → Maybe String
match1From = match1FromImpl Just Nothing

foreign import match1FromImpl
  ∷ (∀ a. a → Maybe a)
  → (∀ a. Maybe a)
  → Rx.Regex
  → Int
  → String
  → (Maybe String)
