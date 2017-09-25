module Data.URI.Common where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..), fromRight)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Unfoldable (replicateA)
import Global (decodeURI, decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError(..), Parser(..), unParser)
import Text.Parsing.StringParser.String (string)

joinWith ∷ String → List String → String
joinWith x y = S.joinWith x $ fromFoldable y

rep ∷ Int → Parser String → Parser String
rep n p = S.joinWith "" <$> replicateA n p

rxPat ∷ String → Parser String
rxPat rx =
  unsafePartial $ fromRight $ anyMatch <$> RX.regex rx RXF.ignoreCase

wrapParser ∷ ∀ a. Parser a → Parser String → Parser a
wrapParser outer inner = Parser \ps → do
  r ← unParser inner ps
  r' ← unParser outer {str : r.result, pos: 0}
  pure { result: r'.result, suffix: r.suffix }

parsePChar ∷ (PCTEncoded → String) → Parser String
parsePChar f
  = parseUnreserved
  <|> parsePCTEncoded f
  <|> parseSubDelims
  <|> string ":"
  <|> string "@"

parseUnreserved ∷ Parser String
parseUnreserved = rxPat "[0-9a-z\\-\\._~]+"

parseFragmentOrQuery ∷ Parser String
parseFragmentOrQuery = parsePChar decodePCTComponent <|> string "/" <|> string "?"

printFragmentOrQuery ∷ String → String
printFragmentOrQuery = S.joinWith "" <<< map printChar <<< S.split (S.Pattern "")
  where
  -- Fragments & queries have a bunch of characters that don't need escaping
  printChar ∷ String → String
  printChar s
    | RX.test rxPrintable s = s
    | otherwise = encodeURIComponent s

rxPrintable ∷ RX.Regex
rxPrintable = unsafePartial fromRight $ RX.regex "[$&+;=/?:@]" RXF.global

newtype PCTEncoded = PCTEncoded String

decodePCT ∷ PCTEncoded → String
decodePCT (PCTEncoded s) = decodeURI s

decodePCTComponent ∷ PCTEncoded → String
decodePCTComponent (PCTEncoded s) = decodeURIComponent s

parsePCTEncoded ∷ (PCTEncoded → String) → Parser String
parsePCTEncoded f = f <<< PCTEncoded <$> rxPat "(%[0-9a-f]{2})+"

parseSubDelims ∷ Parser String
parseSubDelims = rxPat "[!$&'()*+;=]"

anyMatch ∷ RX.Regex → Parser String
anyMatch rx = Parser \{ str: str, pos: i } → case match1From rx i str of
  Just m → pure { result : m, suffix: { str: str, pos: i + (S.length m) }}
  Nothing → Left { error: (ParseError $ "Expected " <> show rx), pos: i }

match1From ∷ RX.Regex → Int → String → Maybe String
match1From rx' n str' =
  case RX.regex (RX.source rx') (RXF.global <> RX.flags rx') of
    Left _ -> Nothing
    Right rx -> do
      let str = S.drop n str'
      i <- RX.search rx str
      guard $ i == 0
      matches <- RX.match rx str
      join $ head matches
