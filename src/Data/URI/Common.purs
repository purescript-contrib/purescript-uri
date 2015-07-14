module Data.URI.Common where

import Prelude
import Control.Alt ((<|>))
import Data.Array (replicateM)
import Data.List (List(..), fromList)
import Data.Function (Fn5(), runFn5)
import Data.Maybe (Maybe(..))
import qualified Data.String as S
import Data.URI.Types
import Text.Parsing.StringParser (Parser(..), ParseError(..), unParser)
import Text.Parsing.StringParser.String (string)
import qualified Data.String.Regex as Rx

joinWith :: String -> List String -> String
joinWith x y = S.joinWith x $ fromList y

rep :: Int -> Parser String -> Parser String
rep n p = S.joinWith "" <$> replicateM n p

rxPat :: String -> Parser String
rxPat rx = anyMatch $ Rx.regex rx (Rx.noFlags { ignoreCase = true })

wrapParser :: forall a. Parser a -> Parser String -> Parser a
wrapParser outer inner = Parser \ps fc sc ->
  unParser inner ps fc (\s ps' ->
    unParser outer { str: s, pos: 0 } fc (\s' _ -> sc s' ps'))

parsePChar :: Parser String
parsePChar = parseUnreserved
         <|> parsePCTEncoded
         <|> parseSubDelims
         <|> string ":"
         <|> string "@"

parseUnreserved :: Parser String
parseUnreserved = rxPat "[0-9a-z\\-\\._~]+"

parsePCTEncoded :: Parser String
parsePCTEncoded = rxPat "%[0-9a-f]{2}"

parseSubDelims :: Parser String
parseSubDelims = rxPat "[!$&'()*+;=]"

anyMatch :: Rx.Regex -> Parser String
anyMatch rx = Parser \{ str: str, pos: i } fc sc -> case match1From rx i str of
  Just m -> sc m { str: str, pos: i + (S.length m) }
  Nothing -> fc i (ParseError $ "Expected " ++ show rx)

match1From :: Rx.Regex -> Int -> String -> Maybe String
match1From rx i str = runFn5 _match1From Just Nothing rx i str

foreign import _match1From :: forall a. Fn5 (a -> Maybe a) (Maybe a) Rx.Regex Int String (Maybe String)
