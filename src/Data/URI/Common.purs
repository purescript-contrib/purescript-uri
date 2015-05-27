module Data.URI.Common where

import Control.Alt ((<|>))
import Control.Monad (replicateM)
import Data.Function (Fn5(), runFn5)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, length)
import Data.URI.Types
import Text.Parsing.StringParser (Parser(..), ParseError(..), unParser)
import Text.Parsing.StringParser.String (string)
import qualified Data.String.Regex as Rx

rep :: Number -> Parser String -> Parser String
rep n p = joinWith "" <$> replicateM n p

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
  Just m -> sc m { str: str, pos: i + (length m) }
  Nothing -> fc i (ParseError $ "Expected " ++ show rx)

match1From :: Rx.Regex -> Number -> String -> Maybe String
match1From rx i str = runFn5 match1FromImpl Just Nothing rx i str

foreign import match1FromImpl
  """
  function match1FromImpl(just, nothing, rx, i, str) {
    var rxStr = rx.toString();
    var flagIndex = rxStr.lastIndexOf("/");
    var pattern = rxStr.substring(1, flagIndex);
    var flags = rxStr.substring(flagIndex + 1);
    rx = new RegExp(pattern, flags.indexOf("g") === -1 ? flags + "g" : flags);
    rx.lastIndex = i;
    var result = rx.exec(str);
    return result && result.index === i ? just(result[0]) : nothing;
  }
  """ :: forall a. Fn5 (a -> Maybe a) (Maybe a) Rx.Regex Number String (Maybe String)
