module Data.URI.Authority where

import Control.Apply ((*>))
import Data.Array (catMaybes)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Tuple(Tuple(..))
import Data.URI.Common
import Data.URI.Host
import Data.URI.Types
import Data.URI.UserInfo
import Global (readInt, isNaN)
import Text.Parsing.StringParser (Parser(), fail)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy1)
import Text.Parsing.StringParser.String (string)

parseAuthority :: Parser Authority
parseAuthority = do
  ui <- optionMaybe parseUserInfo
  hosts <- flip sepBy1 (string ",") $ Tuple <$> parseHost
                                            <*> optionMaybe (string ":" *> parsePort)
  return $ Authority ui hosts

parsePort :: Parser Port
parsePort = do
  s <- rxPat "[0-9]+"
  let n = readInt 10 s
  if isNaN n && show n == s
    then fail "Expected valid port number"
    else pure (fromNumber n)

printAuthority :: Authority -> String
printAuthority (Authority u hs) =
  "//" ++ maybe "" (++ "@") u
       ++ joinWith "," (printHostAndPort <$> hs)
  where
  printHostAndPort (Tuple h p) = printHost h ++ maybe "" (\n -> ":" ++ show (toNumber n)) p
