module Data.URI.Authority where

import Prelude
import Control.Apply ((*>))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple(Tuple(..))
import Data.List (fromList)
import Data.URI.Common
import Data.URI.Host
import Data.URI.Types
import Data.URI.UserInfo
import Global (readInt)
import qualified Data.String as S
import Text.Parsing.StringParser (Parser(), fail)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy)
import Text.Parsing.StringParser.String (string)

parseAuthority :: Parser Authority
parseAuthority = do
  ui <- optionMaybe parseUserInfo
  hosts <- flip sepBy (string ",")
           $ Tuple
             <$> parseHost
             <*> optionMaybe (string ":" *> parsePort)
  return $ Authority ui (fromList hosts)

parsePort :: Parser Port
parsePort = do
 s <- rxPat "[0-9]+"
 case fromNumber $ readInt 10 s of
    Just x  -> pure x
    _       -> fail "Expected valid port number"

printAuthority :: Authority -> String
printAuthority (Authority u hs) =
  "//" ++ maybe "" (++ "@") u
       ++ S.joinWith "," (printHostAndPort <$> hs)
  where
  printHostAndPort (Tuple h p) = printHost h ++ maybe "" (\n -> ":" ++ show n) p
