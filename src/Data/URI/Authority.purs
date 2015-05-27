module Data.URI.Authority where

import Control.Apply ((*>))
import Data.Array (catMaybes)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.URI.Common
import Data.URI.Host
import Data.URI.Types
import Data.URI.UserInfo
import Global (readInt, isNaN)
import Text.Parsing.StringParser (Parser(), fail)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string)

parseAuthority :: Parser Authority
parseAuthority = Authority <$> optionMaybe (parseUserInfo)
                           <*> parseHost
                           <*> optionMaybe (string ":" *> parsePort)

parsePort :: Parser Port
parsePort = do
  s <- rxPat "[0-9]+"
  let n = readInt 10 s
  if isNaN n && show n == s
    then fail "Expected valid port number"
    else pure (fromNumber n)

printAuthority :: Authority -> String
printAuthority (Authority u h p) =
  "//" ++ joinWith "" (catMaybes [ (++ "@") <$> u
                                 , Just (printHost h)
                                 , (\n -> ":" ++ show (toNumber n)) <$> p
                                 ])
