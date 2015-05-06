module Data.URI.Authority where

import Control.Apply ((*>))
import Data.URI.Common
import Data.URI.Host
import Data.URI.Types
import Data.URI.UserInfo
import Text.Parsing.StringParser (Parser())
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (string)

parseAuthority :: Parser Authority
parseAuthority = Authority <$> optionMaybe (parseUserInfo)
                           <*> parseHost
                           <*> optionMaybe (string ":" *> parsePort)

parsePort :: Parser Port
parsePort = rxPat "[0-9]+"
