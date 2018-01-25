module Data.URI.UserInfo where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.URI.Common (decodePCTComponent, parsePCTEncoded, joinWith, parseSubDelims, parseUnreserved)
import Data.String as Str
import Data.Foldable (foldMap)
import Global (encodeURIComponent)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
newtype UserInfo = UserInfo { username :: String, password :: String }

derive instance eqUserInfo ∷ Eq UserInfo
derive instance ordUserInfo ∷ Ord UserInfo
derive instance genericUserInfo ∷ Generic UserInfo _
derive instance newtypeUserInfo ∷ Newtype UserInfo _
instance showUserInfo ∷ Show UserInfo where show = genericShow

parser ∷ Parser UserInfo
parser = do 
  username <- authPart
  password <- (try (string ":") *> authPart) <|> pure ""
  pure $ UserInfo { username, password }
  where
  authPart = map (joinWith "") $ many1 $ parseUnreserved
    <|> parsePCTEncoded decodePCTComponent
    <|> parseSubDelims

print ∷ UserInfo → String
print (UserInfo {username, password }) = 
  let encodedUsername = encodeUserPassword username
  in if password == "" 
    then encodedUsername
    else encodedUsername <> ":" <> encodeUserPassword password

encodeUserPassword :: String -> String
encodeUserPassword s = foldMap encodeChar $ Str.toCharArray s
  where
  -- `shouldEscape` is adopted from "net/url" of golang where mode is `encodeUserPassword`
  -- https://golang.org/src/net/url/url.go?s=10264:10346#L102
  shouldEscape c = 
    -- §2.3 Unreserved characters (alphanum)
    if 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' || '0' <= c && c <= '9'
      then false 
    -- §2.3 Unreserved characters (mark)
    else if c == '-' || c ==  '_' || c ==  '.' || c ==  '~'
      then false 
    -- §2.2 Reserved characters (reserved)
    else if c == '$' || c == '&' || c == '+' || c == ',' || c == '/' || c == ':' || c == ';' || c == '=' || c == '?' || c == '@'
      -- The RFC allows ';', ':', '&', '=', '+', '$', and ',' in
      -- userinfo, so we must escape only '@', '/', and '?'.
      -- The parsing of userinfo treats ':' as special so we must escape
      -- that too.
      then c == '@' || c == '/' || c == '?' || c == ':'
    else true
    
    
  encodeChar :: Char -> String
  encodeChar c =
    let cStr = Str.singleton c
    in if shouldEscape c then encodeURIComponent cStr else cStr