module Data.URI.UserInfo 
  ( UserInfo(..)
  , parser
  , print
  )where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.String as Str
import Data.URI.Common (decodePCTComponent, joinWith, parsePCTEncoded, parseSubDelims, parseUnreserved)
import Global (encodeURIComponent)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
newtype UserInfo = UserInfo String

derive newtype instance eqUserInfo ∷ Eq UserInfo
derive newtype instance ordUserInfo ∷ Ord UserInfo
derive instance genericUserInfo ∷ Generic UserInfo _
derive instance newtypeUserInfo ∷ Newtype UserInfo _
instance showUserInfo ∷ Show UserInfo where show = genericShow

parser ∷ Parser UserInfo
parser = UserInfo <<< joinWith "" <$> many1 p
  where
  p = parseUnreserved
    <|> parsePCTEncoded decodePCTComponent
    <|> parseSubDelims
    <|> string ":"

print ∷ UserInfo → String
print (UserInfo u) = encodeUserPassword u


encodeUserPassword :: String -> String
encodeUserPassword s = foldMap encodeChar $ Str.toCharArray s

shouldNotEscape :: Char -> Boolean
shouldNotEscape c =
  {-
  https://tools.ietf.org/html/rfc3986#section-3.2.1
  userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

  https://tools.ietf.org/html/rfc3986#section-2.1
  pct-encoded = "%" HEXDIG HEXDIG

  https://tools.ietf.org/html/rfc3986#section-2.3
  unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"

  https://tools.ietf.org/html/rfc3986#section-2.1
  sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                  / "*" / "+" / "," / ";" / "="
  -}
    -- unreserved
    ('A' <= c && c <= 'Z')
    || ('a' <= c && c <= 'z')
    || ('0' <= c && c <= '9')
    || c == '-' || c ==  '_' || c ==  '.' || c ==  '~'
    -- sub-delims
    || c == '!' || c == '$' || c == '&' || c == '\'' 
    || c == '(' || c == ')' || c == '*' || c == '+'
    || c == ',' || c == ';' || c == '='
    -- userinfo
    || c == ':'
    
encodeChar :: Char -> String
encodeChar c =
  let cStr = Str.singleton c
  in if shouldNotEscape c then cStr else encodeURIComponent cStr