module Data.URI.UserInfo where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.URI.Common (decodePCT, joinWith, parsePCTEncoded, parseSubDelims, parseUnreserved)
import Global (encodeURI)
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
    <|> parsePCTEncoded decodePCT
    <|> parseSubDelims
    <|> string ":"

print ∷ UserInfo → String
print (UserInfo u) = encodeURI u
