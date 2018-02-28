module URI.Extra.UserPassInfo
  ( UserPassInfo(..)
  , parse
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)
import URI.Common (parseSubDelims, parseUnreserved, printEncoded)
import URI.UserInfo (UserInfo)
import URI.UserInfo as UserInfo

newtype UserPassInfo = UserPassInfo { user ∷ String, password ∷ Maybe String }

derive instance eqUserPassInfo ∷ Eq UserPassInfo
derive instance ordUserPassInfo ∷ Ord UserPassInfo

instance showUserPassInfo ∷ Show UserPassInfo where
  show (UserPassInfo { user, password }) =
    "(UserPassInfo { user: " <> show user <> ", password: " <> show password <> "})"

parse ∷ ∀ e. UserInfo → Either e UserPassInfo
parse ui =
  let
    s = UserInfo.unsafeToString ui
  in
    pure $ UserPassInfo
      case flip String.splitAt s =<< String.indexOf (String.Pattern ":") s of
        Just { before, after } →
          { user: decodeURIComponent before
          , password: Just (decodeURIComponent (String.drop 1 after))
          }
        Nothing →
          { user: decodeURIComponent s, password: Nothing }

print ∷ UserPassInfo → UserInfo
print (UserPassInfo { user, password }) =
  case password of
    Nothing →
      UserInfo.unsafeFromString (printEncoded userPassInfoChar user)
    Just p →
      UserInfo.unsafeFromString (printEncoded userPassInfoChar user)
        <> UserInfo.unsafeFromString ":"
        <> UserInfo.unsafeFromString (printEncoded userPassInfoChar p)

userPassInfoChar ∷ Parser String Char
userPassInfoChar = parseUnreserved <|> parseSubDelims
