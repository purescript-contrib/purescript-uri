module URI.Extra.UserPassInfo
  ( UserPassInfo(..)
  , parse
  , print
  , userPassInfoChar
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CodeUnits (singleton, splitAt, indexOf, drop) as NES
import Text.Parsing.Parser (Parser)
import URI.Common (URIPartParseError(..), decodeURIComponent', subDelims, unreserved, printEncoded')
import URI.UserInfo (UserInfo)
import URI.UserInfo as UserInfo

-- | `user:password` formatted user-info components for URI authorities.
-- |
-- | This format is considered deprecated according to RFC3986 but is still
-- | very common, so this is provided for cases where it is necessary.
-- |
-- | The username part is required, so a value like `:hello` will fail to parse
-- | for this type.
-- |
-- | The `:` characer will be percent-encoded in all locations other than the
-- | `user:password` separator, although the parser will accept passwords
-- | containing un-encoded `:` characters.
newtype UserPassInfo =
  UserPassInfo
    { user :: NonEmptyString
    , password :: Maybe NonEmptyString
    }

derive instance eqUserPassInfo :: Eq UserPassInfo
derive instance ordUserPassInfo :: Ord UserPassInfo
derive instance newtypeUserPassInfo :: Newtype UserPassInfo _

instance showUserPassInfo :: Show UserPassInfo where
  show (UserPassInfo { user, password }) =
    "(UserPassInfo { user: " <> show user <> ", password: " <> show password <> "})"

-- | A parser for `user:password` formatted user-info.
parse :: UserInfo -> Either URIPartParseError UserPassInfo
parse ui =
  let
    s = UserInfo.unsafeToString ui
  in
    case flip NES.splitAt s <$> NES.indexOf (String.Pattern ":") s of
      Just { before: Nothing } ->
        Left (URIPartParseError "Expected a username before a password segment")
      Just { before: Just before, after: Just after } ->
        Right $ UserPassInfo
          { user: decodeURIComponent' before
          , password: decodeURIComponent' <$> NES.drop 1 after
          }
      _ ->
        Right $ UserPassInfo
          { user: decodeURIComponent' s, password: Nothing }

-- | A printer for `user:password` formatted user-info.
print :: UserPassInfo -> UserInfo
print (UserPassInfo { user, password }) =
  case password of
    Nothing ->
      UserInfo.unsafeFromString (printEncoded' userPassInfoChar user)
    Just p ->
      UserInfo.unsafeFromString
        $ printEncoded' userPassInfoChar user
            <> NES.singleton ':'
            <> printEncoded' userPassInfoChar p

-- | The supported user/password characters, excluding percent-encodings.
userPassInfoChar :: Parser String Char
userPassInfoChar = unreserved <|> subDelims
