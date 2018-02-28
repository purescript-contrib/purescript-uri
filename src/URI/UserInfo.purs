module URI.UserInfo
  ( UserInfo
  , fromString
  , toString
  , unsafeFromString
  , unsafeToString
  , parser
  , print
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.String as String
import Global (decodeURIComponent)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (URIPartParseError, parseSubDelims, parseUnreserved, pctEncoded, printEncoded, wrapParser)

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
newtype UserInfo = UserInfo String

derive newtype instance eqUserInfo ∷ Eq UserInfo
derive newtype instance ordUserInfo ∷ Ord UserInfo
derive newtype instance semigroupUserInfo ∷ Semigroup UserInfo
derive newtype instance monoidUserInfo ∷ Monoid UserInfo

instance showUserInfo ∷ Show UserInfo where
  show (UserInfo s) = "(UserInfo.unsafeFromString " <> show s <> ")"

-- | Constructs a `UserInfo` part safely: percent-encoding will be
-- | applied to any character that requires it for the user-info component of a
-- | URI.
fromString ∷ String → UserInfo
fromString = UserInfo <<< printEncoded userInfoChar

-- | Prints `UserInfo` as a string, decoding any percent-encoded
-- | characters contained within.
toString ∷ UserInfo → String
toString (UserInfo s) = decodeURIComponent s

-- | Constructs a `UserInfo` part unsafely: no encoding will be applied
-- | to the value. If an incorrect value is provided, the URI will be invalid
-- | when printed back.
unsafeFromString ∷ String → UserInfo
unsafeFromString = UserInfo

-- | Prints `UserInfo` as a string without performing any decoding of
-- | percent-encoded octets. Only "unsafe" in the sense that values this
-- | produces will need further decoding, the name is more for symmetry with
-- | the `fromString`/`toString` and `unsafeFromString`/`unsafeToString`
-- | pairings.
unsafeToString ∷ UserInfo → String
unsafeToString (UserInfo s) = s

parser ∷ ∀ ui. (UserInfo → Either URIPartParseError ui) → Parser String ui
parser p = wrapParser p (UserInfo <<< String.joinWith "" <$> Array.some parse)
  where
  parse ∷ Parser String String
  parse = String.singleton <$> userInfoChar <|> pctEncoded

print ∷ ∀ ui. (ui → UserInfo) → ui → String
print = map unsafeToString

-- | The supported user info characters, excluding percent-encodings.
userInfoChar ∷ Parser String Char
userInfoChar = parseUnreserved <|> parseSubDelims <|> char ':'
