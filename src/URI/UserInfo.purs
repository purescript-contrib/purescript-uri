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
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Global (decodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (parseSubDelims, parseUnreserved, pctEncoded, printEncoded)

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
-- |
-- | This type treats the entire string as an undifferentiated blob, if you
-- | would like to specifically deal with the `user:password` format, take a
-- | look at `URI.Extra.UserPassInfo`.
newtype UserInfo = UserInfo NonEmptyString

derive newtype instance eqUserInfo ∷ Eq UserInfo
derive newtype instance ordUserInfo ∷ Ord UserInfo
derive newtype instance semigroupUserInfo ∷ Semigroup UserInfo

instance showUserInfo ∷ Show UserInfo where
  show (UserInfo s) = "(UserInfo.unsafeFromString " <> show s <> ")"

-- | Constructs a `UserInfo` part safely: percent-encoding will be
-- | applied to any character that requires it for the user-info component of a
-- | URI.
fromString ∷ NonEmptyString → UserInfo
fromString = UserInfo <<< nes <<< printEncoded userInfoChar <<< NES.toString

-- | Prints `UserInfo` as a string, decoding any percent-encoded characters
-- | contained within.
toString ∷ UserInfo → NonEmptyString
toString (UserInfo s) = nes (decodeURIComponent (NES.toString s))

-- | Constructs a `UserInfo` part unsafely: no encoding will be applied
-- | to the value. If an incorrect value is provided, the URI will be invalid
-- | when printed back.
unsafeFromString ∷ NonEmptyString → UserInfo
unsafeFromString = UserInfo

-- | Prints `UserInfo` as a string without performing any decoding of
-- | percent-encoded octets. Only "unsafe" in the sense that values this
-- | produces will need further decoding, the name is more for symmetry with
-- | the `fromString`/`toString` and `unsafeFromString`/`unsafeToString`
-- | pairings.
unsafeToString ∷ UserInfo → NonEmptyString
unsafeToString (UserInfo s) = s

-- | A parser for the `UserInfo` part of a URI.
parser ∷ Parser String UserInfo
parser = UserInfo <<< nes <<< String.joinWith "" <$> Array.some parse
  where
  parse ∷ Parser String String
  parse = String.singleton <$> userInfoChar <|> pctEncoded

-- | A printer for the `UserInfo` part of a URI.
print ∷ UserInfo → String
print = NES.toString <<< unsafeToString

-- | The supported user info characters, excluding percent-encodings.
userInfoChar ∷ Parser String Char
userInfoChar = parseUnreserved <|> parseSubDelims <|> char ':'

nes ∷ String → NonEmptyString
nes = unsafePartial NES.unsafeFromString
