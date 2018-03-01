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
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import URI.Common (decodeURIComponent', parseSubDelims, parseUnreserved, pctEncoded, printEncoded')

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

-- | Constructs a user-info value from a string, percent-encoding any characters
-- | that require it. Note that running this on a string that has already had
-- | percent-encoding applied will double-encode it, for those situations use
-- | `unsafeFromString` instead.
-- |
-- | ``` purescript
-- | fromString "foo" = unsafeFromString "foo"
-- | fromString "foo@bar" = unsafeFromString "foo%40bar"
-- | fromString "foo%40bar" = unsafeFromString "foo%2540bar"
-- | ```
fromString ∷ NonEmptyString → UserInfo
fromString = UserInfo <<< printEncoded' userInfoChar

-- | Returns the string value for user-info, percent-decoding any characters
-- | that require it.
-- |
-- | ``` purescript
-- | toString (unsafeFromString "foo") = "foo"
-- | toString (unsafeFromString "foo%40bar") = "foo@bar"
-- | ```
toString ∷ UserInfo → NonEmptyString
toString (UserInfo s) = decodeURIComponent' s

-- | Constructs a user-info value from a string directly - no percent-encoding
-- | will be applied. This is useful when using a custom encoding scheme for
-- | the query, to prevent double-encoding.
unsafeFromString ∷ NonEmptyString → UserInfo
unsafeFromString = UserInfo

-- | Returns the string value for user-info without percent-decoding. Only
-- | "unsafe" in the sense that values this produces may need further decoding,
-- | the name is more for symmetry with the `fromString`/`unsafeFromString`
-- | pairing.
unsafeToString ∷ UserInfo → NonEmptyString
unsafeToString (UserInfo s) = s

-- | A parser for the user-info component of a URI.
parser ∷ Parser String UserInfo
parser =
  UserInfo
    <<< unsafePartial NES.unsafeFromString
    <<< String.joinWith "" <$> Array.some parse
  where
  parse ∷ Parser String String
  parse = String.singleton <$> userInfoChar <|> pctEncoded

-- | A printer for the user-info component of a URI.
print ∷ UserInfo → String
print = NES.toString <<< unsafeToString

-- | The supported user info characters, excluding percent-encodings.
userInfoChar ∷ Parser String Char
userInfoChar = parseUnreserved <|> parseSubDelims <|> char ':'
