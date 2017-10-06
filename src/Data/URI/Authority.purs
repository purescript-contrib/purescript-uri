module Data.URI.Authority
  ( Authority(..)
  , parser
  , print
  , _userInfo
  , _hosts
  , module Data.URI.Host
  , module Data.URI.Port
  , module Data.URI.UserInfo
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe, maybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Data.URI.Host (Host(..), _IPv4Address, _IPv6Address, _NameAddress)
import Data.URI.Host as Host
import Data.URI.Port (Port(..))
import Data.URI.Port as Port
import Data.URI.UserInfo (UserInfo(..))
import Data.URI.UserInfo as UserInfo
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy)
import Text.Parsing.StringParser.String (string)

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority = Authority (Maybe UserInfo) (Array (Tuple Host (Maybe Port)))

derive instance eqAuthority ∷ Eq Authority
derive instance ordAuthority ∷ Ord Authority
derive instance genericAuthority ∷ Generic Authority _
instance showAuthority ∷ Show Authority where show = genericShow

parser ∷ Parser Authority
parser = do
  _ ← string "//"
  ui ← optionMaybe $ try (UserInfo.parser <* string "@")
  hosts ← flip sepBy (string ",") $
    Tuple <$> Host.parser <*> optionMaybe (string ":" *> Port.parser)
  pure $ Authority ui (fromFoldable hosts)

print ∷ Authority → String
print (Authority ui hs) =
  "//" <> printUserInfo <> S.joinWith "," (printHostAndPort <$> hs)
  where
  printUserInfo =
    maybe "" (\u → UserInfo.print u <> "@") ui
  printHostAndPort (Tuple h p) =
    Host.print h <> maybe "" (\n → ":" <> Port.print n) p

_userInfo ∷ Lens' Authority (Maybe UserInfo)
_userInfo =
  lens
    (\(Authority ui _) → ui)
    (\(Authority _ hs) ui → Authority ui hs)

_hosts ∷ Lens' Authority (Array (Tuple Host (Maybe Port)))
_hosts =
  lens
    (\(Authority _ hs) → hs)
    (\(Authority ui _) hs → Authority ui hs)
