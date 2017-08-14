module Data.URI.Authority where

import Prelude

import Data.Array (fromFoldable)
import Data.Maybe (maybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Data.URI (Authority(..))
import Data.URI.Host as Host
import Data.URI.Port as Port
import Data.URI.UserInfo as UserInfo
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy)
import Text.Parsing.StringParser.String (string)

parser ∷ Parser Authority
parser = do
  ui ← optionMaybe $ try (UserInfo.parser <* string "@")
  hosts ← flip sepBy (string ",") $
    Tuple <$> Host.parser <*> optionMaybe (string ":" *> Port.parser)
  pure $ Authority ui (fromFoldable hosts)

print ∷ Authority → String
print (Authority ui hs) =
  printUserInfo <> S.joinWith "," (printHostAndPort <$> hs)
  where
  printUserInfo =
    maybe "" (\u → UserInfo.print u <> "@") ui
  printHostAndPort (Tuple h p) =
    Host.print h <> maybe "" (\n → ":" <> Port.print n) p
