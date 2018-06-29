module URI.Host.Gen where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as GenChar
import Data.String.Gen as GenString
import Data.String.NonEmpty.CodeUnits as NES
import URI.Host (Host(..), IPv4Address, RegName)
import URI.Host.IPv4Address as IPv4Address
import URI.Host.RegName as RegName

-- | Generates a random `IPv4Address` for testing purposes.
genIPv4 ∷ ∀ m. Gen.MonadGen m ⇒ m IPv4Address
genIPv4 = do
  a ← Gen.chooseInt 0 255
  b ← Gen.chooseInt 0 255
  c ← Gen.chooseInt 0 255
  d ← Gen.chooseInt 0 255
  pure $ IPv4Address.unsafeFromInts a b c d

-- | Generates a random `RegName` for testing purposes.
genRegName ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m RegName
genRegName = do
  head ← genAlphaNumeric
  tail ← GenString.genString genAlphaNumeric
  pure $ RegName.fromString $ NES.cons head tail
  where
    genAlphaNumeric = Gen.choose GenChar.genAlpha GenChar.genDigitChar

-- | Generates a random `Host` for testing purposes.
genHost ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m Host
genHost = Gen.choose (NameAddress <$> genRegName) (IPv4Address <$> genIPv4)
