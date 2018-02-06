module Data.URI.Host.Gen where

import Prelude

import Control.Monad.Gen as Gen
import Data.URI.Host (IPv4Address)
import Data.URI.Host.IPv4Address as IPv4Address

genIPv4 ∷ ∀ m. Gen.MonadGen m ⇒ m IPv4Address
genIPv4 = do
  a ← Gen.chooseInt 0 255
  b ← Gen.chooseInt 0 255
  c ← Gen.chooseInt 0 255
  d ← Gen.chooseInt 0 255
  pure $ IPv4Address.unsafeFromInts a b c d
