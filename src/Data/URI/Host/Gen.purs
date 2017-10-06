module Data.URI.Host.Gen where

import Prelude

import Control.Monad.Gen as Gen
import Data.String as S
import Data.URI.Host (Host(..))

genIPv4 :: forall m. Gen.MonadGen m => m Host
genIPv4 = do
  a <- Gen.chooseInt 0 255
  b <- Gen.chooseInt 0 255
  c <- Gen.chooseInt 0 255
  d <- Gen.chooseInt 0 255
  pure $ IPv4Address $ S.joinWith "." $ show <$> [a, b, c, d]
