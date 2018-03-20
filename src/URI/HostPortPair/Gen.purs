module URI.HostPortPair.Gen
  ( genHostPortPair
  , module URI.Host.Gen
  , module URI.Port.Gen
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import URI.Host.Gen (genHost, genIPv4, genRegName)
import URI.HostPortPair (HostPortPair)
import URI.Port.Gen (genPort)

-- | Generates a random `HostPortPair` for testing purposes.
genHostPortPair
  ∷ ∀ m host port
  . Gen.MonadGen m
  ⇒ m host
  → m port
  → m (HostPortPair host port)
genHostPortPair host port = do
  h ← sometimes 0.75 host
  p ← sometimes 0.25 port
  pure case h, p of
    Just h', Just p' → Just (Both h' p')
    Just h', Nothing → Just (This h')
    Nothing, Just p' → Just (That p')
    Nothing, Nothing → Nothing
  where
  sometimes ∷ ∀ a. Number → m a → m (Maybe a)
  sometimes chance g = do
    n ← Gen.chooseFloat 0.0 1.0
    if n > chance then Just <$> g else pure Nothing
