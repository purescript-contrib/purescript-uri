module URI.Port.Gen where

import Prelude

import Control.Monad.Gen as Gen
import URI.Port (Port)
import URI.Port as Port

-- | Generates a random `Port` for testing purposes.
genPort ∷ ∀ m. Gen.MonadGen m ⇒ m Port
genPort = Port.unsafeFromInt <$> Gen.chooseInt 0 65535
