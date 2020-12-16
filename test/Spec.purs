module Test.Spec where

import Prelude

import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Data.Monoid (power, guard)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Test.Assert (assertEqual)

-----------------------------------------------------------------
-- Provides a similar API to purescript-spec, but without a dependency

type Spec a = ReaderT Int Aff a

describe :: String -> Spec Unit -> Spec Unit
describe msg runTest = do
  indentation <- ask
  let spacing = guard (indentation > 0) " "
  liftEffect $ log $ (power ">>" indentation) <> spacing <> msg
  local (_ + 1) runTest

it :: String -> Spec Unit -> Spec Unit
it = describe

shouldEqual :: forall m a. MonadEffect m => Eq a => Show a => a -> a -> m Unit
shouldEqual actual expected =
  liftEffect $ assertEqual { actual, expected }

fail :: forall m. MonadThrow Error m => String -> m Unit
fail = throwError <<< error
