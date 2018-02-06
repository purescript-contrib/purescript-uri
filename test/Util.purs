module Test.Util where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(..))
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)
import Text.Parsing.Parser (Parser, runParser)

type TestEffects eff = (console ∷ CONSOLE, random ∷ RANDOM, exception ∷ EXCEPTION | eff)

testPrinter ∷ ∀ a eff. Show a ⇒ (a → String) → String → a → Spec eff Unit
testPrinter f expected value =
  it
    ("prints: " <> expected)
    (equal expected (f value))

testParser ∷ ∀ a eff. Eq a ⇒ Show a ⇒ Parser String a → String → a → Spec eff Unit
testParser p value expected =
  it
    ("parses: " <> value)
    (equal (Right expected) (runParser value p))

equal :: forall a eff. Eq a => Show a => a -> a -> Aff eff Unit
equal expected actual =
  when (expected /= actual) do
    fail $
      "\nexpected: " <> show expected <>
      "\ngot:      " <> show actual

testIso ∷ ∀ a eff. Eq a ⇒ Show a ⇒ Parser String a → (a → String) → String → a → Spec eff Unit
testIso p f value expected = do
  testParser p value expected
  testPrinter f value expected

forAll ∷ ∀ eff prop. QC.Testable prop ⇒ QCG.Gen prop → Aff (TestEffects eff) Unit
forAll = quickCheck

quickCheck ∷ ∀ eff prop. QC.Testable prop ⇒ prop → Aff (TestEffects eff) Unit
quickCheck = liftEff <<< QC.quickCheck' 100
