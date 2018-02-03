module Test.Util where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(..))
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.Unit (Test, TestSuite, failure, success, test)
import Text.Parsing.StringParser (Parser, runParser)

type TestEffects eff = (console ∷ CONSOLE, random ∷ RANDOM, exception ∷ EXCEPTION | eff)

testPrinter ∷ ∀ a b. Show b ⇒ (b → String) → String → b → TestSuite a
testPrinter f expected uri =
  test
    ("prints: " <> expected)
    (equal expected (f uri))

testParser ∷ ∀ a b. Eq b ⇒ Show b ⇒ Parser b → String → b → TestSuite a
testParser p uri expected =
  test
    ("parses: " <> uri)
    (equal (Right expected) (runParser p uri))

equal :: forall a e. Eq a => Show a => a -> a -> Test e
equal expected actual =
  if expected == actual
    then success
    else
      failure $
        "\nexpected: " <> show expected <>
        "\ngot:      " <> show actual

testIso ∷ ∀ a b. Eq b ⇒ Show b ⇒ Parser b → (b → String) → String → b → TestSuite a
testIso p f uri expected = do
  testParser p uri expected
  testPrinter f uri expected

forAll ∷ ∀ eff prop. QC.Testable prop ⇒ QCG.Gen prop → Test (TestEffects eff)
forAll = quickCheck

quickCheck ∷ ∀ eff prop. QC.Testable prop ⇒ prop → Test (TestEffects eff)
quickCheck = liftEff <<< QC.quickCheck' 100
