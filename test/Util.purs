module Test.Util where

import Prelude

import Data.Either (Either(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)
import Text.Parsing.Parser (Parser, runParser)

testPrinter ∷ ∀ a. Show a ⇒ (a → String) → String → a → Spec Unit
testPrinter f expected value =
  it
    ("prints: " <> expected)
    (equal expected (f value))

testParser ∷ ∀ a. Eq a ⇒ Show a ⇒ Parser String a → String → a → Spec Unit
testParser p value expected =
  it
    ("parses: " <> value)
    (equal (Right expected) (runParser value p))

equal :: forall a. Eq a => Show a => a -> a -> Aff Unit
equal expected actual =
  when (expected /= actual) do
    fail $
      "\nexpected: " <> show expected <>
      "\ngot:      " <> show actual

testIso ∷ ∀ a. Eq a ⇒ Show a ⇒ Parser String a → (a → String) → String → a → Spec Unit
testIso p f value expected = do
  testParser p value expected
  testPrinter f value expected

forAll ∷ ∀ prop. QC.Testable prop ⇒ QCG.Gen prop → Aff Unit
forAll = quickCheck

quickCheck ∷ ∀ prop. QC.Testable prop ⇒ prop → Aff Unit
quickCheck = liftEffect <<< QC.quickCheck' 100

nes ∷ String → NonEmptyString
nes = unsafePartial NES.unsafeFromString
