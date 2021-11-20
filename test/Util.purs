module Test.Util where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.Spec (Spec, it, fail)
import Text.Parsing.Parser (Parser, runParser)

testPrinter :: forall a. Show a => (a -> String) -> String -> a -> Spec Unit
testPrinter f expected value =
  it
    ("prints: " <> expected)
    (equal expected (f value))

testParser :: forall a. Eq a => Show a => Parser String a -> String -> a -> Spec Unit
testParser p value expected =
  it
    ("parses: " <> value)
    (equal (Right expected) (runParser value p))

equal :: forall a. Eq a => Show a => a -> a -> Spec Unit
equal expected actual =
  when (expected /= actual) do
    fail $
      "\nexpected: "
        <> show expected
        <> "\ngot:      "
        <> show actual

testIso :: forall a. Eq a => Show a => Parser String a -> (a -> String) -> String -> a -> Spec Unit
testIso p f value expected = do
  testParser p value expected
  testPrinter f value expected

forAll :: forall prop. QC.Testable prop => QCG.Gen prop -> Spec Unit
forAll = quickCheck

quickCheck :: forall prop. QC.Testable prop => prop -> Spec Unit
quickCheck = liftEffect <<< QC.quickCheck' 100
