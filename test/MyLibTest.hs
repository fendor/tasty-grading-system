module Main (main) where

import Control.Exception
import Test.Tasty
import Test.Tasty.Grade
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [composeReporters consoleTestReporter jsonRunner] spec

spec :: TestTree
spec =
  testGroup
    "spec"
    [ testGroupPoints 5 0 9 unitTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $
        [1, 2, 3] `compare` [1, 2] @?= GT,
      -- the following test does not hold
      testCase "List comparison (same length)" $
        [1, 2, 3] `compare` [1, 2, 2] @?= LT,
      -- the following test does not hold
      testCase "throw error" $
        error "Test",
      -- the following test does not hold
      testCase "timeout" $
        let f x = f x in f 0,
      -- the following test does not hold
      testCase "exception" $
        throwIO $ Overflow
    ]
