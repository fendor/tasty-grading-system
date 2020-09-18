module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Grade

main :: IO ()
main = defaultMainWithIngredients [jsonRunner] tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroupPoints 5 0 9 $ testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
