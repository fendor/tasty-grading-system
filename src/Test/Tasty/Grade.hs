module Test.Tasty.Grade where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Data.Tagged

data TestGroupProps = TestGroupProps
  { pointsPerSuccess :: Int
  , pointsPerFailure :: Int
  , maxPointPerGroup :: Int
  }
  deriving (Show, Eq, Ord)


instance IsOption TestGroupProps where
  defaultValue = TestGroupProps
    { pointsPerSuccess = 0
    , pointsPerFailure = 0
    , maxPointPerGroup = 0
    }
  parseValue _ = Nothing
  optionName = Tagged "testgrouppoints"
  optionHelp = Tagged ""

testGroupPoints :: Int -> Int -> Int -> TestTree -> TestTree
testGroupPoints = undefined
