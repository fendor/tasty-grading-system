{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.Tasty.Grade where

import Numeric (showFFloat)
import Control.Applicative
import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), Endo(..), Sum(..))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.FilePath (takeDirectory)


import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Data.Functor.Compose as Functor
import qualified Data.Aeson as Aeson
import qualified Data.IntMap as IntMap
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Providers as Tasty
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty

data TestGroupProps = TestGroupProps
  { pointsPerSuccess :: Int
  , pointsPerFailure :: Int
  , maxPointPerGroup :: Int
  }
  deriving (Show, Eq, Ord)


instance Tasty.IsOption TestGroupProps where
  defaultValue = TestGroupProps
    { pointsPerSuccess = 0
    , pointsPerFailure = 0
    , maxPointPerGroup = 0
    }
  parseValue _ = Nothing
  optionName = Tagged "testgrouppoints"
  optionHelp = Tagged ""

testGroupPoints :: Int -> Int -> Int -> Tasty.TestTree -> Tasty.TestTree
testGroupPoints = undefined

-- ----------------------------------------------------------------------------

newtype JsonPath = JsonPath FilePath
  deriving (Typeable)

instance Tasty.IsOption (Maybe JsonPath) where
  defaultValue = Nothing
  parseValue = Just . Just . JsonPath
  optionName = Tagged "grading-json"
  optionHelp = Tagged "A file path to store the test results in JSON, annotated by points"

-- ----------------------------------------------------------------------------

data Summary = Summary { summaryFailures :: Sum Int
                       , summaryErrors :: Sum Int
                       , summarySuccesses :: Sum Int
                       , jsonRenderer :: Endo [Aeson.Value]
                       } deriving (Generic)

data TestReport
  = SingleTest
  | TestGroup

instance Monoid Summary where
  mempty = memptydefault
#if !MIN_VERSION_base(4,11,0)
  mappend = mappenddefault
#else
instance Semigroup Summary where
  (<>) = mappenddefault
#endif

-- ----------------------------------------------------------------------------

{-|

  To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
  passing 'antXMLRunner' as one possible ingredient. This ingredient will run
  tests if you pass the @--xml@ command line option. For example,
  @--xml=junit.xml@ will run all the tests and generate @junit.xml@ as output.

-}
antXMLRunner :: Tasty.Ingredient
antXMLRunner = Tasty.TestReporter optionDescription runner
 where
  optionDescription = [ Tasty.Option (Proxy :: Proxy (Maybe JsonPath)) ]
  runner options testTree = do
    JsonPath path <- Tasty.lookupOption options

    return $ \statusMap ->
      let
        timeDigits = 3
        showTime time = showFFloat (Just timeDigits) time ""

        runTest :: (Tasty.IsTest t)
                => Tasty.OptionSet
                -> Tasty.TestName
                -> t
                -> Tasty.Traversal (Functor.Compose (Reader.ReaderT [String] (State.StateT IntMap.Key IO)) (Const Summary))
        runTest _ testName _ = Tasty.Traversal $ Functor.Compose $ do
          i <- State.get
          groupNames <- Reader.ask

          summary <- liftIO $ STM.atomically $ do
            status <- STM.readTVar $
              fromMaybe (error "Attempted to lookup test by index outside bounds") $
                IntMap.lookup i statusMap

            let testCaseAttributes time = Aeson.object
                  [ ("name", Aeson.string testName)
                  , ("time", Aeson.string $ showTime time)
                  , ("classname", intercalate "." (reverse groupNames))
                  ]

                mkSummary contents =
                  mempty { jsonRenderer = Endo
                             (contents :)
                         }

                mkSuccess time = (mkSummary (testCaseAttributes time)) { summarySuccesses = Sum 1 }

                mkFailure time reason =
                  mkSummary ( testCaseAttributes time
                            , Aeson.object [("failure", reason)]
                            )

            case status of
              -- If the test is done, generate XML for it
              Tasty.Done result
                | Tasty.resultSuccessful result -> pure (mkSuccess (Tasty.resultTime result))
                | otherwise ->
                    case resultException result of
                      Just e  -> pure $ (mkFailure (Tasty.resultTime result) (show e)) { summaryErrors = Sum 1 }
                      Nothing -> pure $
                        if resultTimedOut result
                          then (mkFailure (Tasty.resultTime result) "TimeOut") { summaryErrors = Sum 1 }
                          else (mkFailure (Tasty.resultTime result) (Tasty.resultDescription result))
                               { summaryFailures = Sum 1 }

              -- Otherwise the test has either not been started or is currently
              -- executing
              _ -> STM.retry

          Const summary <$ State.modify (+ 1)

        runGroup groupName children = Tasty.Traversal $ Functor.Compose $ do
          Const soFar <- Reader.local (groupName :) $ Functor.getCompose $ Tasty.getTraversal children

          let grouped =
                Aeson.object
                  [ ("name", groupName)
                  , ("tests", show . getSum . (summaryFailures `mappend` summaryErrors `mappend` summarySuccesses) $ soFar)
                  , ("groups", appEndo (xmlRenderer soFar) [])
                  ]

          pure $ Const
            soFar { jsonRenderer = Endo (grouped :)
                  }

      in do
        (Const summary, tests) <-
          flip State.runStateT 0 $ flip Reader.runReaderT [] $ Functor.getCompose $ Tasty.getTraversal $
           Tasty.foldTestTree
             Tasty.trivialFold { Tasty.foldSingle = runTest, Tasty.foldGroup = runGroup }
             options
             testTree

        return $ \elapsedTime -> do
          createPathDirIfMissing path
          Aeson.encodeFile path $
            Aeson.object
                [ ("errors", (show . getSum . summaryErrors $ summary))
                , ("failures", (show . getSum . summaryFailures $ summary))
                , ("tests", (show tests))
                , ("time", (showTime elapsedTime))
                , ("results", appEndo (xmlRenderer summary) [])
                ]

          return (getSum ((summaryFailures `mappend` summaryErrors) summary) == 0)

  resultException r =
    case Tasty.resultOutcome r of
         Tasty.Failure (Tasty.TestThrewException e) -> Just e
         _ -> Nothing

  resultTimedOut r =
    case Tasty.resultOutcome r of
         Tasty.Failure (Tasty.TestTimedOut _) -> True
         _ -> False

  createPathDirIfMissing path = fmap takeDirectory (canonicalizePath path)
                                >>= createDirectoryIfMissing True
