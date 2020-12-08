{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.Tasty.Grade where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..), Sum(..))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.FilePath (takeDirectory)


import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.State as State
import qualified Data.Functor.Compose as Functor
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
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


instance Tasty.IsOption (Maybe TestGroupProps) where
  defaultValue = Nothing
  parseValue _ = Nothing
  optionName = Tagged "testgrouppoints"
  optionHelp = Tagged ""

testGroupPoints :: Int -> Int -> Int -> Tasty.TestTree -> Tasty.TestTree
testGroupPoints plus minus upperBound tree = Tasty.PlusTestOptions (Just points `Tasty.setOption`) tree
  where
    points = TestGroupProps plus minus upperBound

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
jsonRunner :: Tasty.Ingredient
jsonRunner = Tasty.TestReporter optionDescription runner
 where
  optionDescription = [ Tasty.Option (Proxy :: Proxy (Maybe JsonPath)) ]
  runner options testTree = do
    JsonPath path <- Tasty.lookupOption options

    return $ \statusMap ->
      let
        timeToNs :: Tasty.Time -> Integer
        timeToNs time = round $ time * 10 ** 9

        runTest :: (Tasty.IsTest t)
                => Tasty.OptionSet
                -> Tasty.TestName
                -> t
                -> Tasty.Traversal (Functor.Compose (State.StateT IntMap.Key IO) (Const Summary))
        runTest _ testName _ = Tasty.Traversal $ Functor.Compose $ do
          i <- State.get

          testResult <- liftIO $ STM.atomically $ do
            status <- STM.readTVar $
              fromMaybe (error "Attempted to lookup test by index outside bounds") $
                IntMap.lookup i statusMap

            case status of
              Tasty.Done result -> pure result
              -- Otherwise the test has either not been started or is currently
              -- executing
              _ -> STM.retry

          let testCaseAttributes time =
                [ "name" .= testName
                , "time" .= timeToNs time
                ]

              mkSummary :: Aeson.Value -> Summary
              mkSummary contents =
                mempty { jsonRenderer = Endo
                            (contents :)
                        }

              mkSuccess :: Tasty.Time -> Summary
              mkSuccess time = (mkSummary (Aeson.object $ testCaseAttributes time)) { summarySuccesses = Sum 1 }

              mkFailure :: Tasty.Time -> String -> Summary
              mkFailure time reason =
                mkSummary $ Aeson.object $
                        testCaseAttributes time <>
                        ["failure" .= reason ]

          summary <- case testResult of
              -- If the test is done, generate XML for it
              result
                | Tasty.resultSuccessful result -> pure (mkSuccess (Tasty.resultTime result))
                | otherwise ->
                    case resultException result of
                      Just e  -> pure $ (mkFailure (Tasty.resultTime result) (show e)) { summaryErrors = Sum 1 }
                      Nothing ->
                        if resultTimedOut result
                          then pure $ (mkFailure (Tasty.resultTime result) "Timeout") { summaryErrors = Sum 1 }
                          else do
                            desc <- liftIO $ Tasty.formatMessage (Tasty.resultDescription result)
                            pure (mkFailure (Tasty.resultTime result) desc)
                               { summaryFailures = Sum 1 }

          Const summary <$ State.modify (+ 1)

        runGroup ::
          Tasty.OptionSet ->
          Tasty.TestName ->
          Tasty.Traversal (Functor.Compose (State.StateT IntMap.Key IO) (Const Summary)) ->
          Tasty.Traversal (Functor.Compose (State.StateT IntMap.Key IO) (Const Summary))
        runGroup opts groupName children = Tasty.Traversal $ Functor.Compose $ do
          Const soFar <- Functor.getCompose $ Tasty.getTraversal children
          let grouped =
                Aeson.object $
                  [ "name" .= groupName
                  , "tests" .= (getSum . (summaryFailures `mappend` summaryErrors `mappend` summarySuccesses) $ soFar)
                  , "groups" .= appEndo (jsonRenderer soFar) []
                  ]
                  <> case Tasty.lookupOption opts of
                      Nothing -> []
                      Just TestGroupProps {..} ->
                        [ "points" .= pointsPerSuccess
                        , "deductions" .= pointsPerFailure
                        , "maximum" .= maxPointPerGroup
                        ]


          pure $ Const
            soFar { jsonRenderer = Endo (grouped :)
                  }

      in do
        (Const summary, tests) <-
          flip State.runStateT 0 $ Functor.getCompose $ Tasty.getTraversal $
           Tasty.foldTestTree
             Tasty.trivialFold { Tasty.foldSingle = runTest, Tasty.foldGroup = runGroup }
             options
             testTree

        return $ \elapsedTime -> do
          createPathDirIfMissing path
          Aeson.encodeFile path $
            Aeson.object
                [ "errors".= (getSum . summaryErrors $ summary)
                , "failures" .= (getSum . summaryFailures $ summary)
                , "tests" .= tests
                , "time" .= timeToNs elapsedTime
                , "results" .= appEndo (jsonRenderer summary) []
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
