{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests where

import Test.Tasty

import Distribution.Simple.Utils
import Distribution.Verbosity

import Distribution.Compat.Time

import qualified UnitTests.Distribution.Solver.Modular.Builder
import qualified UnitTests.Distribution.Solver.Modular.WeightedPSQ
import qualified UnitTests.Distribution.Solver.Modular.Solver
import qualified UnitTests.Distribution.Solver.Modular.RetryLog
import qualified UnitTests.Distribution.Client.FileMonitor
import qualified UnitTests.Distribution.Client.Glob
import qualified UnitTests.Distribution.Client.GZipUtils
import qualified UnitTests.Distribution.Client.Sandbox
import qualified UnitTests.Distribution.Client.Sandbox.Timestamp
import qualified UnitTests.Distribution.Client.Store
import qualified UnitTests.Distribution.Client.Tar
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.UserConfig
import qualified UnitTests.Distribution.Client.ProjectConfig
import qualified UnitTests.Distribution.Client.JobControl
import qualified UnitTests.Distribution.Client.IndexUtils.Timestamp
import qualified UnitTests.Distribution.Client.InstallPlan

import UnitTests.Options


tests :: Int -> TestTree
tests mtimeChangeCalibrated =
  askOption $ \(OptionMtimeChangeDelay mtimeChangeProvided) ->
  let mtimeChange = if mtimeChangeProvided /= 0
                    then mtimeChangeProvided
                    else mtimeChangeCalibrated
  in
  testGroup "Unit Tests"
  [ testGroup "UnitTests.Distribution.Solver.Modular.Builder"
        UnitTests.Distribution.Solver.Modular.Builder.tests
  , testGroup "UnitTests.Distribution.Solver.Modular.WeightedPSQ"
        UnitTests.Distribution.Solver.Modular.WeightedPSQ.tests
  , testGroup "UnitTests.Distribution.Solver.Modular.Solver"
        UnitTests.Distribution.Solver.Modular.Solver.tests
  , testGroup "UnitTests.Distribution.Solver.Modular.RetryLog"
        UnitTests.Distribution.Solver.Modular.RetryLog.tests
  , testGroup "UnitTests.Distribution.Client.FileMonitor" $
        UnitTests.Distribution.Client.FileMonitor.tests mtimeChange
  , testGroup "UnitTests.Distribution.Client.Glob"
        UnitTests.Distribution.Client.Glob.tests
  , testGroup "Distribution.Client.GZipUtils"
       UnitTests.Distribution.Client.GZipUtils.tests
  , testGroup "Distribution.Client.Sandbox"
       UnitTests.Distribution.Client.Sandbox.tests
  , testGroup "Distribution.Client.Sandbox.Timestamp"
       UnitTests.Distribution.Client.Sandbox.Timestamp.tests
  , testGroup "Distribution.Client.Store"
       UnitTests.Distribution.Client.Store.tests
  , testGroup "Distribution.Client.Tar"
       UnitTests.Distribution.Client.Tar.tests
  , testGroup "Distribution.Client.Targets"
       UnitTests.Distribution.Client.Targets.tests
  , testGroup "UnitTests.Distribution.Client.UserConfig"
       UnitTests.Distribution.Client.UserConfig.tests
  , testGroup "UnitTests.Distribution.Client.ProjectConfig"
       UnitTests.Distribution.Client.ProjectConfig.tests
  , testGroup "UnitTests.Distribution.Client.JobControl"
       UnitTests.Distribution.Client.JobControl.tests
  , testGroup "UnitTests.Distribution.Client.IndexUtils.Timestamp"
       UnitTests.Distribution.Client.IndexUtils.Timestamp.tests
  , testGroup "UnitTests.Distribution.Client.InstallPlan"
       UnitTests.Distribution.Client.InstallPlan.tests
  ]

main :: IO ()
main = do
  (mtimeChange, mtimeChange') <- calibrateMtimeChangeDelay
  let toMillis :: Int -> Double
      toMillis x = fromIntegral x / 1000.0
  notice normal $ "File modification time resolution calibration completed, "
    ++ "maximum delay observed: "
    ++ (show . toMillis $ mtimeChange ) ++ " ms. "
    ++ "Will be using delay of " ++ (show . toMillis $ mtimeChange')
    ++ " for test runs."
  defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         (tests mtimeChange')

