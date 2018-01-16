{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Thomas Tuegel 2010
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into testing a built package. It performs the
-- \"@.\/setup test@\" action. It runs test suites designated in the package
-- description and reports on the results.

module Distribution.Simple.Test
    ( test
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.UnqualComponentName
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Compiler
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.Types.LocalBuildInfo as LBI
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import qualified Distribution.Simple.Test.ExeV10 as ExeV10
import qualified Distribution.Simple.Test.LibV09 as LibV09
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils
import Distribution.TestSuite
import Distribution.Text

import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getDirectoryContents
    , removeFile )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath ( (</>) )

-- |Perform the \"@.\/setup test@\" action.
test :: Args                    -- ^positional command-line arguments
     -> PD.PackageDescription   -- ^information from the .cabal file
     -> LBI.LocalBuildInfo      -- ^information from the configure step
     -> TestFlags               -- ^flags sent to test
     -> IO ()
test args pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        machineTemplate = fromFlag $ testMachineLog flags
        distPref = fromFlag $ testDistPref flags
        testLogDir = distPref </> "test"
        testNames = args
        pkgTests = PD.testSuites pkg_descr
        enabledTests = LBI.enabledTestLBIs pkg_descr lbi

        doTest :: ((PD.TestSuite, LBI.ComponentLocalBuildInfo),
                    Maybe TestSuiteLog) -> IO TestSuiteLog
        doTest ((suite, clbi), _) =
            case PD.testInterface suite of
              PD.TestSuiteExeV10 _ _ ->
                  ExeV10.runTest pkg_descr lbi clbi flags suite

              PD.TestSuiteLibV09 _ _ ->
                  LibV09.runTest pkg_descr lbi clbi flags suite

              _ -> return TestSuiteLog
                  { testSuiteName = PD.testName suite
                  , testLogs = TestLog
                      { testName = unUnqualComponentName $ PD.testName suite
                      , testOptionsReturned = []
                      , testResult =
                          Error $ "No support for running test suite type: "
                                  ++ show (disp $ PD.testType suite)
                      }
                  , logFile = ""
                  }

    unless (PD.hasTests pkg_descr) $ do
        notice verbosity "Package has no test suites."
        exitSuccess

    when (PD.hasTests pkg_descr && null enabledTests) $
        die' verbosity $
              "No test suites enabled. Did you remember to configure with "
           ++ "\'--enable-tests\'?"

    testsToRun <- case testNames of
            [] -> return $ zip enabledTests $ repeat Nothing
            names -> for names $ \tName ->
                let testMap = zip enabledNames enabledTests
                    enabledNames = map (PD.testName . fst) enabledTests
                    allNames = map PD.testName pkgTests
                    tCompName = mkUnqualComponentName tName
                in case lookup tCompName testMap of
                    Just t -> return (t, Nothing)
                    _ | tCompName `elem` allNames ->
                          die' verbosity $ "Package configured with test suite "
                                ++ tName ++ " disabled."
                      | otherwise -> die' verbosity $ "no such test: " ++ tName

    createDirectoryIfMissing True testLogDir

    -- Delete ordinary files from test log directory.
    getDirectoryContents testLogDir
        >>= filterM doesFileExist . map (testLogDir </>)
        >>= traverse_ removeFile

    let totalSuites = length testsToRun
    notice verbosity $ "Running " ++ show totalSuites ++ " test suites..."
    suites <- traverse doTest testsToRun
    let packageLog = (localPackageLog pkg_descr lbi) { testSuites = suites }
        packageLogFile = (</>) testLogDir
            $ packageLogPath machineTemplate pkg_descr lbi
    allOk <- summarizePackage verbosity packageLog
    writeFile packageLogFile $ show packageLog

    when (LBI.testCoverage lbi) $
        markupPackage verbosity lbi distPref (display $ PD.package pkg_descr) $
            map (fst . fst) testsToRun

    unless allOk exitFailure

packageLogPath :: PathTemplate
               -> PD.PackageDescription
               -> LBI.LocalBuildInfo
               -> FilePath
packageLogPath template pkg_descr lbi =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (LBI.localUnitId lbi)
                (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi)
