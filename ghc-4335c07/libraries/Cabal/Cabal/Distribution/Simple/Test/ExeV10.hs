{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.Test.ExeV10
       ( runTest
       ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.UnqualComponentName
import Distribution.Compat.CreatePipe
import Distribution.Compat.Environment
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build.PathsModule
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.Types.LocalBuildInfo as LBI
import Distribution.Simple.Setup
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils
import Distribution.System
import Distribution.TestSuite
import Distribution.Text
import Distribution.Verbosity

import Control.Concurrent (forkIO)
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hGetContents, stdout, stderr )

runTest :: PD.PackageDescription
        -> LBI.LocalBuildInfo
        -> LBI.ComponentLocalBuildInfo
        -> TestFlags
        -> PD.TestSuite
        -> IO TestSuiteLog
runTest pkg_descr lbi clbi flags suite = do
    let isCoverageEnabled = LBI.testCoverage lbi
        way = guessWay lbi
        tixDir_ = tixDir distPref way testName'

    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment

    let cmd = LBI.buildDir lbi </> testName'
                  </> testName' <.> exeExtension
    -- Check that the test executable exists.
    exists <- doesFileExist cmd
    unless exists $ die' verbosity $ "Error: Could not find test program \"" ++ cmd
                          ++ "\". Did you build the package first?"

    -- Remove old .tix files if appropriate.
    unless (fromFlag $ testKeepTix flags) $ do
        exists' <- doesDirectoryExist tixDir_
        when exists' $ removeDirectoryRecursive tixDir_

    -- Create directory for HPC files.
    createDirectoryIfMissing True tixDir_

    -- Write summary notices indicating start of test suite
    notice verbosity $ summarizeSuiteStart $ testName'

    (wOut, wErr, logText) <- case details of
        Direct -> return (stdout, stderr, "")
        _ -> do
            (rOut, wOut) <- createPipe

            -- Read test executable's output lazily (returns immediately)
            logText <- hGetContents rOut
            -- Force the IO manager to drain the test output pipe
            void $ forkIO $ length logText `seq` return ()

            -- '--show-details=streaming': print the log output in another thread
            when (details == Streaming) $ void $ forkIO $ putStr logText

            return (wOut, wOut, logText)

    -- Run the test executable
    let opts = map (testOption pkg_descr lbi suite)
                   (testOptions flags)
        dataDirPath = pwd </> PD.dataDir pkg_descr
        tixFile = pwd </> tixFilePath distPref way (testName')
        pkgPathEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                   : existingEnv
        shellEnv = [("HPCTIXFILE", tixFile) | isCoverageEnabled] ++ pkgPathEnv

    -- Add (DY)LD_LIBRARY_PATH if needed
    shellEnv' <- if LBI.withDynExe lbi
                    then do let (Platform _ os) = LBI.hostPlatform lbi
                            paths <- LBI.depLibraryPaths True False lbi clbi
                            return (addLibraryPath os paths shellEnv)
                    else return shellEnv

    exit <- rawSystemIOWithEnv verbosity cmd opts Nothing (Just shellEnv')
                               -- these handles are automatically closed
                               Nothing (Just wOut) (Just wErr)

    -- Generate TestSuiteLog from executable exit code and a machine-
    -- readable test log.
    let suiteLog = buildLog exit

    -- Write summary notice to log file indicating start of test suite
    appendFile (logFile suiteLog) $ summarizeSuiteStart $ testName'

    -- Append contents of temporary log file to the final human-
    -- readable log file
    appendFile (logFile suiteLog) logText

    -- Write end-of-suite summary notice to log file
    appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

    -- Show the contents of the human-readable log file on the terminal
    -- if there is a failure and/or detailed output is requested
    let whenPrinting = when $
            ( details == Always ||
              details == Failures && not (suitePassed $ testLogs suiteLog))
            -- verbosity overrides show-details
            && verbosity >= normal
    whenPrinting $ putStr $ unlines $ lines logText

    -- Write summary notice to terminal indicating end of test suite
    notice verbosity $ summarizeSuiteFinish suiteLog

    when isCoverageEnabled $
        markupTest verbosity lbi distPref (display $ PD.package pkg_descr) suite

    return suiteLog
  where
    testName' = unUnqualComponentName $ PD.testName suite

    distPref = fromFlag $ testDistPref flags
    verbosity = fromFlag $ testVerbosity flags
    details = fromFlag $ testShowDetails flags
    testLogDir = distPref </> "test"

    buildLog exit =
        let r = case exit of
                    ExitSuccess -> Pass
                    ExitFailure c -> Fail $ "exit code: " ++ show c
            --n = unUnqualComponentName $ PD.testName suite
            l = TestLog
                { testName = testName'
                , testOptionsReturned = []
                , testResult = r
                }
        in TestSuiteLog
                { testSuiteName = PD.testName suite
                , testLogs = l
                , logFile =
                    testLogDir
                    </> testSuiteLogPath (fromFlag $ testHumanLog flags)
                                         pkg_descr lbi testName' l
                }

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result isn't
-- necessarily a path.
testOption :: PD.PackageDescription
           -> LBI.LocalBuildInfo
           -> PD.TestSuite
           -> PathTemplate
           -> String
testOption pkg_descr lbi suite template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (LBI.localUnitId lbi)
          (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ unUnqualComponentName $ PD.testName suite)]
