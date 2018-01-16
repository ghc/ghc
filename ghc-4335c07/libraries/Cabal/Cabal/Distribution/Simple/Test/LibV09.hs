{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.Test.LibV09
       ( runTest
         -- Test stub
       , simpleTestStub
       , stubFilePath, stubMain, stubName, stubWriteLog
       , writeSimpleTestStub
       ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Types.UnqualComponentName

import Distribution.Compat.CreatePipe
import Distribution.Compat.Environment
import Distribution.Compat.Internal.TempFile
import Distribution.ModuleName
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

import qualified Control.Exception as CE
import System.Directory
    ( createDirectoryIfMissing, canonicalizePath
    , doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive, removeFile
    , setCurrentDirectory )
import System.Exit ( exitSuccess, exitWith, ExitCode(..) )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose, hGetContents, hPutStr )
import System.Process (StdStream(..), waitForProcess)

runTest :: PD.PackageDescription
        -> LBI.LocalBuildInfo
        -> LBI.ComponentLocalBuildInfo
        -> TestFlags
        -> PD.TestSuite
        -> IO TestSuiteLog
runTest pkg_descr lbi clbi flags suite = do
    let isCoverageEnabled = LBI.testCoverage lbi
        way = guessWay lbi

    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment

    let cmd = LBI.buildDir lbi </> stubName suite
                  </> stubName suite <.> exeExtension
    -- Check that the test executable exists.
    exists <- doesFileExist cmd
    unless exists $
      die' verbosity $ "Error: Could not find test program \"" ++ cmd
                    ++ "\". Did you build the package first?"

    -- Remove old .tix files if appropriate.
    unless (fromFlag $ testKeepTix flags) $ do
        let tDir = tixDir distPref way testName'
        exists' <- doesDirectoryExist tDir
        when exists' $ removeDirectoryRecursive tDir

    -- Create directory for HPC files.
    createDirectoryIfMissing True $ tixDir distPref way testName'

    -- Write summary notices indicating start of test suite
    notice verbosity $ summarizeSuiteStart testName'

    suiteLog <- CE.bracket openCabalTemp deleteIfExists $ \tempLog -> do

        (rOut, wOut) <- createPipe

        -- Run test executable
        (Just wIn, _, _, process) <- do
                let opts = map (testOption pkg_descr lbi suite) $ testOptions flags
                    dataDirPath = pwd </> PD.dataDir pkg_descr
                    tixFile = pwd </> tixFilePath distPref way testName'
                    pkgPathEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                               : existingEnv
                    shellEnv = [("HPCTIXFILE", tixFile) | isCoverageEnabled]
                             ++ pkgPathEnv
                -- Add (DY)LD_LIBRARY_PATH if needed
                shellEnv' <-
                  if LBI.withDynExe lbi
                  then do
                    let (Platform _ os) = LBI.hostPlatform lbi
                    paths <- LBI.depLibraryPaths True False lbi clbi
                    cpath <- canonicalizePath $ LBI.componentBuildDir lbi clbi
                    return (addLibraryPath os (cpath : paths) shellEnv)
                  else return shellEnv
                createProcessWithEnv verbosity cmd opts Nothing (Just shellEnv')
                                     -- these handles are closed automatically
                                     CreatePipe (UseHandle wOut) (UseHandle wOut)

        hPutStr wIn $ show (tempLog, PD.testName suite)
        hClose wIn

        -- Append contents of temporary log file to the final human-
        -- readable log file
        logText <- hGetContents rOut
        -- Force the IO manager to drain the test output pipe
        length logText `seq` return ()

        exitcode <- waitForProcess process
        unless (exitcode == ExitSuccess) $ do
            debug verbosity $ cmd ++ " returned " ++ show exitcode

        -- Generate final log file name
        let finalLogName l = testLogDir
                             </> testSuiteLogPath
                                 (fromFlag $ testHumanLog flags) pkg_descr lbi
                                 (unUnqualComponentName $ testSuiteName l) (testLogs l)
        -- Generate TestSuiteLog from executable exit code and a machine-
        -- readable test log
        suiteLog <- fmap ((\l -> l { logFile = finalLogName l }) . read) -- TODO: eradicateNoParse
                    $ readFile tempLog

        -- Write summary notice to log file indicating start of test suite
        appendFile (logFile suiteLog) $ summarizeSuiteStart testName'

        appendFile (logFile suiteLog) logText

        -- Write end-of-suite summary notice to log file
        appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

        -- Show the contents of the human-readable log file on the terminal
        -- if there is a failure and/or detailed output is requested
        let details = fromFlag $ testShowDetails flags
            whenPrinting = when $ (details > Never)
                && (not (suitePassed $ testLogs suiteLog) || details == Always)
                && verbosity >= normal
        whenPrinting $ putStr $ unlines $ lines logText

        return suiteLog

    -- Write summary notice to terminal indicating end of test suite
    notice verbosity $ summarizeSuiteFinish suiteLog

    when isCoverageEnabled $
        markupTest verbosity lbi distPref (display $ PD.package pkg_descr) suite

    return suiteLog
  where
    testName' = unUnqualComponentName $ PD.testName suite
    
    deleteIfExists file = do
        exists <- doesFileExist file
        when exists $ removeFile file

    testLogDir = distPref </> "test"
    openCabalTemp = do
        (f, h) <- openTempFile testLogDir $ "cabal-test-" <.> "log"
        hClose h >> return f

    distPref = fromFlag $ testDistPref flags
    verbosity = fromFlag $ testVerbosity flags

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

-- Test stub ----------

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: PD.TestSuite -> FilePath
stubName t = unUnqualComponentName (PD.testName t) ++ "Stub"

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: PD.TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

-- | Write the source file for a library 'TestSuite' stub executable.
writeSimpleTestStub :: PD.TestSuite -- ^ library 'TestSuite' for which a stub
                                    -- is being created
                    -> FilePath     -- ^ path to directory where stub source
                                    -- should be located
                    -> NoCallStackIO ()
writeSimpleTestStub t dir = do
    createDirectoryIfMissing True dir
    let filename = dir </> stubFilePath t
        PD.TestSuiteLibV09 _ m = PD.testInterface t
    writeFile filename $ simpleTestStub m

-- | Source code for library test suite stub executable
simpleTestStub :: ModuleName -> String
simpleTestStub m = unlines
    [ "module Main ( main ) where"
    , "import Distribution.Simple.Test.LibV09 ( stubMain )"
    , "import " ++ show (disp m) ++ " ( tests )"
    , "main :: IO ()"
    , "main = stubMain tests"
    ]

-- | Main function for test stubs. Once, it was written directly into the stub,
-- but minimizing the amount of code actually in the stub maximizes the number
-- of detectable errors when Cabal is compiled.
stubMain :: IO [Test] -> IO ()
stubMain tests = do
    (f, n) <- fmap read getContents -- TODO: eradicateNoParse
    dir <- getCurrentDirectory
    results <- (tests >>= stubRunTests) `CE.catch` errHandler
    setCurrentDirectory dir
    stubWriteLog f n results
  where
    errHandler :: CE.SomeException -> NoCallStackIO TestLogs
    errHandler e = case CE.fromException e of
        Just CE.UserInterrupt -> CE.throwIO e
        _ -> return $ TestLog { testName = "Cabal test suite exception",
                                testOptionsReturned = [],
                                testResult = Error $ show e }

-- | The test runner used in library "TestSuite" stub executables.  Runs a list
-- of 'Test's.  An executable calling this function is meant to be invoked as
-- the child of a Cabal process during @.\/setup test@.  A 'TestSuiteLog',
-- provided by Cabal, is read from the standard input; it supplies the name of
-- the test suite and the location of the machine-readable test suite log file.
-- Human-readable log information is written to the standard output for capture
-- by the calling Cabal process.
stubRunTests :: [Test] -> IO TestLogs
stubRunTests tests = do
    logs <- traverse stubRunTests' tests
    return $ GroupLogs "Default" logs
  where
    stubRunTests' (Test t) = do
        l <- run t >>= finish
        summarizeTest normal Always l
        return l
      where
        finish (Finished result) =
            return TestLog
                { testName = name t
                , testOptionsReturned = defaultOptions t
                , testResult = result
                }
        finish (Progress _ next) = next >>= finish
    stubRunTests' g@(Group {}) = do
        logs <- traverse stubRunTests' $ groupTests g
        return $ GroupLogs (groupName g) logs
    stubRunTests' (ExtraOptions _ t) = stubRunTests' t
    maybeDefaultOption opt =
        maybe Nothing (\d -> Just (optionName opt, d)) $ optionDefault opt
    defaultOptions testInst = mapMaybe maybeDefaultOption $ options testInst

-- | From a test stub, write the 'TestSuiteLog' to temporary file for the calling
-- Cabal process to read.
stubWriteLog :: FilePath -> UnqualComponentName -> TestLogs -> NoCallStackIO ()
stubWriteLog f n logs = do
    let testLog = TestSuiteLog { testSuiteName = n, testLogs = logs, logFile = f }
    writeFile (logFile testLog) $ show testLog
    when (suiteError logs) $ exitWith $ ExitFailure 2
    when (suiteFailed logs) $ exitWith $ ExitFailure 1
    exitSuccess
