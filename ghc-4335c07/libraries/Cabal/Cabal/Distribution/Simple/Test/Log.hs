{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.Test.Log
       ( PackageLog(..)
       , TestLogs(..)
       , TestSuiteLog(..)
       , countTestResults
       , localPackageLog
       , summarizePackage
       , summarizeSuiteFinish, summarizeSuiteStart
       , summarizeTest
       , suiteError, suiteFailed, suitePassed
       , testSuiteLogPath
       ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Types.UnqualComponentName
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Compiler
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.TestSuite
import Distribution.Verbosity
import Distribution.Text

-- | Logs all test results for a package, broken down first by test suite and
-- then by test case.
data PackageLog = PackageLog
    { package :: PackageId
    , compiler :: CompilerId
    , platform :: Platform
    , testSuites :: [TestSuiteLog]
    }
    deriving (Read, Show, Eq)

-- | A 'PackageLog' with package and platform information specified.
localPackageLog :: PD.PackageDescription -> LBI.LocalBuildInfo -> PackageLog
localPackageLog pkg_descr lbi = PackageLog
    { package = PD.package pkg_descr
    , compiler = compilerId $ LBI.compiler lbi
    , platform = LBI.hostPlatform lbi
    , testSuites = []
    }

-- | Logs test suite results, itemized by test case.
data TestSuiteLog = TestSuiteLog
    { testSuiteName :: UnqualComponentName
    , testLogs :: TestLogs
    , logFile :: FilePath    -- path to human-readable log file
    }
    deriving (Read, Show, Eq)

data TestLogs
    = TestLog
        { testName              :: String
        , testOptionsReturned   :: Options
        , testResult            :: Result
        }
    | GroupLogs String [TestLogs]
    deriving (Read, Show, Eq)

-- | Count the number of pass, fail, and error test results in a 'TestLogs'
-- tree.
countTestResults :: TestLogs
                 -> (Int, Int, Int) -- ^ Passes, fails, and errors,
                                    -- respectively.
countTestResults = go (0, 0, 0)
  where
    go (p, f, e) (TestLog { testResult = r }) =
        case r of
            Pass -> (p + 1, f, e)
            Fail _ -> (p, f + 1, e)
            Error _ -> (p, f, e + 1)
    go (p, f, e) (GroupLogs _ ts) = foldl go (p, f, e) ts

-- | From a 'TestSuiteLog', determine if the test suite passed.
suitePassed :: TestLogs -> Bool
suitePassed l =
    case countTestResults l of
        (_, 0, 0) -> True
        _ -> False

-- | From a 'TestSuiteLog', determine if the test suite failed.
suiteFailed :: TestLogs -> Bool
suiteFailed l =
    case countTestResults l of
        (_, 0, _) -> False
        _ -> True

-- | From a 'TestSuiteLog', determine if the test suite encountered errors.
suiteError :: TestLogs -> Bool
suiteError l =
    case countTestResults l of
        (_, _, 0) -> False
        _ -> True

resultString :: TestLogs -> String
resultString l | suiteError l = "error"
               | suiteFailed l = "fail"
               | otherwise = "pass"

testSuiteLogPath :: PathTemplate
                 -> PD.PackageDescription
                 -> LBI.LocalBuildInfo
                 -> String -- ^ test suite name
                 -> TestLogs -- ^ test suite results
                 -> FilePath
testSuiteLogPath template pkg_descr lbi test_name result =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (LBI.localUnitId lbi)
                (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi)
                ++  [ (TestSuiteNameVar, toPathTemplate test_name)
                    , (TestSuiteResultVar, toPathTemplate $ resultString result)
                    ]

-- | Print a summary to the console after all test suites have been run
-- indicating the number of successful test suites and cases.  Returns 'True' if
-- all test suites passed and 'False' otherwise.
summarizePackage :: Verbosity -> PackageLog -> IO Bool
summarizePackage verbosity packageLog = do
    let counts = map (countTestResults . testLogs) $ testSuites packageLog
        (passed, failed, errors) = foldl1 addTriple counts
        totalCases = passed + failed + errors
        passedSuites = length
                       $ filter (suitePassed . testLogs)
                       $ testSuites packageLog
        totalSuites = length $ testSuites packageLog
    notice verbosity $ show passedSuites ++ " of " ++ show totalSuites
        ++ " test suites (" ++ show passed ++ " of "
        ++ show totalCases ++ " test cases) passed."
    return $! passedSuites == totalSuites
  where
    addTriple (p1, f1, e1) (p2, f2, e2) = (p1 + p2, f1 + f2, e1 + e2)

-- | Print a summary of a single test case's result to the console, supressing
-- output for certain verbosity or test filter levels.
summarizeTest :: Verbosity -> TestShowDetails -> TestLogs -> IO ()
summarizeTest _ _ (GroupLogs {}) = return ()
summarizeTest verbosity details t =
    when shouldPrint $ notice verbosity $ "Test case " ++ testName t
        ++ ": " ++ show (testResult t)
    where shouldPrint = (details > Never) && (notPassed || details == Always)
          notPassed = testResult t /= Pass

-- | Print a summary of the test suite's results on the console, suppressing
-- output for certain verbosity or test filter levels.
summarizeSuiteFinish :: TestSuiteLog -> String
summarizeSuiteFinish testLog = unlines
    [ "Test suite " ++ display (testSuiteName testLog) ++ ": " ++ resStr
    , "Test suite logged to: " ++ logFile testLog
    ]
    where resStr = map toUpper (resultString $ testLogs testLog)

summarizeSuiteStart :: String -> String
summarizeSuiteStart n = "Test suite " ++ n ++ ": RUNNING...\n"
