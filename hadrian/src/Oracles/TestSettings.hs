-- | We create a file <root>/test/ghcconfig containing configuration of test
-- | compiler. We need to search this file for required keys and setting
-- | required for testsuite e.g. WORDSIZE, HOSTOS etc.

module Oracles.TestSettings
  ( TestSetting (..), testSetting, testRTSSettings
  , getCompilerPath, getBinaryDirectory, isInTreeCompiler
  ) where

import Base
import Hadrian.Oracles.TextFile
import Oracles.Setting (topDirectory, setting, Setting(..))
import Packages
import Settings.Program (programContext)
import Hadrian.Oracles.Path
import System.Directory (makeAbsolute)

testConfigFile :: Action FilePath
testConfigFile = buildRoot <&> (-/- "test/ghcconfig")

-- | Test settings that are obtained from ghcconfig file.
data TestSetting = TestHostOS
                 | TestWORDSIZE
                 | TestTARGETPLATFORM
                 | TestTargetOS_CPP
                 | TestTargetARCH_CPP
                 | TestGhcStage
                 | TestGhcDebugged
                 | TestGhcWithNativeCodeGen
                 | TestGhcWithInterpreter
                 | TestGhcUnregisterised
                 | TestGhcWithSMP
                 | TestGhcDynamic
                 | TestGhcProfiled
                 | TestAR
                 | TestCLANG
                 | TestLLC
                 | TestTEST_CC
                 | TestTEST_CC_OPTS
                 | TestGhcPackageDbFlag
                 | TestMinGhcVersion711
                 | TestMinGhcVersion801
                 deriving (Show)

-- | Lookup a test setting in @ghcconfig@ file.
-- | To obtain RTS ways supported in @ghcconfig@ file, use 'testRTSSettings'.
testSetting :: TestSetting -> Action String
testSetting key = do
    file <- testConfigFile
    lookupValueOrError file $ case key of
        TestHostOS                -> "HostOS"
        TestWORDSIZE              -> "WORDSIZE"
        TestTARGETPLATFORM        -> "TARGETPLATFORM"
        TestTargetOS_CPP          -> "TargetOS_CPP"
        TestTargetARCH_CPP        -> "TargetARCH_CPP"
        TestGhcStage              -> "GhcStage"
        TestGhcDebugged           -> "GhcDebugged"
        TestGhcWithNativeCodeGen  -> "GhcWithNativeCodeGen"
        TestGhcWithInterpreter    -> "GhcWithInterpreter"
        TestGhcUnregisterised     -> "GhcUnregisterised"
        TestGhcWithSMP            -> "GhcWithSMP"
        TestGhcDynamic            -> "GhcDynamic"
        TestGhcProfiled           -> "GhcProfiled"
        TestAR                    -> "AR"
        TestCLANG                 -> "CLANG"
        TestLLC                   -> "LLC"
        TestTEST_CC               -> "TEST_CC"
        TestTEST_CC_OPTS          -> "TEST_CC_OPTS"
        TestGhcPackageDbFlag      -> "GhcPackageDbFlag"
        TestMinGhcVersion711      -> "MinGhcVersion711"
        TestMinGhcVersion801      -> "MinGhcVersion801"

-- | Get the RTS ways of the test compiler
testRTSSettings :: Action [String]
testRTSSettings = do
    file <- testConfigFile
    words <$> lookupValueOrError file "GhcRTSWays"

absoluteBuildRoot :: Action FilePath
absoluteBuildRoot = (fixAbsolutePathOnWindows  =<< liftIO . makeAbsolute =<< buildRoot)

-- | Directory to look for binaries.
--   We assume that required programs are present in the same binary directory
--   in which ghc is stored and that they have their conventional name.
getBinaryDirectory :: String -> Action FilePath
getBinaryDirectory "stage0" = takeDirectory <$> setting SystemGhc
getBinaryDirectory "stage1" = liftM2 (-/-) absoluteBuildRoot  (pure "stage1-test/bin/")
getBinaryDirectory "stage2" = liftM2 (-/-) topDirectory (stageBinPath Stage1)
getBinaryDirectory "stage3" = liftM2 (-/-) topDirectory (stageBinPath Stage2)
getBinaryDirectory compiler = pure $ takeDirectory compiler

-- | Get the path to the given @--test-compiler@.
getCompilerPath :: String -> Action FilePath
getCompilerPath "stage0" = setting SystemGhc
getCompilerPath "stage1" = liftM2 (-/-) absoluteBuildRoot (pure "stage1-test/bin/ghc")
getCompilerPath "stage2" = liftM2 (-/-) topDirectory (fullPath Stage1 ghc)
getCompilerPath "stage3" = liftM2 (-/-) topDirectory (fullPath Stage2 ghc)
getCompilerPath compiler = pure compiler

isInTreeCompiler :: String -> Bool
isInTreeCompiler c = c `elem` ["stage1","stage2","stage3"]

-- | Get the full path to the given program.
fullPath :: Stage -> Package -> Action FilePath
fullPath stage pkg = programPath =<< programContext stage pkg
