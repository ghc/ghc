-- | We create a file <root>/test/ghcconfig containing configuration of test
-- | compiler. We need to search this file for required keys and setting
-- | required for testsuite e.g. WORDSIZE, HOSTOS etc.

module Oracles.TestSettings
  ( TestSetting (..), testSetting, testRTSSettings
  , getCompilerPath, getBinaryDirectory
  ) where

import Base
import Hadrian.Oracles.TextFile
import Oracles.Setting (topDirectory, setting, Setting(..))
import Settings (programContext)
import Packages

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
                 | TestGhcDynamicByDefault
                 | TestGhcDynamic
                 | TestGhcProfiled
                 | TestAR
                 | TestCLANG
                 | TestLLC
                 | TestTEST_CC
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
        TestGhcDynamicByDefault   -> "GhcDynamicByDefault"
        TestGhcDynamic            -> "GhcDynamic"
        TestGhcProfiled           -> "GhcProfiled"
        TestAR                    -> "AR"
        TestCLANG                 -> "CLANG"
        TestLLC                   -> "LLC"
        TestTEST_CC               -> "TEST_CC"
        TestGhcPackageDbFlag      -> "GhcPackageDbFlag"
        TestMinGhcVersion711      -> "MinGhcVersion711"
        TestMinGhcVersion801      -> "MinGhcVersion801"

-- | Get the RTS ways of the test compiler
testRTSSettings :: Action [String]
testRTSSettings = do
    file <- testConfigFile
    words <$> lookupValueOrError file "GhcRTSWays"

-- | Directory to look for Binaries
-- | We assume that required programs are present in the same binary directory
-- | in which ghc is stored and that they have their conventional name.
-- | QUESTION : packages can be named different from their conventional names.
-- | For example, ghc-pkg can be named as ghc-pkg-version. In such cases, it will
-- | be impossible to search the binary. Only possible way will be to take user
-- | inputs for these directory also. boilerplate soes not account for this
-- | problem, but simply returns an error. How should we handle such cases?
getBinaryDirectory :: String -> Action FilePath
getBinaryDirectory "stage0" = takeDirectory <$> setting SystemGhc
getBinaryDirectory "stage1" = liftM2 (-/-) topDirectory (stageBinPath Stage0)
getBinaryDirectory "stage2" = liftM2 (-/-) topDirectory (stageBinPath Stage1)
getBinaryDirectory compiler = pure $ parentPath compiler

-- | Set Test Compiler.
getCompilerPath :: String -> Action FilePath
getCompilerPath "stage0" = setting SystemGhc
getCompilerPath "stage1" = liftM2 (-/-) topDirectory (fullPath Stage0 ghc)
getCompilerPath "stage2" = liftM2 (-/-) topDirectory (fullPath Stage1 ghc)
getCompilerPath compiler = pure compiler

-- | Returns parent path of test compiler
-- | TODO: Is there a simpler way to find parent directory?
parentPath :: String -> String
parentPath path = intercalate "/" $ init $ splitOn "/" path

-- | TODO: Move to Hadrian utilities.
fullPath :: Stage -> Package -> Action FilePath
fullPath stage pkg = programPath =<< programContext stage pkg
