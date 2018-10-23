-- | We create a file <root>/test/ghcconfig containing configuration of test
-- | compiler. We need to search this file for required keys and setting
-- | required for testsuite e.g. WORDSIZE, HOSTOS etc.

module Oracles.TestSettings (TestSetting (..), testSetting, testRTSSettings) where

import Base
import Hadrian.Oracles.TextFile

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
