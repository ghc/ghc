-- | We create a file <root>/test/ghcconfig containing configuration of test
-- | compiler. We need to search this file for required keys and setting
-- | required for testsuite e.g. WORDSIZE, HOSTOS etc.

module Oracles.TestSettings
  ( TestSetting (..), getTestSetting, getBooleanSetting, testRTSSettings
  , getCompilerPath, getBinaryDirectory, isInTreeCompiler
  , stageOfTestCompiler, getTestExePath, getTestCross
  ) where

import Base
import Hadrian.Oracles.TextFile
import Oracles.Setting (topDirectory, setting, ProjectSetting(..), crossStage)
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
                 | TestTargetOS
                 | TestTargetARCH
                 | TestRTSWay
                 | TestGhcStage
                 | TestGhcDebugAssertions
                 | TestGhcWithNativeCodeGen
                 | TestGhcWithInterpreter
                 | TestGhcWithRtsLinker
                 | TestGhcUnregisterised
                 | TestGhcTablesNextToCode
                 | TestGhcWithSMP
                 | TestGhcDynamic
                 | TestGhcProfiled
                 | TestAR
                 | TestLLC
                 | TestTEST_CC
                 | TestTEST_CC_OPTS
                 | TestLeadingUnderscore
                 | TestGhcPackageDb
                 | TestGhcLibDir
                 | TestCrossCompiling
                 deriving (Show)

-- | Lookup a test setting in @ghcconfig@ file.
-- | To obtain RTS ways supported in @ghcconfig@ file, use 'testRTSSettings'.
getTestSetting :: TestSetting -> Action String
getTestSetting key = do
    file <- testConfigFile
    lookupValueOrError Nothing file $ case key of
        TestHostOS                -> "HostOS"
        TestWORDSIZE              -> "WORDSIZE"
        TestTARGETPLATFORM        -> "TARGETPLATFORM"
        TestTargetOS              -> "TargetOS"
        TestTargetARCH            -> "TargetARCH"
        TestRTSWay                -> "RTSWay"
        TestGhcStage              -> "GhcStage"
        TestGhcDebugAssertions    -> "GhcDebugAssertions"
        TestGhcWithNativeCodeGen  -> "GhcWithNativeCodeGen"
        TestGhcWithInterpreter    -> "GhcWithInterpreter"
        TestGhcWithRtsLinker      -> "GhcWithRtsLinker"
        TestGhcUnregisterised     -> "GhcUnregisterised"
        TestGhcTablesNextToCode   -> "GhcTablesNextToCode"
        TestGhcWithSMP            -> "GhcWithSMP"
        TestGhcDynamic            -> "GhcDynamic"
        TestGhcProfiled           -> "GhcProfiled"
        TestAR                    -> "AR"
        TestLLC                   -> "LLC"
        TestTEST_CC               -> "TEST_CC"
        TestTEST_CC_OPTS          -> "TEST_CC_OPTS"
        TestLeadingUnderscore     -> "LeadingUnderscore"
        TestGhcPackageDb          -> "GhcGlobalPackageDb"
        TestGhcLibDir             -> "GhcLibdir"
        TestCrossCompiling        -> "CrossCompiling"

-- | Parse the value of a Boolean test setting or report an error.
getBooleanSetting :: TestSetting -> Action Bool
getBooleanSetting key = fromMaybe (error msg) <$> parseYesNo <$> getTestSetting key
  where
    msg = "Cannot parse test setting " ++ quote (show key)



-- | Get the RTS ways of the test compiler
testRTSSettings :: Action [String]
testRTSSettings = do
    file <- testConfigFile
    words <$> lookupValueOrError Nothing file "GhcRTSWays"

-- | Directory to look for binaries.
--   We assume that required programs are present in the same binary directory
--   in which ghc is stored and that they have their conventional name.
getBinaryDirectory :: String -> Action FilePath
getBinaryDirectory "stage0" = takeDirectory <$> setting SystemGhc
getBinaryDirectory "stage1" = liftM2 (-/-) topDirectory (stageBinPath stage0InTree)
getBinaryDirectory "stage2" = liftM2 (-/-) topDirectory (stageBinPath Stage1)
getBinaryDirectory "stage3" = liftM2 (-/-) topDirectory (stageBinPath Stage2)
getBinaryDirectory "stage-cabal" = do
  top <- topDirectory
  root <- buildRoot
  pure (top -/- root -/- "stage-cabal" -/- "bin")
getBinaryDirectory compiler = pure $ takeDirectory compiler

-- | Get the path to the given @--test-compiler@.
getCompilerPath :: String -> Action FilePath
getCompilerPath "stage0" = setting SystemGhc
getCompilerPath "stage1" = liftM2 (-/-) topDirectory (fullPath stage0InTree ghc)
getCompilerPath "stage2" = liftM2 (-/-) topDirectory (fullPath Stage1 ghc)
getCompilerPath "stage3" = liftM2 (-/-) topDirectory (fullPath Stage2 ghc)
getCompilerPath "stage-cabal" = do
  top <- topDirectory
  root <- buildRoot
  pure (top -/- root -/- "stage-cabal" -/- "bin" -/- "ghc")
getCompilerPath compiler = pure compiler

isInTreeCompiler :: String -> Bool
isInTreeCompiler c = isJust (stageOfTestCompiler c)

-- | Get the full path to the given program.
fullPath :: Stage -> Package -> Action FilePath
fullPath stage pkg = programPath =<< programContext stage pkg

-- stage 1 ghc lives under stage0/bin,
-- stage 2 ghc lives under stage1/bin, etc
stageOfTestCompiler :: String -> Maybe Stage
stageOfTestCompiler "stage1" = Just stage0InTree
stageOfTestCompiler "stage2" = Just Stage1
stageOfTestCompiler "stage3" = Just Stage2
stageOfTestCompiler _ = Nothing


-- | Are we testing a cross compiler
getTestCross :: String -> Action Bool
getTestCross testGhc =
  case stageOfTestCompiler testGhc of
    Just stg -> crossStage stg
    Nothing -> getBooleanSetting TestCrossCompiling


-- Given the testGhc string, either a stage0..stage1..stage2 etc or a path to
-- a compiler. Compute the absolute path to the relevant executable provided by
-- the package in the second argument.
getTestExePath :: String -> Package -> Action FilePath
getTestExePath testGhc pkg = do
  case stageOfTestCompiler testGhc of
    Just stg -> (-/-) <$> topDirectory <*> fullPath stg pkg
    Nothing -> do
     bindir <- getBinaryDirectory testGhc
     compiler_path <- getCompilerPath testGhc
     cross <- getBooleanSetting TestCrossCompiling
     let cross_prefix = if cross then dropWhileEnd ((/=) '-') (takeFileName compiler_path) else ""
     -- get relative path for the given program in the given stage
     let make_absolute rel_path = do
           abs_path <- liftIO (makeAbsolute rel_path)
           fixAbsolutePathOnWindows abs_path
     make_absolute (bindir </> (cross_prefix ++ programBasename vanilla pkg) <.> exe)
    -- get relative path for the given program in the given stage
