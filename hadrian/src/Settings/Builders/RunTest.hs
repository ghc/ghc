{-# LANGUAGE TypeApplications #-}
module Settings.Builders.RunTest (runTestBuilderArgs
                                 , runTestGhcFlags
                                 , assertSameCompilerArgs
                                 , outOfTreeCompilerArgs
                                 , TestCompilerArgs(..) ) where

import Hadrian.Utilities
import qualified System.FilePath
import System.Environment

import CommandLine
import Oracles.TestSettings
import Packages
import Settings.Builders.Common
import qualified Data.Set    as Set
import Flavour
import qualified Context.Type as C
import System.Directory (findExecutable)
import Settings.Program
import qualified Context.Type

getTestSetting :: TestSetting -> Action String
getTestSetting key = testSetting key

-- | Parse the value of a Boolean test setting or report an error.
getBooleanSetting :: TestSetting -> Action Bool
getBooleanSetting key = fromMaybe (error msg) <$> parseYesNo <$> getTestSetting key
  where
    msg = "Cannot parse test setting " ++ quote (show key)

-- | Extra flags to send to the Haskell compiler to run tests.
runTestGhcFlags :: Action String
runTestGhcFlags = do
    unregisterised <- flag GhcUnregisterised

    let ifMinGhcVer ver opt = do v <- ghcCanonVersion
                                 if ver <= v then pure opt
                                             else pure ""

    -- Read extra argument for test from command line, like `-fvectorize`.
    ghcOpts <- fromMaybe "" <$> (liftIO $ lookupEnv "EXTRA_HC_OPTS")

    -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L28
    let ghcExtraFlags = if unregisterised
                           then "-optc-fno-builtin"
                           else ""

    -- Take flags to send to the Haskell compiler from test.mk.
    -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L37
    unwords <$> sequence
        [ pure " -dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -fno-dump-with-ways -rtsopts"
        , pure ghcOpts
        , pure ghcExtraFlags
        , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
        , ifMinGhcVer "711" "-fshow-warning-groups"
        , ifMinGhcVer "801" "-fdiagnostics-color=never"
        , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
        , pure "-Werror=compat" -- See #15278
        , pure "-dno-debug-output"
        ]

data TestCompilerArgs = TestCompilerArgs{
    hasDynamicRts, hasThreadedRts :: Bool
 ,   hasDynamic        :: Bool
 ,   leadingUnderscore :: Bool
 ,   withNativeCodeGen :: Bool
 ,   withInterpreter   :: Bool
 ,   unregisterised    :: Bool
 ,   tables_next_to_code :: Bool
 ,   withSMP           :: Bool
 ,   debugAssertions   :: Bool
      -- ^ Whether the compiler has debug assertions enabled,
      -- corresponding to the -DDEBUG option.
 ,   profiled          :: Bool
 ,   os,arch, platform, wordsize :: String
 ,   libdir :: FilePath
 ,   have_llvm :: Bool
 ,   rtsLinker :: Bool
 ,   pkgConfCacheFile :: FilePath }
   deriving (Eq, Show)


-- | If the tree is in-compiler then we already know how we will build it so
-- don't build anything in order to work out what we will build.
--
inTreeCompilerArgs :: Stage -> Action TestCompilerArgs
inTreeCompilerArgs stg = do


    (hasDynamicRts, hasThreadedRts) <- do
      ways <- interpretInContext (vanillaContext stg rts) getRtsWays
      return (dynamic `elem` ways, threaded `elem` ways)
    -- MP: We should be able to vary if stage1/stage2 is dynamic, ie a dynamic stage1
    -- should be able to built a static stage2?
    hasDynamic          <- (dynamic ==) . Context.Type.way <$> (programContext stg ghc)
    -- LeadingUnderscore is a property of the system so if cross-compiling stage1/stage2 could
    -- have different values? Currently not possible to express.
    leadingUnderscore   <- flag LeadingUnderscore
    withInterpreter     <- ghcWithInterpreter
    unregisterised      <- flag GhcUnregisterised
    tables_next_to_code <- flag TablesNextToCode
    withSMP             <- targetSupportsSMP
    debugAssertions     <- ($ succStage stg) . ghcDebugAssertions <$> flavour
    profiled            <- ghcProfiled        <$> flavour <*> pure stg

    os          <- setting HostOs
    arch        <- setting TargetArch
    let codegen_arches = ["x86_64", "i386", "powerpc", "powerpc64", "powerpc64le", "aarch64", "wasm32"]
    let withNativeCodeGen
          | unregisterised = False
          | arch `elem` codegen_arches = True
          | otherwise = False
    platform    <- setting TargetPlatform
    wordsize    <- (show @Int . (*8) . read) <$> setting TargetWordSize

    llc_cmd   <- settingsFileSetting SettingsFileSetting_LlcCommand
    have_llvm <- liftIO (isJust <$> findExecutable llc_cmd)

    top         <- topDirectory

    pkgConfCacheFile <- System.FilePath.normalise . (top -/-)
                    <$> (packageDbPath (PackageDbLoc stg Final) <&> (-/- "package.cache"))
    libdir           <- System.FilePath.normalise . (top -/-)
                    <$> stageLibPath stg

    rtsLinker <- (== "YES") <$> setting TargetHasRtsLinker

    return TestCompilerArgs{..}

ghcConfigPath :: FilePath
ghcConfigPath = "test/ghcconfig"

-- | If the compiler is out-of-tree then we have to query the compiler to work out
-- facts about it.
outOfTreeCompilerArgs :: Action TestCompilerArgs
outOfTreeCompilerArgs = do
    root <- buildRoot
    need [root -/- ghcConfigPath]
    (hasDynamicRts, hasThreadedRts) <- do
      ways <- testRTSSettings
      return ("dyn" `elem` ways, "thr" `elem` ways)
    hasDynamic          <- getBooleanSetting TestGhcDynamic
    leadingUnderscore   <- getBooleanSetting TestLeadingUnderscore
    withNativeCodeGen   <- getBooleanSetting TestGhcWithNativeCodeGen
    withInterpreter     <- getBooleanSetting TestGhcWithInterpreter
    unregisterised      <- getBooleanSetting TestGhcUnregisterised
    tables_next_to_code <- getBooleanSetting TestGhcTablesNextToCode
    withSMP             <- getBooleanSetting TestGhcWithSMP
    debugAssertions     <- getBooleanSetting TestGhcDebugged

    os          <- getTestSetting TestHostOS
    arch        <- getTestSetting TestTargetARCH_CPP
    platform    <- getTestSetting TestTARGETPLATFORM
    wordsize    <- getTestSetting TestWORDSIZE

    llc_cmd   <- getTestSetting TestLLC
    have_llvm <- liftIO (isJust <$> findExecutable llc_cmd)
    profiled <- getBooleanSetting TestGhcProfiled

    pkgConfCacheFile <- getTestSetting TestGhcPackageDb <&> (</> "package.cache")
    libdir <- getTestSetting TestGhcLibDir

    rtsLinker <- getBooleanSetting TestGhcWithRtsLinker
    return TestCompilerArgs{..}


-- | Assert that the inTree and outOfTree compiler args compute to the same
-- thing
assertSameCompilerArgs :: Stage -> Action ()
assertSameCompilerArgs stg = do
  in_args  <- inTreeCompilerArgs stg
  out_args <- outOfTreeCompilerArgs
  -- The assertion to check we calculated the right thing
  when (in_args /= out_args) $ putFailure $ unlines $
    [ "Hadrian assertion failure: in-tree arguments don't match out-of-tree arguments."
    , "Please report this bug on the GHC issue tracker. Continuing with in-tree arguments."
        -- NB: we always use the in-tree arguments whenever they are available.
    , "in-tree arguments:\n" ++ show in_args
    , "out-of-tree arguments:\n" ++ show out_args
    ]


-- Command line arguments for invoking the @runtest.py@ script. A lot of this
-- mirrors @testsuite/mk/test.mk@.
runTestBuilderArgs :: Args
runTestBuilderArgs = builder Testsuite ? do
    ctx <- getContext
    pkgs     <- expr $ stagePackages (C.stage ctx)
    libTests <- expr $ filterM doesDirectoryExist $ concat
            [ [ pkgPath pkg -/- "tests", pkgPath pkg -/- "tests-ghc" ]
            | pkg <- pkgs, isLibrary pkg, pkg /= rts, pkg /= libffi ]

    testGhc <- expr (testCompiler <$> userSetting defaultTestArgs)

    TestCompilerArgs{..} <- expr $
      case stageOfTestCompiler testGhc of
        Just stg -> inTreeCompilerArgs stg
        Nothing  -> outOfTreeCompilerArgs

    -- MP: TODO, these should be queried from the test compiler?
    bignumBackend <- getBignumBackend
    bignumCheck   <- getBignumCheck

    keepFiles           <- expr (testKeepFiles <$> userSetting defaultTestArgs)

    accept <- expr (testAccept <$> userSetting defaultTestArgs)
    (acceptPlatform, acceptOS) <- expr . liftIO $
        (,) <$> (maybe False (=="YES") <$> lookupEnv "PLATFORM")
            <*> (maybe False (=="YES") <$> lookupEnv "OS")
    (testEnv, testMetricsFile) <- expr . liftIO $
        (,) <$> lookupEnv "TEST_ENV" <*> lookupEnv "METRICS_FILE"
    perfBaseline <- expr . liftIO $ lookupEnv "PERF_BASELINE_COMMIT"
    targetWrapper <- expr . liftIO $ lookupEnv "CROSS_EMULATOR"

    threads     <- shakeThreads <$> expr getShakeOptions
    top         <- expr $ topDirectory
    ghcFlags    <- expr runTestGhcFlags
    cmdrootdirs <- expr (testRootDirs <$> userSetting defaultTestArgs)
    let defaultRootdirs = ("testsuite" -/- "tests") : libTests
        rootdirs | null cmdrootdirs = defaultRootdirs
                 | otherwise        = cmdrootdirs
    root        <- expr buildRoot
    let timeoutProg = root -/- timeoutPath
    statsFilesDir <- expr haddockStatsFilesDir

    let asBool :: String -> Bool -> String
        asBool s b = s ++ show b

    -- TODO: set CABAL_MINIMAL_BUILD/CABAL_PLUGIN_BUILD
    mconcat [ arg $ "testsuite/driver/runtests.py"
            , pure [ "--rootdir=" ++ testdir | testdir <- rootdirs ]
            , arg "--top", arg (top -/- "testsuite")
            , arg "-e", arg $ "windows=" ++ show windowsHost
            , arg "-e", arg $ "darwin=" ++ show osxHost
            , arg "-e", arg $ "config.local=False"
            , arg "-e", arg $ "config.cleanup=" ++ show (not keepFiles)
            , arg "-e", arg $ "config.accept=" ++ show accept
            , arg "-e", arg $ "config.accept_platform=" ++ show acceptPlatform
            , arg "-e", arg $ "config.accept_os=" ++ show acceptOS
            , arg "-e", arg $ "config.exeext=" ++ quote (if null exe then "" else "."<>exe)
            , arg "-e", arg $ "config.compiler_debugged=" ++ show debugAssertions

            -- MP: TODO, we do not need both, they get aliased to the same thing.
            , arg "-e", arg $ asBool "ghc_with_native_codegen=" withNativeCodeGen
            , arg "-e", arg $ asBool "config.have_ncg=" withNativeCodeGen
            , arg "-e", arg $ asBool "config.have_llvm=" have_llvm

            , arg "-e", arg $ asBool "config.compiler_profiled=" profiled

            , arg "-e", arg $ asBool "config.have_RTS_linker="  rtsLinker

            , arg "-e", arg $ "config.package_conf_cache_file=" ++ show pkgConfCacheFile

            , arg "-e", arg $ "config.libdir=" ++ show libdir


            , arg "-e", arg $ "config.have_interp=" ++ show withInterpreter
            , arg "-e", arg $ "config.unregisterised=" ++ show unregisterised
            , arg "-e", arg $ "config.tables_next_to_code=" ++ show tables_next_to_code

            , arg "-e", arg $ "ghc_compiler_always_flags=" ++ quote ghcFlags
            , arg "-e", arg $ asBool "ghc_with_dynamic_rts="  (hasDynamicRts)
            , arg "-e", arg $ asBool "ghc_with_threaded_rts=" (hasThreadedRts)
            , arg "-e", arg $ asBool "config.have_fast_bignum=" (bignumBackend /= "native" && not bignumCheck)
            , arg "-e", arg $ asBool "ghc_with_smp=" withSMP

            , arg "-e", arg $ "config.ghc_dynamic=" ++ show hasDynamic
            , arg "-e", arg $ "config.leading_underscore=" ++ show leadingUnderscore

            , arg "-e", arg $ "config.wordsize=" ++ show wordsize
            , arg "-e", arg $ "config.os="       ++ show os
            , arg "-e", arg $ "config.arch="     ++ show arch
            , arg "-e", arg $ "config.platform=" ++ show platform
            , arg "-e", arg $ "config.stage="    ++ show (stageNumber (C.stage ctx))

            , arg "--config", arg $ "gs=gs"                           -- Use the default value as in test.mk
            , arg "--config", arg $ "timeout_prog=" ++ show (top -/- timeoutProg)
            , arg "--config", arg $ "stats_files_dir=" ++ statsFilesDir
            , arg $ "--threads=" ++ show threads
            , case perfBaseline of
                Just commit | not (null commit) -> arg ("--perf-baseline=" ++ commit)
                _ -> mempty
            , emitWhenSet targetWrapper $ \cmd -> arg ("--target-wrapper=" ++ cmd)
            , emitWhenSet testEnv $ \env -> arg ("--test-env=" ++ env)
            , emitWhenSet testMetricsFile $ \file -> arg ("--metrics-file=" ++ file)
            , getTestArgs -- User-provided arguments from command line.
            ]

      where emitWhenSet Nothing  _ = mempty
            emitWhenSet (Just v) f = f v

            stageNumber :: Stage -> Int
            stageNumber (Stage0 GlobalLibs) = error "stageNumber stageBoot"
            stageNumber (Stage0 InTreeLibs) = 1
            stageNumber Stage1 = 2
            stageNumber Stage2 = 3
            stageNumber Stage3 = 4

-- | Command line arguments for running GHC's test script.
getTestArgs :: Args
getTestArgs = do
    -- targets specified in the TEST env var
    testEnvTargets <- maybe [] words <$> expr (liftIO $ lookupEnv "TEST")
    args            <- expr $ userSetting defaultTestArgs
    bindir          <- expr $ getBinaryDirectory (testCompiler args)
    compiler        <- expr $ getCompilerPath (testCompiler args)
    globalVerbosity <- shakeVerbosity <$> expr getShakeOptions
    -- the testsuite driver will itself tell us if we need to generate the docs target
    -- So we always pass the haddock path if the hadrian configuration allows us to build
    -- docs
    -- If the configuration doesn't allow us to build docs, then we don't pass the haddock
    -- option, and the testsuite driver will not subsequently ask us to build haddocks
    -- for the required tests
    haveDocs        <- willDocsBeBuilt
    let configFileArg= ["--config-file=" ++ (testConfigFile args)]
        testOnlyArg  =  map ("--only=" ++) (testOnly args ++ testEnvTargets)
        onlyPerfArg  = if testOnlyPerf args
                           then Just "--only-perf-tests"
                           else Nothing
        skipPerfArg  = if testSkipPerf args
                           then Just "--skip-perf-tests"
                           else Nothing
        brokenTestArgs = concat [ ["--broken-test", t] | t <- brokenTests args ]
        speedArg     = ["-e", "config.speed=" ++ setTestSpeed (testSpeed args)]
        summaryArg   = case testSummary args of
                           Just filepath -> Just $ "--summary-file=" ++ filepath
                           Nothing -> Just $ "--summary-file=testsuite_summary.txt"
        junitArg     = case testJUnit args of
                           Just filepath -> Just $ "--junit=" ++ filepath
                           Nothing -> Nothing
        metricsArg   = case testMetricsFile args of
                           Just filepath -> Just $ "--metrics-file=" ++ filepath
                           Nothing -> Nothing
        configArgs   = concat [["-e", configArg] | configArg <- testConfigs args]
        globalTestVerbosity = case globalVerbosity of
                                Silent -> "0"
                                Error -> "1"
                                Warn -> "1"
                                Info -> "2"
                                Verbose -> "4"
                                Diagnostic -> "5"
        verbosityArg = case testVerbosity args of
                           Nothing -> Just $ "--verbose=" ++ globalTestVerbosity
                           Just verbosity -> Just $ "--verbose=" ++ verbosity
        wayArgs      = map ("--way=" ++) (testWays args)
        compilerArg  = ["--config", "compiler=" ++ show (compiler)]
        ghcPkgArg    = ["--config", "ghc_pkg=" ++ show (bindir -/- "ghc-pkg" <.> exe)]
        haddockArg   = if haveDocs
          then [ "--config", "haddock=" ++ show (bindir -/- "haddock" <.> exe) ]
          else [ "--config", "haddock=" ]
        hp2psArg     = ["--config", "hp2ps=" ++ show (bindir -/- "hp2ps" <.> exe)]
        hpcArg       = ["--config", "hpc=" ++ show (bindir -/- "hpc" <.> exe)]
        inTreeArg    = [ "-e", "config.in_tree_compiler=" ++
          show (isInTreeCompiler (testCompiler args) || testHasInTreeFiles args) ]

    pure $  configFileArg ++ testOnlyArg ++ speedArg
         ++ catMaybes [ onlyPerfArg, skipPerfArg, summaryArg
                      , junitArg, metricsArg, verbosityArg  ]
         ++ configArgs ++ wayArgs ++  compilerArg ++ ghcPkgArg
         ++ haddockArg ++ hp2psArg ++ hpcArg ++ inTreeArg
         ++ brokenTestArgs

  where willDocsBeBuilt = expr $ do
          doctargets <- ghcDocs =<< flavour
          pure $ Haddocks `Set.member` doctargets


-- | Set speed for test
setTestSpeed :: TestSpeed -> String
setTestSpeed TestSlow   = "0"
setTestSpeed TestNormal = "1"
setTestSpeed TestFast   = "2"
