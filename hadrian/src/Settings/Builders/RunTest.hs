module Settings.Builders.RunTest (runTestBuilderArgs, runTestGhcFlags) where

import Hadrian.Utilities
import System.Environment

import CommandLine
import Oracles.TestSettings
import Packages
import Settings.Builders.Common

getTestSetting :: TestSetting -> Expr String
getTestSetting key = expr $ testSetting key

-- | Parse the value of a Boolean test setting or report an error.
getBooleanSetting :: TestSetting -> Expr Bool
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
        [ pure " -dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -rtsopts"
        , pure ghcOpts
        , pure ghcExtraFlags
        , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
        , ifMinGhcVer "711" "-fshow-warning-groups"
        , ifMinGhcVer "801" "-fdiagnostics-color=never"
        , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
        , pure "-Werror=compat" -- See #15278
        , pure "-dno-debug-output"
        ]

-- Command line arguments for invoking the @runtest.py@ script. A lot of this
-- mirrors @testsuite/mk/test.mk@.
runTestBuilderArgs :: Args
runTestBuilderArgs = builder RunTest ? do
    pkgs     <- expr $ stagePackages Stage1
    libTests <- expr $ filterM doesDirectoryExist $ concat
            [ [ pkgPath pkg -/- "tests", pkgPath pkg -/- "tests-ghc" ]
            | pkg <- pkgs, isLibrary pkg, pkg /= rts, pkg /= libffi ]

    testGhc <- expr (testCompiler <$> userSetting defaultTestArgs)
    rtsWays <- expr testRTSSettings
    libWays <- expr (inferLibraryWays testGhc)
    let hasRtsWay w = elem w rtsWays
        hasLibWay w = elem w libWays
    hasDynamic          <- getBooleanSetting TestGhcDynamic
    hasDynamicByDefault <- getBooleanSetting TestGhcDynamicByDefault
    withNativeCodeGen   <- getBooleanSetting TestGhcWithNativeCodeGen
    withInterpreter     <- getBooleanSetting TestGhcWithInterpreter
    unregisterised      <- getBooleanSetting TestGhcUnregisterised
    withSMP             <- getBooleanSetting TestGhcWithSMP
    debugged            <- getBooleanSetting TestGhcDebugged
    keepFiles           <- expr (testKeepFiles <$> userSetting defaultTestArgs)
    withLlvm            <- expr (not . null <$> settingsFileSetting SettingsFileSetting_LlcCommand)

    accept <- expr (testAccept <$> userSetting defaultTestArgs)
    (acceptPlatform, acceptOS) <- expr . liftIO $
        (,) <$> (maybe False (=="YES") <$> lookupEnv "PLATFORM")
            <*> (maybe False (=="YES") <$> lookupEnv "OS")
    (testEnv, testMetricsFile) <- expr . liftIO $
        (,) <$> lookupEnv "TEST_ENV" <*> lookupEnv "METRICS_FILE"

    threads     <- shakeThreads <$> expr getShakeOptions
    os          <- getTestSetting TestHostOS
    arch        <- getTestSetting TestTargetARCH_CPP
    platform    <- getTestSetting TestTARGETPLATFORM
    wordsize    <- getTestSetting TestWORDSIZE
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
            , arg "-e", arg $ "windows=" ++ show windowsHost
            , arg "-e", arg $ "darwin=" ++ show osxHost
            , arg "-e", arg $ "config.local=False"
            , arg "-e", arg $ "config.cleanup=" ++ show (not keepFiles)
            , arg "-e", arg $ "config.accept=" ++ show accept
            , arg "-e", arg $ "config.accept_platform=" ++ show acceptPlatform
            , arg "-e", arg $ "config.accept_os=" ++ show acceptOS
            , arg "-e", arg $ "config.exeext=" ++ quote (if null exe then "" else "."<>exe)
            , arg "-e", arg $ "config.compiler_debugged=" ++
              show debugged
            , arg "-e", arg $ asBool "ghc_with_native_codegen=" withNativeCodeGen

            , arg "-e", arg $ "config.have_interp=" ++ show withInterpreter
            , arg "-e", arg $ "config.unregisterised=" ++ show unregisterised

            , arg "-e", arg $ "ghc_compiler_always_flags=" ++ quote ghcFlags
            , arg "-e", arg $ asBool "ghc_with_dynamic_rts="  (hasRtsWay "dyn")
            , arg "-e", arg $ asBool "ghc_with_threaded_rts=" (hasRtsWay "thr")
            , arg "-e", arg $ asBool "config.have_vanilla="   (hasLibWay vanilla)
            , arg "-e", arg $ asBool "config.have_dynamic="   (hasLibWay dynamic)
            , arg "-e", arg $ asBool "config.have_profiling=" (hasLibWay profiling)
            , arg "-e", arg $ asBool "ghc_with_smp=" withSMP
            , arg "-e", arg $ asBool "ghc_with_llvm=" withLlvm

            , arg "-e", arg $ "config.ghc_dynamic_by_default=" ++ show hasDynamicByDefault
            , arg "-e", arg $ "config.ghc_dynamic=" ++ show hasDynamic

            , arg "-e", arg $ "config.top=" ++ show (top -/- "testsuite")
            , arg "-e", arg $ "config.wordsize=" ++ show wordsize
            , arg "-e", arg $ "config.os="       ++ show os
            , arg "-e", arg $ "config.arch="     ++ show arch
            , arg "-e", arg $ "config.platform=" ++ show platform

            , arg "--config", arg $ "gs=gs"                           -- Use the default value as in test.mk
            , arg "--config", arg $ "timeout_prog=" ++ show (top -/- timeoutProg)
            , arg "--config", arg $ "stats_files_dir=" ++ statsFilesDir
            , arg $ "--threads=" ++ show threads
            , emitWhenSet testEnv $ \env -> arg ("--test-env=" ++ show env)
            , emitWhenSet testMetricsFile $ \file -> arg ("--metrics-file=" ++ file)
            , getTestArgs -- User-provided arguments from command line.
            ]

      where emitWhenSet Nothing  _ = mempty
            emitWhenSet (Just v) f = f v

-- | Command line arguments for running GHC's test script.
getTestArgs :: Args
getTestArgs = do
    -- targets specified in the TEST env var
    testEnvTargets <- maybe [] words <$> expr (liftIO $ lookupEnv "TEST")
    args            <- expr $ userSetting defaultTestArgs
    bindir          <- expr $ getBinaryDirectory (testCompiler args)
    compiler        <- expr $ getCompilerPath (testCompiler args)
    globalVerbosity <- shakeVerbosity <$> expr getShakeOptions
    haveDocs        <- areDocsPresent
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
        verbosityArg = case testVerbosity args of
                           Nothing -> Just $ "--verbose=" ++ show (fromEnum globalVerbosity)
                           Just verbosity -> Just $ "--verbose=" ++ verbosity
        wayArgs      = map ("--way=" ++) (testWays args)
        compilerArg  = ["--config", "compiler=" ++ show (compiler)]
        ghcPkgArg    = ["--config", "ghc_pkg=" ++ show (bindir -/- "ghc-pkg")]
        haddockArg   = if haveDocs
          then [ "--config", "haddock=" ++ show (bindir -/- "haddock") ]
          else [ "--config", "haddock=" ]
        hp2psArg     = ["--config", "hp2ps=" ++ show (bindir -/- "hp2ps")]
        hpcArg       = ["--config", "hpc=" ++ show (bindir -/- "hpc")]
        inTreeArg    = [ "-e", "config.in_tree_compiler=" ++
          show (testCompiler args `elem` ["stage1", "stage2", "stage3"]) ]

    pure $  configFileArg ++ testOnlyArg ++ speedArg
         ++ catMaybes [ onlyPerfArg, skipPerfArg, summaryArg
                      , junitArg, metricsArg, verbosityArg  ]
         ++ configArgs ++ wayArgs ++  compilerArg ++ ghcPkgArg
         ++ haddockArg ++ hp2psArg ++ hpcArg ++ inTreeArg
         ++ brokenTestArgs

  where areDocsPresent = expr $ do
          root <- buildRoot
          and <$> traverse doesFileExist (docFiles root)

        docFiles root =
          [ root -/- "docs" -/- "html" -/- "libraries" -/- p -/- (p ++ ".haddock")
          -- list of packages from
          -- utils/haddock/haddock-test/src/Test/Haddock/Config.hs
          | p <- [ "array", "base", "ghc-prim", "process", "template-haskell" ]
          ]

-- | Set speed for test
setTestSpeed :: TestSpeed -> String
setTestSpeed TestSlow   = "0"
setTestSpeed TestNormal = "1"
setTestSpeed TestFast   = "2"

-- | The purpose of this function is, given a compiler
--   (stage 1, 2, 3 or an external one), to infer the ways
--   that the libraries have been built in.
--
--   While we have this data readily available for in-tree compilers
--   that we build (through the 'Flavour'), that is not the case for
--   out-of-tree compilers that we may want to test, as is the case when
--   we are running './validate --hadrian' (it packages up a binary
--   distribution, installs it somewhere near and tests it).
--
--   We therefore proceed in a way that works regardless of whether we are
--   dealing with an in-tree compiler or not: we ask the GHC's install
--   ghc-pkg to give us the library directory of its @ghc-prim@ package and
--   look at what ways are available for the interface file of the
--   @GHC.PrimopWrappers@ module, like the Make build system does in
--   @testsuite\/mk\/test.mk@ to compute @HAVE_DYNAMIC@, @HAVE_VANILLA@
--   and @HAVE_PROFILING@:
--
--   - if we find @PrimopWrappers.hi@, we have the vanilla way;
--   - if we find @PrimopWrappers.dyn_hi@, we have the dynamic way;
--   - if we find @PrimopWrappers.p_hi@, we have the profiling way.
inferLibraryWays :: String -> Action [Way]
inferLibraryWays compiler = do
  bindir <- getBinaryDirectory compiler
  Stdout ghcPrimLibdirDirty <- cmd
    [bindir </> "ghc-pkg" <.> exe]
    ["field", "ghc-prim", "library-dirs", "--simple-output"]
  let ghcPrimLibdir = fixup ghcPrimLibdirDirty
  ways <- catMaybes <$> traverse (lookForWay ghcPrimLibdir) candidateWays
  return ways

  where lookForWay dir (hifile, w) = do
          exists <- doesFileExist (dir -/- hifile)
          if exists then return (Just w) else return Nothing

        candidateWays =
          [ ("GHC/PrimopWrappers.hi", vanilla)
          , ("GHC/PrimopWrappers.dyn_hi", dynamic)
          , ("GHC/PrimopWrappers.p_hi", profiling)
          ]

        -- If the ghc is in a directory with spaces in a path component,
        -- 'dir' is prefixed and suffixed with double quotes.
        -- In all cases, there is a \n at the end.
        -- This function cleans it all up.
        fixup = removeQuotes . removeNewline

        removeNewline path
          | "\n" `isSuffixOf` path = init path
          | otherwise              = path

        removeQuotes path
          | "\"" `isPrefixOf` path && "\"" `isSuffixOf` path = tail (init path)
          | otherwise                                        = path
