module Settings.Builders.RunTest (runTestBuilderArgs, runTestGhcFlags) where

import Hadrian.Utilities
import System.Environment

import CommandLine
import Flavour
import Oracles.Setting (setting)
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
        [ pure " -dcore-lint -dcmm-lint -no-user-package-db -rtsopts"
        , pure ghcOpts
        , pure ghcExtraFlags
        , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
        , ifMinGhcVer "711" "-fshow-warning-groups"
        , ifMinGhcVer "801" "-fdiagnostics-color=never"
        , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
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

    flav    <- expr flavour
    rtsWays <- expr testRTSSettings
    libWays <- libraryWays flav
    let hasRtsWay w = elem w rtsWays
        hasLibWay w = elem w libWays
        debugged    = ghcDebugged flav
    hasDynamic          <- getBooleanSetting TestGhcDynamic
    hasDynamicByDefault <- getBooleanSetting TestGhcDynamicByDefault
    withNativeCodeGen   <- getBooleanSetting TestGhcWithNativeCodeGen
    withInterpreter     <- getBooleanSetting TestGhcWithInterpreter
    unregisterised      <- getBooleanSetting TestGhcUnregisterised
    withSMP             <- getBooleanSetting TestGhcWithSMP

    windows     <- expr windowsHost
    darwin      <- expr osxHost
    threads     <- shakeThreads <$> expr getShakeOptions
    os          <- getTestSetting TestHostOS
    arch        <- getTestSetting TestTargetARCH_CPP
    platform    <- getTestSetting TestTARGETPLATFORM
    wordsize    <- getTestSetting TestWORDSIZE
    top         <- expr $ topDirectory
    ghcFlags    <- expr runTestGhcFlags
    timeoutProg <- expr buildRoot <&> (-/- timeoutPath)
    integerLib  <- expr (integerLibrary flav)

    let asZeroOne s b = s ++ zeroOne b

    -- TODO: set CABAL_MINIMAL_BUILD/CABAL_PLUGIN_BUILD
    mconcat [ arg $ "testsuite/driver/runtests.py"
            , arg $ "--rootdir=" ++ ("testsuite" -/- "tests")
            , pure ["--rootdir=" ++ test | test <- libTests]
            , arg "-e", arg $ "windows=" ++ show windows
            , arg "-e", arg $ "darwin=" ++ show darwin
            , arg "-e", arg $ "config.local=True"
            , arg "-e", arg $ "config.cleanup=False" -- Don't clean up.
            , arg "-e", arg $ "config.compiler_debugged=" ++ quote (yesNo debugged)
            , arg "-e", arg $ "ghc_debugged=" ++ quote (yesNo debugged)
            , arg "-e", arg $ asZeroOne "ghc_with_native_codegen=" withNativeCodeGen

            , arg "-e", arg $ "config.have_interp=" ++ show withInterpreter
            , arg "-e", arg $ "config.unregisterised=" ++ show unregisterised

            , arg "-e", arg $ "ghc_compiler_always_flags=" ++ quote ghcFlags
            , arg "-e", arg $ asZeroOne "ghc_with_dynamic_rts="  (hasRtsWay "dyn")
            , arg "-e", arg $ asZeroOne "ghc_with_threaded_rts=" (hasRtsWay "thr")
            , arg "-e", arg $ asZeroOne "config.have_vanilla="   (hasLibWay vanilla)
            , arg "-e", arg $ asZeroOne "config.have_dynamic="   (hasLibWay dynamic)
            , arg "-e", arg $ asZeroOne "config.have_profiling=" (hasLibWay profiling)
            , arg "-e", arg $ asZeroOne "ghc_with_smp=" withSMP
            , arg "-e", arg $ "ghc_with_llvm=0" -- TODO: support LLVM

            , arg "-e", arg $ "config.ghc_dynamic_by_default=" ++ show hasDynamicByDefault
            , arg "-e", arg $ "config.ghc_dynamic=" ++ show hasDynamic
            , arg "-e", arg $ "config.integer_backend=" ++ show (pkgName integerLib)

            -- Use default value, see:
            -- https://github.com/ghc/ghc/blob/master/testsuite/mk/boilerplate.mk
            , arg "-e", arg $ "config.in_tree_compiler=True"
            , arg "-e", arg $ "config.top=" ++ show (top -/- "testsuite")
            , arg "-e", arg $ "config.wordsize=" ++ show wordsize
            , arg "-e", arg $ "config.os="       ++ show os
            , arg "-e", arg $ "config.arch="     ++ show arch
            , arg "-e", arg $ "config.platform=" ++ show platform

            , arg "--config", arg $ "gs=gs"                           -- Use the default value as in test.mk
            , arg "--config", arg $ "timeout_prog=" ++ show (top -/- timeoutProg)
            , arg $ "--threads=" ++ show threads
            , getTestArgs -- User-provided arguments from command line.
            ]

-- | Command line arguments for running GHC's test script.
getTestArgs :: Args
getTestArgs = do
    -- targets specified in the TEST env var
    testEnvTargets <- maybe [] words <$> expr (liftIO $ lookupEnv "TEST")
    args            <- expr $ userSetting defaultTestArgs
    bindir          <- expr $ setBinaryDirectory (testCompiler args)
    compiler        <- expr $ setCompiler (testCompiler args)
    globalVerbosity <- shakeVerbosity <$> expr getShakeOptions
    let configFileArg= ["--config-file=" ++ (testConfigFile args)]
        testOnlyArg  =  map ("--only=" ++) (testOnly args ++ testEnvTargets)
        onlyPerfArg  = if testOnlyPerf args
                           then Just "--only-perf-tests"
                           else Nothing
        skipPerfArg  = if testSkipPerf args
                           then Just "--skip-perf-tests"
                           else Nothing
        speedArg     = ["-e", "config.speed=" ++ setTestSpeed (testSpeed args)]
        summaryArg   = case testSummary args of
                           Just filepath -> Just $ "--summary-file" ++ quote filepath
                           Nothing -> Just $ "--summary-file=testsuite_summary.txt"
        junitArg     = case testJUnit args of
                           Just filepath -> Just $ "--junit " ++ quote filepath
                           Nothing -> Nothing
        configArgs   = concat [["-e", configArg] | configArg <- testConfigs args]
        verbosityArg = case testVerbosity args of
                           Nothing -> Just $ "--verbose=" ++ show (fromEnum globalVerbosity)
                           Just verbosity -> Just $ "--verbose=" ++ verbosity
        wayArgs      = map ("--way=" ++) (testWays args)
        compilerArg  = ["--config", "compiler=" ++ show (compiler)]
        ghcPkgArg    = ["--config", "ghc_pkg=" ++ show (bindir -/- "ghc-pkg")]
        haddockArg   = ["--config", "haddock=" ++ show (bindir -/- "haddock")]
        hp2psArg     = ["--config", "hp2ps=" ++ show (bindir -/- "hp2ps")]
        hpcArg       = ["--config", "hpc=" ++ show (bindir -/- "hpc")]
    pure $  configFileArg ++ testOnlyArg ++ speedArg
         ++ catMaybes [ onlyPerfArg, skipPerfArg, summaryArg
                      , junitArg, verbosityArg  ]
         ++ configArgs ++ wayArgs ++  compilerArg ++ ghcPkgArg
         ++ haddockArg ++ hp2psArg ++ hpcArg

-- TODO: Switch to 'Stage' as the first argument instead of 'String'.
-- | Directory to look for Binaries
-- | We assume that required programs are present in the same binary directory
-- | in which ghc is stored and that they have their conventional name.
-- | QUESTION : packages can be named different from their conventional names.
-- | For example, ghc-pkg can be named as ghc-pkg-version. In such cases, it will
-- | be impossible to search the binary. Only possible way will be to take user
-- | inputs for these directory also. boilerplate soes not account for this
-- | problem, but simply returns an error. How should we handle such cases?
setBinaryDirectory :: String -> Action FilePath
setBinaryDirectory "stage0" = takeDirectory <$> setting SystemGhc
setBinaryDirectory "stage1" = liftM2 (-/-) topDirectory (stageBinPath Stage0)
setBinaryDirectory "stage2" = liftM2 (-/-) topDirectory (stageBinPath Stage1)
setBinaryDirectory compiler = pure $ parentPath compiler

-- TODO: Switch to 'Stage' as the first argument instead of 'String'.
-- | Set Test Compiler.
setCompiler :: String -> Action FilePath
setCompiler "stage0" = setting SystemGhc
setCompiler "stage1" = liftM2 (-/-) topDirectory (fullPath Stage0 ghc)
setCompiler "stage2" = liftM2 (-/-) topDirectory (fullPath Stage1 ghc)
setCompiler compiler = pure compiler

-- | Set speed for test
setTestSpeed :: TestSpeed -> String
setTestSpeed Slow    = "0"
setTestSpeed Average = "1"
setTestSpeed Fast    = "2"

-- | Returns parent path of test compiler
-- | TODO: Is there a simpler way to find parent directory?
parentPath :: String -> String
parentPath path = intercalate "/" $ init $ splitOn "/" path

-- | TODO: Move to Hadrian utilities.
fullPath :: Stage -> Package -> Action FilePath
fullPath stage pkg = programPath =<< programContext stage pkg
