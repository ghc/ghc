module Settings.Builders.RunTest (runTestBuilderArgs) where

import CommandLine (TestArgs(..), defaultTestArgs, TestSpeed(..))
import Context
import Flavour
import GHC
import Hadrian.Utilities
import Oracles.Setting (setting)
import Oracles.TestSettings
import Rules.Test
import Settings.Builders.Common

oneZero :: String -> Bool -> String
oneZero lbl False = lbl ++ "=0"
oneZero lbl True = lbl ++ "=1"

stringToBool :: String -> Bool
stringToBool "YES"  = True
stringToBool "NO"   = False

-- | An abstraction to get boolean value of some settings
getBooleanSetting :: TestSetting -> Action Bool
getBooleanSetting key = fmap stringToBool $ testSetting key

-- Arguments to send to the runtest.py script.
--
-- A lot of this mirrors what's achieved at testsuite/mk/test.mk.
runTestBuilderArgs :: Args
runTestBuilderArgs = builder RunTest ? do
    pkgs     <- expr $ stagePackages Stage1
    libTests <- expr $ filterM doesDirectoryExist $ concat
            [ [ pkgPath pkg -/- "tests", pkgPath pkg -/- "tests-ghc" ]
            | pkg <- pkgs, isLibrary pkg, pkg /= rts, pkg /= libffi ]

    flav <- expr flavour
    rtsways <- expr $ testRTSSettings
    libways <- libraryWays flav
    let hasRtsWay w = elem w rtsways
        hasLibWay w = elem w libways
        debugged = ghcDebugged flav
    hasDynamic    <- expr $ getBooleanSetting TestGhcDynamic
    hasDynamicByDefault <- expr $ getBooleanSetting TestGhcDynamicByDefault
    withNativeCodeGen <- expr $ getBooleanSetting TestGhcWithNativeCodeGen
    withInterpreter   <- expr $ getBooleanSetting TestGhcWithInterpreter
    unregisterised    <- expr $ getBooleanSetting TestGhcUnregisterised
    withSMP           <- expr $ getBooleanSetting TestGhcWithSMP

    windows  <- expr windowsHost
    darwin   <- expr osxHost
    threads  <- shakeThreads <$> expr getShakeOptions
    os       <- expr $ testSetting TestHostOS
    arch     <- expr $ testSetting TestTargetARCH_CPP
    platform <- expr $ testSetting TestTARGETPLATFORM
    wordsize <- expr $ testSetting TestWORDSIZE
    top      <- expr topDirectory
    ghcFlags    <- expr runTestGhcFlags
    timeoutProg <- expr buildRoot <&> (-/- timeoutProgPath)

    -- TODO: set CABAL_MINIMAL_BUILD/CABAL_PLUGIN_BUILD

    mconcat [ arg $ "testsuite/driver/runtests.py"
            , arg $ "--rootdir=" ++ ("testsuite" -/- "tests")
            , pure ["--rootdir=" ++ test | test <- libTests]
            , arg "-e", arg $ "windows=" ++ show windows
            , arg "-e", arg $ "darwin=" ++ show darwin
            , arg "-e", arg $ "config.local=True"
            , arg "-e", arg $ "config.cleanup=False"                  -- Don't clean up.
            , arg "-e", arg $ "config.compiler_debugged=" ++ quote (yesNo debugged)
            , arg "-e", arg $ "ghc_debugged=" ++ quote (yesNo debugged)
            , arg "-e", arg $ oneZero "ghc_with_native_codegen" withNativeCodeGen

            , arg "-e", arg $ "config.have_interp=" ++ show withInterpreter
            , arg "-e", arg $ "config.unregisterised=" ++ show unregisterised

            , arg "-e", arg $ "ghc_compiler_always_flags=" ++ quote ghcFlags
            , arg "-e", arg $ oneZero "ghc_with_dynamic_rts" (hasRtsWay "dyn")
            , arg "-e", arg $ oneZero "ghc_with_threaded_rts" (hasRtsWay "thr")
            , arg "-e", arg $ oneZero "config.have_vanilla" (hasLibWay vanilla)
            , arg "-e", arg $ oneZero "config.have_dynamic" (hasLibWay dynamic)
            , arg "-e", arg $ oneZero "config.have_profiling" (hasLibWay profiling)
            , arg "-e", arg $ oneZero "ghc_with_smp" withSMP
            , arg "-e", arg $ "ghc_with_llvm=0"                       -- TODO: support LLVM

            , arg "-e", arg $ "config.ghc_dynamic_by_default=" ++ show hasDynamicByDefault
            , arg "-e", arg $ "config.ghc_dynamic=" ++ show hasDynamic

            , arg "-e", arg $ "config.in_tree_compiler=True"          -- Use default value, see https://github.com/ghc/ghc/blob/master/testsuite/mk/boilerplate.mk
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

-- | Prepare the command-line arguments to run GHC's test script.
getTestArgs :: Args
getTestArgs = do
    args            <- expr $ userSetting defaultTestArgs
    bindir          <- expr $ setBinaryDirectory (testCompiler args)
    compiler        <- expr $ setCompiler (testCompiler args)
    globalVerbosity <- shakeVerbosity <$> expr getShakeOptions
    let configFileArg= ["--config-file=" ++ (testConfigFile args)]
        testOnlyArg  = case testOnly args of
                           Just cases -> map ("--only=" ++) (words cases)
                           Nothing -> []
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

-- | Directory to look for Binaries
-- | We assume that required programs are present in the same binary directory
-- | in which ghc is stored and that they have their conventional name.
-- | QUESTION : packages can be named different from their conventional names.
-- | For example, ghc-pkg can be named as ghc-pkg-version. In such cases, it will
-- | be impossible to search the binary. Only possible way will be to take user
-- | inputs for these directory also. boilerplate soes not account for this
-- | problem, but simply returns an error. How should we handle such cases?
setBinaryDirectory :: String -> Action FilePath
setBinaryDirectory "stage0" = setting InstallBinDir
setBinaryDirectory "stage1" = liftM2 (-/-) topDirectory (stageBinPath Stage0)
setBinaryDirectory "stage2" = liftM2 (-/-) topDirectory (stageBinPath Stage1)
setBinaryDirectory compiler = pure $ parentPath compiler

-- | Set Test Compiler
setCompiler :: String -> Action FilePath
setCompiler "stage0" = setting SystemGhc
setCompiler "stage1" = liftM2 (-/-) topDirectory (fullpath Stage0 ghc)
setCompiler "stage2" = liftM2 (-/-) topDirectory (fullpath Stage1 ghc)
setCompiler compiler = pure compiler

-- | Set speed for test
setTestSpeed :: TestSpeed -> String
setTestSpeed Fast    = "2"
setTestSpeed Average = "1"
setTestSpeed Slow    = "0"

-- | Returns parent path of test compiler
-- | TODO : Is there a simpler way to find parent directory?
parentPath :: String -> String
parentPath path = let upPath = init $ splitOn "/" path
                  in  intercalate "/" upPath

-- | TODO: move to hadrian utilities.
fullpath :: Stage -> Package -> Action FilePath
fullpath stage pkg = programPath =<< programContext stage pkg
