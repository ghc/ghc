module Rules.Test (testRules) where

import Base
import Expression
import Flavour
import Oracles.Flag
import Oracles.Setting
import Settings
import Target
import Utilities

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    "validate" ~> do
        need inplaceLibCopyTargets
        needBuilder $ Ghc CompileHs Stage2
        needBuilder $ GhcPkg Update Stage1
        needBuilder Hpc
        -- TODO: Figure out why @needBuilder Hsc2Hs@ doesn't work.
        -- TODO: Eliminate explicit filepaths.
        -- See https://github.com/snowleopard/hadrian/issues/376.
        need ["inplace/bin/hp2ps", "inplace/bin/hsc2hs"]
        build $ target (vanillaContext Stage2 compiler) (Make "testsuite/tests") [] []

    "test" ~> do
        pkgs     <- stagePackages Stage1
        tests    <- filterM doesDirectoryExist $ concat
                    [ [ pkgPath pkg -/- "tests", pkgPath pkg -/- "tests-ghc" ]
                    | pkg <- pkgs, isLibrary pkg, pkg /= rts, pkg /= libffi ]
        windows  <- windowsHost
        top      <- topDirectory
        compiler <- builderPath $ Ghc CompileHs Stage2
        ghcPkg   <- builderPath $ GhcPkg Update Stage1
        haddock  <- builderPath (Haddock BuildPackage)
        threads  <- shakeThreads <$> getShakeOptions
        debugged <- ghcDebugged <$> flavour
        ghcWithNativeCodeGenInt <- fromEnum <$> ghcWithNativeCodeGen
        ghcWithInterpreterInt   <- fromEnum <$> ghcWithInterpreter
        ghcUnregisterisedInt    <- fromEnum <$> flag GhcUnregisterised
        quietly . cmd "python2" $
            [ "testsuite/driver/runtests.py" ]
            ++ map ("--rootdir="++) tests ++
            [ "-e", "windows=" ++ show windows
            , "-e", "config.speed=2"
            , "-e", "ghc_compiler_always_flags=" ++ show "-fforce-recomp -dcore-lint -dcmm-lint -dno-debug-output -no-user-package-db -rtsopts"
            , "-e", "ghc_with_native_codegen=" ++ show ghcWithNativeCodeGenInt
            , "-e", "ghc_debugged=" ++ show (yesNo debugged)
            , "-e", "ghc_with_vanilla=1" -- TODO: do we always build vanilla?
            , "-e", "ghc_with_dynamic=0" -- TODO: support dynamic
            , "-e", "ghc_with_profiling=0" -- TODO: support profiling
            , "-e", "ghc_with_interpreter=" ++ show ghcWithInterpreterInt
            , "-e", "ghc_unregisterised=" ++ show ghcUnregisterisedInt
            , "-e", "ghc_with_threaded_rts=0" -- TODO: support threaded
            , "-e", "ghc_with_dynamic_rts=0" -- TODO: support dynamic
            , "-e", "ghc_dynamic_by_default=False" -- TODO: support dynamic
            , "-e", "ghc_dynamic=0" -- TODO: support dynamic
            , "-e", "ghc_with_llvm=0" -- TODO: support LLVM
            , "-e", "in_tree_compiler=True" -- TODO: when is it equal to False?
            , "-e", "clean_only=False" -- TODO: do we need to support True?
            , "--configfile=testsuite/config/ghc"
            , "--config", "compiler=" ++ show (top -/- compiler)
            , "--config", "ghc_pkg="  ++ show (top -/- ghcPkg)
            , "--config", "haddock="  ++ show (top -/- haddock)
            , "--summary-file", "testsuite_summary.txt"
            , "--threads=" ++ show threads
            ]

            -- , "--config", "hp2ps="    ++ quote ("hp2ps")
            -- , "--config", "hpc="      ++ quote ("hpc")
            -- , "--config", "gs=$(call quote_path,$(GS))"
            -- , "--config", "timeout_prog=$(call quote_path,$(TIMEOUT_PROGRAM))"
