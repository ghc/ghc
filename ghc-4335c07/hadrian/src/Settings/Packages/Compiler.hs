module Settings.Packages.Compiler (compilerPackageArgs) where

import Base
import Expression
import Flavour
import Oracles.Flag
import Oracles.Setting
import Settings

compilerPackageArgs :: Args
compilerPackageArgs = package compiler ? do
    stage   <- getStage
    rtsWays <- getRtsWays
    path    <- getBuildPath
    mconcat [ builder Alex ? arg "--latin1"

            , builder (Ghc CompileHs) ? mconcat
              [ inputs ["//GHC.hs", "//GhcMake.hs"] ? arg "-fprof-auto"
              , input "//Parser.hs" ?
                pure ["-O0", "-fno-ignore-interface-pragmas", "-fcmm-sink" ] ]

            , builder GhcCabal ? mconcat
              [ arg $ "--ghc-option=-DSTAGE=" ++ show (fromEnum stage + 1)
              , arg "--disable-library-for-ghci"
              , anyTargetOs ["openbsd"] ? arg "--ld-options=-E"
              , flag GhcUnregisterised ? arg "--ghc-option=-DNO_REGS"
              , notM ghcWithSMP ? arg "--ghc-option=-DNOSMP"
              , notM ghcWithSMP ? arg "--ghc-option=-optc-DNOSMP"
              , (threaded `elem` rtsWays) ?
                notStage0 ? arg "--ghc-option=-optc-DTHREADED_RTS"
              , ghcWithNativeCodeGen ? arg "--flags=ncg"
              , ghcWithInterpreter ?
                notStage0 ? arg "--flags=ghci"
              , crossCompiling ? arg "-f-terminfo"
              , ghcWithInterpreter ?
                ghcEnableTablesNextToCode ?
                notM (flag GhcUnregisterised) ?
                notStage0 ? arg "--ghc-option=-DGHCI_TABLES_NEXT_TO_CODE"
              , ghcWithInterpreter ?
                ghciWithDebugger <$> flavour ?
                notStage0 ? arg "--ghc-option=-DDEBUGGER"
              , ghcProfiled <$> flavour ?
                notStage0 ? arg "--ghc-pkg-option=--force" ]

            , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]
