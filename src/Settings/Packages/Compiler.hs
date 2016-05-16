module Settings.Packages.Compiler (compilerPackageArgs) where

import Base
import Expression
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Predicates
import Settings

compilerPackageArgs :: Args
compilerPackageArgs = package compiler ? do
    stage   <- getStage
    rtsWays <- getRtsWays
    path    <- getBuildPath
    mconcat [ builder Alex ? arg "--latin1"

            , builder Ghc ? arg ("-I" ++ path)

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
              , ghcWithInterpreter ?
                ghcEnableTablesNextToCode ?
                notM (flag GhcUnregisterised) ?
                notStage0 ? arg "--ghc-option=-DGHCI_TABLES_NEXT_TO_CODE"
              , ghcWithInterpreter ?
                ghciWithDebugger ?
                notStage0 ? arg "--ghc-option=-DDEBUGGER"
              , ghcProfiled ?
                notStage0 ? arg "--ghc-pkg-option=--force" ]

            , builder Haddock ? arg ("--optghc=-I" ++ path) ]
