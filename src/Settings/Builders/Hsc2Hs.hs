module Settings.Builders.Hsc2Hs (hsc2HsArgs) where

import Expression
import Oracles
import Predicates (builder, stage0, notStage0)
import Settings
import Settings.Builders.GhcCabal

hsc2HsArgs :: Args
hsc2HsArgs = builder Hsc2Hs ? do
    stage   <- getStage
    src     <- getSource
    file    <- getFile
    ccPath  <- lift . builderPath $ Gcc stage
    gmpDirs <- getSettingList GmpIncludeDirs
    cFlags  <- getCFlags
    lFlags  <- getLFlags
    hArch   <- getSetting HostArch
    hOs     <- getSetting HostOs
    tArch   <- getSetting TargetArch
    tOs     <- getSetting TargetOs
    version <- if stage == Stage0
               then lift $ ghcCanonVersion
               else getSetting ProjectVersionInt
    mconcat [ arg $ "--cc=" ++ ccPath
            , arg $ "--ld=" ++ ccPath
            , notM windowsHost ? arg "--cross-safe"
            , append $ map ("-I"       ++) gmpDirs
            , append $ map ("--cflag=" ++) cFlags
            , append $ map ("--lflag=" ++) lFlags
            , notStage0 ? crossCompiling ? arg "--cross-compile"
            , stage0    ? arg ("--cflag=-D" ++ hArch ++ "_HOST_ARCH=1")
            , stage0    ? arg ("--cflag=-D" ++ hOs   ++ "_HOST_OS=1"  )
            , notStage0 ? arg ("--cflag=-D" ++ tArch ++ "_HOST_ARCH=1")
            , notStage0 ? arg ("--cflag=-D" ++ tOs   ++ "_HOST_OS=1"  )
            , arg ("--cflag=-D__GLASGOW_HASKELL__=" ++ version)
            , arg src
            , arg "-o", arg file ]

getCFlags :: Expr [String]
getCFlags = fromDiffExpr $ do
    pkg       <- getPackage
    path      <- getTargetPath
    iDirs     <- getPkgDataList IncludeDirs
    dDirs     <- getPkgDataList DepIncludeDirs
    cppArgs   <- getPkgDataList CppArgs
    depCcArgs <- getPkgDataList DepCcArgs
    mconcat [ ccArgs
            , argStagedSettingList ConfCcArgs
            , remove ["-O"]
            , argStagedSettingList ConfCppArgs
            , arg $ "-I" ++ path -/- "build/autogen"
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- iDirs ++ dDirs ]
            , append cppArgs
            , append depCcArgs
            , ccWarnings
            , arg "-include", arg $ path -/- "build/autogen/cabal_macros.h" ]

getLFlags :: Expr [String]
getLFlags = fromDiffExpr $ do
    ldArgs    <- getPkgDataList LdArgs
    libDirs   <- getPkgDataList DepLibDirs
    extraLibs <- getPkgDataList DepExtraLibs
    depLdArgs <- getPkgDataList DepLdArgs
    mconcat [ argStagedSettingList ConfGccLinkerArgs
            --, ldArgs -- TODO: resolve name conflict (ldArgs is currently empty)
            , append ldArgs
            , append $ [ "-L" ++ unifyPath dir | dir <- libDirs ]
            , append $ [ "-l" ++ unifyPath dir | dir <- extraLibs ]
            , append depLdArgs ]
