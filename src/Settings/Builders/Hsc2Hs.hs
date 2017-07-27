module Settings.Builders.Hsc2Hs (hsc2hsBuilderArgs) where

import Settings.Builders.Common

hsc2hsBuilderArgs :: Args
hsc2hsBuilderArgs = builder Hsc2Hs ? do
    stage   <- getStage
    ccPath  <- getBuilderPath $ Cc CompileC stage
    gmpDir  <- getSetting GmpIncludeDir
    cFlags  <- getCFlags
    lFlags  <- getLFlags
    top     <- getTopDirectory
    hArch   <- getSetting HostArch
    hOs     <- getSetting HostOs
    tArch   <- getSetting TargetArch
    tOs     <- getSetting TargetOs
    version <- if stage == Stage0
               then expr ghcCanonVersion
               else getSetting ProjectVersionInt
    mconcat [ arg $ "--cc=" ++ ccPath
            , arg $ "--ld=" ++ ccPath
            , notM windowsHost ? arg "--cross-safe"
            , append . map ("-I"       ++) $ words gmpDir
            , append $ map ("--cflag=" ++) cFlags
            , append $ map ("--lflag=" ++) lFlags
            , notStage0 ? crossCompiling ? arg "--cross-compile"
            , stage0    ? arg ("--cflag=-D" ++ hArch ++ "_HOST_ARCH=1")
            , stage0    ? arg ("--cflag=-D" ++ hOs   ++ "_HOST_OS=1"  )
            , notStage0 ? arg ("--cflag=-D" ++ tArch ++ "_HOST_ARCH=1")
            , notStage0 ? arg ("--cflag=-D" ++ tOs   ++ "_HOST_OS=1"  )
            , arg $ "--cflag=-D__GLASGOW_HASKELL__=" ++ version
            , arg $ "--template=" ++ top -/- templateHscPath
            , arg $ "-I" ++ top -/- "inplace/lib/include/"
            , arg =<< getInput
            , arg "-o", arg =<< getOutput ]

getCFlags :: Expr [String]
getCFlags = do
    context   <- getContext
    cppArgs   <- getPkgDataList CppArgs
    depCcArgs <- getPkgDataList DepCcArgs
    mconcat [ remove ["-O"] (cArgs <> argStagedSettingList ConfCcArgs)
            , argStagedSettingList ConfCppArgs
            , cIncludeArgs
            , append cppArgs
            , append depCcArgs
            , cWarnings
            , arg "-include", arg $ autogenPath context -/- "cabal_macros.h" ]

getLFlags :: Expr [String]
getLFlags = do
    pkgLdArgs <- getPkgDataList LdArgs
    libDirs   <- getPkgDataList DepLibDirs
    extraLibs <- getPkgDataList DepExtraLibs
    depLdArgs <- getPkgDataList DepLdArgs
    mconcat [ argStagedSettingList ConfGccLinkerArgs
            , ldArgs
            , append pkgLdArgs
            , append $ [ "-L" ++ unifyPath dir | dir <- libDirs ]
            , append $ [ "-l" ++ unifyPath dir | dir <- extraLibs ]
            , append depLdArgs ]
