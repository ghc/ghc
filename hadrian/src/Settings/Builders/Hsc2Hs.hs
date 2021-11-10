module Settings.Builders.Hsc2Hs (hsc2hsBuilderArgs) where

import Hadrian.Haskell.Cabal.Type

import Builder
import Packages
import Settings.Builders.Common

hsc2hsBuilderArgs :: Args
hsc2hsBuilderArgs = builder Hsc2Hs ? do
    stage   <- getStage
    ccPath  <- getBuilderPath $ Cc CompileC stage
    gmpDir  <- getSetting GmpIncludeDir
    top     <- expr topDirectory
    hArch   <- getSetting HostArch
    hOs     <- getSetting HostOs
    tArch   <- getSetting TargetArch
    tOs     <- getSetting TargetOs
    version <- if stage == Stage0
               then expr ghcCanonVersion
               else getSetting ProjectVersionInt
    tmpl <- (top -/-) <$> expr (templateHscPath Stage0)
    mconcat [ arg $ "--cc=" ++ ccPath
            , arg $ "--ld=" ++ ccPath
            , notM isWinTarget ? notM (flag CrossCompiling) ? arg "--cross-safe"
            , pure $ map ("-I" ++) (words gmpDir)
            , map ("--cflag=" ++) <$> getCFlags
            , map ("--lflag=" ++) <$> getLFlags
            , notStage0 ? flag CrossCompiling ? arg "--cross-compile"
            , stage0    ? arg ("--cflag=-D" ++ hArch ++ "_HOST_ARCH=1")
            , stage0    ? arg ("--cflag=-D" ++ hOs   ++ "_HOST_OS=1"  )
            , notStage0 ? arg ("--cflag=-D" ++ tArch ++ "_HOST_ARCH=1")
            , notStage0 ? arg ("--cflag=-D" ++ tOs   ++ "_HOST_OS=1"  )
            , arg $ "--cflag=-D__GLASGOW_HASKELL__=" ++ version
            , arg $ "--template=" ++ tmpl
              -- We'll assume we compile with gcc or clang, and both support
              -- `-S` and can as such use the --via-asm flag, which should be
              -- faster and is required for cross compiling to windows, as the c
              -- compiler complains about non-constant expressions even though
              -- they are constant and end up as constants in the assembly.
              -- See #12849
            , flag CrossCompiling ? isWinTarget ? arg "--via-asm"
            , arg =<< getInput
            , arg "-o", arg =<< getOutput ]

getCFlags :: Expr [String]
getCFlags = do
    context <- getContext
    autogen <- expr $ autogenPath context
    let cabalMacros = autogen -/- "cabal_macros.h"
    expr $ need [cabalMacros]
    mconcat [ remove ["-O"] (cArgs <> getStagedSettingList ConfCcArgs)
            , getStagedSettingList ConfCppArgs
            , cIncludeArgs
            , getContextData ccOpts
            -- we might be able to leave out cppOpts, to be investigated.
            , getContextData cppOpts
            , getContextData depCcOpts
            , cWarnings
            , arg "-include", arg cabalMacros ]

getLFlags :: Expr [String]
getLFlags =
    mconcat [ getStagedSettingList ConfGccLinkerArgs
            , ldArgs
            , getContextData ldOpts
            , getContextData depLdOpts ]
