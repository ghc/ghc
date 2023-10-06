module Settings.Builders.Hsc2Hs (hsc2hsBuilderArgs) where

import Hadrian.Haskell.Cabal.Type

import Builder
import Settings.Builders.Common
import GHC.Toolchain (tgtCCompilerLink, ccLinkProgram)
import GHC.Toolchain.Program

hsc2hsBuilderArgs :: Args
hsc2hsBuilderArgs = builder Hsc2Hs ? do
    stage   <- getStage
    ccPath  <- getBuilderPath $ Cc CompileC stage
    gmpDir  <- getSetting GmpIncludeDir
    top     <- expr topDirectory
    hArch   <- queryHost queryArch
    hOs     <- queryHost queryOS
    tArch   <- queryTarget stage queryArch
    tOs     <- queryTarget stage queryOS
    version <- case stage of
                  Stage0 {} -> expr ghcCanonVersion
                  _ ->  getSetting ProjectVersionInt
    let cross = case stage of
                  Stage0 {} -> return False
                  _ -> expr (crossStage (predStage stage))
    tmpl <- (top -/-) <$> expr (templateHscPath stage0Boot)
    mconcat [ arg $ "--cc=" ++ ccPath
            , arg $ "--ld=" ++ ccPath
            , notM (isWinTarget stage) ? notM cross ? arg "--cross-safe"
            , pure $ map ("-I" ++) (words gmpDir)
            , map ("--cflag=" ++) <$> getCFlags
            , map ("--lflag=" ++) <$> getLFlags
            , notStage0 ? cross ? arg "--cross-compile"
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
              -- MP: Wrong use of CrossCompiling
            , cross ? isWinTarget stage ? arg "--via-asm"
            , arg =<< getInput
            , arg "-o", arg =<< getOutput ]

getCFlags :: Expr [String]
getCFlags = do
    context <- getContext
    autogen <- expr $ autogenPath context
    let cabalMacros = autogen -/- "cabal_macros.h"
    expr $ need [cabalMacros]
    mconcat [ remove ["-O"] (cArgs <> getStagedCCFlags)
            -- Either "-E" is not part of the configured cpp args, or we can't add those args to invocations of things like this
            -- ROMES:TODO: , prgFlags . cppProgram . tgtCPreprocessor <$> getStagedTargetConfig
            , cIncludeArgs
            , getContextData ccOpts
            -- we might be able to leave out cppOpts, to be investigated.
            , getContextData cppOpts
            , getContextData depCcOpts
            , cWarnings
            , arg "-include", arg cabalMacros ]

getLFlags :: Expr [String]
getLFlags =
    mconcat [ prgFlags . ccLinkProgram . tgtCCompilerLink <$> getStagedTarget
            , ldArgs
            , getContextData ldOpts
            , getContextData depLdOpts ]
