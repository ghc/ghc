module Settings.Default (
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultArgs, defaultPackages, defaultLibraryWays, defaultRtsWays,
    defaultFlavour, defaultSplitObjects
    ) where

import Base
import CmdLineFlag
import Flavour
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.PackageData
import Predicate
import Settings
import Settings.Builders.Alex
import Settings.Builders.Ar
import Settings.Builders.DeriveConstants
import Settings.Builders.Cc
import Settings.Builders.Configure
import Settings.Builders.GenPrimopCode
import Settings.Builders.Ghc
import Settings.Builders.GhcCabal
import Settings.Builders.GhcPkg
import Settings.Builders.Haddock
import Settings.Builders.Happy
import Settings.Builders.Hsc2Hs
import Settings.Builders.HsCpp
import Settings.Builders.Ld
import Settings.Builders.Make
import Settings.Builders.Tar
import Settings.Packages.Base
import Settings.Packages.Compiler
import Settings.Packages.Ghc
import Settings.Packages.GhcCabal
import Settings.Packages.Ghci
import Settings.Packages.GhcPrim
import Settings.Packages.Haddock
import Settings.Packages.IntegerGmp
import Settings.Packages.Rts
import Settings.Packages.RunGhc

-- TODO: Move C source arguments here
-- | Default and package-specific source arguments.
data SourceArgs = SourceArgs
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

-- | Concatenate source arguments in appropriate order.
sourceArgs :: SourceArgs -> Args
sourceArgs SourceArgs {..} = builder Ghc ? do
    pkg       <- getPackage
    mconcat [ hsDefault
            , append =<< getPkgDataList HsArgs
            , isLibrary pkg    ? hsLibrary
            , package compiler ? hsCompiler
            , package ghc      ? hsGhc ]

-- | All default command line arguments.
defaultArgs :: Args
defaultArgs = mconcat
    [ defaultBuilderArgs
    , sourceArgs defaultSourceArgs
    , defaultPackageArgs ]

-- | Default flags about Werror
-- | mk/warnings.mk
defaultErrorGhcFlags :: Args
defaultErrorGhcFlags =
  mconcat [ notStage0 ? arg "-Werror"
          , (not <$> flag GccIsClang) ? mconcat [
                (not <$> flag GccLt46) ? (not <$> windowsHost) ?
                  arg "-Werror=unused-but-set-variable"
              , (not <$> flag GccLt44) ? arg "-Wno-error=inline" ]
          , flag GccIsClang ? arg "-Wno-unknown-pragmas" ]

-- | Default source arguments, e.g. optimisation settings.
defaultSourceArgs :: SourceArgs
defaultSourceArgs = SourceArgs
    { hsDefault  = mconcat [ stage0 ? arg "-O"
                           , notStage0 ? arg "-O2"
                           , arg "-H32m"
                           , defaultErrorGhcFlags ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc      = mempty }

-- | Packages that are built by default. You can change this by editing
-- 'userPackages' in "UserSettings".
defaultPackages :: Packages
defaultPackages = mconcat [ stage0 ? stage0Packages
                          , stage1 ? stage1Packages
                          , stage2 ? stage2Packages ]

stage0Packages :: Packages
stage0Packages = do
    win <- lift windowsHost
    ios <- lift iosHost
    append $ [ binary
             , cabal
             , checkApiAnnotations
             , compareSizes
             , compiler
             , deriveConstants
             , dllSplit
             , genapply
             , genprimopcode
             , ghc
             , ghcBoot
             , ghcBootTh
             , ghcCabal
             , ghci
             , ghcPkg
             , ghcTags
             , hsc2hs
             , hp2ps
             , hpc
             , mkUserGuidePart
             , templateHaskell
             , transformers
             , unlit                       ] ++
             [ terminfo | not win, not ios ] ++
             [ touchy   | win              ]

stage1Packages :: Packages
stage1Packages = do
    win <- lift windowsHost
    doc <- buildHaddock flavour
    mconcat [ stage0Packages
            , apply (filter isLibrary) -- Build all Stage0 libraries in Stage1
            , append $ [ array
                       , base
                       , bytestring
                       , containers
                       , deepseq
                       , directory
                       , filepath
                       , ghc
                       , ghcCabal
                       , ghcCompact
                       , ghcPrim
                       , haskeline
                       , hpcBin
                       , hsc2hs
                       , integerLibrary flavour
                       , pretty
                       , process
                       , rts
                       , runGhc
                       , time               ] ++
                       [ iservBin | not win ] ++
                       [ unix     | not win ] ++
                       [ win32    | win     ] ++
                       [ xhtml    | doc     ] ]

stage2Packages :: Packages
stage2Packages = buildHaddock flavour ? append [ haddock ]

-- | Default build ways for library packages:
-- * We always build 'vanilla' way.
-- * We build 'profiling' way when stage > Stage0.
-- * We build 'dynamic' way when stage > Stage0 and the platform supports it.
defaultLibraryWays :: Ways
defaultLibraryWays = mconcat
    [ append [vanilla]
    , notStage0 ? append [profiling] ]
    -- FIXME: Fix dynamic way and uncomment the line below, #4.
    -- , notStage0 ? platformSupportsSharedLibs ? append [dynamic] ]

-- | Default build ways for the RTS.
defaultRtsWays :: Ways
defaultRtsWays = do
    ways <- getLibraryWays
    mconcat
        [ append [ logging, debug, threaded, threadedDebug, threadedLogging ]
        , (profiling `elem` ways) ? append [threadedProfiling]
        , (dynamic `elem` ways) ?
          append [ dynamic, debugDynamic, threadedDynamic, threadedDebugDynamic
                 , loggingDynamic, threadedLoggingDynamic ] ]

-- | Default build flavour. Other build flavours are defined in modules
-- @Settings.Flavours.*@. Users can add new build flavours in "UserSettings".
defaultFlavour :: Flavour
defaultFlavour = Flavour
    { name               = "default"
    , args               = defaultArgs
    , packages           = defaultPackages
    , integerLibrary     = if cmdIntegerSimple then integerSimple else integerGmp
    , libraryWays        = defaultLibraryWays
    , rtsWays            = defaultRtsWays
    , splitObjects       = defaultSplitObjects
    , buildHaddock       = return cmdBuildHaddock
    , dynamicGhcPrograms = False
    , ghciWithDebugger   = False
    , ghcProfiled        = False
    , ghcDebugged        = False }

-- | Default condition for building split objects.
defaultSplitObjects :: Predicate
defaultSplitObjects = do
    goodStage <- notStage0 -- We don't split bootstrap (stage 0) packages
    pkg       <- getPackage
    supported <- lift supportsSplitObjects
    let goodPackage = isLibrary pkg && pkg /= compiler && pkg /= rts
    return $ cmdSplitObjects && goodStage && goodPackage && supported

-- | All 'Builder'-dependent command line arguments.
defaultBuilderArgs :: Args
defaultBuilderArgs = mconcat
    [ alexBuilderArgs
    , arBuilderArgs
    , ccBuilderArgs
    , configureBuilderArgs
    , deriveConstantsBuilderArgs
    , genPrimopCodeBuilderArgs
    , ghcBuilderArgs
    , ghcCabalBuilderArgs
    , ghcCabalHsColourBuilderArgs
    , ghcMBuilderArgs
    , ghcPkgBuilderArgs
    , haddockBuilderArgs
    , happyBuilderArgs
    , hsc2hsBuilderArgs
    , hsCppBuilderArgs
    , ldBuilderArgs
    , makeBuilderArgs
    , tarBuilderArgs ]

-- | Disable some warnings in packages we use
-- | https://github.com/ghc/ghc/blob/master/mk/warnings.mk#L46
disableWarningArgsStage0 :: Args
disableWarningArgsStage0 = stage Stage0 ? builder Ghc ? mconcat
    [ package transformers ? append [ "-fno-warn-unused-matches", "-fno-warn-unused-imports" ]
    , package terminfo ? append [ "-fno-warn-unused-imports" ] ]

disableWarningArgsStage1 :: Args
disableWarningArgsStage1 = notStage0 ? builder Ghc ? mconcat
    [ package bytestring ? append [ "-Wno-inline-rule-shadowing" ]
    , package haddock ? append [ "-Wno-unused-imports", "-Wno-deprecations" ]
    , package directory ? append [ "-Wno-unused-imports" ]
    , package binary ? append [ "-Wno-deprecations" ]
    , package haskeline ? append [ "-Wno-deprecations", "-Wno-unused-imports",
                                 "-Wno-redundant-constraints",
                                 "-Wno-simplifiable-class-constraints" ]
    , package pretty ? append [ "-Wno-unused-imports" ]
    , package primitive ? append [ "-Wno-unused-imports", "-Wno-deprecations" ]
    , package terminfo ? append [ "-Wno-unused-imports" ]
    , package xhtml ? append [ "-Wno-unused-imports", "-Wno-tabs" ]
    , package transformers ? append [ "-Wno-unused-matches", "-Wno-unused-imports",
                                    "-Wno-redundant-constraints", "-Wno-orphans" ]
    , package base ? append [ "-Wno-trustworthy-safe" ]
    , package ghcPrim ? append [ "-Wno-trustworthy-safe" ]
    , package win32 ? append [ "-Wno-trustworthy-safe" ] ]

-- GhcLibExtraHcOpts += -Wno-deprecated-flags
-- GhcBootLibExtraHcOpts += -fno-warn-deprecated-flags
disableWarningArgsLibs :: Args
disableWarningArgsLibs = do
  pkg <- getPackage
  isLibrary pkg ? builder Ghc ? mconcat
    [ notStage0 ? arg "-Wno-deprecated-flags"
    , stage Stage0 ? arg "-fno-warn-deprecated-flags"]

-- TODO: Disable warnings for Windows specifics

-- | All 'Package'-dependent command line arguments.
defaultPackageArgs :: Args
defaultPackageArgs = mconcat
    [ basePackageArgs
    , compilerPackageArgs
    , ghcPackageArgs
    , ghcCabalPackageArgs
    , ghciPackageArgs
    , ghcPrimPackageArgs
    , haddockPackageArgs
    , integerGmpPackageArgs
    , rtsPackageArgs
    , runGhcPackageArgs
    , disableWarningArgsStage0
    , disableWarningArgsStage1
    , disableWarningArgsLibs ]
