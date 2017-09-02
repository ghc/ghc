module Settings.Default (
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultArgs, defaultLibraryWays, defaultRtsWays,
    defaultFlavour, defaultSplitObjects
    ) where

import qualified Hadrian.Builder.Ar

import CommandLine
import Expression
import Flavour
import Oracles.Flag
import Oracles.PackageData
import Oracles.Setting
import Settings
import Settings.Builders.Alex
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
import Settings.Packages.Cabal
import Settings.Packages.Compiler
import Settings.Packages.Ghc
import Settings.Packages.GhcCabal
import Settings.Packages.Ghci
import Settings.Packages.GhcPkg
import Settings.Packages.GhcPrim
import Settings.Packages.Haddock
import Settings.Packages.Haskeline (haskelinePackageArgs)
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
sourceArgs SourceArgs {..} = builder Ghc ? mconcat
    [ hsDefault
    , getPkgDataList HsArgs
    , libraryPackage   ? hsLibrary
    , package compiler ? hsCompiler
    , package ghc      ? hsGhc ]

-- | All default command line arguments.
defaultArgs :: Args
defaultArgs = mconcat
    [ defaultBuilderArgs
    , sourceArgs defaultSourceArgs
    , defaultPackageArgs ]

-- ref: mk/warnings.mk
-- | Default Haskell warning-related arguments.
defaultHsWarningsArgs :: Args
defaultHsWarningsArgs = mconcat
    [ notStage0 ? arg "-Werror"
    , (not <$> flag GccIsClang) ? mconcat
      [ (not <$> flag GccLt46) ? (not <$> windowsHost) ? arg "-optc-Werror=unused-but-set-variable"
      , (not <$> flag GccLt44) ? arg "-optc-Wno-error=inline" ]
    , flag GccIsClang ? arg "-optc-Wno-unknown-pragmas" ]

-- | Default source arguments, e.g. optimisation settings.
defaultSourceArgs :: SourceArgs
defaultSourceArgs = SourceArgs
    { hsDefault  = mconcat [ stage0    ? arg "-O"
                           , notStage0 ? arg "-O2"
                           , arg "-H32m"
                           , defaultHsWarningsArgs ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc      = mempty }

-- | Default build ways for library packages:
-- * We always build 'vanilla' way.
-- * We build 'profiling' way when stage > Stage0.
-- * We build 'dynamic' way when stage > Stage0 and the platform supports it.
defaultLibraryWays :: Ways
defaultLibraryWays = mconcat
    [ pure [vanilla]
    , notStage0 ? pure [profiling]
    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ]

-- | Default build ways for the RTS.
defaultRtsWays :: Ways
defaultRtsWays = do
    ways <- getLibraryWays
    mconcat
        [ pure [ logging, debug, threaded, threadedDebug, threadedLogging ]
        , (profiling `elem` ways) ? pure [threadedProfiling]
        , (dynamic `elem` ways) ?
          pure [ dynamic, debugDynamic, threadedDynamic, threadedDebugDynamic
                 , loggingDynamic, threadedLoggingDynamic ] ]

-- | Default build flavour. Other build flavours are defined in modules
-- @Settings.Flavours.*@. Users can add new build flavours in "UserSettings".
defaultFlavour :: Flavour
defaultFlavour = Flavour
    { name               = "default"
    , args               = defaultArgs
    , packages           = defaultPackages
    , integerLibrary     = (\x -> if x then integerSimple else integerGmp) <$> cmdIntegerSimple
    , libraryWays        = defaultLibraryWays
    , rtsWays            = defaultRtsWays
    , splitObjects       = defaultSplitObjects
    , buildHaddock       = expr cmdBuildHaddock
    , dynamicGhcPrograms = False
    , ghciWithDebugger   = False
    , ghcProfiled        = False
    , ghcDebugged        = False }

-- | Default condition for building split objects.
defaultSplitObjects :: Predicate
defaultSplitObjects = do
    goodStage <- notStage0 -- We don't split bootstrap (stage 0) packages
    pkg       <- getPackage
    supported <- expr supportsSplitObjects
    split     <- expr cmdSplitObjects
    let goodPackage = isLibrary pkg && pkg /= compiler && pkg /= rts
    return $ split && goodStage && goodPackage && supported

-- | All 'Builder'-dependent command line arguments.
defaultBuilderArgs :: Args
defaultBuilderArgs = mconcat
    [ alexBuilderArgs
    , builder Ar ? Hadrian.Builder.Ar.args
    , ccBuilderArgs
    , configureBuilderArgs
    , deriveConstantsBuilderArgs
    , genPrimopCodeBuilderArgs
    , ghcBuilderArgs
    , ghcCbuilderArgs
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

-- TODO: Disable warnings for Windows specifics.
-- TODO: Move this elsewhere?
-- ref: mk/warnings.mk
-- | Disable warnings in packages we use.
disableWarningArgs :: Args
disableWarningArgs = builder Ghc ? mconcat
    [ stage0 ? mconcat
      [ package terminfo     ? pure [ "-fno-warn-unused-imports" ]
      , package transformers ? pure [ "-fno-warn-unused-matches"
                                    , "-fno-warn-unused-imports" ]
      , libraryPackage       ? pure [ "-fno-warn-deprecated-flags" ] ]

    , notStage0 ? mconcat
      [ package base         ? pure [ "-Wno-trustworthy-safe" ]
      , package binary       ? pure [ "-Wno-deprecations" ]
      , package bytestring   ? pure [ "-Wno-inline-rule-shadowing" ]
      , package directory    ? pure [ "-Wno-unused-imports" ]
      , package ghcPrim      ? pure [ "-Wno-trustworthy-safe" ]
      , package haddock      ? pure [ "-Wno-unused-imports"
                                    , "-Wno-deprecations" ]
      , package haskeline    ? pure [ "-Wno-deprecations"
                                    , "-Wno-unused-imports"
                                    , "-Wno-redundant-constraints"
                                    , "-Wno-simplifiable-class-constraints" ]
      , package pretty       ? pure [ "-Wno-unused-imports" ]
      , package primitive    ? pure [ "-Wno-unused-imports"
                                    , "-Wno-deprecations" ]
      , package terminfo     ? pure [ "-Wno-unused-imports" ]
      , package transformers ? pure [ "-Wno-unused-matches"
                                    , "-Wno-unused-imports"
                                    , "-Wno-redundant-constraints"
                                    , "-Wno-orphans" ]
      , package win32        ? pure [ "-Wno-trustworthy-safe" ]
      , package xhtml        ? pure [ "-Wno-unused-imports"
                                    , "-Wno-tabs" ]
      , libraryPackage       ? pure [ "-Wno-deprecated-flags" ] ] ]

-- | All 'Package'-dependent command line arguments.
defaultPackageArgs :: Args
defaultPackageArgs = mconcat
    [ basePackageArgs
    , cabalPackageArgs
    , compilerPackageArgs
    , ghcPackageArgs
    , ghcCabalPackageArgs
    , ghciPackageArgs
    , ghcPrimPackageArgs
    , haddockPackageArgs
    , integerGmpPackageArgs
    , rtsPackageArgs
    , runGhcPackageArgs
    , disableWarningArgs
    , ghcPkgPackageArgs
    , haskelinePackageArgs ]
