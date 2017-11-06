module Settings.Default (
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultArgs, defaultLibraryWays, defaultRtsWays,
    defaultFlavour, defaultSplitObjects
    ) where

import qualified Hadrian.Builder.Ar
import qualified Hadrian.Builder.Sphinx
import qualified Hadrian.Builder.Tar

import CommandLine
import Expression
import Flavour
import Oracles.Flag
import Oracles.PackageData
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
import Settings.Builders.Xelatex
import Settings.Packages.Base
import Settings.Packages.Cabal
import Settings.Packages.Compiler
import Settings.Packages.Ghc
import Settings.Packages.GhcCabal
import Settings.Packages.Ghci
import Settings.Packages.GhcPkg
import Settings.Packages.GhcPrim
import Settings.Packages.Haddock
import Settings.Packages.Haskeline
import Settings.Packages.IntegerGmp
import Settings.Packages.Rts
import Settings.Packages.RunGhc
import Settings.Warnings

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

-- | Default source arguments, e.g. optimisation settings.
defaultSourceArgs :: SourceArgs
defaultSourceArgs = SourceArgs
    { hsDefault  = mconcat [ stage0    ? arg "-O"
                           , notStage0 ? arg "-O2"
                           , arg "-H32m" ]
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

-- Please update doc/flavours.md when changing the default build flavour.
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
    -- GHC-specific builders:
    [ alexBuilderArgs
    , ccBuilderArgs
    , configureBuilderArgs
    , deriveConstantsBuilderArgs
    , genPrimopCodeBuilderArgs
    , ghcBuilderArgs
    , ghcCabalBuilderArgs
    , ghcPkgBuilderArgs
    , haddockBuilderArgs
    , happyBuilderArgs
    , hsc2hsBuilderArgs
    , hsCppBuilderArgs
    , ldBuilderArgs
    , makeBuilderArgs
    , xelatexBuilderArgs
    -- Generic builders from the Hadrian library:
    , builder (Ar Pack     ) ? Hadrian.Builder.Ar.args Pack
    , builder (Ar Unpack   ) ? Hadrian.Builder.Ar.args Unpack
    , builder (Sphinx Html ) ? Hadrian.Builder.Sphinx.args Html
    , builder (Sphinx Latex) ? Hadrian.Builder.Sphinx.args Latex
    , builder (Sphinx Man  ) ? Hadrian.Builder.Sphinx.args Man
    , builder (Tar Create  ) ? Hadrian.Builder.Tar.args Create
    , builder (Tar Extract ) ? Hadrian.Builder.Tar.args Extract ]

-- | All 'Package'-dependent command line arguments.
defaultPackageArgs :: Args
defaultPackageArgs = mconcat
    [ basePackageArgs
    , cabalPackageArgs
    , compilerPackageArgs
    , ghcCabalPackageArgs
    , ghciPackageArgs
    , ghcPackageArgs
    , ghcPkgPackageArgs
    , ghcPrimPackageArgs
    , haddockPackageArgs
    , haskelinePackageArgs
    , integerGmpPackageArgs
    , rtsPackageArgs
    , runGhcPackageArgs
    , warningArgs ]
