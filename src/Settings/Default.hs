module Settings.Default (
    defaultBuilderArgs, defaultPackageArgs, defaultArgs, defaultPackages,
    defaultLibraryWays, defaultRtsWays, defaultFlavour, defaultSplitObjects
    ) where

import Base
import CmdLineFlag
import Flavour
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Predicate
import Settings
import Settings.Builders.Alex
import Settings.Builders.Ar
import Settings.Builders.DeriveConstants
import Settings.Builders.Cc
import Settings.Builders.Configure
import Settings.Builders.GenApply
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
import Settings.Packages.Directory
import Settings.Packages.Ghc
import Settings.Packages.GhcCabal
import Settings.Packages.GhcPrim
import Settings.Packages.Haddock
import Settings.Packages.Hp2ps
import Settings.Packages.IntegerGmp
import Settings.Packages.IservBin
import Settings.Packages.Rts
import Settings.Packages.RunGhc
import Settings.Packages.Touchy
import Settings.Packages.Unlit
import UserSettings

-- | All 'Builder'-dependent command line arguments.
defaultBuilderArgs :: Args
defaultBuilderArgs = mconcat
    [ alexBuilderArgs
    , arBuilderArgs
    , ccBuilderArgs
    , configureBuilderArgs
    , deriveConstantsBuilderArgs
    , genApplyBuilderArgs
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


-- | All 'Package'-dependent command line arguments.
defaultPackageArgs :: Args
defaultPackageArgs = mconcat
    [ basePackageArgs
    , compilerPackageArgs
    , directoryPackageArgs
    , ghcPackageArgs
    , ghcCabalPackageArgs
    , ghcPrimPackageArgs
    , haddockPackageArgs
    , hp2psPackageArgs
    , integerGmpPackageArgs
    , iservBinPackageArgs
    , rtsPackageArgs
    , runGhcPackageArgs
    , touchyPackageArgs
    , unlitPackageArgs ]

-- | All default command line arguments.
defaultArgs :: Args
defaultArgs = mconcat
    [ defaultBuilderArgs
    , defaultPackageArgs
    , builder Ghc ? remove ["-Wall", "-fwarn-tabs"] ] -- TODO: Fix warning Args.

-- TODO: Simplify.
-- | Packages that are built by default. You can change this by editing
-- 'userPackages' in "UserSettings".
defaultPackages :: Packages
defaultPackages = mconcat
    [ stage0 ? packagesStage0
    , stage1 ? packagesStage1
    , stage2 ? packagesStage2 ]

packagesStage0 :: Packages
packagesStage0 = mconcat
    [ append [ binary, cabal, compiler, ghc, ghcBoot, ghcBootTh, ghcCabal
             , ghcPkg, hsc2hs, hoopl, hpc, templateHaskell, transformers ]
    -- the stage0 predicate makes sure these packages are built only in Stage0
    , stage0 ? append [ deriveConstants, dllSplit, genapply, genprimopcode
                      , hp2ps, unlit ]
    , stage0 ? windowsHost ? append [touchy]
    , notM windowsHost ? notM iosHost ? append [terminfo] ]

packagesStage1 :: Packages
packagesStage1 = mconcat
    [ packagesStage0
    , append [ array, base, bytestring, containers, compareSizes, deepseq
             , directory, filepath, ghci, ghcPrim, haskeline, hpcBin
             , integerLibrary, pretty, process, rts, runGhc, time ]
    , windowsHost      ? append [win32]
    , notM windowsHost ? append [unix]
    , notM windowsHost ? append [iservBin]
    , buildHaddock flavour ? append [xhtml] ]

-- TODO: Currently there is an unchecked assumption that we build only programs
-- in Stage2 and Stage3. Can we check this in compile time?
packagesStage2 :: Packages
packagesStage2 = mconcat
    [ append [checkApiAnnotations, ghcTags, mkUserGuidePart]
    , buildHaddock flavour ? append [haddock] ]

-- TODO: What about profilingDynamic way? Do we need platformSupportsSharedLibs?
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
