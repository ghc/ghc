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

-- | Packages that are built by default. You can change this by editing
-- 'userPackages' in "UserSettings".
defaultPackages :: Packages
defaultPackages = mconcat [ packagesStage0, packagesStage1, packagesStage2 ]

packagesStage0 :: Packages
packagesStage0 = stage0 ? do
    win <- lift windowsHost
    ios <- lift iosHost
    append $ [ binary
             , cabal
             , compiler
             , deriveConstants
             , dllSplit
             , genapply
             , genprimopcode
             , ghc
             , ghcBoot
             , ghcBootTh
             , ghcCabal
             , ghcPkg
             , hsc2hs
             , hoopl
             , hp2ps
             , hpc
             , mkUserGuidePart
             , templateHaskell
             , transformers
             , unlit                       ] ++
             [ terminfo | not win, not ios ] ++
             [ touchy   | win              ]

packagesStage1 :: Packages
packagesStage1 = stage1 ? do
    win <- lift windowsHost
    ios <- lift iosHost
    doc <- buildHaddock flavour
    append $ [ array
             , base
             , binary
             , bytestring
             , cabal
             , containers
             , compareSizes
             , compiler
             , deepseq
             , directory
             , filepath
             , ghc
             , ghcBoot
             , ghcBootTh
             , ghcCabal
             , ghci
             , ghcPkg
             , ghcPrim
             , haskeline
             , hoopl
             , hpc
             , hpcBin
             , hsc2hs
             , integerLibrary
             , pretty
             , process
             , rts
             , runGhc
             , templateHaskell
             , time
             , transformers                ] ++
             [ iservBin | not win          ] ++
             [ terminfo | not win, not ios ] ++
             [ unix     | not win          ] ++
             [ win32    | win              ] ++
             [ xhtml    | doc              ]

-- TODO: Currently there is an unchecked assumption that we build only programs
-- in Stage2 and Stage3. Can we check this in compile time?
packagesStage2 :: Packages
packagesStage2 = stage2 ? do
    doc <- buildHaddock flavour
    append $ [ checkApiAnnotations
             , ghcTags       ] ++
             [ haddock | doc ]

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
