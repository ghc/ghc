module Settings.Default (
    -- * Packages that are build by default and for the testsuite
    defaultPackages, testsuitePackages, stage0Packages,

    -- * Default build ways
    defaultLibraryWays, defaultRtsWays,

    -- * Default command line arguments for various builders
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultExtraArgs, defaultHaddockExtraArgs,

    -- * Default build flavour and BigNum backend
    defaultFlavour, defaultBignumBackend
    ) where

import qualified Data.Set as Set

import qualified Hadrian.Builder.Sphinx
import qualified Hadrian.Builder.Tar

import CommandLine
import Expression
import Flavour.Type
import Oracles.Flag
import Oracles.Setting
import Packages
import Settings.Builders.Alex
import Settings.Builders.DeriveConstants
import Settings.Builders.GenApply
import Settings.Builders.Cabal
import Settings.Builders.Cc
import Settings.Builders.Configure
import Settings.Builders.GenPrimopCode
import Settings.Builders.Ghc
import Settings.Builders.GhcPkg
import Settings.Builders.Haddock
import Settings.Builders.Happy
import Settings.Builders.Hsc2Hs
import Settings.Builders.HsCpp
import Settings.Builders.Ar
import Settings.Builders.Ld
import Settings.Builders.Make
import Settings.Builders.MergeObjects
import Settings.Builders.SplitSections
import Settings.Builders.RunTest
import Settings.Builders.Xelatex
import Settings.Packages
import Settings.Warnings
import qualified Hadrian.Builder.Git
import Settings.Builders.Win32Tarballs

-- | Packages that are built by default. You can change this in "UserSettings".
defaultPackages :: Stage -> Action [Package]
defaultPackages (Stage0 GlobalLibs) = stageBootPackages
defaultPackages (Stage0 InTreeLibs) = stage0Packages
defaultPackages Stage1 = stage1Packages
defaultPackages Stage2 = stage2Packages
defaultPackages Stage3 = return []

-- | Default bignum backend.
defaultBignumBackend :: String
defaultBignumBackend = "gmp"

-- These packages are things needed to do the build.. so they are only built by
-- boot compiler, with global package database. By default we will only build these
-- packages in StageBoot so if you also need to distribute anything here then add
-- it to `stage0packages` or `stage1packages` as appropriate.
stageBootPackages :: Action [Package]
stageBootPackages = return
  [ lintersCommon, lintCommitMsg, lintSubmoduleRefs, lintWhitespace, lintNotes
  , hsc2hs
  , compareSizes
  , deriveConstants
  , genprimopcode
  , unlit
  , genapply
  ]

-- | Packages built in 'Stage0' by default. You can change this in "UserSettings".
stage0Packages :: Action [Package]
stage0Packages = do
    cross <- flag CrossCompiling
    return $ [ cabalSyntax
             , cabal
             , compiler
             , directory -- depends on filepath, fileIo
             , filepath -- depends on os-string
             , fileio
             , ghc
             , ghcBoot
             , ghcBootThNext
             , ghcHeap
             , ghcPkg
             , ghcPlatform
             , ghcToolchain
             , ghci
             , haskeline
             , haddockApi
             , haddockLibrary
             , haddock
             , hp2ps
             , hpc
             , hpcBin
             , hsc2hs
             , osString -- new library not yet present for boot compilers
             , process -- depends on filepath
             , runGhc
             , semaphoreCompat -- depends on
             , time -- depends on win32
             , unlit
             , if windowsHost then win32 else unix
             -- We must use the in-tree `Win32` as the version
             -- bundled with GHC 9.6 is too old for `semaphore-compat`.
             -- Once 9.6 is no longer a boot compiler, we can drop win32/unix.
             -- These depend on `filepath`/`os-string` through an automatic flag
             -- that confused Hadrian, so we must make those a stage0 package as well.
             -- Once we drop `Win32`/`unix` it should be possible to drop those too.
             ]
          ++ [ terminfo | not windowsHost, not cross ]
          ++ [ timeout  | windowsHost                ]

-- | Packages built in 'Stage1' by default. You can change this in "UserSettings".
stage1Packages :: Action [Package]
stage1Packages = do
    let good_stage0_package p
          -- we only keep libraries for some reason
          | not (isLibrary p) = False
          -- but not win32/unix because it depends on cross-compilation target
          | p == win32        = False
          | p == unix         = False
          -- These packages are only needed for bootstrapping.
          -- See Note [Bootstrapping Template Haskell]
          | p == ghcBootThNext = False
          | otherwise         = True

    libraries0 <- filter good_stage0_package <$> stage0Packages
    cross      <- flag CrossCompiling
    winTarget  <- isWinTarget Stage1

    let when c xs = if c then xs else mempty

    return $ mconcat
      [ libraries0 -- Build all Stage0 libraries in Stage1
      , [ array
        , base
        , binary
        , bytestring
        , containers
        , deepseq
        , exceptions
        , ghc
        , ghcBignum
        , ghcBootTh
        , ghcCompact
        , ghcExperimental
        , ghcInternal
        , ghcPkg
        , ghcPrim
        , haddock
        , haskeline
        , hp2ps
        , hsc2hs
        , integerGmp
        , mtl
        , parsec
        , pretty
        , rts
        , semaphoreCompat
        , stm
        , templateHaskell
        , text
        , transformers
        , unlit
        , xhtml
        , hpcBin
        , if winTarget then win32 else unix
        ] ++
        [ iserv
        , runGhc
        , ghcToolchainBin
        ]
      , when (winTarget && not cross)
        [ -- See Note [Hadrian's ghci-wrapper package]
          ghciWrapper
        ]
      ]

-- | Packages built in 'Stage2' by default. You can change this in "UserSettings".
stage2Packages :: Action [Package]
stage2Packages = stage1Packages

-- | Packages that are built only for the testsuite.
testsuitePackages :: Action [Package]
testsuitePackages = return ([ timeout | windowsHost ] ++ [ checkPpr, checkExact, countDeps, lintCodes, ghcConfig, dumpDecls ])

-- | Default build ways for library packages:
-- * We always build 'vanilla' way.
-- * We build 'profiling' way when stage > Stage0.
-- * We build 'dynamic' way when stage > Stage0 and the platform supports it.
defaultLibraryWays :: Ways
defaultLibraryWays = do
    stage <- getStage
    Set.fromList <$>
      mconcat
      [ pure [vanilla]
      , notStage0 ? pure [profiling]
      , notStage0 ? targetSupportsGhciObjects stage ? pure [profilingDynamic, dynamic]
      ]

-- | Default build ways for the RTS.
defaultRtsWays :: Ways
defaultRtsWays = do
  stage <- getStage
  Set.fromList <$>
     mconcat
     [ pure [vanilla]
     , notStage0 ? pure
         [ profiling, debugProfiling
         , debug
         ]
     , notStage0 ? targetSupportsThreadedRts stage ? pure [threaded, threadedProfiling, threadedDebugProfiling, threadedDebug]
     , notStage0 ? targetSupportsSharedLibs stage ? pure
         [ dynamic, profilingDynamic, debugDynamic, debugProfilingDynamic
         ]
     , notStage0 ? targetSupportsSharedLibs stage ? targetSupportsThreadedRts stage ? pure
        [ threadedDynamic, threadedDebugDynamic, threadedProfilingDynamic, threadedDebugProfilingDynamic
        ]
     ]

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
    -- `compiler` is also a library but the specific arguments that we want
    -- to apply to that are given by the hsCompiler option. `ghc` is an
    -- executable so we don't have to exclude that.
    , libraryPackage   ? notM (packageOneOf [compiler]) ? hsLibrary
    , package compiler ? hsCompiler
    , package ghc      ? hsGhc ]

-- | All default command line arguments.
defaultExtraArgs :: Args
defaultExtraArgs =
  mconcat [ sourceArgs defaultSourceArgs, defaultHaddockExtraArgs ]

defaultHaddockExtraArgs :: Args
defaultHaddockExtraArgs = builder (Haddock BuildPackage) ?
  mconcat [ arg "--hyperlinked-source", arg "--hoogle", arg "--quickjump" ]


-- | Default source arguments, e.g. optimisation settings.
defaultSourceArgs :: SourceArgs
defaultSourceArgs = SourceArgs
    { hsDefault  = mconcat [ stage0    ? arg "-O"
                           , notStage0 ? arg "-O2"
                           , arg "-H32m" ]
    , hsLibrary  = notStage0 ? arg "-haddock"
    , hsCompiler = mempty
    , hsGhc      = mempty }

-- Please update doc/flavours.md when changing the default build flavour.
-- | Default build flavour. Other build flavours are defined in modules
-- @Settings.Flavours.*@. Users can add new build flavours in "UserSettings".
defaultFlavour :: Flavour
defaultFlavour = Flavour
    { name               = "default"
    , extraArgs          = defaultExtraArgs
    , packages           = defaultPackages
    , bignumBackend      = defaultBignumBackend
    , bignumCheck        = False
    , textWithSIMDUTF    = const False
    , libraryWays        = defaultLibraryWays
    , rtsWays            = defaultRtsWays
    , dynamicGhcPrograms = defaultDynamicGhcPrograms
    , ghcProfiled        = const False
    , ghcDebugged        = const False
    , ghcThreaded        = const True
    , ghcDebugAssertions = const False
    , ghcSplitSections   = False
    , ghcDocs            = defaultDocsTargets
    , hashUnitIds        = False }


defaultDocsTargets :: Action DocTargets
defaultDocsTargets = do
  cross <- flag CrossCompiling
  -- MP: Building documentation in cross configurations can work and should
  -- work but currently is left as a TODO
  --
  -- This is the ONLY place where we should branch on building documentation in
  -- the cross setting.
  if cross then return Set.empty
           else cmdDocsArgs


-- | Default logic for determining whether to build
--   dynamic GHC programs.
--
--   It corresponds to the DYNAMIC_GHC_PROGRAMS logic implemented
--   in @mk/config.mk.in@.
defaultDynamicGhcPrograms :: Stage -> Action Bool
defaultDynamicGhcPrograms stage = do
  supportsShared <- targetSupportsSharedLibs stage
  return (not windowsHost && supportsShared)

-- | All 'Builder'-dependent command line arguments.
defaultBuilderArgs :: Args
defaultBuilderArgs = mconcat
    -- GHC-specific builders:
    [ alexBuilderArgs
    , cabalBuilderArgs
    , ccBuilderArgs
    , configureBuilderArgs
    , deriveConstantsBuilderArgs
    , genapplyBuilderArgs
    , genPrimopCodeBuilderArgs
    , ghcBuilderArgs
    , ghcPkgBuilderArgs
    , haddockBuilderArgs
    , happyBuilderArgs
    , hsc2hsBuilderArgs
    , hsCppBuilderArgs
    , ldBuilderArgs
    , arBuilderArgs
    , makeBuilderArgs
    , mergeObjectsBuilderArgs
    , runTestBuilderArgs
    , validateBuilderArgs
    , xelatexBuilderArgs
    , win32TarballsArgs
    , splitSectionsArgs
    -- Generic builders from the Hadrian library:
    , builder (Sphinx HtmlMode ) ? Hadrian.Builder.Sphinx.args HtmlMode
    , builder (Sphinx LatexMode) ? Hadrian.Builder.Sphinx.args LatexMode
    , builder (Sphinx ManMode  ) ? Hadrian.Builder.Sphinx.args ManMode
    , builder (Sphinx InfoMode ) ? Hadrian.Builder.Sphinx.args InfoMode
    , builder (Tar Create      ) ? Hadrian.Builder.Tar.args Create
    , builder (Tar Extract     ) ? Hadrian.Builder.Tar.args Extract
    , Hadrian.Builder.Git.gitArgs
    ]

-- | All 'Package'-dependent command line arguments.
defaultPackageArgs :: Args
defaultPackageArgs = mconcat [ packageArgs
                             , builder Ghc ? ghcWarningsArgs ]
