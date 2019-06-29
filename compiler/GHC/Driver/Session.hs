{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------
--
-- | Dynamic flags
--
-- Most flags are dynamic flags, which means they can change from compilation
-- to compilation using @OPTIONS_GHC@ pragmas, and in a multi-session GHC each
-- session can be using different dynamic flags. Dynamic flags can also be set
-- at the prompt in GHCi.
--
-- (c) The University of Glasgow 2005
--
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Driver.Session (
        -- * Dynamic flags and associated configuration types
        DumpFlag(..),
        GeneralFlag(..),
        WarningFlag(..), WarnReason(..),
        Language(..),
        PlatformConstants(..),
        FatalMessager, LogAction, FlushOut(..), FlushErr(..),
        ProfAuto(..),
        glasgowExtsFlags,
        warningGroups, warningHierarchies,
        hasPprDebug, hasNoDebugOutput, hasNoStateHack, hasNoOptCoercion,
        dopt, dopt_set, dopt_unset,
        gopt, gopt_set, gopt_unset, setGeneralFlag', unSetGeneralFlag',
        wopt, wopt_set, wopt_unset,
        wopt_fatal, wopt_set_fatal, wopt_unset_fatal,
        xopt, xopt_set, xopt_unset,
        xopt_set_unlessExplSpec,
        lang_set,
        whenGeneratingDynamicToo, ifGeneratingDynamicToo,
        whenCannotGenerateDynamicToo,
        dynamicTooMkDynamicDynFlags,
        dynamicOutputFile,
        DynFlags(..),
        FlagSpec(..),
        HasDynFlags(..), ContainsDynFlags(..),
        RtsOptsEnabled(..),
        HscTarget(..), isObjectTarget, defaultObjectTarget,
        targetRetainsAllBindings,
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..), PackageArg(..), ModRenaming(..),
        packageFlagsChanged,
        IgnorePackageFlag(..), TrustFlag(..),
        PackageDBFlag(..), PkgDbRef(..),
        Option(..), showOpt,
        DynLibLoader(..),
        fFlags, fLangFlags, xFlags,
        wWarningFlags,
        dynFlagDependencies,
        makeDynFlagsConsistent,
        positionIndependent,
        optimisationFlags,
        setFlagsFromEnvFile,

        addWay', updateWays,

        thisPackage, thisComponentId, thisUnitIdInsts,

        -- ** Log output
        putLogMsg,

        -- ** Safe Haskell
        SafeHaskellMode(..),
        safeHaskellOn, safeHaskellModeEnabled,
        safeImportsOn, safeLanguageOn, safeInferOn,
        packageTrustOn,
        safeDirectImpsReq, safeImplicitImpsReq,
        unsafeFlags, unsafeFlagsForInfer,

        -- ** LLVM Targets
        LlvmTarget(..), LlvmConfig(..),

        -- ** System tool settings and locations
        Settings(..),
        sProgramName,
        sProjectVersion,
        sGhcUsagePath,
        sGhciUsagePath,
        sToolDir,
        sTopDir,
        sTmpDir,
        sGlobalPackageDatabasePath,
        sLdSupportsCompactUnwind,
        sLdSupportsBuildId,
        sLdSupportsFilelist,
        sLdIsGnuLd,
        sGccSupportsNoPie,
        sPgm_L,
        sPgm_P,
        sPgm_F,
        sPgm_c,
        sPgm_a,
        sPgm_l,
        sPgm_dll,
        sPgm_T,
        sPgm_windres,
        sPgm_libtool,
        sPgm_ar,
        sPgm_ranlib,
        sPgm_lo,
        sPgm_lc,
        sPgm_lcc,
        sPgm_i,
        sOpt_L,
        sOpt_P,
        sOpt_P_fingerprint,
        sOpt_F,
        sOpt_c,
        sOpt_cxx,
        sOpt_a,
        sOpt_l,
        sOpt_windres,
        sOpt_lo,
        sOpt_lc,
        sOpt_lcc,
        sOpt_i,
        sExtraGccViaCFlags,
        sTargetPlatformString,
        sIntegerLibrary,
        sIntegerLibraryType,
        sGhcWithInterpreter,
        sGhcWithNativeCodeGen,
        sGhcWithSMP,
        sGhcRTSWays,
        sTablesNextToCode,
        sLeadingUnderscore,
        sLibFFI,
        sGhcThreaded,
        sGhcDebugged,
        sGhcRtsWithLibdw,
        IntegerLibrary(..),
        GhcNameVersion(..),
        FileSettings(..),
        PlatformMisc(..),
        settings,
        programName, projectVersion,
        ghcUsagePath, ghciUsagePath, topDir, tmpDir,
        versionedAppDir, versionedFilePath,
        extraGccViaCFlags, globalPackageDatabasePath,
        pgm_L, pgm_P, pgm_F, pgm_c, pgm_a, pgm_l, pgm_dll, pgm_T,
        pgm_windres, pgm_libtool, pgm_ar, pgm_ranlib, pgm_lo, pgm_lc,
        pgm_lcc, pgm_i,
        opt_L, opt_P, opt_F, opt_c, opt_cxx, opt_a, opt_l, opt_i,
        opt_P_signature,
        opt_windres, opt_lo, opt_lc, opt_lcc,
        tablesNextToCode,

        -- ** Manipulating DynFlags
        addPluginModuleName,
        defaultDynFlags,                -- Settings -> DynFlags
        defaultWays,
        initDynFlags,                   -- DynFlags -> IO DynFlags
        defaultFatalMessager,
        defaultLogAction,
        defaultLogActionHPrintDoc,
        defaultLogActionHPutStrDoc,
        defaultFlushOut,
        defaultFlushErr,

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlags,
        updOptLevel,
        setTmpDir,
        setUnitId,
        canonicalizeHomeModule,
        canonicalizeModuleIfHome,

        -- ** Parsing DynFlags
        parseDynamicFlagsCmdLine,
        parseDynamicFilePragma,
        parseDynamicFlagsFull,

        -- ** Available DynFlags
        allNonDeprecatedFlags,
        flagsAll,
        flagsDynamic,
        flagsPackage,
        flagsForCompletion,

        supportedLanguagesAndExtensions,
        languageExtensions,

        -- ** DynFlags C compiler options
        picCCOpts, picPOpts,

        -- * Compiler configuration suitable for display to the user
        compilerInfo,

#include "GHCConstantsHaskellExports.hs"
        bLOCK_SIZE_W,
        wordAlignment,
        tAG_MASK,
        mAX_PTR_TAG,

        unsafeGlobalDynFlags, setUnsafeGlobalDynFlags,

        -- * SSE and AVX
        isSseEnabled,
        isSse2Enabled,
        isSse4_2Enabled,
        isBmiEnabled,
        isBmi2Enabled,
        isAvxEnabled,
        isAvx2Enabled,
        isAvx512cdEnabled,
        isAvx512erEnabled,
        isAvx512fEnabled,
        isAvx512pfEnabled,

        -- * Linker/compiler information
        LinkerInfo(..),
        CompilerInfo(..),

        -- * File cleanup
        FilesToClean(..), emptyFilesToClean,

        -- * Include specifications
        IncludeSpecs(..), addGlobalInclude, addQuoteInclude, flattenIncludes,

        -- * SDoc
        initSDocContext,

        -- * Make use of the Cmm CFG
        CfgWeights(..)
  ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Platform
import GHC.UniqueSubdir (uniqueSubdir)
import PlatformConstants
import GHC.Types.Module
import {-# SOURCE #-} GHC.Driver.Plugins
import {-# SOURCE #-} GHC.Driver.Hooks
import {-# SOURCE #-} PrelNames ( mAIN )
import {-# SOURCE #-} GHC.Driver.Packages (PackageState, emptyPackageState, PackageDatabase, mkComponentId)
import GHC.Driver.Phases ( Phase(..), phaseInputExt )
import GHC.Driver.Flags
import GHC.Driver.Ways
import Config
import CliOption
import GHC.Driver.CmdLine hiding (WarnReason(..))
import qualified GHC.Driver.CmdLine as Cmd
import Constants
import GhcNameVersion
import Panic
import qualified PprColour as Col
import Util
import Maybes
import MonadUtils
import qualified Pretty
import GHC.Types.SrcLoc
import GHC.Types.Basic ( Alignment, alignmentOf, IntWithInf, treatZeroAsInf )
import FastString
import Fingerprint
import FileSettings
import Outputable
import Settings
import ToolSettings

import {-# SOURCE #-} ErrUtils ( Severity(..), MsgDoc, mkLocMessageAnn
                               , getCaretDiagnostic, DumpAction, TraceAction
                               , defaultDumpAction, defaultTraceAction )
import Json
import SysTools.Terminal ( stderrSupportsAnsiColors )
import SysTools.BaseDir ( expandToolDir, expandTopDir )

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Data.Ord
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath
import System.Directory
import System.Environment (lookupEnv)
import System.IO
import System.IO.Error
import Text.ParserCombinators.ReadP hiding (char)
import Text.ParserCombinators.ReadP as R

import EnumSet (EnumSet)
import qualified EnumSet

import GHC.Foreign (withCString, peekCString)
import qualified GHC.LanguageExtensions as LangExt

#if GHC_STAGE >= 2
-- used by SHARED_GLOBAL_VAR
import Foreign (Ptr)
#endif

-- Note [Updating flag description in the User's Guide]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you modify anything in this file please make sure that your changes are
-- described in the User's Guide. Please update the flag description in the
-- users guide (docs/users_guide) whenever you add or change a flag.

-- Note [Supporting CLI completion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The command line interface completion (in for example bash) is an easy way
-- for the developer to learn what flags are available from GHC.
-- GHC helps by separating which flags are available when compiling with GHC,
-- and which flags are available when using GHCi.
-- A flag is assumed to either work in both these modes, or only in one of them.
-- When adding or changing a flag, please consider for which mode the flag will
-- have effect, and annotate it accordingly. For Flags use defFlag, defGhcFlag,
-- defGhciFlag, and for FlagSpec use flagSpec or flagGhciSpec.

-- Note [Adding a language extension]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There are a few steps to adding (or removing) a language extension,
--
--  * Adding the extension to GHC.LanguageExtensions
--
--    The Extension type in libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs
--    is the canonical list of language extensions known by GHC.
--
--  * Adding a flag to DynFlags.xFlags
--
--    This is fairly self-explanatory. The name should be concise, memorable,
--    and consistent with any previous implementations of the similar idea in
--    other Haskell compilers.
--
--  * Adding the flag to the documentation
--
--    This is the same as any other flag. See
--    Note [Updating flag description in the User's Guide]
--
--  * Adding the flag to Cabal
--
--    The Cabal library has its own list of all language extensions supported
--    by all major compilers. This is the list that user code being uploaded
--    to Hackage is checked against to ensure language extension validity.
--    Consequently, it is very important that this list remains up-to-date.
--
--    To this end, there is a testsuite test (testsuite/tests/driver/T4437.hs)
--    whose job it is to ensure these GHC's extensions are consistent with
--    Cabal.
--
--    The recommended workflow is,
--
--     1. Temporarily add your new language extension to the
--        expectedGhcOnlyExtensions list in T4437 to ensure the test doesn't
--        break while Cabal is updated.
--
--     2. After your GHC change is accepted, submit a Cabal pull request adding
--        your new extension to Cabal's list (found in
--        Cabal/Language/Haskell/Extension.hs).
--
--     3. After your Cabal change is accepted, let the GHC developers know so
--        they can update the Cabal submodule and remove the extensions from
--        expectedGhcOnlyExtensions.
--
--  * Adding the flag to the GHC Wiki
--
--    There is a change log tracking language extension additions and removals
--    on the GHC wiki:  https://gitlab.haskell.org/ghc/ghc/wikis/language-pragma-history
--
--  See #4437 and #8176.

-- -----------------------------------------------------------------------------
-- DynFlags


-- | Used to differentiate the scope an include needs to apply to.
-- We have to split the include paths to avoid accidentally forcing recursive
-- includes since -I overrides the system search paths. See #14312.
data IncludeSpecs
  = IncludeSpecs { includePathsQuote  :: [String]
                 , includePathsGlobal :: [String]
                 }
  deriving Show

-- | Append to the list of includes a path that shall be included using `-I`
-- when the C compiler is called. These paths override system search paths.
addGlobalInclude :: IncludeSpecs -> [String] -> IncludeSpecs
addGlobalInclude spec paths  = let f = includePathsGlobal spec
                               in spec { includePathsGlobal = f ++ paths }

-- | Append to the list of includes a path that shall be included using
-- `-iquote` when the C compiler is called. These paths only apply when quoted
-- includes are used. e.g. #include "foo.h"
addQuoteInclude :: IncludeSpecs -> [String] -> IncludeSpecs
addQuoteInclude spec paths  = let f = includePathsQuote spec
                              in spec { includePathsQuote = f ++ paths }

-- | Concatenate and flatten the list of global and quoted includes returning
-- just a flat list of paths.
flattenIncludes :: IncludeSpecs -> [String]
flattenIncludes specs = includePathsQuote specs ++ includePathsGlobal specs

-- | The various Safe Haskell modes
data SafeHaskellMode
   = Sf_None          -- ^ inferred unsafe
   | Sf_Unsafe        -- ^ declared and checked
   | Sf_Trustworthy   -- ^ declared and checked
   | Sf_Safe          -- ^ declared and checked
   | Sf_SafeInferred  -- ^ inferred as safe
   | Sf_Ignore        -- ^ @-fno-safe-haskell@ state
   deriving (Eq)

instance Show SafeHaskellMode where
    show Sf_None         = "None"
    show Sf_Unsafe       = "Unsafe"
    show Sf_Trustworthy  = "Trustworthy"
    show Sf_Safe         = "Safe"
    show Sf_SafeInferred = "Safe-Inferred"
    show Sf_Ignore       = "Ignore"

instance Outputable SafeHaskellMode where
    ppr = text . show

-- | Contains not only a collection of 'GeneralFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  ghcMode               :: GhcMode,
  ghcLink               :: GhcLink,
  hscTarget             :: HscTarget,

  -- formerly Settings
  ghcNameVersion    :: {-# UNPACK #-} !GhcNameVersion,
  fileSettings      :: {-# UNPACK #-} !FileSettings,
  targetPlatform    :: Platform,       -- Filled in by SysTools
  toolSettings      :: {-# UNPACK #-} !ToolSettings,
  platformMisc      :: {-# UNPACK #-} !PlatformMisc,
  platformConstants :: PlatformConstants,
  rawSettings       :: [(String, String)],

  integerLibrary        :: IntegerLibrary,
    -- ^ IntegerGMP or IntegerSimple. Set at configure time, but may be overridden
    --   by GHC-API users. See Note [The integer library] in PrelNames
  llvmConfig            :: LlvmConfig,
    -- ^ N.B. It's important that this field is lazy since we load the LLVM
    -- configuration lazily. See Note [LLVM Configuration] in SysTools.
  verbosity             :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
  optLevel              :: Int,         -- ^ Optimisation level
  debugLevel            :: Int,         -- ^ How much debug information to produce
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  ruleCheck             :: Maybe String,
  inlineCheck           :: Maybe String, -- ^ A prefix to report inlining decisions about
  strictnessBefore      :: [Int],       -- ^ Additional demand analysis

  parMakeCount          :: Maybe Int,   -- ^ The number of modules to compile in parallel
                                        --   in --make mode, where Nothing ==> compile as
                                        --   many in parallel as there are CPUs.

  enableTimeStats       :: Bool,        -- ^ Enable RTS timing statistics?
  ghcHeapSize           :: Maybe Int,   -- ^ The heap size to set.

  maxRelevantBinds      :: Maybe Int,   -- ^ Maximum number of bindings from the type envt
                                        --   to show in type error messages
  maxValidHoleFits      :: Maybe Int,   -- ^ Maximum number of hole fits to show
                                        --   in typed hole error messages
  maxRefHoleFits        :: Maybe Int,   -- ^ Maximum number of refinement hole
                                        --   fits to show in typed hole error
                                        --   messages
  refLevelHoleFits      :: Maybe Int,   -- ^ Maximum level of refinement for
                                        --   refinement hole fits in typed hole
                                        --   error messages
  maxUncoveredPatterns  :: Int,         -- ^ Maximum number of unmatched patterns to show
                                        --   in non-exhaustiveness warnings
  maxPmCheckModels      :: Int,         -- ^ Soft limit on the number of models
                                        --   the pattern match checker checks
                                        --   a pattern against. A safe guard
                                        --   against exponential blow-up.
  simplTickFactor       :: Int,         -- ^ Multiplier for simplifier ticks
  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  specConstrRecursive   :: Int,         -- ^ Max number of specialisations for recursive types
                                        --   Not optional; otherwise ForceSpecConstr can diverge.
  binBlobThreshold      :: Word,        -- ^ Binary literals (e.g. strings) whose size is above
                                        --   this threshold will be dumped in a binary file
                                        --   by the assembler code generator (0 to disable)
  liberateCaseThreshold :: Maybe Int,   -- ^ Threshold for LiberateCase
  floatLamArgs          :: Maybe Int,   -- ^ Arg count for lambda floating
                                        --   See GHC.Core.Op.Monad.FloatOutSwitches

  liftLamsRecArgs       :: Maybe Int,   -- ^ Maximum number of arguments after lambda lifting a
                                        --   recursive function.
  liftLamsNonRecArgs    :: Maybe Int,   -- ^ Maximum number of arguments after lambda lifting a
                                        --   non-recursive function.
  liftLamsKnown         :: Bool,        -- ^ Lambda lift even when this turns a known call
                                        --   into an unknown call.

  cmmProcAlignment      :: Maybe Int,   -- ^ Align Cmm functions at this boundary or use default.

  historySize           :: Int,         -- ^ Simplification history size

  importPaths           :: [FilePath],
  mainModIs             :: Module,
  mainFunIs             :: Maybe String,
  reductionDepth        :: IntWithInf,   -- ^ Typechecker maximum stack depth
  solverIterations      :: IntWithInf,   -- ^ Number of iterations in the constraints solver
                                         --   Typically only 1 is needed

  thisInstalledUnitId   :: InstalledUnitId,
  thisComponentId_      :: Maybe ComponentId,
  thisUnitIdInsts_      :: Maybe [(ModuleName, Module)],

  -- ways
  ways                  :: Set Way,     -- ^ Way flags from the command line
  buildTag              :: String,      -- ^ The global \"way\" (e.g. \"p\" for prof)

  -- For object splitting
  splitInfo             :: Maybe (String,Int),

  -- paths etc.
  objectDir             :: Maybe String,
  dylibInstallName      :: Maybe String,
  hiDir                 :: Maybe String,
  hieDir                :: Maybe String,
  stubDir               :: Maybe String,
  dumpDir               :: Maybe String,

  objectSuf             :: String,
  hcSuf                 :: String,
  hiSuf                 :: String,
  hieSuf                :: String,

  canGenerateDynamicToo :: IORef Bool,
  dynObjectSuf          :: String,
  dynHiSuf              :: String,

  outputFile            :: Maybe String,
  dynOutputFile         :: Maybe String,
  outputHi              :: Maybe String,
  dynLibLoader          :: DynLibLoader,

  -- | This is set by 'GHC.Driver.Pipeline.runPipeline' based on where
  --    its output is going.
  dumpPrefix            :: Maybe FilePath,

  -- | Override the 'dumpPrefix' set by 'GHC.Driver.Pipeline.runPipeline'.
  --    Set by @-ddump-file-prefix@
  dumpPrefixForce       :: Maybe FilePath,

  ldInputs              :: [Option],

  includePaths          :: IncludeSpecs,
  libraryPaths          :: [String],
  frameworkPaths        :: [String],    -- used on darwin only
  cmdlineFrameworks     :: [String],    -- ditto

  rtsOpts               :: Maybe String,
  rtsOptsEnabled        :: RtsOptsEnabled,
  rtsOptsSuggestions    :: Bool,

  hpcDir                :: String,      -- ^ Path to store the .mix files

  -- Plugins
  pluginModNames        :: [ModuleName],
  pluginModNameOpts     :: [(ModuleName,String)],
  frontendPluginOpts    :: [String],
    -- ^ the @-ffrontend-opt@ flags given on the command line, in *reverse*
    -- order that they're specified on the command line.
  cachedPlugins         :: [LoadedPlugin],
    -- ^ plugins dynamically loaded after processing arguments. What will be
    -- loaded here is directed by pluginModNames. Arguments are loaded from
    -- pluginModNameOpts. The purpose of this field is to cache the plugins so
    -- they don't have to be loaded each time they are needed.  See
    -- 'GHC.Runtime.Loader.initializePlugins'.
  staticPlugins            :: [StaticPlugin],
    -- ^ static plugins which do not need dynamic loading. These plugins are
    -- intended to be added by GHC API users directly to this list.
    --
    -- To add dynamically loaded plugins through the GHC API see
    -- 'addPluginModuleName' instead.

  -- GHC API hooks
  hooks                 :: Hooks,

  --  For ghc -M
  depMakefile           :: FilePath,
  depIncludePkgDeps     :: Bool,
  depIncludeCppDeps     :: Bool,
  depExcludeMods        :: [ModuleName],
  depSuffixes           :: [String],

  --  Package flags
  packageDBFlags        :: [PackageDBFlag],
        -- ^ The @-package-db@ flags given on the command line, In
        -- *reverse* order that they're specified on the command line.
        -- This is intended to be applied with the list of "initial"
        -- package databases derived from @GHC_PACKAGE_PATH@; see
        -- 'getPackageConfRefs'.

  ignorePackageFlags    :: [IgnorePackageFlag],
        -- ^ The @-ignore-package@ flags from the command line.
        -- In *reverse* order that they're specified on the command line.
  packageFlags          :: [PackageFlag],
        -- ^ The @-package@ and @-hide-package@ flags from the command-line.
        -- In *reverse* order that they're specified on the command line.
  pluginPackageFlags    :: [PackageFlag],
        -- ^ The @-plugin-package-id@ flags from command line.
        -- In *reverse* order that they're specified on the command line.
  trustFlags            :: [TrustFlag],
        -- ^ The @-trust@ and @-distrust@ flags.
        -- In *reverse* order that they're specified on the command line.
  packageEnv            :: Maybe FilePath,
        -- ^ Filepath to the package environment file (if overriding default)

  pkgDatabase           :: Maybe [PackageDatabase],
        -- ^ Stack of package databases for the target platform.
        --
        -- A "package database" is a misleading name as it is really a Unit
        -- database (cf Note [The identifier lexicon]).
        --
        -- This field is populated by `initPackages`.
        --
        -- 'Nothing' means the databases have never been read from disk. If
        -- `initPackages` is called again, it doesn't reload the databases from
        -- disk.

  pkgState              :: PackageState,
        -- ^ Consolidated unit database built by 'initPackages' from the package
        -- databases in 'pkgDatabase' and flags ('-ignore-package', etc.).
        --
        -- It also contains mapping from module names to actual Modules.

  -- Temporary files
  -- These have to be IORefs, because the defaultCleanupHandler needs to
  -- know what to clean when an exception happens
  filesToClean          :: IORef FilesToClean,
  dirsToClean           :: IORef (Map FilePath FilePath),
  -- The next available suffix to uniquely name a temp file, updated atomically
  nextTempSuffix        :: IORef Int,

  -- Names of files which were generated from -ddump-to-file; used to
  -- track which ones we need to truncate because it's our first run
  -- through
  generatedDumps        :: IORef (Set FilePath),

  -- hsc dynamic flags
  dumpFlags             :: EnumSet DumpFlag,
  generalFlags          :: EnumSet GeneralFlag,
  warningFlags          :: EnumSet WarningFlag,
  fatalWarningFlags     :: EnumSet WarningFlag,
  -- Don't change this without updating extensionFlags:
  language              :: Maybe Language,
  -- | Safe Haskell mode
  safeHaskell           :: SafeHaskellMode,
  safeInfer             :: Bool,
  safeInferred          :: Bool,
  -- We store the location of where some extension and flags were turned on so
  -- we can produce accurate error messages when Safe Haskell fails due to
  -- them.
  thOnLoc               :: SrcSpan,
  newDerivOnLoc         :: SrcSpan,
  overlapInstLoc        :: SrcSpan,
  incoherentOnLoc       :: SrcSpan,
  pkgTrustOnLoc         :: SrcSpan,
  warnSafeOnLoc         :: SrcSpan,
  warnUnsafeOnLoc       :: SrcSpan,
  trustworthyOnLoc      :: SrcSpan,
  -- Don't change this without updating extensionFlags:
  -- Here we collect the settings of the language extensions
  -- from the command line, the ghci config file and
  -- from interactive :set / :seti commands.
  extensions            :: [OnOff LangExt.Extension],
  -- extensionFlags should always be equal to
  --     flattenExtensionFlags language extensions
  -- LangExt.Extension is defined in libraries/ghc-boot so that it can be used
  -- by template-haskell
  extensionFlags        :: EnumSet LangExt.Extension,

  -- Unfolding control
  -- See Note [Discounts and thresholds] in GHC.Core.Unfold
  ufCreationThreshold   :: Int,
  ufUseThreshold        :: Int,
  ufFunAppDiscount      :: Int,
  ufDictDiscount        :: Int,
  ufKeenessFactor       :: Float,
  ufDearOp              :: Int,
  ufVeryAggressive      :: Bool,

  maxWorkerArgs         :: Int,

  ghciHistSize          :: Int,

  -- | MsgDoc output action: use "ErrUtils" instead of this if you can
  log_action            :: LogAction,
  dump_action           :: DumpAction,
  trace_action          :: TraceAction,
  flushOut              :: FlushOut,
  flushErr              :: FlushErr,

  ghcVersionFile        :: Maybe FilePath,
  haddockOptions        :: Maybe String,

  -- | GHCi scripts specified by -ghci-script, in reverse order
  ghciScripts           :: [String],

  -- Output style options
  pprUserLength         :: Int,
  pprCols               :: Int,

  useUnicode            :: Bool,
  useColor              :: OverridingBool,
  canUseColor           :: Bool,
  colScheme             :: Col.Scheme,

  -- | what kind of {-# SCC #-} to add automatically
  profAuto              :: ProfAuto,

  interactivePrint      :: Maybe String,

  nextWrapperNum        :: IORef (ModuleEnv Int),

  -- | Machine dependent flags (-m<blah> stuff)
  sseVersion            :: Maybe SseVersion,
  bmiVersion            :: Maybe BmiVersion,
  avx                   :: Bool,
  avx2                  :: Bool,
  avx512cd              :: Bool, -- Enable AVX-512 Conflict Detection Instructions.
  avx512er              :: Bool, -- Enable AVX-512 Exponential and Reciprocal Instructions.
  avx512f               :: Bool, -- Enable AVX-512 instructions.
  avx512pf              :: Bool, -- Enable AVX-512 PreFetch Instructions.

  -- | Run-time linker information (what options we need, etc.)
  rtldInfo              :: IORef (Maybe LinkerInfo),

  -- | Run-time compiler information
  rtccInfo              :: IORef (Maybe CompilerInfo),

  -- Constants used to control the amount of optimization done.

  -- | Max size, in bytes, of inline array allocations.
  maxInlineAllocSize    :: Int,

  -- | Only inline memcpy if it generates no more than this many
  -- pseudo (roughly: Cmm) instructions.
  maxInlineMemcpyInsns  :: Int,

  -- | Only inline memset if it generates no more than this many
  -- pseudo (roughly: Cmm) instructions.
  maxInlineMemsetInsns  :: Int,

  -- | Reverse the order of error messages in GHC/GHCi
  reverseErrors         :: Bool,

  -- | Limit the maximum number of errors to show
  maxErrors             :: Maybe Int,

  -- | Unique supply configuration for testing build determinism
  initialUnique         :: Int,
  uniqueIncrement       :: Int,

  -- | Temporary: CFG Edge weights for fast iterations
  cfgWeightInfo         :: CfgWeights
}

-- | Edge weights to use when generating a CFG from CMM
data CfgWeights
    = CFGWeights
    { uncondWeight :: Int
    , condBranchWeight :: Int
    , switchWeight :: Int
    , callWeight :: Int
    , likelyCondWeight :: Int
    , unlikelyCondWeight :: Int
    , infoTablePenalty :: Int
    , backEdgeBonus :: Int
    }

defaultCfgWeights :: CfgWeights
defaultCfgWeights
    = CFGWeights
    { uncondWeight = 1000
    , condBranchWeight = 800
    , switchWeight = 1
    , callWeight = -10
    , likelyCondWeight = 900
    , unlikelyCondWeight = 300
    , infoTablePenalty = 300
    , backEdgeBonus = 400
    }

parseCfgWeights :: String -> CfgWeights -> CfgWeights
parseCfgWeights s oldWeights =
        foldl' (\cfg (n,v) -> update n v cfg) oldWeights assignments
    where
        assignments = map assignment $ settings s
        update "uncondWeight" n w =
            w {uncondWeight = n}
        update "condBranchWeight" n w =
            w {condBranchWeight = n}
        update "switchWeight" n w =
            w {switchWeight = n}
        update "callWeight" n w =
            w {callWeight = n}
        update "likelyCondWeight" n w =
            w {likelyCondWeight = n}
        update "unlikelyCondWeight" n w =
            w {unlikelyCondWeight = n}
        update "infoTablePenalty" n w =
            w {infoTablePenalty = n}
        update "backEdgeBonus" n w =
            w {backEdgeBonus = n}
        update other _ _
            = panic $ other ++
                      " is not a cfg weight parameter. " ++
                      exampleString
        settings s
            | (s1,rest) <- break (== ',') s
            , null rest
            = [s1]
            | (s1,rest) <- break (== ',') s
            = s1 : settings (drop 1 rest)

        assignment as
            | (name, _:val) <- break (== '=') as
            = (name,read val)
            | otherwise
            = panic $ "Invalid cfg parameters." ++ exampleString

        exampleString = "Example parameters: uncondWeight=1000," ++
            "condBranchWeight=800,switchWeight=0,callWeight=300" ++
            ",likelyCondWeight=900,unlikelyCondWeight=300" ++
            ",infoTablePenalty=300,backEdgeBonus=400"

class HasDynFlags m where
    getDynFlags :: m DynFlags

{- It would be desirable to have the more generalised

  instance (MonadTrans t, Monad m, HasDynFlags m) => HasDynFlags (t m) where
      getDynFlags = lift getDynFlags

instance definition. However, that definition would overlap with the
`HasDynFlags (GhcT m)` instance. Instead we define instances for a
couple of common Monad transformers explicitly. -}

instance (Monoid a, Monad m, HasDynFlags m) => HasDynFlags (WriterT a m) where
    getDynFlags = lift getDynFlags

instance (Monad m, HasDynFlags m) => HasDynFlags (ReaderT a m) where
    getDynFlags = lift getDynFlags

instance (Monad m, HasDynFlags m) => HasDynFlags (MaybeT m) where
    getDynFlags = lift getDynFlags

instance (Monad m, HasDynFlags m) => HasDynFlags (ExceptT e m) where
    getDynFlags = lift getDynFlags

class ContainsDynFlags t where
    extractDynFlags :: t -> DynFlags

data ProfAuto
  = NoProfAuto         -- ^ no SCC annotations added
  | ProfAutoAll        -- ^ top-level and nested functions are annotated
  | ProfAutoTop        -- ^ top-level functions annotated only
  | ProfAutoExports    -- ^ exported functions annotated only
  | ProfAutoCalls      -- ^ annotate call-sites
  deriving (Eq,Enum)

data LlvmTarget = LlvmTarget
  { lDataLayout :: String
  , lCPU        :: String
  , lAttributes :: [String]
  }

-- | See Note [LLVM Configuration] in SysTools.
data LlvmConfig = LlvmConfig { llvmTargets :: [(String, LlvmTarget)]
                             , llvmPasses  :: [(Int, String)]
                             }

-----------------------------------------------------------------------------
-- Accessessors from 'DynFlags'

-- | "unbuild" a 'Settings' from a 'DynFlags'. This shouldn't be needed in the
-- vast majority of code. But GHCi questionably uses this to produce a default
-- 'DynFlags' from which to compute a flags diff for printing.
settings :: DynFlags -> Settings
settings dflags = Settings
  { sGhcNameVersion = ghcNameVersion dflags
  , sFileSettings = fileSettings dflags
  , sTargetPlatform = targetPlatform dflags
  , sToolSettings = toolSettings dflags
  , sPlatformMisc = platformMisc dflags
  , sPlatformConstants = platformConstants dflags
  , sRawSettings = rawSettings dflags
  }

programName :: DynFlags -> String
programName dflags = ghcNameVersion_programName $ ghcNameVersion dflags
projectVersion :: DynFlags -> String
projectVersion dflags = ghcNameVersion_projectVersion (ghcNameVersion dflags)
ghcUsagePath          :: DynFlags -> FilePath
ghcUsagePath dflags = fileSettings_ghcUsagePath $ fileSettings dflags
ghciUsagePath         :: DynFlags -> FilePath
ghciUsagePath dflags = fileSettings_ghciUsagePath $ fileSettings dflags
toolDir               :: DynFlags -> Maybe FilePath
toolDir dflags = fileSettings_toolDir $ fileSettings dflags
topDir                :: DynFlags -> FilePath
topDir dflags = fileSettings_topDir $ fileSettings dflags
tmpDir                :: DynFlags -> String
tmpDir dflags = fileSettings_tmpDir $ fileSettings dflags
extraGccViaCFlags     :: DynFlags -> [String]
extraGccViaCFlags dflags = toolSettings_extraGccViaCFlags $ toolSettings dflags
globalPackageDatabasePath   :: DynFlags -> FilePath
globalPackageDatabasePath dflags = fileSettings_globalPackageDatabase $ fileSettings dflags
pgm_L                 :: DynFlags -> String
pgm_L dflags = toolSettings_pgm_L $ toolSettings dflags
pgm_P                 :: DynFlags -> (String,[Option])
pgm_P dflags = toolSettings_pgm_P $ toolSettings dflags
pgm_F                 :: DynFlags -> String
pgm_F dflags = toolSettings_pgm_F $ toolSettings dflags
pgm_c                 :: DynFlags -> String
pgm_c dflags = toolSettings_pgm_c $ toolSettings dflags
pgm_a                 :: DynFlags -> (String,[Option])
pgm_a dflags = toolSettings_pgm_a $ toolSettings dflags
pgm_l                 :: DynFlags -> (String,[Option])
pgm_l dflags = toolSettings_pgm_l $ toolSettings dflags
pgm_dll               :: DynFlags -> (String,[Option])
pgm_dll dflags = toolSettings_pgm_dll $ toolSettings dflags
pgm_T                 :: DynFlags -> String
pgm_T dflags = toolSettings_pgm_T $ toolSettings dflags
pgm_windres           :: DynFlags -> String
pgm_windres dflags = toolSettings_pgm_windres $ toolSettings dflags
pgm_libtool           :: DynFlags -> String
pgm_libtool dflags = toolSettings_pgm_libtool $ toolSettings dflags
pgm_lcc               :: DynFlags -> (String,[Option])
pgm_lcc dflags = toolSettings_pgm_lcc $ toolSettings dflags
pgm_ar                :: DynFlags -> String
pgm_ar dflags = toolSettings_pgm_ar $ toolSettings dflags
pgm_ranlib            :: DynFlags -> String
pgm_ranlib dflags = toolSettings_pgm_ranlib $ toolSettings dflags
pgm_lo                :: DynFlags -> (String,[Option])
pgm_lo dflags = toolSettings_pgm_lo $ toolSettings dflags
pgm_lc                :: DynFlags -> (String,[Option])
pgm_lc dflags = toolSettings_pgm_lc $ toolSettings dflags
pgm_i                 :: DynFlags -> String
pgm_i dflags = toolSettings_pgm_i $ toolSettings dflags
opt_L                 :: DynFlags -> [String]
opt_L dflags = toolSettings_opt_L $ toolSettings dflags
opt_P                 :: DynFlags -> [String]
opt_P dflags = concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_P (toolSettings dflags)

-- This function packages everything that's needed to fingerprint opt_P
-- flags. See Note [Repeated -optP hashing].
opt_P_signature       :: DynFlags -> ([String], Fingerprint)
opt_P_signature dflags =
  ( concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
  , toolSettings_opt_P_fingerprint $ toolSettings dflags
  )

opt_F                 :: DynFlags -> [String]
opt_F dflags= toolSettings_opt_F $ toolSettings dflags
opt_c                 :: DynFlags -> [String]
opt_c dflags = concatMap (wayOptc (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_c (toolSettings dflags)
opt_cxx               :: DynFlags -> [String]
opt_cxx dflags= toolSettings_opt_cxx $ toolSettings dflags
opt_a                 :: DynFlags -> [String]
opt_a dflags= toolSettings_opt_a $ toolSettings dflags
opt_l                 :: DynFlags -> [String]
opt_l dflags = concatMap (wayOptl (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_l (toolSettings dflags)
opt_windres           :: DynFlags -> [String]
opt_windres dflags= toolSettings_opt_windres $ toolSettings dflags
opt_lcc                :: DynFlags -> [String]
opt_lcc dflags= toolSettings_opt_lcc $ toolSettings dflags
opt_lo                :: DynFlags -> [String]
opt_lo dflags= toolSettings_opt_lo $ toolSettings dflags
opt_lc                :: DynFlags -> [String]
opt_lc dflags= toolSettings_opt_lc $ toolSettings dflags
opt_i                 :: DynFlags -> [String]
opt_i dflags= toolSettings_opt_i $ toolSettings dflags

tablesNextToCode :: DynFlags -> Bool
tablesNextToCode = platformMisc_tablesNextToCode . platformMisc

-- | The directory for this version of ghc in the user's app directory
-- (typically something like @~/.ghc/x86_64-linux-7.6.3@)
--
versionedAppDir :: DynFlags -> MaybeT IO FilePath
versionedAppDir dflags = do
  -- Make sure we handle the case the HOME isn't set (see #11678)
  appdir <- tryMaybeT $ getAppUserDataDirectory (programName dflags)
  return $ appdir </> versionedFilePath dflags

versionedFilePath :: DynFlags -> FilePath
versionedFilePath dflags = uniqueSubdir $ platformMini $ targetPlatform dflags

-- | The target code type of the compilation (if any).
--
-- Whenever you change the target, also make sure to set 'ghcLink' to
-- something sensible.
--
-- 'HscNothing' can be used to avoid generating any output, however, note
-- that:
--
--  * If a program uses Template Haskell the typechecker may need to run code
--    from an imported module.  To facilitate this, code generation is enabled
--    for modules imported by modules that use template haskell.
--    See Note [-fno-code mode].
--
data HscTarget
  = HscC           -- ^ Generate C code.
  | HscAsm         -- ^ Generate assembly using the native code generator.
  | HscLlvm        -- ^ Generate assembly using the llvm code generator.
  | HscInterpreted -- ^ Generate bytecode.  (Requires 'LinkInMemory')
  | HscNothing     -- ^ Don't generate any code.  See notes above.
  deriving (Eq, Show)

-- | Will this target result in an object file on the disk?
isObjectTarget :: HscTarget -> Bool
isObjectTarget HscC     = True
isObjectTarget HscAsm   = True
isObjectTarget HscLlvm  = True
isObjectTarget _        = False

-- | Does this target retain *all* top-level bindings for a module,
-- rather than just the exported bindings, in the TypeEnv and compiled
-- code (if any)?  In interpreted mode we do this, so that GHCi can
-- call functions inside a module.  In HscNothing mode we also do it,
-- so that Haddock can get access to the GlobalRdrEnv for a module
-- after typechecking it.
targetRetainsAllBindings :: HscTarget -> Bool
targetRetainsAllBindings HscInterpreted = True
targetRetainsAllBindings HscNothing     = True
targetRetainsAllBindings _              = False

-- | The 'GhcMode' tells us whether we're doing multi-module
-- compilation (controlled via the "GHC" API) or one-shot
-- (single-module) compilation.  This makes a difference primarily to
-- the "Finder": in one-shot mode we look for interface files for
-- imported modules, but in multi-module mode we look for source files
-- in order to check whether they need to be recompiled.
data GhcMode
  = CompManager         -- ^ @\-\-make@, GHCi, etc.
  | OneShot             -- ^ @ghc -c Foo.hs@
  | MkDepend            -- ^ @ghc -M@, see "Finder" for why we need this
  deriving Eq

instance Outputable GhcMode where
  ppr CompManager = text "CompManager"
  ppr OneShot     = text "OneShot"
  ppr MkDepend    = text "MkDepend"

isOneShot :: GhcMode -> Bool
isOneShot OneShot = True
isOneShot _other  = False

-- | What to do in the link step, if there is one.
data GhcLink
  = NoLink              -- ^ Don't link at all
  | LinkBinary          -- ^ Link object code into a binary
  | LinkInMemory        -- ^ Use the in-memory dynamic linker (works for both
                        --   bytecode and object code).
  | LinkDynLib          -- ^ Link objects into a dynamic lib (DLL on Windows, DSO on ELF platforms)
  | LinkStaticLib       -- ^ Link objects into a static lib
  deriving (Eq, Show)

isNoLink :: GhcLink -> Bool
isNoLink NoLink = True
isNoLink _      = False

-- | We accept flags which make packages visible, but how they select
-- the package varies; this data type reflects what selection criterion
-- is used.
data PackageArg =
      PackageArg String    -- ^ @-package@, by 'PackageName'
    | UnitIdArg UnitId     -- ^ @-package-id@, by 'UnitId'
  deriving (Eq, Show)
instance Outputable PackageArg where
    ppr (PackageArg pn) = text "package" <+> text pn
    ppr (UnitIdArg uid) = text "unit" <+> ppr uid

-- | Represents the renaming that may be associated with an exposed
-- package, e.g. the @rns@ part of @-package "foo (rns)"@.
--
-- Here are some example parsings of the package flags (where
-- a string literal is punned to be a 'ModuleName':
--
--      * @-package foo@ is @ModRenaming True []@
--      * @-package foo ()@ is @ModRenaming False []@
--      * @-package foo (A)@ is @ModRenaming False [("A", "A")]@
--      * @-package foo (A as B)@ is @ModRenaming False [("A", "B")]@
--      * @-package foo with (A as B)@ is @ModRenaming True [("A", "B")]@
data ModRenaming = ModRenaming {
    modRenamingWithImplicit :: Bool, -- ^ Bring all exposed modules into scope?
    modRenamings :: [(ModuleName, ModuleName)] -- ^ Bring module @m@ into scope
                                               --   under name @n@.
  } deriving (Eq)
instance Outputable ModRenaming where
    ppr (ModRenaming b rns) = ppr b <+> parens (ppr rns)

-- | Flags for manipulating the set of non-broken packages.
newtype IgnorePackageFlag = IgnorePackage String -- ^ @-ignore-package@
  deriving (Eq)

-- | Flags for manipulating package trust.
data TrustFlag
  = TrustPackage    String -- ^ @-trust@
  | DistrustPackage String -- ^ @-distrust@
  deriving (Eq)

-- | Flags for manipulating packages visibility.
data PackageFlag
  = ExposePackage   String PackageArg ModRenaming -- ^ @-package@, @-package-id@
  | HidePackage     String -- ^ @-hide-package@
  deriving (Eq) -- NB: equality instance is used by packageFlagsChanged

data PackageDBFlag
  = PackageDB PkgDbRef
  | NoUserPackageDB
  | NoGlobalPackageDB
  | ClearPackageDBs
  deriving (Eq)

packageFlagsChanged :: DynFlags -> DynFlags -> Bool
packageFlagsChanged idflags1 idflags0 =
  packageFlags idflags1 /= packageFlags idflags0 ||
  ignorePackageFlags idflags1 /= ignorePackageFlags idflags0 ||
  pluginPackageFlags idflags1 /= pluginPackageFlags idflags0 ||
  trustFlags idflags1 /= trustFlags idflags0 ||
  packageDBFlags idflags1 /= packageDBFlags idflags0 ||
  packageGFlags idflags1 /= packageGFlags idflags0
 where
   packageGFlags dflags = map (`gopt` dflags)
     [ Opt_HideAllPackages
     , Opt_HideAllPluginPackages
     , Opt_AutoLinkPackages ]

instance Outputable PackageFlag where
    ppr (ExposePackage n arg rn) = text n <> braces (ppr arg <+> ppr rn)
    ppr (HidePackage str) = text "-hide-package" <+> text str

-- | The 'HscTarget' value corresponding to the default way to create
-- object files on the current platform.

defaultHscTarget :: Platform -> PlatformMisc -> HscTarget
defaultHscTarget platform pMisc
  | platformUnregisterised platform = HscC
  | platformMisc_ghcWithNativeCodeGen pMisc = HscAsm
  | otherwise = HscLlvm

defaultObjectTarget :: DynFlags -> HscTarget
defaultObjectTarget dflags = defaultHscTarget
  (targetPlatform dflags)
  (platformMisc dflags)

data DynLibLoader
  = Deployable
  | SystemDependent
  deriving Eq

data RtsOptsEnabled
  = RtsOptsNone | RtsOptsIgnore | RtsOptsIgnoreAll | RtsOptsSafeOnly
  | RtsOptsAll
  deriving (Show)

-- | Are we building with @-fPIE@ or @-fPIC@ enabled?
positionIndependent :: DynFlags -> Bool
positionIndependent dflags = gopt Opt_PIC dflags || gopt Opt_PIE dflags

whenGeneratingDynamicToo :: MonadIO m => DynFlags -> m () -> m ()
whenGeneratingDynamicToo dflags f = ifGeneratingDynamicToo dflags f (return ())

ifGeneratingDynamicToo :: MonadIO m => DynFlags -> m a -> m a -> m a
ifGeneratingDynamicToo dflags f g = generateDynamicTooConditional dflags f g g

whenCannotGenerateDynamicToo :: MonadIO m => DynFlags -> m () -> m ()
whenCannotGenerateDynamicToo dflags f
    = ifCannotGenerateDynamicToo dflags f (return ())

ifCannotGenerateDynamicToo :: MonadIO m => DynFlags -> m a -> m a -> m a
ifCannotGenerateDynamicToo dflags f g
    = generateDynamicTooConditional dflags g f g

generateDynamicTooConditional :: MonadIO m
                              => DynFlags -> m a -> m a -> m a -> m a
generateDynamicTooConditional dflags canGen cannotGen notTryingToGen
    = if gopt Opt_BuildDynamicToo dflags
      then do let ref = canGenerateDynamicToo dflags
              b <- liftIO $ readIORef ref
              if b then canGen else cannotGen
      else notTryingToGen

dynamicTooMkDynamicDynFlags :: DynFlags -> DynFlags
dynamicTooMkDynamicDynFlags dflags0
    = let dflags1 = addWay' WayDyn dflags0
          dflags2 = dflags1 {
                        outputFile = dynOutputFile dflags1,
                        hiSuf = dynHiSuf dflags1,
                        objectSuf = dynObjectSuf dflags1
                    }
          dflags3 = updateWays dflags2
          dflags4 = gopt_unset dflags3 Opt_BuildDynamicToo
      in dflags4

-- | Compute the path of the dynamic object corresponding to an object file.
dynamicOutputFile :: DynFlags -> FilePath -> FilePath
dynamicOutputFile dflags outputFile = dynOut outputFile
  where
    dynOut = flip addExtension (dynObjectSuf dflags) . dropExtension

-----------------------------------------------------------------------------

-- | Used by 'GHC.runGhc' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 let -- We can't build with dynamic-too on Windows, as labels before
     -- the fork point are different depending on whether we are
     -- building dynamically or not.
     platformCanGenerateDynamicToo
         = platformOS (targetPlatform dflags) /= OSMinGW32
 refCanGenerateDynamicToo <- newIORef platformCanGenerateDynamicToo
 refNextTempSuffix <- newIORef 0
 refFilesToClean <- newIORef emptyFilesToClean
 refDirsToClean <- newIORef Map.empty
 refGeneratedDumps <- newIORef Set.empty
 refRtldInfo <- newIORef Nothing
 refRtccInfo <- newIORef Nothing
 wrapperNum <- newIORef emptyModuleEnv
 canUseUnicode <- do let enc = localeEncoding
                         str = "‘’"
                     (withCString enc str $ \cstr ->
                          do str' <- peekCString enc cstr
                             return (str == str'))
                         `catchIOError` \_ -> return False
 ghcNoUnicodeEnv <- lookupEnv "GHC_NO_UNICODE"
 let useUnicode' = isNothing ghcNoUnicodeEnv && canUseUnicode
 maybeGhcColorsEnv  <- lookupEnv "GHC_COLORS"
 maybeGhcColoursEnv <- lookupEnv "GHC_COLOURS"
 let adjustCols (Just env) = Col.parseScheme env
     adjustCols Nothing    = id
 let (useColor', colScheme') =
       (adjustCols maybeGhcColoursEnv . adjustCols maybeGhcColorsEnv)
       (useColor dflags, colScheme dflags)
 return dflags{
        canGenerateDynamicToo = refCanGenerateDynamicToo,
        nextTempSuffix = refNextTempSuffix,
        filesToClean   = refFilesToClean,
        dirsToClean    = refDirsToClean,
        generatedDumps = refGeneratedDumps,
        nextWrapperNum = wrapperNum,
        useUnicode    = useUnicode',
        useColor      = useColor',
        canUseColor   = stderrSupportsAnsiColors,
        colScheme     = colScheme',
        rtldInfo      = refRtldInfo,
        rtccInfo      = refRtccInfo
        }

-- | The normal 'DynFlags'. Note that they are not suitable for use in this form
-- and must be fully initialized by 'GHC.runGhc' first.
defaultDynFlags :: Settings -> LlvmConfig -> DynFlags
defaultDynFlags mySettings llvmConfig =
-- See Note [Updating flag description in the User's Guide]
     DynFlags {
        ghcMode                 = CompManager,
        ghcLink                 = LinkBinary,
        hscTarget               = defaultHscTarget (sTargetPlatform mySettings) (sPlatformMisc mySettings),
        integerLibrary          = sIntegerLibraryType mySettings,
        verbosity               = 0,
        optLevel                = 0,
        debugLevel              = 0,
        simplPhases             = 2,
        maxSimplIterations      = 4,
        ruleCheck               = Nothing,
        inlineCheck             = Nothing,
        binBlobThreshold        = 500000, -- 500K is a good default (see #16190)
        maxRelevantBinds        = Just 6,
        maxValidHoleFits   = Just 6,
        maxRefHoleFits     = Just 6,
        refLevelHoleFits   = Nothing,
        maxUncoveredPatterns    = 4,
        maxPmCheckModels        = 30,
        simplTickFactor         = 100,
        specConstrThreshold     = Just 2000,
        specConstrCount         = Just 3,
        specConstrRecursive     = 3,
        liberateCaseThreshold   = Just 2000,
        floatLamArgs            = Just 0, -- Default: float only if no fvs
        liftLamsRecArgs         = Just 5, -- Default: the number of available argument hardware registers on x86_64
        liftLamsNonRecArgs      = Just 5, -- Default: the number of available argument hardware registers on x86_64
        liftLamsKnown           = False,  -- Default: don't turn known calls into unknown ones
        cmmProcAlignment        = Nothing,

        historySize             = 20,
        strictnessBefore        = [],

        parMakeCount            = Just 1,

        enableTimeStats         = False,
        ghcHeapSize             = Nothing,

        importPaths             = ["."],
        mainModIs               = mAIN,
        mainFunIs               = Nothing,
        reductionDepth          = treatZeroAsInf mAX_REDUCTION_DEPTH,
        solverIterations        = treatZeroAsInf mAX_SOLVER_ITERATIONS,

        thisInstalledUnitId     = toInstalledUnitId mainUnitId,
        thisUnitIdInsts_        = Nothing,
        thisComponentId_        = Nothing,

        objectDir               = Nothing,
        dylibInstallName        = Nothing,
        hiDir                   = Nothing,
        hieDir                  = Nothing,
        stubDir                 = Nothing,
        dumpDir                 = Nothing,

        objectSuf               = phaseInputExt StopLn,
        hcSuf                   = phaseInputExt HCc,
        hiSuf                   = "hi",
        hieSuf                  = "hie",

        canGenerateDynamicToo   = panic "defaultDynFlags: No canGenerateDynamicToo",
        dynObjectSuf            = "dyn_" ++ phaseInputExt StopLn,
        dynHiSuf                = "dyn_hi",

        pluginModNames          = [],
        pluginModNameOpts       = [],
        frontendPluginOpts      = [],
        cachedPlugins           = [],
        staticPlugins           = [],
        hooks                   = emptyHooks,

        outputFile              = Nothing,
        dynOutputFile           = Nothing,
        outputHi                = Nothing,
        dynLibLoader            = SystemDependent,
        dumpPrefix              = Nothing,
        dumpPrefixForce         = Nothing,
        ldInputs                = [],
        includePaths            = IncludeSpecs [] [],
        libraryPaths            = [],
        frameworkPaths          = [],
        cmdlineFrameworks       = [],
        rtsOpts                 = Nothing,
        rtsOptsEnabled          = RtsOptsSafeOnly,
        rtsOptsSuggestions      = True,

        hpcDir                  = ".hpc",

        packageDBFlags          = [],
        packageFlags            = [],
        pluginPackageFlags      = [],
        ignorePackageFlags      = [],
        trustFlags              = [],
        packageEnv              = Nothing,
        pkgDatabase             = Nothing,
        pkgState                = emptyPackageState,
        ways                    = defaultWays mySettings,
        buildTag                = waysTag (defaultWays mySettings),
        splitInfo               = Nothing,

        ghcNameVersion = sGhcNameVersion mySettings,
        fileSettings = sFileSettings mySettings,
        toolSettings = sToolSettings mySettings,
        targetPlatform = sTargetPlatform mySettings,
        platformMisc = sPlatformMisc mySettings,
        platformConstants = sPlatformConstants mySettings,
        rawSettings = sRawSettings mySettings,

        -- See Note [LLVM configuration].
        llvmConfig              = llvmConfig,

        -- ghc -M values
        depMakefile       = "Makefile",
        depIncludePkgDeps = False,
        depIncludeCppDeps = False,
        depExcludeMods    = [],
        depSuffixes       = [],
        -- end of ghc -M values
        nextTempSuffix = panic "defaultDynFlags: No nextTempSuffix",
        filesToClean   = panic "defaultDynFlags: No filesToClean",
        dirsToClean    = panic "defaultDynFlags: No dirsToClean",
        generatedDumps = panic "defaultDynFlags: No generatedDumps",
        ghcVersionFile = Nothing,
        haddockOptions = Nothing,
        dumpFlags = EnumSet.empty,
        generalFlags = EnumSet.fromList (defaultFlags mySettings),
        warningFlags = EnumSet.fromList standardWarnings,
        fatalWarningFlags = EnumSet.empty,
        ghciScripts = [],
        language = Nothing,
        safeHaskell = Sf_None,
        safeInfer   = True,
        safeInferred = True,
        thOnLoc = noSrcSpan,
        newDerivOnLoc = noSrcSpan,
        overlapInstLoc = noSrcSpan,
        incoherentOnLoc = noSrcSpan,
        pkgTrustOnLoc = noSrcSpan,
        warnSafeOnLoc = noSrcSpan,
        warnUnsafeOnLoc = noSrcSpan,
        trustworthyOnLoc = noSrcSpan,
        extensions = [],
        extensionFlags = flattenExtensionFlags Nothing [],

        -- The ufCreationThreshold threshold must be reasonably high to
        -- take account of possible discounts.
        -- E.g. 450 is not enough in 'fulsom' for Interval.sqr to inline
        -- into Csg.calc (The unfolding for sqr never makes it into the
        -- interface file.)
        ufCreationThreshold = 750,
        ufUseThreshold      = 60,
        ufFunAppDiscount    = 60,
        -- Be fairly keen to inline a function if that means
        -- we'll be able to pick the right method from a dictionary
        ufDictDiscount      = 30,
        ufKeenessFactor     = 1.5,
        ufDearOp            = 40,
        ufVeryAggressive    = False,

        maxWorkerArgs = 10,

        ghciHistSize = 50, -- keep a log of length 50 by default

        -- Logging

        log_action   = defaultLogAction,
        dump_action  = defaultDumpAction,
        trace_action = defaultTraceAction,

        flushOut = defaultFlushOut,
        flushErr = defaultFlushErr,
        pprUserLength = 5,
        pprCols = 100,
        useUnicode = False,
        useColor = Auto,
        canUseColor = False,
        colScheme = Col.defaultScheme,
        profAuto = NoProfAuto,
        interactivePrint = Nothing,
        nextWrapperNum = panic "defaultDynFlags: No nextWrapperNum",
        sseVersion = Nothing,
        bmiVersion = Nothing,
        avx = False,
        avx2 = False,
        avx512cd = False,
        avx512er = False,
        avx512f = False,
        avx512pf = False,
        rtldInfo = panic "defaultDynFlags: no rtldInfo",
        rtccInfo = panic "defaultDynFlags: no rtccInfo",

        maxInlineAllocSize = 128,
        maxInlineMemcpyInsns = 32,
        maxInlineMemsetInsns = 32,

        initialUnique = 0,
        uniqueIncrement = 1,

        reverseErrors = False,
        maxErrors     = Nothing,
        cfgWeightInfo = defaultCfgWeights
      }

defaultWays :: Settings -> Set Way
defaultWays settings = if pc_DYNAMIC_BY_DEFAULT (sPlatformConstants settings)
                       then Set.singleton WayDyn
                       else Set.empty

--------------------------------------------------------------------------
--
-- Note [JSON Error Messages]
--
-- When the user requests the compiler output to be dumped as json
-- we used to collect them all in an IORef and then print them at the end.
-- This doesn't work very well with GHCi. (See #14078) So instead we now
-- use the simpler method of just outputting a JSON document inplace to
-- stdout.
--
-- Before the compiler calls log_action, it has already turned the `ErrMsg`
-- into a formatted message. This means that we lose some possible
-- information to provide to the user but refactoring log_action is quite
-- invasive as it is called in many places. So, for now I left it alone
-- and we can refine its behaviour as users request different output.

type FatalMessager = String -> IO ()

type LogAction = DynFlags
              -> WarnReason
              -> Severity
              -> SrcSpan
              -> PprStyle
              -> MsgDoc
              -> IO ()

defaultFatalMessager :: FatalMessager
defaultFatalMessager = hPutStrLn stderr


-- See Note [JSON Error Messages]
--
jsonLogAction :: LogAction
jsonLogAction dflags reason severity srcSpan _style msg
  = do
    defaultLogActionHPutStrDoc dflags stdout (doc $$ text "")
                               (mkCodeStyle CStyle)
    where
      doc = renderJSON $
              JSObject [ ( "span", json srcSpan )
                       , ( "doc" , JSString (showSDoc dflags msg) )
                       , ( "severity", json severity )
                       , ( "reason" ,   json reason )
                       ]


defaultLogAction :: LogAction
defaultLogAction dflags reason severity srcSpan style msg
    = case severity of
      SevOutput      -> printOut msg style
      SevDump        -> printOut (msg $$ blankLine) style
      SevInteractive -> putStrSDoc msg style
      SevInfo        -> printErrs msg style
      SevFatal       -> printErrs msg style
      SevWarning     -> printWarns
      SevError       -> printWarns
    where
      printOut   = defaultLogActionHPrintDoc  dflags stdout
      printErrs  = defaultLogActionHPrintDoc  dflags stderr
      putStrSDoc = defaultLogActionHPutStrDoc dflags stdout
      -- Pretty print the warning flag, if any (#10752)
      message = mkLocMessageAnn flagMsg severity srcSpan msg

      printWarns = do
        hPutChar stderr '\n'
        caretDiagnostic <-
            if gopt Opt_DiagnosticsShowCaret dflags
            then getCaretDiagnostic severity srcSpan
            else pure empty
        printErrs (message $+$ caretDiagnostic)
            (setStyleColoured True style)
        -- careful (#2302): printErrs prints in UTF-8,
        -- whereas converting to string first and using
        -- hPutStr would just emit the low 8 bits of
        -- each unicode char.

      flagMsg =
        case reason of
          NoReason -> Nothing
          Reason wflag -> do
            spec <- flagSpecOf wflag
            return ("-W" ++ flagSpecName spec ++ warnFlagGrp wflag)
          ErrReason Nothing ->
            return "-Werror"
          ErrReason (Just wflag) -> do
            spec <- flagSpecOf wflag
            return $
              "-W" ++ flagSpecName spec ++ warnFlagGrp wflag ++
              ", -Werror=" ++ flagSpecName spec

      warnFlagGrp flag
          | gopt Opt_ShowWarnGroups dflags =
                case smallestGroups flag of
                    [] -> ""
                    groups -> " (in " ++ intercalate ", " (map ("-W"++) groups) ++ ")"
          | otherwise = ""

-- | Like 'defaultLogActionHPutStrDoc' but appends an extra newline.
defaultLogActionHPrintDoc :: DynFlags -> Handle -> SDoc -> PprStyle -> IO ()
defaultLogActionHPrintDoc dflags h d sty
 = defaultLogActionHPutStrDoc dflags h (d $$ text "") sty

defaultLogActionHPutStrDoc :: DynFlags -> Handle -> SDoc -> PprStyle -> IO ()
defaultLogActionHPutStrDoc dflags h d sty
  -- Don't add a newline at the end, so that successive
  -- calls to this log-action can output all on the same line
  = printSDoc Pretty.PageMode dflags h sty d

newtype FlushOut = FlushOut (IO ())

defaultFlushOut :: FlushOut
defaultFlushOut = FlushOut $ hFlush stdout

newtype FlushErr = FlushErr (IO ())

defaultFlushErr :: FlushErr
defaultFlushErr = FlushErr $ hFlush stderr

{-
Note [Verbosity levels]
~~~~~~~~~~~~~~~~~~~~~~~
    0   |   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"
-}

data OnOff a = On a
             | Off a
  deriving (Eq, Show)

instance Outputable a => Outputable (OnOff a) where
  ppr (On x)  = text "On" <+> ppr x
  ppr (Off x) = text "Off" <+> ppr x

-- OnOffs accumulate in reverse order, so we use foldr in order to
-- process them in the right order
flattenExtensionFlags :: Maybe Language -> [OnOff LangExt.Extension] -> EnumSet LangExt.Extension
flattenExtensionFlags ml = foldr f defaultExtensionFlags
    where f (On f)  flags = EnumSet.insert f flags
          f (Off f) flags = EnumSet.delete f flags
          defaultExtensionFlags = EnumSet.fromList (languageExtensions ml)

-- | The language extensions implied by the various language variants.
-- When updating this be sure to update the flag documentation in
-- @docs/users-guide/glasgow_exts.rst@.
languageExtensions :: Maybe Language -> [LangExt.Extension]

languageExtensions Nothing
    -- Nothing => the default case
    = LangExt.NondecreasingIndentation -- This has been on by default for some time
    : delete LangExt.DatatypeContexts  -- The Haskell' committee decided to
                                       -- remove datatype contexts from the
                                       -- language:
   -- http://www.haskell.org/pipermail/haskell-prime/2011-January/003335.html
      (languageExtensions (Just Haskell2010))

   -- NB: MonoPatBinds is no longer the default

languageExtensions (Just Haskell98)
    = [LangExt.ImplicitPrelude,
       -- See Note [When is StarIsType enabled]
       LangExt.StarIsType,
       LangExt.CUSKs,
       LangExt.MonomorphismRestriction,
       LangExt.NPlusKPatterns,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.NondecreasingIndentation
           -- strictly speaking non-standard, but we always had this
           -- on implicitly before the option was added in 7.1, and
           -- turning it off breaks code, so we're keeping it on for
           -- backwards compatibility.  Cabal uses -XHaskell98 by
           -- default unless you specify another language.
      ]

languageExtensions (Just Haskell2010)
    = [LangExt.ImplicitPrelude,
       -- See Note [When is StarIsType enabled]
       LangExt.StarIsType,
       LangExt.CUSKs,
       LangExt.MonomorphismRestriction,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.EmptyDataDecls,
       LangExt.ForeignFunctionInterface,
       LangExt.PatternGuards,
       LangExt.DoAndIfThenElse,
       LangExt.RelaxedPolyRec]

hasPprDebug :: DynFlags -> Bool
hasPprDebug = dopt Opt_D_ppr_debug

hasNoDebugOutput :: DynFlags -> Bool
hasNoDebugOutput = dopt Opt_D_no_debug_output

hasNoStateHack :: DynFlags -> Bool
hasNoStateHack = gopt Opt_G_NoStateHack

hasNoOptCoercion :: DynFlags -> Bool
hasNoOptCoercion = gopt Opt_G_NoOptCoercion


-- | Test whether a 'DumpFlag' is set
dopt :: DumpFlag -> DynFlags -> Bool
dopt f dflags = (f `EnumSet.member` dumpFlags dflags)
             || (verbosity dflags >= 4 && enableIfVerbose f)
    where enableIfVerbose Opt_D_dump_tc_trace               = False
          enableIfVerbose Opt_D_dump_rn_trace               = False
          enableIfVerbose Opt_D_dump_cs_trace               = False
          enableIfVerbose Opt_D_dump_if_trace               = False
          enableIfVerbose Opt_D_dump_vt_trace               = False
          enableIfVerbose Opt_D_dump_tc                     = False
          enableIfVerbose Opt_D_dump_rn                     = False
          enableIfVerbose Opt_D_dump_rn_stats               = False
          enableIfVerbose Opt_D_dump_hi_diffs               = False
          enableIfVerbose Opt_D_verbose_core2core           = False
          enableIfVerbose Opt_D_verbose_stg2stg             = False
          enableIfVerbose Opt_D_dump_splices                = False
          enableIfVerbose Opt_D_th_dec_file                 = False
          enableIfVerbose Opt_D_dump_rule_firings           = False
          enableIfVerbose Opt_D_dump_rule_rewrites          = False
          enableIfVerbose Opt_D_dump_simpl_trace            = False
          enableIfVerbose Opt_D_dump_rtti                   = False
          enableIfVerbose Opt_D_dump_inlinings              = False
          enableIfVerbose Opt_D_dump_core_stats             = False
          enableIfVerbose Opt_D_dump_asm_stats              = False
          enableIfVerbose Opt_D_dump_types                  = False
          enableIfVerbose Opt_D_dump_simpl_iterations       = False
          enableIfVerbose Opt_D_dump_ticked                 = False
          enableIfVerbose Opt_D_dump_view_pattern_commoning = False
          enableIfVerbose Opt_D_dump_mod_cycles             = False
          enableIfVerbose Opt_D_dump_mod_map                = False
          enableIfVerbose Opt_D_dump_ec_trace               = False
          enableIfVerbose _                                 = True

-- | Set a 'DumpFlag'
dopt_set :: DynFlags -> DumpFlag -> DynFlags
dopt_set dfs f = dfs{ dumpFlags = EnumSet.insert f (dumpFlags dfs) }

-- | Unset a 'DumpFlag'
dopt_unset :: DynFlags -> DumpFlag -> DynFlags
dopt_unset dfs f = dfs{ dumpFlags = EnumSet.delete f (dumpFlags dfs) }

-- | Test whether a 'GeneralFlag' is set
gopt :: GeneralFlag -> DynFlags -> Bool
gopt f dflags  = f `EnumSet.member` generalFlags dflags

-- | Set a 'GeneralFlag'
gopt_set :: DynFlags -> GeneralFlag -> DynFlags
gopt_set dfs f = dfs{ generalFlags = EnumSet.insert f (generalFlags dfs) }

-- | Unset a 'GeneralFlag'
gopt_unset :: DynFlags -> GeneralFlag -> DynFlags
gopt_unset dfs f = dfs{ generalFlags = EnumSet.delete f (generalFlags dfs) }

-- | Test whether a 'WarningFlag' is set
wopt :: WarningFlag -> DynFlags -> Bool
wopt f dflags  = f `EnumSet.member` warningFlags dflags

-- | Set a 'WarningFlag'
wopt_set :: DynFlags -> WarningFlag -> DynFlags
wopt_set dfs f = dfs{ warningFlags = EnumSet.insert f (warningFlags dfs) }

-- | Unset a 'WarningFlag'
wopt_unset :: DynFlags -> WarningFlag -> DynFlags
wopt_unset dfs f = dfs{ warningFlags = EnumSet.delete f (warningFlags dfs) }

-- | Test whether a 'WarningFlag' is set as fatal
wopt_fatal :: WarningFlag -> DynFlags -> Bool
wopt_fatal f dflags = f `EnumSet.member` fatalWarningFlags dflags

-- | Mark a 'WarningFlag' as fatal (do not set the flag)
wopt_set_fatal :: DynFlags -> WarningFlag -> DynFlags
wopt_set_fatal dfs f
    = dfs { fatalWarningFlags = EnumSet.insert f (fatalWarningFlags dfs) }

-- | Mark a 'WarningFlag' as not fatal
wopt_unset_fatal :: DynFlags -> WarningFlag -> DynFlags
wopt_unset_fatal dfs f
    = dfs { fatalWarningFlags = EnumSet.delete f (fatalWarningFlags dfs) }

-- | Test whether a 'LangExt.Extension' is set
xopt :: LangExt.Extension -> DynFlags -> Bool
xopt f dflags = f `EnumSet.member` extensionFlags dflags

-- | Set a 'LangExt.Extension'
xopt_set :: DynFlags -> LangExt.Extension -> DynFlags
xopt_set dfs f
    = let onoffs = On f : extensions dfs
      in dfs { extensions = onoffs,
               extensionFlags = flattenExtensionFlags (language dfs) onoffs }

-- | Unset a 'LangExt.Extension'
xopt_unset :: DynFlags -> LangExt.Extension -> DynFlags
xopt_unset dfs f
    = let onoffs = Off f : extensions dfs
      in dfs { extensions = onoffs,
               extensionFlags = flattenExtensionFlags (language dfs) onoffs }

-- | Set or unset a 'LangExt.Extension', unless it has been explicitly
--   set or unset before.
xopt_set_unlessExplSpec
        :: LangExt.Extension
        -> (DynFlags -> LangExt.Extension -> DynFlags)
        -> DynFlags -> DynFlags
xopt_set_unlessExplSpec ext setUnset dflags =
    let referedExts = stripOnOff <$> extensions dflags
        stripOnOff (On x)  = x
        stripOnOff (Off x) = x
    in
        if ext `elem` referedExts then dflags else setUnset dflags ext

lang_set :: DynFlags -> Maybe Language -> DynFlags
lang_set dflags lang =
   dflags {
            language = lang,
            extensionFlags = flattenExtensionFlags lang (extensions dflags)
          }

-- | Set the Haskell language standard to use
setLanguage :: Language -> DynP ()
setLanguage l = upd (`lang_set` Just l)

-- | Some modules have dependencies on others through the DynFlags rather than textual imports
dynFlagDependencies :: DynFlags -> [ModuleName]
dynFlagDependencies = pluginModNames

-- | Is the -fpackage-trust mode on
packageTrustOn :: DynFlags -> Bool
packageTrustOn = gopt Opt_PackageTrust

-- | Is Safe Haskell on in some way (including inference mode)
safeHaskellOn :: DynFlags -> Bool
safeHaskellOn dflags = safeHaskellModeEnabled dflags || safeInferOn dflags

safeHaskellModeEnabled :: DynFlags -> Bool
safeHaskellModeEnabled dflags = safeHaskell dflags `elem` [Sf_Unsafe, Sf_Trustworthy
                                                   , Sf_Safe ]


-- | Is the Safe Haskell safe language in use
safeLanguageOn :: DynFlags -> Bool
safeLanguageOn dflags = safeHaskell dflags == Sf_Safe

-- | Is the Safe Haskell safe inference mode active
safeInferOn :: DynFlags -> Bool
safeInferOn = safeInfer

-- | Test if Safe Imports are on in some form
safeImportsOn :: DynFlags -> Bool
safeImportsOn dflags = safeHaskell dflags == Sf_Unsafe ||
                       safeHaskell dflags == Sf_Trustworthy ||
                       safeHaskell dflags == Sf_Safe

-- | Set a 'Safe Haskell' flag
setSafeHaskell :: SafeHaskellMode -> DynP ()
setSafeHaskell s = updM f
    where f dfs = do
              let sf = safeHaskell dfs
              safeM <- combineSafeFlags sf s
              case s of
                Sf_Safe -> return $ dfs { safeHaskell = safeM, safeInfer = False }
                -- leave safe inferrence on in Trustworthy mode so we can warn
                -- if it could have been inferred safe.
                Sf_Trustworthy -> do
                  l <- getCurLoc
                  return $ dfs { safeHaskell = safeM, trustworthyOnLoc = l }
                -- leave safe inference on in Unsafe mode as well.
                _ -> return $ dfs { safeHaskell = safeM }

-- | Are all direct imports required to be safe for this Safe Haskell mode?
-- Direct imports are when the code explicitly imports a module
safeDirectImpsReq :: DynFlags -> Bool
safeDirectImpsReq d = safeLanguageOn d

-- | Are all implicit imports required to be safe for this Safe Haskell mode?
-- Implicit imports are things in the prelude. e.g System.IO when print is used.
safeImplicitImpsReq :: DynFlags -> Bool
safeImplicitImpsReq d = safeLanguageOn d

-- | Combine two Safe Haskell modes correctly. Used for dealing with multiple flags.
-- This makes Safe Haskell very much a monoid but for now I prefer this as I don't
-- want to export this functionality from the module but do want to export the
-- type constructors.
combineSafeFlags :: SafeHaskellMode -> SafeHaskellMode -> DynP SafeHaskellMode
combineSafeFlags a b | a == Sf_None         = return b
                     | b == Sf_None         = return a
                     | a == Sf_Ignore || b == Sf_Ignore = return Sf_Ignore
                     | a == b               = return a
                     | otherwise            = addErr errm >> pure a
    where errm = "Incompatible Safe Haskell flags! ("
                    ++ show a ++ ", " ++ show b ++ ")"

-- | A list of unsafe flags under Safe Haskell. Tuple elements are:
--     * name of the flag
--     * function to get srcspan that enabled the flag
--     * function to test if the flag is on
--     * function to turn the flag off
unsafeFlags, unsafeFlagsForInfer
  :: [(String, DynFlags -> SrcSpan, DynFlags -> Bool, DynFlags -> DynFlags)]
unsafeFlags = [ ("-XGeneralizedNewtypeDeriving", newDerivOnLoc,
                    xopt LangExt.GeneralizedNewtypeDeriving,
                    flip xopt_unset LangExt.GeneralizedNewtypeDeriving)
              , ("-XTemplateHaskell", thOnLoc,
                    xopt LangExt.TemplateHaskell,
                    flip xopt_unset LangExt.TemplateHaskell)
              ]
unsafeFlagsForInfer = unsafeFlags


-- | Retrieve the options corresponding to a particular @opt_*@ field in the correct order
getOpts :: DynFlags             -- ^ 'DynFlags' to retrieve the options from
        -> (DynFlags -> [a])    -- ^ Relevant record accessor: one of the @opt_*@ accessors
        -> [a]                  -- ^ Correctly ordered extracted options
getOpts dflags opts = reverse (opts dflags)
        -- We add to the options from the front, so we need to reverse the list

-- | Gets the verbosity flag for the current verbosity level. This is fed to
-- other tools, so GHC-specific verbosity flags like @-ddump-most@ are not included
getVerbFlags :: DynFlags -> [String]
getVerbFlags dflags
  | verbosity dflags >= 4 = ["-v"]
  | otherwise             = []

setObjectDir, setHiDir, setHieDir, setStubDir, setDumpDir, setOutputDir,
         setDynObjectSuf, setDynHiSuf,
         setDylibInstallName,
         setObjectSuf, setHiSuf, setHieSuf, setHcSuf, parseDynLibLoaderMode,
         setPgmP, addOptl, addOptc, addOptcxx, addOptP,
         addCmdlineFramework, addHaddockOpts, addGhciScript,
         setInteractivePrint
   :: String -> DynFlags -> DynFlags
setOutputFile, setDynOutputFile, setOutputHi, setDumpPrefixForce
   :: Maybe String -> DynFlags -> DynFlags

setObjectDir  f d = d { objectDir  = Just f}
setHiDir      f d = d { hiDir      = Just f}
setHieDir     f d = d { hieDir     = Just f}
setStubDir    f d = d { stubDir    = Just f
                      , includePaths = addGlobalInclude (includePaths d) [f] }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling via C (i.e. unregisterised
  -- builds).
setDumpDir    f d = d { dumpDir    = Just f}
setOutputDir  f = setObjectDir f
                . setHieDir f
                . setHiDir f
                . setStubDir f
                . setDumpDir f
setDylibInstallName  f d = d { dylibInstallName = Just f}

setObjectSuf    f d = d { objectSuf    = f}
setDynObjectSuf f d = d { dynObjectSuf = f}
setHiSuf        f d = d { hiSuf        = f}
setHieSuf       f d = d { hieSuf       = f}
setDynHiSuf     f d = d { dynHiSuf     = f}
setHcSuf        f d = d { hcSuf        = f}

setOutputFile f d = d { outputFile = f}
setDynOutputFile f d = d { dynOutputFile = f}
setOutputHi   f d = d { outputHi   = f}

setJsonLogAction :: DynFlags -> DynFlags
setJsonLogAction d = d { log_action = jsonLogAction }

thisComponentId :: DynFlags -> ComponentId
thisComponentId dflags =
  let pkgstate = pkgState dflags
  in case thisComponentId_ dflags of
    Just (ComponentId raw _) -> mkComponentId pkgstate raw
    Nothing  ->
      case thisUnitIdInsts_ dflags of
        Just _  ->
          throwGhcException $ CmdLineError ("Use of -instantiated-with requires -this-component-id")
        Nothing -> mkComponentId pkgstate (unitIdFS (thisPackage dflags))

thisUnitIdInsts :: DynFlags -> [(ModuleName, Module)]
thisUnitIdInsts dflags =
    case thisUnitIdInsts_ dflags of
        Just insts -> insts
        Nothing    -> []

thisPackage :: DynFlags -> UnitId
thisPackage dflags =
    case thisUnitIdInsts_ dflags of
        Nothing -> default_uid
        Just insts
          | all (\(x,y) -> mkHoleModule x == y) insts
          -> newUnitId (thisComponentId dflags) insts
          | otherwise
          -> default_uid
  where
    default_uid = DefiniteUnitId (DefUnitId (thisInstalledUnitId dflags))

parseUnitIdInsts :: String -> [(ModuleName, Module)]
parseUnitIdInsts str = case filter ((=="").snd) (readP_to_S parse str) of
    [(r, "")] -> r
    _ -> throwGhcException $ CmdLineError ("Can't parse -instantiated-with: " ++ str)
  where parse = sepBy parseEntry (R.char ',')
        parseEntry = do
            n <- parseModuleName
            _ <- R.char '='
            m <- parseModuleId
            return (n, m)

setUnitIdInsts :: String -> DynFlags -> DynFlags
setUnitIdInsts s d =
    d { thisUnitIdInsts_ = Just (parseUnitIdInsts s) }

setComponentId :: String -> DynFlags -> DynFlags
setComponentId s d =
    d { thisComponentId_ = Just (ComponentId (fsLit s) Nothing) }

addPluginModuleName :: String -> DynFlags -> DynFlags
addPluginModuleName name d = d { pluginModNames = (mkModuleName name) : (pluginModNames d) }

clearPluginModuleNames :: DynFlags -> DynFlags
clearPluginModuleNames d =
    d { pluginModNames = []
      , pluginModNameOpts = []
      , cachedPlugins = [] }

addPluginModuleNameOption :: String -> DynFlags -> DynFlags
addPluginModuleNameOption optflag d = d { pluginModNameOpts = (mkModuleName m, option) : (pluginModNameOpts d) }
  where (m, rest) = break (== ':') optflag
        option = case rest of
          [] -> "" -- should probably signal an error
          (_:plug_opt) -> plug_opt -- ignore the ':' from break

addFrontendPluginOption :: String -> DynFlags -> DynFlags
addFrontendPluginOption s d = d { frontendPluginOpts = s : frontendPluginOpts d }

parseDynLibLoaderMode f d =
 case splitAt 8 f of
   ("deploy", "")       -> d { dynLibLoader = Deployable }
   ("sysdep", "")       -> d { dynLibLoader = SystemDependent }
   _                    -> throwGhcException (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f = alterToolSettings (\s -> s { toolSettings_pgm_P   = (pgm, map Option args)})
  where (pgm:args) = words f
addOptl   f = alterToolSettings (\s -> s { toolSettings_opt_l   = f : toolSettings_opt_l s})
addOptc   f = alterToolSettings (\s -> s { toolSettings_opt_c   = f : toolSettings_opt_c s})
addOptcxx f = alterToolSettings (\s -> s { toolSettings_opt_cxx = f : toolSettings_opt_cxx s})
addOptP   f = alterToolSettings $ \s -> s
          { toolSettings_opt_P   = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
          -- See Note [Repeated -optP hashing]
  where
  fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss


setDepMakefile :: FilePath -> DynFlags -> DynFlags
setDepMakefile f d = d { depMakefile = f }

setDepIncludeCppDeps :: Bool -> DynFlags -> DynFlags
setDepIncludeCppDeps b d = d { depIncludeCppDeps = b }

setDepIncludePkgDeps :: Bool -> DynFlags -> DynFlags
setDepIncludePkgDeps b d = d { depIncludePkgDeps = b }

addDepExcludeMod :: String -> DynFlags -> DynFlags
addDepExcludeMod m d
    = d { depExcludeMods = mkModuleName m : depExcludeMods d }

addDepSuffix :: FilePath -> DynFlags -> DynFlags
addDepSuffix s d = d { depSuffixes = s : depSuffixes d }

addCmdlineFramework f d = d { cmdlineFrameworks = f : cmdlineFrameworks d}

addGhcVersionFile :: FilePath -> DynFlags -> DynFlags
addGhcVersionFile f d = d { ghcVersionFile = Just f }

addHaddockOpts f d = d { haddockOptions = Just f}

addGhciScript f d = d { ghciScripts = f : ghciScripts d}

setInteractivePrint f d = d { interactivePrint = Just f}

-----------------------------------------------------------------------------
-- Setting the optimisation level

updOptLevel :: Int -> DynFlags -> DynFlags
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level
updOptLevel n dfs
  = dfs2{ optLevel = final_n }
  where
   final_n = max 0 (min 2 n)    -- Clamp to 0 <= n <= 2
   dfs1 = foldr (flip gopt_unset) dfs  remove_gopts
   dfs2 = foldr (flip gopt_set)   dfs1 extra_gopts

   extra_gopts  = [ f | (ns,f) <- optLevelFlags, final_n `elem` ns ]
   remove_gopts = [ f | (ns,f) <- optLevelFlags, final_n `notElem` ns ]

{- **********************************************************************
%*                                                                      *
                DynFlags parser
%*                                                                      *
%********************************************************************* -}

-- -----------------------------------------------------------------------------
-- Parsing the dynamic flags.


-- | Parse dynamic flags from a list of command line arguments.  Returns
-- the parsed 'DynFlags', the left-over arguments, and a list of warnings.
-- Throws a 'UsageError' if errors occurred during parsing (such as unknown
-- flags or missing arguments).
parseDynamicFlagsCmdLine :: MonadIO m => DynFlags -> [Located String]
                         -> m (DynFlags, [Located String], [Warn])
                            -- ^ Updated 'DynFlags', left-over arguments, and
                            -- list of warnings.
parseDynamicFlagsCmdLine = parseDynamicFlagsFull flagsAll True


-- | Like 'parseDynamicFlagsCmdLine' but does not allow the package flags
-- (-package, -hide-package, -ignore-package, -hide-all-packages, -package-db).
-- Used to parse flags set in a modules pragma.
parseDynamicFilePragma :: MonadIO m => DynFlags -> [Located String]
                       -> m (DynFlags, [Located String], [Warn])
                          -- ^ Updated 'DynFlags', left-over arguments, and
                          -- list of warnings.
parseDynamicFilePragma = parseDynamicFlagsFull flagsDynamic False


-- | Parses the dynamically set flags for GHC. This is the most general form of
-- the dynamic flag parser that the other methods simply wrap. It allows
-- saying which flags are valid flags and indicating if we are parsing
-- arguments from the command line or from a file pragma.
parseDynamicFlagsFull :: MonadIO m
                  => [Flag (CmdLineP DynFlags)]    -- ^ valid flags to match against
                  -> Bool                          -- ^ are the arguments from the command line?
                  -> DynFlags                      -- ^ current dynamic flags
                  -> [Located String]              -- ^ arguments to parse
                  -> m (DynFlags, [Located String], [Warn])
parseDynamicFlagsFull activeFlags cmdline dflags0 args = do
  let ((leftover, errs, warns), dflags1)
          = runCmdLine (processArgs activeFlags args) dflags0

  -- See Note [Handling errors when parsing commandline flags]
  unless (null errs) $ liftIO $ throwGhcExceptionIO $ errorsToGhcException $
    map ((showPpr dflags0 . getLoc &&& unLoc) . errMsg) $ errs

  -- check for disabled flags in safe haskell
  let (dflags2, sh_warns) = safeFlagCheck cmdline dflags1
      dflags3 = updateWays dflags2
      theWays = ways dflags3

  unless (allowed_combination theWays) $ liftIO $
      throwGhcExceptionIO (CmdLineError ("combination not supported: " ++
                               intercalate "/" (map wayDesc (Set.toAscList theWays))))

  let chooseOutput
        | isJust (outputFile dflags3)          -- Only iff user specified -o ...
        , not (isJust (dynOutputFile dflags3)) -- but not -dyno
        = return $ dflags3 { dynOutputFile = Just $ dynamicOutputFile dflags3 outFile }
        | otherwise
        = return dflags3
        where
          outFile = fromJust $ outputFile dflags3
  dflags4 <- ifGeneratingDynamicToo dflags3 chooseOutput (return dflags3)

  let (dflags5, consistency_warnings) = makeDynFlagsConsistent dflags4

  -- Set timer stats & heap size
  when (enableTimeStats dflags5) $ liftIO enableTimingStats
  case (ghcHeapSize dflags5) of
    Just x -> liftIO (setHeapSize x)
    _      -> return ()

  liftIO $ setUnsafeGlobalDynFlags dflags5

  let warns' = map (Warn Cmd.NoReason) (consistency_warnings ++ sh_warns)

  return (dflags5, leftover, warns' ++ warns)

-- | Write an error or warning to the 'LogOutput'.
putLogMsg :: DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle
          -> MsgDoc -> IO ()
putLogMsg dflags = log_action dflags dflags

updateWays :: DynFlags -> DynFlags
updateWays dflags
    = dflags {
        buildTag = waysTag (Set.filter (not . wayRTSOnly) (ways dflags))
      }

-- | Check (and potentially disable) any extensions that aren't allowed
-- in safe mode.
--
-- The bool is to indicate if we are parsing command line flags (false means
-- file pragma). This allows us to generate better warnings.
safeFlagCheck :: Bool -> DynFlags -> (DynFlags, [Located String])
safeFlagCheck _ dflags | safeLanguageOn dflags = (dflagsUnset, warns)
  where
    -- Handle illegal flags under safe language.
    (dflagsUnset, warns) = foldl' check_method (dflags, []) unsafeFlags

    check_method (df, warns) (str,loc,test,fix)
        | test df   = (fix df, warns ++ safeFailure (loc df) str)
        | otherwise = (df, warns)

    safeFailure loc str
       = [L loc $ str ++ " is not allowed in Safe Haskell; ignoring "
           ++ str]

safeFlagCheck cmdl dflags =
  case (safeInferOn dflags) of
    True | safeFlags -> (dflags', warn)
    True             -> (dflags' { safeInferred = False }, warn)
    False            -> (dflags', warn)

  where
    -- dynflags and warn for when -fpackage-trust by itself with no safe
    -- haskell flag
    (dflags', warn)
      | not (safeHaskellModeEnabled dflags) && not cmdl && packageTrustOn dflags
      = (gopt_unset dflags Opt_PackageTrust, pkgWarnMsg)
      | otherwise = (dflags, [])

    pkgWarnMsg = [L (pkgTrustOnLoc dflags') $
                    "-fpackage-trust ignored;" ++
                    " must be specified with a Safe Haskell flag"]

    -- Have we inferred Unsafe? See Note [GHC.Driver.Main . Safe Haskell Inference]
    safeFlags = all (\(_,_,t,_) -> not $ t dflags) unsafeFlagsForInfer


{- **********************************************************************
%*                                                                      *
                DynFlags specifications
%*                                                                      *
%********************************************************************* -}

-- | All dynamic flags option strings without the deprecated ones.
-- These are the user facing strings for enabling and disabling options.
allNonDeprecatedFlags :: [String]
allNonDeprecatedFlags = allFlagsDeps False

-- | All flags with possibility to filter deprecated ones
allFlagsDeps :: Bool -> [String]
allFlagsDeps keepDeprecated = [ '-':flagName flag
                              | (deprecated, flag) <- flagsAllDeps
                              , keepDeprecated || not (isDeprecated deprecated)]
  where isDeprecated Deprecated = True
        isDeprecated _ = False

{-
 - Below we export user facing symbols for GHC dynamic flags for use with the
 - GHC API.
 -}

-- All dynamic flags present in GHC.
flagsAll :: [Flag (CmdLineP DynFlags)]
flagsAll = map snd flagsAllDeps

-- All dynamic flags present in GHC with deprecation information.
flagsAllDeps :: [(Deprecation, Flag (CmdLineP DynFlags))]
flagsAllDeps =  package_flags_deps ++ dynamic_flags_deps


-- All dynamic flags, minus package flags, present in GHC.
flagsDynamic :: [Flag (CmdLineP DynFlags)]
flagsDynamic = map snd dynamic_flags_deps

-- ALl package flags present in GHC.
flagsPackage :: [Flag (CmdLineP DynFlags)]
flagsPackage = map snd package_flags_deps

----------------Helpers to make flags and keep deprecation information----------

type FlagMaker m = String -> OptKind m -> Flag m
type DynFlagMaker = FlagMaker (CmdLineP DynFlags)
data Deprecation = NotDeprecated | Deprecated deriving (Eq, Ord)

-- Make a non-deprecated flag
make_ord_flag :: DynFlagMaker -> String -> OptKind (CmdLineP DynFlags)
              -> (Deprecation, Flag (CmdLineP DynFlags))
make_ord_flag fm name kind = (NotDeprecated, fm name kind)

-- Make a deprecated flag
make_dep_flag :: DynFlagMaker -> String -> OptKind (CmdLineP DynFlags) -> String
                 -> (Deprecation, Flag (CmdLineP DynFlags))
make_dep_flag fm name kind message = (Deprecated,
                                      fm name $ add_dep_message kind message)

add_dep_message :: OptKind (CmdLineP DynFlags) -> String
                -> OptKind (CmdLineP DynFlags)
add_dep_message (NoArg f) message = NoArg $ f >> deprecate message
add_dep_message (HasArg f) message = HasArg $ \s -> f s >> deprecate message
add_dep_message (SepArg f) message = SepArg $ \s -> f s >> deprecate message
add_dep_message (Prefix f) message = Prefix $ \s -> f s >> deprecate message
add_dep_message (OptPrefix f) message =
                                  OptPrefix $ \s -> f s >> deprecate message
add_dep_message (OptIntSuffix f) message =
                               OptIntSuffix $ \oi -> f oi >> deprecate message
add_dep_message (IntSuffix f) message =
                                  IntSuffix $ \i -> f i >> deprecate message
add_dep_message (FloatSuffix f) message =
                                FloatSuffix $ \fl -> f fl >> deprecate message
add_dep_message (PassFlag f) message =
                                   PassFlag $ \s -> f s >> deprecate message
add_dep_message (AnySuffix f) message =
                                  AnySuffix $ \s -> f s >> deprecate message

----------------------- The main flags themselves ------------------------------
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
dynamic_flags_deps :: [(Deprecation, Flag (CmdLineP DynFlags))]
dynamic_flags_deps = [
    make_dep_flag defFlag "n" (NoArg $ return ())
        "The -n flag is deprecated and no longer has any effect"
  , make_ord_flag defFlag "cpp"      (NoArg (setExtensionFlag LangExt.Cpp))
  , make_ord_flag defFlag "F"        (NoArg (setGeneralFlag Opt_Pp))
  , (Deprecated, defFlag "#include"
      (HasArg (\_s ->
         deprecate ("-#include and INCLUDE pragmas are " ++
                    "deprecated: They no longer have any effect"))))
  , make_ord_flag defFlag "v"        (OptIntSuffix setVerbosity)

  , make_ord_flag defGhcFlag "j"     (OptIntSuffix
        (\n -> case n of
                 Just n
                     | n > 0     -> upd (\d -> d { parMakeCount = Just n })
                     | otherwise -> addErr "Syntax: -j[n] where n > 0"
                 Nothing -> upd (\d -> d { parMakeCount = Nothing })))
                 -- When the number of parallel builds
                 -- is omitted, it is the same
                 -- as specifying that the number of
                 -- parallel builds is equal to the
                 -- result of getNumProcessors
  , make_ord_flag defFlag "instantiated-with"   (sepArg setUnitIdInsts)
  , make_ord_flag defFlag "this-component-id"   (sepArg setComponentId)

    -- RTS options -------------------------------------------------------------
  , make_ord_flag defFlag "H"           (HasArg (\s -> upd (\d ->
          d { ghcHeapSize = Just $ fromIntegral (decodeSize s)})))

  , make_ord_flag defFlag "Rghc-timing" (NoArg (upd (\d ->
                                               d { enableTimeStats = True })))

    ------- ways ---------------------------------------------------------------
  , make_ord_flag defGhcFlag "prof"           (NoArg (addWay WayProf))
  , make_ord_flag defGhcFlag "eventlog"       (NoArg (addWay WayEventLog))
  , make_ord_flag defGhcFlag "debug"          (NoArg (addWay WayDebug))
  , make_ord_flag defGhcFlag "threaded"       (NoArg (addWay WayThreaded))

  , make_ord_flag defGhcFlag "ticky"
      (NoArg (setGeneralFlag Opt_Ticky >> addWay WayDebug))

    -- -ticky enables ticky-ticky code generation, and also implies -debug which
    -- is required to get the RTS ticky support.

        ----- Linker --------------------------------------------------------
  , make_ord_flag defGhcFlag "static"         (NoArg removeWayDyn)
  , make_ord_flag defGhcFlag "dynamic"        (NoArg (addWay WayDyn))
  , make_ord_flag defGhcFlag "rdynamic" $ noArg $
#if defined(linux_HOST_OS)
                              addOptl "-rdynamic"
#elif defined(mingw32_HOST_OS)
                              addOptl "-Wl,--export-all-symbols"
#else
    -- ignored for compat w/ gcc:
                              id
#endif
  , make_ord_flag defGhcFlag "relative-dynlib-paths"
      (NoArg (setGeneralFlag Opt_RelativeDynlibPaths))
  , make_ord_flag defGhcFlag "copy-libs-when-linking"
      (NoArg (setGeneralFlag Opt_SingleLibFolder))
  , make_ord_flag defGhcFlag "pie"            (NoArg (setGeneralFlag Opt_PICExecutable))
  , make_ord_flag defGhcFlag "no-pie"         (NoArg (unSetGeneralFlag Opt_PICExecutable))

        ------- Specific phases  --------------------------------------------
    -- need to appear before -pgmL to be parsed as LLVM flags.
  , make_ord_flag defFlag "pgmlo"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_lo  = (f,[]) }
  , make_ord_flag defFlag "pgmlc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_lc  = (f,[]) }
  , make_ord_flag defFlag "pgmi"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_i   =  f }
  , make_ord_flag defFlag "pgmL"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_L   = f }
  , make_ord_flag defFlag "pgmP"
      (hasArg setPgmP)
  , make_ord_flag defFlag "pgmF"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_F   = f }
  , make_ord_flag defFlag "pgmc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s
         { toolSettings_pgm_c   = f
         , -- Don't pass -no-pie with -pgmc
           -- (see #15319)
           toolSettings_ccSupportsNoPie = False
         }
  , make_ord_flag defFlag "pgms"
      (HasArg (\_ -> addWarn "Object splitting was removed in GHC 8.8"))
  , make_ord_flag defFlag "pgma"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_a   = (f,[]) }
  , make_ord_flag defFlag "pgml"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_l   = (f,[]) }
  , make_ord_flag defFlag "pgmdll"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_dll = (f,[]) }
  , make_ord_flag defFlag "pgmwindres"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_windres = f }
  , make_ord_flag defFlag "pgmlibtool"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_libtool = f }
  , make_ord_flag defFlag "pgmar"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_ar = f }
  , make_ord_flag defFlag "pgmranlib"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_ranlib = f }


    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , make_ord_flag defFlag "optlo"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_lo  = f : toolSettings_opt_lo s }
  , make_ord_flag defFlag "optlc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_lc  = f : toolSettings_opt_lc s }
  , make_ord_flag defFlag "opti"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_i   = f : toolSettings_opt_i s }
  , make_ord_flag defFlag "optL"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_L   = f : toolSettings_opt_L s }
  , make_ord_flag defFlag "optP"
      (hasArg addOptP)
  , make_ord_flag defFlag "optF"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_F   = f : toolSettings_opt_F s }
  , make_ord_flag defFlag "optc"
      (hasArg addOptc)
  , make_ord_flag defFlag "optcxx"
      (hasArg addOptcxx)
  , make_ord_flag defFlag "opta"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_a   = f : toolSettings_opt_a s }
  , make_ord_flag defFlag "optl"
      (hasArg addOptl)
  , make_ord_flag defFlag "optwindres"
      $ hasArg $ \f ->
        alterToolSettings $ \s -> s { toolSettings_opt_windres = f : toolSettings_opt_windres s }

  , make_ord_flag defGhcFlag "split-objs"
      (NoArg $ addWarn "ignoring -split-objs")

  , make_ord_flag defGhcFlag "split-sections"
      (noArgM (\dflags -> do
        if platformHasSubsectionsViaSymbols (targetPlatform dflags)
          then do addWarn $
                    "-split-sections is not useful on this platform " ++
                    "since it always uses subsections via symbols. Ignoring."
                  return dflags
          else return (gopt_set dflags Opt_SplitSections)))

        -------- ghc -M -----------------------------------------------------
  , make_ord_flag defGhcFlag "dep-suffix"              (hasArg addDepSuffix)
  , make_ord_flag defGhcFlag "dep-makefile"            (hasArg setDepMakefile)
  , make_ord_flag defGhcFlag "include-cpp-deps"
        (noArg (setDepIncludeCppDeps True))
  , make_ord_flag defGhcFlag "include-pkg-deps"
        (noArg (setDepIncludePkgDeps True))
  , make_ord_flag defGhcFlag "exclude-module"          (hasArg addDepExcludeMod)

        -------- Linking ----------------------------------------------------
  , make_ord_flag defGhcFlag "no-link"
        (noArg (\d -> d { ghcLink=NoLink }))
  , make_ord_flag defGhcFlag "shared"
        (noArg (\d -> d { ghcLink=LinkDynLib }))
  , make_ord_flag defGhcFlag "staticlib"
        (noArg (\d -> d { ghcLink=LinkStaticLib }))
  , make_ord_flag defGhcFlag "dynload"            (hasArg parseDynLibLoaderMode)
  , make_ord_flag defGhcFlag "dylib-install-name" (hasArg setDylibInstallName)

        ------- Libraries ---------------------------------------------------
  , make_ord_flag defFlag "L"   (Prefix addLibraryPath)
  , make_ord_flag defFlag "l"   (hasArg (addLdInputs . Option . ("-l" ++)))

        ------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  , make_ord_flag defFlag "framework-path" (HasArg addFrameworkPath)
  , make_ord_flag defFlag "framework"      (hasArg addCmdlineFramework)

        ------- Output Redirection ------------------------------------------
  , make_ord_flag defGhcFlag "odir"              (hasArg setObjectDir)
  , make_ord_flag defGhcFlag "o"                 (sepArg (setOutputFile . Just))
  , make_ord_flag defGhcFlag "dyno"
        (sepArg (setDynOutputFile . Just))
  , make_ord_flag defGhcFlag "ohi"
        (hasArg (setOutputHi . Just ))
  , make_ord_flag defGhcFlag "osuf"              (hasArg setObjectSuf)
  , make_ord_flag defGhcFlag "dynosuf"           (hasArg setDynObjectSuf)
  , make_ord_flag defGhcFlag "hcsuf"             (hasArg setHcSuf)
  , make_ord_flag defGhcFlag "hisuf"             (hasArg setHiSuf)
  , make_ord_flag defGhcFlag "hiesuf"            (hasArg setHieSuf)
  , make_ord_flag defGhcFlag "dynhisuf"          (hasArg setDynHiSuf)
  , make_ord_flag defGhcFlag "hidir"             (hasArg setHiDir)
  , make_ord_flag defGhcFlag "hiedir"            (hasArg setHieDir)
  , make_ord_flag defGhcFlag "tmpdir"            (hasArg setTmpDir)
  , make_ord_flag defGhcFlag "stubdir"           (hasArg setStubDir)
  , make_ord_flag defGhcFlag "dumpdir"           (hasArg setDumpDir)
  , make_ord_flag defGhcFlag "outputdir"         (hasArg setOutputDir)
  , make_ord_flag defGhcFlag "ddump-file-prefix"
        (hasArg (setDumpPrefixForce . Just))

  , make_ord_flag defGhcFlag "dynamic-too"
        (NoArg (setGeneralFlag Opt_BuildDynamicToo))

        ------- Keeping temporary files -------------------------------------
     -- These can be singular (think ghc -c) or plural (think ghc --make)
  , make_ord_flag defGhcFlag "keep-hc-file"
        (NoArg (setGeneralFlag Opt_KeepHcFiles))
  , make_ord_flag defGhcFlag "keep-hc-files"
        (NoArg (setGeneralFlag Opt_KeepHcFiles))
  , make_ord_flag defGhcFlag "keep-hscpp-file"
        (NoArg (setGeneralFlag Opt_KeepHscppFiles))
  , make_ord_flag defGhcFlag "keep-hscpp-files"
        (NoArg (setGeneralFlag Opt_KeepHscppFiles))
  , make_ord_flag defGhcFlag "keep-s-file"
        (NoArg (setGeneralFlag Opt_KeepSFiles))
  , make_ord_flag defGhcFlag "keep-s-files"
        (NoArg (setGeneralFlag Opt_KeepSFiles))
  , make_ord_flag defGhcFlag "keep-llvm-file"
        (NoArg $ setObjTarget HscLlvm >> setGeneralFlag Opt_KeepLlvmFiles)
  , make_ord_flag defGhcFlag "keep-llvm-files"
        (NoArg $ setObjTarget HscLlvm >> setGeneralFlag Opt_KeepLlvmFiles)
     -- This only makes sense as plural
  , make_ord_flag defGhcFlag "keep-tmp-files"
        (NoArg (setGeneralFlag Opt_KeepTmpFiles))
  , make_ord_flag defGhcFlag "keep-hi-file"
        (NoArg (setGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "no-keep-hi-file"
        (NoArg (unSetGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "keep-hi-files"
        (NoArg (setGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "no-keep-hi-files"
        (NoArg (unSetGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "keep-o-file"
        (NoArg (setGeneralFlag Opt_KeepOFiles))
  , make_ord_flag defGhcFlag "no-keep-o-file"
        (NoArg (unSetGeneralFlag Opt_KeepOFiles))
  , make_ord_flag defGhcFlag "keep-o-files"
        (NoArg (setGeneralFlag Opt_KeepOFiles))
  , make_ord_flag defGhcFlag "no-keep-o-files"
        (NoArg (unSetGeneralFlag Opt_KeepOFiles))

        ------- Miscellaneous ----------------------------------------------
  , make_ord_flag defGhcFlag "no-auto-link-packages"
        (NoArg (unSetGeneralFlag Opt_AutoLinkPackages))
  , make_ord_flag defGhcFlag "no-hs-main"
        (NoArg (setGeneralFlag Opt_NoHsMain))
  , make_ord_flag defGhcFlag "fno-state-hack"
        (NoArg (setGeneralFlag Opt_G_NoStateHack))
  , make_ord_flag defGhcFlag "fno-opt-coercion"
        (NoArg (setGeneralFlag Opt_G_NoOptCoercion))
  , make_ord_flag defGhcFlag "with-rtsopts"
        (HasArg setRtsOpts)
  , make_ord_flag defGhcFlag "rtsopts"
        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , make_ord_flag defGhcFlag "rtsopts=all"
        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , make_ord_flag defGhcFlag "rtsopts=some"
        (NoArg (setRtsOptsEnabled RtsOptsSafeOnly))
  , make_ord_flag defGhcFlag "rtsopts=none"
        (NoArg (setRtsOptsEnabled RtsOptsNone))
  , make_ord_flag defGhcFlag "rtsopts=ignore"
        (NoArg (setRtsOptsEnabled RtsOptsIgnore))
  , make_ord_flag defGhcFlag "rtsopts=ignoreAll"
        (NoArg (setRtsOptsEnabled RtsOptsIgnoreAll))
  , make_ord_flag defGhcFlag "no-rtsopts"
        (NoArg (setRtsOptsEnabled RtsOptsNone))
  , make_ord_flag defGhcFlag "no-rtsopts-suggestions"
      (noArg (\d -> d {rtsOptsSuggestions = False}))
  , make_ord_flag defGhcFlag "dhex-word-literals"
        (NoArg (setGeneralFlag Opt_HexWordLiterals))

  , make_ord_flag defGhcFlag "ghcversion-file"      (hasArg addGhcVersionFile)
  , make_ord_flag defGhcFlag "main-is"              (SepArg setMainIs)
  , make_ord_flag defGhcFlag "haddock"              (NoArg (setGeneralFlag Opt_Haddock))
  , make_ord_flag defGhcFlag "no-haddock"           (NoArg (unSetGeneralFlag Opt_Haddock))
  , make_ord_flag defGhcFlag "haddock-opts"         (hasArg addHaddockOpts)
  , make_ord_flag defGhcFlag "hpcdir"               (SepArg setOptHpcDir)
  , make_ord_flag defGhciFlag "ghci-script"         (hasArg addGhciScript)
  , make_ord_flag defGhciFlag "interactive-print"   (hasArg setInteractivePrint)
  , make_ord_flag defGhcFlag "ticky-allocd"
        (NoArg (setGeneralFlag Opt_Ticky_Allocd))
  , make_ord_flag defGhcFlag "ticky-LNE"
        (NoArg (setGeneralFlag Opt_Ticky_LNE))
  , make_ord_flag defGhcFlag "ticky-dyn-thunk"
        (NoArg (setGeneralFlag Opt_Ticky_Dyn_Thunk))
        ------- recompilation checker --------------------------------------
  , make_dep_flag defGhcFlag "recomp"
        (NoArg $ unSetGeneralFlag Opt_ForceRecomp)
             "Use -fno-force-recomp instead"
  , make_dep_flag defGhcFlag "no-recomp"
        (NoArg $ setGeneralFlag Opt_ForceRecomp) "Use -fforce-recomp instead"
  , make_ord_flag defFlag "fmax-errors"
      (intSuffix (\n d -> d { maxErrors = Just (max 1 n) }))
  , make_ord_flag defFlag "fno-max-errors"
      (noArg (\d -> d { maxErrors = Nothing }))
  , make_ord_flag defFlag "freverse-errors"
        (noArg (\d -> d {reverseErrors = True} ))
  , make_ord_flag defFlag "fno-reverse-errors"
        (noArg (\d -> d {reverseErrors = False} ))

        ------ HsCpp opts ---------------------------------------------------
  , make_ord_flag defFlag "D"              (AnySuffix (upd . addOptP))
  , make_ord_flag defFlag "U"              (AnySuffix (upd . addOptP))

        ------- Include/Import Paths ----------------------------------------
  , make_ord_flag defFlag "I"              (Prefix    addIncludePath)
  , make_ord_flag defFlag "i"              (OptPrefix addImportPath)

        ------ Output style options -----------------------------------------
  , make_ord_flag defFlag "dppr-user-length" (intSuffix (\n d ->
                                                       d { pprUserLength = n }))
  , make_ord_flag defFlag "dppr-cols"        (intSuffix (\n d ->
                                                             d { pprCols = n }))
  , make_ord_flag defFlag "fdiagnostics-color=auto"
      (NoArg (upd (\d -> d { useColor = Auto })))
  , make_ord_flag defFlag "fdiagnostics-color=always"
      (NoArg (upd (\d -> d { useColor = Always })))
  , make_ord_flag defFlag "fdiagnostics-color=never"
      (NoArg (upd (\d -> d { useColor = Never })))

  -- Suppress all that is suppressable in core dumps.
  -- Except for uniques, as some simplifier phases introduce new variables that
  -- have otherwise identical names.
  , make_ord_flag defGhcFlag "dsuppress-all"
      (NoArg $ do setGeneralFlag Opt_SuppressCoercions
                  setGeneralFlag Opt_SuppressVarKinds
                  setGeneralFlag Opt_SuppressModulePrefixes
                  setGeneralFlag Opt_SuppressTypeApplications
                  setGeneralFlag Opt_SuppressIdInfo
                  setGeneralFlag Opt_SuppressTicks
                  setGeneralFlag Opt_SuppressStgExts
                  setGeneralFlag Opt_SuppressTypeSignatures
                  setGeneralFlag Opt_SuppressTimestamps)

        ------ Debugging ----------------------------------------------------
  , make_ord_flag defGhcFlag "dstg-stats"
        (NoArg (setGeneralFlag Opt_StgStats))

  , make_ord_flag defGhcFlag "ddump-cmm"
        (setDumpFlag Opt_D_dump_cmm)
  , make_ord_flag defGhcFlag "ddump-cmm-from-stg"
        (setDumpFlag Opt_D_dump_cmm_from_stg)
  , make_ord_flag defGhcFlag "ddump-cmm-raw"
        (setDumpFlag Opt_D_dump_cmm_raw)
  , make_ord_flag defGhcFlag "ddump-cmm-verbose"
        (setDumpFlag Opt_D_dump_cmm_verbose)
  , make_ord_flag defGhcFlag "ddump-cmm-verbose-by-proc"
        (setDumpFlag Opt_D_dump_cmm_verbose_by_proc)
  , make_ord_flag defGhcFlag "ddump-cmm-cfg"
        (setDumpFlag Opt_D_dump_cmm_cfg)
  , make_ord_flag defGhcFlag "ddump-cmm-cbe"
        (setDumpFlag Opt_D_dump_cmm_cbe)
  , make_ord_flag defGhcFlag "ddump-cmm-switch"
        (setDumpFlag Opt_D_dump_cmm_switch)
  , make_ord_flag defGhcFlag "ddump-cmm-proc"
        (setDumpFlag Opt_D_dump_cmm_proc)
  , make_ord_flag defGhcFlag "ddump-cmm-sp"
        (setDumpFlag Opt_D_dump_cmm_sp)
  , make_ord_flag defGhcFlag "ddump-cmm-sink"
        (setDumpFlag Opt_D_dump_cmm_sink)
  , make_ord_flag defGhcFlag "ddump-cmm-caf"
        (setDumpFlag Opt_D_dump_cmm_caf)
  , make_ord_flag defGhcFlag "ddump-cmm-procmap"
        (setDumpFlag Opt_D_dump_cmm_procmap)
  , make_ord_flag defGhcFlag "ddump-cmm-split"
        (setDumpFlag Opt_D_dump_cmm_split)
  , make_ord_flag defGhcFlag "ddump-cmm-info"
        (setDumpFlag Opt_D_dump_cmm_info)
  , make_ord_flag defGhcFlag "ddump-cmm-cps"
        (setDumpFlag Opt_D_dump_cmm_cps)
  , make_ord_flag defGhcFlag "ddump-cfg-weights"
        (setDumpFlag Opt_D_dump_cfg_weights)
  , make_ord_flag defGhcFlag "ddump-core-stats"
        (setDumpFlag Opt_D_dump_core_stats)
  , make_ord_flag defGhcFlag "ddump-asm"
        (setDumpFlag Opt_D_dump_asm)
  , make_ord_flag defGhcFlag "ddump-asm-native"
        (setDumpFlag Opt_D_dump_asm_native)
  , make_ord_flag defGhcFlag "ddump-asm-liveness"
        (setDumpFlag Opt_D_dump_asm_liveness)
  , make_ord_flag defGhcFlag "ddump-asm-regalloc"
        (setDumpFlag Opt_D_dump_asm_regalloc)
  , make_ord_flag defGhcFlag "ddump-asm-conflicts"
        (setDumpFlag Opt_D_dump_asm_conflicts)
  , make_ord_flag defGhcFlag "ddump-asm-regalloc-stages"
        (setDumpFlag Opt_D_dump_asm_regalloc_stages)
  , make_ord_flag defGhcFlag "ddump-asm-stats"
        (setDumpFlag Opt_D_dump_asm_stats)
  , make_ord_flag defGhcFlag "ddump-asm-expanded"
        (setDumpFlag Opt_D_dump_asm_expanded)
  , make_ord_flag defGhcFlag "ddump-llvm"
        (NoArg $ setObjTarget HscLlvm >> setDumpFlag' Opt_D_dump_llvm)
  , make_ord_flag defGhcFlag "ddump-deriv"
        (setDumpFlag Opt_D_dump_deriv)
  , make_ord_flag defGhcFlag "ddump-ds"
        (setDumpFlag Opt_D_dump_ds)
  , make_ord_flag defGhcFlag "ddump-ds-preopt"
        (setDumpFlag Opt_D_dump_ds_preopt)
  , make_ord_flag defGhcFlag "ddump-foreign"
        (setDumpFlag Opt_D_dump_foreign)
  , make_ord_flag defGhcFlag "ddump-inlinings"
        (setDumpFlag Opt_D_dump_inlinings)
  , make_ord_flag defGhcFlag "ddump-rule-firings"
        (setDumpFlag Opt_D_dump_rule_firings)
  , make_ord_flag defGhcFlag "ddump-rule-rewrites"
        (setDumpFlag Opt_D_dump_rule_rewrites)
  , make_ord_flag defGhcFlag "ddump-simpl-trace"
        (setDumpFlag Opt_D_dump_simpl_trace)
  , make_ord_flag defGhcFlag "ddump-occur-anal"
        (setDumpFlag Opt_D_dump_occur_anal)
  , make_ord_flag defGhcFlag "ddump-parsed"
        (setDumpFlag Opt_D_dump_parsed)
  , make_ord_flag defGhcFlag "ddump-parsed-ast"
        (setDumpFlag Opt_D_dump_parsed_ast)
  , make_ord_flag defGhcFlag "ddump-rn"
        (setDumpFlag Opt_D_dump_rn)
  , make_ord_flag defGhcFlag "ddump-rn-ast"
        (setDumpFlag Opt_D_dump_rn_ast)
  , make_ord_flag defGhcFlag "ddump-simpl"
        (setDumpFlag Opt_D_dump_simpl)
  , make_ord_flag defGhcFlag "ddump-simpl-iterations"
      (setDumpFlag Opt_D_dump_simpl_iterations)
  , make_ord_flag defGhcFlag "ddump-spec"
        (setDumpFlag Opt_D_dump_spec)
  , make_ord_flag defGhcFlag "ddump-prep"
        (setDumpFlag Opt_D_dump_prep)
  , make_ord_flag defGhcFlag "ddump-stg"
        (setDumpFlag Opt_D_dump_stg)
  , make_ord_flag defGhcFlag "ddump-stg-unarised"
        (setDumpFlag Opt_D_dump_stg_unarised)
  , make_ord_flag defGhcFlag "ddump-stg-final"
        (setDumpFlag Opt_D_dump_stg_final)
  , make_ord_flag defGhcFlag "ddump-call-arity"
        (setDumpFlag Opt_D_dump_call_arity)
  , make_ord_flag defGhcFlag "ddump-exitify"
        (setDumpFlag Opt_D_dump_exitify)
  , make_ord_flag defGhcFlag "ddump-stranal"
        (setDumpFlag Opt_D_dump_stranal)
  , make_ord_flag defGhcFlag "ddump-str-signatures"
        (setDumpFlag Opt_D_dump_str_signatures)
  , make_ord_flag defGhcFlag "ddump-cpranal"
        (setDumpFlag Opt_D_dump_cpranal)
  , make_ord_flag defGhcFlag "ddump-cpr-signatures"
        (setDumpFlag Opt_D_dump_cpr_signatures)
  , make_ord_flag defGhcFlag "ddump-tc"
        (setDumpFlag Opt_D_dump_tc)
  , make_ord_flag defGhcFlag "ddump-tc-ast"
        (setDumpFlag Opt_D_dump_tc_ast)
  , make_ord_flag defGhcFlag "ddump-hie"
        (setDumpFlag Opt_D_dump_hie)
  , make_ord_flag defGhcFlag "ddump-types"
        (setDumpFlag Opt_D_dump_types)
  , make_ord_flag defGhcFlag "ddump-rules"
        (setDumpFlag Opt_D_dump_rules)
  , make_ord_flag defGhcFlag "ddump-cse"
        (setDumpFlag Opt_D_dump_cse)
  , make_ord_flag defGhcFlag "ddump-worker-wrapper"
        (setDumpFlag Opt_D_dump_worker_wrapper)
  , make_ord_flag defGhcFlag "ddump-rn-trace"
        (setDumpFlag Opt_D_dump_rn_trace)
  , make_ord_flag defGhcFlag "ddump-if-trace"
        (setDumpFlag Opt_D_dump_if_trace)
  , make_ord_flag defGhcFlag "ddump-cs-trace"
        (setDumpFlag Opt_D_dump_cs_trace)
  , make_ord_flag defGhcFlag "ddump-tc-trace"
        (NoArg (do setDumpFlag' Opt_D_dump_tc_trace
                   setDumpFlag' Opt_D_dump_cs_trace))
  , make_ord_flag defGhcFlag "ddump-ec-trace"
        (setDumpFlag Opt_D_dump_ec_trace)
  , make_ord_flag defGhcFlag "ddump-vt-trace"
        (setDumpFlag Opt_D_dump_vt_trace)
  , make_ord_flag defGhcFlag "ddump-splices"
        (setDumpFlag Opt_D_dump_splices)
  , make_ord_flag defGhcFlag "dth-dec-file"
        (setDumpFlag Opt_D_th_dec_file)

  , make_ord_flag defGhcFlag "ddump-rn-stats"
        (setDumpFlag Opt_D_dump_rn_stats)
  , make_ord_flag defGhcFlag "ddump-opt-cmm"
        (setDumpFlag Opt_D_dump_opt_cmm)
  , make_ord_flag defGhcFlag "ddump-simpl-stats"
        (setDumpFlag Opt_D_dump_simpl_stats)
  , make_ord_flag defGhcFlag "ddump-bcos"
        (setDumpFlag Opt_D_dump_BCOs)
  , make_ord_flag defGhcFlag "dsource-stats"
        (setDumpFlag Opt_D_source_stats)
  , make_ord_flag defGhcFlag "dverbose-core2core"
        (NoArg $ setVerbosity (Just 2) >> setVerboseCore2Core)
  , make_ord_flag defGhcFlag "dverbose-stg2stg"
        (setDumpFlag Opt_D_verbose_stg2stg)
  , make_ord_flag defGhcFlag "ddump-hi"
        (setDumpFlag Opt_D_dump_hi)
  , make_ord_flag defGhcFlag "ddump-minimal-imports"
        (NoArg (setGeneralFlag Opt_D_dump_minimal_imports))
  , make_ord_flag defGhcFlag "ddump-hpc"
        (setDumpFlag Opt_D_dump_ticked) -- back compat
  , make_ord_flag defGhcFlag "ddump-ticked"
        (setDumpFlag Opt_D_dump_ticked)
  , make_ord_flag defGhcFlag "ddump-mod-cycles"
        (setDumpFlag Opt_D_dump_mod_cycles)
  , make_ord_flag defGhcFlag "ddump-mod-map"
        (setDumpFlag Opt_D_dump_mod_map)
  , make_ord_flag defGhcFlag "ddump-timings"
        (setDumpFlag Opt_D_dump_timings)
  , make_ord_flag defGhcFlag "ddump-view-pattern-commoning"
        (setDumpFlag Opt_D_dump_view_pattern_commoning)
  , make_ord_flag defGhcFlag "ddump-to-file"
        (NoArg (setGeneralFlag Opt_DumpToFile))
  , make_ord_flag defGhcFlag "ddump-hi-diffs"
        (setDumpFlag Opt_D_dump_hi_diffs)
  , make_ord_flag defGhcFlag "ddump-rtti"
        (setDumpFlag Opt_D_dump_rtti)
  , make_ord_flag defGhcFlag "dcore-lint"
        (NoArg (setGeneralFlag Opt_DoCoreLinting))
  , make_ord_flag defGhcFlag "dstg-lint"
        (NoArg (setGeneralFlag Opt_DoStgLinting))
  , make_ord_flag defGhcFlag "dcmm-lint"
        (NoArg (setGeneralFlag Opt_DoCmmLinting))
  , make_ord_flag defGhcFlag "dasm-lint"
        (NoArg (setGeneralFlag Opt_DoAsmLinting))
  , make_ord_flag defGhcFlag "dannot-lint"
        (NoArg (setGeneralFlag Opt_DoAnnotationLinting))
  , make_ord_flag defGhcFlag "dshow-passes"
        (NoArg $ forceRecompile >> (setVerbosity $ Just 2))
  , make_ord_flag defGhcFlag "dfaststring-stats"
        (NoArg (setGeneralFlag Opt_D_faststring_stats))
  , make_ord_flag defGhcFlag "dno-llvm-mangler"
        (NoArg (setGeneralFlag Opt_NoLlvmMangler)) -- hidden flag
  , make_ord_flag defGhcFlag "fast-llvm"
        (NoArg (setGeneralFlag Opt_FastLlvm)) -- hidden flag
  , make_ord_flag defGhcFlag "dno-typeable-binds"
        (NoArg (setGeneralFlag Opt_NoTypeableBinds))
  , make_ord_flag defGhcFlag "ddump-debug"
        (setDumpFlag Opt_D_dump_debug)
  , make_ord_flag defGhcFlag "ddump-json"
        (noArg (flip dopt_set Opt_D_dump_json . setJsonLogAction ) )
  , make_ord_flag defGhcFlag "dppr-debug"
        (setDumpFlag Opt_D_ppr_debug)
  , make_ord_flag defGhcFlag "ddebug-output"
        (noArg (flip dopt_unset Opt_D_no_debug_output))
  , make_ord_flag defGhcFlag "dno-debug-output"
        (setDumpFlag Opt_D_no_debug_output)

        ------ Machine dependent (-m<blah>) stuff ---------------------------

  , make_ord_flag defGhcFlag "msse"         (noArg (\d ->
                                                  d { sseVersion = Just SSE1 }))
  , make_ord_flag defGhcFlag "msse2"        (noArg (\d ->
                                                  d { sseVersion = Just SSE2 }))
  , make_ord_flag defGhcFlag "msse3"        (noArg (\d ->
                                                  d { sseVersion = Just SSE3 }))
  , make_ord_flag defGhcFlag "msse4"        (noArg (\d ->
                                                  d { sseVersion = Just SSE4 }))
  , make_ord_flag defGhcFlag "msse4.2"      (noArg (\d ->
                                                 d { sseVersion = Just SSE42 }))
  , make_ord_flag defGhcFlag "mbmi"         (noArg (\d ->
                                                 d { bmiVersion = Just BMI1 }))
  , make_ord_flag defGhcFlag "mbmi2"        (noArg (\d ->
                                                 d { bmiVersion = Just BMI2 }))
  , make_ord_flag defGhcFlag "mavx"         (noArg (\d -> d { avx = True }))
  , make_ord_flag defGhcFlag "mavx2"        (noArg (\d -> d { avx2 = True }))
  , make_ord_flag defGhcFlag "mavx512cd"    (noArg (\d ->
                                                         d { avx512cd = True }))
  , make_ord_flag defGhcFlag "mavx512er"    (noArg (\d ->
                                                         d { avx512er = True }))
  , make_ord_flag defGhcFlag "mavx512f"     (noArg (\d -> d { avx512f = True }))
  , make_ord_flag defGhcFlag "mavx512pf"    (noArg (\d ->
                                                         d { avx512pf = True }))

     ------ Warning opts -------------------------------------------------
  , make_ord_flag defFlag "W"       (NoArg (mapM_ setWarningFlag minusWOpts))
  , make_ord_flag defFlag "Werror"
               (NoArg (do { setGeneralFlag Opt_WarnIsError
                          ; mapM_ setFatalWarningFlag minusWeverythingOpts   }))
  , make_ord_flag defFlag "Wwarn"
               (NoArg (do { unSetGeneralFlag Opt_WarnIsError
                          ; mapM_ unSetFatalWarningFlag minusWeverythingOpts }))
                          -- Opt_WarnIsError is still needed to pass -Werror
                          -- to CPP; see runCpp in SysTools
  , make_dep_flag defFlag "Wnot"    (NoArg (upd (\d ->
                                              d {warningFlags = EnumSet.empty})))
                                             "Use -w or -Wno-everything instead"
  , make_ord_flag defFlag "w"       (NoArg (upd (\d ->
                                              d {warningFlags = EnumSet.empty})))

     -- New-style uniform warning sets
     --
     -- Note that -Weverything > -Wall > -Wextra > -Wdefault > -Wno-everything
  , make_ord_flag defFlag "Weverything"    (NoArg (mapM_
                                           setWarningFlag minusWeverythingOpts))
  , make_ord_flag defFlag "Wno-everything"
                           (NoArg (upd (\d -> d {warningFlags = EnumSet.empty})))

  , make_ord_flag defFlag "Wall"           (NoArg (mapM_
                                                  setWarningFlag minusWallOpts))
  , make_ord_flag defFlag "Wno-all"        (NoArg (mapM_
                                                unSetWarningFlag minusWallOpts))

  , make_ord_flag defFlag "Wextra"         (NoArg (mapM_
                                                     setWarningFlag minusWOpts))
  , make_ord_flag defFlag "Wno-extra"      (NoArg (mapM_
                                                   unSetWarningFlag minusWOpts))

  , make_ord_flag defFlag "Wdefault"       (NoArg (mapM_
                                               setWarningFlag standardWarnings))
  , make_ord_flag defFlag "Wno-default"    (NoArg (mapM_
                                             unSetWarningFlag standardWarnings))

  , make_ord_flag defFlag "Wcompat"        (NoArg (mapM_
                                               setWarningFlag minusWcompatOpts))
  , make_ord_flag defFlag "Wno-compat"     (NoArg (mapM_
                                             unSetWarningFlag minusWcompatOpts))

        ------ Plugin flags ------------------------------------------------
  , make_ord_flag defGhcFlag "fplugin-opt" (hasArg addPluginModuleNameOption)
  , make_ord_flag defGhcFlag "fplugin-trustworthy"
      (NoArg (setGeneralFlag Opt_PluginTrustworthy))
  , make_ord_flag defGhcFlag "fplugin"     (hasArg addPluginModuleName)
  , make_ord_flag defGhcFlag "fclear-plugins" (noArg clearPluginModuleNames)
  , make_ord_flag defGhcFlag "ffrontend-opt" (hasArg addFrontendPluginOption)

        ------ Optimisation flags ------------------------------------------
  , make_dep_flag defGhcFlag "Onot"   (noArgM $ setOptLevel 0 )
                                                            "Use -O0 instead"
  , make_ord_flag defGhcFlag "O"      (optIntSuffixM (\mb_n ->
                                                setOptLevel (mb_n `orElse` 1)))
                -- If the number is missing, use 1

  , make_ord_flag defFlag "fbinary-blob-threshold"
      (intSuffix (\n d -> d { binBlobThreshold = fromIntegral n }))

  , make_ord_flag defFlag "fmax-relevant-binds"
      (intSuffix (\n d -> d { maxRelevantBinds = Just n }))
  , make_ord_flag defFlag "fno-max-relevant-binds"
      (noArg (\d -> d { maxRelevantBinds = Nothing }))

  , make_ord_flag defFlag "fmax-valid-hole-fits"
      (intSuffix (\n d -> d { maxValidHoleFits = Just n }))
  , make_ord_flag defFlag "fno-max-valid-hole-fits"
      (noArg (\d -> d { maxValidHoleFits = Nothing }))
  , make_ord_flag defFlag "fmax-refinement-hole-fits"
      (intSuffix (\n d -> d { maxRefHoleFits = Just n }))
  , make_ord_flag defFlag "fno-max-refinement-hole-fits"
      (noArg (\d -> d { maxRefHoleFits = Nothing }))
  , make_ord_flag defFlag "frefinement-level-hole-fits"
      (intSuffix (\n d -> d { refLevelHoleFits = Just n }))
  , make_ord_flag defFlag "fno-refinement-level-hole-fits"
      (noArg (\d -> d { refLevelHoleFits = Nothing }))

  , make_dep_flag defGhcFlag "fllvm-pass-vectors-in-regs"
            (noArg id)
            "vectors registers are now passed in registers by default."
  , make_ord_flag defFlag "fmax-uncovered-patterns"
      (intSuffix (\n d -> d { maxUncoveredPatterns = n }))
  , make_ord_flag defFlag "fmax-pmcheck-models"
      (intSuffix (\n d -> d { maxPmCheckModels = n }))
  , make_ord_flag defFlag "fsimplifier-phases"
      (intSuffix (\n d -> d { simplPhases = n }))
  , make_ord_flag defFlag "fmax-simplifier-iterations"
      (intSuffix (\n d -> d { maxSimplIterations = n }))
  , (Deprecated, defFlag "fmax-pmcheck-iterations"
      (intSuffixM (\_ d ->
       do { deprecate $ "use -fmax-pmcheck-models instead"
          ; return d })))
  , make_ord_flag defFlag "fsimpl-tick-factor"
      (intSuffix (\n d -> d { simplTickFactor = n }))
  , make_ord_flag defFlag "fspec-constr-threshold"
      (intSuffix (\n d -> d { specConstrThreshold = Just n }))
  , make_ord_flag defFlag "fno-spec-constr-threshold"
      (noArg (\d -> d { specConstrThreshold = Nothing }))
  , make_ord_flag defFlag "fspec-constr-count"
      (intSuffix (\n d -> d { specConstrCount = Just n }))
  , make_ord_flag defFlag "fno-spec-constr-count"
      (noArg (\d -> d { specConstrCount = Nothing }))
  , make_ord_flag defFlag "fspec-constr-recursive"
      (intSuffix (\n d -> d { specConstrRecursive = n }))
  , make_ord_flag defFlag "fliberate-case-threshold"
      (intSuffix (\n d -> d { liberateCaseThreshold = Just n }))
  , make_ord_flag defFlag "fno-liberate-case-threshold"
      (noArg (\d -> d { liberateCaseThreshold = Nothing }))
  , make_ord_flag defFlag "drule-check"
      (sepArg (\s d -> d { ruleCheck = Just s }))
  , make_ord_flag defFlag "dinline-check"
      (sepArg (\s d -> d { inlineCheck = Just s }))
  , make_ord_flag defFlag "freduction-depth"
      (intSuffix (\n d -> d { reductionDepth = treatZeroAsInf n }))
  , make_ord_flag defFlag "fconstraint-solver-iterations"
      (intSuffix (\n d -> d { solverIterations = treatZeroAsInf n }))
  , (Deprecated, defFlag "fcontext-stack"
      (intSuffixM (\n d ->
       do { deprecate $ "use -freduction-depth=" ++ show n ++ " instead"
          ; return $ d { reductionDepth = treatZeroAsInf n } })))
  , (Deprecated, defFlag "ftype-function-depth"
      (intSuffixM (\n d ->
       do { deprecate $ "use -freduction-depth=" ++ show n ++ " instead"
          ; return $ d { reductionDepth = treatZeroAsInf n } })))
  , make_ord_flag defFlag "fstrictness-before"
      (intSuffix (\n d -> d { strictnessBefore = n : strictnessBefore d }))
  , make_ord_flag defFlag "ffloat-lam-args"
      (intSuffix (\n d -> d { floatLamArgs = Just n }))
  , make_ord_flag defFlag "ffloat-all-lams"
      (noArg (\d -> d { floatLamArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-rec-args"
      (intSuffix (\n d -> d { liftLamsRecArgs = Just n }))
  , make_ord_flag defFlag "fstg-lift-lams-rec-args-any"
      (noArg (\d -> d { liftLamsRecArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-non-rec-args"
      (intSuffix (\n d -> d { liftLamsRecArgs = Just n }))
  , make_ord_flag defFlag "fstg-lift-lams-non-rec-args-any"
      (noArg (\d -> d { liftLamsRecArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-known"
      (noArg (\d -> d { liftLamsKnown = True }))
  , make_ord_flag defFlag "fno-stg-lift-lams-known"
      (noArg (\d -> d { liftLamsKnown = False }))
  , make_ord_flag defFlag "fproc-alignment"
      (intSuffix (\n d -> d { cmmProcAlignment = Just n }))
  , make_ord_flag defFlag "fblock-layout-weights"
        (HasArg (\s ->
            upd (\d -> d { cfgWeightInfo =
                parseCfgWeights s (cfgWeightInfo d)})))
  , make_ord_flag defFlag "fhistory-size"
      (intSuffix (\n d -> d { historySize = n }))
  , make_ord_flag defFlag "funfolding-creation-threshold"
      (intSuffix   (\n d -> d {ufCreationThreshold = n}))
  , make_ord_flag defFlag "funfolding-use-threshold"
      (intSuffix   (\n d -> d {ufUseThreshold = n}))
  , make_ord_flag defFlag "funfolding-fun-discount"
      (intSuffix   (\n d -> d {ufFunAppDiscount = n}))
  , make_ord_flag defFlag "funfolding-dict-discount"
      (intSuffix   (\n d -> d {ufDictDiscount = n}))
  , make_ord_flag defFlag "funfolding-keeness-factor"
      (floatSuffix (\n d -> d {ufKeenessFactor = n}))
  , make_ord_flag defFlag "fmax-worker-args"
      (intSuffix (\n d -> d {maxWorkerArgs = n}))
  , make_ord_flag defGhciFlag "fghci-hist-size"
      (intSuffix (\n d -> d {ghciHistSize = n}))
  , make_ord_flag defGhcFlag "fmax-inline-alloc-size"
      (intSuffix (\n d -> d { maxInlineAllocSize = n }))
  , make_ord_flag defGhcFlag "fmax-inline-memcpy-insns"
      (intSuffix (\n d -> d { maxInlineMemcpyInsns = n }))
  , make_ord_flag defGhcFlag "fmax-inline-memset-insns"
      (intSuffix (\n d -> d { maxInlineMemsetInsns = n }))
  , make_ord_flag defGhcFlag "dinitial-unique"
      (intSuffix (\n d -> d { initialUnique = n }))
  , make_ord_flag defGhcFlag "dunique-increment"
      (intSuffix (\n d -> d { uniqueIncrement = n }))

        ------ Profiling ----------------------------------------------------

        -- OLD profiling flags
  , make_dep_flag defGhcFlag "auto-all"
                    (noArg (\d -> d { profAuto = ProfAutoAll } ))
                    "Use -fprof-auto instead"
  , make_dep_flag defGhcFlag "no-auto-all"
                    (noArg (\d -> d { profAuto = NoProfAuto } ))
                    "Use -fno-prof-auto instead"
  , make_dep_flag defGhcFlag "auto"
                    (noArg (\d -> d { profAuto = ProfAutoExports } ))
                    "Use -fprof-auto-exported instead"
  , make_dep_flag defGhcFlag "no-auto"
            (noArg (\d -> d { profAuto = NoProfAuto } ))
                    "Use -fno-prof-auto instead"
  , make_dep_flag defGhcFlag "caf-all"
            (NoArg (setGeneralFlag Opt_AutoSccsOnIndividualCafs))
                    "Use -fprof-cafs instead"
  , make_dep_flag defGhcFlag "no-caf-all"
            (NoArg (unSetGeneralFlag Opt_AutoSccsOnIndividualCafs))
                    "Use -fno-prof-cafs instead"

        -- NEW profiling flags
  , make_ord_flag defGhcFlag "fprof-auto"
      (noArg (\d -> d { profAuto = ProfAutoAll } ))
  , make_ord_flag defGhcFlag "fprof-auto-top"
      (noArg (\d -> d { profAuto = ProfAutoTop } ))
  , make_ord_flag defGhcFlag "fprof-auto-exported"
      (noArg (\d -> d { profAuto = ProfAutoExports } ))
  , make_ord_flag defGhcFlag "fprof-auto-calls"
      (noArg (\d -> d { profAuto = ProfAutoCalls } ))
  , make_ord_flag defGhcFlag "fno-prof-auto"
      (noArg (\d -> d { profAuto = NoProfAuto } ))

        ------ Compiler flags -----------------------------------------------

  , make_ord_flag defGhcFlag "fasm"             (NoArg (setObjTarget HscAsm))
  , make_ord_flag defGhcFlag "fvia-c"           (NoArg
         (deprecate $ "The -fvia-c flag does nothing; " ++
                      "it will be removed in a future GHC release"))
  , make_ord_flag defGhcFlag "fvia-C"           (NoArg
         (deprecate $ "The -fvia-C flag does nothing; " ++
                      "it will be removed in a future GHC release"))
  , make_ord_flag defGhcFlag "fllvm"            (NoArg (setObjTarget HscLlvm))

  , make_ord_flag defFlag "fno-code"         (NoArg ((upd $ \d ->
                  d { ghcLink=NoLink }) >> setTarget HscNothing))
  , make_ord_flag defFlag "fbyte-code"
      (noArgM $ \dflags -> do
        setTarget HscInterpreted
        pure $ gopt_set dflags Opt_ByteCode)
  , make_ord_flag defFlag "fobject-code"     $ NoArg $ do
      dflags <- liftEwM getCmdLineState
      setTarget $ defaultObjectTarget dflags

  , make_dep_flag defFlag "fglasgow-exts"
      (NoArg enableGlasgowExts) "Use individual extensions instead"
  , make_dep_flag defFlag "fno-glasgow-exts"
      (NoArg disableGlasgowExts) "Use individual extensions instead"
  , make_ord_flag defFlag "Wunused-binds" (NoArg enableUnusedBinds)
  , make_ord_flag defFlag "Wno-unused-binds" (NoArg disableUnusedBinds)
  , make_ord_flag defHiddenFlag "fwarn-unused-binds" (NoArg enableUnusedBinds)
  , make_ord_flag defHiddenFlag "fno-warn-unused-binds" (NoArg
                                                            disableUnusedBinds)

        ------ Safe Haskell flags -------------------------------------------
  , make_ord_flag defFlag "fpackage-trust"   (NoArg setPackageTrust)
  , make_ord_flag defFlag "fno-safe-infer"   (noArg (\d ->
                                                    d { safeInfer = False }))
  , make_ord_flag defFlag "fno-safe-haskell" (NoArg (setSafeHaskell Sf_Ignore))

        ------ position independent flags  ----------------------------------
  , make_ord_flag defGhcFlag "fPIC"          (NoArg (setGeneralFlag Opt_PIC))
  , make_ord_flag defGhcFlag "fno-PIC"       (NoArg (unSetGeneralFlag Opt_PIC))
  , make_ord_flag defGhcFlag "fPIE"          (NoArg (setGeneralFlag Opt_PIC))
  , make_ord_flag defGhcFlag "fno-PIE"       (NoArg (unSetGeneralFlag Opt_PIC))

         ------ Debugging flags ----------------------------------------------
  , make_ord_flag defGhcFlag "g"             (OptIntSuffix setDebugLevel)
 ]
 ++ map (mkFlag turnOn  ""          setGeneralFlag    ) negatableFlagsDeps
 ++ map (mkFlag turnOff "no-"       unSetGeneralFlag  ) negatableFlagsDeps
 ++ map (mkFlag turnOn  "d"         setGeneralFlag    ) dFlagsDeps
 ++ map (mkFlag turnOff "dno-"      unSetGeneralFlag  ) dFlagsDeps
 ++ map (mkFlag turnOn  "f"         setGeneralFlag    ) fFlagsDeps
 ++ map (mkFlag turnOff "fno-"      unSetGeneralFlag  ) fFlagsDeps
 ++ map (mkFlag turnOn  "W"         setWarningFlag    ) wWarningFlagsDeps
 ++ map (mkFlag turnOff "Wno-"      unSetWarningFlag  ) wWarningFlagsDeps
 ++ map (mkFlag turnOn  "Werror="   setWErrorFlag )     wWarningFlagsDeps
 ++ map (mkFlag turnOn  "Wwarn="     unSetFatalWarningFlag )
                                                        wWarningFlagsDeps
 ++ map (mkFlag turnOn  "Wno-error=" unSetFatalWarningFlag )
                                                        wWarningFlagsDeps
 ++ map (mkFlag turnOn  "fwarn-"    setWarningFlag   . hideFlag)
    wWarningFlagsDeps
 ++ map (mkFlag turnOff "fno-warn-" unSetWarningFlag . hideFlag)
    wWarningFlagsDeps
 ++ [ (NotDeprecated, unrecognisedWarning "W"),
      (Deprecated,    unrecognisedWarning "fwarn-"),
      (Deprecated,    unrecognisedWarning "fno-warn-") ]
 ++ [ make_ord_flag defFlag "Werror=compat"
        (NoArg (mapM_ setWErrorFlag minusWcompatOpts))
    , make_ord_flag defFlag "Wno-error=compat"
        (NoArg (mapM_ unSetFatalWarningFlag minusWcompatOpts))
    , make_ord_flag defFlag "Wwarn=compat"
        (NoArg (mapM_ unSetFatalWarningFlag minusWcompatOpts)) ]
 ++ map (mkFlag turnOn  "f"         setExtensionFlag  ) fLangFlagsDeps
 ++ map (mkFlag turnOff "fno-"      unSetExtensionFlag) fLangFlagsDeps
 ++ map (mkFlag turnOn  "X"         setExtensionFlag  ) xFlagsDeps
 ++ map (mkFlag turnOff "XNo"       unSetExtensionFlag) xFlagsDeps
 ++ map (mkFlag turnOn  "X"         setLanguage       ) languageFlagsDeps
 ++ map (mkFlag turnOn  "X"         setSafeHaskell    ) safeHaskellFlagsDeps
 ++ [ make_dep_flag defFlag "XGenerics"
        (NoArg $ return ())
                  ("it does nothing; look into -XDefaultSignatures " ++
                   "and -XDeriveGeneric for generic programming support.")
    , make_dep_flag defFlag "XNoGenerics"
        (NoArg $ return ())
               ("it does nothing; look into -XDefaultSignatures and " ++
                  "-XDeriveGeneric for generic programming support.") ]

-- | This is where we handle unrecognised warning flags. We only issue a warning
-- if -Wunrecognised-warning-flags is set. See #11429 for context.
unrecognisedWarning :: String -> Flag (CmdLineP DynFlags)
unrecognisedWarning prefix = defHiddenFlag prefix (Prefix action)
  where
    action :: String -> EwM (CmdLineP DynFlags) ()
    action flag = do
      f <- wopt Opt_WarnUnrecognisedWarningFlags <$> liftEwM getCmdLineState
      when f $ addFlagWarn Cmd.ReasonUnrecognisedFlag $
        "unrecognised warning flag: -" ++ prefix ++ flag

-- See Note [Supporting CLI completion]
package_flags_deps :: [(Deprecation, Flag (CmdLineP DynFlags))]
package_flags_deps = [
        ------- Packages ----------------------------------------------------
    make_ord_flag defFlag "package-db"
      (HasArg (addPkgDbRef . PkgDbPath))
  , make_ord_flag defFlag "clear-package-db"      (NoArg clearPkgDb)
  , make_ord_flag defFlag "no-global-package-db"  (NoArg removeGlobalPkgDb)
  , make_ord_flag defFlag "no-user-package-db"    (NoArg removeUserPkgDb)
  , make_ord_flag defFlag "global-package-db"
      (NoArg (addPkgDbRef GlobalPkgDb))
  , make_ord_flag defFlag "user-package-db"
      (NoArg (addPkgDbRef UserPkgDb))
    -- backwards compat with GHC<=7.4 :
  , make_dep_flag defFlag "package-conf"
      (HasArg $ addPkgDbRef . PkgDbPath) "Use -package-db instead"
  , make_dep_flag defFlag "no-user-package-conf"
      (NoArg removeUserPkgDb)              "Use -no-user-package-db instead"
  , make_ord_flag defGhcFlag "package-name"       (HasArg $ \name -> do
                                      upd (setUnitId name))
                                      -- TODO: Since we JUST deprecated
                                      -- -this-package-key, let's keep this
                                      -- undeprecated for another cycle.
                                      -- Deprecate this eventually.
                                      -- deprecate "Use -this-unit-id instead")
  , make_dep_flag defGhcFlag "this-package-key"   (HasArg $ upd . setUnitId)
                                                  "Use -this-unit-id instead"
  , make_ord_flag defGhcFlag "this-unit-id"       (hasArg setUnitId)
  , make_ord_flag defFlag "package"               (HasArg exposePackage)
  , make_ord_flag defFlag "plugin-package-id"     (HasArg exposePluginPackageId)
  , make_ord_flag defFlag "plugin-package"        (HasArg exposePluginPackage)
  , make_ord_flag defFlag "package-id"            (HasArg exposePackageId)
  , make_ord_flag defFlag "hide-package"          (HasArg hidePackage)
  , make_ord_flag defFlag "hide-all-packages"
      (NoArg (setGeneralFlag Opt_HideAllPackages))
  , make_ord_flag defFlag "hide-all-plugin-packages"
      (NoArg (setGeneralFlag Opt_HideAllPluginPackages))
  , make_ord_flag defFlag "package-env"           (HasArg setPackageEnv)
  , make_ord_flag defFlag "ignore-package"        (HasArg ignorePackage)
  , make_dep_flag defFlag "syslib" (HasArg exposePackage) "Use -package instead"
  , make_ord_flag defFlag "distrust-all-packages"
      (NoArg (setGeneralFlag Opt_DistrustAllPackages))
  , make_ord_flag defFlag "trust"                 (HasArg trustPackage)
  , make_ord_flag defFlag "distrust"              (HasArg distrustPackage)
  ]
  where
    setPackageEnv env = upd $ \s -> s { packageEnv = Just env }

-- | Make a list of flags for shell completion.
-- Filter all available flags into two groups, for interactive GHC vs all other.
flagsForCompletion :: Bool -> [String]
flagsForCompletion isInteractive
    = [ '-':flagName flag
      | flag <- flagsAll
      , modeFilter (flagGhcMode flag)
      ]
    where
      modeFilter AllModes = True
      modeFilter OnlyGhci = isInteractive
      modeFilter OnlyGhc = not isInteractive
      modeFilter HiddenFlag = False

type TurnOnFlag = Bool   -- True  <=> we are turning the flag on
                         -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn  = True
turnOff :: TurnOnFlag; turnOff = False

data FlagSpec flag
   = FlagSpec
       { flagSpecName :: String   -- ^ Flag in string form
       , flagSpecFlag :: flag     -- ^ Flag in internal form
       , flagSpecAction :: (TurnOnFlag -> DynP ())
           -- ^ Extra action to run when the flag is found
           -- Typically, emit a warning or error
       , flagSpecGhcMode :: GhcFlagMode
           -- ^ In which ghc mode the flag has effect
       }

-- | Define a new flag.
flagSpec :: String -> flag -> (Deprecation, FlagSpec flag)
flagSpec name flag = flagSpec' name flag nop

-- | Define a new flag with an effect.
flagSpec' :: String -> flag -> (TurnOnFlag -> DynP ())
          -> (Deprecation, FlagSpec flag)
flagSpec' name flag act = (NotDeprecated, FlagSpec name flag act AllModes)

-- | Define a new deprecated flag with an effect.
depFlagSpecOp :: String -> flag -> (TurnOnFlag -> DynP ()) -> String
            -> (Deprecation, FlagSpec flag)
depFlagSpecOp name flag act dep =
    (Deprecated, snd (flagSpec' name flag (\f -> act f >> deprecate dep)))

-- | Define a new deprecated flag.
depFlagSpec :: String -> flag -> String
            -> (Deprecation, FlagSpec flag)
depFlagSpec name flag dep = depFlagSpecOp name flag nop dep

-- | Define a new deprecated flag with an effect where the deprecation message
-- depends on the flag value
depFlagSpecOp' :: String
             -> flag
             -> (TurnOnFlag -> DynP ())
             -> (TurnOnFlag -> String)
             -> (Deprecation, FlagSpec flag)
depFlagSpecOp' name flag act dep =
    (Deprecated, FlagSpec name flag (\f -> act f >> (deprecate $ dep f))
                                                                       AllModes)

-- | Define a new deprecated flag where the deprecation message
-- depends on the flag value
depFlagSpec' :: String
             -> flag
             -> (TurnOnFlag -> String)
             -> (Deprecation, FlagSpec flag)
depFlagSpec' name flag dep = depFlagSpecOp' name flag nop dep


-- | Define a new deprecated flag where the deprecation message
-- is shown depending on the flag value
depFlagSpecCond :: String
                -> flag
                -> (TurnOnFlag -> Bool)
                -> String
                -> (Deprecation, FlagSpec flag)
depFlagSpecCond name flag cond dep =
    (Deprecated, FlagSpec name flag (\f -> when (cond f) $ deprecate dep)
                                                                       AllModes)

-- | Define a new flag for GHCi.
flagGhciSpec :: String -> flag -> (Deprecation, FlagSpec flag)
flagGhciSpec name flag = flagGhciSpec' name flag nop

-- | Define a new flag for GHCi with an effect.
flagGhciSpec' :: String -> flag -> (TurnOnFlag -> DynP ())
              -> (Deprecation, FlagSpec flag)
flagGhciSpec' name flag act = (NotDeprecated, FlagSpec name flag act OnlyGhci)

-- | Define a new flag invisible to CLI completion.
flagHiddenSpec :: String -> flag -> (Deprecation, FlagSpec flag)
flagHiddenSpec name flag = flagHiddenSpec' name flag nop

-- | Define a new flag invisible to CLI completion with an effect.
flagHiddenSpec' :: String -> flag -> (TurnOnFlag -> DynP ())
                -> (Deprecation, FlagSpec flag)
flagHiddenSpec' name flag act = (NotDeprecated, FlagSpec name flag act
                                                                     HiddenFlag)

-- | Hide a 'FlagSpec' from being displayed in @--show-options@.
--
-- This is for example useful for flags that are obsolete, but should not
-- (yet) be deprecated for compatibility reasons.
hideFlag :: (Deprecation, FlagSpec a) -> (Deprecation, FlagSpec a)
hideFlag (dep, fs) = (dep, fs { flagSpecGhcMode = HiddenFlag })

mkFlag :: TurnOnFlag            -- ^ True <=> it should be turned on
       -> String                -- ^ The flag prefix
       -> (flag -> DynP ())     -- ^ What to do when the flag is found
       -> (Deprecation, FlagSpec flag)  -- ^ Specification of
                                        -- this particular flag
       -> (Deprecation, Flag (CmdLineP DynFlags))
mkFlag turn_on flagPrefix f (dep, (FlagSpec name flag extra_action mode))
    = (dep,
       Flag (flagPrefix ++ name) (NoArg (f flag >> extra_action turn_on)) mode)

deprecatedForExtension :: String -> TurnOnFlag -> String
deprecatedForExtension lang turn_on
    = "use -X" ++ flag ++
      " or pragma {-# LANGUAGE " ++ flag ++ " #-} instead"
    where
      flag | turn_on   = lang
           | otherwise = "No" ++ lang

useInstead :: String -> String -> TurnOnFlag -> String
useInstead prefix flag turn_on
  = "Use " ++ prefix ++ no ++ flag ++ " instead"
  where
    no = if turn_on then "" else "no-"

nop :: TurnOnFlag -> DynP ()
nop _ = return ()

-- | Find the 'FlagSpec' for a 'WarningFlag'.
flagSpecOf :: WarningFlag -> Maybe (FlagSpec WarningFlag)
flagSpecOf flag = listToMaybe $ filter check wWarningFlags
  where
    check fs = flagSpecFlag fs == flag

-- | These @-W\<blah\>@ flags can all be reversed with @-Wno-\<blah\>@
wWarningFlags :: [FlagSpec WarningFlag]
wWarningFlags = map snd (sortBy (comparing fst) wWarningFlagsDeps)

wWarningFlagsDeps :: [(Deprecation, FlagSpec WarningFlag)]
wWarningFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- Please keep the list of flags below sorted alphabetically
  flagSpec "alternative-layout-rule-transitional"
                                      Opt_WarnAlternativeLayoutRuleTransitional,
  depFlagSpec "auto-orphans"             Opt_WarnAutoOrphans
    "it has no effect",
  flagSpec "cpp-undef"                   Opt_WarnCPPUndef,
  flagSpec "unbanged-strict-patterns"    Opt_WarnUnbangedStrictPatterns,
  flagSpec "deferred-type-errors"        Opt_WarnDeferredTypeErrors,
  flagSpec "deferred-out-of-scope-variables"
                                         Opt_WarnDeferredOutOfScopeVariables,
  flagSpec "deprecations"                Opt_WarnWarningsDeprecations,
  flagSpec "deprecated-flags"            Opt_WarnDeprecatedFlags,
  flagSpec "deriving-defaults"           Opt_WarnDerivingDefaults,
  flagSpec "deriving-typeable"           Opt_WarnDerivingTypeable,
  flagSpec "dodgy-exports"               Opt_WarnDodgyExports,
  flagSpec "dodgy-foreign-imports"       Opt_WarnDodgyForeignImports,
  flagSpec "dodgy-imports"               Opt_WarnDodgyImports,
  flagSpec "empty-enumerations"          Opt_WarnEmptyEnumerations,
  depFlagSpec "duplicate-constraints"    Opt_WarnDuplicateConstraints
    "it is subsumed by -Wredundant-constraints",
  flagSpec "redundant-constraints"       Opt_WarnRedundantConstraints,
  flagSpec "duplicate-exports"           Opt_WarnDuplicateExports,
  depFlagSpec "hi-shadowing"                Opt_WarnHiShadows
    "it is not used, and was never implemented",
  flagSpec "inaccessible-code"           Opt_WarnInaccessibleCode,
  flagSpec "implicit-prelude"            Opt_WarnImplicitPrelude,
  depFlagSpec "implicit-kind-vars"       Opt_WarnImplicitKindVars
    "it is now an error",
  flagSpec "incomplete-patterns"         Opt_WarnIncompletePatterns,
  flagSpec "incomplete-record-updates"   Opt_WarnIncompletePatternsRecUpd,
  flagSpec "incomplete-uni-patterns"     Opt_WarnIncompleteUniPatterns,
  flagSpec "inline-rule-shadowing"       Opt_WarnInlineRuleShadowing,
  flagSpec "identities"                  Opt_WarnIdentities,
  flagSpec "missing-fields"              Opt_WarnMissingFields,
  flagSpec "missing-import-lists"        Opt_WarnMissingImportList,
  flagSpec "missing-export-lists"        Opt_WarnMissingExportList,
  depFlagSpec "missing-local-sigs"       Opt_WarnMissingLocalSignatures
    "it is replaced by -Wmissing-local-signatures",
  flagSpec "missing-local-signatures"    Opt_WarnMissingLocalSignatures,
  flagSpec "missing-methods"             Opt_WarnMissingMethods,
  flagSpec "missing-monadfail-instances" Opt_WarnMissingMonadFailInstances,
  flagSpec "semigroup"                   Opt_WarnSemigroup,
  flagSpec "missing-signatures"          Opt_WarnMissingSignatures,
  depFlagSpec "missing-exported-sigs"    Opt_WarnMissingExportedSignatures
    "it is replaced by -Wmissing-exported-signatures",
  flagSpec "missing-exported-signatures" Opt_WarnMissingExportedSignatures,
  flagSpec "monomorphism-restriction"    Opt_WarnMonomorphism,
  flagSpec "name-shadowing"              Opt_WarnNameShadowing,
  flagSpec "noncanonical-monad-instances"
                                         Opt_WarnNonCanonicalMonadInstances,
  depFlagSpec "noncanonical-monadfail-instances"
                                         Opt_WarnNonCanonicalMonadInstances
    "fail is no longer a method of Monad",
  flagSpec "noncanonical-monoid-instances"
                                         Opt_WarnNonCanonicalMonoidInstances,
  flagSpec "orphans"                     Opt_WarnOrphans,
  flagSpec "overflowed-literals"         Opt_WarnOverflowedLiterals,
  flagSpec "overlapping-patterns"        Opt_WarnOverlappingPatterns,
  flagSpec "missed-specialisations"      Opt_WarnMissedSpecs,
  flagSpec "missed-specializations"      Opt_WarnMissedSpecs,
  flagSpec "all-missed-specialisations"  Opt_WarnAllMissedSpecs,
  flagSpec "all-missed-specializations"  Opt_WarnAllMissedSpecs,
  flagSpec' "safe"                       Opt_WarnSafe setWarnSafe,
  flagSpec "trustworthy-safe"            Opt_WarnTrustworthySafe,
  flagSpec "inferred-safe-imports"       Opt_WarnInferredSafeImports,
  flagSpec "missing-safe-haskell-mode"   Opt_WarnMissingSafeHaskellMode,
  flagSpec "tabs"                        Opt_WarnTabs,
  flagSpec "type-defaults"               Opt_WarnTypeDefaults,
  flagSpec "typed-holes"                 Opt_WarnTypedHoles,
  flagSpec "partial-type-signatures"     Opt_WarnPartialTypeSignatures,
  flagSpec "unrecognised-pragmas"        Opt_WarnUnrecognisedPragmas,
  flagSpec' "unsafe"                     Opt_WarnUnsafe setWarnUnsafe,
  flagSpec "unsupported-calling-conventions"
                                         Opt_WarnUnsupportedCallingConventions,
  flagSpec "unsupported-llvm-version"    Opt_WarnUnsupportedLlvmVersion,
  flagSpec "missed-extra-shared-lib"     Opt_WarnMissedExtraSharedLib,
  flagSpec "unticked-promoted-constructors"
                                         Opt_WarnUntickedPromotedConstructors,
  flagSpec "unused-do-bind"              Opt_WarnUnusedDoBind,
  flagSpec "unused-foralls"              Opt_WarnUnusedForalls,
  flagSpec "unused-imports"              Opt_WarnUnusedImports,
  flagSpec "unused-local-binds"          Opt_WarnUnusedLocalBinds,
  flagSpec "unused-matches"              Opt_WarnUnusedMatches,
  flagSpec "unused-pattern-binds"        Opt_WarnUnusedPatternBinds,
  flagSpec "unused-top-binds"            Opt_WarnUnusedTopBinds,
  flagSpec "unused-type-patterns"        Opt_WarnUnusedTypePatterns,
  flagSpec "unused-record-wildcards"     Opt_WarnUnusedRecordWildcards,
  flagSpec "redundant-record-wildcards"  Opt_WarnRedundantRecordWildcards,
  flagSpec "warnings-deprecations"       Opt_WarnWarningsDeprecations,
  flagSpec "wrong-do-bind"               Opt_WarnWrongDoBind,
  flagSpec "missing-pattern-synonym-signatures"
                                    Opt_WarnMissingPatternSynonymSignatures,
  flagSpec "missing-deriving-strategies" Opt_WarnMissingDerivingStrategies,
  flagSpec "simplifiable-class-constraints" Opt_WarnSimplifiableClassConstraints,
  flagSpec "missing-home-modules"        Opt_WarnMissingHomeModules,
  flagSpec "unrecognised-warning-flags"  Opt_WarnUnrecognisedWarningFlags,
  flagSpec "star-binder"                 Opt_WarnStarBinder,
  flagSpec "star-is-type"                Opt_WarnStarIsType,
  depFlagSpec "missing-space-after-bang" Opt_WarnSpaceAfterBang
    "bang patterns can no longer be written with a space",
  flagSpec "partial-fields"              Opt_WarnPartialFields,
  flagSpec "prepositive-qualified-module"
                                         Opt_WarnPrepositiveQualifiedModule,
  flagSpec "unused-packages"             Opt_WarnUnusedPackages,
  flagSpec "compat-unqualified-imports"  Opt_WarnCompatUnqualifiedImports
 ]

-- | These @-\<blah\>@ flags can all be reversed with @-no-\<blah\>@
negatableFlagsDeps :: [(Deprecation, FlagSpec GeneralFlag)]
negatableFlagsDeps = [
  flagGhciSpec "ignore-dot-ghci"         Opt_IgnoreDotGhci ]

-- | These @-d\<blah\>@ flags can all be reversed with @-dno-\<blah\>@
dFlagsDeps :: [(Deprecation, FlagSpec GeneralFlag)]
dFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- Please keep the list of flags below sorted alphabetically
  flagSpec "ppr-case-as-let"            Opt_PprCaseAsLet,
  depFlagSpec' "ppr-ticks"              Opt_PprShowTicks
     (\turn_on -> useInstead "-d" "suppress-ticks" (not turn_on)),
  flagSpec "suppress-ticks"             Opt_SuppressTicks,
  depFlagSpec' "suppress-stg-free-vars" Opt_SuppressStgExts
     (useInstead "-d" "suppress-stg-exts"),
  flagSpec "suppress-stg-exts"          Opt_SuppressStgExts,
  flagSpec "suppress-coercions"         Opt_SuppressCoercions,
  flagSpec "suppress-idinfo"            Opt_SuppressIdInfo,
  flagSpec "suppress-unfoldings"        Opt_SuppressUnfoldings,
  flagSpec "suppress-module-prefixes"   Opt_SuppressModulePrefixes,
  flagSpec "suppress-timestamps"        Opt_SuppressTimestamps,
  flagSpec "suppress-type-applications" Opt_SuppressTypeApplications,
  flagSpec "suppress-type-signatures"   Opt_SuppressTypeSignatures,
  flagSpec "suppress-uniques"           Opt_SuppressUniques,
  flagSpec "suppress-var-kinds"         Opt_SuppressVarKinds
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fFlags :: [FlagSpec GeneralFlag]
fFlags = map snd fFlagsDeps

fFlagsDeps :: [(Deprecation, FlagSpec GeneralFlag)]
fFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- Please keep the list of flags below sorted alphabetically
  flagSpec "asm-shortcutting"                 Opt_AsmShortcutting,
  flagGhciSpec "break-on-error"               Opt_BreakOnError,
  flagGhciSpec "break-on-exception"           Opt_BreakOnException,
  flagSpec "building-cabal-package"           Opt_BuildingCabalPackage,
  flagSpec "call-arity"                       Opt_CallArity,
  flagSpec "exitification"                    Opt_Exitification,
  flagSpec "case-merge"                       Opt_CaseMerge,
  flagSpec "case-folding"                     Opt_CaseFolding,
  flagSpec "cmm-elim-common-blocks"           Opt_CmmElimCommonBlocks,
  flagSpec "cmm-sink"                         Opt_CmmSink,
  flagSpec "cse"                              Opt_CSE,
  flagSpec "stg-cse"                          Opt_StgCSE,
  flagSpec "stg-lift-lams"                    Opt_StgLiftLams,
  flagSpec "cpr-anal"                         Opt_CprAnal,
  flagSpec "defer-diagnostics"                Opt_DeferDiagnostics,
  flagSpec "defer-type-errors"                Opt_DeferTypeErrors,
  flagSpec "defer-typed-holes"                Opt_DeferTypedHoles,
  flagSpec "defer-out-of-scope-variables"     Opt_DeferOutOfScopeVariables,
  flagSpec "diagnostics-show-caret"           Opt_DiagnosticsShowCaret,
  flagSpec "dicts-cheap"                      Opt_DictsCheap,
  flagSpec "dicts-strict"                     Opt_DictsStrict,
  flagSpec "dmd-tx-dict-sel"                  Opt_DmdTxDictSel,
  flagSpec "do-eta-reduction"                 Opt_DoEtaReduction,
  flagSpec "do-lambda-eta-expansion"          Opt_DoLambdaEtaExpansion,
  flagSpec "eager-blackholing"                Opt_EagerBlackHoling,
  flagSpec "embed-manifest"                   Opt_EmbedManifest,
  flagSpec "enable-rewrite-rules"             Opt_EnableRewriteRules,
  flagSpec "enable-th-splice-warnings"        Opt_EnableThSpliceWarnings,
  flagSpec "error-spans"                      Opt_ErrorSpans,
  flagSpec "excess-precision"                 Opt_ExcessPrecision,
  flagSpec "expose-all-unfoldings"            Opt_ExposeAllUnfoldings,
  flagSpec "external-dynamic-refs"            Opt_ExternalDynamicRefs,
  flagSpec "external-interpreter"             Opt_ExternalInterpreter,
  flagSpec "flat-cache"                       Opt_FlatCache,
  flagSpec "float-in"                         Opt_FloatIn,
  flagSpec "force-recomp"                     Opt_ForceRecomp,
  flagSpec "ignore-optim-changes"             Opt_IgnoreOptimChanges,
  flagSpec "ignore-hpc-changes"               Opt_IgnoreHpcChanges,
  flagSpec "full-laziness"                    Opt_FullLaziness,
  flagSpec "fun-to-thunk"                     Opt_FunToThunk,
  flagSpec "gen-manifest"                     Opt_GenManifest,
  flagSpec "ghci-history"                     Opt_GhciHistory,
  flagSpec "ghci-leak-check"                  Opt_GhciLeakCheck,
  flagSpec "validate-ide-info"                Opt_ValidateHie,
  flagGhciSpec "local-ghci-history"           Opt_LocalGhciHistory,
  flagGhciSpec "no-it"                        Opt_NoIt,
  flagSpec "ghci-sandbox"                     Opt_GhciSandbox,
  flagSpec "helpful-errors"                   Opt_HelpfulErrors,
  flagSpec "hpc"                              Opt_Hpc,
  flagSpec "ignore-asserts"                   Opt_IgnoreAsserts,
  flagSpec "ignore-interface-pragmas"         Opt_IgnoreInterfacePragmas,
  flagGhciSpec "implicit-import-qualified"    Opt_ImplicitImportQualified,
  flagSpec "irrefutable-tuples"               Opt_IrrefutableTuples,
  flagSpec "keep-going"                       Opt_KeepGoing,
  flagSpec "late-dmd-anal"                    Opt_LateDmdAnal,
  flagSpec "late-specialise"                  Opt_LateSpecialise,
  flagSpec "liberate-case"                    Opt_LiberateCase,
  flagHiddenSpec "llvm-tbaa"                  Opt_LlvmTBAA,
  flagHiddenSpec "llvm-fill-undef-with-garbage" Opt_LlvmFillUndefWithGarbage,
  flagSpec "loopification"                    Opt_Loopification,
  flagSpec "block-layout-cfg"                 Opt_CfgBlocklayout,
  flagSpec "block-layout-weightless"          Opt_WeightlessBlocklayout,
  flagSpec "omit-interface-pragmas"           Opt_OmitInterfacePragmas,
  flagSpec "omit-yields"                      Opt_OmitYields,
  flagSpec "optimal-applicative-do"           Opt_OptimalApplicativeDo,
  flagSpec "pedantic-bottoms"                 Opt_PedanticBottoms,
  flagSpec "pre-inlining"                     Opt_SimplPreInlining,
  flagGhciSpec "print-bind-contents"          Opt_PrintBindContents,
  flagGhciSpec "print-bind-result"            Opt_PrintBindResult,
  flagGhciSpec "print-evld-with-show"         Opt_PrintEvldWithShow,
  flagSpec "print-explicit-foralls"           Opt_PrintExplicitForalls,
  flagSpec "print-explicit-kinds"             Opt_PrintExplicitKinds,
  flagSpec "print-explicit-coercions"         Opt_PrintExplicitCoercions,
  flagSpec "print-explicit-runtime-reps"      Opt_PrintExplicitRuntimeReps,
  flagSpec "print-equality-relations"         Opt_PrintEqualityRelations,
  flagSpec "print-axiom-incomps"              Opt_PrintAxiomIncomps,
  flagSpec "print-unicode-syntax"             Opt_PrintUnicodeSyntax,
  flagSpec "print-expanded-synonyms"          Opt_PrintExpandedSynonyms,
  flagSpec "print-potential-instances"        Opt_PrintPotentialInstances,
  flagSpec "print-typechecker-elaboration"    Opt_PrintTypecheckerElaboration,
  flagSpec "prof-cafs"                        Opt_AutoSccsOnIndividualCafs,
  flagSpec "prof-count-entries"               Opt_ProfCountEntries,
  flagSpec "regs-graph"                       Opt_RegsGraph,
  flagSpec "regs-iterative"                   Opt_RegsIterative,
  depFlagSpec' "rewrite-rules"                Opt_EnableRewriteRules
   (useInstead "-f" "enable-rewrite-rules"),
  flagSpec "shared-implib"                    Opt_SharedImplib,
  flagSpec "spec-constr"                      Opt_SpecConstr,
  flagSpec "spec-constr-keen"                 Opt_SpecConstrKeen,
  flagSpec "specialise"                       Opt_Specialise,
  flagSpec "specialize"                       Opt_Specialise,
  flagSpec "specialise-aggressively"          Opt_SpecialiseAggressively,
  flagSpec "specialize-aggressively"          Opt_SpecialiseAggressively,
  flagSpec "cross-module-specialise"          Opt_CrossModuleSpecialise,
  flagSpec "cross-module-specialize"          Opt_CrossModuleSpecialise,
  flagSpec "static-argument-transformation"   Opt_StaticArgumentTransformation,
  flagSpec "strictness"                       Opt_Strictness,
  flagSpec "use-rpaths"                       Opt_RPath,
  flagSpec "write-interface"                  Opt_WriteInterface,
  flagSpec "write-ide-info"                   Opt_WriteHie,
  flagSpec "unbox-small-strict-fields"        Opt_UnboxSmallStrictFields,
  flagSpec "unbox-strict-fields"              Opt_UnboxStrictFields,
  flagSpec "version-macros"                   Opt_VersionMacros,
  flagSpec "worker-wrapper"                   Opt_WorkerWrapper,
  flagSpec "solve-constant-dicts"             Opt_SolveConstantDicts,
  flagSpec "catch-bottoms"                    Opt_CatchBottoms,
  flagSpec "alignment-sanitisation"           Opt_AlignmentSanitisation,
  flagSpec "num-constant-folding"             Opt_NumConstantFolding,
  flagSpec "show-warning-groups"              Opt_ShowWarnGroups,
  flagSpec "hide-source-paths"                Opt_HideSourcePaths,
  flagSpec "show-loaded-modules"              Opt_ShowLoadedModules,
  flagSpec "whole-archive-hs-libs"            Opt_WholeArchiveHsLibs,
  flagSpec "keep-cafs"                        Opt_KeepCAFs
  ]
  ++ fHoleFlags

-- | These @-f\<blah\>@ flags have to do with the typed-hole error message or
-- the valid hole fits in that message. See Note [Valid hole fits include ...]
-- in the TcHoleErrors module. These flags can all be reversed with
-- @-fno-\<blah\>@
fHoleFlags :: [(Deprecation, FlagSpec GeneralFlag)]
fHoleFlags = [
  flagSpec "show-hole-constraints"            Opt_ShowHoleConstraints,
  depFlagSpec' "show-valid-substitutions"     Opt_ShowValidHoleFits
   (useInstead "-f" "show-valid-hole-fits"),
  flagSpec "show-valid-hole-fits"             Opt_ShowValidHoleFits,
  -- Sorting settings
  flagSpec "sort-valid-hole-fits"             Opt_SortValidHoleFits,
  flagSpec "sort-by-size-hole-fits"           Opt_SortBySizeHoleFits,
  flagSpec "sort-by-subsumption-hole-fits"    Opt_SortBySubsumHoleFits,
  flagSpec "abstract-refinement-hole-fits"    Opt_AbstractRefHoleFits,
  -- Output format settings
  flagSpec "show-hole-matches-of-hole-fits"   Opt_ShowMatchesOfHoleFits,
  flagSpec "show-provenance-of-hole-fits"     Opt_ShowProvOfHoleFits,
  flagSpec "show-type-of-hole-fits"           Opt_ShowTypeOfHoleFits,
  flagSpec "show-type-app-of-hole-fits"       Opt_ShowTypeAppOfHoleFits,
  flagSpec "show-type-app-vars-of-hole-fits"  Opt_ShowTypeAppVarsOfHoleFits,
  flagSpec "show-docs-of-hole-fits"           Opt_ShowDocsOfHoleFits,
  flagSpec "unclutter-valid-hole-fits"        Opt_UnclutterValidHoleFits
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fLangFlags :: [FlagSpec LangExt.Extension]
fLangFlags = map snd fLangFlagsDeps

fLangFlagsDeps :: [(Deprecation, FlagSpec LangExt.Extension)]
fLangFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
  depFlagSpecOp' "th"                           LangExt.TemplateHaskell
    checkTemplateHaskellOk
    (deprecatedForExtension "TemplateHaskell"),
  depFlagSpec' "fi"                             LangExt.ForeignFunctionInterface
    (deprecatedForExtension "ForeignFunctionInterface"),
  depFlagSpec' "ffi"                            LangExt.ForeignFunctionInterface
    (deprecatedForExtension "ForeignFunctionInterface"),
  depFlagSpec' "arrows"                         LangExt.Arrows
    (deprecatedForExtension "Arrows"),
  depFlagSpec' "implicit-prelude"               LangExt.ImplicitPrelude
    (deprecatedForExtension "ImplicitPrelude"),
  depFlagSpec' "bang-patterns"                  LangExt.BangPatterns
    (deprecatedForExtension "BangPatterns"),
  depFlagSpec' "monomorphism-restriction"       LangExt.MonomorphismRestriction
    (deprecatedForExtension "MonomorphismRestriction"),
  depFlagSpec' "mono-pat-binds"                 LangExt.MonoPatBinds
    (deprecatedForExtension "MonoPatBinds"),
  depFlagSpec' "extended-default-rules"         LangExt.ExtendedDefaultRules
    (deprecatedForExtension "ExtendedDefaultRules"),
  depFlagSpec' "implicit-params"                LangExt.ImplicitParams
    (deprecatedForExtension "ImplicitParams"),
  depFlagSpec' "scoped-type-variables"          LangExt.ScopedTypeVariables
    (deprecatedForExtension "ScopedTypeVariables"),
  depFlagSpec' "allow-overlapping-instances"    LangExt.OverlappingInstances
    (deprecatedForExtension "OverlappingInstances"),
  depFlagSpec' "allow-undecidable-instances"    LangExt.UndecidableInstances
    (deprecatedForExtension "UndecidableInstances"),
  depFlagSpec' "allow-incoherent-instances"     LangExt.IncoherentInstances
    (deprecatedForExtension "IncoherentInstances")
  ]

supportedLanguages :: [String]
supportedLanguages = map (flagSpecName . snd) languageFlagsDeps

supportedLanguageOverlays :: [String]
supportedLanguageOverlays = map (flagSpecName . snd) safeHaskellFlagsDeps

supportedExtensions :: PlatformMini -> [String]
supportedExtensions targetPlatformMini = concatMap toFlagSpecNamePair xFlags
  where
    toFlagSpecNamePair flg
      -- IMPORTANT! Make sure that `ghc --supported-extensions` omits
      -- "TemplateHaskell"/"QuasiQuotes" when it's known not to work out of the
      -- box. See also GHC #11102 and #16331 for more details about
      -- the rationale
      | isAIX, flagSpecFlag flg == LangExt.TemplateHaskell  = [noName]
      | isAIX, flagSpecFlag flg == LangExt.QuasiQuotes      = [noName]
      | otherwise = [name, noName]
      where
        isAIX = platformMini_os targetPlatformMini == OSAIX
        noName = "No" ++ name
        name = flagSpecName flg

supportedLanguagesAndExtensions :: PlatformMini -> [String]
supportedLanguagesAndExtensions targetPlatformMini =
    supportedLanguages ++ supportedLanguageOverlays ++ supportedExtensions targetPlatformMini

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
languageFlagsDeps :: [(Deprecation, FlagSpec Language)]
languageFlagsDeps = [
  flagSpec "Haskell98"   Haskell98,
  flagSpec "Haskell2010" Haskell2010
  ]

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
-- They are used to place hard requirements on what GHC Haskell language
-- features can be used.
safeHaskellFlagsDeps :: [(Deprecation, FlagSpec SafeHaskellMode)]
safeHaskellFlagsDeps = [mkF Sf_Unsafe, mkF Sf_Trustworthy, mkF Sf_Safe]
    where mkF flag = flagSpec (show flag) flag

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [FlagSpec LangExt.Extension]
xFlags = map snd xFlagsDeps

xFlagsDeps :: [(Deprecation, FlagSpec LangExt.Extension)]
xFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- See Note [Adding a language extension]
-- Please keep the list of flags below sorted alphabetically
  flagSpec "AllowAmbiguousTypes"              LangExt.AllowAmbiguousTypes,
  flagSpec "AlternativeLayoutRule"            LangExt.AlternativeLayoutRule,
  flagSpec "AlternativeLayoutRuleTransitional"
                                              LangExt.AlternativeLayoutRuleTransitional,
  flagSpec "Arrows"                           LangExt.Arrows,
  depFlagSpecCond "AutoDeriveTypeable"        LangExt.AutoDeriveTypeable
    id
         ("Typeable instances are created automatically " ++
                     "for all types since GHC 8.2."),
  flagSpec "BangPatterns"                     LangExt.BangPatterns,
  flagSpec "BinaryLiterals"                   LangExt.BinaryLiterals,
  flagSpec "CApiFFI"                          LangExt.CApiFFI,
  flagSpec "CPP"                              LangExt.Cpp,
  flagSpec "CUSKs"                            LangExt.CUSKs,
  flagSpec "ConstrainedClassMethods"          LangExt.ConstrainedClassMethods,
  flagSpec "ConstraintKinds"                  LangExt.ConstraintKinds,
  flagSpec "DataKinds"                        LangExt.DataKinds,
  depFlagSpecCond "DatatypeContexts"          LangExt.DatatypeContexts
    id
         ("It was widely considered a misfeature, " ++
                     "and has been removed from the Haskell language."),
  flagSpec "DefaultSignatures"                LangExt.DefaultSignatures,
  flagSpec "DeriveAnyClass"                   LangExt.DeriveAnyClass,
  flagSpec "DeriveDataTypeable"               LangExt.DeriveDataTypeable,
  flagSpec "DeriveFoldable"                   LangExt.DeriveFoldable,
  flagSpec "DeriveFunctor"                    LangExt.DeriveFunctor,
  flagSpec "DeriveGeneric"                    LangExt.DeriveGeneric,
  flagSpec "DeriveLift"                       LangExt.DeriveLift,
  flagSpec "DeriveTraversable"                LangExt.DeriveTraversable,
  flagSpec "DerivingStrategies"               LangExt.DerivingStrategies,
  flagSpec "DerivingVia"                      LangExt.DerivingVia,
  flagSpec "DisambiguateRecordFields"         LangExt.DisambiguateRecordFields,
  flagSpec "DoAndIfThenElse"                  LangExt.DoAndIfThenElse,
  flagSpec "BlockArguments"                   LangExt.BlockArguments,
  depFlagSpec' "DoRec"                        LangExt.RecursiveDo
    (deprecatedForExtension "RecursiveDo"),
  flagSpec "DuplicateRecordFields"            LangExt.DuplicateRecordFields,
  flagSpec "EmptyCase"                        LangExt.EmptyCase,
  flagSpec "EmptyDataDecls"                   LangExt.EmptyDataDecls,
  flagSpec "EmptyDataDeriving"                LangExt.EmptyDataDeriving,
  flagSpec "ExistentialQuantification"        LangExt.ExistentialQuantification,
  flagSpec "ExplicitForAll"                   LangExt.ExplicitForAll,
  flagSpec "ExplicitNamespaces"               LangExt.ExplicitNamespaces,
  flagSpec "ExtendedDefaultRules"             LangExt.ExtendedDefaultRules,
  flagSpec "FlexibleContexts"                 LangExt.FlexibleContexts,
  flagSpec "FlexibleInstances"                LangExt.FlexibleInstances,
  flagSpec "ForeignFunctionInterface"         LangExt.ForeignFunctionInterface,
  flagSpec "FunctionalDependencies"           LangExt.FunctionalDependencies,
  flagSpec "GADTSyntax"                       LangExt.GADTSyntax,
  flagSpec "GADTs"                            LangExt.GADTs,
  flagSpec "GHCForeignImportPrim"             LangExt.GHCForeignImportPrim,
  flagSpec' "GeneralizedNewtypeDeriving"      LangExt.GeneralizedNewtypeDeriving
                                              setGenDeriving,
  flagSpec' "GeneralisedNewtypeDeriving"      LangExt.GeneralizedNewtypeDeriving
                                              setGenDeriving,
  flagSpec "ImplicitParams"                   LangExt.ImplicitParams,
  flagSpec "ImplicitPrelude"                  LangExt.ImplicitPrelude,
  flagSpec "ImportQualifiedPost"              LangExt.ImportQualifiedPost,
  flagSpec "ImpredicativeTypes"               LangExt.ImpredicativeTypes,
  flagSpec' "IncoherentInstances"             LangExt.IncoherentInstances
                                              setIncoherentInsts,
  flagSpec "TypeFamilyDependencies"           LangExt.TypeFamilyDependencies,
  flagSpec "InstanceSigs"                     LangExt.InstanceSigs,
  flagSpec "ApplicativeDo"                    LangExt.ApplicativeDo,
  flagSpec "InterruptibleFFI"                 LangExt.InterruptibleFFI,
  flagSpec "JavaScriptFFI"                    LangExt.JavaScriptFFI,
  flagSpec "KindSignatures"                   LangExt.KindSignatures,
  flagSpec "LambdaCase"                       LangExt.LambdaCase,
  flagSpec "LiberalTypeSynonyms"              LangExt.LiberalTypeSynonyms,
  flagSpec "MagicHash"                        LangExt.MagicHash,
  flagSpec "MonadComprehensions"              LangExt.MonadComprehensions,
  depFlagSpec "MonadFailDesugaring"           LangExt.MonadFailDesugaring
    "MonadFailDesugaring is now the default behavior",
  flagSpec "MonoLocalBinds"                   LangExt.MonoLocalBinds,
  depFlagSpecCond "MonoPatBinds"              LangExt.MonoPatBinds
    id
         "Experimental feature now removed; has no effect",
  flagSpec "MonomorphismRestriction"          LangExt.MonomorphismRestriction,
  flagSpec "MultiParamTypeClasses"            LangExt.MultiParamTypeClasses,
  flagSpec "MultiWayIf"                       LangExt.MultiWayIf,
  flagSpec "NumericUnderscores"               LangExt.NumericUnderscores,
  flagSpec "NPlusKPatterns"                   LangExt.NPlusKPatterns,
  flagSpec "NamedFieldPuns"                   LangExt.RecordPuns,
  flagSpec "NamedWildCards"                   LangExt.NamedWildCards,
  flagSpec "NegativeLiterals"                 LangExt.NegativeLiterals,
  flagSpec "HexFloatLiterals"                 LangExt.HexFloatLiterals,
  flagSpec "NondecreasingIndentation"         LangExt.NondecreasingIndentation,
  depFlagSpec' "NullaryTypeClasses"           LangExt.NullaryTypeClasses
    (deprecatedForExtension "MultiParamTypeClasses"),
  flagSpec "NumDecimals"                      LangExt.NumDecimals,
  depFlagSpecOp "OverlappingInstances"        LangExt.OverlappingInstances
    setOverlappingInsts
    "instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS",
  flagSpec "OverloadedLabels"                 LangExt.OverloadedLabels,
  flagSpec "OverloadedLists"                  LangExt.OverloadedLists,
  flagSpec "OverloadedStrings"                LangExt.OverloadedStrings,
  flagSpec "PackageImports"                   LangExt.PackageImports,
  flagSpec "ParallelArrays"                   LangExt.ParallelArrays,
  flagSpec "ParallelListComp"                 LangExt.ParallelListComp,
  flagSpec "PartialTypeSignatures"            LangExt.PartialTypeSignatures,
  flagSpec "PatternGuards"                    LangExt.PatternGuards,
  depFlagSpec' "PatternSignatures"            LangExt.ScopedTypeVariables
    (deprecatedForExtension "ScopedTypeVariables"),
  flagSpec "PatternSynonyms"                  LangExt.PatternSynonyms,
  flagSpec "PolyKinds"                        LangExt.PolyKinds,
  flagSpec "PolymorphicComponents"            LangExt.RankNTypes,
  flagSpec "QuantifiedConstraints"            LangExt.QuantifiedConstraints,
  flagSpec "PostfixOperators"                 LangExt.PostfixOperators,
  flagSpec "QuasiQuotes"                      LangExt.QuasiQuotes,
  flagSpec "Rank2Types"                       LangExt.RankNTypes,
  flagSpec "RankNTypes"                       LangExt.RankNTypes,
  flagSpec "RebindableSyntax"                 LangExt.RebindableSyntax,
  depFlagSpec' "RecordPuns"                   LangExt.RecordPuns
    (deprecatedForExtension "NamedFieldPuns"),
  flagSpec "RecordWildCards"                  LangExt.RecordWildCards,
  flagSpec "RecursiveDo"                      LangExt.RecursiveDo,
  flagSpec "RelaxedLayout"                    LangExt.RelaxedLayout,
  depFlagSpecCond "RelaxedPolyRec"            LangExt.RelaxedPolyRec
    not
         "You can't turn off RelaxedPolyRec any more",
  flagSpec "RoleAnnotations"                  LangExt.RoleAnnotations,
  flagSpec "ScopedTypeVariables"              LangExt.ScopedTypeVariables,
  flagSpec "StandaloneDeriving"               LangExt.StandaloneDeriving,
  flagSpec "StarIsType"                       LangExt.StarIsType,
  flagSpec "StaticPointers"                   LangExt.StaticPointers,
  flagSpec "Strict"                           LangExt.Strict,
  flagSpec "StrictData"                       LangExt.StrictData,
  flagSpec' "TemplateHaskell"                 LangExt.TemplateHaskell
                                              checkTemplateHaskellOk,
  flagSpec "TemplateHaskellQuotes"            LangExt.TemplateHaskellQuotes,
  flagSpec "StandaloneKindSignatures"         LangExt.StandaloneKindSignatures,
  flagSpec "TraditionalRecordSyntax"          LangExt.TraditionalRecordSyntax,
  flagSpec "TransformListComp"                LangExt.TransformListComp,
  flagSpec "TupleSections"                    LangExt.TupleSections,
  flagSpec "TypeApplications"                 LangExt.TypeApplications,
  flagSpec "TypeInType"                       LangExt.TypeInType,
  flagSpec "TypeFamilies"                     LangExt.TypeFamilies,
  flagSpec "TypeOperators"                    LangExt.TypeOperators,
  flagSpec "TypeSynonymInstances"             LangExt.TypeSynonymInstances,
  flagSpec "UnboxedTuples"                    LangExt.UnboxedTuples,
  flagSpec "UnboxedSums"                      LangExt.UnboxedSums,
  flagSpec "UndecidableInstances"             LangExt.UndecidableInstances,
  flagSpec "UndecidableSuperClasses"          LangExt.UndecidableSuperClasses,
  flagSpec "UnicodeSyntax"                    LangExt.UnicodeSyntax,
  flagSpec "UnliftedFFITypes"                 LangExt.UnliftedFFITypes,
  flagSpec "UnliftedNewtypes"                 LangExt.UnliftedNewtypes,
  flagSpec "ViewPatterns"                     LangExt.ViewPatterns
  ]

defaultFlags :: Settings -> [GeneralFlag]
defaultFlags settings
-- See Note [Updating flag description in the User's Guide]
  = [ Opt_AutoLinkPackages,
      Opt_DiagnosticsShowCaret,
      Opt_EmbedManifest,
      Opt_FlatCache,
      Opt_GenManifest,
      Opt_GhciHistory,
      Opt_GhciSandbox,
      Opt_HelpfulErrors,
      Opt_KeepHiFiles,
      Opt_KeepOFiles,
      Opt_OmitYields,
      Opt_PrintBindContents,
      Opt_ProfCountEntries,
      Opt_RPath,
      Opt_SharedImplib,
      Opt_SimplPreInlining,
      Opt_VersionMacros
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    ++ default_PIC platform

    ++ concatMap (wayGeneralFlags platform) (defaultWays settings)
    ++ validHoleFitDefaults

    where platform = sTargetPlatform settings

-- | These are the default settings for the display and sorting of valid hole
--  fits in typed-hole error messages. See Note [Valid hole fits include ...]
 -- in the TcHoleErrors module.
validHoleFitDefaults :: [GeneralFlag]
validHoleFitDefaults
  =  [ Opt_ShowTypeAppOfHoleFits
     , Opt_ShowTypeOfHoleFits
     , Opt_ShowProvOfHoleFits
     , Opt_ShowMatchesOfHoleFits
     , Opt_ShowValidHoleFits
     , Opt_SortValidHoleFits
     , Opt_SortBySizeHoleFits
     , Opt_ShowHoleConstraints ]


validHoleFitsImpliedGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
validHoleFitsImpliedGFlags
  = [ (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowTypeAppOfHoleFits)
    , (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowTypeAppVarsOfHoleFits)
    , (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowDocsOfHoleFits)
    , (Opt_ShowTypeAppVarsOfHoleFits, turnOff, Opt_ShowTypeAppOfHoleFits)
    , (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowProvOfHoleFits) ]

default_PIC :: Platform -> [GeneralFlag]
default_PIC platform =
  case (platformOS platform, platformArch platform) of
    (OSDarwin, ArchX86_64) -> [Opt_PIC]
    (OSOpenBSD, ArchX86_64) -> [Opt_PIC] -- Due to PIE support in
                                         -- OpenBSD since 5.3 release
                                         -- (1 May 2013) we need to
                                         -- always generate PIC. See
                                         -- #10597 for more
                                         -- information.
    _                      -> []

-- General flags that are switched on/off when other general flags are switched
-- on
impliedGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedGFlags = [(Opt_DeferTypeErrors, turnOn, Opt_DeferTypedHoles)
                ,(Opt_DeferTypeErrors, turnOn, Opt_DeferOutOfScopeVariables)
                ,(Opt_Strictness, turnOn, Opt_WorkerWrapper)
                ] ++ validHoleFitsImpliedGFlags

-- General flags that are switched on/off when other general flags are switched
-- off
impliedOffGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedOffGFlags = [(Opt_Strictness, turnOff, Opt_WorkerWrapper)]

impliedXFlags :: [(LangExt.Extension, TurnOnFlag, LangExt.Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (LangExt.RankNTypes,                turnOn, LangExt.ExplicitForAll)
    , (LangExt.QuantifiedConstraints,     turnOn, LangExt.ExplicitForAll)
    , (LangExt.ScopedTypeVariables,       turnOn, LangExt.ExplicitForAll)
    , (LangExt.LiberalTypeSynonyms,       turnOn, LangExt.ExplicitForAll)
    , (LangExt.ExistentialQuantification, turnOn, LangExt.ExplicitForAll)
    , (LangExt.FlexibleInstances,         turnOn, LangExt.TypeSynonymInstances)
    , (LangExt.FunctionalDependencies,    turnOn, LangExt.MultiParamTypeClasses)
    , (LangExt.MultiParamTypeClasses,     turnOn, LangExt.ConstrainedClassMethods)  -- c.f. #7854
    , (LangExt.TypeFamilyDependencies,    turnOn, LangExt.TypeFamilies)

    , (LangExt.RebindableSyntax, turnOff, LangExt.ImplicitPrelude)      -- NB: turn off!

    , (LangExt.DerivingVia, turnOn, LangExt.DerivingStrategies)

    , (LangExt.GADTs,            turnOn, LangExt.GADTSyntax)
    , (LangExt.GADTs,            turnOn, LangExt.MonoLocalBinds)
    , (LangExt.TypeFamilies,     turnOn, LangExt.MonoLocalBinds)

    , (LangExt.TypeFamilies,     turnOn, LangExt.KindSignatures)  -- Type families use kind signatures
    , (LangExt.PolyKinds,        turnOn, LangExt.KindSignatures)  -- Ditto polymorphic kinds

    -- TypeInType is now just a synonym for a couple of other extensions.
    , (LangExt.TypeInType,       turnOn, LangExt.DataKinds)
    , (LangExt.TypeInType,       turnOn, LangExt.PolyKinds)
    , (LangExt.TypeInType,       turnOn, LangExt.KindSignatures)

    -- Standalone kind signatures are a replacement for CUSKs.
    , (LangExt.StandaloneKindSignatures, turnOff, LangExt.CUSKs)

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (LangExt.AutoDeriveTypeable, turnOn, LangExt.DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (LangExt.TypeFamilies,     turnOn, LangExt.ExplicitNamespaces)
    , (LangExt.TypeOperators, turnOn, LangExt.ExplicitNamespaces)

    , (LangExt.ImpredicativeTypes,  turnOn, LangExt.RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (LangExt.RecordWildCards,     turnOn, LangExt.DisambiguateRecordFields)

    , (LangExt.ParallelArrays, turnOn, LangExt.ParallelListComp)

    , (LangExt.JavaScriptFFI, turnOn, LangExt.InterruptibleFFI)

    , (LangExt.DeriveTraversable, turnOn, LangExt.DeriveFunctor)
    , (LangExt.DeriveTraversable, turnOn, LangExt.DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (LangExt.DuplicateRecordFields, turnOn, LangExt.DisambiguateRecordFields)

    , (LangExt.TemplateHaskell, turnOn, LangExt.TemplateHaskellQuotes)
    , (LangExt.Strict, turnOn, LangExt.StrictData)
  ]

-- Note [When is StarIsType enabled]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The StarIsType extension determines whether to treat '*' as a regular type
-- operator or as a synonym for 'Data.Kind.Type'. Many existing pre-TypeInType
-- programs expect '*' to be synonymous with 'Type', so by default StarIsType is
-- enabled.
--
-- Programs that use TypeOperators might expect to repurpose '*' for
-- multiplication or another binary operation, but making TypeOperators imply
-- NoStarIsType caused too much breakage on Hackage.
--

-- Note [Documenting optimisation flags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you change the list of flags enabled for particular optimisation levels
-- please remember to update the User's Guide. The relevant file is:
--
--   docs/users_guide/using-optimisation.rst
--
-- Make sure to note whether a flag is implied by -O0, -O or -O2.

optLevelFlags :: [([Int], GeneralFlag)]
-- Default settings of flags, before any command-line overrides
optLevelFlags -- see Note [Documenting optimisation flags]
  = [ ([0,1,2], Opt_DoLambdaEtaExpansion)
    , ([0,1,2], Opt_DoEtaReduction)       -- See Note [Eta-reduction in -O0]
    , ([0,1,2], Opt_DmdTxDictSel)
    , ([0,1,2], Opt_LlvmTBAA)

    , ([0],     Opt_IgnoreInterfacePragmas)
    , ([0],     Opt_OmitInterfacePragmas)

    , ([1,2],   Opt_CallArity)
    , ([1,2],   Opt_Exitification)
    , ([1,2],   Opt_CaseMerge)
    , ([1,2],   Opt_CaseFolding)
    , ([1,2],   Opt_CmmElimCommonBlocks)
    , ([2],     Opt_AsmShortcutting)
    , ([1,2],   Opt_CmmSink)
    , ([1,2],   Opt_CSE)
    , ([1,2],   Opt_StgCSE)
    , ([2],     Opt_StgLiftLams)

    , ([1,2],   Opt_EnableRewriteRules)
          -- Off for -O0.   Otherwise we desugar list literals
          -- to 'build' but don't run the simplifier passes that
          -- would rewrite them back to cons cells!  This seems
          -- silly, and matters for the GHCi debugger.

    , ([1,2],   Opt_FloatIn)
    , ([1,2],   Opt_FullLaziness)
    , ([1,2],   Opt_IgnoreAsserts)
    , ([1,2],   Opt_Loopification)
    , ([1,2],   Opt_CfgBlocklayout)      -- Experimental

    , ([1,2],   Opt_Specialise)
    , ([1,2],   Opt_CrossModuleSpecialise)
    , ([1,2],   Opt_Strictness)
    , ([1,2],   Opt_UnboxSmallStrictFields)
    , ([1,2],   Opt_CprAnal)
    , ([1,2],   Opt_WorkerWrapper)
    , ([1,2],   Opt_SolveConstantDicts)
    , ([1,2],   Opt_NumConstantFolding)

    , ([2],     Opt_LiberateCase)
    , ([2],     Opt_SpecConstr)
--  , ([2],     Opt_RegsGraph)
--   RegsGraph suffers performance regression. See #7679
--  , ([2],     Opt_StaticArgumentTransformation)
--   Static Argument Transformation needs investigation. See #9374
    ]


-- -----------------------------------------------------------------------------
-- Standard sets of warning options

-- Note [Documenting warning flags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you change the list of warning enabled by default
-- please remember to update the User's Guide. The relevant file is:
--
--  docs/users_guide/using-warnings.rst

-- | Warning groups.
--
-- As all warnings are in the Weverything set, it is ignored when
-- displaying to the user which group a warning is in.
warningGroups :: [(String, [WarningFlag])]
warningGroups =
    [ ("compat",       minusWcompatOpts)
    , ("unused-binds", unusedBindsFlags)
    , ("default",      standardWarnings)
    , ("extra",        minusWOpts)
    , ("all",          minusWallOpts)
    , ("everything",   minusWeverythingOpts)
    ]

-- | Warning group hierarchies, where there is an explicit inclusion
-- relation.
--
-- Each inner list is a hierarchy of warning groups, ordered from
-- smallest to largest, where each group is a superset of the one
-- before it.
--
-- Separating this from 'warningGroups' allows for multiple
-- hierarchies with no inherent relation to be defined.
--
-- The special-case Weverything group is not included.
warningHierarchies :: [[String]]
warningHierarchies = hierarchies ++ map (:[]) rest
  where
    hierarchies = [["default", "extra", "all"]]
    rest = filter (`notElem` "everything" : concat hierarchies) $
           map fst warningGroups

-- | Find the smallest group in every hierarchy which a warning
-- belongs to, excluding Weverything.
smallestGroups :: WarningFlag -> [String]
smallestGroups flag = mapMaybe go warningHierarchies where
    -- Because each hierarchy is arranged from smallest to largest,
    -- the first group we find in a hierarchy which contains the flag
    -- is the smallest.
    go (group:rest) = fromMaybe (go rest) $ do
        flags <- lookup group warningGroups
        guard (flag `elem` flags)
        pure (Just group)
    go [] = Nothing

-- | Warnings enabled unless specified otherwise
standardWarnings :: [WarningFlag]
standardWarnings -- see Note [Documenting warning flags]
    = [ Opt_WarnOverlappingPatterns,
        Opt_WarnWarningsDeprecations,
        Opt_WarnDeprecatedFlags,
        Opt_WarnDeferredTypeErrors,
        Opt_WarnTypedHoles,
        Opt_WarnDeferredOutOfScopeVariables,
        Opt_WarnPartialTypeSignatures,
        Opt_WarnUnrecognisedPragmas,
        Opt_WarnDuplicateExports,
        Opt_WarnDerivingDefaults,
        Opt_WarnOverflowedLiterals,
        Opt_WarnEmptyEnumerations,
        Opt_WarnMissingFields,
        Opt_WarnMissingMethods,
        Opt_WarnWrongDoBind,
        Opt_WarnUnsupportedCallingConventions,
        Opt_WarnDodgyForeignImports,
        Opt_WarnInlineRuleShadowing,
        Opt_WarnAlternativeLayoutRuleTransitional,
        Opt_WarnUnsupportedLlvmVersion,
        Opt_WarnMissedExtraSharedLib,
        Opt_WarnTabs,
        Opt_WarnUnrecognisedWarningFlags,
        Opt_WarnSimplifiableClassConstraints,
        Opt_WarnStarBinder,
        Opt_WarnInaccessibleCode,
        Opt_WarnSpaceAfterBang
      ]

-- | Things you get with -W
minusWOpts :: [WarningFlag]
minusWOpts
    = standardWarnings ++
      [ Opt_WarnUnusedTopBinds,
        Opt_WarnUnusedLocalBinds,
        Opt_WarnUnusedPatternBinds,
        Opt_WarnUnusedMatches,
        Opt_WarnUnusedForalls,
        Opt_WarnUnusedImports,
        Opt_WarnIncompletePatterns,
        Opt_WarnDodgyExports,
        Opt_WarnDodgyImports,
        Opt_WarnUnbangedStrictPatterns
      ]

-- | Things you get with -Wall
minusWallOpts :: [WarningFlag]
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSignatures,
        Opt_WarnHiShadows,
        Opt_WarnOrphans,
        Opt_WarnUnusedDoBind,
        Opt_WarnTrustworthySafe,
        Opt_WarnUntickedPromotedConstructors,
        Opt_WarnMissingPatternSynonymSignatures,
        Opt_WarnUnusedRecordWildcards,
        Opt_WarnRedundantRecordWildcards,
        Opt_WarnStarIsType
      ]

-- | Things you get with -Weverything, i.e. *all* known warnings flags
minusWeverythingOpts :: [WarningFlag]
minusWeverythingOpts = [ toEnum 0 .. ]

-- | Things you get with -Wcompat.
--
-- This is intended to group together warnings that will be enabled by default
-- at some point in the future, so that library authors eager to make their
-- code future compatible to fix issues before they even generate warnings.
minusWcompatOpts :: [WarningFlag]
minusWcompatOpts
    = [ Opt_WarnSemigroup
      , Opt_WarnNonCanonicalMonoidInstances
      , Opt_WarnStarIsType
      , Opt_WarnCompatUnqualifiedImports
      ]

enableUnusedBinds :: DynP ()
enableUnusedBinds = mapM_ setWarningFlag unusedBindsFlags

disableUnusedBinds :: DynP ()
disableUnusedBinds = mapM_ unSetWarningFlag unusedBindsFlags

-- Things you get with -Wunused-binds
unusedBindsFlags :: [WarningFlag]
unusedBindsFlags = [ Opt_WarnUnusedTopBinds
                   , Opt_WarnUnusedLocalBinds
                   , Opt_WarnUnusedPatternBinds
                   ]

enableGlasgowExts :: DynP ()
enableGlasgowExts = do setGeneralFlag Opt_PrintExplicitForalls
                       mapM_ setExtensionFlag glasgowExtsFlags

disableGlasgowExts :: DynP ()
disableGlasgowExts = do unSetGeneralFlag Opt_PrintExplicitForalls
                        mapM_ unSetExtensionFlag glasgowExtsFlags

-- Please keep what_glasgow_exts_does.rst up to date with this list
glasgowExtsFlags :: [LangExt.Extension]
glasgowExtsFlags = [
             LangExt.ConstrainedClassMethods
           , LangExt.DeriveDataTypeable
           , LangExt.DeriveFoldable
           , LangExt.DeriveFunctor
           , LangExt.DeriveGeneric
           , LangExt.DeriveTraversable
           , LangExt.EmptyDataDecls
           , LangExt.ExistentialQuantification
           , LangExt.ExplicitNamespaces
           , LangExt.FlexibleContexts
           , LangExt.FlexibleInstances
           , LangExt.ForeignFunctionInterface
           , LangExt.FunctionalDependencies
           , LangExt.GeneralizedNewtypeDeriving
           , LangExt.ImplicitParams
           , LangExt.KindSignatures
           , LangExt.LiberalTypeSynonyms
           , LangExt.MagicHash
           , LangExt.MultiParamTypeClasses
           , LangExt.ParallelListComp
           , LangExt.PatternGuards
           , LangExt.PostfixOperators
           , LangExt.RankNTypes
           , LangExt.RecursiveDo
           , LangExt.ScopedTypeVariables
           , LangExt.StandaloneDeriving
           , LangExt.TypeOperators
           , LangExt.TypeSynonymInstances
           , LangExt.UnboxedTuples
           , LangExt.UnicodeSyntax
           , LangExt.UnliftedFFITypes ]

setWarnSafe :: Bool -> DynP ()
setWarnSafe True  = getCurLoc >>= \l -> upd (\d -> d { warnSafeOnLoc = l })
setWarnSafe False = return ()

setWarnUnsafe :: Bool -> DynP ()
setWarnUnsafe True  = getCurLoc >>= \l -> upd (\d -> d { warnUnsafeOnLoc = l })
setWarnUnsafe False = return ()

setPackageTrust :: DynP ()
setPackageTrust = do
    setGeneralFlag Opt_PackageTrust
    l <- getCurLoc
    upd $ \d -> d { pkgTrustOnLoc = l }

setGenDeriving :: TurnOnFlag -> DynP ()
setGenDeriving True  = getCurLoc >>= \l -> upd (\d -> d { newDerivOnLoc = l })
setGenDeriving False = return ()

setOverlappingInsts :: TurnOnFlag -> DynP ()
setOverlappingInsts False = return ()
setOverlappingInsts True = do
  l <- getCurLoc
  upd (\d -> d { overlapInstLoc = l })

setIncoherentInsts :: TurnOnFlag -> DynP ()
setIncoherentInsts False = return ()
setIncoherentInsts True = do
  l <- getCurLoc
  upd (\d -> d { incoherentOnLoc = l })

checkTemplateHaskellOk :: TurnOnFlag -> DynP ()
checkTemplateHaskellOk _turn_on
  = getCurLoc >>= \l -> upd (\d -> d { thOnLoc = l })

{- **********************************************************************
%*                                                                      *
                DynFlags constructors
%*                                                                      *
%********************************************************************* -}

type DynP = EwM (CmdLineP DynFlags)

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = liftEwM (do dflags <- getCmdLineState
                    putCmdLineState $! f dflags)

updM :: (DynFlags -> DynP DynFlags) -> DynP ()
updM f = do dflags <- liftEwM getCmdLineState
            dflags' <- f dflags
            liftEwM $ putCmdLineState $! dflags'

--------------- Constructor functions for OptKind -----------------
noArg :: (DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
noArg fn = NoArg (upd fn)

noArgM :: (DynFlags -> DynP DynFlags) -> OptKind (CmdLineP DynFlags)
noArgM fn = NoArg (updM fn)

hasArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
hasArg fn = HasArg (upd . fn)

sepArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
sepArg fn = SepArg (upd . fn)

intSuffix :: (Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffix fn = IntSuffix (\n -> upd (fn n))

intSuffixM :: (Int -> DynFlags -> DynP DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffixM fn = IntSuffix (\n -> updM (fn n))

floatSuffix :: (Float -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
floatSuffix fn = FloatSuffix (\n -> upd (fn n))

optIntSuffixM :: (Maybe Int -> DynFlags -> DynP DynFlags)
              -> OptKind (CmdLineP DynFlags)
optIntSuffixM fn = OptIntSuffix (\mi -> updM (fn mi))

setDumpFlag :: DumpFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

--------------------------
addWay :: Way -> DynP ()
addWay w = upd (addWay' w)

addWay' :: Way -> DynFlags -> DynFlags
addWay' w dflags0 = let platform = targetPlatform dflags0
                        dflags1 = dflags0 { ways = Set.insert w (ways dflags0) }
                        dflags2 = foldr setGeneralFlag' dflags1
                                        (wayGeneralFlags platform w)
                        dflags3 = foldr unSetGeneralFlag' dflags2
                                        (wayUnsetGeneralFlags platform w)
                    in dflags3

removeWayDyn :: DynP ()
removeWayDyn = upd (\dfs -> dfs { ways = Set.filter (WayDyn /=) (ways dfs) })

--------------------------
setGeneralFlag, unSetGeneralFlag :: GeneralFlag -> DynP ()
setGeneralFlag   f = upd (setGeneralFlag' f)
unSetGeneralFlag f = upd (unSetGeneralFlag' f)

setGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags
setGeneralFlag' f dflags = foldr ($) (gopt_set dflags f) deps
  where
    deps = [ if turn_on then setGeneralFlag'   d
                        else unSetGeneralFlag' d
           | (f', turn_on, d) <- impliedGFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setGeneralFlag recursively, in case the implied flags
        --     implies further flags

unSetGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags
unSetGeneralFlag' f dflags = foldr ($) (gopt_unset dflags f) deps
  where
    deps = [ if turn_on then setGeneralFlag' d
                        else unSetGeneralFlag' d
           | (f', turn_on, d) <- impliedOffGFlags, f' == f ]
   -- In general, when you un-set f, we don't un-set the things it implies.
   -- There are however some exceptions, e.g., -fno-strictness implies
   -- -fno-worker-wrapper.
   --
   -- NB: use unSetGeneralFlag' recursively, in case the implied off flags
   --     imply further flags.

--------------------------
setWarningFlag, unSetWarningFlag :: WarningFlag -> DynP ()
setWarningFlag   f = upd (\dfs -> wopt_set dfs f)
unSetWarningFlag f = upd (\dfs -> wopt_unset dfs f)

setFatalWarningFlag, unSetFatalWarningFlag :: WarningFlag -> DynP ()
setFatalWarningFlag   f = upd (\dfs -> wopt_set_fatal dfs f)
unSetFatalWarningFlag f = upd (\dfs -> wopt_unset_fatal dfs f)

setWErrorFlag :: WarningFlag -> DynP ()
setWErrorFlag flag =
  do { setWarningFlag flag
     ; setFatalWarningFlag flag }

--------------------------
setExtensionFlag, unSetExtensionFlag :: LangExt.Extension -> DynP ()
setExtensionFlag f = upd (setExtensionFlag' f)
unSetExtensionFlag f = upd (unSetExtensionFlag' f)

setExtensionFlag', unSetExtensionFlag' :: LangExt.Extension -> DynFlags -> DynFlags
setExtensionFlag' f dflags = foldr ($) (xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedXFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags

unSetExtensionFlag' f dflags = xopt_unset dflags f
   -- When you un-set f, however, we don't un-set the things it implies
   --      (except for -fno-glasgow-exts, which is treated specially)

--------------------------
alterFileSettings :: (FileSettings -> FileSettings) -> DynFlags -> DynFlags
alterFileSettings f dynFlags = dynFlags { fileSettings = f (fileSettings dynFlags) }

alterToolSettings :: (ToolSettings -> ToolSettings) -> DynFlags -> DynFlags
alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }

--------------------------
setDumpFlag' :: DumpFlag -> DynP ()
setDumpFlag' dump_flag
  = do upd (\dfs -> dopt_set dfs dump_flag)
       when want_recomp forceRecompile
    where -- Certain dumpy-things are really interested in what's going
          -- on during recompilation checking, so in those cases we
          -- don't want to turn it off.
          want_recomp = dump_flag `notElem` [Opt_D_dump_if_trace,
                                             Opt_D_dump_hi_diffs,
                                             Opt_D_no_debug_output]

forceRecompile :: DynP ()
-- Whenever we -ddump, force recompilation (by switching off the
-- recompilation checker), else you don't see the dump! However,
-- don't switch it off in --make mode, else *everything* gets
-- recompiled which probably isn't what you want
forceRecompile = do dfs <- liftEwM getCmdLineState
                    when (force_recomp dfs) (setGeneralFlag Opt_ForceRecomp)
        where
          force_recomp dfs = isOneShot (ghcMode dfs)


setVerboseCore2Core :: DynP ()
setVerboseCore2Core = setDumpFlag' Opt_D_verbose_core2core

setVerbosity :: Maybe Int -> DynP ()
setVerbosity mb_n = upd (\dfs -> dfs{ verbosity = mb_n `orElse` 3 })

setDebugLevel :: Maybe Int -> DynP ()
setDebugLevel mb_n = upd (\dfs -> dfs{ debugLevel = mb_n `orElse` 2 })

data PkgDbRef
  = GlobalPkgDb
  | UserPkgDb
  | PkgDbPath FilePath
  deriving Eq

addPkgDbRef :: PkgDbRef -> DynP ()
addPkgDbRef p = upd $ \s ->
  s { packageDBFlags = PackageDB p : packageDBFlags s }

removeUserPkgDb :: DynP ()
removeUserPkgDb = upd $ \s ->
  s { packageDBFlags = NoUserPackageDB : packageDBFlags s }

removeGlobalPkgDb :: DynP ()
removeGlobalPkgDb = upd $ \s ->
 s { packageDBFlags = NoGlobalPackageDB : packageDBFlags s }

clearPkgDb :: DynP ()
clearPkgDb = upd $ \s ->
  s { packageDBFlags = ClearPackageDBs : packageDBFlags s }

parsePackageFlag :: String                 -- the flag
                 -> ReadP PackageArg       -- type of argument
                 -> String                 -- string to parse
                 -> PackageFlag
parsePackageFlag flag arg_parse str
 = case filter ((=="").snd) (readP_to_S parse str) of
    [(r, "")] -> r
    _ -> throwGhcException $ CmdLineError ("Can't parse package flag: " ++ str)
  where doc = flag ++ " " ++ str
        parse = do
            pkg_arg <- tok arg_parse
            let mk_expose = ExposePackage doc pkg_arg
            ( do _ <- tok $ string "with"
                 fmap (mk_expose . ModRenaming True) parseRns
             <++ fmap (mk_expose . ModRenaming False) parseRns
             <++ return (mk_expose (ModRenaming True [])))
        parseRns = do _ <- tok $ R.char '('
                      rns <- tok $ sepBy parseItem (tok $ R.char ',')
                      _ <- tok $ R.char ')'
                      return rns
        parseItem = do
            orig <- tok $ parseModuleName
            (do _ <- tok $ string "as"
                new <- tok $ parseModuleName
                return (orig, new)
              +++
             return (orig, orig))
        tok m = m >>= \x -> skipSpaces >> return x

exposePackage, exposePackageId, hidePackage,
        exposePluginPackage, exposePluginPackageId,
        ignorePackage,
        trustPackage, distrustPackage :: String -> DynP ()
exposePackage p = upd (exposePackage' p)
exposePackageId p =
  upd (\s -> s{ packageFlags =
    parsePackageFlag "-package-id" parseUnitIdArg p : packageFlags s })
exposePluginPackage p =
  upd (\s -> s{ pluginPackageFlags =
    parsePackageFlag "-plugin-package" parsePackageArg p : pluginPackageFlags s })
exposePluginPackageId p =
  upd (\s -> s{ pluginPackageFlags =
    parsePackageFlag "-plugin-package-id" parseUnitIdArg p : pluginPackageFlags s })
hidePackage p =
  upd (\s -> s{ packageFlags = HidePackage p : packageFlags s })
ignorePackage p =
  upd (\s -> s{ ignorePackageFlags = IgnorePackage p : ignorePackageFlags s })

trustPackage p = exposePackage p >> -- both trust and distrust also expose a package
  upd (\s -> s{ trustFlags = TrustPackage p : trustFlags s })
distrustPackage p = exposePackage p >>
  upd (\s -> s{ trustFlags = DistrustPackage p : trustFlags s })

exposePackage' :: String -> DynFlags -> DynFlags
exposePackage' p dflags
    = dflags { packageFlags =
            parsePackageFlag "-package" parsePackageArg p : packageFlags dflags }

parsePackageArg :: ReadP PackageArg
parsePackageArg =
    fmap PackageArg (munch1 (\c -> isAlphaNum c || c `elem` ":-_."))

parseUnitIdArg :: ReadP PackageArg
parseUnitIdArg =
    fmap UnitIdArg parseUnitId

setUnitId :: String -> DynFlags -> DynFlags
setUnitId p d = d { thisInstalledUnitId = stringToInstalledUnitId p }

-- | Given a 'ModuleName' of a signature in the home library, find
-- out how it is instantiated.  E.g., the canonical form of
-- A in @p[A=q[]:A]@ is @q[]:A@.
canonicalizeHomeModule :: DynFlags -> ModuleName -> Module
canonicalizeHomeModule dflags mod_name =
    case lookup mod_name (thisUnitIdInsts dflags) of
        Nothing  -> mkModule (thisPackage dflags) mod_name
        Just mod -> mod

canonicalizeModuleIfHome :: DynFlags -> Module -> Module
canonicalizeModuleIfHome dflags mod
    = if thisPackage dflags == moduleUnitId mod
                      then canonicalizeHomeModule dflags (moduleName mod)
                      else mod

-- If we're linking a binary, then only targets that produce object
-- code are allowed (requests for other target types are ignored).
setTarget :: HscTarget -> DynP ()
setTarget l = upd $ \ dfs ->
  if ghcLink dfs /= LinkBinary || isObjectTarget l
  then dfs{ hscTarget = l }
  else dfs

-- Changes the target only if we're compiling object code.  This is
-- used by -fasm and -fllvm, which switch from one to the other, but
-- not from bytecode to object-code.  The idea is that -fasm/-fllvm
-- can be safely used in an OPTIONS_GHC pragma.
setObjTarget :: HscTarget -> DynP ()
setObjTarget l = updM set
  where
   set dflags
     | isObjectTarget (hscTarget dflags)
       = return $ dflags { hscTarget = l }
     | otherwise = return dflags

setOptLevel :: Int -> DynFlags -> DynP DynFlags
setOptLevel n dflags = return (updOptLevel n dflags)

checkOptLevel :: Int -> DynFlags -> Either String DynFlags
checkOptLevel n dflags
   | hscTarget dflags == HscInterpreted && n > 0
     = Left "-O conflicts with --interactive; -O ignored."
   | otherwise
     = Right dflags

setMainIs :: String -> DynP ()
setMainIs arg
  | not (null main_fn) && isLower (head main_fn)
     -- The arg looked like "Foo.Bar.baz"
  = upd $ \d -> d { mainFunIs = Just main_fn,
                   mainModIs = mkModule mainUnitId (mkModuleName main_mod) }

  | isUpper (head arg)  -- The arg looked like "Foo" or "Foo.Bar"
  = upd $ \d -> d { mainModIs = mkModule mainUnitId (mkModuleName arg) }

  | otherwise                   -- The arg looked like "baz"
  = upd $ \d -> d { mainFunIs = Just arg }
  where
    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

addLdInputs :: Option -> DynFlags -> DynFlags
addLdInputs p dflags = dflags{ldInputs = ldInputs dflags ++ [p]}

-- -----------------------------------------------------------------------------
-- Load dynflags from environment files.

setFlagsFromEnvFile :: FilePath -> String -> DynP ()
setFlagsFromEnvFile envfile content = do
  setGeneralFlag Opt_HideAllPackages
  parseEnvFile envfile content

parseEnvFile :: FilePath -> String -> DynP ()
parseEnvFile envfile = mapM_ parseEntry . lines
  where
    parseEntry str = case words str of
      ("package-db": _)     -> addPkgDbRef (PkgDbPath (envdir </> db))
        -- relative package dbs are interpreted relative to the env file
        where envdir = takeDirectory envfile
              db     = drop 11 str
      ["clear-package-db"]  -> clearPkgDb
      ["global-package-db"] -> addPkgDbRef GlobalPkgDb
      ["user-package-db"]   -> addPkgDbRef UserPkgDb
      ["package-id", pkgid] -> exposePackageId pkgid
      (('-':'-':_):_)       -> return () -- comments
      -- and the original syntax introduced in 7.10:
      [pkgid]               -> exposePackageId pkgid
      []                    -> return ()
      _                     -> throwGhcException $ CmdLineError $
                                    "Can't parse environment file entry: "
                                 ++ envfile ++ ": " ++ str


-----------------------------------------------------------------------------
-- Paths & Libraries

addImportPath, addLibraryPath, addIncludePath, addFrameworkPath :: FilePath -> DynP ()

-- -i on its own deletes the import paths
addImportPath "" = upd (\s -> s{importPaths = []})
addImportPath p  = upd (\s -> s{importPaths = importPaths s ++ splitPathList p})

addLibraryPath p =
  upd (\s -> s{libraryPaths = libraryPaths s ++ splitPathList p})

addIncludePath p =
  upd (\s -> s{includePaths =
                  addGlobalInclude (includePaths s) (splitPathList p)})

addFrameworkPath p =
  upd (\s -> s{frameworkPaths = frameworkPaths s ++ splitPathList p})

#if !defined(mingw32_HOST_OS)
split_marker :: Char
split_marker = ':'   -- not configurable (ToDo)
#endif

splitPathList :: String -> [String]
splitPathList s = filter notNull (splitUp s)
                -- empty paths are ignored: there might be a trailing
                -- ':' in the initial list, for example.  Empty paths can
                -- cause confusion when they are translated into -I options
                -- for passing to gcc.
  where
#if !defined(mingw32_HOST_OS)
    splitUp xs = split split_marker xs
#else
     -- Windows: 'hybrid' support for DOS-style paths in directory lists.
     --
     -- That is, if "foo:bar:baz" is used, this interpreted as
     -- consisting of three entries, 'foo', 'bar', 'baz'.
     -- However, with "c:/foo:c:\\foo;x:/bar", this is interpreted
     -- as 3 elts, "c:/foo", "c:\\foo", "x:/bar"
     --
     -- Notice that no attempt is made to fully replace the 'standard'
     -- split marker ':' with the Windows / DOS one, ';'. The reason being
     -- that this will cause too much breakage for users & ':' will
     -- work fine even with DOS paths, if you're not insisting on being silly.
     -- So, use either.
    splitUp []             = []
    splitUp (x:':':div:xs) | div `elem` dir_markers
                           = ((x:':':div:p): splitUp rs)
                           where
                              (p,rs) = findNextPath xs
          -- we used to check for existence of the path here, but that
          -- required the IO monad to be threaded through the command-line
          -- parser which is quite inconvenient.  The
    splitUp xs = cons p (splitUp rs)
               where
                 (p,rs) = findNextPath xs

                 cons "" xs = xs
                 cons x  xs = x:xs

    -- will be called either when we've consumed nought or the
    -- "<Drive>:/" part of a DOS path, so splitting is just a Q of
    -- finding the next split marker.
    findNextPath xs =
        case break (`elem` split_markers) xs of
           (p, _:ds) -> (p, ds)
           (p, xs)   -> (p, xs)

    split_markers :: [Char]
    split_markers = [':', ';']

    dir_markers :: [Char]
    dir_markers = ['/', '\\']
#endif

-- -----------------------------------------------------------------------------
-- tmpDir, where we store temporary files.

setTmpDir :: FilePath -> DynFlags -> DynFlags
setTmpDir dir = alterFileSettings $ \s -> s { fileSettings_tmpDir = normalise dir }
  -- we used to fix /cygdrive/c/.. on Windows, but this doesn't
  -- seem necessary now --SDM 7/2/2008

-----------------------------------------------------------------------------
-- RTS opts

setRtsOpts :: String -> DynP ()
setRtsOpts arg  = upd $ \ d -> d {rtsOpts = Just arg}

setRtsOptsEnabled :: RtsOptsEnabled -> DynP ()
setRtsOptsEnabled arg  = upd $ \ d -> d {rtsOptsEnabled = arg}

-----------------------------------------------------------------------------
-- Hpc stuff

setOptHpcDir :: String -> DynP ()
setOptHpcDir arg  = upd $ \ d -> d {hpcDir = arg}

-----------------------------------------------------------------------------
-- Via-C compilation stuff

-- There are some options that we need to pass to gcc when compiling
-- Haskell code via C, but are only supported by recent versions of
-- gcc.  The configure script decides which of these options we need,
-- and puts them in the "settings" file in $topdir. The advantage of
-- having these in a separate file is that the file can be created at
-- install-time depending on the available gcc version, and even
-- re-generated later if gcc is upgraded.
--
-- The options below are not dependent on the version of gcc, only the
-- platform.

picCCOpts :: DynFlags -> [String]
picCCOpts dflags = pieOpts ++ picOpts
  where
    picOpts =
      case platformOS (targetPlatform dflags) of
      OSDarwin
          -- Apple prefers to do things the other way round.
          -- PIC is on by default.
          -- -mdynamic-no-pic:
          --     Turn off PIC code generation.
          -- -fno-common:
          --     Don't generate "common" symbols - these are unwanted
          --     in dynamic libraries.

       | gopt Opt_PIC dflags -> ["-fno-common", "-U__PIC__", "-D__PIC__"]
       | otherwise           -> ["-mdynamic-no-pic"]
      OSMinGW32 -- no -fPIC for Windows
       | gopt Opt_PIC dflags -> ["-U__PIC__", "-D__PIC__"]
       | otherwise           -> []
      _
      -- we need -fPIC for C files when we are compiling with -dynamic,
      -- otherwise things like stub.c files don't get compiled
      -- correctly.  They need to reference data in the Haskell
      -- objects, but can't without -fPIC.  See
      -- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/position-independent-code
       | gopt Opt_PIC dflags || WayDyn `Set.member` ways dflags ->
          ["-fPIC", "-U__PIC__", "-D__PIC__"]
      -- gcc may be configured to have PIC on by default, let's be
      -- explicit here, see #15847
       | otherwise -> ["-fno-PIC"]

    pieOpts
      | gopt Opt_PICExecutable dflags       = ["-pie"]
        -- See Note [No PIE when linking]
      | toolSettings_ccSupportsNoPie (toolSettings dflags) = ["-no-pie"]
      | otherwise                           = []


{-
Note [No PIE while linking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As of 2016 some Linux distributions (e.g. Debian) have started enabling -pie by
default in their gcc builds. This is incompatible with -r as it implies that we
are producing an executable. Consequently, we must manually pass -no-pie to gcc
when joining object files or linking dynamic libraries. Unless, of course, the
user has explicitly requested a PIE executable with -pie. See #12759.
-}

picPOpts :: DynFlags -> [String]
picPOpts dflags
 | gopt Opt_PIC dflags = ["-U__PIC__", "-D__PIC__"]
 | otherwise           = []

-- -----------------------------------------------------------------------------
-- Compiler Info

compilerInfo :: DynFlags -> [(String, String)]
compilerInfo dflags
    = -- We always make "Project name" be first to keep parsing in
      -- other languages simple, i.e. when looking for other fields,
      -- you don't have to worry whether there is a leading '[' or not
      ("Project name",                 cProjectName)
      -- Next come the settings, so anything else can be overridden
      -- in the settings file (as "lookup" uses the first match for the
      -- key)
    : map (fmap $ expandDirectories (topDir dflags) (toolDir dflags))
          (rawSettings dflags)
   ++ [("Project version",             projectVersion dflags),
       ("Project Git commit id",       cProjectGitCommitId),
       ("Booter version",              cBooterVersion),
       ("Stage",                       cStage),
       ("Build platform",              cBuildPlatformString),
       ("Host platform",               cHostPlatformString),
       ("Target platform",             platformMisc_targetPlatformString $ platformMisc dflags),
       ("Have interpreter",            showBool $ platformMisc_ghcWithInterpreter $ platformMisc dflags),
       ("Object splitting supported",  showBool False),
       ("Have native code generator",  showBool $ platformMisc_ghcWithNativeCodeGen $ platformMisc dflags),
       -- Whether or not we support @-dynamic-too@
       ("Support dynamic-too",         showBool $ not isWindows),
       -- Whether or not we support the @-j@ flag with @--make@.
       ("Support parallel --make",     "YES"),
       -- Whether or not we support "Foo from foo-0.1-XXX:Foo" syntax in
       -- installed package info.
       ("Support reexported-modules",  "YES"),
       -- Whether or not we support extended @-package foo (Foo)@ syntax.
       ("Support thinning and renaming package flags", "YES"),
       -- Whether or not we support Backpack.
       ("Support Backpack", "YES"),
       -- If true, we require that the 'id' field in installed package info
       -- match what is passed to the @-this-unit-id@ flag for modules
       -- built in it
       ("Requires unified installed package IDs", "YES"),
       -- Whether or not we support the @-this-package-key@ flag.  Prefer
       -- "Uses unit IDs" over it.
       ("Uses package keys",           "YES"),
       -- Whether or not we support the @-this-unit-id@ flag
       ("Uses unit IDs",               "YES"),
       -- Whether or not GHC compiles libraries as dynamic by default
       ("Dynamic by default",          showBool $ dYNAMIC_BY_DEFAULT dflags),
       -- Whether or not GHC was compiled using -dynamic
       ("GHC Dynamic",                 showBool hostIsDynamic),
       -- Whether or not GHC was compiled using -prof
       ("GHC Profiled",                showBool hostIsProfiled),
       ("Debug on",                    showBool debugIsOn),
       ("LibDir",                      topDir dflags),
       -- The path of the global package database used by GHC
       ("Global Package DB",           globalPackageDatabasePath dflags)
      ]
  where
    showBool True  = "YES"
    showBool False = "NO"
    isWindows = platformOS (targetPlatform dflags) == OSMinGW32
    expandDirectories :: FilePath -> Maybe FilePath -> String -> String
    expandDirectories topd mtoold = expandToolDir mtoold . expandTopDir topd

-- Produced by deriveConstants
#include "GHCConstantsHaskellWrappers.hs"

bLOCK_SIZE_W :: DynFlags -> Int
bLOCK_SIZE_W dflags = bLOCK_SIZE dflags `quot` platformWordSizeInBytes platform
   where platform = targetPlatform dflags

wordAlignment :: Platform -> Alignment
wordAlignment platform = alignmentOf (platformWordSizeInBytes platform)

tAG_MASK :: DynFlags -> Int
tAG_MASK dflags = (1 `shiftL` tAG_BITS dflags) - 1

mAX_PTR_TAG :: DynFlags -> Int
mAX_PTR_TAG = tAG_MASK

{- -----------------------------------------------------------------------------
Note [DynFlags consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a number of number of DynFlags configurations which either
do not make sense or lead to unimplemented or buggy codepaths in the
compiler. makeDynFlagsConsistent is responsible for verifying the validity
of a set of DynFlags, fixing any issues, and reporting them back to the
caller.

GHCi and -O
---------------

When using optimization, the compiler can introduce several things
(such as unboxed tuples) into the intermediate code, which GHCi later
chokes on since the bytecode interpreter can't handle this (and while
this is arguably a bug these aren't handled, there are no plans to fix
it.)

While the driver pipeline always checks for this particular erroneous
combination when parsing flags, we also need to check when we update
the flags; this is because API clients may parse flags but update the
DynFlags afterwords, before finally running code inside a session (see
T10052 and #10052).
-}

-- | Resolve any internal inconsistencies in a set of 'DynFlags'.
-- Returns the consistent 'DynFlags' as well as a list of warnings
-- to report to the user.
makeDynFlagsConsistent :: DynFlags -> (DynFlags, [Located String])
-- Whenever makeDynFlagsConsistent does anything, it starts over, to
-- ensure that a later change doesn't invalidate an earlier check.
-- Be careful not to introduce potential loops!
makeDynFlagsConsistent dflags
 -- Disable -dynamic-too on Windows (#8228, #7134, #5987)
 | os == OSMinGW32 && gopt Opt_BuildDynamicToo dflags
    = let dflags' = gopt_unset dflags Opt_BuildDynamicToo
          warn    = "-dynamic-too is not supported on Windows"
      in loop dflags' warn
 | hscTarget dflags == HscC &&
   not (platformUnregisterised (targetPlatform dflags))
    = if platformMisc_ghcWithNativeCodeGen $ platformMisc dflags
      then let dflags' = dflags { hscTarget = HscAsm }
               warn = "Compiler not unregisterised, so using native code generator rather than compiling via C"
           in loop dflags' warn
      else let dflags' = dflags { hscTarget = HscLlvm }
               warn = "Compiler not unregisterised, so using LLVM rather than compiling via C"
           in loop dflags' warn
 | gopt Opt_Hpc dflags && hscTarget dflags == HscInterpreted
    = let dflags' = gopt_unset dflags Opt_Hpc
          warn = "Hpc can't be used with byte-code interpreter. Ignoring -fhpc."
      in loop dflags' warn
 | hscTarget dflags `elem` [HscAsm, HscLlvm] &&
   platformUnregisterised (targetPlatform dflags)
    = loop (dflags { hscTarget = HscC })
           "Compiler unregisterised, so compiling via C"
 | hscTarget dflags == HscAsm &&
   not (platformMisc_ghcWithNativeCodeGen $ platformMisc dflags)
      = let dflags' = dflags { hscTarget = HscLlvm }
            warn = "No native code generator, so using LLVM"
        in loop dflags' warn
 | not (osElfTarget os) && gopt Opt_PIE dflags
    = loop (gopt_unset dflags Opt_PIE)
           "Position-independent only supported on ELF platforms"
 | os == OSDarwin &&
   arch == ArchX86_64 &&
   not (gopt Opt_PIC dflags)
    = loop (gopt_set dflags Opt_PIC)
           "Enabling -fPIC as it is always on for this platform"
 | Left err <- checkOptLevel (optLevel dflags) dflags
    = loop (updOptLevel 0 dflags) err

 | LinkInMemory <- ghcLink dflags
 , not (gopt Opt_ExternalInterpreter dflags)
 , hostIsProfiled
 , isObjectTarget (hscTarget dflags)
 , WayProf `Set.notMember` ways dflags
    = loop dflags{ways = Set.insert WayProf (ways dflags)}
         "Enabling -prof, because -fobject-code is enabled and GHCi is profiled"

 | otherwise = (dflags, [])
    where loc = mkGeneralSrcSpan (fsLit "when making flags consistent")
          loop updated_dflags warning
              = case makeDynFlagsConsistent updated_dflags of
                (dflags', ws) -> (dflags', L loc warning : ws)
          platform = targetPlatform dflags
          arch = platformArch platform
          os   = platformOS   platform


--------------------------------------------------------------------------
-- Do not use unsafeGlobalDynFlags!
--
-- unsafeGlobalDynFlags is a hack, necessary because we need to be able
-- to show SDocs when tracing, but we don't always have DynFlags
-- available.
--
-- Do not use it if you can help it. You may get the wrong value, or this
-- panic!

-- | This is the value that 'unsafeGlobalDynFlags' takes before it is
-- initialized.
defaultGlobalDynFlags :: DynFlags
defaultGlobalDynFlags =
    (defaultDynFlags settings llvmConfig) { verbosity = 2 }
  where
    settings = panic "v_unsafeGlobalDynFlags: settings not initialised"
    llvmConfig = panic "v_unsafeGlobalDynFlags: llvmConfig not initialised"

#if GHC_STAGE < 2
GLOBAL_VAR(v_unsafeGlobalDynFlags, defaultGlobalDynFlags, DynFlags)
#else
SHARED_GLOBAL_VAR( v_unsafeGlobalDynFlags
                 , getOrSetLibHSghcGlobalDynFlags
                 , "getOrSetLibHSghcGlobalDynFlags"
                 , defaultGlobalDynFlags
                 , DynFlags )
#endif

unsafeGlobalDynFlags :: DynFlags
unsafeGlobalDynFlags = unsafePerformIO $ readIORef v_unsafeGlobalDynFlags

setUnsafeGlobalDynFlags :: DynFlags -> IO ()
setUnsafeGlobalDynFlags = writeIORef v_unsafeGlobalDynFlags

-- -----------------------------------------------------------------------------
-- SSE and AVX

-- TODO: Instead of using a separate predicate (i.e. isSse2Enabled) to
-- check if SSE is enabled, we might have x86-64 imply the -msse2
-- flag.

data SseVersion = SSE1
                | SSE2
                | SSE3
                | SSE4
                | SSE42
                deriving (Eq, Ord)

isSseEnabled :: DynFlags -> Bool
isSseEnabled dflags = case platformArch (targetPlatform dflags) of
    ArchX86_64 -> True
    ArchX86    -> True
    _          -> False

isSse2Enabled :: DynFlags -> Bool
isSse2Enabled dflags = case platformArch (targetPlatform dflags) of
  -- We Assume  SSE1 and SSE2 operations are available on both
  -- x86 and x86_64. Historically we didn't default to SSE2 and
  -- SSE1 on x86, which results in defacto nondeterminism for how
  -- rounding behaves in the associated x87 floating point instructions
  -- because variations in the spill/fpu stack placement of arguments for
  -- operations would change the precision and final result of what
  -- would otherwise be the same expressions with respect to single or
  -- double precision IEEE floating point computations.
    ArchX86_64 -> True
    ArchX86    -> True
    _          -> False


isSse4_2Enabled :: DynFlags -> Bool
isSse4_2Enabled dflags = sseVersion dflags >= Just SSE42

isAvxEnabled :: DynFlags -> Bool
isAvxEnabled dflags = avx dflags || avx2 dflags || avx512f dflags

isAvx2Enabled :: DynFlags -> Bool
isAvx2Enabled dflags = avx2 dflags || avx512f dflags

isAvx512cdEnabled :: DynFlags -> Bool
isAvx512cdEnabled dflags = avx512cd dflags

isAvx512erEnabled :: DynFlags -> Bool
isAvx512erEnabled dflags = avx512er dflags

isAvx512fEnabled :: DynFlags -> Bool
isAvx512fEnabled dflags = avx512f dflags

isAvx512pfEnabled :: DynFlags -> Bool
isAvx512pfEnabled dflags = avx512pf dflags

-- -----------------------------------------------------------------------------
-- BMI2

data BmiVersion = BMI1
                | BMI2
                deriving (Eq, Ord)

isBmiEnabled :: DynFlags -> Bool
isBmiEnabled dflags = case platformArch (targetPlatform dflags) of
    ArchX86_64 -> bmiVersion dflags >= Just BMI1
    ArchX86    -> bmiVersion dflags >= Just BMI1
    _          -> False

isBmi2Enabled :: DynFlags -> Bool
isBmi2Enabled dflags = case platformArch (targetPlatform dflags) of
    ArchX86_64 -> bmiVersion dflags >= Just BMI2
    ArchX86    -> bmiVersion dflags >= Just BMI2
    _          -> False

-- -----------------------------------------------------------------------------
-- Linker/compiler information

-- LinkerInfo contains any extra options needed by the system linker.
data LinkerInfo
  = GnuLD    [Option]
  | GnuGold  [Option]
  | LlvmLLD  [Option]
  | DarwinLD [Option]
  | SolarisLD [Option]
  | AixLD    [Option]
  | UnknownLD
  deriving Eq

-- CompilerInfo tells us which C compiler we're using
data CompilerInfo
   = GCC
   | Clang
   | AppleClang
   | AppleClang51
   | UnknownCC
   deriving Eq

-- -----------------------------------------------------------------------------
-- RTS hooks

-- Convert sizes like "3.5M" into integers
decodeSize :: String -> Integer
decodeSize str
  | c == ""      = truncate n
  | c == "K" || c == "k" = truncate (n * 1000)
  | c == "M" || c == "m" = truncate (n * 1000 * 1000)
  | c == "G" || c == "g" = truncate (n * 1000 * 1000 * 1000)
  | otherwise            = throwGhcException (CmdLineError ("can't decode size: " ++ str))
  where (m, c) = span pred str
        n      = readRational m
        pred c = isDigit c || c == '.'

foreign import ccall unsafe "setHeapSize"       setHeapSize       :: Int -> IO ()
foreign import ccall unsafe "enableTimingStats" enableTimingStats :: IO ()

-- -----------------------------------------------------------------------------
-- Types for managing temporary files.
--
-- these are here because FilesToClean is used in DynFlags

-- | A collection of files that must be deleted before ghc exits.
-- The current collection
-- is stored in an IORef in DynFlags, 'filesToClean'.
data FilesToClean = FilesToClean {
  ftcGhcSession :: !(Set FilePath),
  -- ^ Files that will be deleted at the end of runGhc(T)
  ftcCurrentModule :: !(Set FilePath)
  -- ^ Files that will be deleted the next time
  -- 'FileCleanup.cleanCurrentModuleTempFiles' is called, or otherwise at the
  -- end of the session.
  }

-- | An empty FilesToClean
emptyFilesToClean :: FilesToClean
emptyFilesToClean = FilesToClean Set.empty Set.empty



initSDocContext :: DynFlags -> PprStyle -> SDocContext
initSDocContext dflags style = SDC
  { sdocStyle                       = style
  , sdocColScheme                   = colScheme dflags
  , sdocLastColour                  = Col.colReset
  , sdocShouldUseColor              = overrideWith (canUseColor dflags) (useColor dflags)
  , sdocLineLength                  = pprCols dflags
  , sdocCanUseUnicode               = useUnicode dflags
  , sdocHexWordLiterals             = gopt Opt_HexWordLiterals dflags
  , sdocPprDebug                    = dopt Opt_D_ppr_debug dflags
  , sdocPrintUnicodeSyntax          = gopt Opt_PrintUnicodeSyntax dflags
  , sdocPrintCaseAsLet              = gopt Opt_PprCaseAsLet dflags
  , sdocPrintTypecheckerElaboration = gopt Opt_PrintTypecheckerElaboration dflags
  , sdocPrintAxiomIncomps           = gopt Opt_PrintAxiomIncomps dflags
  , sdocPrintExplicitKinds          = gopt Opt_PrintExplicitKinds dflags
  , sdocPrintExplicitCoercions      = gopt Opt_PrintExplicitCoercions dflags
  , sdocPrintExplicitRuntimeReps    = gopt Opt_PrintExplicitRuntimeReps dflags
  , sdocPrintExplicitForalls        = gopt Opt_PrintExplicitForalls dflags
  , sdocPrintPotentialInstances     = gopt Opt_PrintPotentialInstances dflags
  , sdocPrintEqualityRelations      = gopt Opt_PrintEqualityRelations dflags
  , sdocSuppressTicks               = gopt Opt_SuppressTicks dflags
  , sdocSuppressTypeSignatures      = gopt Opt_SuppressTypeSignatures dflags
  , sdocSuppressTypeApplications    = gopt Opt_SuppressTypeApplications dflags
  , sdocSuppressIdInfo              = gopt Opt_SuppressIdInfo dflags
  , sdocSuppressCoercions           = gopt Opt_SuppressCoercions dflags
  , sdocSuppressUnfoldings          = gopt Opt_SuppressUnfoldings dflags
  , sdocSuppressVarKinds            = gopt Opt_SuppressVarKinds dflags
  , sdocSuppressUniques             = gopt Opt_SuppressUniques dflags
  , sdocSuppressModulePrefixes      = gopt Opt_SuppressModulePrefixes dflags
  , sdocSuppressStgExts             = gopt Opt_SuppressStgExts dflags
  , sdocErrorSpans                  = gopt Opt_ErrorSpans dflags
  , sdocStarIsType                  = xopt LangExt.StarIsType dflags
  , sdocImpredicativeTypes          = xopt LangExt.ImpredicativeTypes dflags
  , sdocDynFlags                    = dflags
  }
