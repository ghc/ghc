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

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Driver.Session (
        -- * Dynamic flags and associated configuration types
        DumpFlag(..),
        GeneralFlag(..),
        WarningFlag(..), DiagnosticReason(..),
        Language(..),
        PlatformConstants(..),
        FatalMessager, FlushOut(..),
        ProfAuto(..),
        glasgowExtsFlags,
        hasPprDebug, hasNoDebugOutput, hasNoStateHack, hasNoOptCoercion,
        dopt, dopt_set, dopt_unset,
        gopt, gopt_set, gopt_unset, setGeneralFlag', unSetGeneralFlag',
        wopt, wopt_set, wopt_unset,
        wopt_fatal, wopt_set_fatal, wopt_unset_fatal,
        xopt, xopt_set, xopt_unset,
        xopt_set_unlessExplSpec,
        xopt_DuplicateRecordFields,
        xopt_FieldSelectors,
        lang_set,
        DynamicTooState(..), dynamicTooState, setDynamicNow,
        sccProfilingEnabled,
        DynFlags(..),
        outputFile, objectSuf, ways,
        FlagSpec(..),
        HasDynFlags(..), ContainsDynFlags(..),
        RtsOptsEnabled(..),
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
        makeDynFlagsConsistent,
        positionIndependent,
        optimisationFlags,
        setFlagsFromEnvFile,
        pprDynFlagsDiff,
        flagSpecOf,

        targetProfile,

        -- ** Safe Haskell
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
        sPgm_lm,
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
        sOpt_lm,
        sOpt_windres,
        sOpt_lo,
        sOpt_lc,
        sOpt_lcc,
        sOpt_i,
        sExtraGccViaCFlags,
        sTargetPlatformString,
        sGhcWithInterpreter,
        sLibFFI,
        GhcNameVersion(..),
        FileSettings(..),
        PlatformMisc(..),
        settings,
        programName, projectVersion,
        ghcUsagePath, ghciUsagePath, topDir,
        versionedAppDir, versionedFilePath,
        extraGccViaCFlags, globalPackageDatabasePath,
        pgm_L, pgm_P, pgm_F, pgm_c, pgm_a, pgm_l, pgm_lm, pgm_dll, pgm_T,
        pgm_windres, pgm_libtool, pgm_ar, pgm_otool, pgm_install_name_tool,
        pgm_ranlib, pgm_lo, pgm_lc, pgm_lcc, pgm_i,
        opt_L, opt_P, opt_F, opt_c, opt_cxx, opt_a, opt_l, opt_lm, opt_i,
        opt_P_signature,
        opt_windres, opt_lo, opt_lc, opt_lcc,
        updatePlatformConstants,

        -- ** Manipulating DynFlags
        addPluginModuleName,
        defaultDynFlags,                -- Settings -> DynFlags
        initDynFlags,                   -- DynFlags -> IO DynFlags
        defaultFatalMessager,
        defaultFlushOut,
        setOutputFile, setDynOutputFile, setOutputHi, setDynOutputHi,

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlags,
        updOptLevel,
        setTmpDir,
        setUnitId,

        TurnOnFlag,
        turnOn,
        turnOff,
        impliedGFlags,
        impliedOffGFlags,
        impliedXFlags,

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

        -- ** DynFlags C linker options
        pieCCLDOpts,

        -- * Compiler configuration suitable for display to the user
        compilerInfo,

        wordAlignment,

        setUnsafeGlobalDynFlags,

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
        useXLinkerRPath,

        -- * Include specifications
        IncludeSpecs(..), addGlobalInclude, addQuoteInclude, flattenIncludes,
        addImplicitQuoteInclude,

        -- * SDoc
        initSDocContext, initDefaultSDocContext,
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways
import GHC.Platform.Profile

import GHC.UniqueSubdir (uniqueSubdir)
import GHC.Unit.Types
import GHC.Unit.Parser
import GHC.Unit.Module
import GHC.Builtin.Names ( mAIN_NAME )
import GHC.Driver.Phases ( Phase(..), phaseInputExt )
import GHC.Driver.Flags
import GHC.Driver.Backend
import GHC.Settings.Config
import GHC.Utils.CliOption
import GHC.Core.Unfold
import GHC.Driver.CmdLine
import GHC.Settings.Constants
import GHC.Utils.Panic
import qualified GHC.Utils.Ppr.Colour as Col
import GHC.Utils.Misc
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.GlobalVars
import GHC.Data.Maybe
import GHC.Data.Bool
import GHC.Utils.Monad
import GHC.Types.Error (DiagnosticReason(..))
import GHC.Types.SrcLoc
import GHC.Types.SafeHaskell
import GHC.Types.Basic ( Alignment, alignmentOf, IntWithInf, treatZeroAsInf )
import qualified GHC.Types.FieldLabel as FieldLabel
import GHC.Data.FastString
import GHC.Utils.TmpFs
import GHC.Utils.Fingerprint
import GHC.Utils.Outputable
import GHC.Settings
import GHC.CmmToAsm.CFG.Weight
import {-# SOURCE #-} GHC.Core.Opt.CallerCC

import GHC.SysTools.Terminal ( stderrSupportsAnsiColors )
import GHC.SysTools.BaseDir ( expandToolDir, expandTopDir )

import Data.IORef
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Data.Ord
import Data.Char
import Data.List (intercalate, sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath
import System.Directory
import System.Environment (lookupEnv)
import System.IO
import System.IO.Error
import Text.ParserCombinators.ReadP hiding (char)
import Text.ParserCombinators.ReadP as R

import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet

import GHC.Foreign (withCString, peekCString)
import qualified GHC.LanguageExtensions as LangExt

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
                 -- | See note [Implicit include paths]
                 , includePathsQuoteImplicit :: [String]
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

-- | These includes are not considered while fingerprinting the flags for iface
-- | See note [Implicit include paths]
addImplicitQuoteInclude :: IncludeSpecs -> [String] -> IncludeSpecs
addImplicitQuoteInclude spec paths  = let f = includePathsQuoteImplicit spec
                              in spec { includePathsQuoteImplicit = f ++ paths }


-- | Concatenate and flatten the list of global and quoted includes returning
-- just a flat list of paths.
flattenIncludes :: IncludeSpecs -> [String]
flattenIncludes specs =
    includePathsQuote specs ++
    includePathsQuoteImplicit specs ++
    includePathsGlobal specs

{- Note [Implicit include paths]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  The compile driver adds the path to the folder containing the source file being
  compiled to the 'IncludeSpecs', and this change gets recorded in the 'DynFlags'
  that are used later to compute the interface file. Because of this,
  the flags fingerprint derived from these 'DynFlags' and recorded in the
  interface file will end up containing the absolute path to the source folder.

  Build systems with a remote cache like Bazel or Buck (or Shake, see #16956)
  store the build artifacts produced by a build BA for reuse in subsequent builds.

  Embedding source paths in interface fingerprints will thwart these attemps and
  lead to unnecessary recompilations when the source paths in BA differ from the
  source paths in subsequent builds.
 -}


-- | Contains not only a collection of 'GeneralFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  ghcMode               :: GhcMode,
  ghcLink               :: GhcLink,
  backend               :: !Backend,
   -- ^ The backend to use (if any).
   --
   -- Whenever you change the backend, also make sure to set 'ghcLink' to
   -- something sensible.
   --
   -- 'NoBackend' can be used to avoid generating any output, however, note that:
   --
   --  * If a program uses Template Haskell the typechecker may need to run code
   --    from an imported module.  To facilitate this, code generation is enabled
   --    for modules imported by modules that use template haskell, using the
   --    default backend for the platform.
   --    See Note [-fno-code mode].


  -- formerly Settings
  ghcNameVersion    :: {-# UNPACK #-} !GhcNameVersion,
  fileSettings      :: {-# UNPACK #-} !FileSettings,
  targetPlatform    :: Platform,       -- Filled in by SysTools
  toolSettings      :: {-# UNPACK #-} !ToolSettings,
  platformMisc      :: {-# UNPACK #-} !PlatformMisc,
  rawSettings       :: [(String, String)],
  tmpDir            :: TempDir,

  llvmConfig            :: LlvmConfig,
    -- ^ N.B. It's important that this field is lazy since we load the LLVM
    -- configuration lazily. See Note [LLVM Configuration] in "GHC.SysTools".
  llvmOptLevel          :: Int,         -- ^ LLVM optimisation level
  verbosity             :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
  debugLevel            :: Int,         -- ^ How much debug information to produce
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  ruleCheck             :: Maybe String,
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
  dmdUnboxWidth         :: !Int,        -- ^ Whether DmdAnal should optimistically put an
                                        --   Unboxed demand on returned products with at most
                                        --   this number of fields
  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  specConstrRecursive   :: Int,         -- ^ Max number of specialisations for recursive types
                                        --   Not optional; otherwise ForceSpecConstr can diverge.
  binBlobThreshold      :: Word,        -- ^ Binary literals (e.g. strings) whose size is above
                                        --   this threshold will be dumped in a binary file
                                        --   by the assembler code generator (0 to disable)
  liberateCaseThreshold :: Maybe Int,   -- ^ Threshold for LiberateCase
  floatLamArgs          :: Maybe Int,   -- ^ Arg count for lambda floating
                                        --   See 'GHC.Core.Opt.Monad.FloatOutSwitches'

  liftLamsRecArgs       :: Maybe Int,   -- ^ Maximum number of arguments after lambda lifting a
                                        --   recursive function.
  liftLamsNonRecArgs    :: Maybe Int,   -- ^ Maximum number of arguments after lambda lifting a
                                        --   non-recursive function.
  liftLamsKnown         :: Bool,        -- ^ Lambda lift even when this turns a known call
                                        --   into an unknown call.

  cmmProcAlignment      :: Maybe Int,   -- ^ Align Cmm functions at this boundary or use default.

  historySize           :: Int,         -- ^ Simplification history size

  importPaths           :: [FilePath],
  mainModuleNameIs      :: ModuleName,
  mainFunIs             :: Maybe String,
  reductionDepth        :: IntWithInf,   -- ^ Typechecker maximum stack depth
  solverIterations      :: IntWithInf,   -- ^ Number of iterations in the constraints solver
                                         --   Typically only 1 is needed

  homeUnitId_             :: UnitId,                 -- ^ Target home unit-id
  homeUnitInstanceOf_     :: Maybe UnitId,           -- ^ Id of the unit to instantiate
  homeUnitInstantiations_ :: [(ModuleName, Module)], -- ^ Module instantiations

  -- ways
  targetWays_           :: Ways,         -- ^ Target way flags from the command line

  -- For object splitting
  splitInfo             :: Maybe (String,Int),

  -- paths etc.
  objectDir             :: Maybe String,
  dylibInstallName      :: Maybe String,
  hiDir                 :: Maybe String,
  hieDir                :: Maybe String,
  stubDir               :: Maybe String,
  dumpDir               :: Maybe String,

  objectSuf_            :: String,
  hcSuf                 :: String,
  hiSuf_                :: String,
  hieSuf                :: String,

  dynObjectSuf_         :: String,
  dynHiSuf_             :: String,

  outputFile_           :: Maybe String,
  dynOutputFile_        :: Maybe String,
  outputHi              :: Maybe String,
  dynOutputHi           :: Maybe String,
  dynLibLoader          :: DynLibLoader,

  dynamicNow            :: !Bool, -- ^ Indicate if we are now generating dynamic output
                                  -- because of -dynamic-too. This predicate is
                                  -- used to query the appropriate fields
                                  -- (outputFile/dynOutputFile, ways, etc.)

  -- | This defaults to 'non-module'. It can be set by
  -- 'GHC.Driver.Pipeline.setDumpPrefix' or 'ghc.GHCi.UI.runStmt' based on
  -- where its output is going.
  dumpPrefix            :: FilePath,

  -- | Override the 'dumpPrefix' set by 'GHC.Driver.Pipeline.setDumpPrefix'
  --    or 'ghc.GHCi.UI.runStmt'.
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
        -- 'getUnitDbRefs'.

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
  deriveViaOnLoc        :: SrcSpan,
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

  -- | Unfolding control
  -- See Note [Discounts and thresholds] in GHC.Core.Unfold
  unfoldingOpts         :: !UnfoldingOpts,

  maxWorkerArgs         :: Int,

  ghciHistSize          :: Int,

  flushOut              :: FlushOut,

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
  callerCcFilters       :: [CallerCcFilter],

  interactivePrint      :: Maybe String,

  -- | Machine dependent flags (-m\<blah> stuff)
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

  -- | Run-time C compiler information
  rtccInfo              :: IORef (Maybe CompilerInfo),

  -- | Run-time assembler information
  rtasmInfo              :: IORef (Maybe CompilerInfo),

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
  initialUnique         :: Word,
  uniqueIncrement       :: Int,
    -- 'Int' because it can be used to test uniques in decreasing order.

  -- | Temporary: CFG Edge weights for fast iterations
  cfgWeights            :: Weights
}

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

-- | See Note [LLVM Configuration] in "GHC.SysTools".
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
pgm_lm                 :: DynFlags -> (String,[Option])
pgm_lm dflags = toolSettings_pgm_lm $ toolSettings dflags
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
pgm_otool             :: DynFlags -> String
pgm_otool dflags = toolSettings_pgm_otool $ toolSettings dflags
pgm_install_name_tool :: DynFlags -> String
pgm_install_name_tool dflags = toolSettings_pgm_install_name_tool $ toolSettings dflags
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
opt_lm                :: DynFlags -> [String]
opt_lm dflags= toolSettings_opt_lm $ toolSettings dflags
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

-- | The directory for this version of ghc in the user's app directory
-- (typically something like @~/.ghc/x86_64-linux-7.6.3@)
--
versionedAppDir :: String -> ArchOS -> MaybeT IO FilePath
versionedAppDir appname platform = do
  -- Make sure we handle the case the HOME isn't set (see #11678)
  appdir <- tryMaybeT $ getXdgDirectory XdgData appname
  return $ appdir </> versionedFilePath platform

versionedFilePath :: ArchOS -> FilePath
versionedFilePath platform = uniqueSubdir platform

-- | The 'GhcMode' tells us whether we're doing multi-module
-- compilation (controlled via the "GHC" API) or one-shot
-- (single-module) compilation.  This makes a difference primarily to
-- the "GHC.Unit.Finder": in one-shot mode we look for interface files for
-- imported modules, but in multi-module mode we look for source files
-- in order to check whether they need to be recompiled.
data GhcMode
  = CompManager         -- ^ @\-\-make@, GHCi, etc.
  | OneShot             -- ^ @ghc -c Foo.hs@
  | MkDepend            -- ^ @ghc -M@, see "GHC.Unit.Finder" for why we need this
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
    | UnitIdArg Unit       -- ^ @-package-id@, by 'Unit'
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

-- Note [-dynamic-too business]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- With -dynamic-too flag, we try to build both the non-dynamic and dynamic
-- objects in a single run of the compiler: the pipeline is the same down to
-- Core optimisation, then the backend (from Core to object code) is executed
-- twice.
--
-- The implementation is currently rather hacky, for example, we don't clearly separate non-dynamic
-- and dynamic loaded interfaces (#9176).
--
-- To make matters worse, we automatically enable -dynamic-too when some modules
-- need Template-Haskell and GHC is dynamically linked (cf
-- GHC.Driver.Pipeline.compileOne').
--
-- We used to try and fall back from a dynamic-too failure but this feature
-- didn't work as expected (#20446) so it was removed to simplify the
-- implementation and not obscure latent bugs.

data DynamicTooState
   = DT_Dont    -- ^ Don't try to build dynamic objects too
   | DT_OK      -- ^ Will still try to generate dynamic objects
   | DT_Dyn     -- ^ Currently generating dynamic objects (in the backend)
   deriving (Eq,Show,Ord)

dynamicTooState :: DynFlags -> DynamicTooState
dynamicTooState dflags
   | not (gopt Opt_BuildDynamicToo dflags) = DT_Dont
   | dynamicNow dflags = DT_Dyn
   | otherwise = DT_OK

setDynamicNow :: DynFlags -> DynFlags
setDynamicNow dflags0 =
   dflags0
      { dynamicNow = True
      }

-----------------------------------------------------------------------------

-- | Used by 'GHC.runGhc' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 let
 refRtldInfo <- newIORef Nothing
 refRtccInfo <- newIORef Nothing
 refRtasmInfo <- newIORef Nothing
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
 tmp_dir <- normalise <$> getTemporaryDirectory
 return dflags{
        useUnicode    = useUnicode',
        useColor      = useColor',
        canUseColor   = stderrSupportsAnsiColors,
        colScheme     = colScheme',
        rtldInfo      = refRtldInfo,
        rtccInfo      = refRtccInfo,
        rtasmInfo     = refRtasmInfo,
        tmpDir        = TempDir tmp_dir
        }

-- | The normal 'DynFlags'. Note that they are not suitable for use in this form
-- and must be fully initialized by 'GHC.runGhc' first.
defaultDynFlags :: Settings -> LlvmConfig -> DynFlags
defaultDynFlags mySettings llvmConfig =
-- See Note [Updating flag description in the User's Guide]
     DynFlags {
        ghcMode                 = CompManager,
        ghcLink                 = LinkBinary,
        backend                 = platformDefaultBackend (sTargetPlatform mySettings),
        verbosity               = 0,
        debugLevel              = 0,
        simplPhases             = 2,
        maxSimplIterations      = 4,
        ruleCheck               = Nothing,
        binBlobThreshold        = 500000, -- 500K is a good default (see #16190)
        maxRelevantBinds        = Just 6,
        maxValidHoleFits   = Just 6,
        maxRefHoleFits     = Just 6,
        refLevelHoleFits   = Nothing,
        maxUncoveredPatterns    = 4,
        maxPmCheckModels        = 30,
        simplTickFactor         = 100,
        dmdUnboxWidth           = 3,      -- Default: Assume an unboxed demand on function bodies returning a triple
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
        mainModuleNameIs        = mAIN_NAME,
        mainFunIs               = Nothing,
        reductionDepth          = treatZeroAsInf mAX_REDUCTION_DEPTH,
        solverIterations        = treatZeroAsInf mAX_SOLVER_ITERATIONS,

        homeUnitId_             = mainUnitId,
        homeUnitInstanceOf_     = Nothing,
        homeUnitInstantiations_ = [],

        objectDir               = Nothing,
        dylibInstallName        = Nothing,
        hiDir                   = Nothing,
        hieDir                  = Nothing,
        stubDir                 = Nothing,
        dumpDir                 = Nothing,

        objectSuf_              = phaseInputExt StopLn,
        hcSuf                   = phaseInputExt HCc,
        hiSuf_                  = "hi",
        hieSuf                  = "hie",

        dynObjectSuf_           = "dyn_" ++ phaseInputExt StopLn,
        dynHiSuf_               = "dyn_hi",
        dynamicNow              = False,

        pluginModNames          = [],
        pluginModNameOpts       = [],
        frontendPluginOpts      = [],

        outputFile_             = Nothing,
        dynOutputFile_          = Nothing,
        outputHi                = Nothing,
        dynOutputHi             = Nothing,
        dynLibLoader            = SystemDependent,
        dumpPrefix              = "non-module.",
        dumpPrefixForce         = Nothing,
        ldInputs                = [],
        includePaths            = IncludeSpecs [] [] [],
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
        targetWays_             = Set.empty,
        splitInfo               = Nothing,

        ghcNameVersion = sGhcNameVersion mySettings,
        fileSettings = sFileSettings mySettings,
        toolSettings = sToolSettings mySettings,
        targetPlatform = sTargetPlatform mySettings,
        platformMisc = sPlatformMisc mySettings,
        rawSettings = sRawSettings mySettings,

        tmpDir                  = panic "defaultDynFlags: uninitialized tmpDir",

        -- See Note [LLVM configuration].
        llvmConfig              = llvmConfig,
        llvmOptLevel            = 0,

        -- ghc -M values
        depMakefile       = "Makefile",
        depIncludePkgDeps = False,
        depIncludeCppDeps = False,
        depExcludeMods    = [],
        depSuffixes       = [],
        -- end of ghc -M values
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
        deriveViaOnLoc = noSrcSpan,
        overlapInstLoc = noSrcSpan,
        incoherentOnLoc = noSrcSpan,
        pkgTrustOnLoc = noSrcSpan,
        warnSafeOnLoc = noSrcSpan,
        warnUnsafeOnLoc = noSrcSpan,
        trustworthyOnLoc = noSrcSpan,
        extensions = [],
        extensionFlags = flattenExtensionFlags Nothing [],

        unfoldingOpts = defaultUnfoldingOpts,
        maxWorkerArgs = 10,

        ghciHistSize = 50, -- keep a log of length 50 by default

        flushOut = defaultFlushOut,
        pprUserLength = 5,
        pprCols = 100,
        useUnicode = False,
        useColor = Auto,
        canUseColor = False,
        colScheme = Col.defaultScheme,
        profAuto = NoProfAuto,
        callerCcFilters = [],
        interactivePrint = Nothing,
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
        rtasmInfo = panic "defaultDynFlags: no rtasmInfo",

        maxInlineAllocSize = 128,
        maxInlineMemcpyInsns = 32,
        maxInlineMemsetInsns = 32,

        initialUnique = 0,
        uniqueIncrement = 1,

        reverseErrors = False,
        maxErrors     = Nothing,
        cfgWeights    = defaultWeights
      }

type FatalMessager = String -> IO ()

defaultFatalMessager :: FatalMessager
defaultFatalMessager = hPutStrLn stderr


newtype FlushOut = FlushOut (IO ())

defaultFlushOut :: FlushOut
defaultFlushOut = FlushOut $ hFlush stdout

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
-- @docs/users_guide/exts@.
languageExtensions :: Maybe Language -> [LangExt.Extension]

-- Nothing: the default case
languageExtensions Nothing = languageExtensions (Just GHC2021)

languageExtensions (Just Haskell98)
    = [LangExt.ImplicitPrelude,
       -- See Note [When is StarIsType enabled]
       LangExt.StarIsType,
       LangExt.CUSKs,
       LangExt.MonomorphismRestriction,
       LangExt.NPlusKPatterns,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.FieldSelectors,
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
       LangExt.FieldSelectors,
       LangExt.RelaxedPolyRec]

languageExtensions (Just GHC2021)
    = [LangExt.ImplicitPrelude,
       -- See Note [When is StarIsType enabled]
       LangExt.StarIsType,
       LangExt.MonomorphismRestriction,
       LangExt.TraditionalRecordSyntax,
       LangExt.EmptyDataDecls,
       LangExt.ForeignFunctionInterface,
       LangExt.PatternGuards,
       LangExt.DoAndIfThenElse,
       LangExt.FieldSelectors,
       LangExt.RelaxedPolyRec,
       -- Now the new extensions (not in Haskell2010)
       LangExt.BangPatterns,
       LangExt.BinaryLiterals,
       LangExt.ConstrainedClassMethods,
       LangExt.ConstraintKinds,
       LangExt.DeriveDataTypeable,
       LangExt.DeriveFoldable,
       LangExt.DeriveFunctor,
       LangExt.DeriveGeneric,
       LangExt.DeriveLift,
       LangExt.DeriveTraversable,
       LangExt.EmptyCase,
       LangExt.EmptyDataDeriving,
       LangExt.ExistentialQuantification,
       LangExt.ExplicitForAll,
       LangExt.FlexibleContexts,
       LangExt.FlexibleInstances,
       LangExt.GADTSyntax,
       LangExt.GeneralizedNewtypeDeriving,
       LangExt.HexFloatLiterals,
       LangExt.ImportQualifiedPost,
       LangExt.InstanceSigs,
       LangExt.KindSignatures,
       LangExt.MultiParamTypeClasses,
       LangExt.NamedFieldPuns,
       LangExt.NamedWildCards,
       LangExt.NumericUnderscores,
       LangExt.PolyKinds,
       LangExt.PostfixOperators,
       LangExt.RankNTypes,
       LangExt.ScopedTypeVariables,
       LangExt.StandaloneDeriving,
       LangExt.StandaloneKindSignatures,
       LangExt.TupleSections,
       LangExt.TypeApplications,
       LangExt.TypeOperators,
       LangExt.TypeSynonymInstances]

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
          enableIfVerbose Opt_D_dump_verbose_inlinings      = False
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
--
-- Note that `dynamicNow` (i.e., dynamic objects built with `-dynamic-too`)
-- always implicitly enables Opt_PIC, Opt_ExternalDynamicRefs, and disables
-- Opt_SplitSections.
--
gopt :: GeneralFlag -> DynFlags -> Bool
gopt Opt_PIC dflags
   | dynamicNow dflags = True
gopt Opt_ExternalDynamicRefs dflags
   | dynamicNow dflags = True
gopt Opt_SplitSections dflags
   | dynamicNow dflags = False
gopt f dflags = f `EnumSet.member` generalFlags dflags

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

xopt_DuplicateRecordFields :: DynFlags -> FieldLabel.DuplicateRecordFields
xopt_DuplicateRecordFields dfs
  | xopt LangExt.DuplicateRecordFields dfs = FieldLabel.DuplicateRecordFields
  | otherwise                              = FieldLabel.NoDuplicateRecordFields

xopt_FieldSelectors :: DynFlags -> FieldLabel.FieldSelectors
xopt_FieldSelectors dfs
  | xopt LangExt.FieldSelectors dfs = FieldLabel.FieldSelectors
  | otherwise                       = FieldLabel.NoFieldSelectors

lang_set :: DynFlags -> Maybe Language -> DynFlags
lang_set dflags lang =
   dflags {
            language = lang,
            extensionFlags = flattenExtensionFlags lang (extensions dflags)
          }

-- | Set the Haskell language standard to use
setLanguage :: Language -> DynP ()
setLanguage l = upd (`lang_set` Just l)

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
              , ("-XDerivingVia", deriveViaOnLoc,
                    xopt LangExt.DerivingVia,
                    flip xopt_unset LangExt.DerivingVia)
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
setOutputFile, setDynOutputFile, setOutputHi, setDynOutputHi, setDumpPrefixForce
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

setObjectSuf    f d = d { objectSuf_    = f}
setDynObjectSuf f d = d { dynObjectSuf_ = f}
setHiSuf        f d = d { hiSuf_        = f}
setHieSuf       f d = d { hieSuf        = f}
setDynHiSuf     f d = d { dynHiSuf_     = f}
setHcSuf        f d = d { hcSuf         = f}

setOutputFile    f d = d { outputFile_    = f}
setDynOutputFile f d = d { dynOutputFile_ = f}
setOutputHi      f d = d { outputHi       = f}
setDynOutputHi   f d = d { dynOutputHi    = f}

parseUnitInsts :: String -> Instantiations
parseUnitInsts str = case filter ((=="").snd) (readP_to_S parse str) of
    [(r, "")] -> r
    _ -> throwGhcException $ CmdLineError ("Can't parse -instantiated-with: " ++ str)
  where parse = sepBy parseEntry (R.char ',')
        parseEntry = do
            n <- parseModuleName
            _ <- R.char '='
            m <- parseHoleyModule
            return (n, m)

setUnitInstantiations :: String -> DynFlags -> DynFlags
setUnitInstantiations s d =
    d { homeUnitInstantiations_ = parseUnitInsts s }

setUnitInstanceOf :: String -> DynFlags -> DynFlags
setUnitInstanceOf s d =
    d { homeUnitInstanceOf_ = Just (UnitId (fsLit s)) }

addPluginModuleName :: String -> DynFlags -> DynFlags
addPluginModuleName name d = d { pluginModNames = (mkModuleName name) : (pluginModNames d) }

clearPluginModuleNames :: DynFlags -> DynFlags
clearPluginModuleNames d =
    d { pluginModNames = []
      , pluginModNameOpts = []
      }

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

updOptLevelChanged :: Int -> DynFlags -> (DynFlags, Bool)
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level and signals if any changes took place
updOptLevelChanged n dfs
  = (dfs3, changed1 || changed2 || changed3)
  where
   final_n = max 0 (min 2 n)    -- Clamp to 0 <= n <= 2
   (dfs1, changed1) = foldr unset (dfs , False) remove_gopts
   (dfs2, changed2) = foldr set   (dfs1, False) extra_gopts
   (dfs3, changed3) = setLlvmOptLevel dfs2

   extra_gopts  = [ f | (ns,f) <- optLevelFlags, final_n `elem` ns ]
   remove_gopts = [ f | (ns,f) <- optLevelFlags, final_n `notElem` ns ]

   set f (dfs, changed)
     | gopt f dfs = (dfs, changed)
     | otherwise = (gopt_set dfs f, True)

   unset f (dfs, changed)
     | not (gopt f dfs) = (dfs, changed)
     | otherwise = (gopt_unset dfs f, True)

   setLlvmOptLevel dfs
     | llvmOptLevel dfs /= final_n = (dfs{ llvmOptLevel = final_n }, True)
     | otherwise = (dfs, False)

updOptLevel :: Int -> DynFlags -> DynFlags
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level
updOptLevel n = fst . updOptLevelChanged n

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
  let rdr = renderWithContext (initSDocContext dflags0 defaultUserStyle)
  unless (null errs) $ liftIO $ throwGhcExceptionIO $ errorsToGhcException $
    map ((rdr . ppr . getLoc &&& unLoc) . errMsg) $ errs

  -- check for disabled flags in safe haskell
  let (dflags2, sh_warns) = safeFlagCheck cmdline dflags1
      theWays = ways dflags2

  unless (allowed_combination theWays) $ liftIO $
      throwGhcExceptionIO (CmdLineError ("combination not supported: " ++
                               intercalate "/" (map wayDesc (Set.toAscList theWays))))

  let (dflags3, consistency_warnings) = makeDynFlagsConsistent dflags2

  -- Set timer stats & heap size
  when (enableTimeStats dflags3) $ liftIO enableTimingStats
  case (ghcHeapSize dflags3) of
    Just x -> liftIO (setHeapSize x)
    _      -> return ()

  liftIO $ setUnsafeGlobalDynFlags dflags3

  let warns' = map (Warn WarningWithoutFlag) (consistency_warnings ++ sh_warns)

  return (dflags3, leftover, warns' ++ warns)

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
  case safeInferOn dflags of
    True   -> (dflags' { safeInferred = safeFlags }, warn)
    False  -> (dflags', warn)

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
add_dep_message (WordSuffix f) message =
                                  WordSuffix $ \i -> f i >> deprecate message
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
  , make_ord_flag defFlag "instantiated-with"   (sepArg setUnitInstantiations)
  , make_ord_flag defFlag "this-component-id"   (sepArg setUnitInstanceOf)

    -- RTS options -------------------------------------------------------------
  , make_ord_flag defFlag "H"           (HasArg (\s -> upd (\d ->
          d { ghcHeapSize = Just $ fromIntegral (decodeSize s)})))

  , make_ord_flag defFlag "Rghc-timing" (NoArg (upd (\d ->
                                               d { enableTimeStats = True })))

    ------- ways ---------------------------------------------------------------
  , make_ord_flag defGhcFlag "prof"           (NoArg (addWayDynP WayProf))
  , make_ord_flag defGhcFlag "eventlog"       (NoArg (addWayDynP WayTracing))
  , make_ord_flag defGhcFlag "debug"          (NoArg (addWayDynP WayDebug))
  , make_ord_flag defGhcFlag "threaded"       (NoArg (addWayDynP WayThreaded))

  , make_ord_flag defGhcFlag "ticky"
      (NoArg (setGeneralFlag Opt_Ticky >> addWayDynP WayDebug))

    -- -ticky enables ticky-ticky code generation, and also implies -debug which
    -- is required to get the RTS ticky support.

        ----- Linker --------------------------------------------------------
  , make_ord_flag defGhcFlag "static"         (NoArg removeWayDyn)
  , make_ord_flag defGhcFlag "dynamic"        (NoArg (addWayDynP WayDyn))
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
  , make_ord_flag defFlag "pgmlm"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_lm  = (f,[]) }
  , make_ord_flag defFlag "pgmi"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_i   =  f }
  , make_ord_flag defFlag "pgmL"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_L   = f }
  , make_ord_flag defFlag "pgmP"
      (hasArg setPgmP)
  , make_ord_flag defFlag "pgmF"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_F   = f }
  , make_ord_flag defFlag "pgmc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_c   = f }
  , (Deprecated, defFlag  "pgmc-supports-no-pie"
      $ noArgM  $ \d -> do
        deprecate $ "use -pgml-supports-no-pie instead"
        pure $ alterToolSettings (\s -> s { toolSettings_ccSupportsNoPie = True }) d)
  , make_ord_flag defFlag "pgms"
      (HasArg (\_ -> addWarn "Object splitting was removed in GHC 8.8"))
  , make_ord_flag defFlag "pgma"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_a   = (f,[]) }
  , make_ord_flag defFlag "pgml"
      $ hasArg $ \f -> alterToolSettings $ \s -> s
         { toolSettings_pgm_l   = (f,[])
         , -- Don't pass -no-pie with custom -pgml (see #15319). Note
           -- that this could break when -no-pie is actually needed.
           -- But the CC_SUPPORTS_NO_PIE check only happens at
           -- buildtime, and -pgml is a runtime option. A better
           -- solution would be running this check for each custom
           -- -pgml.
           toolSettings_ccSupportsNoPie = False
         }
  , make_ord_flag defFlag "pgml-supports-no-pie"
      $ noArg $ alterToolSettings $ \s -> s { toolSettings_ccSupportsNoPie = True }
  , make_ord_flag defFlag "pgmdll"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_dll = (f,[]) }
  , make_ord_flag defFlag "pgmwindres"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_windres = f }
  , make_ord_flag defFlag "pgmlibtool"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_libtool = f }
  , make_ord_flag defFlag "pgmar"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_ar = f }
  , make_ord_flag defFlag "pgmotool"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_otool = f}
  , make_ord_flag defFlag "pgminstall_name_tool"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_install_name_tool = f}
  , make_ord_flag defFlag "pgmranlib"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_ranlib = f }


    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , make_ord_flag defFlag "optlm"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_lm  = f : toolSettings_opt_lm s }
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
        (noArg (\d -> setGeneralFlag' Opt_LinkRts (d { ghcLink=LinkStaticLib })))
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
  , make_ord_flag defGhcFlag "dynohi"
        (hasArg (setDynOutputHi . Just ))
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
        (hasArg (setDumpPrefixForce . Just . flip (++) "."))

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
        (NoArg $ setObjBackend LLVM >> setGeneralFlag Opt_KeepLlvmFiles)
  , make_ord_flag defGhcFlag "keep-llvm-files"
        (NoArg $ setObjBackend LLVM >> setGeneralFlag Opt_KeepLlvmFiles)
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
                  setGeneralFlag Opt_SuppressCoreSizes
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
  , make_ord_flag defGhcFlag "ddump-cmm-opt"
        (setDumpFlag Opt_D_dump_opt_cmm)
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
        (NoArg $ setObjBackend LLVM >> setDumpFlag' Opt_D_dump_llvm)
  , make_ord_flag defGhcFlag "ddump-c-backend"
        (NoArg $ setDumpFlag' Opt_D_dump_c_backend)
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
  , make_ord_flag defGhcFlag "ddump-verbose-inlinings"
        (setDumpFlag Opt_D_dump_verbose_inlinings)
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
  , make_ord_flag defGhcFlag "ddump-stg-from-core"
        (setDumpFlag Opt_D_dump_stg_from_core)
  , make_ord_flag defGhcFlag "ddump-stg-unarised"
        (setDumpFlag Opt_D_dump_stg_unarised)
  , make_ord_flag defGhcFlag "ddump-stg-final"
        (setDumpFlag Opt_D_dump_stg_final)
  , make_dep_flag defGhcFlag "ddump-stg"
        (setDumpFlag Opt_D_dump_stg_from_core)
        "Use `-ddump-stg-from-core` or `-ddump-stg-final` instead"
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
  , make_ord_flag defGhcFlag "ddump-splices"
        (setDumpFlag Opt_D_dump_splices)
  , make_ord_flag defGhcFlag "dth-dec-file"
        (setDumpFlag Opt_D_th_dec_file)

  , make_ord_flag defGhcFlag "ddump-rn-stats"
        (setDumpFlag Opt_D_dump_rn_stats)
  , make_ord_flag defGhcFlag "ddump-opt-cmm" --old alias for cmm-opt
        (setDumpFlag Opt_D_dump_opt_cmm)
  , make_ord_flag defGhcFlag "ddump-simpl-stats"
        (setDumpFlag Opt_D_dump_simpl_stats)
  , make_ord_flag defGhcFlag "ddump-bcos"
        (setDumpFlag Opt_D_dump_BCOs)
  , make_ord_flag defGhcFlag "dsource-stats"
        (setDumpFlag Opt_D_source_stats)
  , make_ord_flag defGhcFlag "dverbose-core2core"
        (NoArg $ setVerbosity (Just 2) >> setDumpFlag' Opt_D_verbose_core2core)
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
  , make_ord_flag defGhcFlag "dlinear-core-lint"
        (NoArg (setGeneralFlag Opt_DoLinearCoreLinting))
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
        (setDumpFlag Opt_D_faststring_stats)
  , make_ord_flag defGhcFlag "dno-llvm-mangler"
        (NoArg (setGeneralFlag Opt_NoLlvmMangler)) -- hidden flag
  , make_ord_flag defGhcFlag "dno-typeable-binds"
        (NoArg (setGeneralFlag Opt_NoTypeableBinds))
  , make_ord_flag defGhcFlag "ddump-debug"
        (setDumpFlag Opt_D_dump_debug)
  , make_ord_flag defGhcFlag "ddump-json"
        (setDumpFlag Opt_D_dump_json )
  , make_ord_flag defGhcFlag "dppr-debug"
        (setDumpFlag Opt_D_ppr_debug)
  , make_ord_flag defGhcFlag "ddebug-output"
        (noArg (flip dopt_unset Opt_D_no_debug_output))
  , make_ord_flag defGhcFlag "dno-debug-output"
        (setDumpFlag Opt_D_no_debug_output)

  , make_ord_flag defGhcFlag "ddump-faststrings"
        (setDumpFlag Opt_D_dump_faststrings)

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
  , make_ord_flag defFlag "fdmd-unbox-width"
      (intSuffix (\n d -> d { dmdUnboxWidth = n }))
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
      (sepArg (\s d -> d { unfoldingOpts = updateReportPrefix (Just s) (unfoldingOpts d)}))
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
      (intSuffix (\n d -> d { liftLamsNonRecArgs = Just n }))
  , make_ord_flag defFlag "fstg-lift-lams-non-rec-args-any"
      (noArg (\d -> d { liftLamsNonRecArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-known"
      (noArg (\d -> d { liftLamsKnown = True }))
  , make_ord_flag defFlag "fno-stg-lift-lams-known"
      (noArg (\d -> d { liftLamsKnown = False }))
  , make_ord_flag defFlag "fproc-alignment"
      (intSuffix (\n d -> d { cmmProcAlignment = Just n }))
  , make_ord_flag defFlag "fblock-layout-weights"
        (HasArg (\s ->
            upd (\d -> d { cfgWeights =
                parseWeights s (cfgWeights d)})))
  , make_ord_flag defFlag "fhistory-size"
      (intSuffix (\n d -> d { historySize = n }))

  , make_ord_flag defFlag "funfolding-creation-threshold"
      (intSuffix   (\n d -> d { unfoldingOpts = updateCreationThreshold n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-use-threshold"
      (intSuffix   (\n d -> d { unfoldingOpts = updateUseThreshold n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-fun-discount"
      (intSuffix   (\n d -> d { unfoldingOpts = updateFunAppDiscount n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-dict-discount"
      (intSuffix   (\n d -> d { unfoldingOpts = updateDictDiscount n (unfoldingOpts d)}))

  , make_ord_flag defFlag "funfolding-case-threshold"
      (intSuffix   (\n d -> d { unfoldingOpts = updateCaseThreshold n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-case-scaling"
      (intSuffix   (\n d -> d { unfoldingOpts = updateCaseScaling n (unfoldingOpts d)}))

  , make_dep_flag defFlag "funfolding-keeness-factor"
      (floatSuffix (\_ d -> d))
      "-funfolding-keeness-factor is no longer respected as of GHC 9.0"

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
      (wordSuffix (\n d -> d { initialUnique = n }))
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

        -- Caller-CC
  , make_ord_flag defGhcFlag "fprof-callers"
         (HasArg setCallerCcFilters)
  , make_ord_flag defGhcFlag "fdistinct-constructor-tables"
      (NoArg (setGeneralFlag Opt_DistinctConstructorTables))
  , make_ord_flag defGhcFlag "finfo-table-map"
      (NoArg (setGeneralFlag Opt_InfoTableMap))
        ------ Compiler flags -----------------------------------------------

  , make_ord_flag defGhcFlag "fasm"             (NoArg (setObjBackend NCG))
  , make_ord_flag defGhcFlag "fvia-c"           (NoArg
         (deprecate $ "The -fvia-c flag does nothing; " ++
                      "it will be removed in a future GHC release"))
  , make_ord_flag defGhcFlag "fvia-C"           (NoArg
         (deprecate $ "The -fvia-C flag does nothing; " ++
                      "it will be removed in a future GHC release"))
  , make_ord_flag defGhcFlag "fllvm"            (NoArg (setObjBackend LLVM))

  , make_ord_flag defFlag "fno-code"         (NoArg ((upd $ \d ->
                  d { ghcLink=NoLink }) >> setBackend NoBackend))
  , make_ord_flag defFlag "fbyte-code"
      (noArgM $ \dflags -> do
        setBackend Interpreter
        pure $ gopt_set dflags Opt_ByteCode)
  , make_ord_flag defFlag "fobject-code"     $ NoArg $ do
      dflags <- liftEwM getCmdLineState
      setBackend $ platformDefaultBackend (targetPlatform dflags)

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
  , make_ord_flag defGhcFlag "fPIE"          (NoArg (setGeneralFlag Opt_PIE))
  , make_ord_flag defGhcFlag "fno-PIE"       (NoArg (unSetGeneralFlag Opt_PIE))

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

-- | This is where we handle unrecognised warning flags. We only issue a warning
-- if -Wunrecognised-warning-flags is set. See #11429 for context.
unrecognisedWarning :: String -> Flag (CmdLineP DynFlags)
unrecognisedWarning prefix = defHiddenFlag prefix (Prefix action)
  where
    action :: String -> EwM (CmdLineP DynFlags) ()
    action flag = do
      f <- wopt Opt_WarnUnrecognisedWarningFlags <$> liftEwM getCmdLineState
      when f $ addFlagWarn (WarningWithFlag Opt_WarnUnrecognisedWarningFlags) $
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
  , make_ord_flag defGhcFlag "package-name"       (HasArg $ \name ->
                                      upd (setUnitId name))
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

-- | Define a warning flag.
warnSpec :: WarningFlag -> [(Deprecation, FlagSpec WarningFlag)]
warnSpec flag = warnSpec' flag nop

-- | Define a warning flag with an effect.
warnSpec' :: WarningFlag -> (TurnOnFlag -> DynP ())
          -> [(Deprecation, FlagSpec WarningFlag)]
warnSpec' flag act = [ (NotDeprecated, FlagSpec name flag act AllModes)
                     | name <- NE.toList (warnFlagNames flag)
                     ]

-- | Define a new deprecated flag with an effect.
depFlagSpecOp :: String -> flag -> (TurnOnFlag -> DynP ()) -> String
            -> (Deprecation, FlagSpec flag)
depFlagSpecOp name flag act dep =
    (Deprecated, snd (flagSpec' name flag (\f -> act f >> deprecate dep)))

-- | Define a new deprecated flag.
depFlagSpec :: String -> flag -> String
            -> (Deprecation, FlagSpec flag)
depFlagSpec name flag dep = depFlagSpecOp name flag nop dep

-- | Define a deprecated warning flag.
depWarnSpec :: WarningFlag -> String
            -> [(Deprecation, FlagSpec WarningFlag)]
depWarnSpec flag dep = [ depFlagSpecOp name flag nop dep
                       | name <- NE.toList (warnFlagNames flag)
                       ]

-- | Define a deprecated warning name substituted by another.
subWarnSpec :: String -> WarningFlag -> String
            -> [(Deprecation, FlagSpec WarningFlag)]
subWarnSpec oldname flag dep = [ depFlagSpecOp oldname flag nop dep ]


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

-- here to avoid module cycle with GHC.Driver.CmdLine
deprecate :: Monad m => String -> EwM m ()
deprecate s = do
    arg <- getArg
    addFlagWarn (WarningWithFlag Opt_WarnDeprecatedFlags) (arg ++ " is deprecated: " ++ s)

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
flagSpecOf = flip Map.lookup wWarningFlagMap

wWarningFlagMap :: Map.Map WarningFlag (FlagSpec WarningFlag)
wWarningFlagMap = Map.fromListWith (\_ x -> x) $ map (flagSpecFlag &&& id) wWarningFlags

-- | These @-W\<blah\>@ flags can all be reversed with @-Wno-\<blah\>@
wWarningFlags :: [FlagSpec WarningFlag]
wWarningFlags = map snd (sortBy (comparing fst) wWarningFlagsDeps)

wWarningFlagsDeps :: [(Deprecation, FlagSpec WarningFlag)]
wWarningFlagsDeps = mconcat [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- Please keep the list of flags below sorted alphabetically
  warnSpec    Opt_WarnAlternativeLayoutRuleTransitional,
  warnSpec    Opt_WarnAmbiguousFields,
  depWarnSpec Opt_WarnAutoOrphans
              "it has no effect",
  warnSpec    Opt_WarnCPPUndef,
  warnSpec    Opt_WarnUnbangedStrictPatterns,
  warnSpec    Opt_WarnDeferredTypeErrors,
  warnSpec    Opt_WarnDeferredOutOfScopeVariables,
  warnSpec    Opt_WarnWarningsDeprecations,
  warnSpec    Opt_WarnDeprecatedFlags,
  warnSpec    Opt_WarnDerivingDefaults,
  warnSpec    Opt_WarnDerivingTypeable,
  warnSpec    Opt_WarnDodgyExports,
  warnSpec    Opt_WarnDodgyForeignImports,
  warnSpec    Opt_WarnDodgyImports,
  warnSpec    Opt_WarnEmptyEnumerations,
  subWarnSpec "duplicate-constraints"
              Opt_WarnDuplicateConstraints
              "it is subsumed by -Wredundant-constraints",
  warnSpec    Opt_WarnRedundantConstraints,
  warnSpec    Opt_WarnDuplicateExports,
  depWarnSpec Opt_WarnHiShadows
              "it is not used, and was never implemented",
  warnSpec    Opt_WarnInaccessibleCode,
  warnSpec    Opt_WarnImplicitPrelude,
  depWarnSpec Opt_WarnImplicitKindVars
              "it is now an error",
  warnSpec    Opt_WarnIncompletePatterns,
  warnSpec    Opt_WarnIncompletePatternsRecUpd,
  warnSpec    Opt_WarnIncompleteUniPatterns,
  warnSpec    Opt_WarnInlineRuleShadowing,
  warnSpec    Opt_WarnIdentities,
  warnSpec    Opt_WarnMissingFields,
  warnSpec    Opt_WarnMissingImportList,
  warnSpec    Opt_WarnMissingExportList,
  subWarnSpec "missing-local-sigs"
              Opt_WarnMissingLocalSignatures
              "it is replaced by -Wmissing-local-signatures",
  warnSpec    Opt_WarnMissingLocalSignatures,
  warnSpec    Opt_WarnMissingMethods,
  depWarnSpec Opt_WarnMissingMonadFailInstances
              "fail is no longer a method of Monad",
  warnSpec    Opt_WarnSemigroup,
  warnSpec    Opt_WarnMissingSignatures,
  warnSpec    Opt_WarnMissingKindSignatures,
  subWarnSpec "missing-exported-sigs"
              Opt_WarnMissingExportedSignatures
              "it is replaced by -Wmissing-exported-signatures",
  warnSpec    Opt_WarnMissingExportedSignatures,
  warnSpec    Opt_WarnMonomorphism,
  warnSpec    Opt_WarnNameShadowing,
  warnSpec    Opt_WarnNonCanonicalMonadInstances,
  depWarnSpec Opt_WarnNonCanonicalMonadFailInstances
              "fail is no longer a method of Monad",
  warnSpec    Opt_WarnNonCanonicalMonoidInstances,
  warnSpec    Opt_WarnOrphans,
  warnSpec    Opt_WarnOverflowedLiterals,
  warnSpec    Opt_WarnOverlappingPatterns,
  warnSpec    Opt_WarnMissedSpecs,
  warnSpec    Opt_WarnAllMissedSpecs,
  warnSpec'   Opt_WarnSafe setWarnSafe,
  warnSpec    Opt_WarnTrustworthySafe,
  warnSpec    Opt_WarnInferredSafeImports,
  warnSpec    Opt_WarnMissingSafeHaskellMode,
  warnSpec    Opt_WarnTabs,
  warnSpec    Opt_WarnTypeDefaults,
  warnSpec    Opt_WarnTypedHoles,
  warnSpec    Opt_WarnPartialTypeSignatures,
  warnSpec    Opt_WarnUnrecognisedPragmas,
  warnSpec'   Opt_WarnUnsafe setWarnUnsafe,
  warnSpec    Opt_WarnUnsupportedCallingConventions,
  warnSpec    Opt_WarnUnsupportedLlvmVersion,
  warnSpec    Opt_WarnMissedExtraSharedLib,
  warnSpec    Opt_WarnUntickedPromotedConstructors,
  warnSpec    Opt_WarnUnusedDoBind,
  warnSpec    Opt_WarnUnusedForalls,
  warnSpec    Opt_WarnUnusedImports,
  warnSpec    Opt_WarnUnusedLocalBinds,
  warnSpec    Opt_WarnUnusedMatches,
  warnSpec    Opt_WarnUnusedPatternBinds,
  warnSpec    Opt_WarnUnusedTopBinds,
  warnSpec    Opt_WarnUnusedTypePatterns,
  warnSpec    Opt_WarnUnusedRecordWildcards,
  warnSpec    Opt_WarnRedundantBangPatterns,
  warnSpec    Opt_WarnRedundantRecordWildcards,
  warnSpec    Opt_WarnRedundantStrictnessFlags,
  warnSpec    Opt_WarnWrongDoBind,
  warnSpec    Opt_WarnMissingPatternSynonymSignatures,
  warnSpec    Opt_WarnMissingDerivingStrategies,
  warnSpec    Opt_WarnSimplifiableClassConstraints,
  warnSpec    Opt_WarnMissingHomeModules,
  warnSpec    Opt_WarnUnrecognisedWarningFlags,
  warnSpec    Opt_WarnStarBinder,
  warnSpec    Opt_WarnStarIsType,
  depWarnSpec Opt_WarnSpaceAfterBang
              "bang patterns can no longer be written with a space",
  warnSpec    Opt_WarnPartialFields,
  warnSpec    Opt_WarnPrepositiveQualifiedModule,
  warnSpec    Opt_WarnUnusedPackages,
  warnSpec    Opt_WarnCompatUnqualifiedImports,
  warnSpec    Opt_WarnInvalidHaddock,
  warnSpec    Opt_WarnOperatorWhitespaceExtConflict,
  warnSpec    Opt_WarnOperatorWhitespace,
  warnSpec    Opt_WarnImplicitLift,
  warnSpec    Opt_WarnMissingExportedPatternSynonymSignatures,
  warnSpec    Opt_WarnForallIdentifier,
  warnSpec    Opt_WarnUnicodeBidirectionalFormatCharacters,
  warnSpec    Opt_WarnGADTMonoLocalBinds
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
  flagSpec "suppress-var-kinds"         Opt_SuppressVarKinds,
  flagSpec "suppress-core-sizes"        Opt_SuppressCoreSizes
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
  flagSpec "cmm-static-pred"                  Opt_CmmStaticPred,
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
  depFlagSpec "dmd-tx-dict-sel"
      Opt_DmdTxDictSel "effect is now unconditionally enabled",
  flagSpec "do-eta-reduction"                 Opt_DoEtaReduction,
  flagSpec "do-lambda-eta-expansion"          Opt_DoLambdaEtaExpansion,
  flagSpec "eager-blackholing"                Opt_EagerBlackHoling,
  flagSpec "embed-manifest"                   Opt_EmbedManifest,
  flagSpec "enable-rewrite-rules"             Opt_EnableRewriteRules,
  flagSpec "enable-th-splice-warnings"        Opt_EnableThSpliceWarnings,
  flagSpec "error-spans"                      Opt_ErrorSpans,
  flagSpec "excess-precision"                 Opt_ExcessPrecision,
  flagSpec "expose-all-unfoldings"            Opt_ExposeAllUnfoldings,
  flagSpec "expose-internal-symbols"          Opt_ExposeInternalSymbols,
  flagSpec "external-dynamic-refs"            Opt_ExternalDynamicRefs,
  flagSpec "external-interpreter"             Opt_ExternalInterpreter,
  flagSpec "family-application-cache"         Opt_FamAppCache,
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
  flagSpec "inline-generics"                  Opt_InlineGenerics,
  flagSpec "inline-generics-aggressively"     Opt_InlineGenericsAggressively,
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
  flagSpec "core-constant-folding"            Opt_CoreConstantFolding,
  flagSpec "fast-pap-calls"                   Opt_FastPAPCalls,
  flagSpec "cmm-control-flow"                 Opt_CmmControlFlow,
  flagSpec "show-warning-groups"              Opt_ShowWarnGroups,
  flagSpec "hide-source-paths"                Opt_HideSourcePaths,
  flagSpec "show-loaded-modules"              Opt_ShowLoadedModules,
  flagSpec "whole-archive-hs-libs"            Opt_WholeArchiveHsLibs,
  flagSpec "keep-cafs"                        Opt_KeepCAFs,
  flagSpec "link-rts"                         Opt_LinkRts
  ]
  ++ fHoleFlags

-- | These @-f\<blah\>@ flags have to do with the typed-hole error message or
-- the valid hole fits in that message. See Note [Valid hole fits include ...]
-- in the "GHC.Tc.Errors.Hole" module. These flags can all be reversed with
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

supportedExtensions :: ArchOS -> [String]
supportedExtensions (ArchOS _ os) = concatMap toFlagSpecNamePair xFlags
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
        isAIX = os == OSAIX
        noName = "No" ++ name
        name = flagSpecName flg

supportedLanguagesAndExtensions :: ArchOS -> [String]
supportedLanguagesAndExtensions arch_os =
    supportedLanguages ++ supportedLanguageOverlays ++ supportedExtensions arch_os

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
languageFlagsDeps :: [(Deprecation, FlagSpec Language)]
languageFlagsDeps = [
  flagSpec "Haskell98"   Haskell98,
  flagSpec "Haskell2010" Haskell2010,
  flagSpec "GHC2021"     GHC2021
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
  flagSpec' "DerivingVia"                     LangExt.DerivingVia
                                              setDeriveVia,
  flagSpec "DisambiguateRecordFields"         LangExt.DisambiguateRecordFields,
  flagSpec "DoAndIfThenElse"                  LangExt.DoAndIfThenElse,
  flagSpec "BlockArguments"                   LangExt.BlockArguments,
  depFlagSpec' "DoRec"                        LangExt.RecursiveDo
    (deprecatedForExtension "RecursiveDo"),
  flagSpec "DuplicateRecordFields"            LangExt.DuplicateRecordFields,
  flagSpec "FieldSelectors"                   LangExt.FieldSelectors,
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
  flagSpec "LexicalNegation"                  LangExt.LexicalNegation,
  flagSpec "LiberalTypeSynonyms"              LangExt.LiberalTypeSynonyms,
  flagSpec "LinearTypes"                      LangExt.LinearTypes,
  flagSpec "MagicHash"                        LangExt.MagicHash,
  flagSpec "MonadComprehensions"              LangExt.MonadComprehensions,
  flagSpec "MonoLocalBinds"                   LangExt.MonoLocalBinds,
  flagSpec "MonomorphismRestriction"          LangExt.MonomorphismRestriction,
  flagSpec "MultiParamTypeClasses"            LangExt.MultiParamTypeClasses,
  flagSpec "MultiWayIf"                       LangExt.MultiWayIf,
  flagSpec "NumericUnderscores"               LangExt.NumericUnderscores,
  flagSpec "NPlusKPatterns"                   LangExt.NPlusKPatterns,
  flagSpec "NamedFieldPuns"                   LangExt.NamedFieldPuns,
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
  flagSpec "QualifiedDo"                      LangExt.QualifiedDo,
  flagSpec "Rank2Types"                       LangExt.RankNTypes,
  flagSpec "RankNTypes"                       LangExt.RankNTypes,
  flagSpec "RebindableSyntax"                 LangExt.RebindableSyntax,
  flagSpec "OverloadedRecordDot"              LangExt.OverloadedRecordDot,
  flagSpec "OverloadedRecordUpdate"           LangExt.OverloadedRecordUpdate,
  depFlagSpec' "RecordPuns"                   LangExt.NamedFieldPuns
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
  flagSpec "UnliftedDatatypes"                LangExt.UnliftedDatatypes,
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
      Opt_FamAppCache,
      Opt_GenManifest,
      Opt_GhciHistory,
      Opt_GhciSandbox,
      Opt_HelpfulErrors,
      Opt_KeepHiFiles,
      Opt_KeepOFiles,
      Opt_OmitYields,
      Opt_PrintBindContents,
      Opt_ProfCountEntries,
      Opt_SharedImplib,
      Opt_SimplPreInlining,
      Opt_VersionMacros,
      Opt_RPath
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    ++ default_PIC platform

    ++ validHoleFitDefaults

    where platform = sTargetPlatform settings

-- | These are the default settings for the display and sorting of valid hole
--  fits in typed-hole error messages. See Note [Valid hole fits include ...]
 -- in the "GHC.Tc.Errors.Hole" module.
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
    -- Darwin always requires PIC.  Especially on more recent macOS releases
    -- there will be a 4GB __ZEROPAGE that prevents us from using 32bit addresses
    -- while we could work around this on x86_64 (like WINE does), we won't be
    -- able on aarch64, where this is enforced.
    (OSDarwin,  ArchX86_64)  -> [Opt_PIC]
    -- For AArch64, we need to always have PIC enabled.  The relocation model
    -- on AArch64 does not permit arbitrary relocations.  Under ASLR, we can't
    -- control much how far apart symbols are in memory for our in-memory static
    -- linker;  and thus need to ensure we get sufficiently capable relocations.
    -- This requires PIC on AArch64, and ExternalDynamicRefs on Linux as on top
    -- of that.  Subsequently we expect all code on aarch64/linux (and macOS) to
    -- be built with -fPIC.
    (OSDarwin,  ArchAArch64) -> [Opt_PIC]
    (OSLinux,   ArchAArch64) -> [Opt_PIC, Opt_ExternalDynamicRefs]
    (OSLinux,   ArchARM {})  -> [Opt_PIC, Opt_ExternalDynamicRefs]
    (OSOpenBSD, ArchX86_64)  -> [Opt_PIC] -- Due to PIE support in
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
                ,(Opt_DoLinearCoreLinting, turnOn, Opt_DoCoreLinting)
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

    -- The extensions needed to declare an H98 unlifted data type
    , (LangExt.UnliftedDatatypes, turnOn, LangExt.DataKinds)
    , (LangExt.UnliftedDatatypes, turnOn, LangExt.StandaloneKindSignatures)
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
    , ([0,1,2], Opt_LlvmTBAA)
    , ([2], Opt_DictsStrict)

    , ([0],     Opt_IgnoreInterfacePragmas)
    , ([0],     Opt_OmitInterfacePragmas)

    , ([1,2],   Opt_CoreConstantFolding)

    , ([1,2],   Opt_CallArity)
    , ([1,2],   Opt_Exitification)
    , ([1,2],   Opt_CaseMerge)
    , ([1,2],   Opt_CaseFolding)
    , ([1,2],   Opt_CmmElimCommonBlocks)
    , ([2],     Opt_AsmShortcutting)
    , ([1,2],   Opt_CmmSink)
    , ([1,2],   Opt_CmmStaticPred)
    , ([1,2],   Opt_CSE)
    , ([1,2],   Opt_StgCSE)
    , ([2],     Opt_StgLiftLams)
    , ([1,2],   Opt_CmmControlFlow)

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
    , ([1,2],   Opt_InlineGenerics)
    , ([1,2],   Opt_Strictness)
    , ([1,2],   Opt_UnboxSmallStrictFields)
    , ([1,2],   Opt_CprAnal)
    , ([1,2],   Opt_WorkerWrapper)
    , ([1,2],   Opt_SolveConstantDicts)
    , ([1,2],   Opt_NumConstantFolding)

    , ([2],     Opt_LiberateCase)
    , ([2],     Opt_SpecConstr)
    , ([2],     Opt_FastPAPCalls)
--  , ([2],     Opt_RegsGraph)
--   RegsGraph suffers performance regression. See #7679
--  , ([2],     Opt_StaticArgumentTransformation)
--   Static Argument Transformation needs investigation. See #9374
    ]


enableUnusedBinds :: DynP ()
enableUnusedBinds = mapM_ setWarningFlag unusedBindsFlags

disableUnusedBinds :: DynP ()
disableUnusedBinds = mapM_ unSetWarningFlag unusedBindsFlags

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

setDeriveVia :: TurnOnFlag -> DynP ()
setDeriveVia True  = getCurLoc >>= \l -> upd (\d -> d { deriveViaOnLoc = l })
setDeriveVia False = return ()

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

wordSuffix :: (Word -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
wordSuffix fn = WordSuffix (\n -> upd (fn n))

floatSuffix :: (Float -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
floatSuffix fn = FloatSuffix (\n -> upd (fn n))

optIntSuffixM :: (Maybe Int -> DynFlags -> DynP DynFlags)
              -> OptKind (CmdLineP DynFlags)
optIntSuffixM fn = OptIntSuffix (\mi -> updM (fn mi))

setDumpFlag :: DumpFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

--------------------------
addWayDynP :: Way -> DynP ()
addWayDynP = upd . addWay'

addWay' :: Way -> DynFlags -> DynFlags
addWay' w dflags0 =
   let platform = targetPlatform dflags0
       dflags1 = dflags0 { targetWays_ = addWay w (targetWays_ dflags0) }
       dflags2 = foldr setGeneralFlag' dflags1
                       (wayGeneralFlags platform w)
       dflags3 = foldr unSetGeneralFlag' dflags2
                       (wayUnsetGeneralFlags platform w)
   in dflags3

removeWayDyn :: DynP ()
removeWayDyn = upd (\dfs -> dfs { targetWays_ = removeWay WayDyn (targetWays_ dfs) })

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


setVerbosity :: Maybe Int -> DynP ()
setVerbosity mb_n = upd (\dfs -> dfs{ verbosity = mb_n `orElse` 3 })

setDebugLevel :: Maybe Int -> DynP ()
setDebugLevel mb_n =
  upd (\dfs -> exposeSyms $ dfs{ debugLevel = n })
  where
    n = mb_n `orElse` 2
    exposeSyms
      | n > 2     = setGeneralFlag' Opt_ExposeInternalSymbols
      | otherwise = id

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
    parsePackageFlag "-package-id" parseUnitArg p : packageFlags s })
exposePluginPackage p =
  upd (\s -> s{ pluginPackageFlags =
    parsePackageFlag "-plugin-package" parsePackageArg p : pluginPackageFlags s })
exposePluginPackageId p =
  upd (\s -> s{ pluginPackageFlags =
    parsePackageFlag "-plugin-package-id" parseUnitArg p : pluginPackageFlags s })
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

parseUnitArg :: ReadP PackageArg
parseUnitArg =
    fmap UnitIdArg parseUnit

setUnitId :: String -> DynFlags -> DynFlags
setUnitId p d = d { homeUnitId_ = stringToUnitId p }

-- If we're linking a binary, then only backends that produce object
-- code are allowed (requests for other target types are ignored).
setBackend :: Backend -> DynP ()
setBackend l = upd $ \ dfs ->
  if ghcLink dfs /= LinkBinary || backendProducesObject l
  then dfs{ backend = l }
  else dfs

-- Changes the target only if we're compiling object code.  This is
-- used by -fasm and -fllvm, which switch from one to the other, but
-- not from bytecode to object-code.  The idea is that -fasm/-fllvm
-- can be safely used in an OPTIONS_GHC pragma.
setObjBackend :: Backend -> DynP ()
setObjBackend l = updM set
  where
   set dflags
     | backendProducesObject (backend dflags)
       = return $ dflags { backend = l }
     | otherwise = return dflags

setOptLevel :: Int -> DynFlags -> DynP DynFlags
setOptLevel n dflags = return (updOptLevel n dflags)

setCallerCcFilters :: String -> DynP ()
setCallerCcFilters arg =
  case parseCallerCcFilter arg of
    Right filt -> upd $ \d -> d { callerCcFilters = filt : callerCcFilters d }
    Left err -> addErr err

setMainIs :: String -> DynP ()
setMainIs arg
  | not (null main_fn) && isLower (head main_fn)
     -- The arg looked like "Foo.Bar.baz"
  = upd $ \d -> d { mainFunIs = Just main_fn,
                    mainModuleNameIs = mkModuleName main_mod }

  | isUpper (head arg)  -- The arg looked like "Foo" or "Foo.Bar"
  = upd $ \d -> d { mainModuleNameIs = mkModuleName arg }

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
setTmpDir dir d = d { tmpDir = TempDir (normalise dir) }
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
picCCOpts dflags =
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
       | gopt Opt_PIC dflags || ways dflags `hasWay` WayDyn ->
          ["-fPIC", "-U__PIC__", "-D__PIC__"]
      -- gcc may be configured to have PIC on by default, let's be
      -- explicit here, see #15847
       | otherwise -> ["-fno-PIC"]

pieCCLDOpts :: DynFlags -> [String]
pieCCLDOpts dflags
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
       ("Have native code generator",  showBool $ platformNcgSupported (targetPlatform dflags)),
       ("Target default backend",      show $ platformDefaultBackend (targetPlatform dflags)),
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
       -- "Uses unit IDs" over it. We still say yes even if @-this-package-key@
       -- flag has been removed, otherwise it breaks Cabal...
       ("Uses package keys",           "YES"),
       -- Whether or not we support the @-this-unit-id@ flag
       ("Uses unit IDs",               "YES"),
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
    platform  = targetPlatform dflags
    isWindows = platformOS platform == OSMinGW32
    useInplaceMinGW = toolSettings_useInplaceMinGW $ toolSettings dflags
    expandDirectories :: FilePath -> Maybe FilePath -> String -> String
    expandDirectories topd mtoold = expandToolDir useInplaceMinGW mtoold . expandTopDir topd


wordAlignment :: Platform -> Alignment
wordAlignment platform = alignmentOf (platformWordSizeInBytes platform)

-- | Get target profile
targetProfile :: DynFlags -> Profile
targetProfile dflags = Profile (targetPlatform dflags) (ways dflags)

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
 -- Disable -dynamic-too if we are are compiling with -dynamic already, otherwise
 -- you get two dynamic object files (.o and .dyn_o). (#20436)
 | ways dflags `hasWay` WayDyn && gopt Opt_BuildDynamicToo dflags
    = let dflags' = gopt_unset dflags Opt_BuildDynamicToo
          warn = "-dynamic-too is ignored when using -dynamic"
      in loop dflags' warn

   -- Via-C backend only supports unregisterised ABI. Switch to a backend
   -- supporting it if possible.
 | backend dflags == ViaC &&
   not (platformUnregisterised (targetPlatform dflags))
    = case platformDefaultBackend (targetPlatform dflags) of
         NCG ->  let dflags' = dflags { backend = NCG }
                     warn = "Target platform doesn't use unregisterised ABI, so using native code generator rather than compiling via C"
                 in loop dflags' warn
         LLVM -> let dflags' = dflags { backend = LLVM }
                     warn = "Target platform doesn't use unregisterised ABI, so using LLVM rather than compiling via C"
                 in loop dflags' warn
         _    -> pgmError "Compiling via C only supports unregisterised ABI but target platform doesn't use it."

 | gopt Opt_Hpc dflags && backend dflags == Interpreter
    = let dflags' = gopt_unset dflags Opt_Hpc
          warn = "Hpc can't be used with byte-code interpreter. Ignoring -fhpc."
      in loop dflags' warn

 | backend dflags `elem` [NCG, LLVM] &&
   platformUnregisterised (targetPlatform dflags)
    = loop (dflags { backend = ViaC })
           "Target platform uses unregisterised ABI, so compiling via C"

 | backend dflags == NCG &&
   not (platformNcgSupported $ targetPlatform dflags)
      = let dflags' = dflags { backend = LLVM }
            warn = "Native code generator doesn't support target platform, so using LLVM"
        in loop dflags' warn

 | not (osElfTarget os) && gopt Opt_PIE dflags
    = loop (gopt_unset dflags Opt_PIE)
           "Position-independent only supported on ELF platforms"
 | os == OSDarwin &&
   arch == ArchX86_64 &&
   not (gopt Opt_PIC dflags)
    = loop (gopt_set dflags Opt_PIC)
           "Enabling -fPIC as it is always on for this platform"

 | backend dflags == Interpreter
 , let (dflags', changed) = updOptLevelChanged 0 dflags
 , changed
    = loop dflags' "Optimization flags conflict with --interactive; optimization flags ignored."

 | LinkInMemory <- ghcLink dflags
 , not (gopt Opt_ExternalInterpreter dflags)
 , hostIsProfiled
 , backendProducesObject (backend dflags)
 , ways dflags `hasNotWay` WayProf
    = loop dflags{targetWays_ = addWay WayProf (targetWays_ dflags)}
         "Enabling -prof, because -fobject-code is enabled and GHCi is profiled"

 | otherwise = (dflags, [])
    where loc = mkGeneralSrcSpan (fsLit "when making flags consistent")
          loop updated_dflags warning
              = case makeDynFlagsConsistent updated_dflags of
                (dflags', ws) -> (dflags', L loc warning : ws)
          platform = targetPlatform dflags
          arch = platformArch platform
          os   = platformOS   platform


setUnsafeGlobalDynFlags :: DynFlags -> IO ()
setUnsafeGlobalDynFlags dflags = do
   writeIORef v_unsafeHasPprDebug (hasPprDebug dflags)
   writeIORef v_unsafeHasNoDebugOutput (hasNoDebugOutput dflags)
   writeIORef v_unsafeHasNoStateHack (hasNoStateHack dflags)


-- -----------------------------------------------------------------------------
-- SSE and AVX

-- TODO: Instead of using a separate predicate (i.e. isSse2Enabled) to
-- check if SSE is enabled, we might have x86-64 imply the -msse2
-- flag.

isSseEnabled :: Platform -> Bool
isSseEnabled platform = case platformArch platform of
    ArchX86_64 -> True
    ArchX86    -> True
    _          -> False

isSse2Enabled :: Platform -> Bool
isSse2Enabled platform = case platformArch platform of
  -- We assume  SSE1 and SSE2 operations are available on both
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

-- | Indicate if cost-centre profiling is enabled
sccProfilingEnabled :: DynFlags -> Bool
sccProfilingEnabled dflags = profileIsProfiling (targetProfile dflags)

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


-- | Should we use `-XLinker -rpath` when linking or not?
-- See Note [-fno-use-rpaths]
useXLinkerRPath :: DynFlags -> OS -> Bool
useXLinkerRPath _ OSDarwin = False -- See Note [Dynamic linking on macOS]
useXLinkerRPath dflags _ = gopt Opt_RPath dflags

{-
Note [-fno-use-rpaths]
~~~~~~~~~~~~~~~~~~~~~~

First read, Note [Dynamic linking on macOS] to understand why on darwin we never
use `-XLinker -rpath`.

The specification of `Opt_RPath` is as follows:

The default case `-fuse-rpaths`:
* On darwin, never use `-Xlinker -rpath -Xlinker`, always inject the rpath
  afterwards, see `runInjectRPaths`. There is no way to use `-Xlinker` on darwin
  as things stand but it wasn't documented in the user guide before this patch how
  `-fuse-rpaths` should behave and the fact it was always disabled on darwin.
* Otherwise, use `-Xlinker -rpath -Xlinker` to set the rpath of the executable,
  this is the normal way you should set the rpath.

The case of `-fno-use-rpaths`
* Never inject anything into the rpath.

When this was first implemented, `Opt_RPath` was disabled on darwin, but
the rpath was still always augmented by `runInjectRPaths`, and there was no way to
stop this. This was problematic because you couldn't build an executable in CI
with a clean rpath.

-}

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


-- | Initialize the pretty-printing options
initSDocContext :: DynFlags -> PprStyle -> SDocContext
initSDocContext dflags style = SDC
  { sdocStyle                       = style
  , sdocColScheme                   = colScheme dflags
  , sdocLastColour                  = Col.colReset
  , sdocShouldUseColor              = overrideWith (canUseColor dflags) (useColor dflags)
  , sdocDefaultDepth                = pprUserLength dflags
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
  , sdocLinearTypes                 = xopt LangExt.LinearTypes dflags
  , sdocPrintTypeAbbreviations      = True
  , sdocUnitIdForUser               = ftext
  }

-- | Initialize the pretty-printing options using the default user style
initDefaultSDocContext :: DynFlags -> SDocContext
initDefaultSDocContext dflags = initSDocContext dflags defaultUserStyle

outputFile :: DynFlags -> Maybe String
outputFile dflags
   | dynamicNow dflags = dynOutputFile_ dflags
   | otherwise         = outputFile_    dflags

objectSuf :: DynFlags -> String
objectSuf dflags
   | dynamicNow dflags = dynObjectSuf_ dflags
   | otherwise         = objectSuf_    dflags

ways :: DynFlags -> Ways
ways dflags
   | dynamicNow dflags = addWay WayDyn (targetWays_ dflags)
   | otherwise         = targetWays_ dflags

-- | Pretty-print the difference between 2 DynFlags.
--
-- For now only their general flags but it could be extended.
-- Useful mostly for debugging.
pprDynFlagsDiff :: DynFlags -> DynFlags -> SDoc
pprDynFlagsDiff d1 d2 =
   let gf_removed  = EnumSet.difference (generalFlags d1) (generalFlags d2)
       gf_added    = EnumSet.difference (generalFlags d2) (generalFlags d1)
       ext_removed = EnumSet.difference (extensionFlags d1) (extensionFlags d2)
       ext_added   = EnumSet.difference (extensionFlags d2) (extensionFlags d1)
   in vcat
      [ text "Added general flags:"
      , text $ show $ EnumSet.toList $ gf_added
      , text "Removed general flags:"
      , text $ show $ EnumSet.toList $ gf_removed
      , text "Added extension flags:"
      , text $ show $ EnumSet.toList $ ext_added
      , text "Removed extension flags:"
      , text $ show $ EnumSet.toList $ ext_removed
      ]

updatePlatformConstants :: DynFlags -> Maybe PlatformConstants -> IO DynFlags
updatePlatformConstants dflags mconstants = do
  let platform1 = (targetPlatform dflags) { platform_constants = mconstants }
  let dflags1   = dflags { targetPlatform = platform1 }
  return dflags1
