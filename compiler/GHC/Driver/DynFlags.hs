{-# LANGUAGE LambdaCase #-}
module GHC.Driver.DynFlags (
        -- * Dynamic flags and associated configuration types
        DumpFlag(..),
        GeneralFlag(..),
        WarningFlag(..), DiagnosticReason(..),
        Language(..),
        FatalMessager, FlushOut(..),
        ProfAuto(..),
        hasPprDebug, hasNoDebugOutput, hasNoStateHack, hasNoOptCoercion,
        dopt, dopt_set, dopt_unset,
        gopt, gopt_set, gopt_unset,
        wopt, wopt_set, wopt_unset,
        wopt_fatal, wopt_set_fatal, wopt_unset_fatal,
        wopt_set_all_custom, wopt_unset_all_custom,
        wopt_set_all_fatal_custom, wopt_unset_all_fatal_custom,
        wopt_set_custom, wopt_unset_custom,
        wopt_set_fatal_custom, wopt_unset_fatal_custom,
        wopt_any_custom,
        xopt, xopt_set, xopt_unset,
        xopt_set_unlessExplSpec,
        xopt_DuplicateRecordFields,
        xopt_FieldSelectors,
        lang_set,
        DynamicTooState(..), dynamicTooState, setDynamicNow,
        OnOff(..),
        DynFlags(..),
        ParMakeCount(..),
        ways,
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
        positionIndependent,
        optimisationFlags,

        targetProfile,

        ReexportedModule(..),

        -- ** Manipulating DynFlags
        defaultDynFlags,                -- Settings -> DynFlags
        initDynFlags,                   -- DynFlags -> IO DynFlags
        defaultFatalMessager,
        defaultFlushOut,
        optLevelFlags,
        languageExtensions,

        TurnOnFlag,
        turnOn,
        turnOff,

        -- ** System tool settings and locations
        programName, projectVersion,
        ghcUsagePath, ghciUsagePath, topDir, toolDir,
        versionedAppDir, versionedFilePath,
        extraGccViaCFlags, globalPackageDatabasePath,

        --
        baseUnitId,


        -- * Include specifications
        IncludeSpecs(..), addGlobalInclude, addQuoteInclude, flattenIncludes,
        addImplicitQuoteInclude,

        -- * SDoc
        initSDocContext, initDefaultSDocContext,
        initPromotionTickContext,

        -- * Platform features
        isSse3Enabled,
        isSse4_1Enabled,
        isSse4_2Enabled,
        isAvxEnabled,
        isAvx2Enabled,
        isAvx512cdEnabled,
        isAvx512erEnabled,
        isAvx512fEnabled,
        isAvx512pfEnabled,
        isFmaEnabled,
        isBmiEnabled,
        isBmi2Enabled
) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways
import GHC.Platform.Profile

import GHC.CmmToAsm.CFG.Weight
import GHC.Core.Unfold
import GHC.Data.Bool
import GHC.Data.EnumSet (EnumSet)
import GHC.Data.Maybe
import GHC.Builtin.Names ( mAIN_NAME )
import GHC.Driver.Backend
import GHC.Driver.Flags
import GHC.Driver.IncludeSpecs
import GHC.Driver.Phases ( Phase(..), phaseInputExt )
import GHC.Driver.Plugins.External
import GHC.Settings
import GHC.Settings.Constants
import GHC.Types.Basic ( IntWithInf, treatZeroAsInf )
import GHC.Types.Error (DiagnosticReason(..))
import GHC.Types.ProfAuto
import GHC.Types.SafeHaskell
import GHC.Types.SrcLoc
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Utils.CliOption
import GHC.SysTools.Terminal ( stderrSupportsAnsiColors )
import GHC.UniqueSubdir (uniqueSubdir)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.TmpFs

import qualified GHC.Types.FieldLabel as FieldLabel
import qualified GHC.Utils.Ppr.Colour as Col
import qualified GHC.Data.EnumSet as EnumSet

import GHC.Core.Opt.CallerCC.Types

import Control.Monad (msum, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Word
import System.IO
import System.IO.Error (catchIOError)
import System.Environment (lookupEnv)
import System.FilePath (normalise, (</>))
import System.Directory
import GHC.Foreign (withCString, peekCString)

import qualified Data.Set as Set

import qualified GHC.LanguageExtensions as LangExt

-- -----------------------------------------------------------------------------
-- DynFlags

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
  unitSettings      :: {-# UNPACK #-} !UnitSettings,

  targetPlatform    :: Platform,       -- Filled in by SysTools
  toolSettings      :: {-# UNPACK #-} !ToolSettings,
  platformMisc      :: {-# UNPACK #-} !PlatformMisc,
  rawSettings       :: [(String, String)],
  tmpDir            :: TempDir,

  llvmOptLevel          :: Int,         -- ^ LLVM optimisation level
  verbosity             :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
  debugLevel            :: Int,         -- ^ How much debug information to produce
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  ruleCheck             :: Maybe String,
  strictnessBefore      :: [Int],       -- ^ Additional demand analysis

  parMakeCount          :: Maybe ParMakeCount,
    -- ^ The number of modules to compile in parallel
    --   If unspecified, compile with a single job.

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
  ifCompression         :: Int,
  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  specConstrRecursive   :: Int,         -- ^ Max number of specialisations for recursive types
                                        --   Not optional; otherwise ForceSpecConstr can diverge.
  binBlobThreshold      :: Maybe Word,  -- ^ Binary literals (e.g. strings) whose size is above
                                        --   this threshold will be dumped in a binary file
                                        --   by the assembler code generator. 0 and Nothing disables
                                        --   this feature. See 'GHC.StgToCmm.Config'.
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
  givensFuel            :: Int,          -- ^ Number of layers of superclass expansion for givens
                                         --   Should be < solverIterations
                                         --   See Note [Expanding Recursive Superclasses and ExpansionFuel]
  wantedsFuel           :: Int,          -- ^ Number of layers of superclass expansion for wanteds
                                         --   Should be < givensFuel
                                         --   See Note [Expanding Recursive Superclasses and ExpansionFuel]
  qcsFuel                :: Int,          -- ^ Number of layers of superclass expansion for quantified constraints
                                         --   Should be < givensFuel
                                         --   See Note [Expanding Recursive Superclasses and ExpansionFuel]
  homeUnitId_             :: UnitId,                 -- ^ Target home unit-id
  homeUnitInstanceOf_     :: Maybe UnitId,           -- ^ Id of the unit to instantiate
  homeUnitInstantiations_ :: [(ModuleName, Module)], -- ^ Module instantiations

  -- Note [Filepaths and Multiple Home Units]
  workingDirectory      :: Maybe FilePath,
  thisPackageName       :: Maybe String, -- ^ What the package is called, use with multiple home units
  hiddenModules         :: Set.Set ModuleName,
  reexportedModules     :: [ReexportedModule],

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
    -- ^ the @-fplugin@ flags given on the command line, in *reverse*
    -- order that they're specified on the command line.
  pluginModNameOpts     :: [(ModuleName,String)],
  frontendPluginOpts    :: [String],
    -- ^ the @-ffrontend-opt@ flags given on the command line, in *reverse*
    -- order that they're specified on the command line.

  externalPluginSpecs   :: [ExternalPluginSpec],
    -- ^ External plugins loaded from shared libraries

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
  customWarningCategories      :: WarningCategorySet, -- See Note [Warning categories]
  fatalCustomWarningCategories :: WarningCategorySet, -- in GHC.Unit.Module.Warnings
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
  maxForcedSpecArgs     :: Int,

  ghciHistSize          :: Int,

  -- wasm ghci browser mode
  ghciBrowserHost                  :: !String,
  ghciBrowserPort                  :: !Int,
  ghciBrowserPuppeteerLaunchOpts   :: !(Maybe String),
  ghciBrowserPlaywrightBrowserType :: !(Maybe String),
  ghciBrowserPlaywrightLaunchOpts  :: !(Maybe String),

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
  useErrorLinks         :: OverridingBool,
  canUseErrorLinks      :: Bool,
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
  fma                   :: Bool, -- ^ Enable FMA instructions.

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
  initialUnique         :: Word64,
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

-----------------------------------------------------------------------------

-- | Used by 'GHC.runGhc' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 let
 -- This is not bulletproof: we test that 'localeEncoding' is Unicode-capable,
 -- but potentially 'hGetEncoding' 'stdout' might be different. Still good enough.
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
        -- if the terminal supports color, we assume it supports links as well
        canUseErrorLinks = stderrSupportsAnsiColors,
        colScheme     = colScheme',
        tmpDir        = TempDir tmp_dir
        }

-- | The normal 'DynFlags'. Note that they are not suitable for use in this form
-- and must be fully initialized by 'GHC.runGhc' first.
defaultDynFlags :: Settings -> DynFlags
defaultDynFlags mySettings =
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
        binBlobThreshold        = Just 500000, -- 500K is a good default (see #16190)
        maxRelevantBinds        = Just 6,
        maxValidHoleFits   = Just 6,
        maxRefHoleFits     = Just 6,
        refLevelHoleFits   = Nothing,
        maxUncoveredPatterns    = 4,
        maxPmCheckModels        = 30,
        simplTickFactor         = 100,
        dmdUnboxWidth           = 3,      -- Default: Assume an unboxed demand on function bodies returning a triple
        ifCompression           = 2,      -- Default: Apply safe compressions
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

        parMakeCount            = Nothing,

        enableTimeStats         = False,
        ghcHeapSize             = Nothing,

        importPaths             = ["."],
        mainModuleNameIs        = mAIN_NAME,
        mainFunIs               = Nothing,
        reductionDepth          = treatZeroAsInf mAX_REDUCTION_DEPTH,
        solverIterations        = treatZeroAsInf mAX_SOLVER_ITERATIONS,
        givensFuel              = mAX_GIVENS_FUEL,
        wantedsFuel             = mAX_WANTEDS_FUEL,
        qcsFuel                 = mAX_QC_FUEL,

        homeUnitId_             = mainUnitId,
        homeUnitInstanceOf_     = Nothing,
        homeUnitInstantiations_ = [],

        workingDirectory        = Nothing,
        thisPackageName         = Nothing,
        hiddenModules           = Set.empty,
        reexportedModules       = [],

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

        externalPluginSpecs     = [],

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
        unitSettings   = sUnitSettings mySettings,
        fileSettings = sFileSettings mySettings,
        toolSettings = sToolSettings mySettings,
        targetPlatform = sTargetPlatform mySettings,
        platformMisc = sPlatformMisc mySettings,
        rawSettings = sRawSettings mySettings,

        tmpDir                  = panic "defaultDynFlags: uninitialized tmpDir",

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
        customWarningCategories = completeWarningCategorySet,
        fatalCustomWarningCategories = emptyWarningCategorySet,
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
        maxForcedSpecArgs = 333,
        -- 333 is fairly arbitrary, see Note [Forcing specialisation]:FS5

        ghciHistSize = 50, -- keep a log of length 50 by default

        ghciBrowserHost = "127.0.0.1",
        ghciBrowserPort = 0,
        ghciBrowserPuppeteerLaunchOpts = Nothing,
        ghciBrowserPlaywrightBrowserType = Nothing,
        ghciBrowserPlaywrightLaunchOpts = Nothing,

        flushOut = defaultFlushOut,
        pprUserLength = 5,
        pprCols = 100,
        useUnicode = False,
        useColor = Auto,
        canUseColor = False,
        useErrorLinks = Auto,
        canUseErrorLinks = False,
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
        -- Use FMA by default on AArch64
        fma = (platformArch . sTargetPlatform $ mySettings) == ArchAArch64,

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

-- OnOffs accumulate in reverse order, so we use foldr in order to
-- process them in the right order
flattenExtensionFlags :: Maybe Language -> [OnOff LangExt.Extension] -> EnumSet LangExt.Extension
flattenExtensionFlags ml = foldr g defaultExtensionFlags
    where g (On f)  flags = EnumSet.insert f flags
          g (Off f) flags = EnumSet.delete f flags
          defaultExtensionFlags = EnumSet.fromList (languageExtensions ml)

-- -----------------------------------------------------------------------------
-- -jN

-- | The type for the -jN argument, specifying that -j on its own represents
-- using the number of machine processors.
data ParMakeCount
  -- | Use this many processors (@-j<n>@ flag).
  = ParMakeThisMany Int
  -- | Use parallelism with as many processors as possible (@-j@ flag without an argument).
  | ParMakeNumProcessors
  -- | Use the specific semaphore @<sem>@ to control parallelism (@-jsem <sem>@ flag).
  | ParMakeSemaphore FilePath

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
  | LinkMergedObj       -- ^ Link objects into a merged "GHCi object"
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

data PkgDbRef
  = GlobalPkgDb
  | UserPkgDb
  | PkgDbPath FilePath
  deriving Eq



-- An argument to --reexported-module which can optionally specify a module renaming.
data ReexportedModule = ReexportedModule { reexportFrom :: ModuleName
                                         , reexportTo   :: ModuleName
                                         }

instance Outputable ReexportedModule where
  ppr (ReexportedModule from to) =
    if from == to then ppr from
                  else ppr from <+> text "as" <+> ppr to

{- Note [Implicit include paths]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  The compile driver adds the path to the folder containing the source file being
  compiled to the 'IncludeSpecs', and this change gets recorded in the 'DynFlags'
  that are used later to compute the interface file. Because of this,
  the flags fingerprint derived from these 'DynFlags' and recorded in the
  interface file will end up containing the absolute path to the source folder.

  Build systems with a remote cache like Bazel or Buck (or Shake, see #16956)
  store the build artifacts produced by a build BA for reuse in subsequent builds.

  Embedding source paths in interface fingerprints will thwart these attempts and
  lead to unnecessary recompilations when the source paths in BA differ from the
  source paths in subsequent builds.
 -}

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
dopt = getDumpFlagFrom verbosity dumpFlags

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


-- | Enable all custom warning categories.
wopt_set_all_custom :: DynFlags -> DynFlags
wopt_set_all_custom dfs
    = dfs{ customWarningCategories = completeWarningCategorySet }

-- | Disable all custom warning categories.
wopt_unset_all_custom :: DynFlags -> DynFlags
wopt_unset_all_custom dfs
    = dfs{ customWarningCategories = emptyWarningCategorySet }

-- | Mark all custom warning categories as fatal (do not set the flags).
wopt_set_all_fatal_custom :: DynFlags -> DynFlags
wopt_set_all_fatal_custom dfs
    = dfs { fatalCustomWarningCategories = completeWarningCategorySet }

-- | Mark all custom warning categories as non-fatal.
wopt_unset_all_fatal_custom :: DynFlags -> DynFlags
wopt_unset_all_fatal_custom dfs
    = dfs { fatalCustomWarningCategories = emptyWarningCategorySet }

-- | Set a custom 'WarningCategory'
wopt_set_custom :: DynFlags -> WarningCategory -> DynFlags
wopt_set_custom dfs f = dfs{ customWarningCategories = insertWarningCategorySet f (customWarningCategories dfs) }

-- | Unset a custom 'WarningCategory'
wopt_unset_custom :: DynFlags -> WarningCategory -> DynFlags
wopt_unset_custom dfs f = dfs{ customWarningCategories = deleteWarningCategorySet f (customWarningCategories dfs) }

-- | Mark a custom 'WarningCategory' as fatal (do not set the flag)
wopt_set_fatal_custom :: DynFlags -> WarningCategory -> DynFlags
wopt_set_fatal_custom dfs f
    = dfs { fatalCustomWarningCategories = insertWarningCategorySet f (fatalCustomWarningCategories dfs) }

-- | Mark a custom 'WarningCategory' as not fatal
wopt_unset_fatal_custom :: DynFlags -> WarningCategory -> DynFlags
wopt_unset_fatal_custom dfs f
    = dfs { fatalCustomWarningCategories = deleteWarningCategorySet f (fatalCustomWarningCategories dfs) }

-- | Are there any custom warning categories enabled?
wopt_any_custom :: DynFlags -> Bool
wopt_any_custom dfs = not (nullWarningCategorySet (customWarningCategories dfs))


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
      Opt_RPath,
      Opt_DumpWithWays,
      Opt_CompactUnwind,
      Opt_ShowErrorContext,
      Opt_SuppressStgReps,
      Opt_UnoptimizedCoreForInterpreter,
      Opt_SpecialiseIncoherents,
      Opt_WriteSelfRecompInfo
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    -- Default floating flags (see Note [RHS Floating])
    ++ [ Opt_LocalFloatOut, Opt_LocalFloatOutTopLevel ]

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
    , ([1,2],   Opt_DoCleverArgEtaExpansion) -- See Note [Eta expansion of arguments in CorePrep]
    , ([0,1,2], Opt_DoEtaReduction)          -- See Note [Eta-reduction in -O0]
    , ([0,1,2], Opt_ProfManualCcs )
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
    , ([0,1,2], Opt_SpecEval)
    , ([],      Opt_SpecEvalDictFun)
    ]


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
    (OSLinux,   ArchRISCV64 {}) -> [Opt_PIC, Opt_ExternalDynamicRefs]
    (OSOpenBSD, ArchX86_64)  -> [Opt_PIC] -- Due to PIE support in
                                         -- OpenBSD since 5.3 release
                                         -- (1 May 2013) we need to
                                         -- always generate PIC. See
                                         -- #10597 for more
                                         -- information.
    _                      -> []

-- | The language extensions implied by the various language variants.
-- When updating this be sure to update the flag documentation in
-- @docs/users_guide/exts@.
languageExtensions :: Maybe Language -> [LangExt.Extension]

-- Nothing: the default case
languageExtensions Nothing = languageExtensions (Just defaultLanguage)

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
       LangExt.NondecreasingIndentation,
           -- strictly speaking non-standard, but we always had this
           -- on implicitly before the option was added in 7.1, and
           -- turning it off breaks code, so we're keeping it on for
           -- backwards compatibility.  Cabal uses -XHaskell98 by
           -- default unless you specify another language.
       LangExt.DeepSubsumption,
       -- Non-standard but enabled for backwards compatability (see GHC proposal #511)
       LangExt.ListTuplePuns
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
       LangExt.RelaxedPolyRec,
       LangExt.DeepSubsumption,
       LangExt.ListTuplePuns ]

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
       LangExt.ListTuplePuns,
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

languageExtensions (Just GHC2024)
    = languageExtensions (Just GHC2021) ++
      [LangExt.DataKinds,
       LangExt.DerivingStrategies,
       LangExt.DisambiguateRecordFields,
       LangExt.ExplicitNamespaces,
       LangExt.GADTs,
       LangExt.MonoLocalBinds,
       LangExt.LambdaCase,
       LangExt.RoleAnnotations]

ways :: DynFlags -> Ways
ways dflags
   | dynamicNow dflags = addWay WayDyn (targetWays_ dflags)
   | otherwise         = targetWays_ dflags

-- | Get target profile
targetProfile :: DynFlags -> Profile
targetProfile dflags = Profile (targetPlatform dflags) (ways dflags)

--
-- System tool settings and locations

programName :: DynFlags -> String
programName dflags = ghcNameVersion_programName $ ghcNameVersion dflags
projectVersion :: DynFlags -> String
projectVersion dflags = ghcNameVersion_projectVersion (ghcNameVersion dflags)
ghcUsagePath          :: DynFlags -> FilePath
ghcUsagePath dflags = fileSettings_ghcUsagePath $ fileSettings dflags
ghciUsagePath         :: DynFlags -> FilePath
ghciUsagePath dflags = fileSettings_ghciUsagePath $ fileSettings dflags
topDir                :: DynFlags -> FilePath
topDir dflags = fileSettings_topDir $ fileSettings dflags
toolDir               :: DynFlags -> Maybe FilePath
toolDir dflags = fileSettings_toolDir $ fileSettings dflags
extraGccViaCFlags     :: DynFlags -> [String]
extraGccViaCFlags dflags = toolSettings_extraGccViaCFlags $ toolSettings dflags
globalPackageDatabasePath   :: DynFlags -> FilePath
globalPackageDatabasePath dflags = fileSettings_globalPackageDatabase $ fileSettings dflags

-- | The directory for this version of ghc in the user's app directory
-- The appdir used to be in ~/.ghc but to respect the XDG specification
-- we want to move it under $XDG_DATA_HOME/
-- However, old tooling (like cabal) might still write package environments
-- to the old directory, so we prefer that if a subdirectory of ~/.ghc
-- with the correct target and GHC version suffix exists.
--
-- i.e. if ~/.ghc/$UNIQUE_SUBDIR exists we use that
-- otherwise we use $XDG_DATA_HOME/$UNIQUE_SUBDIR
--
-- UNIQUE_SUBDIR is typically a combination of the target platform and GHC version
versionedAppDir :: String -> ArchOS -> MaybeT IO FilePath
versionedAppDir appname platform = do
  -- Make sure we handle the case the HOME isn't set (see #11678)
  -- We need to fallback to the old scheme if the subdirectory exists.
  msum $ map (checkIfExists <=< fmap (</> versionedFilePath platform))
       [ tryMaybeT $ getAppUserDataDirectory appname  -- this is ~/.ghc/
       , tryMaybeT $ getXdgDirectory XdgData appname -- this is $XDG_DATA_HOME/
       ]
  where
    checkIfExists dir = tryMaybeT (doesDirectoryExist dir) >>= \case
      True -> pure dir
      False -> MaybeT (pure Nothing)

versionedFilePath :: ArchOS -> FilePath
versionedFilePath platform = uniqueSubdir platform

-- | Access the unit-id of the version of `base` which we will automatically link
-- against.
baseUnitId :: DynFlags -> UnitId
baseUnitId dflags = unitSettings_baseUnitId (unitSettings dflags)

-- SDoc
-------------------------------------------
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
  , sdocPrintErrIndexLinks          = overrideWith (canUseErrorLinks dflags) (useErrorLinks dflags)
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
  , sdocSuppressCoercionTypes       = gopt Opt_SuppressCoercionTypes dflags
  , sdocSuppressUnfoldings          = gopt Opt_SuppressUnfoldings dflags
  , sdocSuppressVarKinds            = gopt Opt_SuppressVarKinds dflags
  , sdocSuppressUniques             = gopt Opt_SuppressUniques dflags
  , sdocSuppressModulePrefixes      = gopt Opt_SuppressModulePrefixes dflags
  , sdocSuppressStgExts             = gopt Opt_SuppressStgExts dflags
  , sdocSuppressStgReps             = gopt Opt_SuppressStgReps dflags
  , sdocErrorSpans                  = gopt Opt_ErrorSpans dflags
  , sdocStarIsType                  = xopt LangExt.StarIsType dflags
  , sdocLinearTypes                 = xopt LangExt.LinearTypes dflags
  , sdocListTuplePuns               = xopt LangExt.ListTuplePuns dflags
  , sdocPrintTypeAbbreviations      = True
  , sdocUnitIdForUser               = ftext
  }

-- | Initialize the pretty-printing options using the default user style
initDefaultSDocContext :: DynFlags -> SDocContext
initDefaultSDocContext dflags = initSDocContext dflags defaultUserStyle

initPromotionTickContext :: DynFlags -> PromotionTickContext
initPromotionTickContext dflags =
  PromTickCtx {
    ptcListTuplePuns = xopt LangExt.ListTuplePuns dflags,
    ptcPrintRedundantPromTicks = gopt Opt_PrintRedundantPromotionTicks dflags
  }

-- -----------------------------------------------------------------------------
-- SSE, AVX, FMA

isSse3Enabled :: DynFlags -> Bool
isSse3Enabled dflags = sseVersion dflags >= Just SSE3

isSse4_1Enabled :: DynFlags -> Bool
isSse4_1Enabled dflags = sseVersion dflags >= Just SSE4

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

isFmaEnabled :: DynFlags -> Bool
isFmaEnabled dflags = fma dflags

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
