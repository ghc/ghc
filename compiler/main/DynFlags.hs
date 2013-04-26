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

{-# OPTIONS -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

module DynFlags (
        -- * Dynamic flags and associated configuration types
        DumpFlag(..),
        GeneralFlag(..),
        WarningFlag(..),
        ExtensionFlag(..),
        Language(..),
        PlatformConstants(..),
        FatalMessager, LogAction, FlushOut(..), FlushErr(..),
        ProfAuto(..),
        glasgowExtsFlags,
        dopt, dopt_set, dopt_unset,
        gopt, gopt_set, gopt_unset,
        wopt, wopt_set, wopt_unset,
        xopt, xopt_set, xopt_unset,
        lang_set,
        whenGeneratingDynamicToo, ifGeneratingDynamicToo,
        whenCannotGenerateDynamicToo,
        doDynamicToo,
        DynFlags(..),
        HasDynFlags(..), ContainsDynFlags(..),
        RtsOptsEnabled(..),
        HscTarget(..), isObjectTarget, defaultObjectTarget,
        targetRetainsAllBindings,
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..),
        PkgConfRef(..),
        Option(..), showOpt,
        DynLibLoader(..),
        fFlags, fWarningFlags, fLangFlags, xFlags,
        dynFlagDependencies,
        tablesNextToCode, mkTablesNextToCode,

        printOutputForUser, printInfoForUser,

        Way(..), mkBuildTag, wayRTSOnly, updateWays,
        wayGeneralFlags, wayUnsetGeneralFlags,

        -- ** Safe Haskell
        SafeHaskellMode(..),
        safeHaskellOn, safeImportsOn, safeLanguageOn, safeInferOn,
        packageTrustOn,
        safeDirectImpsReq, safeImplicitImpsReq,
        unsafeFlags,

        -- ** System tool settings and locations
        Settings(..),
        targetPlatform,
        ghcUsagePath, ghciUsagePath, topDir, tmpDir, rawSettings,
        extraGccViaCFlags, systemPackageConfig,
        pgm_L, pgm_P, pgm_F, pgm_c, pgm_s, pgm_a, pgm_l, pgm_dll, pgm_T,
        pgm_sysman, pgm_windres, pgm_lo, pgm_lc,
        opt_L, opt_P, opt_F, opt_c, opt_a, opt_l,
        opt_windres, opt_lo, opt_lc,


        -- ** Manipulating DynFlags
        defaultDynFlags,                -- Settings -> DynFlags
        defaultWays,
        interpWays,
        initDynFlags,                   -- DynFlags -> IO DynFlags
        defaultFatalMessager,
        defaultLogAction,
        defaultLogActionHPrintDoc,
        defaultFlushOut,
        defaultFlushErr,

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlags,
        updOptLevel,
        setTmpDir,
        setPackageName,

        -- ** Parsing DynFlags
        parseDynamicFlagsCmdLine,
        parseDynamicFilePragma,
        parseDynamicFlagsFull,

        -- ** Available DynFlags
        allFlags,
        flagsAll,
        flagsDynamic,
        flagsPackage,

        supportedLanguagesAndExtensions,

        -- ** DynFlags C compiler options
        picCCOpts, picPOpts,

        -- * Configuration of the stg-to-stg passes
        StgToDo(..),
        getStgToDo,

        -- * Compiler configuration suitable for display to the user
        compilerInfo,

#ifdef GHCI
-- Only in stage 2 can we be sure that the RTS
-- exposes the appropriate runtime boolean
        rtsIsProfiled,
#endif

#include "../includes/dist-derivedconstants/header/GHCConstantsHaskellExports.hs"
        bLOCK_SIZE_W,
        wORD_SIZE_IN_BITS,
        tAG_MASK,
        mAX_PTR_TAG,
        tARGET_MIN_INT, tARGET_MAX_INT, tARGET_MAX_WORD,

        unsafeGlobalDynFlags, setUnsafeGlobalDynFlags,

        -- * SSE
        isSse2Enabled,
        isSse4_2Enabled,
  ) where

#include "HsVersions.h"

import Platform
import PlatformConstants
import Module
import PackageConfig
import {-# SOURCE #-} PrelNames ( mAIN )
import {-# SOURCE #-} Packages (PackageState)
import DriverPhases     ( Phase(..), phaseInputExt )
import Config
import CmdLineParser
import Constants
import Panic
import Util
import Maybes           ( orElse )
import MonadUtils
import qualified Pretty
import SrcLoc
import FastString
import Outputable
#ifdef GHCI
import Foreign.C        ( CInt(..) )
#endif
import {-# SOURCE #-} ErrUtils ( Severity(..), MsgDoc, mkLocMessage )

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Control.Monad

import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import System.FilePath
import System.IO
import System.IO.Error

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import GHC.Foreign (withCString, peekCString)

-- -----------------------------------------------------------------------------
-- DynFlags

data DumpFlag

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_cmm_raw
   -- All of the cmm subflags (there are a lot!)  Automatically
   -- enabled if you run -ddump-cmm
   | Opt_D_dump_cmm_cfg
   | Opt_D_dump_cmm_cbe
   | Opt_D_dump_cmm_proc
   | Opt_D_dump_cmm_rewrite
   | Opt_D_dump_cmm_sp
   | Opt_D_dump_cmm_procmap
   | Opt_D_dump_cmm_split
   | Opt_D_dump_cmm_info
   | Opt_D_dump_cmm_cps
   -- end cmm subflags
   | Opt_D_dump_asm
   | Opt_D_dump_asm_native
   | Opt_D_dump_asm_liveness
   | Opt_D_dump_asm_regalloc
   | Opt_D_dump_asm_regalloc_stages
   | Opt_D_dump_asm_conflicts
   | Opt_D_dump_asm_stats
   | Opt_D_dump_asm_expanded
   | Opt_D_dump_llvm
   | Opt_D_dump_core_stats
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_rule_firings
   | Opt_D_dump_rule_rewrites
   | Opt_D_dump_simpl_trace
   | Opt_D_dump_occur_anal
   | Opt_D_dump_parsed
   | Opt_D_dump_rn
   | Opt_D_dump_core_pipeline -- TODO FIXME: dump after simplifier stats
   | Opt_D_dump_simpl
   | Opt_D_dump_simpl_iterations
   | Opt_D_dump_simpl_phases
   | Opt_D_dump_spec
   | Opt_D_dump_prep
   | Opt_D_dump_stg
   | Opt_D_dump_stranal
   | Opt_D_dump_tc
   | Opt_D_dump_types
   | Opt_D_dump_rules
   | Opt_D_dump_cse
   | Opt_D_dump_worker_wrapper
   | Opt_D_dump_rn_trace
   | Opt_D_dump_rn_stats
   | Opt_D_dump_opt_cmm
   | Opt_D_dump_simpl_stats
   | Opt_D_dump_cs_trace -- Constraint solver in type checker
   | Opt_D_dump_tc_trace
   | Opt_D_dump_if_trace
   | Opt_D_dump_vt_trace
   | Opt_D_dump_splices
   | Opt_D_dump_BCOs
   | Opt_D_dump_vect
   | Opt_D_dump_ticked
   | Opt_D_dump_rtti
   | Opt_D_source_stats
   | Opt_D_verbose_stg2stg
   | Opt_D_dump_hi
   | Opt_D_dump_hi_diffs
   | Opt_D_dump_mod_cycles
   | Opt_D_dump_view_pattern_commoning
   | Opt_D_verbose_core2core

   deriving (Eq, Show, Enum)

-- | Enumerates the simple on-or-off dynamic flags
data GeneralFlag

   = Opt_DumpToFile                     -- ^ Append dump output to files instead of stdout.
   | Opt_D_faststring_stats
   | Opt_D_dump_minimal_imports
   | Opt_DoCoreLinting
   | Opt_DoStgLinting
   | Opt_DoCmmLinting
   | Opt_DoAsmLinting
   | Opt_NoLlvmMangler                 -- hidden flag

   | Opt_WarnIsError                    -- -Werror; makes warnings fatal

   | Opt_PrintExplicitForalls

   -- optimisation opts
   | Opt_Strictness
   | Opt_FullLaziness
   | Opt_FloatIn
   | Opt_Specialise
   | Opt_StaticArgumentTransformation
   | Opt_CSE
   | Opt_LiberateCase
   | Opt_SpecConstr
   | Opt_DoLambdaEtaExpansion
   | Opt_IgnoreAsserts
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_UnboxStrictFields
   | Opt_UnboxSmallStrictFields
   | Opt_DictsCheap
   | Opt_EnableRewriteRules             -- Apply rewrite rules during simplification
   | Opt_Vectorise
   | Opt_VectorisationAvoidance
   | Opt_RegsGraph                      -- do graph coloring register allocation
   | Opt_RegsIterative                  -- do iterative coalescing graph coloring register allocation
   | Opt_PedanticBottoms                -- Be picky about how we treat bottom
   | Opt_LlvmTBAA                       -- Use LLVM TBAA infastructure for improving AA (hidden flag)
   | Opt_IrrefutableTuples
   | Opt_CmmSink
   | Opt_CmmElimCommonBlocks
   | Opt_OmitYields
   | Opt_SimpleListLiterals
   | Opt_FunToThunk               -- allow WwLib.mkWorkerArgs to remove all value lambdas

   -- Interface files
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_ExposeAllUnfoldings

   -- profiling opts
   | Opt_AutoSccsOnIndividualCafs
   | Opt_ProfCountEntries

   -- misc opts
   | Opt_Pp
   | Opt_ForceRecomp
   | Opt_ExcessPrecision
   | Opt_EagerBlackHoling
   | Opt_NoHsMain
   | Opt_SplitObjs
   | Opt_StgStats
   | Opt_HideAllPackages
   | Opt_PrintBindResult
   | Opt_Haddock
   | Opt_HaddockOptions
   | Opt_Hpc_No_Auto
   | Opt_BreakOnException
   | Opt_BreakOnError
   | Opt_PrintEvldWithShow
   | Opt_PrintBindContents
   | Opt_GenManifest
   | Opt_EmbedManifest
   | Opt_EmitExternalCore
   | Opt_SharedImplib
   | Opt_BuildingCabalPackage
   | Opt_IgnoreDotGhci
   | Opt_GhciSandbox
   | Opt_GhciHistory
   | Opt_HelpfulErrors
   | Opt_DeferTypeErrors
   | Opt_Parallel
   | Opt_GranMacros
   | Opt_PIC
   | Opt_SccProfilingOn
   | Opt_Ticky
   | Opt_Ticky_Allocd
   | Opt_Ticky_LNE
   | Opt_Ticky_Dyn_Thunk
   | Opt_Static
   | Opt_RPath
   | Opt_RelativeDynlibPaths
   | Opt_Hpc

   -- PreInlining is on by default. The option is there just to see how
   -- bad things get if you turn it off!
   | Opt_SimplPreInlining

   -- output style opts
   | Opt_ErrorSpans -- Include full span info in error messages,
                    -- instead of just the start position.
   | Opt_PprCaseAsLet

   -- Suppress all coercions, them replacing with '...'
   | Opt_SuppressCoercions
   | Opt_SuppressVarKinds
   -- Suppress module id prefixes on variables.
   | Opt_SuppressModulePrefixes
   -- Suppress type applications.
   | Opt_SuppressTypeApplications
   -- Suppress info such as arity and unfoldings on identifiers.
   | Opt_SuppressIdInfo
   -- Suppress separate type signatures in core, but leave types on
   -- lambda bound vars
   | Opt_SuppressTypeSignatures
   -- Suppress unique ids on variables.
   -- Except for uniques, as some simplifier phases introduce new
   -- variables that have otherwise identical names.
   | Opt_SuppressUniques

   -- temporary flags
   | Opt_RunCPS
   | Opt_RunCPSZ
   | Opt_AutoLinkPackages
   | Opt_ImplicitImportQualified

   -- keeping stuff
   | Opt_KeepHiDiffs
   | Opt_KeepHcFiles
   | Opt_KeepSFiles
   | Opt_KeepTmpFiles
   | Opt_KeepRawTokenStream
   | Opt_KeepLlvmFiles

   | Opt_BuildDynamicToo

   -- safe haskell flags
   | Opt_DistrustAllPackages
   | Opt_PackageTrust

   deriving (Eq, Show, Enum)

data WarningFlag =
     Opt_WarnDuplicateExports
   | Opt_WarnDuplicateConstraints
   | Opt_WarnHiShadows
   | Opt_WarnImplicitPrelude
   | Opt_WarnIncompletePatterns
   | Opt_WarnIncompleteUniPatterns
   | Opt_WarnIncompletePatternsRecUpd
   | Opt_WarnMissingFields
   | Opt_WarnMissingImportList
   | Opt_WarnMissingMethods
   | Opt_WarnMissingSigs
   | Opt_WarnMissingLocalSigs
   | Opt_WarnNameShadowing
   | Opt_WarnOverlappingPatterns
   | Opt_WarnTypeDefaults
   | Opt_WarnMonomorphism
   | Opt_WarnUnusedBinds
   | Opt_WarnUnusedImports
   | Opt_WarnUnusedMatches
   | Opt_WarnWarningsDeprecations
   | Opt_WarnDeprecatedFlags
   | Opt_WarnDodgyExports
   | Opt_WarnDodgyImports
   | Opt_WarnOrphans
   | Opt_WarnAutoOrphans
   | Opt_WarnIdentities
   | Opt_WarnTabs
   | Opt_WarnUnrecognisedPragmas
   | Opt_WarnDodgyForeignImports
   | Opt_WarnLazyUnliftedBindings
   | Opt_WarnUnusedDoBind
   | Opt_WarnWrongDoBind
   | Opt_WarnAlternativeLayoutRuleTransitional
   | Opt_WarnUnsafe
   | Opt_WarnSafe
   | Opt_WarnPointlessPragmas
   | Opt_WarnUnsupportedCallingConventions
   | Opt_WarnUnsupportedLlvmVersion
   | Opt_WarnInlineRuleShadowing
   | Opt_WarnTypeableInstances
   deriving (Eq, Show, Enum)

data Language = Haskell98 | Haskell2010
   deriving Enum

-- | The various Safe Haskell modes
data SafeHaskellMode
   = Sf_None
   | Sf_Unsafe
   | Sf_Trustworthy
   | Sf_Safe
   | Sf_SafeInferred
   deriving (Eq)

instance Show SafeHaskellMode where
    show Sf_None         = "None"
    show Sf_Unsafe       = "Unsafe"
    show Sf_Trustworthy  = "Trustworthy"
    show Sf_Safe         = "Safe"
    show Sf_SafeInferred = "Safe-Inferred"

instance Outputable SafeHaskellMode where
    ppr = text . show

data ExtensionFlag
   = Opt_Cpp
   | Opt_OverlappingInstances
   | Opt_UndecidableInstances
   | Opt_IncoherentInstances
   | Opt_MonomorphismRestriction
   | Opt_MonoPatBinds
   | Opt_MonoLocalBinds
   | Opt_RelaxedPolyRec           -- Deprecated
   | Opt_ExtendedDefaultRules     -- Use GHC's extended rules for defaulting
   | Opt_ForeignFunctionInterface
   | Opt_UnliftedFFITypes
   | Opt_InterruptibleFFI
   | Opt_CApiFFI
   | Opt_GHCForeignImportPrim
   | Opt_ParallelArrays           -- Syntactic support for parallel arrays
   | Opt_Arrows                   -- Arrow-notation syntax
   | Opt_TemplateHaskell
   | Opt_QuasiQuotes
   | Opt_ImplicitParams
   | Opt_ImplicitPrelude
   | Opt_ScopedTypeVariables
   | Opt_AllowAmbiguousTypes
   | Opt_UnboxedTuples
   | Opt_BangPatterns
   | Opt_TypeFamilies
   | Opt_OverloadedStrings
   | Opt_OverloadedLists
   | Opt_DisambiguateRecordFields
   | Opt_RecordWildCards
   | Opt_RecordPuns
   | Opt_ViewPatterns
   | Opt_GADTs
   | Opt_GADTSyntax
   | Opt_NPlusKPatterns
   | Opt_DoAndIfThenElse
   | Opt_RebindableSyntax
   | Opt_ConstraintKinds
   | Opt_PolyKinds                -- Kind polymorphism
   | Opt_DataKinds                -- Datatype promotion
   | Opt_InstanceSigs
 
   | Opt_StandaloneDeriving
   | Opt_DeriveDataTypeable
   | Opt_AutoDeriveTypeable       -- Automatic derivation of Typeable
   | Opt_DeriveFunctor
   | Opt_DeriveTraversable
   | Opt_DeriveFoldable
   | Opt_DeriveGeneric            -- Allow deriving Generic/1
   | Opt_DefaultSignatures        -- Allow extra signatures for defmeths

   | Opt_TypeSynonymInstances
   | Opt_FlexibleContexts
   | Opt_FlexibleInstances
   | Opt_ConstrainedClassMethods
   | Opt_MultiParamTypeClasses
   | Opt_NullaryTypeClasses
   | Opt_FunctionalDependencies
   | Opt_UnicodeSyntax
   | Opt_ExistentialQuantification
   | Opt_MagicHash
   | Opt_EmptyDataDecls
   | Opt_KindSignatures
   | Opt_ParallelListComp
   | Opt_TransformListComp
   | Opt_MonadComprehensions
   | Opt_GeneralizedNewtypeDeriving
   | Opt_RecursiveDo
   | Opt_PostfixOperators
   | Opt_TupleSections
   | Opt_PatternGuards
   | Opt_LiberalTypeSynonyms
   | Opt_RankNTypes
   | Opt_ImpredicativeTypes
   | Opt_TypeOperators
   | Opt_ExplicitNamespaces
   | Opt_PackageImports
   | Opt_ExplicitForAll
   | Opt_AlternativeLayoutRule
   | Opt_AlternativeLayoutRuleTransitional
   | Opt_DatatypeContexts
   | Opt_NondecreasingIndentation
   | Opt_RelaxedLayout
   | Opt_TraditionalRecordSyntax
   | Opt_LambdaCase
   | Opt_MultiWayIf
   | Opt_TypeHoles
   | Opt_EmptyCase
   deriving (Eq, Enum, Show)

-- | Contains not only a collection of 'GeneralFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  ghcMode               :: GhcMode,
  ghcLink               :: GhcLink,
  hscTarget             :: HscTarget,
  settings              :: Settings,
  extCoreName           :: String,      -- ^ Name of the .hcr output file
  verbosity             :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
  optLevel              :: Int,         -- ^ Optimisation level
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  shouldDumpSimplPhase  :: Maybe String,
  ruleCheck             :: Maybe String,
  strictnessBefore      :: [Int],       -- ^ Additional demand analysis

  simplTickFactor       :: Int,         -- ^ Multiplier for simplifier ticks
  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  specConstrRecursive   :: Int,         -- ^ Max number of specialisations for recursive types
                                        --   Not optional; otherwise ForceSpecConstr can diverge.
  liberateCaseThreshold :: Maybe Int,   -- ^ Threshold for LiberateCase
  floatLamArgs          :: Maybe Int,   -- ^ Arg count for lambda floating
                                        --   See CoreMonad.FloatOutSwitches
  historySize           :: Int,

  cmdlineHcIncludes     :: [String],    -- ^ @\-\#includes@
  importPaths           :: [FilePath],
  mainModIs             :: Module,
  mainFunIs             :: Maybe String,
  ctxtStkDepth          :: Int,         -- ^ Typechecker context stack depth

  thisPackage           :: PackageId,   -- ^ name of package currently being compiled

  -- ways
  ways                  :: [Way],       -- ^ Way flags from the command line
  buildTag              :: String,      -- ^ The global \"way\" (e.g. \"p\" for prof)
  rtsBuildTag           :: String,      -- ^ The RTS \"way\"

  -- For object splitting
  splitInfo             :: Maybe (String,Int),

  -- paths etc.
  objectDir             :: Maybe String,
  dylibInstallName      :: Maybe String,
  hiDir                 :: Maybe String,
  stubDir               :: Maybe String,
  dumpDir               :: Maybe String,

  objectSuf             :: String,
  hcSuf                 :: String,
  hiSuf                 :: String,

  canGenerateDynamicToo :: IORef Bool,
  dynObjectSuf          :: String,
  dynHiSuf              :: String,

  outputFile            :: Maybe String,
  dynOutputFile         :: Maybe String,
  outputHi              :: Maybe String,
  dynLibLoader          :: DynLibLoader,

  -- | This is set by 'DriverPipeline.runPipeline' based on where
  --    its output is going.
  dumpPrefix            :: Maybe FilePath,

  -- | Override the 'dumpPrefix' set by 'DriverPipeline.runPipeline'.
  --    Set by @-ddump-file-prefix@
  dumpPrefixForce       :: Maybe FilePath,

  ldInputs              :: [String],

  includePaths          :: [String],
  libraryPaths          :: [String],
  frameworkPaths        :: [String],    -- used on darwin only
  cmdlineFrameworks     :: [String],    -- ditto

  rtsOpts               :: Maybe String,
  rtsOptsEnabled        :: RtsOptsEnabled,

  hpcDir                :: String,      -- ^ Path to store the .mix files

  -- Plugins
  pluginModNames        :: [ModuleName],
  pluginModNameOpts     :: [(ModuleName,String)],

  --  For ghc -M
  depMakefile           :: FilePath,
  depIncludePkgDeps     :: Bool,
  depExcludeMods        :: [ModuleName],
  depSuffixes           :: [String],

  --  Package flags
  extraPkgConfs         :: [PkgConfRef] -> [PkgConfRef],
        -- ^ The @-package-db@ flags given on the command line, in the order
        -- they appeared.

  packageFlags          :: [PackageFlag],
        -- ^ The @-package@ and @-hide-package@ flags from the command-line

  -- Package state
  -- NB. do not modify this field, it is calculated by
  -- Packages.initPackages and Packages.updatePackages.
  pkgDatabase           :: Maybe [PackageConfig],
  pkgState              :: PackageState,

  -- Temporary files
  -- These have to be IORefs, because the defaultCleanupHandler needs to
  -- know what to clean when an exception happens
  filesToClean          :: IORef [FilePath],
  dirsToClean           :: IORef (Map FilePath FilePath),
  filesToNotIntermediateClean :: IORef [FilePath],


  -- Names of files which were generated from -ddump-to-file; used to
  -- track which ones we need to truncate because it's our first run
  -- through
  generatedDumps        :: IORef (Set FilePath),

  -- hsc dynamic flags
  dumpFlags             :: IntSet,
  generalFlags          :: IntSet,
  warningFlags          :: IntSet,
  -- Don't change this without updating extensionFlags:
  language              :: Maybe Language,
  -- | Safe Haskell mode
  safeHaskell           :: SafeHaskellMode,
  -- We store the location of where some extension and flags were turned on so
  -- we can produce accurate error messages when Safe Haskell fails due to
  -- them.
  thOnLoc               :: SrcSpan,
  newDerivOnLoc         :: SrcSpan,
  pkgTrustOnLoc         :: SrcSpan,
  warnSafeOnLoc         :: SrcSpan,
  warnUnsafeOnLoc       :: SrcSpan,
  -- Don't change this without updating extensionFlags:
  extensions            :: [OnOff ExtensionFlag],
  -- extensionFlags should always be equal to
  --     flattenExtensionFlags language extensions
  extensionFlags        :: IntSet,

  -- Unfolding control
  -- See Note [Discounts and thresholds] in CoreUnfold
  ufCreationThreshold   :: Int,
  ufUseThreshold        :: Int,
  ufFunAppDiscount      :: Int,
  ufDictDiscount        :: Int,
  ufKeenessFactor       :: Float,
  ufDearOp              :: Int,

  maxWorkerArgs         :: Int,

  ghciHistSize          :: Int,

  -- | MsgDoc output action: use "ErrUtils" instead of this if you can
  log_action            :: LogAction,
  flushOut              :: FlushOut,
  flushErr              :: FlushErr,

  haddockOptions        :: Maybe String,
  ghciScripts           :: [String],

  -- Output style options
  pprUserLength         :: Int,
  pprCols               :: Int,
  traceLevel            :: Int, -- Standard level is 1. Less verbose is 0.

  useUnicodeQuotes      :: Bool,

  -- | what kind of {-# SCC #-} to add automatically
  profAuto              :: ProfAuto,

  interactivePrint      :: Maybe String,

  llvmVersion           :: IORef Int,

  nextWrapperNum        :: IORef Int,

  -- | Machine dependant flags (-m<blah> stuff)
  sseVersion            :: Maybe (Int, Int)  -- (major, minor)
 }

class HasDynFlags m where
    getDynFlags :: m DynFlags

class ContainsDynFlags t where
    extractDynFlags :: t -> DynFlags
    replaceDynFlags :: t -> DynFlags -> t

data ProfAuto
  = NoProfAuto         -- ^ no SCC annotations added
  | ProfAutoAll        -- ^ top-level and nested functions are annotated
  | ProfAutoTop        -- ^ top-level functions annotated only
  | ProfAutoExports    -- ^ exported functions annotated only
  | ProfAutoCalls      -- ^ annotate call-sites
  deriving (Enum)

data Settings = Settings {
  sTargetPlatform        :: Platform,    -- Filled in by SysTools
  sGhcUsagePath          :: FilePath,    -- Filled in by SysTools
  sGhciUsagePath         :: FilePath,    -- ditto
  sTopDir                :: FilePath,
  sTmpDir                :: String,      -- no trailing '/'
  -- You shouldn't need to look things up in rawSettings directly.
  -- They should have their own fields instead.
  sRawSettings           :: [(String, String)],
  sExtraGccViaCFlags     :: [String],
  sSystemPackageConfig   :: FilePath,
  sLdSupportsCompactUnwind :: Bool,
  sLdSupportsBuildId       :: Bool,
  sLdSupportsFilelist      :: Bool,
  sLdIsGnuLd               :: Bool,
  -- commands for particular phases
  sPgm_L                 :: String,
  sPgm_P                 :: (String,[Option]),
  sPgm_F                 :: String,
  sPgm_c                 :: (String,[Option]),
  sPgm_s                 :: (String,[Option]),
  sPgm_a                 :: (String,[Option]),
  sPgm_l                 :: (String,[Option]),
  sPgm_dll               :: (String,[Option]),
  sPgm_T                 :: String,
  sPgm_sysman            :: String,
  sPgm_windres           :: String,
  sPgm_lo                :: (String,[Option]), -- LLVM: opt llvm optimiser
  sPgm_lc                :: (String,[Option]), -- LLVM: llc static compiler
  -- options for particular phases
  sOpt_L                 :: [String],
  sOpt_P                 :: [String],
  sOpt_F                 :: [String],
  sOpt_c                 :: [String],
  sOpt_a                 :: [String],
  sOpt_l                 :: [String],
  sOpt_windres           :: [String],
  sOpt_lo                :: [String], -- LLVM: llvm optimiser
  sOpt_lc                :: [String], -- LLVM: llc static compiler

  sPlatformConstants     :: PlatformConstants
 }

targetPlatform :: DynFlags -> Platform
targetPlatform dflags = sTargetPlatform (settings dflags)

ghcUsagePath          :: DynFlags -> FilePath
ghcUsagePath dflags = sGhcUsagePath (settings dflags)
ghciUsagePath         :: DynFlags -> FilePath
ghciUsagePath dflags = sGhciUsagePath (settings dflags)
topDir                :: DynFlags -> FilePath
topDir dflags = sTopDir (settings dflags)
tmpDir                :: DynFlags -> String
tmpDir dflags = sTmpDir (settings dflags)
rawSettings           :: DynFlags -> [(String, String)]
rawSettings dflags = sRawSettings (settings dflags)
extraGccViaCFlags     :: DynFlags -> [String]
extraGccViaCFlags dflags = sExtraGccViaCFlags (settings dflags)
systemPackageConfig   :: DynFlags -> FilePath
systemPackageConfig dflags = sSystemPackageConfig (settings dflags)
pgm_L                 :: DynFlags -> String
pgm_L dflags = sPgm_L (settings dflags)
pgm_P                 :: DynFlags -> (String,[Option])
pgm_P dflags = sPgm_P (settings dflags)
pgm_F                 :: DynFlags -> String
pgm_F dflags = sPgm_F (settings dflags)
pgm_c                 :: DynFlags -> (String,[Option])
pgm_c dflags = sPgm_c (settings dflags)
pgm_s                 :: DynFlags -> (String,[Option])
pgm_s dflags = sPgm_s (settings dflags)
pgm_a                 :: DynFlags -> (String,[Option])
pgm_a dflags = sPgm_a (settings dflags)
pgm_l                 :: DynFlags -> (String,[Option])
pgm_l dflags = sPgm_l (settings dflags)
pgm_dll               :: DynFlags -> (String,[Option])
pgm_dll dflags = sPgm_dll (settings dflags)
pgm_T                 :: DynFlags -> String
pgm_T dflags = sPgm_T (settings dflags)
pgm_sysman            :: DynFlags -> String
pgm_sysman dflags = sPgm_sysman (settings dflags)
pgm_windres           :: DynFlags -> String
pgm_windres dflags = sPgm_windres (settings dflags)
pgm_lo                :: DynFlags -> (String,[Option])
pgm_lo dflags = sPgm_lo (settings dflags)
pgm_lc                :: DynFlags -> (String,[Option])
pgm_lc dflags = sPgm_lc (settings dflags)
opt_L                 :: DynFlags -> [String]
opt_L dflags = sOpt_L (settings dflags)
opt_P                 :: DynFlags -> [String]
opt_P dflags = concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
            ++ sOpt_P (settings dflags)
opt_F                 :: DynFlags -> [String]
opt_F dflags = sOpt_F (settings dflags)
opt_c                 :: DynFlags -> [String]
opt_c dflags = concatMap (wayOptc (targetPlatform dflags)) (ways dflags)
            ++ sOpt_c (settings dflags)
opt_a                 :: DynFlags -> [String]
opt_a dflags = sOpt_a (settings dflags)
opt_l                 :: DynFlags -> [String]
opt_l dflags = concatMap (wayOptl (targetPlatform dflags)) (ways dflags)
            ++ sOpt_l (settings dflags)
opt_windres           :: DynFlags -> [String]
opt_windres dflags = sOpt_windres (settings dflags)
opt_lo                :: DynFlags -> [String]
opt_lo dflags = sOpt_lo (settings dflags)
opt_lc                :: DynFlags -> [String]
opt_lc dflags = sOpt_lc (settings dflags)

-- | The target code type of the compilation (if any).
--
-- Whenever you change the target, also make sure to set 'ghcLink' to
-- something sensible.
--
-- 'HscNothing' can be used to avoid generating any output, however, note
-- that:
--
--  * This will not run the desugaring step, thus no warnings generated in
--    this step will be output.  In particular, this includes warnings related
--    to pattern matching.  You can run the desugarer manually using
--    'GHC.desugarModule'.
--
--  * If a program uses Template Haskell the typechecker may try to run code
--    from an imported module.  This will fail if no code has been generated
--    for this module.  You can use 'GHC.needsTemplateHaskell' to detect
--    whether this might be the case and choose to either switch to a
--    different target or avoid typechecking such modules.  (The latter may be
--    preferable for security reasons.)
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
  ppr CompManager = ptext (sLit "CompManager")
  ppr OneShot     = ptext (sLit "OneShot")
  ppr MkDepend    = ptext (sLit "MkDepend")

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
  deriving (Eq, Show)

isNoLink :: GhcLink -> Bool
isNoLink NoLink = True
isNoLink _      = False

data PackageFlag
  = ExposePackage   String
  | ExposePackageId String
  | HidePackage     String
  | IgnorePackage   String
  | TrustPackage    String
  | DistrustPackage String
  deriving (Eq, Show)

defaultHscTarget :: Platform -> HscTarget
defaultHscTarget = defaultObjectTarget

-- | The 'HscTarget' value corresponding to the default way to create
-- object files on the current platform.
defaultObjectTarget :: Platform -> HscTarget
defaultObjectTarget platform
  | platformUnregisterised platform     =  HscC
  | cGhcWithNativeCodeGen == "YES"      =  HscAsm
  | otherwise                           =  HscLlvm

tablesNextToCode :: DynFlags -> Bool
tablesNextToCode dflags
    = mkTablesNextToCode (platformUnregisterised (targetPlatform dflags))

-- Determines whether we will be compiling
-- info tables that reside just before the entry code, or with an
-- indirection to the entry code.  See TABLES_NEXT_TO_CODE in
-- includes/rts/storage/InfoTables.h.
mkTablesNextToCode :: Bool -> Bool
mkTablesNextToCode unregisterised
    = not unregisterised && cGhcEnableTablesNextToCode == "YES"

data DynLibLoader
  = Deployable
  | SystemDependent
  deriving Eq

data RtsOptsEnabled = RtsOptsNone | RtsOptsSafeOnly | RtsOptsAll
  deriving (Show)

-----------------------------------------------------------------------------
-- Ways

-- The central concept of a "way" is that all objects in a given
-- program must be compiled in the same "way".  Certain options change
-- parameters of the virtual machine, eg. profiling adds an extra word
-- to the object header, so profiling objects cannot be linked with
-- non-profiling objects.

-- After parsing the command-line options, we determine which "way" we
-- are building - this might be a combination way, eg. profiling+threaded.

-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.

data Way
  = WayThreaded
  | WayDebug
  | WayProf
  | WayEventLog
  | WayPar
  | WayGran
  | WayNDP
  | WayDyn
  deriving (Eq, Ord, Show)

allowed_combination :: [Way] -> Bool
allowed_combination way = and [ x `allowedWith` y
                              | x <- way, y <- way, x < y ]
  where
        -- Note ordering in these tests: the left argument is
        -- <= the right argument, according to the Ord instance
        -- on Way above.

        -- dyn is allowed with everything
        _ `allowedWith` WayDyn                  = True
        WayDyn `allowedWith` _                  = True

        -- debug is allowed with everything
        _ `allowedWith` WayDebug                = True
        WayDebug `allowedWith` _                = True

        WayProf `allowedWith` WayNDP            = True
        WayThreaded `allowedWith` WayProf       = True
        WayThreaded `allowedWith` WayEventLog   = True
        _ `allowedWith` _                       = False

mkBuildTag :: [Way] -> String
mkBuildTag ways = concat (intersperse "_" (map wayTag ways))

wayTag :: Way -> String
wayTag WayThreaded = "thr"
wayTag WayDebug    = "debug"
wayTag WayDyn      = "dyn"
wayTag WayProf     = "p"
wayTag WayEventLog = "l"
wayTag WayPar      = "mp"
wayTag WayGran     = "mg"
wayTag WayNDP      = "ndp"

wayRTSOnly :: Way -> Bool
wayRTSOnly WayThreaded = True
wayRTSOnly WayDebug    = True
wayRTSOnly WayDyn      = False
wayRTSOnly WayProf     = False
wayRTSOnly WayEventLog = True
wayRTSOnly WayPar      = False
wayRTSOnly WayGran     = False
wayRTSOnly WayNDP      = False

wayDesc :: Way -> String
wayDesc WayThreaded = "Threaded"
wayDesc WayDebug    = "Debug"
wayDesc WayDyn      = "Dynamic"
wayDesc WayProf     = "Profiling"
wayDesc WayEventLog = "RTS Event Logging"
wayDesc WayPar      = "Parallel"
wayDesc WayGran     = "GranSim"
wayDesc WayNDP      = "Nested data parallelism"

-- Turn these flags on when enabling this way
wayGeneralFlags :: Platform -> Way -> [GeneralFlag]
wayGeneralFlags _ WayThreaded = []
wayGeneralFlags _ WayDebug    = []
wayGeneralFlags _ WayDyn      = [Opt_PIC]
wayGeneralFlags _ WayProf     = [Opt_SccProfilingOn]
wayGeneralFlags _ WayEventLog = []
wayGeneralFlags _ WayPar      = [Opt_Parallel]
wayGeneralFlags _ WayGran     = [Opt_GranMacros]
wayGeneralFlags _ WayNDP      = []

-- Turn these flags off when enabling this way
wayUnsetGeneralFlags :: Platform -> Way -> [GeneralFlag]
wayUnsetGeneralFlags _ WayThreaded = []
wayUnsetGeneralFlags _ WayDebug    = []
wayUnsetGeneralFlags _ WayDyn      = [-- There's no point splitting objects
                                      -- when we're going to be dynamically
                                      -- linking. Plus it breaks compilation
                                      -- on OSX x86.
                                      Opt_SplitObjs]
wayUnsetGeneralFlags _ WayProf     = []
wayUnsetGeneralFlags _ WayEventLog = []
wayUnsetGeneralFlags _ WayPar      = []
wayUnsetGeneralFlags _ WayGran     = []
wayUnsetGeneralFlags _ WayNDP      = []

wayExtras :: Platform -> Way -> DynFlags -> DynFlags
wayExtras _ WayThreaded dflags = dflags
wayExtras _ WayDebug    dflags = dflags
wayExtras _ WayDyn      dflags = dflags
wayExtras _ WayProf     dflags = dflags
wayExtras _ WayEventLog dflags = dflags
wayExtras _ WayPar      dflags = exposePackage' "concurrent" dflags
wayExtras _ WayGran     dflags = exposePackage' "concurrent" dflags
wayExtras _ WayNDP      dflags = setExtensionFlag' Opt_ParallelArrays
                               $ setGeneralFlag' Opt_Vectorise dflags

wayOptc :: Platform -> Way -> [String]
wayOptc platform WayThreaded = case platformOS platform of
                               OSOpenBSD -> ["-pthread"]
                               OSNetBSD  -> ["-pthread"]
                               _         -> []
wayOptc _ WayDebug      = []
wayOptc _ WayDyn        = []
wayOptc _ WayProf       = ["-DPROFILING"]
wayOptc _ WayEventLog   = ["-DTRACING"]
wayOptc _ WayPar        = ["-DPAR", "-w"]
wayOptc _ WayGran       = ["-DGRAN"]
wayOptc _ WayNDP        = []

wayOptl :: Platform -> Way -> [String]
wayOptl platform WayThreaded =
        case platformOS platform of
        -- FreeBSD's default threading library is the KSE-based M:N libpthread,
        -- which GHC has some problems with.  It's currently not clear whether
        -- the problems are our fault or theirs, but it seems that using the
        -- alternative 1:1 threading library libthr works around it:
        OSFreeBSD  -> ["-lthr"]
        OSSolaris2 -> ["-lrt"]
        OSOpenBSD  -> ["-pthread"]
        OSNetBSD   -> ["-pthread"]
        _          -> []
wayOptl _ WayDebug      = []
wayOptl _ WayDyn        = []
wayOptl _ WayProf       = []
wayOptl _ WayEventLog   = []
wayOptl _ WayPar        = ["-L${PVM_ROOT}/lib/${PVM_ARCH}",
                           "-lpvm3",
                           "-lgpvm3"]
wayOptl _ WayGran       = []
wayOptl _ WayNDP        = []

wayOptP :: Platform -> Way -> [String]
wayOptP _ WayThreaded = []
wayOptP _ WayDebug    = []
wayOptP _ WayDyn      = []
wayOptP _ WayProf     = ["-DPROFILING"]
wayOptP _ WayEventLog = ["-DTRACING"]
wayOptP _ WayPar      = ["-D__PARALLEL_HASKELL__"]
wayOptP _ WayGran     = ["-D__GRANSIM__"]
wayOptP _ WayNDP      = []

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

doDynamicToo :: DynFlags -> DynFlags
doDynamicToo dflags0 = let dflags1 = addWay' WayDyn dflags0
                           dflags2 = dflags1 {
                                         outputFile = dynOutputFile dflags1,
                                         hiSuf = dynHiSuf dflags1,
                                         objectSuf = dynObjectSuf dflags1
                                     }
                           dflags3 = updateWays dflags2
                           dflags4 = gopt_unset dflags3 Opt_BuildDynamicToo
                       in dflags4

-----------------------------------------------------------------------------

-- | Used by 'GHC.newSession' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 refCanGenerateDynamicToo <- newIORef True
 refFilesToClean <- newIORef []
 refDirsToClean <- newIORef Map.empty
 refFilesToNotIntermediateClean <- newIORef []
 refGeneratedDumps <- newIORef Set.empty
 refLlvmVersion <- newIORef 28
 wrapperNum <- newIORef 0
 canUseUnicodeQuotes <- do let enc = localeEncoding
                               str = "‛’"
                           (withCString enc str $ \cstr ->
                                do str' <- peekCString enc cstr
                                   return (str == str'))
                               `catchIOError` \_ -> return False
 return dflags{
        canGenerateDynamicToo = refCanGenerateDynamicToo,
        filesToClean   = refFilesToClean,
        dirsToClean    = refDirsToClean,
        filesToNotIntermediateClean = refFilesToNotIntermediateClean,
        generatedDumps = refGeneratedDumps,
        llvmVersion    = refLlvmVersion,
        nextWrapperNum = wrapperNum,
        useUnicodeQuotes = canUseUnicodeQuotes
        }

-- | The normal 'DynFlags'. Note that they is not suitable for use in this form
-- and must be fully initialized by 'GHC.newSession' first.
defaultDynFlags :: Settings -> DynFlags
defaultDynFlags mySettings =
     DynFlags {
        ghcMode                 = CompManager,
        ghcLink                 = LinkBinary,
        hscTarget               = defaultHscTarget (sTargetPlatform mySettings),
        extCoreName             = "",
        verbosity               = 0,
        optLevel                = 0,
        simplPhases             = 2,
        maxSimplIterations      = 4,
        shouldDumpSimplPhase    = Nothing,
        ruleCheck               = Nothing,
        simplTickFactor         = 100,
        specConstrThreshold     = Just 2000,
        specConstrCount         = Just 3,
        specConstrRecursive     = 3,
        liberateCaseThreshold   = Just 2000,
        floatLamArgs            = Just 0, -- Default: float only if no fvs
        historySize             = 20,
        strictnessBefore        = [],

        cmdlineHcIncludes       = [],
        importPaths             = ["."],
        mainModIs               = mAIN,
        mainFunIs               = Nothing,
        ctxtStkDepth            = mAX_CONTEXT_REDUCTION_DEPTH,

        thisPackage             = mainPackageId,

        objectDir               = Nothing,
        dylibInstallName        = Nothing,
        hiDir                   = Nothing,
        stubDir                 = Nothing,
        dumpDir                 = Nothing,

        objectSuf               = phaseInputExt StopLn,
        hcSuf                   = phaseInputExt HCc,
        hiSuf                   = "hi",

        canGenerateDynamicToo   = panic "defaultDynFlags: No canGenerateDynamicToo",
        dynObjectSuf            = "dyn_" ++ phaseInputExt StopLn,
        dynHiSuf                = "dyn_hi",

        pluginModNames          = [],
        pluginModNameOpts       = [],

        outputFile              = Nothing,
        dynOutputFile           = Nothing,
        outputHi                = Nothing,
        dynLibLoader            = SystemDependent,
        dumpPrefix              = Nothing,
        dumpPrefixForce         = Nothing,
        ldInputs                = [],
        includePaths            = [],
        libraryPaths            = [],
        frameworkPaths          = [],
        cmdlineFrameworks       = [],
        rtsOpts                 = Nothing,
        rtsOptsEnabled          = RtsOptsSafeOnly,

        hpcDir                  = ".hpc",

        extraPkgConfs           = id,
        packageFlags            = [],
        pkgDatabase             = Nothing,
        pkgState                = panic "no package state yet: call GHC.setSessionDynFlags",
        ways                    = defaultWays mySettings,
        buildTag                = mkBuildTag (defaultWays mySettings),
        rtsBuildTag             = mkBuildTag (defaultWays mySettings),
        splitInfo               = Nothing,
        settings                = mySettings,
        -- ghc -M values
        depMakefile       = "Makefile",
        depIncludePkgDeps = False,
        depExcludeMods    = [],
        depSuffixes       = [],
        -- end of ghc -M values
        filesToClean   = panic "defaultDynFlags: No filesToClean",
        dirsToClean    = panic "defaultDynFlags: No dirsToClean",
        filesToNotIntermediateClean = panic "defaultDynFlags: No filesToNotIntermediateClean",
        generatedDumps = panic "defaultDynFlags: No generatedDumps",
        haddockOptions = Nothing,
        dumpFlags = IntSet.empty,
        generalFlags = IntSet.fromList (map fromEnum (defaultFlags mySettings)),
        warningFlags = IntSet.fromList (map fromEnum standardWarnings),
        ghciScripts = [],
        language = Nothing,
        safeHaskell = Sf_SafeInferred,
        thOnLoc = noSrcSpan,
        newDerivOnLoc = noSrcSpan,
        pkgTrustOnLoc = noSrcSpan,
        warnSafeOnLoc = noSrcSpan,
        warnUnsafeOnLoc = noSrcSpan,
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
        -- Be fairly keen to inline a fuction if that means
        -- we'll be able to pick the right method from a dictionary
        ufDictDiscount      = 30,
        ufKeenessFactor     = 1.5,
        ufDearOp            = 40,

        maxWorkerArgs = 10,

        ghciHistSize = 50, -- keep a log of length 50 by default

        log_action = defaultLogAction,
        flushOut = defaultFlushOut,
        flushErr = defaultFlushErr,
        pprUserLength = 5,
        pprCols = 100,
        useUnicodeQuotes = False,
        traceLevel = 1,
        profAuto = NoProfAuto,
        llvmVersion = panic "defaultDynFlags: No llvmVersion",
        interactivePrint = Nothing,
        nextWrapperNum = panic "defaultDynFlags: No nextWrapperNum",
        sseVersion = Nothing
      }

defaultWays :: Settings -> [Way]
defaultWays settings = if pc_DYNAMIC_BY_DEFAULT (sPlatformConstants settings)
                       then [WayDyn]
                       else []

interpWays :: [Way]
interpWays = if cDYNAMIC_GHC_PROGRAMS
             then [WayDyn]
             else []

--------------------------------------------------------------------------

type FatalMessager = String -> IO ()
type LogAction = DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()

defaultFatalMessager :: FatalMessager
defaultFatalMessager = hPutStrLn stderr

defaultLogAction :: LogAction
defaultLogAction dflags severity srcSpan style msg
    = case severity of
      SevOutput -> printSDoc msg style
      SevDump   -> printSDoc (msg $$ blankLine) style
      SevInfo   -> printErrs msg style
      SevFatal  -> printErrs msg style
      _         -> do hPutChar stderr '\n'
                      printErrs (mkLocMessage severity srcSpan msg) style
                      -- careful (#2302): printErrs prints in UTF-8, whereas
                      -- converting to string first and using hPutStr would
                      -- just emit the low 8 bits of each unicode char.
    where printSDoc = defaultLogActionHPrintDoc dflags stdout
          printErrs = defaultLogActionHPrintDoc dflags stderr

defaultLogActionHPrintDoc :: DynFlags -> Handle -> SDoc -> PprStyle -> IO ()
defaultLogActionHPrintDoc dflags h d sty
    = do let doc = runSDoc d (initSDocContext dflags sty)
         Pretty.printDoc Pretty.PageMode (pprCols dflags) h doc
         hFlush h

newtype FlushOut = FlushOut (IO ())

defaultFlushOut :: FlushOut
defaultFlushOut = FlushOut $ hFlush stdout

newtype FlushErr = FlushErr (IO ())

defaultFlushErr :: FlushErr
defaultFlushErr = FlushErr $ hFlush stderr

printOutputForUser :: DynFlags -> PrintUnqualified -> SDoc -> IO ()
printOutputForUser = printSevForUser SevOutput

printInfoForUser :: DynFlags -> PrintUnqualified -> SDoc -> IO ()
printInfoForUser = printSevForUser SevInfo

printSevForUser :: Severity -> DynFlags -> PrintUnqualified -> SDoc -> IO ()
printSevForUser sev dflags unqual doc
    = log_action dflags dflags sev noSrcSpan (mkUserStyle unqual AllTheWay) doc

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

-- OnOffs accumulate in reverse order, so we use foldr in order to
-- process them in the right order
flattenExtensionFlags :: Maybe Language -> [OnOff ExtensionFlag] -> IntSet
flattenExtensionFlags ml = foldr f defaultExtensionFlags
    where f (On f)  flags = IntSet.insert (fromEnum f) flags
          f (Off f) flags = IntSet.delete (fromEnum f) flags
          defaultExtensionFlags = IntSet.fromList (map fromEnum (languageExtensions ml))

languageExtensions :: Maybe Language -> [ExtensionFlag]

languageExtensions Nothing
    -- Nothing => the default case
    = Opt_NondecreasingIndentation -- This has been on by default for some time
    : delete Opt_DatatypeContexts  -- The Haskell' committee decided to
                                   -- remove datatype contexts from the
                                   -- language:
   -- http://www.haskell.org/pipermail/haskell-prime/2011-January/003335.html
      (languageExtensions (Just Haskell2010))

   -- NB: MonoPatBinds is no longer the default

languageExtensions (Just Haskell98)
    = [Opt_ImplicitPrelude,
       Opt_MonomorphismRestriction,
       Opt_NPlusKPatterns,
       Opt_DatatypeContexts,
       Opt_TraditionalRecordSyntax,
       Opt_NondecreasingIndentation
           -- strictly speaking non-standard, but we always had this
           -- on implicitly before the option was added in 7.1, and
           -- turning it off breaks code, so we're keeping it on for
           -- backwards compatibility.  Cabal uses -XHaskell98 by
           -- default unless you specify another language.
      ]

languageExtensions (Just Haskell2010)
    = [Opt_ImplicitPrelude,
       Opt_MonomorphismRestriction,
       Opt_DatatypeContexts,
       Opt_TraditionalRecordSyntax,
       Opt_EmptyDataDecls,
       Opt_ForeignFunctionInterface,
       Opt_PatternGuards,
       Opt_DoAndIfThenElse,
       Opt_RelaxedPolyRec]

-- | Test whether a 'DumpFlag' is set
dopt :: DumpFlag -> DynFlags -> Bool
dopt f dflags = (fromEnum f `IntSet.member` dumpFlags dflags)
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
          enableIfVerbose _                                 = True

-- | Set a 'DumpFlag'
dopt_set :: DynFlags -> DumpFlag -> DynFlags
dopt_set dfs f = dfs{ dumpFlags = IntSet.insert (fromEnum f) (dumpFlags dfs) }

-- | Unset a 'DumpFlag'
dopt_unset :: DynFlags -> DumpFlag -> DynFlags
dopt_unset dfs f = dfs{ dumpFlags = IntSet.delete (fromEnum f) (dumpFlags dfs) }

-- | Test whether a 'GeneralFlag' is set
gopt :: GeneralFlag -> DynFlags -> Bool
gopt f dflags  = fromEnum f `IntSet.member` generalFlags dflags

-- | Set a 'GeneralFlag'
gopt_set :: DynFlags -> GeneralFlag -> DynFlags
gopt_set dfs f = dfs{ generalFlags = IntSet.insert (fromEnum f) (generalFlags dfs) }

-- | Unset a 'GeneralFlag'
gopt_unset :: DynFlags -> GeneralFlag -> DynFlags
gopt_unset dfs f = dfs{ generalFlags = IntSet.delete (fromEnum f) (generalFlags dfs) }

-- | Test whether a 'WarningFlag' is set
wopt :: WarningFlag -> DynFlags -> Bool
wopt f dflags  = fromEnum f `IntSet.member` warningFlags dflags

-- | Set a 'WarningFlag'
wopt_set :: DynFlags -> WarningFlag -> DynFlags
wopt_set dfs f = dfs{ warningFlags = IntSet.insert (fromEnum f) (warningFlags dfs) }

-- | Unset a 'WarningFlag'
wopt_unset :: DynFlags -> WarningFlag -> DynFlags
wopt_unset dfs f = dfs{ warningFlags = IntSet.delete (fromEnum f) (warningFlags dfs) }

-- | Test whether a 'ExtensionFlag' is set
xopt :: ExtensionFlag -> DynFlags -> Bool
xopt f dflags = fromEnum f `IntSet.member` extensionFlags dflags

-- | Set a 'ExtensionFlag'
xopt_set :: DynFlags -> ExtensionFlag -> DynFlags
xopt_set dfs f
    = let onoffs = On f : extensions dfs
      in dfs { extensions = onoffs,
               extensionFlags = flattenExtensionFlags (language dfs) onoffs }

-- | Unset a 'ExtensionFlag'
xopt_unset :: DynFlags -> ExtensionFlag -> DynFlags
xopt_unset dfs f
    = let onoffs = Off f : extensions dfs
      in dfs { extensions = onoffs,
               extensionFlags = flattenExtensionFlags (language dfs) onoffs }

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
safeHaskellOn dflags = safeHaskell dflags /= Sf_None

-- | Is the Safe Haskell safe language in use
safeLanguageOn :: DynFlags -> Bool
safeLanguageOn dflags = safeHaskell dflags == Sf_Safe

-- | Is the Safe Haskell safe inference mode active
safeInferOn :: DynFlags -> Bool
safeInferOn dflags = safeHaskell dflags == Sf_SafeInferred

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
              return $ dfs { safeHaskell = safeM }

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
combineSafeFlags a b | a == Sf_SafeInferred = return b
                     | b == Sf_SafeInferred = return a
                     | a == Sf_None         = return b
                     | b == Sf_None         = return a
                     | a == b               = return a
                     | otherwise            = addErr errm >> return (panic errm)
    where errm = "Incompatible Safe Haskell flags! ("
                    ++ show a ++ ", " ++ show b ++ ")"

-- | A list of unsafe flags under Safe Haskell. Tuple elements are:
--     * name of the flag
--     * function to get srcspan that enabled the flag
--     * function to test if the flag is on
--     * function to turn the flag off
unsafeFlags :: [(String, DynFlags -> SrcSpan, DynFlags -> Bool, DynFlags -> DynFlags)]
unsafeFlags = [("-XGeneralizedNewtypeDeriving", newDerivOnLoc,
                   xopt Opt_GeneralizedNewtypeDeriving,
                   flip xopt_unset Opt_GeneralizedNewtypeDeriving),
               ("-XTemplateHaskell", thOnLoc,
                   xopt Opt_TemplateHaskell,
                   flip xopt_unset Opt_TemplateHaskell)]

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

setObjectDir, setHiDir, setStubDir, setDumpDir, setOutputDir,
         setDynObjectSuf, setDynHiSuf,
         setDylibInstallName,
         setObjectSuf, setHiSuf, setHcSuf, parseDynLibLoaderMode,
         setPgmP, addOptl, addOptc, addOptP,
         addCmdlineFramework, addHaddockOpts, addGhciScript, 
         setInteractivePrint
   :: String -> DynFlags -> DynFlags
setOutputFile, setDynOutputFile, setOutputHi, setDumpPrefixForce
   :: Maybe String -> DynFlags -> DynFlags

setObjectDir  f d = d{ objectDir  = Just f}
setHiDir      f d = d{ hiDir      = Just f}
setStubDir    f d = d{ stubDir    = Just f, includePaths = f : includePaths d }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling via C (i.e. unregisterised
  -- builds).
setDumpDir    f d = d{ dumpDir    = Just f}
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f . setDumpDir f
setDylibInstallName  f d = d{ dylibInstallName = Just f}

setObjectSuf    f d = d{ objectSuf    = f}
setDynObjectSuf f d = d{ dynObjectSuf = f}
setHiSuf        f d = d{ hiSuf        = f}
setDynHiSuf     f d = d{ dynHiSuf     = f}
setHcSuf        f d = d{ hcSuf        = f}

setOutputFile f d = d{ outputFile = f}
setDynOutputFile f d = d{ dynOutputFile = f}
setOutputHi   f d = d{ outputHi   = f}

addPluginModuleName :: String -> DynFlags -> DynFlags
addPluginModuleName name d = d { pluginModNames = (mkModuleName name) : (pluginModNames d) }

addPluginModuleNameOption :: String -> DynFlags -> DynFlags
addPluginModuleNameOption optflag d = d { pluginModNameOpts = (mkModuleName m, option) : (pluginModNameOpts d) }
  where (m, rest) = break (== ':') optflag
        option = case rest of
          [] -> "" -- should probably signal an error
          (_:plug_opt) -> plug_opt -- ignore the ':' from break

parseDynLibLoaderMode f d =
 case splitAt 8 f of
   ("deploy", "")       -> d{ dynLibLoader = Deployable }
   ("sysdep", "")       -> d{ dynLibLoader = SystemDependent }
   _                    -> throwGhcException (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f = let (pgm:args) = words f in alterSettings (\s -> s { sPgm_P   = (pgm, map Option args)})
addOptl   f = alterSettings (\s -> s { sOpt_l   = f : sOpt_l s})
addOptc   f = alterSettings (\s -> s { sOpt_c   = f : sOpt_c s})
addOptP   f = alterSettings (\s -> s { sOpt_P   = f : sOpt_P s})


setDepMakefile :: FilePath -> DynFlags -> DynFlags
setDepMakefile f d = d { depMakefile = deOptDep f }

setDepIncludePkgDeps :: Bool -> DynFlags -> DynFlags
setDepIncludePkgDeps b d = d { depIncludePkgDeps = b }

addDepExcludeMod :: String -> DynFlags -> DynFlags
addDepExcludeMod m d
    = d { depExcludeMods = mkModuleName (deOptDep m) : depExcludeMods d }

addDepSuffix :: FilePath -> DynFlags -> DynFlags
addDepSuffix s d = d { depSuffixes = deOptDep s : depSuffixes d }

-- XXX Legacy code:
-- We used to use "-optdep-flag -optdeparg", so for legacy applications
-- we need to strip the "-optdep" off of the arg
deOptDep :: String -> String
deOptDep x = case stripPrefix "-optdep" x of
             Just rest -> rest
             Nothing -> x

addCmdlineFramework f d = d{ cmdlineFrameworks = f : cmdlineFrameworks d}

addHaddockOpts f d = d{ haddockOptions = Just f}

addGhciScript f d = d{ ghciScripts = f : ghciScripts d}

setInteractivePrint f d = d{ interactivePrint = Just f}

-- -----------------------------------------------------------------------------
-- Command-line options

-- | When invoking external tools as part of the compilation pipeline, we
-- pass these a sequence of options on the command-line. Rather than
-- just using a list of Strings, we use a type that allows us to distinguish
-- between filepaths and 'other stuff'. The reason for this is that
-- this type gives us a handle on transforming filenames, and filenames only,
-- to whatever format they're expected to be on a particular platform.
data Option
 = FileOption -- an entry that _contains_ filename(s) / filepaths.
              String  -- a non-filepath prefix that shouldn't be
                      -- transformed (e.g., "/out=")
              String  -- the filepath/filename portion
 | Option     String
 deriving ( Eq )

showOpt :: Option -> String
showOpt (FileOption pre f) = pre ++ f
showOpt (Option s)  = s

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

-- -----------------------------------------------------------------------------
-- StgToDo:  abstraction of stg-to-stg passes to run.

data StgToDo
  = StgDoMassageForProfiling  -- should be (next to) last
  -- There's also setStgVarInfo, but its absolute "lastness"
  -- is so critical that it is hardwired in (no flag).
  | D_stg_stats

getStgToDo :: DynFlags -> [StgToDo]
getStgToDo dflags
  = todo2
  where
        stg_stats = gopt Opt_StgStats dflags

        todo1 = if stg_stats then [D_stg_stats] else []

        todo2 | WayProf `elem` ways dflags
              = StgDoMassageForProfiling : todo1
              | otherwise
              = todo1

{- **********************************************************************
%*                                                                      *
                DynFlags parser
%*                                                                      *
%********************************************************************* -}

-- -----------------------------------------------------------------------------
-- Parsing the dynamic flags.


-- | Parse dynamic flags from a list of command line arguments.  Returns the
-- the parsed 'DynFlags', the left-over arguments, and a list of warnings.
-- Throws a 'UsageError' if errors occurred during parsing (such as unknown
-- flags or missing arguments).
parseDynamicFlagsCmdLine :: MonadIO m => DynFlags -> [Located String]
                         -> m (DynFlags, [Located String], [Located String])
                            -- ^ Updated 'DynFlags', left-over arguments, and
                            -- list of warnings.
parseDynamicFlagsCmdLine = parseDynamicFlagsFull flagsAll True


-- | Like 'parseDynamicFlagsCmdLine' but does not allow the package flags
-- (-package, -hide-package, -ignore-package, -hide-all-packages, -package-db).
-- Used to parse flags set in a modules pragma.
parseDynamicFilePragma :: MonadIO m => DynFlags -> [Located String]
                       -> m (DynFlags, [Located String], [Located String])
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
                  -> m (DynFlags, [Located String], [Located String])
parseDynamicFlagsFull activeFlags cmdline dflags0 args = do
  -- XXX Legacy support code
  -- We used to accept things like
  --     optdep-f  -optdepdepend
  --     optdep-f  -optdep depend
  --     optdep -f -optdepdepend
  --     optdep -f -optdep depend
  -- but the spaces trip up proper argument handling. So get rid of them.
  let f (L p "-optdep" : L _ x : xs) = (L p ("-optdep" ++ x)) : f xs
      f (x : xs) = x : f xs
      f xs = xs
      args' = f args

  let ((leftover, errs, warns), dflags1)
          = runCmdLine (processArgs activeFlags args') dflags0
  when (not (null errs)) $ liftIO $
      throwGhcExceptionIO $ errorsToGhcException errs

  -- check for disabled flags in safe haskell
  let (dflags2, sh_warns) = safeFlagCheck cmdline dflags1
      dflags3 = updateWays dflags2
      theWays = ways dflags3

  unless (allowed_combination theWays) $ liftIO $
      throwGhcExceptionIO (CmdLineError ("combination not supported: " ++
                               intercalate "/" (map wayDesc theWays)))

  whenGeneratingDynamicToo dflags3 $
      unless (isJust (outputFile dflags3) == isJust (dynOutputFile dflags3)) $
          liftIO $ throwGhcExceptionIO $ CmdLineError
              "With -dynamic-too, must give -dyno iff giving -o"

  let (dflags4, consistency_warnings) = makeDynFlagsConsistent dflags3

  liftIO $ setUnsafeGlobalDynFlags dflags4

  return (dflags4, leftover, consistency_warnings ++ sh_warns ++ warns)

updateWays :: DynFlags -> DynFlags
updateWays dflags
    = let theWays = sort $ nub $ ways dflags
          f = if WayDyn `elem` theWays then unSetGeneralFlag'
                                       else setGeneralFlag'
      in f Opt_Static
       $ dflags {
             ways        = theWays,
             buildTag    = mkBuildTag (filter (not . wayRTSOnly) theWays),
             rtsBuildTag = mkBuildTag                            theWays
         }

-- | Check (and potentially disable) any extensions that aren't allowed
-- in safe mode.
--
-- The bool is to indicate if we are parsing command line flags (false means
-- file pragma). This allows us to generate better warnings.
safeFlagCheck :: Bool -> DynFlags -> (DynFlags, [Located String])
safeFlagCheck _  dflags | not (safeLanguageOn dflags || safeInferOn dflags)
                        = (dflags, [])

-- safe or safe-infer ON
safeFlagCheck cmdl dflags =
    case safeLanguageOn dflags of
        True -> (dflags', warns)

        -- throw error if -fpackage-trust by itself with no safe haskell flag
        False | not cmdl && packageTrustOn dflags
              -> (gopt_unset dflags' Opt_PackageTrust,
                  [L (pkgTrustOnLoc dflags') $
                      "-fpackage-trust ignored;" ++
                      " must be specified with a Safe Haskell flag"]
                  )

        False | null warns && safeInfOk
              -> (dflags', [])

              | otherwise
              -> (dflags' { safeHaskell = Sf_None }, [])
                -- Have we inferred Unsafe?
                -- See Note [HscMain . Safe Haskell Inference]
    where
        -- TODO: Can we do better than this for inference?
        safeInfOk = not $ xopt Opt_OverlappingInstances dflags

        (dflags', warns) = foldl check_method (dflags, []) unsafeFlags

        check_method (df, warns) (str,loc,test,fix)
            | test df   = (apFix fix df, warns ++ safeFailure (loc dflags) str)
            | otherwise = (df, warns)

        apFix f = if safeInferOn dflags then id else f

        safeFailure loc str 
           = [L loc $ str ++ " is not allowed in Safe Haskell; ignoring " ++ str]

{- **********************************************************************
%*                                                                      *
                DynFlags specifications
%*                                                                      *
%********************************************************************* -}

-- | All dynamic flags option strings. These are the user facing strings for
-- enabling and disabling options.
allFlags :: [String]
allFlags = map ('-':) $
           [ flagName flag | flag <- dynamic_flags ++ package_flags, ok (flagOptKind flag) ] ++
           map ("fno-"++) fflags ++
           map ("f"++) fflags ++
           map ("X"++) supportedExtensions
    where ok (PrefixPred _ _) = False
          ok _   = True
          fflags = fflags0 ++ fflags1 ++ fflags2
          fflags0 = [ name | (name, _, _) <- fFlags ]
          fflags1 = [ name | (name, _, _) <- fWarningFlags ]
          fflags2 = [ name | (name, _, _) <- fLangFlags ]

{-
 - Below we export user facing symbols for GHC dynamic flags for use with the
 - GHC API.
 -}

-- All dynamic flags present in GHC.
flagsAll :: [Flag (CmdLineP DynFlags)]
flagsAll     = package_flags ++ dynamic_flags

-- All dynamic flags, minus package flags, present in GHC.
flagsDynamic :: [Flag (CmdLineP DynFlags)]
flagsDynamic = dynamic_flags

-- ALl package flags present in GHC.
flagsPackage :: [Flag (CmdLineP DynFlags)]
flagsPackage = package_flags

--------------- The main flags themselves ------------------
dynamic_flags :: [Flag (CmdLineP DynFlags)]
dynamic_flags = [
    Flag "n"        (NoArg (addWarn "The -n flag is deprecated and no longer has any effect"))
  , Flag "cpp"      (NoArg (setExtensionFlag Opt_Cpp))
  , Flag "F"        (NoArg (setGeneralFlag Opt_Pp))
  , Flag "#include"
         (HasArg (\s -> do addCmdlineHCInclude s
                           addWarn "-#include and INCLUDE pragmas are deprecated: They no longer have any effect"))
  , Flag "v"        (OptIntSuffix setVerbosity)

        ------- ways --------------------------------------------------------
  , Flag "prof"           (NoArg (addWay WayProf))
  , Flag "eventlog"       (NoArg (addWay WayEventLog))
  , Flag "parallel"       (NoArg (addWay WayPar))
  , Flag "gransim"        (NoArg (addWay WayGran))
  , Flag "smp"            (NoArg (addWay WayThreaded >> deprecate "Use -threaded instead"))
  , Flag "debug"          (NoArg (addWay WayDebug))
  , Flag "ndp"            (NoArg (addWay WayNDP))
  , Flag "threaded"       (NoArg (addWay WayThreaded))

  , Flag "ticky"          (NoArg (setGeneralFlag Opt_Ticky >> addWay WayDebug))

    -- -ticky enables ticky-ticky code generation, and also implies -debug which
    -- is required to get the RTS ticky support.

        ----- Linker --------------------------------------------------------
  , Flag "static"         (NoArg removeWayDyn)
  , Flag "dynamic"        (NoArg (addWay WayDyn))
    -- ignored for compat w/ gcc:
  , Flag "rdynamic"       (NoArg (return ()))
  , Flag "relative-dynlib-paths"  (NoArg (setGeneralFlag Opt_RelativeDynlibPaths))

        ------- Specific phases  --------------------------------------------
    -- need to appear before -pgmL to be parsed as LLVM flags.
  , Flag "pgmlo"          (hasArg (\f -> alterSettings (\s -> s { sPgm_lo  = (f,[])})))
  , Flag "pgmlc"          (hasArg (\f -> alterSettings (\s -> s { sPgm_lc  = (f,[])})))
  , Flag "pgmL"           (hasArg (\f -> alterSettings (\s -> s { sPgm_L   = f})))
  , Flag "pgmP"           (hasArg setPgmP)
  , Flag "pgmF"           (hasArg (\f -> alterSettings (\s -> s { sPgm_F   = f})))
  , Flag "pgmc"           (hasArg (\f -> alterSettings (\s -> s { sPgm_c   = (f,[])})))
  , Flag "pgmm"           (HasArg (\_ -> addWarn "The -pgmm flag does nothing; it will be removed in a future GHC release"))
  , Flag "pgms"           (hasArg (\f -> alterSettings (\s -> s { sPgm_s   = (f,[])})))
  , Flag "pgma"           (hasArg (\f -> alterSettings (\s -> s { sPgm_a   = (f,[])})))
  , Flag "pgml"           (hasArg (\f -> alterSettings (\s -> s { sPgm_l   = (f,[])})))
  , Flag "pgmdll"         (hasArg (\f -> alterSettings (\s -> s { sPgm_dll = (f,[])})))
  , Flag "pgmwindres"     (hasArg (\f -> alterSettings (\s -> s { sPgm_windres = f})))

    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , Flag "optlo"          (hasArg (\f -> alterSettings (\s -> s { sOpt_lo  = f : sOpt_lo s})))
  , Flag "optlc"          (hasArg (\f -> alterSettings (\s -> s { sOpt_lc  = f : sOpt_lc s})))
  , Flag "optL"           (hasArg (\f -> alterSettings (\s -> s { sOpt_L   = f : sOpt_L s})))
  , Flag "optP"           (hasArg addOptP)
  , Flag "optF"           (hasArg (\f -> alterSettings (\s -> s { sOpt_F   = f : sOpt_F s})))
  , Flag "optc"           (hasArg addOptc)
  , Flag "optm"           (HasArg (\_ -> addWarn "The -optm flag does nothing; it will be removed in a future GHC release"))
  , Flag "opta"           (hasArg (\f -> alterSettings (\s -> s { sOpt_a   = f : sOpt_a s})))
  , Flag "optl"           (hasArg addOptl)
  , Flag "optwindres"     (hasArg (\f -> alterSettings (\s -> s { sOpt_windres = f : sOpt_windres s})))

  , Flag "split-objs"
         (NoArg (if can_split
                 then setGeneralFlag Opt_SplitObjs
                 else addWarn "ignoring -fsplit-objs"))

        -------- ghc -M -----------------------------------------------------
  , Flag "dep-suffix"     (hasArg addDepSuffix)
  , Flag "optdep-s"       (hasArgDF addDepSuffix "Use -dep-suffix instead")
  , Flag "dep-makefile"   (hasArg setDepMakefile)
  , Flag "optdep-f"       (hasArgDF setDepMakefile "Use -dep-makefile instead")
  , Flag "optdep-w"       (NoArg  (deprecate "doesn't do anything"))
  , Flag "include-pkg-deps"         (noArg (setDepIncludePkgDeps True))
  , Flag "optdep--include-prelude"  (noArgDF (setDepIncludePkgDeps True) "Use -include-pkg-deps instead")
  , Flag "optdep--include-pkg-deps" (noArgDF (setDepIncludePkgDeps True) "Use -include-pkg-deps instead")
  , Flag "exclude-module"           (hasArg addDepExcludeMod)
  , Flag "optdep--exclude-module"   (hasArgDF addDepExcludeMod "Use -exclude-module instead")
  , Flag "optdep-x"                 (hasArgDF addDepExcludeMod "Use -exclude-module instead")

        -------- Linking ----------------------------------------------------
  , Flag "no-link"            (noArg (\d -> d{ ghcLink=NoLink }))
  , Flag "shared"             (noArg (\d -> d{ ghcLink=LinkDynLib }))
  , Flag "dynload"            (hasArg parseDynLibLoaderMode)
  , Flag "dylib-install-name" (hasArg setDylibInstallName)

        ------- Libraries ---------------------------------------------------
  , Flag "L"   (Prefix addLibraryPath)
  , Flag "l"   (hasArg (addOptl . ("-l" ++)))

        ------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  , Flag "framework-path" (HasArg addFrameworkPath)
  , Flag "framework"      (hasArg addCmdlineFramework)

        ------- Output Redirection ------------------------------------------
  , Flag "odir"              (hasArg setObjectDir)
  , Flag "o"                 (sepArg (setOutputFile . Just))
  , Flag "dyno"              (sepArg (setDynOutputFile . Just))
  , Flag "ohi"               (hasArg (setOutputHi . Just ))
  , Flag "osuf"              (hasArg setObjectSuf)
  , Flag "dynosuf"           (hasArg setDynObjectSuf)
  , Flag "hcsuf"             (hasArg setHcSuf)
  , Flag "hisuf"             (hasArg setHiSuf)
  , Flag "dynhisuf"          (hasArg setDynHiSuf)
  , Flag "hidir"             (hasArg setHiDir)
  , Flag "tmpdir"            (hasArg setTmpDir)
  , Flag "stubdir"           (hasArg setStubDir)
  , Flag "dumpdir"           (hasArg setDumpDir)
  , Flag "outputdir"         (hasArg setOutputDir)
  , Flag "ddump-file-prefix" (hasArg (setDumpPrefixForce . Just))

  , Flag "dynamic-too"       (NoArg (setGeneralFlag Opt_BuildDynamicToo))

        ------- Keeping temporary files -------------------------------------
     -- These can be singular (think ghc -c) or plural (think ghc --make)
  , Flag "keep-hc-file"     (NoArg (setGeneralFlag Opt_KeepHcFiles))
  , Flag "keep-hc-files"    (NoArg (setGeneralFlag Opt_KeepHcFiles))
  , Flag "keep-s-file"      (NoArg (setGeneralFlag Opt_KeepSFiles))
  , Flag "keep-s-files"     (NoArg (setGeneralFlag Opt_KeepSFiles))
  , Flag "keep-raw-s-file"  (NoArg (addWarn "The -keep-raw-s-file flag does nothing; it will be removed in a future GHC release"))
  , Flag "keep-raw-s-files" (NoArg (addWarn "The -keep-raw-s-files flag does nothing; it will be removed in a future GHC release"))
  , Flag "keep-llvm-file"   (NoArg (do setObjTarget HscLlvm
                                       setGeneralFlag Opt_KeepLlvmFiles))
  , Flag "keep-llvm-files"  (NoArg (do setObjTarget HscLlvm
                                       setGeneralFlag Opt_KeepLlvmFiles))
     -- This only makes sense as plural
  , Flag "keep-tmp-files"   (NoArg (setGeneralFlag Opt_KeepTmpFiles))

        ------- Miscellaneous ----------------------------------------------
  , Flag "no-auto-link-packages" (NoArg (unSetGeneralFlag Opt_AutoLinkPackages))
  , Flag "no-hs-main"     (NoArg (setGeneralFlag Opt_NoHsMain))
  , Flag "with-rtsopts"   (HasArg setRtsOpts)
  , Flag "rtsopts"        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , Flag "rtsopts=all"    (NoArg (setRtsOptsEnabled RtsOptsAll))
  , Flag "rtsopts=some"   (NoArg (setRtsOptsEnabled RtsOptsSafeOnly))
  , Flag "rtsopts=none"   (NoArg (setRtsOptsEnabled RtsOptsNone))
  , Flag "no-rtsopts"     (NoArg (setRtsOptsEnabled RtsOptsNone))
  , Flag "main-is"        (SepArg setMainIs)
  , Flag "haddock"        (NoArg (setGeneralFlag Opt_Haddock))
  , Flag "haddock-opts"   (hasArg addHaddockOpts)
  , Flag "hpcdir"         (SepArg setOptHpcDir)
  , Flag "ghci-script"    (hasArg addGhciScript)
  , Flag "interactive-print" (hasArg setInteractivePrint)
  , Flag "ticky-allocd"      (NoArg (setGeneralFlag Opt_Ticky_Allocd))
  , Flag "ticky-LNE"         (NoArg (setGeneralFlag Opt_Ticky_LNE))
  , Flag "ticky-dyn-thunk"   (NoArg (setGeneralFlag Opt_Ticky_Dyn_Thunk))
        ------- recompilation checker --------------------------------------
  , Flag "recomp"         (NoArg (do unSetGeneralFlag Opt_ForceRecomp
                                     deprecate "Use -fno-force-recomp instead"))
  , Flag "no-recomp"      (NoArg (do setGeneralFlag Opt_ForceRecomp
                                     deprecate "Use -fforce-recomp instead"))

        ------ HsCpp opts ---------------------------------------------------
  , Flag "D"              (AnySuffix (upd . addOptP))
  , Flag "U"              (AnySuffix (upd . addOptP))

        ------- Include/Import Paths ----------------------------------------
  , Flag "I"              (Prefix    addIncludePath)
  , Flag "i"              (OptPrefix addImportPath)

        ------ Output style options -----------------------------------------
  , Flag "dppr-user-length" (intSuffix (\n d -> d{ pprUserLength = n }))
  , Flag "dppr-cols"        (intSuffix (\n d -> d{ pprCols = n }))
  , Flag "dtrace-level"     (intSuffix (\n d -> d{ traceLevel = n }))
  -- Suppress all that is suppressable in core dumps.
  -- Except for uniques, as some simplifier phases introduce new varibles that
  -- have otherwise identical names.
  , Flag "dsuppress-all"    (NoArg $ do setGeneralFlag Opt_SuppressCoercions
                                        setGeneralFlag Opt_SuppressVarKinds
                                        setGeneralFlag Opt_SuppressModulePrefixes
                                        setGeneralFlag Opt_SuppressTypeApplications
                                        setGeneralFlag Opt_SuppressIdInfo
                                        setGeneralFlag Opt_SuppressTypeSignatures)

        ------ Debugging ----------------------------------------------------
  , Flag "dstg-stats"     (NoArg (setGeneralFlag Opt_StgStats))

  , Flag "ddump-cmm"               (setDumpFlag Opt_D_dump_cmm)
  , Flag "ddump-cmm-raw"           (setDumpFlag Opt_D_dump_cmm_raw)
  , Flag "ddump-cmm-cfg"           (setDumpFlag Opt_D_dump_cmm_cfg)
  , Flag "ddump-cmm-cbe"           (setDumpFlag Opt_D_dump_cmm_cbe)
  , Flag "ddump-cmm-proc"          (setDumpFlag Opt_D_dump_cmm_proc)
  , Flag "ddump-cmm-rewrite"       (setDumpFlag Opt_D_dump_cmm_rewrite)
  , Flag "ddump-cmm-sp"            (setDumpFlag Opt_D_dump_cmm_sp)
  , Flag "ddump-cmm-procmap"       (setDumpFlag Opt_D_dump_cmm_procmap)
  , Flag "ddump-cmm-split"         (setDumpFlag Opt_D_dump_cmm_split)
  , Flag "ddump-cmm-info"          (setDumpFlag Opt_D_dump_cmm_info)
  , Flag "ddump-cmm-cps"           (setDumpFlag Opt_D_dump_cmm_cps)
  , Flag "ddump-core-stats"        (setDumpFlag Opt_D_dump_core_stats)
  , Flag "ddump-asm"               (setDumpFlag Opt_D_dump_asm)
  , Flag "ddump-asm-native"        (setDumpFlag Opt_D_dump_asm_native)
  , Flag "ddump-asm-liveness"      (setDumpFlag Opt_D_dump_asm_liveness)
  , Flag "ddump-asm-regalloc"      (setDumpFlag Opt_D_dump_asm_regalloc)
  , Flag "ddump-asm-conflicts"     (setDumpFlag Opt_D_dump_asm_conflicts)
  , Flag "ddump-asm-regalloc-stages" (setDumpFlag Opt_D_dump_asm_regalloc_stages)
  , Flag "ddump-asm-stats"         (setDumpFlag Opt_D_dump_asm_stats)
  , Flag "ddump-asm-expanded"      (setDumpFlag Opt_D_dump_asm_expanded)
  , Flag "ddump-llvm"              (NoArg (do setObjTarget HscLlvm
                                              setDumpFlag' Opt_D_dump_llvm))
  , Flag "ddump-deriv"             (setDumpFlag Opt_D_dump_deriv)
  , Flag "ddump-ds"                (setDumpFlag Opt_D_dump_ds)
  , Flag "ddump-foreign"           (setDumpFlag Opt_D_dump_foreign)
  , Flag "ddump-inlinings"         (setDumpFlag Opt_D_dump_inlinings)
  , Flag "ddump-rule-firings"      (setDumpFlag Opt_D_dump_rule_firings)
  , Flag "ddump-rule-rewrites"     (setDumpFlag Opt_D_dump_rule_rewrites)
  , Flag "ddump-simpl-trace"       (setDumpFlag Opt_D_dump_simpl_trace)
  , Flag "ddump-occur-anal"        (setDumpFlag Opt_D_dump_occur_anal)
  , Flag "ddump-parsed"            (setDumpFlag Opt_D_dump_parsed)
  , Flag "ddump-rn"                (setDumpFlag Opt_D_dump_rn)
  , Flag "ddump-core-pipeline"     (setDumpFlag Opt_D_dump_core_pipeline)
  , Flag "ddump-simpl"             (setDumpFlag Opt_D_dump_simpl)
  , Flag "ddump-simpl-iterations"  (setDumpFlag Opt_D_dump_simpl_iterations)
  , Flag "ddump-simpl-phases"      (OptPrefix setDumpSimplPhases)
  , Flag "ddump-spec"              (setDumpFlag Opt_D_dump_spec)
  , Flag "ddump-prep"              (setDumpFlag Opt_D_dump_prep)
  , Flag "ddump-stg"               (setDumpFlag Opt_D_dump_stg)
  , Flag "ddump-stranal"           (setDumpFlag Opt_D_dump_stranal)
  , Flag "ddump-tc"                (setDumpFlag Opt_D_dump_tc)
  , Flag "ddump-types"             (setDumpFlag Opt_D_dump_types)
  , Flag "ddump-rules"             (setDumpFlag Opt_D_dump_rules)
  , Flag "ddump-cse"               (setDumpFlag Opt_D_dump_cse)
  , Flag "ddump-worker-wrapper"    (setDumpFlag Opt_D_dump_worker_wrapper)
  , Flag "ddump-rn-trace"          (setDumpFlag Opt_D_dump_rn_trace)
  , Flag "ddump-if-trace"          (setDumpFlag Opt_D_dump_if_trace)
  , Flag "ddump-cs-trace"          (setDumpFlag Opt_D_dump_cs_trace)
  , Flag "ddump-tc-trace"          (setDumpFlag Opt_D_dump_tc_trace)
  , Flag "ddump-vt-trace"          (setDumpFlag Opt_D_dump_vt_trace)
  , Flag "ddump-splices"           (setDumpFlag Opt_D_dump_splices)
  , Flag "ddump-rn-stats"          (setDumpFlag Opt_D_dump_rn_stats)
  , Flag "ddump-opt-cmm"           (setDumpFlag Opt_D_dump_opt_cmm)
  , Flag "ddump-simpl-stats"       (setDumpFlag Opt_D_dump_simpl_stats)
  , Flag "ddump-bcos"              (setDumpFlag Opt_D_dump_BCOs)
  , Flag "dsource-stats"           (setDumpFlag Opt_D_source_stats)
  , Flag "dverbose-core2core"      (NoArg (do setVerbosity (Just 2)
                                              setVerboseCore2Core))
  , Flag "dverbose-stg2stg"        (setDumpFlag Opt_D_verbose_stg2stg)
  , Flag "ddump-hi"                (setDumpFlag Opt_D_dump_hi)
  , Flag "ddump-minimal-imports"   (NoArg (setGeneralFlag Opt_D_dump_minimal_imports))
  , Flag "ddump-vect"              (setDumpFlag Opt_D_dump_vect)
  , Flag "ddump-hpc"               (setDumpFlag Opt_D_dump_ticked) -- back compat
  , Flag "ddump-ticked"            (setDumpFlag Opt_D_dump_ticked)
  , Flag "ddump-mod-cycles"        (setDumpFlag Opt_D_dump_mod_cycles)
  , Flag "ddump-view-pattern-commoning" (setDumpFlag Opt_D_dump_view_pattern_commoning)
  , Flag "ddump-to-file"           (NoArg (setGeneralFlag Opt_DumpToFile))
  , Flag "ddump-hi-diffs"          (setDumpFlag Opt_D_dump_hi_diffs)
  , Flag "ddump-rtti"              (setDumpFlag Opt_D_dump_rtti)
  , Flag "dcore-lint"              (NoArg (setGeneralFlag Opt_DoCoreLinting))
  , Flag "dstg-lint"               (NoArg (setGeneralFlag Opt_DoStgLinting))
  , Flag "dcmm-lint"               (NoArg (setGeneralFlag Opt_DoCmmLinting))
  , Flag "dasm-lint"               (NoArg (setGeneralFlag Opt_DoAsmLinting))
  , Flag "dshow-passes"            (NoArg (do forceRecompile
                                              setVerbosity $ Just 2))
  , Flag "dfaststring-stats"       (NoArg (setGeneralFlag Opt_D_faststring_stats))
  , Flag "dno-llvm-mangler"        (NoArg (setGeneralFlag Opt_NoLlvmMangler)) -- hidden flag

        ------ Machine dependant (-m<blah>) stuff ---------------------------

  , Flag "monly-2-regs" (NoArg (addWarn "The -monly-2-regs flag does nothing; it will be removed in a future GHC release"))
  , Flag "monly-3-regs" (NoArg (addWarn "The -monly-3-regs flag does nothing; it will be removed in a future GHC release"))
  , Flag "monly-4-regs" (NoArg (addWarn "The -monly-4-regs flag does nothing; it will be removed in a future GHC release"))
  , Flag "msse"         (versionSuffix (\maj min d -> d{ sseVersion = Just (maj, min) }))

     ------ Warning opts -------------------------------------------------
  , Flag "W"      (NoArg (mapM_ setWarningFlag minusWOpts))
  , Flag "Werror" (NoArg (setGeneralFlag           Opt_WarnIsError))
  , Flag "Wwarn"  (NoArg (unSetGeneralFlag         Opt_WarnIsError))
  , Flag "Wall"   (NoArg (mapM_ setWarningFlag minusWallOpts))
  , Flag "Wnot"   (NoArg (do upd (\dfs -> dfs {warningFlags = IntSet.empty})
                             deprecate "Use -w instead"))
  , Flag "w"      (NoArg (upd (\dfs -> dfs {warningFlags = IntSet.empty})))

        ------ Plugin flags ------------------------------------------------
  , Flag "fplugin-opt" (hasArg addPluginModuleNameOption)
  , Flag "fplugin"     (hasArg addPluginModuleName)

        ------ Optimisation flags ------------------------------------------
  , Flag "O"      (noArgM (setOptLevel 1))
  , Flag "Onot"   (noArgM (\dflags -> do deprecate "Use -O0 instead"
                                         setOptLevel 0 dflags))
  , Flag "Odph"   (noArgM setDPHOpt)
  , Flag "O"      (optIntSuffixM (\mb_n -> setOptLevel (mb_n `orElse` 1)))
                -- If the number is missing, use 1

  , Flag "fsimplifier-phases"          (intSuffix (\n d -> d{ simplPhases = n }))
  , Flag "fmax-simplifier-iterations"  (intSuffix (\n d -> d{ maxSimplIterations = n }))
  , Flag "fsimpl-tick-factor"          (intSuffix (\n d -> d{ simplTickFactor = n }))
  , Flag "fspec-constr-threshold"      (intSuffix (\n d -> d{ specConstrThreshold = Just n }))
  , Flag "fno-spec-constr-threshold"   (noArg (\d -> d{ specConstrThreshold = Nothing }))
  , Flag "fspec-constr-count"          (intSuffix (\n d -> d{ specConstrCount = Just n }))
  , Flag "fno-spec-constr-count"       (noArg (\d -> d{ specConstrCount = Nothing }))
  , Flag "fspec-constr-recursive"      (intSuffix (\n d -> d{ specConstrRecursive = n }))
  , Flag "fliberate-case-threshold"    (intSuffix (\n d -> d{ liberateCaseThreshold = Just n }))
  , Flag "fno-liberate-case-threshold" (noArg (\d -> d{ liberateCaseThreshold = Nothing }))
  , Flag "frule-check"                 (sepArg (\s d -> d{ ruleCheck = Just s }))
  , Flag "fcontext-stack"              (intSuffix (\n d -> d{ ctxtStkDepth = n }))
  , Flag "fstrictness-before"          (intSuffix (\n d -> d{ strictnessBefore = n : strictnessBefore d }))
  , Flag "ffloat-lam-args"             (intSuffix (\n d -> d{ floatLamArgs = Just n }))
  , Flag "ffloat-all-lams"             (noArg (\d -> d{ floatLamArgs = Nothing }))
  , Flag "fhistory-size"               (intSuffix (\n d -> d{ historySize = n }))

  , Flag "funfolding-creation-threshold" (intSuffix   (\n d -> d {ufCreationThreshold = n}))
  , Flag "funfolding-use-threshold"      (intSuffix   (\n d -> d {ufUseThreshold = n}))
  , Flag "funfolding-fun-discount"       (intSuffix   (\n d -> d {ufFunAppDiscount = n}))
  , Flag "funfolding-dict-discount"      (intSuffix   (\n d -> d {ufDictDiscount = n}))
  , Flag "funfolding-keeness-factor"     (floatSuffix (\n d -> d {ufKeenessFactor = n}))

  , Flag "fmax-worker-args" (intSuffix (\n d -> d {maxWorkerArgs = n}))

  , Flag "fghci-hist-size" (intSuffix (\n d -> d {ghciHistSize = n}))

        ------ Profiling ----------------------------------------------------

        -- OLD profiling flags
  , Flag "auto-all"              (noArg (\d -> d { profAuto = ProfAutoAll } ))
  , Flag "no-auto-all"           (noArg (\d -> d { profAuto = NoProfAuto } ))
  , Flag "auto"                  (noArg (\d -> d { profAuto = ProfAutoExports } ))
  , Flag "no-auto"               (noArg (\d -> d { profAuto = NoProfAuto } ))
  , Flag "caf-all"               (NoArg (setGeneralFlag Opt_AutoSccsOnIndividualCafs))
  , Flag "no-caf-all"            (NoArg (unSetGeneralFlag Opt_AutoSccsOnIndividualCafs))

        -- NEW profiling flags
  , Flag "fprof-auto"             (noArg (\d -> d { profAuto = ProfAutoAll } ))
  , Flag "fprof-auto-top"         (noArg (\d -> d { profAuto = ProfAutoTop } ))
  , Flag "fprof-auto-exported"    (noArg (\d -> d { profAuto = ProfAutoExports } ))
  , Flag "fprof-auto-calls"       (noArg (\d -> d { profAuto = ProfAutoCalls } ))
  , Flag "fno-prof-auto"          (noArg (\d -> d { profAuto = NoProfAuto } ))

        ------ Compiler flags -----------------------------------------------

  , Flag "fasm"             (NoArg (setObjTarget HscAsm))
  , Flag "fvia-c"           (NoArg
         (addWarn "The -fvia-c flag does nothing; it will be removed in a future GHC release"))
  , Flag "fvia-C"           (NoArg
         (addWarn "The -fvia-C flag does nothing; it will be removed in a future GHC release"))
  , Flag "fllvm"            (NoArg (setObjTarget HscLlvm))

  , Flag "fno-code"         (NoArg (do upd $ \d -> d{ ghcLink=NoLink }
                                       setTarget HscNothing))
  , Flag "fbyte-code"       (NoArg (setTarget HscInterpreted))
  , Flag "fobject-code"     (NoArg (setTargetWithPlatform defaultHscTarget))
  , Flag "fglasgow-exts"    (NoArg (enableGlasgowExts >> deprecate "Use individual extensions instead"))
  , Flag "fno-glasgow-exts" (NoArg (disableGlasgowExts >> deprecate "Use individual extensions instead"))

        ------ Safe Haskell flags -------------------------------------------
  , Flag "fpackage-trust"   (NoArg setPackageTrust)
  , Flag "fno-safe-infer"   (NoArg (setSafeHaskell Sf_None))
  , Flag "fPIC"             (NoArg (setGeneralFlag Opt_PIC))
  , Flag "fno-PIC"          (NoArg (unSetGeneralFlag Opt_PIC))
 ]
 ++ map (mkFlag turnOn  ""     setGeneralFlag  ) negatableFlags
 ++ map (mkFlag turnOff "no-"  unSetGeneralFlag) negatableFlags
 ++ map (mkFlag turnOn  "d"    setGeneralFlag  ) dFlags
 ++ map (mkFlag turnOff "dno-" unSetGeneralFlag) dFlags
 ++ map (mkFlag turnOn  "f"    setGeneralFlag  ) fFlags
 ++ map (mkFlag turnOff "fno-" unSetGeneralFlag) fFlags
 ++ map (mkFlag turnOn  "f"    setWarningFlag  ) fWarningFlags
 ++ map (mkFlag turnOff "fno-" unSetWarningFlag) fWarningFlags
 ++ map (mkFlag turnOn  "f"    setExtensionFlag  ) fLangFlags
 ++ map (mkFlag turnOff "fno-" unSetExtensionFlag) fLangFlags
 ++ map (mkFlag turnOn  "X"    setExtensionFlag  ) xFlags
 ++ map (mkFlag turnOff "XNo"  unSetExtensionFlag) xFlags
 ++ map (mkFlag turnOn  "X"    setLanguage) languageFlags
 ++ map (mkFlag turnOn  "X"    setSafeHaskell) safeHaskellFlags
 ++ [ Flag "XGenerics"       (NoArg (deprecate "it does nothing; look into -XDefaultSignatures and -XDeriveGeneric for generic programming support."))
    , Flag "XNoGenerics"     (NoArg (deprecate "it does nothing; look into -XDefaultSignatures and -XDeriveGeneric for generic programming support.")) ]

package_flags :: [Flag (CmdLineP DynFlags)]
package_flags = [
        ------- Packages ----------------------------------------------------
    Flag "package-db"            (HasArg (addPkgConfRef . PkgConfFile))
  , Flag "clear-package-db"      (NoArg clearPkgConf)
  , Flag "no-global-package-db"  (NoArg removeGlobalPkgConf)
  , Flag "no-user-package-db"    (NoArg removeUserPkgConf)
  , Flag "global-package-db"     (NoArg (addPkgConfRef GlobalPkgConf))
  , Flag "user-package-db"       (NoArg (addPkgConfRef UserPkgConf))

    -- backwards compat with GHC<=7.4 :
  , Flag "package-conf"          (HasArg $ \path -> do
                                    addPkgConfRef (PkgConfFile path)
                                    deprecate "Use -package-db instead")
  , Flag "no-user-package-conf"  (NoArg $ do
                                    removeUserPkgConf
                                    deprecate "Use -no-user-package-db instead")

  , Flag "package-name"          (hasArg setPackageName)
  , Flag "package-id"            (HasArg exposePackageId)
  , Flag "package"               (HasArg exposePackage)
  , Flag "hide-package"          (HasArg hidePackage)
  , Flag "hide-all-packages"     (NoArg (setGeneralFlag Opt_HideAllPackages))
  , Flag "ignore-package"        (HasArg ignorePackage)
  , Flag "syslib"                (HasArg (\s -> do exposePackage s
                                                   deprecate "Use -package instead"))
  , Flag "distrust-all-packages" (NoArg (setGeneralFlag Opt_DistrustAllPackages))
  , Flag "trust"                 (HasArg trustPackage)
  , Flag "distrust"              (HasArg distrustPackage)
  ]

type TurnOnFlag = Bool   -- True  <=> we are turning the flag on
                         -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn  = True
turnOff :: TurnOnFlag; turnOff = False

type FlagSpec flag
   = ( String   -- Flag in string form
     , flag     -- Flag in internal form
     , TurnOnFlag -> DynP ())    -- Extra action to run when the flag is found
                                 -- Typically, emit a warning or error

mkFlag :: TurnOnFlag            -- ^ True <=> it should be turned on
       -> String                -- ^ The flag prefix
       -> (flag -> DynP ())     -- ^ What to do when the flag is found
       -> FlagSpec flag         -- ^ Specification of this particular flag
       -> Flag (CmdLineP DynFlags)
mkFlag turn_on flagPrefix f (name, flag, extra_action)
    = Flag (flagPrefix ++ name) (NoArg (f flag >> extra_action turn_on))

deprecatedForExtension :: String -> TurnOnFlag -> DynP ()
deprecatedForExtension lang turn_on
    = deprecate ("use -X"  ++ flag ++ " or pragma {-# LANGUAGE " ++ flag ++ " #-} instead")
    where
      flag | turn_on    = lang
           | otherwise = "No"++lang

useInstead :: String -> TurnOnFlag -> DynP ()
useInstead flag turn_on
  = deprecate ("Use -f" ++ no ++ flag ++ " instead")
  where
    no = if turn_on then "" else "no-"

nop :: TurnOnFlag -> DynP ()
nop _ = return ()

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fWarningFlags :: [FlagSpec WarningFlag]
fWarningFlags = [
  ( "warn-dodgy-foreign-imports",       Opt_WarnDodgyForeignImports, nop ),
  ( "warn-dodgy-exports",               Opt_WarnDodgyExports, nop ),
  ( "warn-dodgy-imports",               Opt_WarnDodgyImports, nop ),
  ( "warn-duplicate-exports",           Opt_WarnDuplicateExports, nop ),
  ( "warn-duplicate-constraints",       Opt_WarnDuplicateConstraints, nop ),
  ( "warn-hi-shadowing",                Opt_WarnHiShadows, nop ),
  ( "warn-implicit-prelude",            Opt_WarnImplicitPrelude, nop ),
  ( "warn-incomplete-patterns",         Opt_WarnIncompletePatterns, nop ),
  ( "warn-incomplete-uni-patterns",     Opt_WarnIncompleteUniPatterns, nop ),
  ( "warn-incomplete-record-updates",   Opt_WarnIncompletePatternsRecUpd, nop ),
  ( "warn-missing-fields",              Opt_WarnMissingFields, nop ),
  ( "warn-missing-import-lists",        Opt_WarnMissingImportList, nop ),
  ( "warn-missing-methods",             Opt_WarnMissingMethods, nop ),
  ( "warn-missing-signatures",          Opt_WarnMissingSigs, nop ),
  ( "warn-missing-local-sigs",          Opt_WarnMissingLocalSigs, nop ),
  ( "warn-name-shadowing",              Opt_WarnNameShadowing, nop ),
  ( "warn-overlapping-patterns",        Opt_WarnOverlappingPatterns, nop ),
  ( "warn-type-defaults",               Opt_WarnTypeDefaults, nop ),
  ( "warn-monomorphism-restriction",    Opt_WarnMonomorphism, nop ),
  ( "warn-unused-binds",                Opt_WarnUnusedBinds, nop ),
  ( "warn-unused-imports",              Opt_WarnUnusedImports, nop ),
  ( "warn-unused-matches",              Opt_WarnUnusedMatches, nop ),
  ( "warn-warnings-deprecations",       Opt_WarnWarningsDeprecations, nop ),
  ( "warn-deprecations",                Opt_WarnWarningsDeprecations, nop ),
  ( "warn-deprecated-flags",            Opt_WarnDeprecatedFlags, nop ),
  ( "warn-orphans",                     Opt_WarnOrphans, nop ),
  ( "warn-identities",                  Opt_WarnIdentities, nop ),
  ( "warn-auto-orphans",                Opt_WarnAutoOrphans, nop ),
  ( "warn-tabs",                        Opt_WarnTabs, nop ),
  ( "warn-unrecognised-pragmas",        Opt_WarnUnrecognisedPragmas, nop ),
  ( "warn-lazy-unlifted-bindings",      Opt_WarnLazyUnliftedBindings, nop ),
  ( "warn-unused-do-bind",              Opt_WarnUnusedDoBind, nop ),
  ( "warn-wrong-do-bind",               Opt_WarnWrongDoBind, nop ),
  ( "warn-alternative-layout-rule-transitional", Opt_WarnAlternativeLayoutRuleTransitional, nop ),
  ( "warn-unsafe",                      Opt_WarnUnsafe, setWarnUnsafe ),
  ( "warn-safe",                        Opt_WarnSafe, setWarnSafe ),
  ( "warn-pointless-pragmas",           Opt_WarnPointlessPragmas, nop ),
  ( "warn-unsupported-calling-conventions", Opt_WarnUnsupportedCallingConventions, nop ),
  ( "warn-inline-rule-shadowing",       Opt_WarnInlineRuleShadowing, nop ),
  ( "warn-unsupported-llvm-version",    Opt_WarnUnsupportedLlvmVersion, nop ),
  ( "warn-typeable-instances",          Opt_WarnTypeableInstances, nop ) ]

-- | These @-\<blah\>@ flags can all be reversed with @-no-\<blah\>@
negatableFlags :: [FlagSpec GeneralFlag]
negatableFlags = [
  ( "ignore-dot-ghci",                  Opt_IgnoreDotGhci, nop ) ]

-- | These @-d\<blah\>@ flags can all be reversed with @-dno-\<blah\>@
dFlags :: [FlagSpec GeneralFlag]
dFlags = [
  ( "suppress-coercions",               Opt_SuppressCoercions,          nop),
  ( "suppress-var-kinds",               Opt_SuppressVarKinds,           nop),
  ( "suppress-module-prefixes",         Opt_SuppressModulePrefixes,     nop),
  ( "suppress-type-applications",       Opt_SuppressTypeApplications,   nop),
  ( "suppress-idinfo",                  Opt_SuppressIdInfo,             nop),
  ( "suppress-type-signatures",         Opt_SuppressTypeSignatures,     nop),
  ( "suppress-uniques",                 Opt_SuppressUniques,            nop),
  ( "ppr-case-as-let",                  Opt_PprCaseAsLet,               nop)]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fFlags :: [FlagSpec GeneralFlag]
fFlags = [
  ( "error-spans",                      Opt_ErrorSpans, nop ),
  ( "print-explicit-foralls",           Opt_PrintExplicitForalls, nop ),
  ( "strictness",                       Opt_Strictness, nop ),
  ( "specialise",                       Opt_Specialise, nop ),
  ( "float-in",                         Opt_FloatIn, nop ),
  ( "static-argument-transformation",   Opt_StaticArgumentTransformation, nop ),
  ( "full-laziness",                    Opt_FullLaziness, nop ),
  ( "liberate-case",                    Opt_LiberateCase, nop ),
  ( "spec-constr",                      Opt_SpecConstr, nop ),
  ( "cse",                              Opt_CSE, nop ),
  ( "pedantic-bottoms",                 Opt_PedanticBottoms, nop ),
  ( "ignore-interface-pragmas",         Opt_IgnoreInterfacePragmas, nop ),
  ( "omit-interface-pragmas",           Opt_OmitInterfacePragmas, nop ),
  ( "expose-all-unfoldings",            Opt_ExposeAllUnfoldings, nop ),
  ( "do-lambda-eta-expansion",          Opt_DoLambdaEtaExpansion, nop ),
  ( "ignore-asserts",                   Opt_IgnoreAsserts, nop ),
  ( "do-eta-reduction",                 Opt_DoEtaReduction, nop ),
  ( "case-merge",                       Opt_CaseMerge, nop ),
  ( "unbox-strict-fields",              Opt_UnboxStrictFields, nop ),
  ( "unbox-small-strict-fields",        Opt_UnboxSmallStrictFields, nop ),
  ( "dicts-cheap",                      Opt_DictsCheap, nop ),
  ( "excess-precision",                 Opt_ExcessPrecision, nop ),
  ( "eager-blackholing",                Opt_EagerBlackHoling, nop ),
  ( "print-bind-result",                Opt_PrintBindResult, nop ),
  ( "force-recomp",                     Opt_ForceRecomp, nop ),
  ( "hpc-no-auto",                      Opt_Hpc_No_Auto, nop ),
  ( "rewrite-rules",                    Opt_EnableRewriteRules, useInstead "enable-rewrite-rules" ),
  ( "enable-rewrite-rules",             Opt_EnableRewriteRules, nop ),
  ( "break-on-exception",               Opt_BreakOnException, nop ),
  ( "break-on-error",                   Opt_BreakOnError, nop ),
  ( "print-evld-with-show",             Opt_PrintEvldWithShow, nop ),
  ( "print-bind-contents",              Opt_PrintBindContents, nop ),
  ( "run-cps",                          Opt_RunCPS, nop ),
  ( "run-cpsz",                         Opt_RunCPSZ, nop ),
  ( "vectorise",                        Opt_Vectorise, nop ),
  ( "vectorisation-avoidance",          Opt_VectorisationAvoidance, nop ),
  ( "regs-graph",                       Opt_RegsGraph, nop ),
  ( "regs-iterative",                   Opt_RegsIterative, nop ),
  ( "llvm-tbaa",                        Opt_LlvmTBAA, nop), -- hidden flag
  ( "irrefutable-tuples",               Opt_IrrefutableTuples, nop ),
  ( "cmm-sink",                         Opt_CmmSink, nop ),
  ( "cmm-elim-common-blocks",           Opt_CmmElimCommonBlocks, nop ),
  ( "omit-yields",                      Opt_OmitYields, nop ),
  ( "simple-list-literals",             Opt_SimpleListLiterals, nop ),
  ( "fun-to-thunk",                     Opt_FunToThunk, nop ),
  ( "gen-manifest",                     Opt_GenManifest, nop ),
  ( "embed-manifest",                   Opt_EmbedManifest, nop ),
  ( "ext-core",                         Opt_EmitExternalCore, nop ),
  ( "shared-implib",                    Opt_SharedImplib, nop ),
  ( "ghci-sandbox",                     Opt_GhciSandbox, nop ),
  ( "ghci-history",                     Opt_GhciHistory, nop ),
  ( "helpful-errors",                   Opt_HelpfulErrors, nop ),
  ( "defer-type-errors",                Opt_DeferTypeErrors, nop ),
  ( "building-cabal-package",           Opt_BuildingCabalPackage, nop ),
  ( "implicit-import-qualified",        Opt_ImplicitImportQualified, nop ),
  ( "prof-count-entries",               Opt_ProfCountEntries, nop ),
  ( "prof-cafs",                        Opt_AutoSccsOnIndividualCafs, nop ),
  ( "hpc",                              Opt_Hpc, nop ),
  ( "pre-inlining",                     Opt_SimplPreInlining, nop ),
  ( "use-rpaths",                       Opt_RPath, nop )
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fLangFlags :: [FlagSpec ExtensionFlag]
fLangFlags = [
  ( "th",                               Opt_TemplateHaskell,
    \on -> deprecatedForExtension "TemplateHaskell" on 
        >> checkTemplateHaskellOk on ),
  ( "fi",                               Opt_ForeignFunctionInterface,
    deprecatedForExtension "ForeignFunctionInterface" ),
  ( "ffi",                              Opt_ForeignFunctionInterface,
    deprecatedForExtension "ForeignFunctionInterface" ),
  ( "arrows",                           Opt_Arrows,
    deprecatedForExtension "Arrows" ),
  ( "implicit-prelude",                 Opt_ImplicitPrelude,
    deprecatedForExtension "ImplicitPrelude" ),
  ( "bang-patterns",                    Opt_BangPatterns,
    deprecatedForExtension "BangPatterns" ),
  ( "monomorphism-restriction",         Opt_MonomorphismRestriction,
    deprecatedForExtension "MonomorphismRestriction" ),
  ( "mono-pat-binds",                   Opt_MonoPatBinds,
    deprecatedForExtension "MonoPatBinds" ),
  ( "extended-default-rules",           Opt_ExtendedDefaultRules,
    deprecatedForExtension "ExtendedDefaultRules" ),
  ( "implicit-params",                  Opt_ImplicitParams,
    deprecatedForExtension "ImplicitParams" ),
  ( "scoped-type-variables",            Opt_ScopedTypeVariables,
    deprecatedForExtension "ScopedTypeVariables" ),
  ( "parr",                             Opt_ParallelArrays,
    deprecatedForExtension "ParallelArrays" ),
  ( "PArr",                             Opt_ParallelArrays,
    deprecatedForExtension "ParallelArrays" ),
  ( "allow-overlapping-instances",      Opt_OverlappingInstances,
    deprecatedForExtension "OverlappingInstances" ),
  ( "allow-undecidable-instances",      Opt_UndecidableInstances,
    deprecatedForExtension "UndecidableInstances" ),
  ( "allow-incoherent-instances",       Opt_IncoherentInstances,
    deprecatedForExtension "IncoherentInstances" )
  ]

supportedLanguages :: [String]
supportedLanguages = [ name | (name, _, _) <- languageFlags ]

supportedLanguageOverlays :: [String]
supportedLanguageOverlays = [ name | (name, _, _) <- safeHaskellFlags ]

supportedExtensions :: [String]
supportedExtensions = [ name' | (name, _, _) <- xFlags, name' <- [name, "No" ++ name] ]

supportedLanguagesAndExtensions :: [String]
supportedLanguagesAndExtensions =
    supportedLanguages ++ supportedLanguageOverlays ++ supportedExtensions

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
languageFlags :: [FlagSpec Language]
languageFlags = [
  ( "Haskell98",   Haskell98, nop ),
  ( "Haskell2010", Haskell2010, nop )
  ]

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
-- They are used to place hard requirements on what GHC Haskell language
-- features can be used.
safeHaskellFlags :: [FlagSpec SafeHaskellMode]
safeHaskellFlags = [mkF Sf_Unsafe, mkF Sf_Trustworthy, mkF Sf_Safe]
    where mkF flag = (show flag, flag, nop)

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [FlagSpec ExtensionFlag]
xFlags = [
  ( "CPP",                              Opt_Cpp, nop ),
  ( "PostfixOperators",                 Opt_PostfixOperators, nop ),
  ( "TupleSections",                    Opt_TupleSections, nop ),
  ( "PatternGuards",                    Opt_PatternGuards, nop ),
  ( "UnicodeSyntax",                    Opt_UnicodeSyntax, nop ),
  ( "MagicHash",                        Opt_MagicHash, nop ),
  ( "ExistentialQuantification",        Opt_ExistentialQuantification, nop ),
  ( "KindSignatures",                   Opt_KindSignatures, nop ),
  ( "EmptyDataDecls",                   Opt_EmptyDataDecls, nop ),
  ( "ParallelListComp",                 Opt_ParallelListComp, nop ),
  ( "TransformListComp",                Opt_TransformListComp, nop ),
  ( "MonadComprehensions",              Opt_MonadComprehensions, nop),
  ( "ForeignFunctionInterface",         Opt_ForeignFunctionInterface, nop ),
  ( "UnliftedFFITypes",                 Opt_UnliftedFFITypes, nop ),
  ( "InterruptibleFFI",                 Opt_InterruptibleFFI, nop ),
  ( "CApiFFI",                          Opt_CApiFFI, nop ),
  ( "GHCForeignImportPrim",             Opt_GHCForeignImportPrim, nop ),
  ( "LiberalTypeSynonyms",              Opt_LiberalTypeSynonyms, nop ),

  ( "PolymorphicComponents",            Opt_RankNTypes, nop),
  ( "Rank2Types",                       Opt_RankNTypes, nop),
  ( "RankNTypes",                       Opt_RankNTypes, nop ),

  ( "ImpredicativeTypes",               Opt_ImpredicativeTypes, nop),
  ( "TypeOperators",                    Opt_TypeOperators, nop ),
  ( "ExplicitNamespaces",               Opt_ExplicitNamespaces, nop ),
  ( "RecursiveDo",                      Opt_RecursiveDo, nop ),  -- Enables 'mdo' and 'rec'
  ( "DoRec",                            Opt_RecursiveDo, 
     deprecatedForExtension "RecursiveDo" ),
  ( "Arrows",                           Opt_Arrows, nop ),
  ( "ParallelArrays",                   Opt_ParallelArrays, nop ),
  ( "TemplateHaskell",                  Opt_TemplateHaskell, checkTemplateHaskellOk ),
  ( "QuasiQuotes",                      Opt_QuasiQuotes, nop ),
  ( "ImplicitPrelude",                  Opt_ImplicitPrelude, nop ),
  ( "RecordWildCards",                  Opt_RecordWildCards, nop ),
  ( "NamedFieldPuns",                   Opt_RecordPuns, nop ),
  ( "RecordPuns",                       Opt_RecordPuns,
    deprecatedForExtension "NamedFieldPuns" ),
  ( "DisambiguateRecordFields",         Opt_DisambiguateRecordFields, nop ),
  ( "OverloadedStrings",                Opt_OverloadedStrings, nop ),
  ( "OverloadedLists",                  Opt_OverloadedLists, nop),
  ( "GADTs",                            Opt_GADTs, nop ),
  ( "GADTSyntax",                       Opt_GADTSyntax, nop ),
  ( "ViewPatterns",                     Opt_ViewPatterns, nop ),
  ( "TypeFamilies",                     Opt_TypeFamilies, nop ),
  ( "BangPatterns",                     Opt_BangPatterns, nop ),
  ( "MonomorphismRestriction",          Opt_MonomorphismRestriction, nop ),
  ( "NPlusKPatterns",                   Opt_NPlusKPatterns, nop ),
  ( "DoAndIfThenElse",                  Opt_DoAndIfThenElse, nop ),
  ( "RebindableSyntax",                 Opt_RebindableSyntax, nop ),
  ( "ConstraintKinds",                  Opt_ConstraintKinds, nop ),
  ( "PolyKinds",                        Opt_PolyKinds, nop ),
  ( "DataKinds",                        Opt_DataKinds, nop ),
  ( "InstanceSigs",                     Opt_InstanceSigs, nop ),
  ( "MonoPatBinds",                     Opt_MonoPatBinds,
    \ turn_on -> when turn_on $ deprecate "Experimental feature now removed; has no effect" ),
  ( "ExplicitForAll",                   Opt_ExplicitForAll, nop ),
  ( "AlternativeLayoutRule",            Opt_AlternativeLayoutRule, nop ),
  ( "AlternativeLayoutRuleTransitional",Opt_AlternativeLayoutRuleTransitional, nop ),
  ( "DatatypeContexts",                 Opt_DatatypeContexts,
    \ turn_on -> when turn_on $ deprecate "It was widely considered a misfeature, and has been removed from the Haskell language." ),
  ( "NondecreasingIndentation",         Opt_NondecreasingIndentation, nop ),
  ( "RelaxedLayout",                    Opt_RelaxedLayout, nop ),
  ( "TraditionalRecordSyntax",          Opt_TraditionalRecordSyntax, nop ),
  ( "LambdaCase",                       Opt_LambdaCase, nop ),
  ( "MultiWayIf",                       Opt_MultiWayIf, nop ),
  ( "MonoLocalBinds",                   Opt_MonoLocalBinds, nop ),
  ( "RelaxedPolyRec",                   Opt_RelaxedPolyRec,
    \ turn_on -> unless turn_on
               $ deprecate "You can't turn off RelaxedPolyRec any more" ),
  ( "ExtendedDefaultRules",             Opt_ExtendedDefaultRules, nop ),
  ( "ImplicitParams",                   Opt_ImplicitParams, nop ),
  ( "ScopedTypeVariables",              Opt_ScopedTypeVariables, nop ),
  ( "AllowAmbiguousTypes",              Opt_AllowAmbiguousTypes, nop),

  ( "PatternSignatures",                Opt_ScopedTypeVariables,
    deprecatedForExtension "ScopedTypeVariables" ),

  ( "UnboxedTuples",                    Opt_UnboxedTuples, nop ),
  ( "StandaloneDeriving",               Opt_StandaloneDeriving, nop ),
  ( "DeriveDataTypeable",               Opt_DeriveDataTypeable, nop ),
  ( "AutoDeriveTypeable",               Opt_AutoDeriveTypeable, nop ),
  ( "DeriveFunctor",                    Opt_DeriveFunctor, nop ),
  ( "DeriveTraversable",                Opt_DeriveTraversable, nop ),
  ( "DeriveFoldable",                   Opt_DeriveFoldable, nop ),
  ( "DeriveGeneric",                    Opt_DeriveGeneric, nop ),
  ( "DefaultSignatures",                Opt_DefaultSignatures, nop ),
  ( "TypeSynonymInstances",             Opt_TypeSynonymInstances, nop ),
  ( "FlexibleContexts",                 Opt_FlexibleContexts, nop ),
  ( "FlexibleInstances",                Opt_FlexibleInstances, nop ),
  ( "ConstrainedClassMethods",          Opt_ConstrainedClassMethods, nop ),
  ( "MultiParamTypeClasses",            Opt_MultiParamTypeClasses, nop ),
  ( "NullaryTypeClasses",               Opt_NullaryTypeClasses, nop ),
  ( "FunctionalDependencies",           Opt_FunctionalDependencies, nop ),
  ( "GeneralizedNewtypeDeriving",       Opt_GeneralizedNewtypeDeriving, setGenDeriving ),
  ( "OverlappingInstances",             Opt_OverlappingInstances, nop ),
  ( "UndecidableInstances",             Opt_UndecidableInstances, nop ),
  ( "IncoherentInstances",              Opt_IncoherentInstances, nop ),
  ( "PackageImports",                   Opt_PackageImports, nop ),
  ( "TypeHoles",                        Opt_TypeHoles, nop ),
  ( "EmptyCase",                        Opt_EmptyCase, nop )
  ]

defaultFlags :: Settings -> [GeneralFlag]
defaultFlags settings
  = [ Opt_AutoLinkPackages,

      Opt_SharedImplib,

      Opt_OmitYields,

      Opt_GenManifest,
      Opt_EmbedManifest,
      Opt_PrintBindContents,
      Opt_GhciSandbox,
      Opt_GhciHistory,
      Opt_HelpfulErrors,
      Opt_ProfCountEntries,
      Opt_SimplPreInlining,
      Opt_RPath
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    ++ default_PIC platform

    ++ (if pc_DYNAMIC_BY_DEFAULT (sPlatformConstants settings)
        then wayGeneralFlags platform WayDyn
        else [])

    where platform = sTargetPlatform settings

default_PIC :: Platform -> [GeneralFlag]
default_PIC platform =
  case (platformOS platform, platformArch platform) of
    (OSDarwin, ArchX86_64) -> [Opt_PIC]
    _                      -> []

impliedFlags :: [(ExtensionFlag, TurnOnFlag, ExtensionFlag)]
impliedFlags
  = [ (Opt_RankNTypes,                turnOn, Opt_ExplicitForAll)
    , (Opt_ScopedTypeVariables,       turnOn, Opt_ExplicitForAll)
    , (Opt_LiberalTypeSynonyms,       turnOn, Opt_ExplicitForAll)
    , (Opt_ExistentialQuantification, turnOn, Opt_ExplicitForAll)
    , (Opt_FlexibleInstances,         turnOn, Opt_TypeSynonymInstances)
    , (Opt_FunctionalDependencies,    turnOn, Opt_MultiParamTypeClasses)

    , (Opt_RebindableSyntax, turnOff, Opt_ImplicitPrelude)      -- NB: turn off!

    , (Opt_GADTs,            turnOn, Opt_GADTSyntax)
    , (Opt_GADTs,            turnOn, Opt_MonoLocalBinds)
    , (Opt_TypeFamilies,     turnOn, Opt_MonoLocalBinds)

    , (Opt_TypeFamilies,     turnOn, Opt_KindSignatures)  -- Type families use kind signatures
    , (Opt_PolyKinds,        turnOn, Opt_KindSignatures)  -- Ditto polymorphic kinds

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (Opt_AutoDeriveTypeable, turnOn, Opt_DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (Opt_TypeFamilies,     turnOn, Opt_ExplicitNamespaces)
    , (Opt_TypeOperators, turnOn, Opt_ExplicitNamespaces)

    , (Opt_ImpredicativeTypes,  turnOn, Opt_RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (Opt_RecordWildCards,     turnOn, Opt_DisambiguateRecordFields)

    , (Opt_ParallelArrays, turnOn, Opt_ParallelListComp)

    -- An implicit parameter constraint, `?x::Int`, is desugared into
    -- `IP "x" Int`, which requires a flexible context/instance.
    , (Opt_ImplicitParams, turnOn, Opt_FlexibleContexts)
    , (Opt_ImplicitParams, turnOn, Opt_FlexibleInstances)
  ]

optLevelFlags :: [([Int], GeneralFlag)]
optLevelFlags
  = [ ([0],     Opt_IgnoreInterfacePragmas)
    , ([0],     Opt_OmitInterfacePragmas)

    , ([1,2],   Opt_IgnoreAsserts)
    , ([1,2],   Opt_EnableRewriteRules)  -- Off for -O0; see Note [Scoping for Builtin rules]
                                         --              in PrelRules
    , ([1,2],   Opt_DoEtaReduction)
    , ([1,2],   Opt_CaseMerge)
    , ([1,2],   Opt_Strictness)
    , ([1,2],   Opt_CSE)
    , ([1,2],   Opt_FullLaziness)
    , ([1,2],   Opt_Specialise)
    , ([1,2],   Opt_FloatIn)

    , ([2],     Opt_LiberateCase)
    , ([2],     Opt_SpecConstr)
-- XXX disabled, see #7192
--    , ([2],     Opt_RegsGraph)
    , ([0,1,2], Opt_LlvmTBAA)
    , ([1,2],   Opt_CmmSink)
    , ([1,2],   Opt_CmmElimCommonBlocks)

--     , ([2],     Opt_StaticArgumentTransformation)
-- Max writes: I think it's probably best not to enable SAT with -O2 for the
-- 6.10 release. The version of SAT in HEAD at the moment doesn't incorporate
-- several improvements to the heuristics, and I'm concerned that without
-- those changes SAT will interfere with some attempts to write "high
-- performance Haskell", as we saw in some posts on Haskell-Cafe earlier
-- this year. In particular, the version in HEAD lacks the tail call
-- criterion, so many things that look like reasonable loops will be
-- turned into functions with extra (unneccesary) thunk creation.

    , ([0,1,2], Opt_DoLambdaEtaExpansion)
                -- This one is important for a tiresome reason:
                -- we want to make sure that the bindings for data
                -- constructors are eta-expanded.  This is probably
                -- a good thing anyway, but it seems fragile.
    , ([0,1,2], Opt_VectorisationAvoidance)
    ]

-- -----------------------------------------------------------------------------
-- Standard sets of warning options

standardWarnings :: [WarningFlag]
standardWarnings
    = [ Opt_WarnWarningsDeprecations,
        Opt_WarnDeprecatedFlags,
        Opt_WarnUnrecognisedPragmas,
        Opt_WarnOverlappingPatterns,
        Opt_WarnMissingFields,
        Opt_WarnMissingMethods,
        Opt_WarnDuplicateExports,
        Opt_WarnLazyUnliftedBindings,
        Opt_WarnDodgyForeignImports,
        Opt_WarnWrongDoBind,
        Opt_WarnAlternativeLayoutRuleTransitional,
        Opt_WarnPointlessPragmas,
        Opt_WarnUnsupportedCallingConventions,
        Opt_WarnUnsupportedLlvmVersion,
        Opt_WarnInlineRuleShadowing,
        Opt_WarnDuplicateConstraints,
        Opt_WarnInlineRuleShadowing,
        Opt_WarnTypeableInstances
      ]

minusWOpts :: [WarningFlag]
-- Things you get with -W
minusWOpts
    = standardWarnings ++
      [ Opt_WarnUnusedBinds,
        Opt_WarnUnusedMatches,
        Opt_WarnUnusedImports,
        Opt_WarnIncompletePatterns,
        Opt_WarnDodgyExports,
        Opt_WarnDodgyImports
      ]

minusWallOpts :: [WarningFlag]
-- Things you get with -Wall
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSigs,
        Opt_WarnHiShadows,
        Opt_WarnOrphans,
        Opt_WarnUnusedDoBind
      ]

enableGlasgowExts :: DynP ()
enableGlasgowExts = do setGeneralFlag Opt_PrintExplicitForalls
                       mapM_ setExtensionFlag glasgowExtsFlags

disableGlasgowExts :: DynP ()
disableGlasgowExts = do unSetGeneralFlag Opt_PrintExplicitForalls
                        mapM_ unSetExtensionFlag glasgowExtsFlags

glasgowExtsFlags :: [ExtensionFlag]
glasgowExtsFlags = [
             Opt_ForeignFunctionInterface
           , Opt_UnliftedFFITypes
           , Opt_ImplicitParams
           , Opt_ScopedTypeVariables
           , Opt_UnboxedTuples
           , Opt_TypeSynonymInstances
           , Opt_StandaloneDeriving
           , Opt_DeriveDataTypeable
           , Opt_DeriveFunctor
           , Opt_DeriveFoldable
           , Opt_DeriveTraversable
           , Opt_DeriveGeneric
           , Opt_FlexibleContexts
           , Opt_FlexibleInstances
           , Opt_ConstrainedClassMethods
           , Opt_MultiParamTypeClasses
           , Opt_FunctionalDependencies
           , Opt_MagicHash
           , Opt_ExistentialQuantification
           , Opt_UnicodeSyntax
           , Opt_PostfixOperators
           , Opt_PatternGuards
           , Opt_LiberalTypeSynonyms
           , Opt_RankNTypes
           , Opt_TypeOperators
           , Opt_ExplicitNamespaces
           , Opt_RecursiveDo
           , Opt_ParallelListComp
           , Opt_EmptyDataDecls
           , Opt_KindSignatures
           , Opt_GeneralizedNewtypeDeriving ]

#ifdef GHCI
-- Consult the RTS to find whether GHC itself has been built profiled
-- If so, you can't use Template Haskell
foreign import ccall unsafe "rts_isProfiled" rtsIsProfiledIO :: IO CInt

rtsIsProfiled :: Bool
rtsIsProfiled = unsafePerformIO rtsIsProfiledIO /= 0
#endif

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

checkTemplateHaskellOk :: TurnOnFlag -> DynP ()
#ifdef GHCI
checkTemplateHaskellOk turn_on
  | turn_on && rtsIsProfiled
  = addErr "You can't use Template Haskell with a profiled compiler"
  | otherwise
  = getCurLoc >>= \l -> upd (\d -> d { thOnLoc = l })
#else
-- In stage 1 we don't know that the RTS has rts_isProfiled,
-- so we simply say "ok".  It doesn't matter because TH isn't
-- available in stage 1 anyway.
checkTemplateHaskellOk _ = return ()
#endif

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

noArgDF :: (DynFlags -> DynFlags) -> String -> OptKind (CmdLineP DynFlags)
noArgDF fn deprec = NoArg (upd fn >> deprecate deprec)

hasArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
hasArg fn = HasArg (upd . fn)

hasArgDF :: (String -> DynFlags -> DynFlags) -> String -> OptKind (CmdLineP DynFlags)
hasArgDF fn deprec = HasArg (\s -> do upd (fn s)
                                      deprecate deprec)

sepArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
sepArg fn = SepArg (upd . fn)

intSuffix :: (Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffix fn = IntSuffix (\n -> upd (fn n))

floatSuffix :: (Float -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
floatSuffix fn = FloatSuffix (\n -> upd (fn n))

optIntSuffixM :: (Maybe Int -> DynFlags -> DynP DynFlags)
              -> OptKind (CmdLineP DynFlags)
optIntSuffixM fn = OptIntSuffix (\mi -> updM (fn mi))

versionSuffix :: (Int -> Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
versionSuffix fn = VersionSuffix (\maj min -> upd (fn maj min))

setDumpFlag :: DumpFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

--------------------------
addWay :: Way -> DynP ()
addWay w = upd (addWay' w)

addWay' :: Way -> DynFlags -> DynFlags
addWay' w dflags0 = let platform = targetPlatform dflags0
                        dflags1 = dflags0 { ways = w : ways dflags0 }
                        dflags2 = wayExtras platform w dflags1
                        dflags3 = foldr setGeneralFlag' dflags2
                                        (wayGeneralFlags platform w)
                        dflags4 = foldr unSetGeneralFlag' dflags3
                                        (wayUnsetGeneralFlags platform w)
                    in dflags4

removeWayDyn :: DynP ()
removeWayDyn = upd (\dfs -> dfs { ways = filter (WayDyn /=) (ways dfs) })

--------------------------
setGeneralFlag, unSetGeneralFlag :: GeneralFlag -> DynP ()
setGeneralFlag   f = upd (setGeneralFlag' f)
unSetGeneralFlag f = upd (unSetGeneralFlag' f)

setGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags
setGeneralFlag' f dflags = gopt_set dflags f
unSetGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags
unSetGeneralFlag' f dflags = gopt_unset dflags f

--------------------------
setWarningFlag, unSetWarningFlag :: WarningFlag -> DynP ()
setWarningFlag   f = upd (\dfs -> wopt_set dfs f)
unSetWarningFlag f = upd (\dfs -> wopt_unset dfs f)

--------------------------
setExtensionFlag, unSetExtensionFlag :: ExtensionFlag -> DynP ()
setExtensionFlag f = upd (setExtensionFlag' f)
unSetExtensionFlag f = upd (unSetExtensionFlag' f)

setExtensionFlag', unSetExtensionFlag' :: ExtensionFlag -> DynFlags -> DynFlags
setExtensionFlag' f dflags = foldr ($) (xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags

unSetExtensionFlag' f dflags = xopt_unset dflags f
   -- When you un-set f, however, we don't un-set the things it implies
   --      (except for -fno-glasgow-exts, which is treated specially)

--------------------------
alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
alterSettings f dflags = dflags { settings = f (settings dflags) }

--------------------------
setDumpFlag' :: DumpFlag -> DynP ()
setDumpFlag' dump_flag
  = do upd (\dfs -> dopt_set dfs dump_flag)
       when want_recomp forceRecompile
    where -- Certain dumpy-things are really interested in what's going
          -- on during recompilation checking, so in those cases we
          -- don't want to turn it off.
          want_recomp = dump_flag `notElem` [Opt_D_dump_if_trace,
                                             Opt_D_dump_hi_diffs]

forceRecompile :: DynP ()
-- Whenver we -ddump, force recompilation (by switching off the
-- recompilation checker), else you don't see the dump! However,
-- don't switch it off in --make mode, else *everything* gets
-- recompiled which probably isn't what you want
forceRecompile = do dfs <- liftEwM getCmdLineState
                    when (force_recomp dfs) (setGeneralFlag Opt_ForceRecomp)
        where
          force_recomp dfs = isOneShot (ghcMode dfs)

setVerboseCore2Core :: DynP ()
setVerboseCore2Core = do setDumpFlag' Opt_D_verbose_core2core
                         upd (\dfs -> dfs { shouldDumpSimplPhase = Nothing })

setDumpSimplPhases :: String -> DynP ()
setDumpSimplPhases s = do forceRecompile
                          upd (\dfs -> dfs { shouldDumpSimplPhase = Just spec })
  where
    spec = case s of { ('=' : s') -> s';  _ -> s }

setVerbosity :: Maybe Int -> DynP ()
setVerbosity mb_n = upd (\dfs -> dfs{ verbosity = mb_n `orElse` 3 })

addCmdlineHCInclude :: String -> DynP ()
addCmdlineHCInclude a = upd (\s -> s{cmdlineHcIncludes =  a : cmdlineHcIncludes s})

data PkgConfRef
  = GlobalPkgConf
  | UserPkgConf
  | PkgConfFile FilePath

addPkgConfRef :: PkgConfRef -> DynP ()
addPkgConfRef p = upd $ \s -> s { extraPkgConfs = (p:) . extraPkgConfs s }

removeUserPkgConf :: DynP ()
removeUserPkgConf = upd $ \s -> s { extraPkgConfs = filter isNotUser . extraPkgConfs s }
  where
    isNotUser UserPkgConf = False
    isNotUser _ = True

removeGlobalPkgConf :: DynP ()
removeGlobalPkgConf = upd $ \s -> s { extraPkgConfs = filter isNotGlobal . extraPkgConfs s }
  where
    isNotGlobal GlobalPkgConf = False
    isNotGlobal _ = True

clearPkgConf :: DynP ()
clearPkgConf = upd $ \s -> s { extraPkgConfs = const [] }

exposePackage, exposePackageId, hidePackage, ignorePackage,
        trustPackage, distrustPackage :: String -> DynP ()
exposePackage p = upd (exposePackage' p)
exposePackageId p =
  upd (\s -> s{ packageFlags = ExposePackageId p : packageFlags s })
hidePackage p =
  upd (\s -> s{ packageFlags = HidePackage p : packageFlags s })
ignorePackage p =
  upd (\s -> s{ packageFlags = IgnorePackage p : packageFlags s })
trustPackage p = exposePackage p >> -- both trust and distrust also expose a package
  upd (\s -> s{ packageFlags = TrustPackage p : packageFlags s })
distrustPackage p = exposePackage p >>
  upd (\s -> s{ packageFlags = DistrustPackage p : packageFlags s })

exposePackage' :: String -> DynFlags -> DynFlags
exposePackage' p dflags
    = dflags { packageFlags = ExposePackage p : packageFlags dflags }

setPackageName :: String -> DynFlags -> DynFlags
setPackageName p s =  s{ thisPackage = stringToPackageId p }

-- If we're linking a binary, then only targets that produce object
-- code are allowed (requests for other target types are ignored).
setTarget :: HscTarget -> DynP ()
setTarget l = setTargetWithPlatform (const l)

setTargetWithPlatform :: (Platform -> HscTarget) -> DynP ()
setTargetWithPlatform f = upd set
  where
   set dfs = let l = f (targetPlatform dfs)
             in if ghcLink dfs /= LinkBinary || isObjectTarget l
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
setOptLevel n dflags
   | hscTarget dflags == HscInterpreted && n > 0
        = do addWarn "-O conflicts with --interactive; -O ignored."
             return dflags
   | otherwise
        = return (updOptLevel n dflags)


-- -Odph is equivalent to
--
--    -O2                               optimise as much as possible
--    -fmax-simplifier-iterations20     this is necessary sometimes
--    -fsimplifier-phases=3             we use an additional simplifier phase for fusion
--
setDPHOpt :: DynFlags -> DynP DynFlags
setDPHOpt dflags = setOptLevel 2 (dflags { maxSimplIterations  = 20
                                         , simplPhases         = 3
                                         })

setMainIs :: String -> DynP ()
setMainIs arg
  | not (null main_fn) && isLower (head main_fn)
     -- The arg looked like "Foo.Bar.baz"
  = upd $ \d -> d{ mainFunIs = Just main_fn,
                   mainModIs = mkModule mainPackageId (mkModuleName main_mod) }

  | isUpper (head arg)  -- The arg looked like "Foo" or "Foo.Bar"
  = upd $ \d -> d{ mainModIs = mkModule mainPackageId (mkModuleName arg) }

  | otherwise                   -- The arg looked like "baz"
  = upd $ \d -> d{ mainFunIs = Just arg }
  where
    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

-----------------------------------------------------------------------------
-- Paths & Libraries

addImportPath, addLibraryPath, addIncludePath, addFrameworkPath :: FilePath -> DynP ()

-- -i on its own deletes the import paths
addImportPath "" = upd (\s -> s{importPaths = []})
addImportPath p  = upd (\s -> s{importPaths = importPaths s ++ splitPathList p})

addLibraryPath p =
  upd (\s -> s{libraryPaths = libraryPaths s ++ splitPathList p})

addIncludePath p =
  upd (\s -> s{includePaths = includePaths s ++ splitPathList p})

addFrameworkPath p =
  upd (\s -> s{frameworkPaths = frameworkPaths s ++ splitPathList p})

#ifndef mingw32_TARGET_OS
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
#ifndef mingw32_TARGET_OS
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
setTmpDir dir = alterSettings (\s -> s { sTmpDir = normalise dir })
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
setOptHpcDir arg  = upd $ \ d -> d{hpcDir = arg}

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
picCCOpts dflags
    = case platformOS (targetPlatform dflags) of
      OSDarwin
          -- Apple prefers to do things the other way round.
          -- PIC is on by default.
          -- -mdynamic-no-pic:
          --     Turn off PIC code generation.
          -- -fno-common:
          --     Don't generate "common" symbols - these are unwanted
          --     in dynamic libraries.

       | gopt Opt_PIC dflags -> ["-fno-common", "-U __PIC__", "-D__PIC__"]
       | otherwise           -> ["-mdynamic-no-pic"]
      OSMinGW32 -- no -fPIC for Windows
       | gopt Opt_PIC dflags -> ["-U __PIC__", "-D__PIC__"]
       | otherwise           -> []
      _
      -- we need -fPIC for C files when we are compiling with -dynamic,
      -- otherwise things like stub.c files don't get compiled
      -- correctly.  They need to reference data in the Haskell
      -- objects, but can't without -fPIC.  See
      -- http://hackage.haskell.org/trac/ghc/wiki/Commentary/PositionIndependentCode
       | gopt Opt_PIC dflags || not (gopt Opt_Static dflags) ->
          ["-fPIC", "-U __PIC__", "-D__PIC__"]
       | otherwise                             -> []

picPOpts :: DynFlags -> [String]
picPOpts dflags
 | gopt Opt_PIC dflags = ["-U __PIC__", "-D__PIC__"]
 | otherwise           = []

-- -----------------------------------------------------------------------------
-- Splitting

can_split :: Bool
can_split = cSupportsSplitObjs == "YES"

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
    : rawSettings dflags
   ++ [("Project version",             cProjectVersion),
       ("Booter version",              cBooterVersion),
       ("Stage",                       cStage),
       ("Build platform",              cBuildPlatformString),
       ("Host platform",               cHostPlatformString),
       ("Target platform",             cTargetPlatformString),
       ("Have interpreter",            cGhcWithInterpreter),
       ("Object splitting supported",  cSupportsSplitObjs),
       ("Have native code generator",  cGhcWithNativeCodeGen),
       ("Support SMP",                 cGhcWithSMP),
       ("Tables next to code",         cGhcEnableTablesNextToCode),
       ("RTS ways",                    cGhcRTSWays),
       ("Support dynamic-too",         "YES"),
       ("Dynamic by default",          if dYNAMIC_BY_DEFAULT dflags
                                       then "YES" else "NO"),
       ("GHC Dynamic",                 if cDYNAMIC_GHC_PROGRAMS
                                       then "YES" else "NO"),
       ("Leading underscore",          cLeadingUnderscore),
       ("Debug on",                    show debugIsOn),
       ("LibDir",                      topDir dflags),
       ("Global Package DB",           systemPackageConfig dflags)
      ]

#include "../includes/dist-derivedconstants/header/GHCConstantsHaskellWrappers.hs"

bLOCK_SIZE_W :: DynFlags -> Int
bLOCK_SIZE_W dflags = bLOCK_SIZE dflags `quot` wORD_SIZE dflags

wORD_SIZE_IN_BITS :: DynFlags -> Int
wORD_SIZE_IN_BITS dflags = wORD_SIZE dflags * 8

tAG_MASK :: DynFlags -> Int
tAG_MASK dflags = (1 `shiftL` tAG_BITS dflags) - 1

mAX_PTR_TAG :: DynFlags -> Int
mAX_PTR_TAG = tAG_MASK

-- Might be worth caching these in targetPlatform?
tARGET_MIN_INT, tARGET_MAX_INT, tARGET_MAX_WORD :: DynFlags -> Integer
tARGET_MIN_INT dflags
    = case platformWordSize (targetPlatform dflags) of
      4 -> toInteger (minBound :: Int32)
      8 -> toInteger (minBound :: Int64)
      w -> panic ("tARGET_MIN_INT: Unknown platformWordSize: " ++ show w)
tARGET_MAX_INT dflags
    = case platformWordSize (targetPlatform dflags) of
      4 -> toInteger (maxBound :: Int32)
      8 -> toInteger (maxBound :: Int64)
      w -> panic ("tARGET_MAX_INT: Unknown platformWordSize: " ++ show w)
tARGET_MAX_WORD dflags
    = case platformWordSize (targetPlatform dflags) of
      4 -> toInteger (maxBound :: Word32)
      8 -> toInteger (maxBound :: Word64)
      w -> panic ("tARGET_MAX_WORD: Unknown platformWordSize: " ++ show w)

-- Whenever makeDynFlagsConsistent does anything, it starts over, to
-- ensure that a later change doesn't invalidate an earlier check.
-- Be careful not to introduce potential loops!
makeDynFlagsConsistent :: DynFlags -> (DynFlags, [Located String])
makeDynFlagsConsistent dflags
 | hscTarget dflags == HscC &&
   not (platformUnregisterised (targetPlatform dflags))
    = if cGhcWithNativeCodeGen == "YES"
      then let dflags' = dflags { hscTarget = HscAsm }
               warn = "Compiler not unregisterised, so using native code generator rather than compiling via C"
           in loop dflags' warn
      else let dflags' = dflags { hscTarget = HscLlvm }
               warn = "Compiler not unregisterised, so using LLVM rather than compiling via C"
           in loop dflags' warn
 | hscTarget dflags == HscAsm &&
   platformUnregisterised (targetPlatform dflags)
    = loop (dflags { hscTarget = HscC })
           "Compiler unregisterised, so compiling via C"
 | hscTarget dflags == HscAsm &&
   cGhcWithNativeCodeGen /= "YES"
      = let dflags' = dflags { hscTarget = HscLlvm }
            warn = "No native code generator, so using LLVM"
        in loop dflags' warn
 | hscTarget dflags == HscLlvm &&
   not ((arch == ArchX86_64) && (os == OSLinux || os == OSDarwin)) &&
   not ((isARM arch) && (os == OSLinux)) &&
   (not (gopt Opt_Static dflags) || gopt Opt_PIC dflags)
    = if cGhcWithNativeCodeGen == "YES"
      then let dflags' = dflags { hscTarget = HscAsm }
               warn = "Using native code generator rather than LLVM, as LLVM is incompatible with -fPIC and -dynamic on this platform"
           in loop dflags' warn
      else throwGhcException $ CmdLineError "Can't use -fPIC or -dynamic on this platform"
 | os == OSDarwin &&
   arch == ArchX86_64 &&
   not (gopt Opt_PIC dflags)
    = loop (gopt_set dflags Opt_PIC)
           "Enabling -fPIC as it is always on for this platform"
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
-- Do not use it if you can help it. You may get the wrong value!

GLOBAL_VAR(v_unsafeGlobalDynFlags, panic "v_unsafeGlobalDynFlags: not initialised", DynFlags)

unsafeGlobalDynFlags :: DynFlags
unsafeGlobalDynFlags = unsafePerformIO $ readIORef v_unsafeGlobalDynFlags

setUnsafeGlobalDynFlags :: DynFlags -> IO ()
setUnsafeGlobalDynFlags = writeIORef v_unsafeGlobalDynFlags

-- -----------------------------------------------------------------------------
-- SSE

-- TODO: Instead of using a separate predicate (i.e. isSse2Enabled) to
-- check if SSE is enabled, we might have x86-64 imply the -msse2
-- flag.

isSse2Enabled :: DynFlags -> Bool
isSse2Enabled dflags = case platformArch (targetPlatform dflags) of
    ArchX86_64 -> -- SSE2 is fixed on for x86_64.  It would be
                  -- possible to make it optional, but we'd need to
                  -- fix at least the foreign call code where the
                  -- calling convention specifies the use of xmm regs,
                  -- and possibly other places.
                  True
    ArchX86    -> sseVersion dflags >= Just (2,0)
    _          -> False

isSse4_2Enabled :: DynFlags -> Bool
isSse4_2Enabled dflags = sseVersion dflags >= Just (4,2)
