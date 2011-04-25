-- |
-- Dynamic flags
--
--
-- (c) The University of Glasgow 2005
--

-- Most flags are dynamic flags, which means they can change from
-- compilation to compilation using @OPTIONS_GHC@ pragmas, and in a
-- multi-session GHC each session can be using different dynamic
-- flags.  Dynamic flags can also be set at the prompt in GHCi.
module DynFlags (
        -- * Dynamic flags and associated configuration types
        DynFlag(..),
        ExtensionFlag(..),
        glasgowExtsFlags,
        dopt,
        dopt_set,
        dopt_unset,
        xopt,
        xopt_set,
        xopt_unset,
        DynFlags(..),
        RtsOptsEnabled(..),
        HscTarget(..), isObjectTarget, defaultObjectTarget,
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..),
        Option(..), showOpt,
        DynLibLoader(..),
        fFlags, fLangFlags, xFlags,
        DPHBackend(..), dphPackageMaybe,
        wayNames,

        -- ** SafeHaskell
        SafeHaskellMode(..),
        safeHaskellOn, safeLanguageOn,
        safeDirectImpsReq, safeImplicitImpsReq,

        Settings(..),
        ghcUsagePath, ghciUsagePath, topDir, tmpDir, rawSettings,
        extraGccViaCFlags, systemPackageConfig,
        pgm_L, pgm_P, pgm_F, pgm_c, pgm_s, pgm_a, pgm_l, pgm_dll, pgm_T,
        pgm_sysman, pgm_windres, pgm_lo, pgm_lc,
        opt_L, opt_P, opt_F, opt_c, opt_m, opt_a, opt_l,
        opt_windres, opt_lo, opt_lc,


        -- ** Manipulating DynFlags
        defaultDynFlags,                -- Settings -> DynFlags
        initDynFlags,                   -- DynFlags -> IO DynFlags

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlags,
        updOptLevel,
        setTmpDir,
        setPackageName,
        doingTickyProfiling,

        -- ** Parsing DynFlags
        parseDynamicFlagsCmdLine,
        parseDynamicFilePragma,
        allFlags,

        supportedLanguagesAndExtensions,

        -- ** DynFlag C compiler options
        picCCOpts,

        -- * Configuration of the stg-to-stg passes
        StgToDo(..),
        getStgToDo,

        -- * Compiler configuration suitable for display to the user
        compilerInfo
#ifdef GHCI
-- Only in stage 2 can we be sure that the RTS 
-- exposes the appropriate runtime boolean
        , rtsIsProfiled
#endif
  ) where

#include "HsVersions.h"

import Platform
import Module
import PackageConfig
import PrelNames        ( mAIN )
import StaticFlags
import {-# SOURCE #-} Packages (PackageState)
import DriverPhases     ( Phase(..), phaseInputExt )
import Config
import CmdLineParser
import Constants        ( mAX_CONTEXT_REDUCTION_DEPTH )
import Panic
import Util
import Maybes           ( orElse )
import SrcLoc
import FastString
import Outputable
#ifdef GHCI
import Foreign.C	( CInt )
#endif
import {-# SOURCE #-} ErrUtils ( Severity(..), Message, mkLocMessage )

#ifdef GHCI
import System.IO.Unsafe	( unsafePerformIO )
#endif
import Data.IORef
import Control.Monad    ( when )

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath
import System.IO        ( stderr, hPutChar )

-- -----------------------------------------------------------------------------
-- DynFlags

-- | Enumerates the simple on-or-off dynamic flags
data DynFlag

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_raw_cmm
   | Opt_D_dump_cmmz
   | Opt_D_dump_cmmz_pretty
   -- All of the cmmz subflags (there are a lot!)  Automatically
   -- enabled if you run -ddump-cmmz
   | Opt_D_dump_cmmz_cbe
   | Opt_D_dump_cmmz_proc
   | Opt_D_dump_cmmz_spills
   | Opt_D_dump_cmmz_rewrite
   | Opt_D_dump_cmmz_dead
   | Opt_D_dump_cmmz_stub
   | Opt_D_dump_cmmz_sp
   | Opt_D_dump_cmmz_procmap
   | Opt_D_dump_cmmz_split
   | Opt_D_dump_cmmz_lower
   | Opt_D_dump_cmmz_info
   | Opt_D_dump_cmmz_cafs
   -- end cmmz subflags
   | Opt_D_dump_cps_cmm
   | Opt_D_dump_cvt_cmm
   | Opt_D_dump_asm
   | Opt_D_dump_asm_native
   | Opt_D_dump_asm_liveness
   | Opt_D_dump_asm_coalesce
   | Opt_D_dump_asm_regalloc
   | Opt_D_dump_asm_regalloc_stages
   | Opt_D_dump_asm_conflicts
   | Opt_D_dump_asm_stats
   | Opt_D_dump_asm_expanded
   | Opt_D_dump_llvm
   | Opt_D_dump_core_stats
   | Opt_D_dump_cpranal
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_flatC
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_rule_firings
   | Opt_D_dump_rule_rewrites
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
   | Opt_D_dump_cs_trace	-- Constraint solver in type checker
   | Opt_D_dump_tc_trace
   | Opt_D_dump_if_trace
   | Opt_D_dump_vt_trace
   | Opt_D_dump_splices
   | Opt_D_dump_BCOs
   | Opt_D_dump_vect
   | Opt_D_dump_hpc
   | Opt_D_dump_rtti
   | Opt_D_source_stats
   | Opt_D_verbose_core2core
   | Opt_D_verbose_stg2stg
   | Opt_D_dump_hi
   | Opt_D_dump_hi_diffs
   | Opt_D_dump_minimal_imports
   | Opt_D_dump_mod_cycles
   | Opt_D_dump_view_pattern_commoning
   | Opt_D_faststring_stats
   | Opt_DumpToFile                     -- ^ Append dump output to files instead of stdout.
   | Opt_D_no_debug_output
   | Opt_DoCoreLinting
   | Opt_DoStgLinting
   | Opt_DoCmmLinting
   | Opt_DoAsmLinting

   | Opt_WarnIsError                    -- -Werror; makes warnings fatal
   | Opt_WarnDuplicateExports
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
   | Opt_MethodSharing	-- Now a no-op; remove in GHC 7.2
   | Opt_DictsCheap
   | Opt_EnableRewriteRules		-- Apply rewrite rules during simplification
   | Opt_Vectorise
   | Opt_RegsGraph                      -- do graph coloring register allocation
   | Opt_RegsIterative                  -- do iterative coalescing graph coloring register allocation

   -- Interface files
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_ExposeAllUnfoldings

   -- profiling opts
   | Opt_AutoSccsOnAllToplevs
   | Opt_AutoSccsOnExportedToplevs
   | Opt_AutoSccsOnIndividualCafs

   -- misc opts
   | Opt_Pp
   | Opt_ForceRecomp
   | Opt_ExcessPrecision
   | Opt_EagerBlackHoling
   | Opt_ReadUserPackageConf
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
   | Opt_SSE2
   | Opt_GhciSandbox
   | Opt_HelpfulErrors

	-- temporary flags
   | Opt_RunCPS
   | Opt_RunCPSZ
   | Opt_ConvertToZipCfgAndBack
   | Opt_AutoLinkPackages
   | Opt_ImplicitImportQualified
   | Opt_TryNewCodeGen

   -- keeping stuff
   | Opt_KeepHiDiffs
   | Opt_KeepHcFiles
   | Opt_KeepSFiles
   | Opt_KeepTmpFiles
   | Opt_KeepRawTokenStream
   | Opt_KeepLlvmFiles

   deriving (Eq, Show)

data Language = Haskell98 | Haskell2010

-- | The various SafeHaskell modes
data SafeHaskellMode
   = Sf_None
   | Sf_SafeImports
   | Sf_SafeLanguage
   | Sf_Trustworthy
   | Sf_TrustworthyWithSafeLanguage
   | Sf_Safe
   deriving (Eq)

instance Show SafeHaskellMode where
    show Sf_None = "None"
    show Sf_SafeImports = "SafeImports"
    show Sf_SafeLanguage = "SafeLanguage"
    show Sf_Trustworthy = "Trustworthy"
    show Sf_TrustworthyWithSafeLanguage = "Trustworthy + SafeLanguage"
    show Sf_Safe = "Safe"

data ExtensionFlag
   = Opt_Cpp
   | Opt_OverlappingInstances
   | Opt_UndecidableInstances
   | Opt_IncoherentInstances
   | Opt_MonomorphismRestriction
   | Opt_MonoPatBinds
   | Opt_MonoLocalBinds
   | Opt_RelaxedPolyRec		-- Deprecated
   | Opt_ExtendedDefaultRules           -- Use GHC's extended rules for defaulting
   | Opt_ForeignFunctionInterface
   | Opt_UnliftedFFITypes
   | Opt_GHCForeignImportPrim
   | Opt_ParallelArrays                 -- Syntactic support for parallel arrays
   | Opt_Arrows                         -- Arrow-notation syntax
   | Opt_TemplateHaskell
   | Opt_QuasiQuotes
   | Opt_ImplicitParams
   | Opt_ImplicitPrelude
   | Opt_ScopedTypeVariables
   | Opt_UnboxedTuples
   | Opt_BangPatterns
   | Opt_TypeFamilies
   | Opt_OverloadedStrings
   | Opt_DisambiguateRecordFields
   | Opt_RecordWildCards
   | Opt_RecordPuns
   | Opt_ViewPatterns
   | Opt_GADTs
   | Opt_GADTSyntax
   | Opt_NPlusKPatterns
   | Opt_DoAndIfThenElse
   | Opt_RebindableSyntax

   | Opt_StandaloneDeriving
   | Opt_DeriveDataTypeable
   | Opt_DeriveFunctor
   | Opt_DeriveTraversable
   | Opt_DeriveFoldable
   | Opt_DeriveGeneric            -- Allow deriving Generic/1
   | Opt_DefaultSignatures        -- Allow extra signatures for defmeths
   | Opt_Generics                 -- Old generic classes, now deprecated

   | Opt_TypeSynonymInstances
   | Opt_FlexibleContexts
   | Opt_FlexibleInstances
   | Opt_ConstrainedClassMethods
   | Opt_MultiParamTypeClasses
   | Opt_FunctionalDependencies
   | Opt_UnicodeSyntax
   | Opt_PolymorphicComponents
   | Opt_ExistentialQuantification
   | Opt_MagicHash
   | Opt_EmptyDataDecls
   | Opt_KindSignatures
   | Opt_ParallelListComp
   | Opt_TransformListComp
   | Opt_MonadComprehensions
   | Opt_GeneralizedNewtypeDeriving
   | Opt_RecursiveDo
   | Opt_DoRec
   | Opt_PostfixOperators
   | Opt_TupleSections
   | Opt_PatternGuards
   | Opt_LiberalTypeSynonyms
   | Opt_Rank2Types
   | Opt_RankNTypes
   | Opt_ImpredicativeTypes
   | Opt_TypeOperators
   | Opt_PackageImports
   | Opt_ExplicitForAll
   | Opt_AlternativeLayoutRule
   | Opt_AlternativeLayoutRuleTransitional
   | Opt_DatatypeContexts
   | Opt_NondecreasingIndentation
   | Opt_RelaxedLayout
   deriving (Eq, Show)

-- | Contains not only a collection of 'DynFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  ghcMode               :: GhcMode,
  ghcLink               :: GhcLink,
  hscTarget             :: HscTarget,
  hscOutName            :: String,      -- ^ Name of the output file
  extCoreName           :: String,      -- ^ Name of the .hcr output file
  verbosity             :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
  optLevel              :: Int,         -- ^ Optimisation level
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  shouldDumpSimplPhase  :: Maybe String,
  ruleCheck             :: Maybe String,
  strictnessBefore      :: [Int],       -- ^ Additional demand analysis

  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  liberateCaseThreshold :: Maybe Int,   -- ^ Threshold for LiberateCase
  floatLamArgs          :: Maybe Int,   -- ^ Arg count for lambda floating
  			   	 	--   See CoreMonad.FloatOutSwitches

  targetPlatform        :: Platform.Platform, -- ^ The platform we're compiling for. Used by the NCG.
  cmdlineHcIncludes     :: [String],    -- ^ @\-\#includes@
  importPaths           :: [FilePath],
  mainModIs             :: Module,
  mainFunIs             :: Maybe String,
  ctxtStkDepth          :: Int,         -- ^ Typechecker context stack depth

  dphBackend            :: DPHBackend,

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

  objectSuf             :: String,
  hcSuf                 :: String,
  hiSuf                 :: String,

  outputFile            :: Maybe String,
  outputHi              :: Maybe String,
  dynLibLoader          :: DynLibLoader,

  -- | This is set by 'DriverPipeline.runPipeline' based on where
  --    its output is going.
  dumpPrefix            :: Maybe FilePath,

  -- | Override the 'dumpPrefix' set by 'DriverPipeline.runPipeline'.
  --    Set by @-ddump-file-prefix@
  dumpPrefixForce       :: Maybe FilePath,

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

  settings              :: Settings,

  --  For ghc -M
  depMakefile           :: FilePath,
  depIncludePkgDeps     :: Bool,
  depExcludeMods        :: [ModuleName],
  depSuffixes           :: [String],

  --  Package flags
  extraPkgConfs         :: [FilePath],
        -- ^ The @-package-conf@ flags given on the command line, in the order
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

  -- Names of files which were generated from -ddump-to-file; used to
  -- track which ones we need to truncate because it's our first run
  -- through
  generatedDumps        :: IORef (Set FilePath),

  -- hsc dynamic flags
  flags                 :: [DynFlag],
  -- Don't change this without updating extensionFlags:
  language              :: Maybe Language,
  -- | Safe Haskell mode
  safeHaskell           :: SafeHaskellMode,
  -- Don't change this without updating extensionFlags:
  extensions            :: [OnOff ExtensionFlag],
  -- extensionFlags should always be equal to
  --     flattenExtensionFlags language extensions
  extensionFlags        :: [ExtensionFlag],

  -- | Message output action: use "ErrUtils" instead of this if you can
  log_action            :: Severity -> SrcSpan -> PprStyle -> Message -> IO (),

  haddockOptions :: Maybe String
 }

data Settings = Settings {
  sGhcUsagePath          :: FilePath,    -- Filled in by SysTools
  sGhciUsagePath         :: FilePath,    -- ditto
  sTopDir                :: FilePath,
  sTmpDir                :: String,      -- no trailing '/'
  -- You shouldn't need to look things up in rawSettings directly.
  -- They should have their own fields instead.
  sRawSettings           :: [(String, String)],
  sExtraGccViaCFlags     :: [String],
  sSystemPackageConfig   :: FilePath,
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
  sOpt_m                 :: [String],
  sOpt_a                 :: [String],
  sOpt_l                 :: [String],
  sOpt_windres           :: [String],
  sOpt_lo                :: [String], -- LLVM: llvm optimiser
  sOpt_lc                :: [String]  -- LLVM: llc static compiler

 }

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
opt_P dflags = sOpt_P (settings dflags)
opt_F                 :: DynFlags -> [String]
opt_F dflags = sOpt_F (settings dflags)
opt_c                 :: DynFlags -> [String]
opt_c dflags = sOpt_c (settings dflags)
opt_m                 :: DynFlags -> [String]
opt_m dflags = sOpt_m (settings dflags)
opt_a                 :: DynFlags -> [String]
opt_a dflags = sOpt_a (settings dflags)
opt_l                 :: DynFlags -> [String]
opt_l dflags = sOpt_l (settings dflags)
opt_windres           :: DynFlags -> [String]
opt_windres dflags = sOpt_windres (settings dflags)
opt_lo                :: DynFlags -> [String]
opt_lo dflags = sOpt_lo (settings dflags)
opt_lc                :: DynFlags -> [String]
opt_lc dflags = sOpt_lc (settings dflags)

wayNames :: DynFlags -> [WayName]
wayNames = map wayName . ways

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
--    different target or avoid typechecking such modules.  (The latter may
--    preferable for security reasons.)
--
data HscTarget
  = HscC           -- ^ Generate C code.
  | HscAsm         -- ^ Generate assembly using the native code generator.
  | HscLlvm        -- ^ Generate assembly using the llvm code generator.
  | HscInterpreted -- ^ Generate bytecode.  (Requires 'LinkInMemory')
  | HscNothing     -- ^ Don't generate any code.  See notes above.
  deriving (Eq, Show)

showHscTargetFlag :: HscTarget -> String
showHscTargetFlag HscC           = "-fvia-c"
showHscTargetFlag HscAsm         = "-fasm"
showHscTargetFlag HscLlvm        = "-fllvm"
showHscTargetFlag HscInterpreted = "-fbyte-code"
showHscTargetFlag HscNothing     = "-fno-code"

-- | Will this target result in an object file on the disk?
isObjectTarget :: HscTarget -> Bool
isObjectTarget HscC     = True
isObjectTarget HscAsm   = True
isObjectTarget HscLlvm  = True
isObjectTarget _        = False

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

-- Is it worth evaluating this Bool and caching it in the DynFlags value
-- during initDynFlags?
doingTickyProfiling :: DynFlags -> Bool
doingTickyProfiling _ = opt_Ticky
  -- XXX -ticky is a static flag, because it implies -debug which is also
  -- static.  If the way flags were made dynamic, we could fix this.

data PackageFlag
  = ExposePackage  String
  | ExposePackageId String
  | HidePackage    String
  | IgnorePackage  String
  deriving Eq

defaultHscTarget :: HscTarget
defaultHscTarget = defaultObjectTarget

-- | The 'HscTarget' value corresponding to the default way to create
-- object files on the current platform.
defaultObjectTarget :: HscTarget
defaultObjectTarget
  | cGhcUnregisterised    == "YES"      =  HscC
  | cGhcWithNativeCodeGen == "YES"      =  HscAsm
  | otherwise                           =  HscLlvm

data DynLibLoader
  = Deployable
  | SystemDependent
  deriving Eq

data RtsOptsEnabled = RtsOptsNone | RtsOptsSafeOnly | RtsOptsAll
  deriving (Show)

-- | Used by 'GHC.newSession' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 -- someday these will be dynamic flags
 ways <- readIORef v_Ways
 refFilesToClean <- newIORef []
 refDirsToClean <- newIORef Map.empty
 refGeneratedDumps <- newIORef Set.empty
 return dflags{
        ways            = ways,
        buildTag        = mkBuildTag (filter (not . wayRTSOnly) ways),
        rtsBuildTag     = mkBuildTag ways,
        filesToClean    = refFilesToClean,
        dirsToClean     = refDirsToClean,
        generatedDumps   = refGeneratedDumps
        }

-- | The normal 'DynFlags'. Note that they is not suitable for use in this form
-- and must be fully initialized by 'GHC.newSession' first.
defaultDynFlags :: Settings -> DynFlags
defaultDynFlags mySettings =
     DynFlags {
        ghcMode                 = CompManager,
        ghcLink                 = LinkBinary,
        hscTarget               = defaultHscTarget,
        hscOutName              = "",
        extCoreName             = "",
        verbosity               = 0,
        optLevel                = 0,
        simplPhases             = 2,
        maxSimplIterations      = 4,
        shouldDumpSimplPhase    = Nothing,
        ruleCheck               = Nothing,
        specConstrThreshold     = Just 2000,
        specConstrCount         = Just 3,
        liberateCaseThreshold   = Just 2000,
        floatLamArgs            = Just 0,	-- Default: float only if no fvs
        strictnessBefore        = [],

        targetPlatform          = defaultTargetPlatform,
        cmdlineHcIncludes       = [],
        importPaths             = ["."],
        mainModIs               = mAIN,
        mainFunIs               = Nothing,
        ctxtStkDepth            = mAX_CONTEXT_REDUCTION_DEPTH,

        dphBackend              = DPHNone,

        thisPackage             = mainPackageId,

        objectDir               = Nothing,
        dylibInstallName        = Nothing,
        hiDir                   = Nothing,
        stubDir                 = Nothing,

        objectSuf               = phaseInputExt StopLn,
        hcSuf                   = phaseInputExt HCc,
        hiSuf                   = "hi",

        pluginModNames          = [],
        pluginModNameOpts       = [],

        outputFile              = Nothing,
        outputHi                = Nothing,
        dynLibLoader            = SystemDependent,
        dumpPrefix              = Nothing,
        dumpPrefixForce         = Nothing,
        includePaths            = [],
        libraryPaths            = [],
        frameworkPaths          = [],
        cmdlineFrameworks       = [],
        rtsOpts                 = Nothing,
        rtsOptsEnabled          = RtsOptsSafeOnly,

        hpcDir                  = ".hpc",

        extraPkgConfs           = [],
        packageFlags            = [],
        pkgDatabase             = Nothing,
        pkgState                = panic "no package state yet: call GHC.setSessionDynFlags",
        ways                    = panic "defaultDynFlags: No ways",
        buildTag                = panic "defaultDynFlags: No buildTag",
        rtsBuildTag             = panic "defaultDynFlags: No rtsBuildTag",
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
        generatedDumps = panic "defaultDynFlags: No generatedDumps",
        haddockOptions = Nothing,
        flags = defaultFlags,
        language = Nothing,
        safeHaskell = Sf_None,
        extensions = [],
        extensionFlags = flattenExtensionFlags Nothing [],

        log_action = \severity srcSpan style msg ->
                        case severity of
                          SevOutput -> printSDoc msg style
                          SevInfo   -> printErrs msg style
                          SevFatal  -> printErrs msg style
                          _         -> do 
                                hPutChar stderr '\n'
                                printErrs (mkLocMessage srcSpan msg) style
                     -- careful (#2302): printErrs prints in UTF-8, whereas
                     -- converting to string first and using hPutStr would
                     -- just emit the low 8 bits of each unicode char.
      }

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
flattenExtensionFlags :: Maybe Language -> [OnOff ExtensionFlag]
                      -> [ExtensionFlag]
flattenExtensionFlags ml = foldr f defaultExtensionFlags
    where f (On f)  flags = f : delete f flags
          f (Off f) flags =     delete f flags
          defaultExtensionFlags = languageExtensions ml

languageExtensions :: Maybe Language -> [ExtensionFlag]

languageExtensions Nothing
    -- Nothing => the default case
    = Opt_MonoPatBinds   -- Experimentally, I'm making this non-standard
                         -- behaviour the default, to see if anyone notices
                         -- SLPJ July 06
      -- In due course I'd like Opt_MonoLocalBinds to be on by default
      -- But NB it's implied by GADTs etc
      -- SLPJ September 2010
    : Opt_NondecreasingIndentation -- This has been on by default for some time
    : delete Opt_DatatypeContexts  -- The Haskell' committee decided to
                                   -- remove datatype contexts from the
                                   -- language:
   -- http://www.haskell.org/pipermail/haskell-prime/2011-January/003335.html
      (languageExtensions (Just Haskell2010))

languageExtensions (Just Haskell98)
    = [Opt_ImplicitPrelude,
       Opt_MonomorphismRestriction,
       Opt_NPlusKPatterns,
       Opt_DatatypeContexts,
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
       Opt_EmptyDataDecls,
       Opt_ForeignFunctionInterface,
       Opt_PatternGuards,
       Opt_DoAndIfThenElse,
       Opt_RelaxedPolyRec]

-- | Test whether a 'DynFlag' is set
dopt :: DynFlag -> DynFlags -> Bool
dopt f dflags  = f `elem` (flags dflags)

-- | Set a 'DynFlag'
dopt_set :: DynFlags -> DynFlag -> DynFlags
dopt_set dfs f = dfs{ flags = f : flags dfs }

-- | Unset a 'DynFlag'
dopt_unset :: DynFlags -> DynFlag -> DynFlags
dopt_unset dfs f = dfs{ flags = filter (/= f) (flags dfs) }

-- | Test whether a 'ExtensionFlag' is set
xopt :: ExtensionFlag -> DynFlags -> Bool
xopt f dflags = f `elem` extensionFlags dflags

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

-- | Set the Haskell language standard to use
setLanguage :: Language -> DynP ()
setLanguage l = upd f
    where f dfs = let mLang = Just l
                      oneoffs = extensions dfs
                  in dfs {
                         language = mLang,
                         extensionFlags = flattenExtensionFlags mLang oneoffs
                     }

safeLanguageOn :: DynFlags -> Bool
safeLanguageOn dflags = s == Sf_SafeLanguage || s == Sf_Safe
                          where s = safeHaskell dflags

-- | Test if SafeHaskell is on in some form
safeHaskellOn :: DynFlags -> Bool
safeHaskellOn dflags = safeHaskell dflags /= Sf_None

-- | Set a 'SafeHaskell' flag
setSafeHaskell :: SafeHaskellMode -> DynP ()
setSafeHaskell s = upd f
    where f dfs = let sf = safeHaskell dfs
                  in dfs {
                         safeHaskell = combineSafeFlags sf s
                     }

-- | Are all direct imports required to be safe for this SafeHaskell mode?
-- Direct imports are when the code explicitly imports a module
safeDirectImpsReq :: DynFlags -> Bool
safeDirectImpsReq = safeLanguageOn

-- | Are all implicit imports required to be safe for this SafeHaskell mode?
-- Implicit imports are things in the prelude. e.g System.IO when print is used.
safeImplicitImpsReq :: DynFlags -> Bool
safeImplicitImpsReq _ = False

-- | Combine two SafeHaskell modes correctly. Used for dealing with multiple flags.
-- This makes SafeHaskell very much a monoid but for now I prefer this as I don't
-- want to export this functionality from the module but do want to export the
-- type constructors.
combineSafeFlags :: SafeHaskellMode -> SafeHaskellMode -> SafeHaskellMode
combineSafeFlags a b =
    case (a,b) of
        (Sf_None, sf) -> sf
        (sf, Sf_None) -> sf

        (Sf_SafeImports, sf) -> sf
        (sf, Sf_SafeImports) -> sf

        (Sf_SafeLanguage, Sf_Safe) -> err
        (Sf_Safe, Sf_SafeLanguage) -> err

        (Sf_SafeLanguage, Sf_Trustworthy) -> Sf_TrustworthyWithSafeLanguage
        (Sf_Trustworthy, Sf_SafeLanguage) -> Sf_TrustworthyWithSafeLanguage

        (Sf_TrustworthyWithSafeLanguage, Sf_Trustworthy)  -> Sf_TrustworthyWithSafeLanguage
        (Sf_TrustworthyWithSafeLanguage, Sf_SafeLanguage) -> Sf_TrustworthyWithSafeLanguage
        (Sf_Trustworthy, Sf_TrustworthyWithSafeLanguage)  -> Sf_TrustworthyWithSafeLanguage
        (Sf_SafeLanguage, Sf_TrustworthyWithSafeLanguage) -> Sf_TrustworthyWithSafeLanguage

        (Sf_Trustworthy, Sf_Safe) -> err
        (Sf_Safe, Sf_Trustworthy) -> err

        (a,b) | a == b -> a
              | otherwise -> err

    where err = ghcError (CmdLineError $ "Incompatible SafeHaskell flags! ("
                                        ++ show a ++ "," ++ show b ++ ")")

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

setObjectDir, setHiDir, setStubDir, setOutputDir, setDylibInstallName,
         setObjectSuf, setHiSuf, setHcSuf, parseDynLibLoaderMode,
         setPgmP, addOptl, addOptP,
         addCmdlineFramework, addHaddockOpts
   :: String -> DynFlags -> DynFlags
setOutputFile, setOutputHi, setDumpPrefixForce
   :: Maybe String -> DynFlags -> DynFlags

setObjectDir  f d = d{ objectDir  = Just f}
setHiDir      f d = d{ hiDir      = Just f}
setStubDir    f d = d{ stubDir    = Just f, includePaths = f : includePaths d }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling via C (i.e. unregisterised
  -- builds).
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f
setDylibInstallName  f d = d{ dylibInstallName = Just f}

setObjectSuf  f d = d{ objectSuf  = f}
setHiSuf      f d = d{ hiSuf      = f}
setHcSuf      f d = d{ hcSuf      = f}

setOutputFile f d = d{ outputFile = f}
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
   _                    -> ghcError (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f = let (pgm:args) = words f in alterSettings (\s -> s { sPgm_P   = (pgm, map Option args)})
addOptl   f = alterSettings (\s -> s { sOpt_l   = f : sOpt_l s})
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
   dfs1 = foldr (flip dopt_unset) dfs  remove_dopts
   dfs2 = foldr (flip dopt_set)   dfs1 extra_dopts

   extra_dopts  = [ f | (ns,f) <- optLevelFlags, final_n `elem` ns ]
   remove_dopts = [ f | (ns,f) <- optLevelFlags, final_n `notElem` ns ]

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
        stg_stats = dopt Opt_StgStats dflags

        todo1 = if stg_stats then [D_stg_stats] else []

        todo2 | WayProf `elem` wayNames dflags
              = StgDoMassageForProfiling : todo1
              | otherwise
              = todo1

{- **********************************************************************
%*									*
		DynFlags parser
%*									*
%********************************************************************* -}

-- -----------------------------------------------------------------------------
-- Parsing the dynamic flags.

-- | Parse dynamic flags from a list of command line arguments.  Returns the
-- the parsed 'DynFlags', the left-over arguments, and a list of warnings.
-- Throws a 'UsageError' if errors occurred during parsing (such as unknown
-- flags or missing arguments).
parseDynamicFlagsCmdLine :: Monad m =>
                     DynFlags -> [Located String]
                  -> m (DynFlags, [Located String], [Located String])
                     -- ^ Updated 'DynFlags', left-over arguments, and
                     -- list of warnings.
parseDynamicFlagsCmdLine dflags args = parseDynamicFlags dflags args True

-- | Like 'parseDynamicFlagsCmdLine' but does not allow the package flags
-- (-package, -hide-package, -ignore-package, -hide-all-packages, -package-conf).
-- Used to parse flags set in a modules pragma.
parseDynamicFilePragma :: Monad m =>
                     DynFlags -> [Located String]
                  -> m (DynFlags, [Located String], [Located String])
                     -- ^ Updated 'DynFlags', left-over arguments, and
                     -- list of warnings.
parseDynamicFilePragma dflags args = parseDynamicFlags dflags args False

parseDynamicFlags :: Monad m =>
                      DynFlags -> [Located String] -> Bool
                  -> m (DynFlags, [Located String], [Located String])
parseDynamicFlags dflags0 args cmdline = do
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

      -- Note: -ignore-package (package_flags) must precede -i* (dynamic_flags)
      flag_spec | cmdline   = package_flags ++ dynamic_flags
                | otherwise = dynamic_flags

  let safeLevel = if safeLanguageOn dflags0
                     then determineSafeLevel cmdline else NeverAllowed
  let ((leftover, errs, warns), dflags1)
          = runCmdLine (processArgs flag_spec args' safeLevel cmdline) dflags0
  when (not (null errs)) $ ghcError $ errorsToGhcException errs

  -- check for disabled flags in safe haskell
  -- Hack: unfortunately flags that are completely disabled can't be stopped from being
  -- enabled on the command line before a -XSafe or -XSafeLanguage flag is encountered.
  -- the easiest way to fix this is to just check that they aren't enabled now. The down
  -- side is that flags marked as NeverAllowed must also be checked here placing a sync
  -- burden on the ghc hacker.
  let (dflags2, sh_warns) = if (safeLanguageOn dflags1)
                                then shFlagsDisallowed dflags1
                                else (dflags1, [])

  return (dflags2, leftover, sh_warns ++ warns)

-- | Extensions that can't be enabled at all when compiling in Safe mode
-- checkSafeHaskellFlags :: MonadIO m => DynFlags -> m ()
shFlagsDisallowed :: DynFlags -> (DynFlags, [Located String])
shFlagsDisallowed dflags = foldl check_method (dflags, []) bad_flags
    where
        check_method (df, warns) (test,str,fix)
            | test df   = (fix df, warns ++ safeFailure str)
            | otherwise = (df, warns)

        bad_flags = [(xopt Opt_GeneralizedNewtypeDeriving, "-XGeneralizedNewtypeDeriving",
                     flip xopt_unset Opt_GeneralizedNewtypeDeriving),
                     (xopt Opt_TemplateHaskell, "-XTemplateHaskell",
                     flip xopt_unset Opt_TemplateHaskell)]

        safeFailure str = [L noSrcSpan $ "Warning2: " ++ str ++ " is not allowed in"
                                      ++ " SafeHaskell; ignoring " ++ str]

{-
    -- ALTERNATE SAFE HASKELL CHECK METHOD

-- | Extensions that can only be enabled on the command line when compiling in
-- Safe mode
shFlagsCmdLineOnly :: Monad m => DynFlags -> DynFlags -> m ()
shFlagsCmdLineOnly oldf newf = mapM_ check_method bad_flags
    where
        check_method (test,str) = when test $ safeFailure str
        
        ext_test ext = xopt ext newf && not (xopt ext oldf)
        pgm_test pgm = pgm oldf == pgm newf
        dyn_test dyn = dopt dyn newf && not (dopt dyn oldf)

        bad_flags = [ (ext_test Opt_TemplateHaskell, "TemplateHaskell")
                    , (ext_test Opt_Cpp,             "CPP")
                    , (dyn_test Opt_Pp,              "F")

                    , (pgm_test pgm_lo,              "pgmlo")
                    , (pgm_test pgm_lc,              "pgmlc")
                    , (pgm_test pgm_L,               "pgmL")
                    , (pgm_test pgm_P,               "pgmP")
                    , (pgm_test pgm_F,               "pgmF")
                    , (pgm_test pgm_c,               "pgmc")
                    , (pgm_test pgm_m,               "pgmm")
                    , (pgm_test pgm_s,               "pgms")
                    , (pgm_test pgm_a,               "pgma")
                    , (pgm_test pgm_l,               "pgml")
                    , (pgm_test pgm_dll,             "pgmdll")
                    , (pgm_test pgm_windres,         "pgmwindres")

                    , (pgm_test opt_lo,              "optlo")
                    , (pgm_test opt_lc,              "optlc")
                    , (pgm_test opt_L,               "optL")
                    , (pgm_test opt_P,               "optP")
                    , (pgm_test opt_F,               "optF")
                    , (pgm_test opt_c,               "optc")
                    , (pgm_test opt_m,               "optm")
                    , (pgm_test opt_a,               "opta")
                    , (pgm_test opt_l,               "optl OR l")
                    , (pgm_test opt_windres,         "optlwindres")

                    , (pgm_test mainFunIs
                       && pgm_test mainModIs,        "main-is")
                    , (pgm_test libraryPaths,        "L")
                    , (pgm_test dynLibLoader,        "dynload")

                    , (pgm_test hcSuf,               "hcsuf")
                    , (pgm_test hiSuf,               "hisuf")
                    , (pgm_test objectSuf,           "osuf")
                    , (pgm_test hiDir,               "hidir")
                    , (pgm_test objectDir,           "odir")
                    , (pgm_test stubDir,             "stubdir")
                    , (pgm_test outputHi,            "ohi")
                    , (pgm_test outputFile,          "o")
                    , (pgm_test tmpDir,              "tmpdir")

                    , (pgm_test includePaths,        "I")

                    , (pgm_test rtsOpts,             "with-rtsopts")
                    , (pgm_test rtsOptsEnabled,      "rtsopts")

                    , (pgm_test dylibInstallName,    "dylib-install-name")
                    ]

-- safeFailure :: MonadIO m => String -> m ()
safeFailure :: Monad m => String -> m ()
safeFailure s = ghcError $ CmdLineError $ "Illegal extension (" ++ s
                    ++ ") in use while compiling with Safe Haskell!"
{-
  -- prefer this error but circular imports arise.
  = liftIO $ throwIO $ mkSrcErr $ unitBag $ mkPlainErrMsg noSrcSpan $
      text "Illegal extension (" <> text s <>
          text ") in use while compiling with Safe Haskell!"
-}
-}


{- **********************************************************************
%*									*
		DynFlags specifications
%*									*
%********************************************************************* -}

allFlags :: [String]
allFlags = map ('-':) $
           [ flagName flag | flag <- dynamic_flags, ok (flagOptKind flag) ] ++
           map ("fno-"++) flags ++
           map ("f"++) flags ++
           map ("f"++) flags' ++
           map ("X"++) supportedExtensions
    where ok (PrefixPred _ _) = False
          ok _   = True
          flags  = [ name | (name, _, _, _) <- fFlags ]
          flags' = [ name | (name, _, _, _) <- fLangFlags ]

--------------- The main flags themselves ------------------
dynamic_flags :: [Flag (CmdLineP DynFlags)]
dynamic_flags = [
    flagA "n"        (NoArg (addWarn "The -n flag is deprecated and no longer has any effect"))
  , flagC "cpp"      (NoArg (setExtensionFlag Opt_Cpp)) 
  , flagC "F"        (NoArg (setDynFlag Opt_Pp)) 
  , flagA "#include" 
         (HasArg (\s -> do { addCmdlineHCInclude s
                           ; addWarn "-#include and INCLUDE pragmas are deprecated: They no longer have any effect" }))
  , flagA "v"        (OptIntSuffix setVerbosity)

        ------- Specific phases  --------------------------------------------
    -- need to appear before -pgmL to be parsed as LLVM flags.
  , flagC "pgmlo"          (hasArg (\f -> alterSettings (\s -> s { sPgm_lo  = (f,[])})))
  , flagC "pgmlc"          (hasArg (\f -> alterSettings (\s -> s { sPgm_lc  = (f,[])})))
  , flagC "pgmL"           (hasArg (\f -> alterSettings (\s -> s { sPgm_L   = f})))
  , flagC "pgmP"           (hasArg setPgmP)
  , flagC "pgmF"           (hasArg (\f -> alterSettings (\s -> s { sPgm_F   = f})))
  , flagC "pgmc"           (hasArg (\f -> alterSettings (\s -> s { sPgm_c   = (f,[])})))
  , flagC "pgmm"           (HasArg (\_ -> addWarn "The -keep-raw-s-files flag does nothing; it will be removed in a future GHC release"))
  , flagC "pgms"           (hasArg (\f -> alterSettings (\s -> s { sPgm_s   = (f,[])})))
  , flagC "pgma"           (hasArg (\f -> alterSettings (\s -> s { sPgm_a   = (f,[])})))
  , flagC "pgml"           (hasArg (\f -> alterSettings (\s -> s { sPgm_l   = (f,[])})))
  , flagC "pgmdll"         (hasArg (\f -> alterSettings (\s -> s { sPgm_dll = (f,[])})))
  , flagC "pgmwindres"     (hasArg (\f -> alterSettings (\s -> s { sPgm_windres = f})))

    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , flagC "optlo"          (hasArg (\f -> alterSettings (\s -> s { sOpt_lo  = f : sOpt_lo s})))
  , flagC "optlc"          (hasArg (\f -> alterSettings (\s -> s { sOpt_lc  = f : sOpt_lc s})))
  , flagC "optL"           (hasArg (\f -> alterSettings (\s -> s { sOpt_L   = f : sOpt_L s})))
  , flagC "optP"           (hasArg addOptP)
  , flagC "optF"           (hasArg (\f -> alterSettings (\s -> s { sOpt_F   = f : sOpt_F s})))
  , flagC "optc"           (hasArg (\f -> alterSettings (\s -> s { sOpt_c   = f : sOpt_c s})))
  , flagC "optm"           (hasArg (\f -> alterSettings (\s -> s { sOpt_m   = f : sOpt_m s})))
  , flagC "opta"           (hasArg (\f -> alterSettings (\s -> s { sOpt_a   = f : sOpt_a s})))
  , flagC "optl"           (hasArg addOptl)
  , flagC "optwindres"     (hasArg (\f -> alterSettings (\s -> s { sOpt_windres = f : sOpt_windres s})))

  , flagC "split-objs"
         (NoArg (if can_split 
                 then setDynFlag Opt_SplitObjs
                 else addWarn "ignoring -fsplit-objs"))

        -------- ghc -M -----------------------------------------------------
  , flagA "dep-suffix"     (hasArg addDepSuffix)
  , flagA "optdep-s"       (hasArgDF addDepSuffix "Use -dep-suffix instead")
  , flagA "dep-makefile"   (hasArg setDepMakefile)
  , flagA "optdep-f"       (hasArgDF setDepMakefile "Use -dep-makefile instead")
  , flagA "optdep-w"       (NoArg  (deprecate "doesn't do anything"))
  , flagA "include-pkg-deps"         (noArg (setDepIncludePkgDeps True))
  , flagA "optdep--include-prelude"  (noArgDF (setDepIncludePkgDeps True) "Use -include-pkg-deps instead")
  , flagA "optdep--include-pkg-deps" (noArgDF (setDepIncludePkgDeps True) "Use -include-pkg-deps instead")
  , flagA "exclude-module"           (hasArg addDepExcludeMod)
  , flagA "optdep--exclude-module"   (hasArgDF addDepExcludeMod "Use -exclude-module instead")
  , flagA "optdep-x"                 (hasArgDF addDepExcludeMod "Use -exclude-module instead")

        -------- Linking ----------------------------------------------------
  , flagA "no-link"            (noArg (\d -> d{ ghcLink=NoLink }))
  , flagA "shared"             (noArg (\d -> d{ ghcLink=LinkDynLib }))
  , flagC "dynload"            (hasArg parseDynLibLoaderMode)
  , flagC "dylib-install-name" (hasArg setDylibInstallName)

        ------- Libraries ---------------------------------------------------
  , flagC "L"   (Prefix addLibraryPath)
  , flagC "l"   (hasArg (addOptl . ("-l" ++)))

        ------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  , flagC "framework-path" (HasArg addFrameworkPath)
  , flagC "framework"      (hasArg addCmdlineFramework)

        ------- Output Redirection ------------------------------------------
  , flagC "odir"              (hasArg setObjectDir)
  , flagC "o"                 (SepArg (upd . setOutputFile . Just))
  , flagC "ohi"               (hasArg (setOutputHi . Just ))
  , flagC "osuf"              (hasArg setObjectSuf)
  , flagC "hcsuf"             (hasArg setHcSuf)
  , flagC "hisuf"             (hasArg setHiSuf)
  , flagC "hidir"             (hasArg setHiDir)
  , flagC "tmpdir"            (hasArg setTmpDir)
  , flagC "stubdir"           (hasArg setStubDir)
  , flagC "outputdir"         (hasArg setOutputDir)
  , flagC "ddump-file-prefix" (hasArg (setDumpPrefixForce . Just))

        ------- Keeping temporary files -------------------------------------
     -- These can be singular (think ghc -c) or plural (think ghc --make)
  , flagA "keep-hc-file"     (NoArg (setDynFlag Opt_KeepHcFiles))
  , flagA "keep-hc-files"    (NoArg (setDynFlag Opt_KeepHcFiles))
  , flagA "keep-s-file"      (NoArg (setDynFlag Opt_KeepSFiles))
  , flagA "keep-s-files"     (NoArg (setDynFlag Opt_KeepSFiles))
  , flagA "keep-raw-s-file"  (NoArg (addWarn "The -keep-raw-s-file flag does nothing; it will be removed in a future GHC release"))
  , flagA "keep-raw-s-files" (NoArg (addWarn "The -keep-raw-s-files flag does nothing; it will be removed in a future GHC release"))
  , flagA "keep-llvm-file"   (NoArg (setDynFlag Opt_KeepLlvmFiles))
  , flagA "keep-llvm-files"  (NoArg (setDynFlag Opt_KeepLlvmFiles))
     -- This only makes sense as plural
  , flagA "keep-tmp-files"   (NoArg (setDynFlag Opt_KeepTmpFiles))

        ------- Miscellaneous ----------------------------------------------
  , flagA "no-auto-link-packages" (NoArg (unSetDynFlag Opt_AutoLinkPackages))
  , flagA "no-hs-main"     (NoArg (setDynFlag Opt_NoHsMain))
  , flagC "with-rtsopts"   (HasArg setRtsOpts)
  , flagC "rtsopts"        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , flagC "rtsopts=all"    (NoArg (setRtsOptsEnabled RtsOptsAll))
  , flagC "rtsopts=some"   (NoArg (setRtsOptsEnabled RtsOptsSafeOnly))
  , flagC "rtsopts=none"   (NoArg (setRtsOptsEnabled RtsOptsNone))
  , flagA "no-rtsopts"     (NoArg (setRtsOptsEnabled RtsOptsNone))
  , flagC "main-is"        (SepArg setMainIs)
  , flagA "haddock"        (NoArg (setDynFlag Opt_Haddock))
  , flagA "haddock-opts"   (hasArg addHaddockOpts)
  , flagA "hpcdir"         (SepArg setOptHpcDir)

        ------- recompilation checker --------------------------------------
  , flagA "recomp"         (NoArg (do { unSetDynFlag Opt_ForceRecomp
                                     ; deprecate "Use -fno-force-recomp instead" }))
  , flagA "no-recomp"      (NoArg (do { setDynFlag Opt_ForceRecomp
                                     ; deprecate "Use -fforce-recomp instead" }))

        ------ HsCpp opts ---------------------------------------------------
  , flagC "D"              (AnySuffix (upd . addOptP))
  , flagC "U"              (AnySuffix (upd . addOptP))

        ------- Include/Import Paths ----------------------------------------
  , flagC "I"              (Prefix    addIncludePath)
  , flagC "i"              (OptPrefix addImportPath)

        ------ Debugging ----------------------------------------------------
  , flagA "dstg-stats"     (NoArg (setDynFlag Opt_StgStats))

  , flagA "ddump-cmm"               (setDumpFlag Opt_D_dump_cmm)
  , flagA "ddump-raw-cmm"           (setDumpFlag Opt_D_dump_raw_cmm)
  , flagA "ddump-cmmz"              (setDumpFlag Opt_D_dump_cmmz)
  , flagA "ddump-cmmz-pretty"       (setDumpFlag Opt_D_dump_cmmz_pretty)
  , flagA "ddump-cmmz-cbe"          (setDumpFlag Opt_D_dump_cmmz_cbe)
  , flagA "ddump-cmmz-spills"       (setDumpFlag Opt_D_dump_cmmz_spills)
  , flagA "ddump-cmmz-proc"         (setDumpFlag Opt_D_dump_cmmz_proc)
  , flagA "ddump-cmmz-rewrite"      (setDumpFlag Opt_D_dump_cmmz_rewrite)
  , flagA "ddump-cmmz-dead"         (setDumpFlag Opt_D_dump_cmmz_dead)
  , flagA "ddump-cmmz-stub"         (setDumpFlag Opt_D_dump_cmmz_stub)
  , flagA "ddump-cmmz-sp"           (setDumpFlag Opt_D_dump_cmmz_sp)
  , flagA "ddump-cmmz-procmap"      (setDumpFlag Opt_D_dump_cmmz_procmap)
  , flagA "ddump-cmmz-split"        (setDumpFlag Opt_D_dump_cmmz_split)
  , flagA "ddump-cmmz-lower"        (setDumpFlag Opt_D_dump_cmmz_lower)
  , flagA "ddump-cmmz-info"         (setDumpFlag Opt_D_dump_cmmz_info)
  , flagA "ddump-cmmz-cafs"         (setDumpFlag Opt_D_dump_cmmz_cafs)
  , flagA "ddump-core-stats"        (setDumpFlag Opt_D_dump_core_stats)
  , flagA "ddump-cps-cmm"           (setDumpFlag Opt_D_dump_cps_cmm)
  , flagA "ddump-cvt-cmm"           (setDumpFlag Opt_D_dump_cvt_cmm)
  , flagA "ddump-asm"               (setDumpFlag Opt_D_dump_asm)
  , flagA "ddump-asm-native"        (setDumpFlag Opt_D_dump_asm_native)
  , flagA "ddump-asm-liveness"      (setDumpFlag Opt_D_dump_asm_liveness)
  , flagA "ddump-asm-coalesce"      (setDumpFlag Opt_D_dump_asm_coalesce)
  , flagA "ddump-asm-regalloc"      (setDumpFlag Opt_D_dump_asm_regalloc)
  , flagA "ddump-asm-conflicts"     (setDumpFlag Opt_D_dump_asm_conflicts)
  , flagA "ddump-asm-regalloc-stages" (setDumpFlag Opt_D_dump_asm_regalloc_stages)
  , flagA "ddump-asm-stats"         (setDumpFlag Opt_D_dump_asm_stats)
  , flagA "ddump-asm-expanded"      (setDumpFlag Opt_D_dump_asm_expanded)
  , flagA "ddump-llvm"              (NoArg (do { setObjTarget HscLlvm
                                               ; setDumpFlag' Opt_D_dump_llvm}))
  , flagA "ddump-cpranal"           (setDumpFlag Opt_D_dump_cpranal)
  , flagA "ddump-deriv"             (setDumpFlag Opt_D_dump_deriv)
  , flagA "ddump-ds"                (setDumpFlag Opt_D_dump_ds)
  , flagA "ddump-flatC"             (setDumpFlag Opt_D_dump_flatC)
  , flagA "ddump-foreign"           (setDumpFlag Opt_D_dump_foreign)
  , flagA "ddump-inlinings"         (setDumpFlag Opt_D_dump_inlinings)
  , flagA "ddump-rule-firings"      (setDumpFlag Opt_D_dump_rule_firings)
  , flagA "ddump-rule-rewrites"     (setDumpFlag Opt_D_dump_rule_rewrites)
  , flagA "ddump-occur-anal"        (setDumpFlag Opt_D_dump_occur_anal)
  , flagA "ddump-parsed"            (setDumpFlag Opt_D_dump_parsed)
  , flagA "ddump-rn"                (setDumpFlag Opt_D_dump_rn)
  , flagA "ddump-core-pipeline"     (setDumpFlag Opt_D_dump_core_pipeline)
  , flagA "ddump-simpl"             (setDumpFlag Opt_D_dump_simpl)
  , flagA "ddump-simpl-iterations"  (setDumpFlag Opt_D_dump_simpl_iterations)
  , flagA "ddump-simpl-phases"      (OptPrefix setDumpSimplPhases)
  , flagA "ddump-spec"              (setDumpFlag Opt_D_dump_spec)
  , flagA "ddump-prep"              (setDumpFlag Opt_D_dump_prep)
  , flagA "ddump-stg"               (setDumpFlag Opt_D_dump_stg)
  , flagA "ddump-stranal"           (setDumpFlag Opt_D_dump_stranal)
  , flagA "ddump-tc"                (setDumpFlag Opt_D_dump_tc)
  , flagA "ddump-types"             (setDumpFlag Opt_D_dump_types)
  , flagA "ddump-rules"             (setDumpFlag Opt_D_dump_rules)
  , flagA "ddump-cse"               (setDumpFlag Opt_D_dump_cse)
  , flagA "ddump-worker-wrapper"    (setDumpFlag Opt_D_dump_worker_wrapper)
  , flagA "ddump-rn-trace"          (setDumpFlag Opt_D_dump_rn_trace)
  , flagA "ddump-if-trace"          (setDumpFlag Opt_D_dump_if_trace)
  , flagA "ddump-cs-trace"          (setDumpFlag Opt_D_dump_cs_trace)
  , flagA "ddump-tc-trace"          (setDumpFlag Opt_D_dump_tc_trace)
  , flagA "ddump-vt-trace"          (setDumpFlag Opt_D_dump_vt_trace)
  , flagA "ddump-splices"           (setDumpFlag Opt_D_dump_splices)
  , flagA "ddump-rn-stats"          (setDumpFlag Opt_D_dump_rn_stats)
  , flagA "ddump-opt-cmm"           (setDumpFlag Opt_D_dump_opt_cmm)
  , flagA "ddump-simpl-stats"       (setDumpFlag Opt_D_dump_simpl_stats)
  , flagA "ddump-bcos"              (setDumpFlag Opt_D_dump_BCOs)
  , flagA "dsource-stats"           (setDumpFlag Opt_D_source_stats)
  , flagA "dverbose-core2core"      (NoArg (do { setVerbosity (Just 2)
                                               ; setVerboseCore2Core }))
  , flagA "dverbose-stg2stg"        (setDumpFlag Opt_D_verbose_stg2stg)
  , flagA "ddump-hi"                (setDumpFlag Opt_D_dump_hi)
  , flagA "ddump-minimal-imports"   (setDumpFlag Opt_D_dump_minimal_imports)
  , flagA "ddump-vect"              (setDumpFlag Opt_D_dump_vect)
  , flagA "ddump-hpc"               (setDumpFlag Opt_D_dump_hpc)
  , flagA "ddump-mod-cycles"        (setDumpFlag Opt_D_dump_mod_cycles)
  , flagA "ddump-view-pattern-commoning" (setDumpFlag Opt_D_dump_view_pattern_commoning)
  , flagA "ddump-to-file"           (setDumpFlag Opt_DumpToFile)
  , flagA "ddump-hi-diffs"          (setDumpFlag Opt_D_dump_hi_diffs)
  , flagA "ddump-rtti"      	   (setDumpFlag Opt_D_dump_rtti)
  , flagA "dcore-lint"              (NoArg (setDynFlag Opt_DoCoreLinting))
  , flagA "dstg-lint"               (NoArg (setDynFlag Opt_DoStgLinting))
  , flagA "dcmm-lint"               (NoArg (setDynFlag Opt_DoCmmLinting))
  , flagA "dasm-lint"               (NoArg (setDynFlag Opt_DoAsmLinting))
  , flagA "dshow-passes"            (NoArg (do forceRecompile
                                               setVerbosity (Just 2)))
  , flagA "dfaststring-stats"       (NoArg (setDynFlag Opt_D_faststring_stats))

        ------ Machine dependant (-m<blah>) stuff ---------------------------

  , flagA "monly-2-regs" (NoArg (addWarn "The -monly-2-regs flag does nothing; it will be removed in a future GHC release"))
  , flagA "monly-3-regs" (NoArg (addWarn "The -monly-3-regs flag does nothing; it will be removed in a future GHC release"))
  , flagA "monly-4-regs" (NoArg (addWarn "The -monly-4-regs flag does nothing; it will be removed in a future GHC release"))
  , flagA "msse2"        (NoArg (setDynFlag Opt_SSE2))

     ------ Warning opts -------------------------------------------------
  , flagA "W"      (NoArg (mapM_ setDynFlag   minusWOpts))
  , flagA "Werror" (NoArg (setDynFlag         Opt_WarnIsError))
  , flagA "Wwarn"  (NoArg (unSetDynFlag       Opt_WarnIsError))
  , flagA "Wall"   (NoArg (mapM_ setDynFlag   minusWallOpts))
  , flagA "Wnot"   (NoArg (do { mapM_ unSetDynFlag minusWallOpts
                             ; deprecate "Use -w instead" }))
  , flagA "w"      (NoArg (mapM_ unSetDynFlag minuswRemovesOpts))
        
        ------ Plugin flags ------------------------------------------------
  , flagA "fplugin"     (hasArg addPluginModuleName)
  , flagA "fplugin-opt" (hasArg addPluginModuleNameOption)
    
        ------ Optimisation flags ------------------------------------------
  , flagA "O"      (noArgM (setOptLevel 1))
  , flagA "Onot"   (noArgM (\dflags -> do deprecate "Use -O0 instead"
                                          setOptLevel 0 dflags))
  , flagA "Odph"   (noArgM setDPHOpt)
  , flagA "O"      (optIntSuffixM (\mb_n -> setOptLevel (mb_n `orElse` 1)))
                -- If the number is missing, use 1

  , flagA "fsimplifier-phases"          (intSuffix (\n d -> d{ simplPhases = n }))
  , flagA "fmax-simplifier-iterations"  (intSuffix (\n d -> d{ maxSimplIterations = n }))
  , flagA "fspec-constr-threshold"      (intSuffix (\n d -> d{ specConstrThreshold = Just n }))
  , flagA "fno-spec-constr-threshold"   (noArg (\d -> d{ specConstrThreshold = Nothing }))
  , flagA "fspec-constr-count"          (intSuffix (\n d -> d{ specConstrCount = Just n }))
  , flagA "fno-spec-constr-count"       (noArg (\d -> d{ specConstrCount = Nothing }))
  , flagA "fliberate-case-threshold"    (intSuffix (\n d -> d{ liberateCaseThreshold = Just n }))
  , flagA "fno-liberate-case-threshold" (noArg (\d -> d{ liberateCaseThreshold = Nothing }))
  , flagA "frule-check"                 (SepArg (\s -> upd (\d -> d{ ruleCheck = Just s })))
  , flagA "fcontext-stack"              (intSuffix (\n d -> d{ ctxtStkDepth = n }))
  , flagA "fstrictness-before"          (intSuffix (\n d -> d{ strictnessBefore = n : strictnessBefore d }))
  , flagA "ffloat-lam-args"             (intSuffix (\n d -> d{ floatLamArgs = Just n }))
  , flagA "ffloat-all-lams"             (noArg (\d -> d{ floatLamArgs = Nothing }))

        ------ Profiling ----------------------------------------------------

  -- XXX Should the -f* flags be deprecated?
  -- They don't seem to be documented
  , flagA "fauto-sccs-on-all-toplevs"   	   (NoArg (setDynFlag Opt_AutoSccsOnAllToplevs))
  , flagA "auto-all"                    	   (NoArg (setDynFlag Opt_AutoSccsOnAllToplevs))
  , flagA "no-auto-all"                 	   (NoArg (unSetDynFlag Opt_AutoSccsOnAllToplevs))
  , flagA "fauto-sccs-on-exported-toplevs"  (NoArg (setDynFlag Opt_AutoSccsOnExportedToplevs))
  , flagA "auto"                            (NoArg (setDynFlag Opt_AutoSccsOnExportedToplevs))
  , flagA "no-auto"                         (NoArg (unSetDynFlag Opt_AutoSccsOnExportedToplevs))
  , flagA "fauto-sccs-on-individual-cafs"   (NoArg (setDynFlag Opt_AutoSccsOnIndividualCafs))
  , flagA "caf-all"                         (NoArg (setDynFlag Opt_AutoSccsOnIndividualCafs))
  , flagA "no-caf-all"                      (NoArg (unSetDynFlag Opt_AutoSccsOnIndividualCafs))

        ------ DPH flags ----------------------------------------------------

  , flagA "fdph-seq"         (NoArg (setDPHBackend DPHSeq))
  , flagA "fdph-par"         (NoArg (setDPHBackend DPHPar))
  , flagA "fdph-this"        (NoArg (setDPHBackend DPHThis))
  , flagA "fdph-none"        (NoArg (setDPHBackend DPHNone))

        ------ Compiler flags -----------------------------------------------

  , flagA "fasm"             (NoArg (setObjTarget HscAsm))
  , flagA "fvia-c"           (NoArg
         (addWarn "The -fvia-c flag does nothing; it will be removed in a future GHC release"))
  , flagA "fvia-C"           (NoArg
         (addWarn "The -fvia-C flag does nothing; it will be removed in a future GHC release"))
  , flagA "fllvm"            (NoArg (setObjTarget HscLlvm))

  , flagA "fno-code"         (NoArg (do { upd $ \d -> d{ ghcLink=NoLink }
                                        ; setTarget HscNothing }))
  , flagA "fbyte-code"       (NoArg (setTarget HscInterpreted))
  , flagA "fobject-code"     (NoArg (setTarget defaultHscTarget))
  , flagA "fglasgow-exts"    (NoArg (enableGlasgowExts >> deprecate "Use individual extensions instead"))
  , flagA "fno-glasgow-exts" (NoArg (disableGlasgowExts >> deprecate "Use individual extensions instead"))
 ]
 ++ map (mkFlag turnOn  "f"    setDynFlag  ) fFlags
 ++ map (mkFlag turnOff "fno-" unSetDynFlag) fFlags
 ++ map (mkFlag turnOn  "f"    setExtensionFlag  ) fLangFlags
 ++ map (mkFlag turnOff "fno-" unSetExtensionFlag) fLangFlags
 ++ map (mkFlag turnOn  "X"    setExtensionFlag  ) xFlags
 ++ map (mkFlag turnOff "XNo"  unSetExtensionFlag) xFlags
 ++ map (mkFlag turnOn  "X"    setLanguage) languageFlags
 ++ map (mkFlag turnOn  "X"    setSafeHaskell) safeHaskellFlags

package_flags :: [Flag (CmdLineP DynFlags)]
package_flags = [
        ------- Packages ----------------------------------------------------
    flagC "package-conf"         (HasArg extraPkgConf_)
  , flagC "no-user-package-conf" (NoArg (unSetDynFlag Opt_ReadUserPackageConf))
  , flagC "package-name"      	 (hasArg setPackageName)
  , flagC "package-id"        	 (HasArg exposePackageId)
  , flagC "package"           	 (HasArg exposePackage)
  , flagC "hide-package"      	 (HasArg hidePackage)
  , flagC "hide-all-packages" 	 (NoArg (setDynFlag Opt_HideAllPackages))
  , flagC "ignore-package"    	 (HasArg ignorePackage)
  , flagC "syslib"            	 (HasArg (\s -> do { exposePackage s
                                                   ; deprecate "Use -package instead" }))
  ]

type TurnOnFlag = Bool   -- True  <=> we are turning the flag on
       	       	    	 -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn = True
turnOff :: TurnOnFlag; turnOff = False

type FlagSpec flag
   = ( String	-- Flag in string form
     , FlagSafety
     , flag     -- Flag in internal form
     , TurnOnFlag -> DynP ())    -- Extra action to run when the flag is found
                                 -- Typically, emit a warning or error

mkFlag :: TurnOnFlag            -- ^ True <=> it should be turned on
       -> String                -- ^ The flag prefix
       -> (flag -> DynP ())	-- ^ What to do when the flag is found
       -> FlagSpec flag		-- ^ Specification of this particular flag
       -> Flag (CmdLineP DynFlags)
mkFlag turn_on flagPrefix f (name, fsafe, flag, extra_action)
    = Flag (flagPrefix ++ name) fsafe (NoArg (f flag >> extra_action turn_on))

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
fFlags :: [FlagSpec DynFlag]
fFlags = [
  ( "warn-dodgy-foreign-imports",       AlwaysAllowed, Opt_WarnDodgyForeignImports, nop ),
  ( "warn-dodgy-exports",               AlwaysAllowed, Opt_WarnDodgyExports, nop ),
  ( "warn-dodgy-imports",               AlwaysAllowed, Opt_WarnDodgyImports, nop ),
  ( "warn-duplicate-exports",           AlwaysAllowed, Opt_WarnDuplicateExports, nop ),
  ( "warn-hi-shadowing",                AlwaysAllowed, Opt_WarnHiShadows, nop ),
  ( "warn-implicit-prelude",            AlwaysAllowed, Opt_WarnImplicitPrelude, nop ),
  ( "warn-incomplete-patterns",         AlwaysAllowed, Opt_WarnIncompletePatterns, nop ),
  ( "warn-incomplete-uni-patterns",     AlwaysAllowed, Opt_WarnIncompleteUniPatterns, nop ),
  ( "warn-incomplete-record-updates",   AlwaysAllowed, Opt_WarnIncompletePatternsRecUpd, nop ),
  ( "warn-missing-fields",              AlwaysAllowed, Opt_WarnMissingFields, nop ),
  ( "warn-missing-import-lists",        AlwaysAllowed, Opt_WarnMissingImportList, nop ),
  ( "warn-missing-methods",             AlwaysAllowed, Opt_WarnMissingMethods, nop ),
  ( "warn-missing-signatures",          AlwaysAllowed, Opt_WarnMissingSigs, nop ),
  ( "warn-missing-local-sigs",          AlwaysAllowed, Opt_WarnMissingLocalSigs, nop ),
  ( "warn-name-shadowing",              AlwaysAllowed, Opt_WarnNameShadowing, nop ),
  ( "warn-overlapping-patterns",        AlwaysAllowed, Opt_WarnOverlappingPatterns, nop ),
  ( "warn-type-defaults",               AlwaysAllowed, Opt_WarnTypeDefaults, nop ),
  ( "warn-monomorphism-restriction",    AlwaysAllowed, Opt_WarnMonomorphism, nop ),
  ( "warn-unused-binds",                AlwaysAllowed, Opt_WarnUnusedBinds, nop ),
  ( "warn-unused-imports",              AlwaysAllowed, Opt_WarnUnusedImports, nop ),
  ( "warn-unused-matches",              AlwaysAllowed, Opt_WarnUnusedMatches, nop ),
  ( "warn-warnings-deprecations",       AlwaysAllowed, Opt_WarnWarningsDeprecations, nop ),
  ( "warn-deprecations",                AlwaysAllowed, Opt_WarnWarningsDeprecations, nop ),
  ( "warn-deprecated-flags",            AlwaysAllowed, Opt_WarnDeprecatedFlags, nop ),
  ( "warn-orphans",                     AlwaysAllowed, Opt_WarnOrphans, nop ),
  ( "warn-identities",                  AlwaysAllowed, Opt_WarnIdentities, nop ),
  ( "warn-auto-orphans",                AlwaysAllowed, Opt_WarnAutoOrphans, nop ),
  ( "warn-tabs",                        AlwaysAllowed, Opt_WarnTabs, nop ),
  ( "warn-unrecognised-pragmas",        AlwaysAllowed, Opt_WarnUnrecognisedPragmas, nop ),
  ( "warn-lazy-unlifted-bindings",      AlwaysAllowed, Opt_WarnLazyUnliftedBindings, nop),
  ( "warn-unused-do-bind",              AlwaysAllowed, Opt_WarnUnusedDoBind, nop ),
  ( "warn-wrong-do-bind",               AlwaysAllowed, Opt_WarnWrongDoBind, nop ),
  ( "warn-alternative-layout-rule-transitional", AlwaysAllowed, Opt_WarnAlternativeLayoutRuleTransitional, nop ),
  ( "print-explicit-foralls",           AlwaysAllowed, Opt_PrintExplicitForalls, nop ),
  ( "strictness",                       AlwaysAllowed, Opt_Strictness, nop ),
  ( "specialise",                       AlwaysAllowed, Opt_Specialise, nop ),
  ( "float-in",                         AlwaysAllowed, Opt_FloatIn, nop ),
  ( "static-argument-transformation",   AlwaysAllowed, Opt_StaticArgumentTransformation, nop ),
  ( "full-laziness",                    AlwaysAllowed, Opt_FullLaziness, nop ),
  ( "liberate-case",                    AlwaysAllowed, Opt_LiberateCase, nop ),
  ( "spec-constr",                      AlwaysAllowed, Opt_SpecConstr, nop ),
  ( "cse",                              AlwaysAllowed, Opt_CSE, nop ),
  ( "ignore-interface-pragmas",         AlwaysAllowed, Opt_IgnoreInterfacePragmas, nop ),
  ( "omit-interface-pragmas",           AlwaysAllowed, Opt_OmitInterfacePragmas, nop ),
  ( "expose-all-unfoldings",            AlwaysAllowed, Opt_ExposeAllUnfoldings, nop ),
  ( "do-lambda-eta-expansion",          AlwaysAllowed, Opt_DoLambdaEtaExpansion, nop ),
  ( "ignore-asserts",                   AlwaysAllowed, Opt_IgnoreAsserts, nop ),
  ( "do-eta-reduction",                 AlwaysAllowed, Opt_DoEtaReduction, nop ),
  ( "case-merge",                       AlwaysAllowed, Opt_CaseMerge, nop ),
  ( "unbox-strict-fields",              AlwaysAllowed, Opt_UnboxStrictFields, nop ),
  ( "method-sharing",                   AlwaysAllowed, Opt_MethodSharing, 
     \_ -> deprecate "doesn't do anything any more"),
     -- Remove altogether in GHC 7.2
  ( "dicts-cheap",                      AlwaysAllowed, Opt_DictsCheap, nop ),
  ( "excess-precision",                 AlwaysAllowed, Opt_ExcessPrecision, nop ),
  ( "eager-blackholing",                AlwaysAllowed, Opt_EagerBlackHoling, nop ),
  ( "print-bind-result",                AlwaysAllowed, Opt_PrintBindResult, nop ),
  ( "force-recomp",                     AlwaysAllowed, Opt_ForceRecomp, nop ),
  ( "hpc-no-auto",                      AlwaysAllowed, Opt_Hpc_No_Auto, nop ),
  ( "rewrite-rules",                    AlwaysAllowed, Opt_EnableRewriteRules, useInstead "enable-rewrite-rules" ),
  ( "enable-rewrite-rules",             AlwaysAllowed, Opt_EnableRewriteRules, nop ),
  ( "break-on-exception",               AlwaysAllowed, Opt_BreakOnException, nop ),
  ( "break-on-error",                   AlwaysAllowed, Opt_BreakOnError, nop ),
  ( "print-evld-with-show",             AlwaysAllowed, Opt_PrintEvldWithShow, nop ),
  ( "print-bind-contents",              AlwaysAllowed, Opt_PrintBindContents, nop ),
  ( "run-cps",                          AlwaysAllowed, Opt_RunCPS, nop ),
  ( "run-cpsz",                         AlwaysAllowed, Opt_RunCPSZ, nop ),
  ( "new-codegen",                      AlwaysAllowed, Opt_TryNewCodeGen, nop ),
  ( "convert-to-zipper-and-back",       AlwaysAllowed, Opt_ConvertToZipCfgAndBack, nop ),
  ( "vectorise",                        AlwaysAllowed, Opt_Vectorise, nop ),
  ( "regs-graph",                       AlwaysAllowed, Opt_RegsGraph, nop ),
  ( "regs-iterative",                   AlwaysAllowed, Opt_RegsIterative, nop ),
  ( "gen-manifest",                     AlwaysAllowed, Opt_GenManifest, nop ),
  ( "embed-manifest",                   AlwaysAllowed, Opt_EmbedManifest, nop ),
  ( "ext-core",                         AlwaysAllowed, Opt_EmitExternalCore, nop ),
  ( "shared-implib",                    AlwaysAllowed, Opt_SharedImplib, nop ),
  ( "ghci-sandbox",                     AlwaysAllowed, Opt_GhciSandbox, nop ),
  ( "helpful-errors",                   AlwaysAllowed, Opt_HelpfulErrors, nop ),
  ( "building-cabal-package",           AlwaysAllowed, Opt_BuildingCabalPackage, nop ),
  ( "implicit-import-qualified",        AlwaysAllowed, Opt_ImplicitImportQualified, nop )
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fLangFlags :: [FlagSpec ExtensionFlag]
fLangFlags = [
  ( "th",                               NeverAllowed, Opt_TemplateHaskell,
    deprecatedForExtension "TemplateHaskell" >> checkTemplateHaskellOk ),
  ( "fi",                               RestrictedFunction, Opt_ForeignFunctionInterface,
    deprecatedForExtension "ForeignFunctionInterface" ),
  ( "ffi",                              RestrictedFunction, Opt_ForeignFunctionInterface,
    deprecatedForExtension "ForeignFunctionInterface" ),
  ( "arrows",                           AlwaysAllowed, Opt_Arrows,
    deprecatedForExtension "Arrows" ),
  ( "generics",                         AlwaysAllowed, Opt_Generics,
    deprecatedForExtension "Generics" ),
  ( "implicit-prelude",                 AlwaysAllowed, Opt_ImplicitPrelude,
    deprecatedForExtension "ImplicitPrelude" ),
  ( "bang-patterns",                    AlwaysAllowed, Opt_BangPatterns,
    deprecatedForExtension "BangPatterns" ),
  ( "monomorphism-restriction",         AlwaysAllowed, Opt_MonomorphismRestriction,
    deprecatedForExtension "MonomorphismRestriction" ),
  ( "mono-pat-binds",                   AlwaysAllowed, Opt_MonoPatBinds,
    deprecatedForExtension "MonoPatBinds" ),
  ( "extended-default-rules",           AlwaysAllowed, Opt_ExtendedDefaultRules,
    deprecatedForExtension "ExtendedDefaultRules" ),
  ( "implicit-params",                  AlwaysAllowed, Opt_ImplicitParams,
    deprecatedForExtension "ImplicitParams" ),
  ( "scoped-type-variables",            AlwaysAllowed, Opt_ScopedTypeVariables,
    deprecatedForExtension "ScopedTypeVariables" ),
  ( "parr",                             AlwaysAllowed, Opt_ParallelArrays,
    deprecatedForExtension "ParallelArrays" ),
  ( "PArr",                             AlwaysAllowed, Opt_ParallelArrays,
    deprecatedForExtension "ParallelArrays" ),
  ( "allow-overlapping-instances",      RestrictedFunction, Opt_OverlappingInstances,
    deprecatedForExtension "OverlappingInstances" ),
  ( "allow-undecidable-instances",      AlwaysAllowed, Opt_UndecidableInstances,
    deprecatedForExtension "UndecidableInstances" ),
  ( "allow-incoherent-instances",       AlwaysAllowed, Opt_IncoherentInstances,
    deprecatedForExtension "IncoherentInstances" )
  ]

supportedLanguages :: [String]
supportedLanguages = [ name | (name, _, _, _) <- languageFlags ]

supportedLanguageOverlays :: [String]
supportedLanguageOverlays = [ name | (name, _, _, _) <- safeHaskellFlags ]

supportedExtensions :: [String]
supportedExtensions = [ name' | (name, _, _, _) <- xFlags, name' <- [name, "No" ++ name] ]

supportedLanguagesAndExtensions :: [String]
supportedLanguagesAndExtensions =
    supportedLanguages ++ supportedLanguageOverlays ++ supportedExtensions

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
languageFlags :: [FlagSpec Language]
languageFlags = [
  ( "Haskell98",   AlwaysAllowed, Haskell98, nop ),
  ( "Haskell2010", AlwaysAllowed, Haskell2010, nop )
  ]

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
-- They are used to place hard requirements on what GHC Haskell language
-- features can be used.
safeHaskellFlags :: [FlagSpec SafeHaskellMode]
safeHaskellFlags = [mkF Sf_SafeImports, mkF' Sf_SafeLanguage,
                    mkF Sf_Trustworthy, mkF' Sf_Safe]
    where mkF  flag = (show flag, AlwaysAllowed, flag, nop)
          mkF' flag = (show flag, EnablesSafe,   flag, nop)

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [FlagSpec ExtensionFlag]
xFlags = [
  ( "CPP",                              CmdLineOnly,  Opt_Cpp, nop ),
  ( "PostfixOperators",                 AlwaysAllowed, Opt_PostfixOperators, nop ),
  ( "TupleSections",                    AlwaysAllowed, Opt_TupleSections, nop ),
  ( "PatternGuards",                    AlwaysAllowed, Opt_PatternGuards, nop ),
  ( "UnicodeSyntax",                    AlwaysAllowed, Opt_UnicodeSyntax, nop ),
  ( "MagicHash",                        AlwaysAllowed, Opt_MagicHash, nop ),
  ( "PolymorphicComponents",            AlwaysAllowed, Opt_PolymorphicComponents, nop ),
  ( "ExistentialQuantification",        AlwaysAllowed, Opt_ExistentialQuantification, nop ),
  ( "KindSignatures",                   AlwaysAllowed, Opt_KindSignatures, nop ),
  ( "EmptyDataDecls",                   AlwaysAllowed, Opt_EmptyDataDecls, nop ),
  ( "ParallelListComp",                 AlwaysAllowed, Opt_ParallelListComp, nop ),
  ( "TransformListComp",                AlwaysAllowed, Opt_TransformListComp, nop ),
  ( "MonadComprehensions",              AlwaysAllowed, Opt_MonadComprehensions, nop),
  ( "ForeignFunctionInterface",   RestrictedFunction, Opt_ForeignFunctionInterface, nop ),
  ( "UnliftedFFITypes",                 AlwaysAllowed, Opt_UnliftedFFITypes, nop ),
  ( "GHCForeignImportPrim",             AlwaysAllowed, Opt_GHCForeignImportPrim, nop ),
  ( "LiberalTypeSynonyms",              AlwaysAllowed, Opt_LiberalTypeSynonyms, nop ),
  ( "Rank2Types",                       AlwaysAllowed, Opt_Rank2Types, nop ),
  ( "RankNTypes",                       AlwaysAllowed, Opt_RankNTypes, nop ),
  ( "ImpredicativeTypes",               AlwaysAllowed, Opt_ImpredicativeTypes, nop), 
  ( "TypeOperators",                    AlwaysAllowed, Opt_TypeOperators, nop ),
  ( "RecursiveDo",                      AlwaysAllowed, Opt_RecursiveDo,     -- Enables 'mdo'
    deprecatedForExtension "DoRec"),
  ( "DoRec",                            AlwaysAllowed, Opt_DoRec, nop ),    -- Enables 'rec' keyword 
  ( "Arrows",                           AlwaysAllowed, Opt_Arrows, nop ),
  ( "ParallelArrays",                   AlwaysAllowed, Opt_ParallelArrays, nop ),
  ( "TemplateHaskell",                  NeverAllowed, Opt_TemplateHaskell, checkTemplateHaskellOk ),
  ( "QuasiQuotes",                      AlwaysAllowed, Opt_QuasiQuotes, nop ),
  ( "Generics",                         AlwaysAllowed, Opt_Generics,
    \ _ -> deprecate "it does nothing; look into -XDefaultSignatures and -XDeriveGeneric for generic programming support." ),
  ( "ImplicitPrelude",                  AlwaysAllowed, Opt_ImplicitPrelude, nop ),
  ( "RecordWildCards",                  AlwaysAllowed, Opt_RecordWildCards, nop ),
  ( "NamedFieldPuns",                   AlwaysAllowed, Opt_RecordPuns, nop ),
  ( "RecordPuns",                       AlwaysAllowed, Opt_RecordPuns,
    deprecatedForExtension "NamedFieldPuns" ),
  ( "DisambiguateRecordFields",         AlwaysAllowed, Opt_DisambiguateRecordFields, nop ),
  ( "OverloadedStrings",                AlwaysAllowed, Opt_OverloadedStrings, nop ),
  ( "GADTs",                            AlwaysAllowed, Opt_GADTs, nop ),
  ( "GADTSyntax",                       AlwaysAllowed, Opt_GADTSyntax, nop ),
  ( "ViewPatterns",                     AlwaysAllowed, Opt_ViewPatterns, nop ),
  ( "TypeFamilies",                     AlwaysAllowed, Opt_TypeFamilies, nop ),
  ( "BangPatterns",                     AlwaysAllowed, Opt_BangPatterns, nop ),
  ( "MonomorphismRestriction",          AlwaysAllowed, Opt_MonomorphismRestriction, nop ),
  ( "NPlusKPatterns",                   AlwaysAllowed, Opt_NPlusKPatterns, nop ),
  ( "DoAndIfThenElse",                  AlwaysAllowed, Opt_DoAndIfThenElse, nop ),
  ( "RebindableSyntax",                 AlwaysAllowed, Opt_RebindableSyntax, nop ),
  ( "MonoPatBinds",                     AlwaysAllowed, Opt_MonoPatBinds, nop ),
  ( "ExplicitForAll",                   AlwaysAllowed, Opt_ExplicitForAll, nop ),
  ( "AlternativeLayoutRule",            AlwaysAllowed, Opt_AlternativeLayoutRule, nop ),
  ( "AlternativeLayoutRuleTransitional",AlwaysAllowed, Opt_AlternativeLayoutRuleTransitional, nop ),
  ( "DatatypeContexts",                 AlwaysAllowed, Opt_DatatypeContexts,
    \ turn_on -> when turn_on $ deprecate "It was widely considered a misfeature, and has been removed from the Haskell language." ),
  ( "NondecreasingIndentation",         AlwaysAllowed, Opt_NondecreasingIndentation, nop ),
  ( "RelaxedLayout",                    AlwaysAllowed, Opt_RelaxedLayout, nop ),
  ( "MonoLocalBinds",                   AlwaysAllowed, Opt_MonoLocalBinds, nop ),
  ( "RelaxedPolyRec",                   AlwaysAllowed, Opt_RelaxedPolyRec, 
    \ turn_on -> if not turn_on 
                 then deprecate "You can't turn off RelaxedPolyRec any more"
                 else return () ),
  ( "ExtendedDefaultRules",             AlwaysAllowed, Opt_ExtendedDefaultRules, nop ),
  ( "ImplicitParams",                   AlwaysAllowed, Opt_ImplicitParams, nop ),
  ( "ScopedTypeVariables",              AlwaysAllowed, Opt_ScopedTypeVariables, nop ),

  ( "PatternSignatures",                AlwaysAllowed, Opt_ScopedTypeVariables, 
    deprecatedForExtension "ScopedTypeVariables" ),

  ( "UnboxedTuples",                    AlwaysAllowed, Opt_UnboxedTuples, nop ),
  ( "StandaloneDeriving",               AlwaysAllowed, Opt_StandaloneDeriving, nop ),
  ( "DeriveDataTypeable",               AlwaysAllowed, Opt_DeriveDataTypeable, nop ),
  ( "DeriveFunctor",                    AlwaysAllowed, Opt_DeriveFunctor, nop ),
  ( "DeriveTraversable",                AlwaysAllowed, Opt_DeriveTraversable, nop ),
  ( "DeriveFoldable",                   AlwaysAllowed, Opt_DeriveFoldable, nop ),
  ( "DeriveGeneric",                    AlwaysAllowed, Opt_DeriveGeneric, nop ),
  ( "DefaultSignatures",                AlwaysAllowed, Opt_DefaultSignatures, nop ),
  ( "TypeSynonymInstances",             AlwaysAllowed, Opt_TypeSynonymInstances, nop ),
  ( "FlexibleContexts",                 AlwaysAllowed, Opt_FlexibleContexts, nop ),
  ( "FlexibleInstances",                AlwaysAllowed, Opt_FlexibleInstances, nop ),
  ( "ConstrainedClassMethods",          AlwaysAllowed, Opt_ConstrainedClassMethods, nop ),
  ( "MultiParamTypeClasses",            AlwaysAllowed, Opt_MultiParamTypeClasses, nop ),
  ( "FunctionalDependencies",           AlwaysAllowed, Opt_FunctionalDependencies, nop ),
  ( "GeneralizedNewtypeDeriving",       AlwaysAllowed, Opt_GeneralizedNewtypeDeriving, nop ),
  ( "OverlappingInstances",        RestrictedFunction, Opt_OverlappingInstances, nop ),
  ( "UndecidableInstances",             AlwaysAllowed, Opt_UndecidableInstances, nop ),
  ( "IncoherentInstances",              AlwaysAllowed, Opt_IncoherentInstances, nop ),
  ( "PackageImports",                   AlwaysAllowed, Opt_PackageImports, nop )
  ]

defaultFlags :: [DynFlag]
defaultFlags 
  = [ Opt_AutoLinkPackages,
      Opt_ReadUserPackageConf,

      Opt_SharedImplib,

#if GHC_DEFAULT_NEW_CODEGEN
      Opt_TryNewCodeGen,
#endif

      Opt_GenManifest,
      Opt_EmbedManifest,
      Opt_PrintBindContents,
      Opt_GhciSandbox,
      Opt_HelpfulErrors
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    ++ standardWarnings

impliedFlags :: [(ExtensionFlag, TurnOnFlag, ExtensionFlag)]
impliedFlags
  = [ (Opt_RankNTypes,                turnOn, Opt_ExplicitForAll)
    , (Opt_Rank2Types,                turnOn, Opt_ExplicitForAll)
    , (Opt_ScopedTypeVariables,       turnOn, Opt_ExplicitForAll)
    , (Opt_LiberalTypeSynonyms,       turnOn, Opt_ExplicitForAll)
    , (Opt_ExistentialQuantification, turnOn, Opt_ExplicitForAll)
    , (Opt_PolymorphicComponents,     turnOn, Opt_ExplicitForAll)
    , (Opt_FlexibleInstances,         turnOn, Opt_TypeSynonymInstances)
    , (Opt_FunctionalDependencies,    turnOn, Opt_MultiParamTypeClasses)

    , (Opt_RebindableSyntax, turnOff, Opt_ImplicitPrelude)      -- NB: turn off!

    , (Opt_GADTs,            turnOn, Opt_GADTSyntax)
    , (Opt_GADTs,            turnOn, Opt_MonoLocalBinds)
    , (Opt_TypeFamilies,     turnOn, Opt_MonoLocalBinds)

    , (Opt_TypeFamilies,     turnOn, Opt_KindSignatures)  -- Type families use kind signatures
      						     -- all over the place

    , (Opt_ImpredicativeTypes,  turnOn, Opt_RankNTypes)

	-- Record wild-cards implies field disambiguation
	-- Otherwise if you write (C {..}) you may well get
	-- stuff like " 'a' not in scope ", which is a bit silly
 	-- if the compiler has just filled in field 'a' of constructor 'C'
    , (Opt_RecordWildCards,     turnOn, Opt_DisambiguateRecordFields)
    
    , (Opt_ParallelArrays, turnOn, Opt_ParallelListComp)
  ]

optLevelFlags :: [([Int], DynFlag)]
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
    , ([2],     Opt_RegsGraph)

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
    ]

-- -----------------------------------------------------------------------------
-- Standard sets of warning options

standardWarnings :: [DynFlag]
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
        Opt_WarnAlternativeLayoutRuleTransitional
      ]

minusWOpts :: [DynFlag]
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

minusWallOpts :: [DynFlag]
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

minuswRemovesOpts :: [DynFlag]
-- minuswRemovesOpts should be every warning option 
minuswRemovesOpts
    = minusWallOpts ++
      [Opt_WarnTabs,
       Opt_WarnIncompletePatternsRecUpd,
       Opt_WarnIncompleteUniPatterns,
       Opt_WarnMonomorphism,
       Opt_WarnUnrecognisedPragmas,
       Opt_WarnAutoOrphans,
       Opt_WarnImplicitPrelude
     ]       

enableGlasgowExts :: DynP ()
enableGlasgowExts = do setDynFlag Opt_PrintExplicitForalls
                       mapM_ setExtensionFlag glasgowExtsFlags

disableGlasgowExts :: DynP ()
disableGlasgowExts = do unSetDynFlag Opt_PrintExplicitForalls
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
           , Opt_PolymorphicComponents
           , Opt_ExistentialQuantification
           , Opt_UnicodeSyntax
           , Opt_PostfixOperators
           , Opt_PatternGuards
           , Opt_LiberalTypeSynonyms
           , Opt_RankNTypes
           , Opt_TypeOperators
           , Opt_DoRec
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

checkTemplateHaskellOk :: Bool -> DynP ()
#ifdef GHCI
checkTemplateHaskellOk turn_on
  | turn_on && rtsIsProfiled
  = addErr "You can't use Template Haskell with a profiled compiler"
  | otherwise
  = return ()
#else
-- In stage 1 we don't know that the RTS has rts_isProfiled,
-- so we simply say "ok".  It doesn't matter because TH isn't
-- available in stage 1 anyway.
checkTemplateHaskellOk _ = return ()
#endif

{- **********************************************************************
%*									*
		DynFlags constructors
%*									*
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
hasArgDF fn deprec = HasArg (\s -> do { upd (fn s)
                                      ; deprecate deprec })

intSuffix :: (Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffix fn = IntSuffix (\n -> upd (fn n))

optIntSuffixM :: (Maybe Int -> DynFlags -> DynP DynFlags)
              -> OptKind (CmdLineP DynFlags)
optIntSuffixM fn = OptIntSuffix (\mi -> updM (fn mi))

setDumpFlag :: DynFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

--------------------------
setDynFlag, unSetDynFlag :: DynFlag -> DynP ()
setDynFlag   f = upd (\dfs -> dopt_set dfs f)
unSetDynFlag f = upd (\dfs -> dopt_unset dfs f)

--------------------------
setExtensionFlag, unSetExtensionFlag :: ExtensionFlag -> DynP ()
setExtensionFlag f = do { upd (\dfs -> xopt_set dfs f)
                        ; sequence_ deps }
  where
    deps = [ if turn_on then setExtensionFlag   d
                        else unSetExtensionFlag d
           | (f', turn_on, d) <- impliedFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags

unSetExtensionFlag f = upd (\dfs -> xopt_unset dfs f)
   -- When you un-set f, however, we don't un-set the things it implies
   --      (except for -fno-glasgow-exts, which is treated specially)

--------------------------
alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
alterSettings f dflags = dflags { settings = f (settings dflags) }

--------------------------
setDumpFlag' :: DynFlag -> DynP ()
setDumpFlag' dump_flag
  = do { setDynFlag dump_flag
       ; when want_recomp forceRecompile }
  where
	-- Certain dumpy-things are really interested in what's going
        -- on during recompilation checking, so in those cases we
        -- don't want to turn it off.
    want_recomp = dump_flag `notElem` [Opt_D_dump_if_trace,
                          	       Opt_D_dump_hi_diffs]

forceRecompile :: DynP ()
-- Whenver we -ddump, force recompilation (by switching off the 
-- recompilation checker), else you don't see the dump! However, 
-- don't switch it off in --make mode, else *everything* gets
-- recompiled which probably isn't what you want
forceRecompile = do { dfs <- liftEwM getCmdLineState
	       	    ; when (force_recomp dfs) (setDynFlag Opt_ForceRecomp) }
        where
	  force_recomp dfs = isOneShot (ghcMode dfs)

setVerboseCore2Core :: DynP ()
setVerboseCore2Core = do forceRecompile
                         setDynFlag Opt_D_verbose_core2core 
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

extraPkgConf_ :: FilePath -> DynP ()
extraPkgConf_  p = upd (\s -> s{ extraPkgConfs = p : extraPkgConfs s })

exposePackage, exposePackageId, hidePackage, ignorePackage :: String -> DynP ()
exposePackage p =
  upd (\s -> s{ packageFlags = ExposePackage p : packageFlags s })
exposePackageId p =
  upd (\s -> s{ packageFlags = ExposePackageId p : packageFlags s })
hidePackage p =
  upd (\s -> s{ packageFlags = HidePackage p : packageFlags s })
ignorePackage p =
  upd (\s -> s{ packageFlags = IgnorePackage p : packageFlags s })

setPackageName :: String -> DynFlags -> DynFlags
setPackageName p s =  s{ thisPackage = stringToPackageId p }

-- If we're linking a binary, then only targets that produce object
-- code are allowed (requests for other target types are ignored).
setTarget :: HscTarget -> DynP ()
setTarget l = upd set
  where
   set dfs
     | ghcLink dfs /= LinkBinary || isObjectTarget l  = dfs{ hscTarget = l }
     | otherwise = dfs

-- Changes the target only if we're compiling object code.  This is
-- used by -fasm and -fllvm, which switch from one to the other, but
-- not from bytecode to object-code.  The idea is that -fasm/-fllvm
-- can be safely used in an OPTIONS_GHC pragma.
setObjTarget :: HscTarget -> DynP ()
setObjTarget l = updM set
  where
   set dflags
     | isObjectTarget (hscTarget dflags)
       = case l of
         HscC
          | cGhcUnregisterised /= "YES" ->
             do addWarn ("Compiler not unregisterised, so ignoring " ++ flag)
                return dflags
         HscAsm
          | cGhcWithNativeCodeGen /= "YES" ->
             do addWarn ("Compiler has no native codegen, so ignoring " ++
                         flag)
                return dflags
         HscLlvm
          | cGhcUnregisterised == "YES" ->
             do addWarn ("Compiler unregisterised, so ignoring " ++ flag)
                return dflags
          | not ((arch == ArchX86_64) && (os == OSLinux || os == OSDarwin)) &&
            (not opt_Static || opt_PIC)
            ->
             do addWarn ("Ignoring " ++ flag ++ " as it is incompatible with -fPIC and -dynamic on this platform")
                return dflags
         _ -> return $ dflags { hscTarget = l }
     | otherwise = return dflags
     where platform = targetPlatform dflags
           arch = platformArch platform
           os   = platformOS   platform
           flag = showHscTargetFlag l

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

-- Determines the package used by the vectoriser for the symbols of the vectorised code.
-- 'DPHNone' indicates that no data-parallel backend library is available; hence, the
-- vectoriser cannot be used.
--
data DPHBackend = DPHPar    -- "dph-par"
                | DPHSeq    -- "dph-seq"
                | DPHThis   -- the currently compiled package
                | DPHNone   -- no DPH library available
        deriving(Eq, Ord, Enum, Show)

setDPHBackend :: DPHBackend -> DynP ()
setDPHBackend backend = upd $ \dflags -> dflags { dphBackend = backend }

-- Query the DPH backend package to be used by the vectoriser and desugaring of DPH syntax.
--
dphPackageMaybe :: DynFlags -> Maybe PackageId
dphPackageMaybe dflags 
  = case dphBackend dflags of
      DPHPar  -> Just dphParPackageId
      DPHSeq  -> Just dphSeqPackageId
      DPHThis -> Just (thisPackage dflags)
      DPHNone -> Nothing

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
picCCOpts _dflags
#if darwin_TARGET_OS
      -- Apple prefers to do things the other way round.
      -- PIC is on by default.
      -- -mdynamic-no-pic:
      --     Turn off PIC code generation.
      -- -fno-common:
      --     Don't generate "common" symbols - these are unwanted
      --     in dynamic libraries.

    | opt_PIC
        = ["-fno-common", "-U __PIC__","-D__PIC__"]
    | otherwise
        = ["-mdynamic-no-pic"]
#elif mingw32_TARGET_OS
      -- no -fPIC for Windows
    | opt_PIC
        = ["-U __PIC__","-D__PIC__"]
    | otherwise
        = []
#else
      -- we need -fPIC for C files when we are compiling with -dynamic,
      -- otherwise things like stub.c files don't get compiled
      -- correctly.  They need to reference data in the Haskell
      -- objects, but can't without -fPIC.  See
      -- http://hackage.haskell.org/trac/ghc/wiki/Commentary/PositionIndependentCode
    | opt_PIC || not opt_Static
        = ["-fPIC", "-U __PIC__", "-D__PIC__"]
    | otherwise
        = []
#endif

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
       ("Unregisterised",              cGhcUnregisterised),
       ("Tables next to code",         cGhcEnableTablesNextToCode),
       ("RTS ways",                    cGhcRTSWays),
       ("Leading underscore",          cLeadingUnderscore),
       ("Debug on",                    show debugIsOn),
       ("LibDir",                      topDir dflags),
       ("Global Package DB",           systemPackageConfig dflags),
       ("Gcc Linker flags",            show cGccLinkerOpts),
       ("Ld Linker flags",             show cLdLinkerOpts)
      ]

