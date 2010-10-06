{-# OPTIONS_GHC -w #-}
-- Temporary, until rtsIsProfiled is fixed

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
        flattenExtensionFlags,
        ensureFlattenedExtensionFlags,
        dopt,
        dopt_set,
        dopt_unset,
        xopt,
        xopt_set,
        xopt_unset,
        xopt_set_flattened,
        xopt_unset_flattened,
        DynFlags(..),
        RtsOptsEnabled(..),
        HscTarget(..), isObjectTarget, defaultObjectTarget,
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..),
        Option(..), showOpt,
        DynLibLoader(..),
        fFlags, fLangFlags, xFlags,
        dphPackage,
        wayNames,

        -- ** Manipulating DynFlags
        defaultDynFlags,                -- DynFlags
        initDynFlags,                   -- DynFlags -> IO DynFlags

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlag,
        updOptLevel,
        setTmpDir,
        setPackageName,
        doingTickyProfiling,

        -- ** Parsing DynFlags
        parseDynamicFlags,
        parseDynamicNoPackageFlags,
        allFlags,

        supportedLanguagesAndExtensions,

        -- ** DynFlag C compiler options
        machdepCCOpts, picCCOpts,

        -- * Configuration of the stg-to-stg passes
        StgToDo(..),
        getStgToDo,

        -- * Compiler configuration suitable for display to the user
        Printable(..),
        compilerInfo
#ifdef GHCI
-- Only in stage 2 can we be sure that the RTS 
-- exposes the appropriate runtime boolean
        , rtsIsProfiled
#endif
  ) where

#include "HsVersions.h"

#ifndef OMIT_NATIVE_CODEGEN
import Platform
#endif
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
import Foreign.C	( CInt )
import {-# SOURCE #-} ErrUtils ( Severity(..), Message, mkLocMessage )

import System.IO.Unsafe	( unsafePerformIO )
import Data.IORef
import Control.Monad    ( when )

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath
import System.IO        ( stderr, hPutChar )

-- -----------------------------------------------------------------------------
-- DynFlags

-- | Enumerates the simple on-or-off dynamic flags
data DynFlag

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_cmmz
   | Opt_D_dump_cmmz_pretty
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
   | Opt_D_dump_cpranal
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_flatC
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_rule_firings
   | Opt_D_dump_occur_anal
   | Opt_D_dump_parsed
   | Opt_D_dump_rn
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
   | Opt_D_dump_tc_trace
   | Opt_D_dump_if_trace
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
   | Opt_MethodSharing
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
   | Opt_DryRun
   | Opt_DoAsmMangling
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
   | Opt_KeepRawSFiles
   | Opt_KeepTmpFiles
   | Opt_KeepRawTokenStream
   | Opt_KeepLlvmFiles

   deriving (Eq, Show)

data Language = Haskell98 | Haskell2010

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
   | Opt_PArr                           -- Syntactic support for parallel arrays
   | Opt_Arrows                         -- Arrow-notation syntax
   | Opt_TemplateHaskell
   | Opt_QuasiQuotes
   | Opt_ImplicitParams
   | Opt_Generics			-- "Derivable type classes"
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
   | Opt_NPlusKPatterns
   | Opt_DoAndIfThenElse

   | Opt_StandaloneDeriving
   | Opt_DeriveDataTypeable
   | Opt_DeriveFunctor
   | Opt_DeriveTraversable
   | Opt_DeriveFoldable

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
   | Opt_NewQualifiedOperators
   | Opt_ExplicitForAll
   | Opt_AlternativeLayoutRule
   | Opt_AlternativeLayoutRuleTransitional
   | Opt_DatatypeContexts
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

#ifndef OMIT_NATIVE_CODEGEN
  targetPlatform	:: Platform,	-- ^ The platform we're compiling for. Used by the NCG.
#endif
  stolen_x86_regs       :: Int,
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
  tmpDir                :: String,      -- no trailing '/'

  ghcUsagePath          :: FilePath,    -- Filled in by SysTools
  ghciUsagePath         :: FilePath,    -- ditto
  rtsOpts               :: Maybe String,
  rtsOptsEnabled        :: RtsOptsEnabled,

  hpcDir                :: String,      -- ^ Path to store the .mix files

  -- options for particular phases
  opt_L                 :: [String],
  opt_P                 :: [String],
  opt_F                 :: [String],
  opt_c                 :: [String],
  opt_m                 :: [String],
  opt_a                 :: [String],
  opt_l                 :: [String],
  opt_windres           :: [String],
  opt_lo                :: [String], -- LLVM: llvm optimiser
  opt_lc                :: [String], -- LLVM: llc static compiler

  -- commands for particular phases
  pgm_L                 :: String,
  pgm_P                 :: (String,[Option]),
  pgm_F                 :: String,
  pgm_c                 :: (String,[Option]),
  pgm_m                 :: (String,[Option]),
  pgm_s                 :: (String,[Option]),
  pgm_a                 :: (String,[Option]),
  pgm_l                 :: (String,[Option]),
  pgm_dll               :: (String,[Option]),
  pgm_T                 :: String,
  pgm_sysman            :: String,
  pgm_windres           :: String,
  pgm_lo                :: (String,[Option]), -- LLVM: opt llvm optimiser
  pgm_lc                :: (String,[Option]), -- LLVM: llc static compiler

  --  For ghc -M
  depMakefile           :: FilePath,
  depIncludePkgDeps     :: Bool,
  depExcludeMods        :: [ModuleName],
  depSuffixes           :: [String],

  --  Package flags
  extraPkgConfs         :: [FilePath],
  topDir                :: FilePath,    -- filled in by SysTools
  systemPackageConfig   :: FilePath,    -- ditto
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

  -- hsc dynamic flags
  flags                 :: [DynFlag],
  language              :: Maybe Language,
  extensionFlags        :: Either [OnOff ExtensionFlag]
                                  [ExtensionFlag],

  -- | Message output action: use "ErrUtils" instead of this if you can
  log_action            :: Severity -> SrcSpan -> PprStyle -> Message -> IO (),

  haddockOptions :: Maybe String
 }

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
  | HscJava        -- ^ Generate Java bytecode.
  | HscInterpreted -- ^ Generate bytecode.  (Requires 'LinkInMemory')
  | HscNothing     -- ^ Don't generate any code.  See notes above.
  deriving (Eq, Show)

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
  | cGhcWithNativeCodeGen == "YES"      =  HscAsm
  | otherwise                           =  HscC

data DynLibLoader
  = Deployable
  | SystemDependent
  deriving Eq

data RtsOptsEnabled = RtsOptsNone | RtsOptsSafeOnly | RtsOptsAll

-- | Used by 'GHC.newSession' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 -- someday these will be dynamic flags
 ways <- readIORef v_Ways
 refFilesToClean <- newIORef []
 refDirsToClean <- newIORef Map.empty
 return dflags{
        ways            = ways,
        buildTag        = mkBuildTag (filter (not . wayRTSOnly) ways),
        rtsBuildTag     = mkBuildTag ways,
        filesToClean    = refFilesToClean,
        dirsToClean     = refDirsToClean
        }

-- | The normal 'DynFlags'. Note that they is not suitable for use in this form
-- and must be fully initialized by 'GHC.newSession' first.
defaultDynFlags :: DynFlags
defaultDynFlags =
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
        specConstrThreshold     = Just 200,
        specConstrCount         = Just 3,
        liberateCaseThreshold   = Just 200,
        strictnessBefore        = [],

#ifndef OMIT_NATIVE_CODEGEN
        targetPlatform          = defaultTargetPlatform,
#endif
        stolen_x86_regs         = 4,
        cmdlineHcIncludes       = [],
        importPaths             = ["."],
        mainModIs               = mAIN,
        mainFunIs               = Nothing,
        ctxtStkDepth            = mAX_CONTEXT_REDUCTION_DEPTH,

        dphBackend              = DPHPar,

        thisPackage             = mainPackageId,

        objectDir               = Nothing,
        dylibInstallName        = Nothing,
        hiDir                   = Nothing,
        stubDir                 = Nothing,

        objectSuf               = phaseInputExt StopLn,
        hcSuf                   = phaseInputExt HCc,
        hiSuf                   = "hi",

        outputFile              = Nothing,
        outputHi                = Nothing,
        dynLibLoader            = SystemDependent,
        dumpPrefix              = Nothing,
        dumpPrefixForce         = Nothing,
        includePaths            = [],
        libraryPaths            = [],
        frameworkPaths          = [],
        cmdlineFrameworks       = [],
        tmpDir                  = cDEFAULT_TMPDIR,
        rtsOpts                 = Nothing,
        rtsOptsEnabled          = RtsOptsSafeOnly,

        hpcDir                  = ".hpc",

        opt_L                   = [],
        opt_P                   = (if opt_PIC
                                   then ["-D__PIC__", "-U __PIC__"] -- this list is reversed
                                   else []),
        opt_F                   = [],
        opt_c                   = [],
        opt_a                   = [],
        opt_m                   = [],
        opt_l                   = [],
        opt_windres             = [],
        opt_lo                  = [],
        opt_lc                  = [],

        extraPkgConfs           = [],
        packageFlags            = [],
        pkgDatabase             = Nothing,
        pkgState                = panic "no package state yet: call GHC.setSessionDynFlags",
        ways                    = panic "defaultDynFlags: No ways",
        buildTag                = panic "defaultDynFlags: No buildTag",
        rtsBuildTag             = panic "defaultDynFlags: No rtsBuildTag",
        splitInfo               = Nothing,
        -- initSysTools fills all these in
        ghcUsagePath            = panic "defaultDynFlags: No ghciUsagePath",
        ghciUsagePath           = panic "defaultDynFlags: No ghciUsagePath",
        topDir                  = panic "defaultDynFlags: No topDir",
        systemPackageConfig     = panic  "no systemPackageConfig: call GHC.setSessionDynFlags",
        pgm_L                   = panic "defaultDynFlags: No pgm_L",
        pgm_P                   = panic "defaultDynFlags: No pgm_P",
        pgm_F                   = panic "defaultDynFlags: No pgm_F",
        pgm_c                   = panic "defaultDynFlags: No pgm_c",
        pgm_m                   = panic "defaultDynFlags: No pgm_m",
        pgm_s                   = panic "defaultDynFlags: No pgm_s",
        pgm_a                   = panic "defaultDynFlags: No pgm_a",
        pgm_l                   = panic "defaultDynFlags: No pgm_l",
        pgm_dll                 = panic "defaultDynFlags: No pgm_dll",
        pgm_T                   = panic "defaultDynFlags: No pgm_T",
        pgm_sysman              = panic "defaultDynFlags: No pgm_sysman",
        pgm_windres             = panic "defaultDynFlags: No pgm_windres",
        pgm_lo                  = panic "defaultDynFlags: No pgm_lo",
        pgm_lc                  = panic "defaultDynFlags: No pgm_lc",
        -- end of initSysTools values
        -- ghc -M values
        depMakefile       = "Makefile",
        depIncludePkgDeps = False,
        depExcludeMods    = [],
        depSuffixes       = [],
        -- end of ghc -M values
        filesToClean   = panic "defaultDynFlags: No filesToClean",
        dirsToClean    = panic "defaultDynFlags: No dirsToClean",
        haddockOptions = Nothing,
        flags = defaultFlags,
        language = Nothing,
        extensionFlags = Left [],

        log_action = \severity srcSpan style msg ->
                        case severity of
                          SevOutput -> printOutput (msg style)
                          SevInfo   -> printErrs (msg style)
                          SevFatal  -> printErrs (msg style)
                          _         -> do 
                                hPutChar stderr '\n'
                                printErrs ((mkLocMessage srcSpan msg) style)
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

flattenExtensionFlags :: DynFlags -> DynFlags
flattenExtensionFlags dflags
    = case extensionFlags dflags of
      Left onoffs ->
          dflags {
              extensionFlags = Right $ flattenExtensionFlags' (language dflags) onoffs
          }
      Right _ ->
          panic "Flattening already-flattened extension flags"

ensureFlattenedExtensionFlags :: DynFlags -> DynFlags
ensureFlattenedExtensionFlags dflags
    = case extensionFlags dflags of
      Left onoffs ->
          dflags {
              extensionFlags = Right $ flattenExtensionFlags' (language dflags) onoffs
          }
      Right _ ->
          dflags

-- OnOffs accumulate in reverse order, so we use foldr in order to
-- process them in the right order
flattenExtensionFlags' :: Maybe Language -> [OnOff ExtensionFlag]
                       -> [ExtensionFlag]
flattenExtensionFlags' ml = foldr f defaultExtensionFlags
    where f (On f)  flags = f : delete f flags
          f (Off f) flags =     delete f flags
          defaultExtensionFlags = languageExtensions ml

languageExtensions :: Maybe Language -> [ExtensionFlag]
languageExtensions Nothing
    = Opt_MonoPatBinds   -- Experimentally, I'm making this non-standard
                         -- behaviour the default, to see if anyone notices
                         -- SLPJ July 06
      -- In due course I'd like Opt_MonoLocalBinds to be on by default
      -- But NB it's implied by GADTs etc
      -- SLPJ September 2010
    : languageExtensions (Just Haskell2010)
languageExtensions (Just Haskell98)
    = [Opt_ImplicitPrelude,
       Opt_MonomorphismRestriction,
       Opt_NPlusKPatterns,
       Opt_DatatypeContexts]
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
xopt f dflags = case extensionFlags dflags of
                Left _ -> panic ("Testing for extension flag " ++ show f ++ " before flattening")
                Right flags -> f `elem` flags

-- | Set a 'ExtensionFlag'
xopt_set :: DynFlags -> ExtensionFlag -> DynFlags
xopt_set dfs f = case extensionFlags dfs of
                 Left onoffs -> dfs { extensionFlags = Left (On f : onoffs) }
                 Right _ -> panic ("Setting extension flag " ++ show f ++ " after flattening")

-- | Set a 'ExtensionFlag'
xopt_set_flattened :: DynFlags -> ExtensionFlag -> DynFlags
xopt_set_flattened dfs f = case extensionFlags dfs of
                           Left _ ->
                               panic ("Setting extension flag " ++ show f ++ " before flattening, but expected flattened")
                           Right flags ->
                               dfs { extensionFlags = Right (f : delete f flags) }

-- | Unset a 'ExtensionFlag'
xopt_unset :: DynFlags -> ExtensionFlag -> DynFlags
xopt_unset dfs f = case extensionFlags dfs of
                   Left onoffs -> dfs { extensionFlags = Left (Off f : onoffs) }
                   Right _ -> panic ("Unsetting extension flag " ++ show f ++ " after flattening")

-- | Unset a 'ExtensionFlag'
xopt_unset_flattened :: DynFlags -> ExtensionFlag -> DynFlags
xopt_unset_flattened dfs f = case extensionFlags dfs of
                             Left _ ->
                                 panic ("Unsetting extension flag " ++ show f ++ " before flattening, but expected flattened")
                             Right flags ->
                                 dfs { extensionFlags = Right (delete f flags) }

-- | Retrieve the options corresponding to a particular @opt_*@ field in the correct order
getOpts :: DynFlags             -- ^ 'DynFlags' to retrieve the options from
        -> (DynFlags -> [a])    -- ^ Relevant record accessor: one of the @opt_*@ accessors
        -> [a]                  -- ^ Correctly ordered extracted options
getOpts dflags opts = reverse (opts dflags)
        -- We add to the options from the front, so we need to reverse the list

-- | Gets the verbosity flag for the current verbosity level. This is fed to
-- other tools, so GHC-specific verbosity flags like @-ddump-most@ are not included
getVerbFlag :: DynFlags -> String
getVerbFlag dflags
  | verbosity dflags >= 3  = "-v"
  | otherwise =  ""

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
  -- \#included from the .hc file when compiling with -fvia-C.
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f
setDylibInstallName  f d = d{ dylibInstallName = Just f}

setObjectSuf  f d = d{ objectSuf  = f}
setHiSuf      f d = d{ hiSuf      = f}
setHcSuf      f d = d{ hcSuf      = f}

setOutputFile f d = d{ outputFile = f}
setOutputHi   f d = d{ outputHi   = f}

parseDynLibLoaderMode f d =
 case splitAt 8 f of
   ("deploy", "")       -> d{ dynLibLoader = Deployable }
   ("sysdep", "")       -> d{ dynLibLoader = SystemDependent }
   _                    -> ghcError (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f d = let (pgm:args) = words f in d{ pgm_P   = (pgm, map Option args)}
addOptl   f d = d{ opt_l   = f : opt_l d}
addOptP   f d = d{ opt_P   = f : opt_P d}


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
parseDynamicFlags :: Monad m =>
                     DynFlags -> [Located String]
                  -> m (DynFlags, [Located String], [Located String])
                     -- ^ Updated 'DynFlags', left-over arguments, and
                     -- list of warnings.
parseDynamicFlags dflags args = parseDynamicFlags_ dflags args True

-- | Like 'parseDynamicFlags' but does not allow the package flags (-package,
-- -hide-package, -ignore-package, -hide-all-packages, -package-conf).
parseDynamicNoPackageFlags :: Monad m =>
                     DynFlags -> [Located String]
                  -> m (DynFlags, [Located String], [Located String])
                     -- ^ Updated 'DynFlags', left-over arguments, and
                     -- list of warnings.
parseDynamicNoPackageFlags dflags args = parseDynamicFlags_ dflags args False

parseDynamicFlags_ :: Monad m =>
                      DynFlags -> [Located String] -> Bool
                  -> m (DynFlags, [Located String], [Located String])
parseDynamicFlags_ dflags0 args pkg_flags = do
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
      flag_spec | pkg_flags = package_flags ++ dynamic_flags
                | otherwise = dynamic_flags

  let ((leftover, errs, warns), dflags1)
          = runCmdLine (processArgs flag_spec args') dflags0
  when (not (null errs)) $ ghcError $ errorsToGhcException errs

  -- Cannot use -fPIC with registerised -fvia-C, because the mangler
  -- isn't up to the job.  We know that if hscTarget == HscC, then the
  -- user has explicitly used -fvia-C, because -fasm is the default,
  -- unless there is no NCG on this platform.  The latter case is
  -- checked when the -fPIC flag is parsed.
  --
  let (pic_warns, dflags2)
        | opt_PIC && hscTarget dflags1 == HscC && cGhcUnregisterised == "NO"
        = ([L noSrcSpan $ "Warning: -fvia-C is incompatible with -fPIC; ignoring -fvia-C"],
                dflags1{ hscTarget = HscAsm })
#if !(x86_64_TARGET_ARCH && linux_TARGET_OS)
        | (not opt_Static || opt_PIC) && hscTarget dflags1 == HscLlvm
        = ([L noSrcSpan $ "Warning: -fllvm is incompatible with -fPIC and -"
                ++ "dynamic on this platform;\n              ignoring -fllvm"],
                dflags1{ hscTarget = HscAsm })
#endif
        | otherwise = ([], dflags1)

  return (dflags2, leftover, pic_warns ++ warns)


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
          ok _ = True
          flags = [ name | (name, _, _) <- fFlags ]
          flags' = [ name | (name, _, _) <- fLangFlags ]

--------------- The main flags themselves ------------------
dynamic_flags :: [Flag (CmdLineP DynFlags)]
dynamic_flags = [
    Flag "n"        (NoArg (setDynFlag Opt_DryRun))
  , Flag "cpp"      (NoArg (setExtensionFlag Opt_Cpp)) 
  , Flag "F"        (NoArg (setDynFlag Opt_Pp)) 
  , Flag "#include" 
         (HasArg (\s -> do { addCmdlineHCInclude s
                           ; addWarn "-#include and INCLUDE pragmas are deprecated: They no longer have any effect" }))
  , Flag "v"        (OptIntSuffix setVerbosity)

        ------- Specific phases  --------------------------------------------
    -- need to appear before -pgmL to be parsed as LLVM flags.
  , Flag "pgmlo"          (hasArg (\f d -> d{ pgm_lo  = (f,[])}))
  , Flag "pgmlc"          (hasArg (\f d -> d{ pgm_lc  = (f,[])}))
  , Flag "pgmL"           (hasArg (\f d -> d{ pgm_L   = f}))
  , Flag "pgmP"           (hasArg setPgmP)
  , Flag "pgmF"           (hasArg (\f d -> d{ pgm_F   = f}))
  , Flag "pgmc"           (hasArg (\f d -> d{ pgm_c   = (f,[])}))
  , Flag "pgmm"           (hasArg (\f d -> d{ pgm_m   = (f,[])}))
  , Flag "pgms"           (hasArg (\f d -> d{ pgm_s   = (f,[])}))
  , Flag "pgma"           (hasArg (\f d -> d{ pgm_a   = (f,[])}))
  , Flag "pgml"           (hasArg (\f d -> d{ pgm_l   = (f,[])}))
  , Flag "pgmdll"         (hasArg (\f d -> d{ pgm_dll = (f,[])}))
  , Flag "pgmwindres"     (hasArg (\f d -> d{ pgm_windres = f}))

    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , Flag "optlo"          (hasArg (\f d -> d{ opt_lo  = f : opt_lo d}))
  , Flag "optlc"          (hasArg (\f d -> d{ opt_lc  = f : opt_lc d}))
  , Flag "optL"           (hasArg (\f d -> d{ opt_L   = f : opt_L d}))
  , Flag "optP"           (hasArg addOptP)
  , Flag "optF"           (hasArg (\f d -> d{ opt_F   = f : opt_F d}))
  , Flag "optc"           (hasArg (\f d -> d{ opt_c   = f : opt_c d}))
  , Flag "optm"           (hasArg (\f d -> d{ opt_m   = f : opt_m d}))
  , Flag "opta"           (hasArg (\f d -> d{ opt_a   = f : opt_a d}))
  , Flag "optl"           (hasArg addOptl)
  , Flag "optwindres"     (hasArg (\f d -> d{ opt_windres = f : opt_windres d}))

  , Flag "split-objs"
         (NoArg (if can_split 
                 then setDynFlag Opt_SplitObjs
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
  , Flag "L"   (Prefix    addLibraryPath)
  , Flag "l"   (AnySuffix (upd . addOptl))

        ------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  , Flag "framework-path" (HasArg addFrameworkPath)
  , Flag "framework"      (hasArg addCmdlineFramework)

        ------- Output Redirection ------------------------------------------
  , Flag "odir"              (hasArg setObjectDir)
  , Flag "o"                 (SepArg (upd . setOutputFile . Just))
  , Flag "ohi"               (hasArg (setOutputHi . Just ))
  , Flag "osuf"              (hasArg setObjectSuf)
  , Flag "hcsuf"             (hasArg setHcSuf)
  , Flag "hisuf"             (hasArg setHiSuf)
  , Flag "hidir"             (hasArg setHiDir)
  , Flag "tmpdir"            (hasArg setTmpDir)
  , Flag "stubdir"           (hasArg setStubDir)
  , Flag "outputdir"         (hasArg setOutputDir)
  , Flag "ddump-file-prefix" (hasArg (setDumpPrefixForce . Just))

        ------- Keeping temporary files -------------------------------------
     -- These can be singular (think ghc -c) or plural (think ghc --make)
  , Flag "keep-hc-file"     (NoArg (setDynFlag Opt_KeepHcFiles))
  , Flag "keep-hc-files"    (NoArg (setDynFlag Opt_KeepHcFiles))
  , Flag "keep-s-file"      (NoArg (setDynFlag Opt_KeepSFiles))
  , Flag "keep-s-files"     (NoArg (setDynFlag Opt_KeepSFiles))
  , Flag "keep-raw-s-file"  (NoArg (setDynFlag Opt_KeepRawSFiles))
  , Flag "keep-raw-s-files" (NoArg (setDynFlag Opt_KeepRawSFiles))
  , Flag "keep-llvm-file"   (NoArg (setDynFlag Opt_KeepLlvmFiles))
  , Flag "keep-llvm-files"  (NoArg (setDynFlag Opt_KeepLlvmFiles))
     -- This only makes sense as plural
  , Flag "keep-tmp-files"   (NoArg (setDynFlag Opt_KeepTmpFiles))

        ------- Miscellaneous ----------------------------------------------
  , Flag "no-auto-link-packages" (NoArg (unSetDynFlag Opt_AutoLinkPackages))
  , Flag "no-hs-main"     (NoArg (setDynFlag Opt_NoHsMain))
  , Flag "with-rtsopts"   (HasArg setRtsOpts)
  , Flag "rtsopts"        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , Flag "rtsopts=all"    (NoArg (setRtsOptsEnabled RtsOptsAll))
  , Flag "rtsopts=some"   (NoArg (setRtsOptsEnabled RtsOptsSafeOnly))
  , Flag "rtsopts=none"   (NoArg (setRtsOptsEnabled RtsOptsNone))
  , Flag "no-rtsopts"     (NoArg (setRtsOptsEnabled RtsOptsNone))
  , Flag "main-is"        (SepArg setMainIs)
  , Flag "haddock"        (NoArg (setDynFlag Opt_Haddock))
  , Flag "haddock-opts"   (hasArg addHaddockOpts)
  , Flag "hpcdir"         (SepArg setOptHpcDir)

        ------- recompilation checker --------------------------------------
  , Flag "recomp"         (NoArg (do { unSetDynFlag Opt_ForceRecomp
                                     ; deprecate "Use -fno-force-recomp instead" }))
  , Flag "no-recomp"      (NoArg (do { setDynFlag Opt_ForceRecomp
                                     ; deprecate "Use -fforce-recomp instead" }))

        ------ HsCpp opts ---------------------------------------------------
  , Flag "D"              (AnySuffix (upd . addOptP))
  , Flag "U"              (AnySuffix (upd . addOptP))

        ------- Include/Import Paths ----------------------------------------
  , Flag "I"              (Prefix    addIncludePath)
  , Flag "i"              (OptPrefix addImportPath)

        ------ Debugging ----------------------------------------------------
  , Flag "dstg-stats"     (NoArg (setDynFlag Opt_StgStats))

  , Flag "ddump-cmm"               (setDumpFlag Opt_D_dump_cmm)
  , Flag "ddump-cmmz"              (setDumpFlag Opt_D_dump_cmmz)
  , Flag "ddump-cmmz-pretty"       (setDumpFlag Opt_D_dump_cmmz_pretty)
  , Flag "ddump-cps-cmm"           (setDumpFlag Opt_D_dump_cps_cmm)
  , Flag "ddump-cvt-cmm"           (setDumpFlag Opt_D_dump_cvt_cmm)
  , Flag "ddump-asm"               (setDumpFlag Opt_D_dump_asm)
  , Flag "ddump-asm-native"        (setDumpFlag Opt_D_dump_asm_native)
  , Flag "ddump-asm-liveness"      (setDumpFlag Opt_D_dump_asm_liveness)
  , Flag "ddump-asm-coalesce"      (setDumpFlag Opt_D_dump_asm_coalesce)
  , Flag "ddump-asm-regalloc"      (setDumpFlag Opt_D_dump_asm_regalloc)
  , Flag "ddump-asm-conflicts"     (setDumpFlag Opt_D_dump_asm_conflicts)
  , Flag "ddump-asm-regalloc-stages" (setDumpFlag Opt_D_dump_asm_regalloc_stages)
  , Flag "ddump-asm-stats"         (setDumpFlag Opt_D_dump_asm_stats)
  , Flag "ddump-asm-expanded"      (setDumpFlag Opt_D_dump_asm_expanded)
  , Flag "ddump-llvm"              (NoArg (do { setObjTarget HscLlvm
                                              ; setDumpFlag' Opt_D_dump_llvm}))
  , Flag "ddump-cpranal"           (setDumpFlag Opt_D_dump_cpranal)
  , Flag "ddump-deriv"             (setDumpFlag Opt_D_dump_deriv)
  , Flag "ddump-ds"                (setDumpFlag Opt_D_dump_ds)
  , Flag "ddump-flatC"             (setDumpFlag Opt_D_dump_flatC)
  , Flag "ddump-foreign"           (setDumpFlag Opt_D_dump_foreign)
  , Flag "ddump-inlinings"         (setDumpFlag Opt_D_dump_inlinings)
  , Flag "ddump-rule-firings"      (setDumpFlag Opt_D_dump_rule_firings)
  , Flag "ddump-occur-anal"        (setDumpFlag Opt_D_dump_occur_anal)
  , Flag "ddump-parsed"            (setDumpFlag Opt_D_dump_parsed)
  , Flag "ddump-rn"                (setDumpFlag Opt_D_dump_rn)
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
  , Flag "ddump-tc-trace"          (setDumpFlag Opt_D_dump_tc_trace)
  , Flag "ddump-splices"           (setDumpFlag Opt_D_dump_splices)
  , Flag "ddump-rn-stats"          (setDumpFlag Opt_D_dump_rn_stats)
  , Flag "ddump-opt-cmm"           (setDumpFlag Opt_D_dump_opt_cmm)
  , Flag "ddump-simpl-stats"       (setDumpFlag Opt_D_dump_simpl_stats)
  , Flag "ddump-bcos"              (setDumpFlag Opt_D_dump_BCOs)
  , Flag "dsource-stats"           (setDumpFlag Opt_D_source_stats)
  , Flag "dverbose-core2core"      (NoArg (do { setVerbosity (Just 2)
                                              ; setVerboseCore2Core }))
  , Flag "dverbose-stg2stg"        (setDumpFlag Opt_D_verbose_stg2stg)
  , Flag "ddump-hi"                (setDumpFlag Opt_D_dump_hi)
  , Flag "ddump-minimal-imports"   (setDumpFlag Opt_D_dump_minimal_imports)
  , Flag "ddump-vect"              (setDumpFlag Opt_D_dump_vect)
  , Flag "ddump-hpc"               (setDumpFlag Opt_D_dump_hpc)
  , Flag "ddump-mod-cycles"        (setDumpFlag Opt_D_dump_mod_cycles)
  , Flag "ddump-view-pattern-commoning" (setDumpFlag Opt_D_dump_view_pattern_commoning)
  , Flag "ddump-to-file"           (setDumpFlag Opt_DumpToFile)
  , Flag "ddump-hi-diffs"          (setDumpFlag Opt_D_dump_hi_diffs)
  , Flag "ddump-rtti"      	   (setDumpFlag Opt_D_dump_rtti)
  , Flag "dcore-lint"              (NoArg (setDynFlag Opt_DoCoreLinting))
  , Flag "dstg-lint"               (NoArg (setDynFlag Opt_DoStgLinting))
  , Flag "dcmm-lint"               (NoArg (setDynFlag Opt_DoCmmLinting))
  , Flag "dasm-lint"               (NoArg (setDynFlag Opt_DoAsmLinting))
  , Flag "dshow-passes"            (NoArg (do forceRecompile
                                              setVerbosity (Just 2)))
  , Flag "dfaststring-stats"       (NoArg (setDynFlag Opt_D_faststring_stats))

        ------ Machine dependant (-m<blah>) stuff ---------------------------

  , Flag "monly-2-regs" (noArg (\s -> s{stolen_x86_regs = 2}))
  , Flag "monly-3-regs" (noArg (\s -> s{stolen_x86_regs = 3}))
  , Flag "monly-4-regs" (noArg (\s -> s{stolen_x86_regs = 4}))
  , Flag "msse2"        (NoArg (setDynFlag Opt_SSE2))

     ------ Warning opts -------------------------------------------------
  , Flag "W"      (NoArg (mapM_ setDynFlag   minusWOpts))
  , Flag "Werror" (NoArg (setDynFlag         Opt_WarnIsError))
  , Flag "Wwarn"  (NoArg (unSetDynFlag       Opt_WarnIsError))
  , Flag "Wall"   (NoArg (mapM_ setDynFlag   minusWallOpts))
  , Flag "Wnot"   (NoArg (do { mapM_ unSetDynFlag minusWallOpts
                             ; deprecate "Use -w instead" }))
  , Flag "w"      (NoArg (mapM_ unSetDynFlag minuswRemovesOpts))

        ------ Optimisation flags ------------------------------------------
  , Flag "O"      (noArg (setOptLevel 1))
  , Flag "Onot"   (noArgDF (setOptLevel 0) "Use -O0 instead")
  , Flag "Odph"   (noArg setDPHOpt)
  , Flag "O"      (OptIntSuffix (\mb_n -> upd (setOptLevel (mb_n `orElse` 1))))
                -- If the number is missing, use 1

  , Flag "fsimplifier-phases"          (intSuffix (\n d -> d{ simplPhases = n }))
  , Flag "fmax-simplifier-iterations"  (intSuffix (\n d -> d{ maxSimplIterations = n }))
  , Flag "fspec-constr-threshold"      (intSuffix (\n d -> d{ specConstrThreshold = Just n }))
  , Flag "fno-spec-constr-threshold"   (noArg (\d -> d{ specConstrThreshold = Nothing }))
  , Flag "fspec-constr-count"          (intSuffix (\n d -> d{ specConstrCount = Just n }))
  , Flag "fno-spec-constr-count"       (noArg (\d -> d{ specConstrCount = Nothing }))
  , Flag "fliberate-case-threshold"    (intSuffix (\n d -> d{ liberateCaseThreshold = Just n }))
  , Flag "fno-liberate-case-threshold" (noArg (\d -> d{ liberateCaseThreshold = Nothing }))
  , Flag "frule-check"                 (SepArg (\s -> upd (\d -> d{ ruleCheck = Just s })))
  , Flag "fcontext-stack"              (intSuffix (\n d -> d{ ctxtStkDepth = n }))
  , Flag "fstrictness-before"          (intSuffix (\n d -> d{ strictnessBefore = n : strictnessBefore d }))

        ------ Profiling ----------------------------------------------------

  -- XXX Should the -f* flags be deprecated?
  -- They don't seem to be documented
  , Flag "fauto-sccs-on-all-toplevs"   	   (NoArg (setDynFlag Opt_AutoSccsOnAllToplevs))
  , Flag "auto-all"                    	   (NoArg (setDynFlag Opt_AutoSccsOnAllToplevs))
  , Flag "no-auto-all"                 	   (NoArg (unSetDynFlag Opt_AutoSccsOnAllToplevs))
  , Flag "fauto-sccs-on-exported-toplevs"  (NoArg (setDynFlag Opt_AutoSccsOnExportedToplevs))
  , Flag "auto"                            (NoArg (setDynFlag Opt_AutoSccsOnExportedToplevs))
  , Flag "no-auto"                         (NoArg (unSetDynFlag Opt_AutoSccsOnExportedToplevs))
  , Flag "fauto-sccs-on-individual-cafs"   (NoArg (setDynFlag Opt_AutoSccsOnIndividualCafs))
  , Flag "caf-all"                         (NoArg (setDynFlag Opt_AutoSccsOnIndividualCafs))
  , Flag "no-caf-all"                      (NoArg (unSetDynFlag Opt_AutoSccsOnIndividualCafs))

        ------ DPH flags ----------------------------------------------------

  , Flag "fdph-seq"         (NoArg (setDPHBackend DPHSeq))
  , Flag "fdph-par"         (NoArg (setDPHBackend DPHPar))
  , Flag "fdph-this"        (NoArg (setDPHBackend DPHThis))

        ------ Compiler flags -----------------------------------------------

  , Flag "fasm"             (NoArg (setObjTarget HscAsm))
  , Flag "fvia-c"           (NoArg (setObjTarget HscC >>
         (addWarn "The -fvia-c flag will be removed in a future GHC release")))
  , Flag "fvia-C"           (NoArg (setObjTarget HscC >>
         (addWarn "The -fvia-C flag will be removed in a future GHC release")))
  , Flag "fllvm"            (NoArg (setObjTarget HscLlvm))

  , Flag "fno-code"         (NoArg (do upd $ \d -> d{ ghcLink=NoLink }
                                       setTarget HscNothing))
  , Flag "fbyte-code"       (NoArg (setTarget HscInterpreted))
  , Flag "fobject-code"     (NoArg (setTarget defaultHscTarget))
  , Flag "fglasgow-exts"    (NoArg enableGlasgowExts)
  , Flag "fno-glasgow-exts" (NoArg disableGlasgowExts)
 ]
 ++ map (mkFlag True  "f"    setDynFlag  ) fFlags
 ++ map (mkFlag False "fno-" unSetDynFlag) fFlags
 ++ map (mkFlag True  "f"    setExtensionFlag  ) fLangFlags
 ++ map (mkFlag False "fno-" unSetExtensionFlag) fLangFlags
 ++ map (mkFlag True  "X"    setExtensionFlag  ) xFlags
 ++ map (mkFlag False "XNo"  unSetExtensionFlag) xFlags
 ++ map (mkFlag True  "X"    setLanguage) languageFlags

package_flags :: [Flag (CmdLineP DynFlags)]
package_flags = [
        ------- Packages ----------------------------------------------------
    Flag "package-conf"         (HasArg extraPkgConf_)
  , Flag "no-user-package-conf" (NoArg (unSetDynFlag Opt_ReadUserPackageConf))
  , Flag "package-name"      	(hasArg setPackageName)
  , Flag "package-id"        	(HasArg exposePackageId)
  , Flag "package"           	(HasArg exposePackage)
  , Flag "hide-package"      	(HasArg hidePackage)
  , Flag "hide-all-packages" 	(NoArg (setDynFlag Opt_HideAllPackages))
  , Flag "ignore-package"    	(HasArg ignorePackage)
  , Flag "syslib"            	(HasArg (\s -> do { exposePackage s
                                                  ; deprecate "Use -package instead" }))
  ]

type FlagSpec flag 
   = ( String	-- Flag in string form
     , flag     -- Flag in internal form
     , Bool -> DynP ())	 -- Extra action to run when the flag is found
       	       	    	 -- Typically, emit a warning or error
       	       	    	 -- True  <=> we are turning the flag on
       	       	    	 -- False <=> we are turning the flag off


mkFlag :: Bool                  -- ^ True <=> it should be turned on
       -> String                -- ^ The flag prefix
       -> (flag -> DynP ())	-- ^ What to do when the flag is found
       -> FlagSpec flag		-- ^ Specification of this particular flag
       -> Flag (CmdLineP DynFlags)
mkFlag turnOn flagPrefix f (name, flag, extra_action)
    = Flag (flagPrefix ++ name) (NoArg (f flag >> extra_action turnOn))

deprecatedForExtension :: String -> Bool -> DynP ()
deprecatedForExtension lang turn_on
    = deprecate ("use -X"  ++ flag ++ " or pragma {-# LANGUAGE " ++ flag ++ " #-} instead")
    where 
      flag | turn_on    = lang
           | otherwise = "No"++lang

useInstead :: String -> Bool -> DynP ()
useInstead flag turn_on
  = deprecate ("Use -f" ++ no ++ flag ++ " instead")
  where
    no = if turn_on then "" else "no-"

nop :: Bool -> DynP ()
nop _ = return ()

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fFlags :: [FlagSpec DynFlag]
fFlags = [
  ( "warn-dodgy-foreign-imports",       Opt_WarnDodgyForeignImports, nop ),
  ( "warn-dodgy-exports",               Opt_WarnDodgyExports, nop ),
  ( "warn-dodgy-imports",               Opt_WarnDodgyImports, nop ),
  ( "warn-duplicate-exports",           Opt_WarnDuplicateExports, nop ),
  ( "warn-hi-shadowing",                Opt_WarnHiShadows, nop ),
  ( "warn-implicit-prelude",            Opt_WarnImplicitPrelude, nop ),
  ( "warn-incomplete-patterns",         Opt_WarnIncompletePatterns, nop ),
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
  ( "warn-auto-orphans",                Opt_WarnAutoOrphans, nop ),
  ( "warn-tabs",                        Opt_WarnTabs, nop ),
  ( "warn-unrecognised-pragmas",        Opt_WarnUnrecognisedPragmas, nop ),
  ( "warn-lazy-unlifted-bindings",      Opt_WarnLazyUnliftedBindings, nop),
  ( "warn-unused-do-bind",              Opt_WarnUnusedDoBind, nop ),
  ( "warn-wrong-do-bind",               Opt_WarnWrongDoBind, nop ),
  ( "warn-alternative-layout-rule-transitional", Opt_WarnAlternativeLayoutRuleTransitional, nop ),
  ( "print-explicit-foralls",           Opt_PrintExplicitForalls, nop ),
  ( "strictness",                       Opt_Strictness, nop ),
  ( "specialise",                       Opt_Specialise, nop ),
  ( "float-in",                         Opt_FloatIn, nop ),
  ( "static-argument-transformation",   Opt_StaticArgumentTransformation, nop ),
  ( "full-laziness",                    Opt_FullLaziness, nop ),
  ( "liberate-case",                    Opt_LiberateCase, nop ),
  ( "spec-constr",                      Opt_SpecConstr, nop ),
  ( "cse",                              Opt_CSE, nop ),
  ( "ignore-interface-pragmas",         Opt_IgnoreInterfacePragmas, nop ),
  ( "omit-interface-pragmas",           Opt_OmitInterfacePragmas, nop ),
  ( "expose-all-unfoldings",            Opt_ExposeAllUnfoldings, nop ),
  ( "do-lambda-eta-expansion",          Opt_DoLambdaEtaExpansion, nop ),
  ( "ignore-asserts",                   Opt_IgnoreAsserts, nop ),
  ( "do-eta-reduction",                 Opt_DoEtaReduction, nop ),
  ( "case-merge",                       Opt_CaseMerge, nop ),
  ( "unbox-strict-fields",              Opt_UnboxStrictFields, nop ),
  ( "method-sharing",                   Opt_MethodSharing, nop ),
  ( "dicts-cheap",                      Opt_DictsCheap, nop ),
  ( "excess-precision",                 Opt_ExcessPrecision, nop ),
  ( "eager-blackholing",                Opt_EagerBlackHoling, nop ),
  ( "asm-mangling",                     Opt_DoAsmMangling, nop ),
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
  ( "new-codegen",                      Opt_TryNewCodeGen, nop ),
  ( "convert-to-zipper-and-back",       Opt_ConvertToZipCfgAndBack, nop ),
  ( "vectorise",                        Opt_Vectorise, nop ),
  ( "regs-graph",                       Opt_RegsGraph, nop ),
  ( "regs-iterative",                   Opt_RegsIterative, nop ),
  ( "gen-manifest",                     Opt_GenManifest, nop ),
  ( "embed-manifest",                   Opt_EmbedManifest, nop ),
  ( "ext-core",                         Opt_EmitExternalCore, nop ),
  ( "shared-implib",                    Opt_SharedImplib, nop ),
  ( "building-cabal-package",           Opt_BuildingCabalPackage, nop ),
  ( "implicit-import-qualified",        Opt_ImplicitImportQualified, nop )
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fLangFlags :: [FlagSpec ExtensionFlag]
fLangFlags = [
  ( "th",                               Opt_TemplateHaskell,
    deprecatedForExtension "TemplateHaskell" >> checkTemplateHaskellOk ),
  ( "fi",                               Opt_ForeignFunctionInterface,
    deprecatedForExtension "ForeignFunctionInterface" ),
  ( "ffi",                              Opt_ForeignFunctionInterface,
    deprecatedForExtension "ForeignFunctionInterface" ),
  ( "arrows",                           Opt_Arrows,
    deprecatedForExtension "Arrows" ),
  ( "generics",                         Opt_Generics,
    deprecatedForExtension "Generics" ),
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
  ( "parr",                             Opt_PArr,
    deprecatedForExtension "PArr" ),
  ( "allow-overlapping-instances",      Opt_OverlappingInstances,
    deprecatedForExtension "OverlappingInstances" ),
  ( "allow-undecidable-instances",      Opt_UndecidableInstances,
    deprecatedForExtension "UndecidableInstances" ),
  ( "allow-incoherent-instances",       Opt_IncoherentInstances,
    deprecatedForExtension "IncoherentInstances" )
  ]

supportedLanguages :: [String]
supportedLanguages = [ name | (name, _, _) <- languageFlags ]

supportedExtensions :: [String]
supportedExtensions = [ name' | (name, _, _) <- xFlags, name' <- [name, "No" ++ name] ]

supportedLanguagesAndExtensions :: [String]
supportedLanguagesAndExtensions = supportedLanguages ++ supportedExtensions

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
languageFlags :: [FlagSpec Language]
languageFlags = [
  ( "Haskell98",                        Haskell98, nop ),
  ( "Haskell2010",                      Haskell2010, nop )
  ]

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [FlagSpec ExtensionFlag]
xFlags = [
  ( "CPP",                              Opt_Cpp, nop ),
  ( "PostfixOperators",                 Opt_PostfixOperators, nop ),
  ( "TupleSections",                    Opt_TupleSections, nop ),
  ( "PatternGuards",                    Opt_PatternGuards, nop ),
  ( "UnicodeSyntax",                    Opt_UnicodeSyntax, nop ),
  ( "MagicHash",                        Opt_MagicHash, nop ),
  ( "PolymorphicComponents",            Opt_PolymorphicComponents, nop ),
  ( "ExistentialQuantification",        Opt_ExistentialQuantification, nop ),
  ( "KindSignatures",                   Opt_KindSignatures, nop ),
  ( "EmptyDataDecls",                   Opt_EmptyDataDecls, nop ),
  ( "ParallelListComp",                 Opt_ParallelListComp, nop ),
  ( "TransformListComp",                Opt_TransformListComp, nop ),
  ( "ForeignFunctionInterface",         Opt_ForeignFunctionInterface, nop ),
  ( "UnliftedFFITypes",                 Opt_UnliftedFFITypes, nop ),
  ( "GHCForeignImportPrim",             Opt_GHCForeignImportPrim, nop ),
  ( "LiberalTypeSynonyms",              Opt_LiberalTypeSynonyms, nop ),
  ( "Rank2Types",                       Opt_Rank2Types, nop ),
  ( "RankNTypes",                       Opt_RankNTypes, nop ),
  ( "ImpredicativeTypes",               Opt_ImpredicativeTypes, nop), 
  ( "TypeOperators",                    Opt_TypeOperators, nop ),
  ( "RecursiveDo",                      Opt_RecursiveDo,
    deprecatedForExtension "DoRec"),
  ( "DoRec",                            Opt_DoRec, nop ),
  ( "Arrows",                           Opt_Arrows, nop ),
  ( "PArr",                             Opt_PArr, nop ),
  ( "TemplateHaskell",                  Opt_TemplateHaskell, checkTemplateHaskellOk ),
  ( "QuasiQuotes",                      Opt_QuasiQuotes, nop ),
  ( "Generics",                         Opt_Generics, nop ),
  ( "ImplicitPrelude",                  Opt_ImplicitPrelude, nop ),
  ( "RecordWildCards",                  Opt_RecordWildCards, nop ),
  ( "NamedFieldPuns",                   Opt_RecordPuns, nop ),
  ( "RecordPuns",                       Opt_RecordPuns,
    deprecatedForExtension "NamedFieldPuns" ),
  ( "DisambiguateRecordFields",         Opt_DisambiguateRecordFields, nop ),
  ( "OverloadedStrings",                Opt_OverloadedStrings, nop ),
  ( "GADTs",                            Opt_GADTs, nop ),
  ( "ViewPatterns",                     Opt_ViewPatterns, nop ),
  ( "TypeFamilies",                     Opt_TypeFamilies, nop ),
  ( "BangPatterns",                     Opt_BangPatterns, nop ),
  ( "MonomorphismRestriction",          Opt_MonomorphismRestriction, nop ),
  ( "NPlusKPatterns",                   Opt_NPlusKPatterns, nop ),
  ( "DoAndIfThenElse",                  Opt_DoAndIfThenElse, nop ),
  ( "MonoPatBinds",                     Opt_MonoPatBinds, nop ),
  ( "ExplicitForAll",                   Opt_ExplicitForAll, nop ),
  ( "AlternativeLayoutRule",            Opt_AlternativeLayoutRule, nop ),
  ( "AlternativeLayoutRuleTransitional",Opt_AlternativeLayoutRuleTransitional, nop ),
  ( "DatatypeContexts",                 Opt_DatatypeContexts, nop ),
  ( "MonoLocalBinds",                   Opt_MonoLocalBinds, nop ),
  ( "RelaxedPolyRec",                   Opt_RelaxedPolyRec, 
    \ turn_on -> if not turn_on 
                 then deprecate "You can't turn off RelaxedPolyRec any more"
                 else return () ),
  ( "ExtendedDefaultRules",             Opt_ExtendedDefaultRules, nop ),
  ( "ImplicitParams",                   Opt_ImplicitParams, nop ),
  ( "ScopedTypeVariables",              Opt_ScopedTypeVariables, nop ),

  ( "PatternSignatures",                Opt_ScopedTypeVariables, 
    deprecatedForExtension "ScopedTypeVariables" ),

  ( "UnboxedTuples",                    Opt_UnboxedTuples, nop ),
  ( "StandaloneDeriving",               Opt_StandaloneDeriving, nop ),
  ( "DeriveDataTypeable",               Opt_DeriveDataTypeable, nop ),
  ( "DeriveFunctor",                    Opt_DeriveFunctor, nop ),
  ( "DeriveTraversable",                Opt_DeriveTraversable, nop ),
  ( "DeriveFoldable",                   Opt_DeriveFoldable, nop ),
  ( "TypeSynonymInstances",             Opt_TypeSynonymInstances, nop ),
  ( "FlexibleContexts",                 Opt_FlexibleContexts, nop ),
  ( "FlexibleInstances",                Opt_FlexibleInstances, nop ),
  ( "ConstrainedClassMethods",          Opt_ConstrainedClassMethods, nop ),
  ( "MultiParamTypeClasses",            Opt_MultiParamTypeClasses, nop ),
  ( "FunctionalDependencies",           Opt_FunctionalDependencies, nop ),
  ( "GeneralizedNewtypeDeriving",       Opt_GeneralizedNewtypeDeriving, nop ),
  ( "OverlappingInstances",             Opt_OverlappingInstances, nop ),
  ( "UndecidableInstances",             Opt_UndecidableInstances, nop ),
  ( "IncoherentInstances",              Opt_IncoherentInstances, nop ),
  ( "PackageImports",                   Opt_PackageImports, nop ),
  ( "NewQualifiedOperators",            Opt_NewQualifiedOperators,
    \_ -> deprecate "The new qualified operator syntax was rejected by Haskell'" )
  ]

defaultFlags :: [DynFlag]
defaultFlags 
  = [ Opt_AutoLinkPackages,
      Opt_ReadUserPackageConf,

      Opt_MethodSharing,

      Opt_DoAsmMangling,

      Opt_SharedImplib,

      Opt_GenManifest,
      Opt_EmbedManifest,
      Opt_PrintBindContents
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    ++ standardWarnings

impliedFlags :: [(ExtensionFlag, ExtensionFlag)]
impliedFlags
  = [ (Opt_RankNTypes,                Opt_ExplicitForAll)
    , (Opt_Rank2Types,                Opt_ExplicitForAll)
    , (Opt_ScopedTypeVariables,       Opt_ExplicitForAll)
    , (Opt_LiberalTypeSynonyms,       Opt_ExplicitForAll)
    , (Opt_ExistentialQuantification, Opt_ExplicitForAll)
    , (Opt_PolymorphicComponents,     Opt_ExplicitForAll)

    , (Opt_GADTs,                  Opt_MonoLocalBinds)
    , (Opt_TypeFamilies,           Opt_MonoLocalBinds)

    , (Opt_TypeFamilies,        Opt_KindSignatures)  -- Type families use kind signatures
      						     -- all over the place

    , (Opt_ImpredicativeTypes,  Opt_RankNTypes)

	-- Record wild-cards implies field disambiguation
	-- Otherwise if you write (C {..}) you may well get
	-- stuff like " 'a' not in scope ", which is a bit silly
 	-- if the compiler has just filled in field 'a' of constructor 'C'
    , (Opt_RecordWildCards,     Opt_DisambiguateRecordFields)
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
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSigs,
        Opt_WarnHiShadows,
        Opt_WarnOrphans,
        Opt_WarnUnusedDoBind
      ]

-- minuswRemovesOpts should be every warning option
minuswRemovesOpts :: [DynFlag]
minuswRemovesOpts
    = minusWallOpts ++
      [Opt_WarnImplicitPrelude,
       Opt_WarnIncompletePatternsRecUpd,
       Opt_WarnMonomorphism,
       Opt_WarnUnrecognisedPragmas,
       Opt_WarnAutoOrphans,
       Opt_WarnTabs
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

checkTemplateHaskellOk :: Bool -> DynP ()
checkTemplateHaskellOk turn_on 
  | turn_on && rtsIsProfiled
  = addErr "You can't use Template Haskell with a profiled compiler"
  | otherwise
  = return ()
#else
-- In stage 1 we don't know that the RTS has rts_isProfiled, 
-- so we simply say "ok".  It doesn't matter because TH isn't
-- available in stage 1 anyway.
checkTemplateHaskellOk turn_on = return ()
#endif

{- **********************************************************************
%*									*
		DynFlags constructors
%*									*
%********************************************************************* -}

type DynP = EwM (CmdLineP DynFlags)

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = liftEwM (do { dfs <- getCmdLineState
                    ; putCmdLineState $! (f dfs) })

--------------- Constructor functions for OptKind -----------------
noArg :: (DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
noArg fn = NoArg (upd fn)

noArgDF :: (DynFlags -> DynFlags) -> String -> OptKind (CmdLineP DynFlags)
noArgDF fn deprec = NoArg (upd fn >> deprecate deprec)

hasArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
hasArg fn = HasArg (upd . fn)

hasArgDF :: (String -> DynFlags -> DynFlags) -> String -> OptKind (CmdLineP DynFlags)
hasArgDF fn deprec = HasArg (\s -> do { upd (fn s)
                                      ; deprecate deprec })

intSuffix :: (Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffix fn = IntSuffix (\n -> upd (fn n))

setDumpFlag :: DynFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

--------------------------
setDynFlag, unSetDynFlag :: DynFlag -> DynP ()
setDynFlag   f = upd (\dfs -> dopt_set dfs f)
unSetDynFlag f = upd (\dfs -> dopt_unset dfs f)

--------------------------
setLanguage :: Language -> DynP ()
setLanguage l = upd (\dfs -> dfs { language = Just l })

--------------------------
setExtensionFlag, unSetExtensionFlag :: ExtensionFlag -> DynP ()
setExtensionFlag f = do { upd (\dfs -> xopt_set dfs f)
                        ; mapM_ setExtensionFlag deps }
  where
    deps = [ d | (f', d) <- impliedFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags
        -- When you un-set f, however, we don't un-set the things it implies
        --      (except for -fno-glasgow-exts, which is treated specially)

unSetExtensionFlag f = upd (\dfs -> xopt_unset dfs f)

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
-- used by -fasm and -fvia-C, which switch from one to the other, but
-- not from bytecode to object-code.  The idea is that -fasm/-fvia-C
-- can be safely used in an OPTIONS_GHC pragma.
setObjTarget :: HscTarget -> DynP ()
setObjTarget l = upd set
  where
   set dfs
     | isObjectTarget (hscTarget dfs) = dfs { hscTarget = l }
     | otherwise = dfs

setOptLevel :: Int -> DynFlags -> DynFlags
setOptLevel n dflags
   | hscTarget dflags == HscInterpreted && n > 0
        = dflags
            -- not in IO any more, oh well:
            -- putStr "warning: -O conflicts with --interactive; -O ignored.\n"
   | otherwise
        = updOptLevel n dflags


-- -Odph is equivalent to
--
--    -O2                               optimise as much as possible
--    -fno-method-sharing               sharing specialisation defeats fusion
--                                      sometimes
--    -fdicts-cheap                     always inline dictionaries
--    -fmax-simplifier-iterations20     this is necessary sometimes
--    -fsimplifier-phases=3             we use an additional simplifier phase
--                                      for fusion
--    -fno-spec-constr-threshold        run SpecConstr even for big loops
--    -fno-spec-constr-count            SpecConstr as much as possible
--    -finline-enough-args              hack to prevent excessive inlining
--
setDPHOpt :: DynFlags -> DynFlags
setDPHOpt dflags = setOptLevel 2 (dflags { maxSimplIterations  = 20
                                         , simplPhases         = 3
                                         , specConstrThreshold = Nothing
                                         , specConstrCount     = Nothing
                                         })
                   `dopt_set`   Opt_DictsCheap
                   `dopt_unset` Opt_MethodSharing

data DPHBackend = DPHPar
                | DPHSeq
                | DPHThis
        deriving(Eq, Ord, Enum, Show)

setDPHBackend :: DPHBackend -> DynP ()
setDPHBackend backend 
  = do
      upd $ \dflags -> dflags { dphBackend = backend }
      mapM_ exposePackage (dph_packages backend)
  where
    dph_packages DPHThis = []
    dph_packages DPHPar  = ["dph-prim-par", "dph-par"]
    dph_packages DPHSeq  = ["dph-prim-seq", "dph-seq"]

dphPackage :: DynFlags -> PackageId
dphPackage dflags = case dphBackend dflags of
                      DPHPar  -> dphParPackageId
                      DPHSeq  -> dphSeqPackageId
                      DPHThis -> thisPackage dflags

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
setTmpDir dir dflags = dflags{ tmpDir = normalise dir }
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
-- and puts them in the file "extra-gcc-opts" in $topdir, which is
-- read before each via-C compilation.  The advantage of having these
-- in a separate file is that the file can be created at install-time
-- depending on the available gcc version, and even re-generated  later
-- if gcc is upgraded.
--
-- The options below are not dependent on the version of gcc, only the
-- platform.

machdepCCOpts :: DynFlags -> ([String], -- flags for all C compilations
                              [String]) -- for registerised HC compilations
machdepCCOpts dflags = let (flagsAll, flagsRegHc) = machdepCCOpts' dflags
                       in (cCcOpts ++ flagsAll, flagsRegHc)

machdepCCOpts' :: DynFlags -> ([String], -- flags for all C compilations
                               [String]) -- for registerised HC compilations
machdepCCOpts' _dflags
#if alpha_TARGET_ARCH
        =       ( ["-w", "-mieee"
#ifdef HAVE_THREADED_RTS_SUPPORT
                    , "-D_REENTRANT"
#endif
                   ], [] )
        -- For now, to suppress the gcc warning "call-clobbered
        -- register used for global register variable", we simply
        -- disable all warnings altogether using the -w flag. Oh well.

#elif hppa_TARGET_ARCH
        -- ___HPUX_SOURCE, not _HPUX_SOURCE, is #defined if -ansi!
        -- (very nice, but too bad the HP /usr/include files don't agree.)
        = ( ["-D_HPUX_SOURCE"], [] )

#elif m68k_TARGET_ARCH
      -- -fno-defer-pop : for the .hc files, we want all the pushing/
      --    popping of args to routines to be explicit; if we let things
      --    be deferred 'til after an STGJUMP, imminent death is certain!
      --
      -- -fomit-frame-pointer : *don't*
      --     It's better to have a6 completely tied up being a frame pointer
      --     rather than let GCC pick random things to do with it.
      --     (If we want to steal a6, then we would try to do things
      --     as on iX86, where we *do* steal the frame pointer [%ebp].)
        = ( [], ["-fno-defer-pop", "-fno-omit-frame-pointer"] )

#elif i386_TARGET_ARCH
      -- -fno-defer-pop : basically the same game as for m68k
      --
      -- -fomit-frame-pointer : *must* in .hc files; because we're stealing
      --   the fp (%ebp) for our register maps.
        =  let n_regs = stolen_x86_regs _dflags
           in
                    (
                      [ if opt_Static then "-DDONT_WANT_WIN32_DLL_SUPPORT" else ""
                      ],
                      [ "-fno-defer-pop",
                        "-fomit-frame-pointer",
                        -- we want -fno-builtin, because when gcc inlines
                        -- built-in functions like memcpy() it tends to
                        -- run out of registers, requiring -monly-n-regs
                        "-fno-builtin",
                        "-DSTOLEN_X86_REGS="++show n_regs ]
                    )

#elif ia64_TARGET_ARCH
        = ( [], ["-fomit-frame-pointer", "-G0"] )

#elif x86_64_TARGET_ARCH
        = (
                [],
                ["-fomit-frame-pointer",
                 "-fno-asynchronous-unwind-tables",
                        -- the unwind tables are unnecessary for HC code,
                        -- and get in the way of -split-objs.  Another option
                        -- would be to throw them away in the mangler, but this
                        -- is easier.
                 "-fno-builtin"
                        -- calling builtins like strlen() using the FFI can
                        -- cause gcc to run out of regs, so use the external
                        -- version.
                ] )

#elif sparc_TARGET_ARCH
        = ( [], ["-w"] )
        -- For now, to suppress the gcc warning "call-clobbered
        -- register used for global register variable", we simply
        -- disable all warnings altogether using the -w flag. Oh well.

#elif powerpc_apple_darwin_TARGET
      -- -no-cpp-precomp:
      --     Disable Apple's precompiling preprocessor. It's a great thing
      --     for "normal" programs, but it doesn't support register variable
      --     declarations.
        = ( [], ["-no-cpp-precomp"] )
#else
        = ( [], [] )
#endif

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
can_split = cSplitObjs == "YES"

-- -----------------------------------------------------------------------------
-- Compiler Info

data Printable = String String
               | FromDynFlags (DynFlags -> String)

compilerInfo :: [(String, Printable)]
compilerInfo = [("Project name",                String cProjectName),
                ("Project version",             String cProjectVersion),
                ("Booter version",              String cBooterVersion),
                ("Stage",                       String cStage),
                ("Build platform",              String cBuildPlatform),
                ("Host platform",               String cHostPlatform),
                ("Target platform",             String cTargetPlatform),
                ("Have interpreter",            String cGhcWithInterpreter),
                ("Object splitting",            String cSplitObjs),
                ("Have native code generator",  String cGhcWithNativeCodeGen),
                ("Have llvm code generator",    String cGhcWithLlvmCodeGen),
                ("Use archives for ghci",       String (show cUseArchivesForGhci)),
                ("Support SMP",                 String cGhcWithSMP),
                ("Unregisterised",              String cGhcUnregisterised),
                ("Tables next to code",         String cGhcEnableTablesNextToCode),
                ("RTS ways",                    String cGhcRTSWays),
                ("Leading underscore",          String cLeadingUnderscore),
                ("Debug on",                    String (show debugIsOn)),
                ("LibDir",                      FromDynFlags topDir),
                ("Global Package DB",           FromDynFlags systemPackageConfig)
               ]

