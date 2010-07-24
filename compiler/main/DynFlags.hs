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
        DOpt(..),
        DynFlag(..),
        ExtensionFlag(..),
        flattenExtensionFlags,
        ensureFlattenedExtensionFlags,
        lopt_set_flattened,
        lopt_unset_flattened,
        DynFlags(..),
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

        supportedExtensions, extensionOptions,

        -- ** DynFlag C compiler options
        machdepCCOpts, picCCOpts,

        -- * Configuration of the stg-to-stg passes
        StgToDo(..),
        getStgToDo,

        -- * Compiler configuration suitable for display to the user
        Printable(..),
        compilerInfo
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
import FiniteMap
import Outputable
import {-# SOURCE #-} ErrUtils ( Severity(..), Message, mkLocMessage )

import Data.IORef
import Control.Monad    ( when )

import Data.Char
import Data.List
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
   | Opt_WarnNameShadowing
   | Opt_WarnOverlappingPatterns
   | Opt_WarnSimplePatterns
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
   | Opt_RtsOptsEnabled
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

data ExtensionFlag
   = Opt_Cpp
   | Opt_OverlappingInstances
   | Opt_UndecidableInstances
   | Opt_IncoherentInstances
   | Opt_MonomorphismRestriction
   | Opt_MonoPatBinds
   | Opt_MonoLocalBinds
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
   | Opt_RelaxedPolyRec
   | Opt_NPlusKPatterns

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
  dirsToClean           :: IORef (FiniteMap FilePath FilePath),

  -- hsc dynamic flags
  flags                 :: [DynFlag],
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
  | Wrapped (Maybe String)
  | SystemDependent
  deriving Eq

-- | Used by 'GHC.newSession' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 -- someday these will be dynamic flags
 ways <- readIORef v_Ways
 refFilesToClean <- newIORef []
 refDirsToClean <- newIORef emptyFM
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
        flags = [
            Opt_AutoLinkPackages,
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
            ++ standardWarnings,

        extensionFlags = Left [],

        log_action = \severity srcSpan style msg ->
                        case severity of
                          SevInfo  -> printErrs (msg style)
                          SevFatal -> printErrs (msg style)
                          _        -> do 
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
              extensionFlags = Right $ flattenExtensionFlags' onoffs
          }
      Right _ ->
          panic "Flattening already-flattened extension flags"

ensureFlattenedExtensionFlags :: DynFlags -> DynFlags
ensureFlattenedExtensionFlags dflags
    = case extensionFlags dflags of
      Left onoffs ->
          dflags {
              extensionFlags = Right $ flattenExtensionFlags' onoffs
          }
      Right _ ->
          dflags

-- OnOffs accumulate in reverse order, so we use foldr in order to
-- process them in the right order
flattenExtensionFlags' :: [OnOff ExtensionFlag] -> [ExtensionFlag]
flattenExtensionFlags' = foldr f defaultExtensionFlags
    where f (On f)  flags = f : delete f flags
          f (Off f) flags =     delete f flags
          defaultExtensionFlags = [
            Opt_MonoPatBinds,   -- Experimentally, I'm making this non-standard
                                -- behaviour the default, to see if anyone notices
                                -- SLPJ July 06

            Opt_ImplicitPrelude,
            Opt_MonomorphismRestriction,
            Opt_NPlusKPatterns,
            Opt_DatatypeContexts
            ]

-- The DOpt class is a temporary workaround, to avoid having to do
-- a mass-renaming dopt->lopt at the moment
class DOpt a where
    dopt :: a -> DynFlags -> Bool
    dopt_set :: DynFlags -> a -> DynFlags
    dopt_unset :: DynFlags -> a -> DynFlags

instance DOpt DynFlag where
    dopt = dopt'
    dopt_set = dopt_set'
    dopt_unset = dopt_unset'

instance DOpt ExtensionFlag where
    dopt = lopt
    dopt_set = lopt_set
    dopt_unset = lopt_unset

-- | Test whether a 'DynFlag' is set
dopt' :: DynFlag -> DynFlags -> Bool
dopt' f dflags  = f `elem` (flags dflags)

-- | Set a 'DynFlag'
dopt_set' :: DynFlags -> DynFlag -> DynFlags
dopt_set' dfs f = dfs{ flags = f : flags dfs }

-- | Unset a 'DynFlag'
dopt_unset' :: DynFlags -> DynFlag -> DynFlags
dopt_unset' dfs f = dfs{ flags = filter (/= f) (flags dfs) }

-- | Test whether a 'ExtensionFlag' is set
lopt :: ExtensionFlag -> DynFlags -> Bool
lopt f dflags = case extensionFlags dflags of
                Left _ -> panic ("Testing for extension flag " ++ show f ++ " before flattening")
                Right flags -> f `elem` flags

-- | Set a 'ExtensionFlag'
lopt_set :: DynFlags -> ExtensionFlag -> DynFlags
lopt_set dfs f = case extensionFlags dfs of
                 Left onoffs -> dfs { extensionFlags = Left (On f : onoffs) }
                 Right _ -> panic ("Setting extension flag " ++ show f ++ " after flattening")

-- | Set a 'ExtensionFlag'
lopt_set_flattened :: DynFlags -> ExtensionFlag -> DynFlags
lopt_set_flattened dfs f = case extensionFlags dfs of
                           Left _ ->
                               panic ("Setting extension flag " ++ show f ++ " before flattening, but expected flattened")
                           Right flags ->
                               dfs { extensionFlags = Right (f : delete f flags) }

-- | Unset a 'ExtensionFlag'
lopt_unset :: DynFlags -> ExtensionFlag -> DynFlags
lopt_unset dfs f = case extensionFlags dfs of
                   Left onoffs -> dfs { extensionFlags = Left (Off f : onoffs) }
                   Right _ -> panic ("Unsetting extension flag " ++ show f ++ " after flattening")

-- | Unset a 'ExtensionFlag'
lopt_unset_flattened :: DynFlags -> ExtensionFlag -> DynFlags
lopt_unset_flattened dfs f = case extensionFlags dfs of
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
         setPgmP, setPgmL, setPgmF, setPgmc, setPgmm, setPgms, setPgma, setPgml, setPgmdll, setPgmwindres,
         setPgmlo, setPgmlc,
         addOptL, addOptP, addOptF, addOptc, addOptm, addOpta, addOptl, addOptwindres, addOptlo, addOptlc,
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
   ("wrapped", "")      -> d{ dynLibLoader = Wrapped Nothing }
   ("wrapped:", "hard") -> d{ dynLibLoader = Wrapped Nothing }
   ("wrapped:", flex)   -> d{ dynLibLoader = Wrapped (Just flex) }
   _                    -> ghcError (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f d = let (pgm:args) = words f in d{ pgm_P   = (pgm, map Option args)}

setPgmL   f d = d{ pgm_L   = f}
setPgmF   f d = d{ pgm_F   = f}
setPgmc   f d = d{ pgm_c   = (f,[])}
setPgmm   f d = d{ pgm_m   = (f,[])}
setPgms   f d = d{ pgm_s   = (f,[])}
setPgma   f d = d{ pgm_a   = (f,[])}
setPgml   f d = d{ pgm_l   = (f,[])}
setPgmdll f d = d{ pgm_dll = (f,[])}
setPgmwindres f d = d{ pgm_windres = f}
setPgmlo  f d = d{ pgm_lo  = (f,[])}
setPgmlc  f d = d{ pgm_lc  = (f,[])}

addOptL   f d = d{ opt_L   = f : opt_L d}
addOptP   f d = d{ opt_P   = f : opt_P d}
addOptF   f d = d{ opt_F   = f : opt_F d}
addOptc   f d = d{ opt_c   = f : opt_c d}
addOptm   f d = d{ opt_m   = f : opt_m d}
addOpta   f d = d{ opt_a   = f : opt_a d}
addOptl   f d = d{ opt_l   = f : opt_l d}
addOptwindres f d = d{ opt_windres = f : opt_windres d}
addOptlo  f d = d{ opt_lo  = f : opt_lo d}
addOptlc  f d = d{ opt_lc  = f : opt_lc d}

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
       Opt_WarnSimplePatterns,
       Opt_WarnMonomorphism,
       Opt_WarnUnrecognisedPragmas,
       Opt_WarnTabs
      ]

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

-- -----------------------------------------------------------------------------
-- DynFlags parser

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

dynamic_flags :: [Flag DynP]
dynamic_flags = [
    Flag "n"              (NoArg  (setDynFlag Opt_DryRun)) Supported
  , Flag "cpp"            (NoArg  (setExtensionFlag Opt_Cpp)) Supported
  , Flag "F"              (NoArg  (setDynFlag Opt_Pp)) Supported
  , Flag "#include"       (HasArg (addCmdlineHCInclude))
                             (DeprecatedFullText "-#include and INCLUDE pragmas are deprecated: They no longer have any effect")
  , Flag "v"              (OptIntSuffix setVerbosity) Supported

        ------- Specific phases  --------------------------------------------
    -- need to appear before -pgmL to be parsed as LLVM flags.
  , Flag "pgmlo"         (HasArg (upd . setPgmlo)) Supported
  , Flag "pgmlc"         (HasArg (upd . setPgmlc)) Supported

  , Flag "pgmL"           (HasArg (upd . setPgmL)) Supported
  , Flag "pgmP"           (HasArg (upd . setPgmP)) Supported
  , Flag "pgmF"           (HasArg (upd . setPgmF)) Supported
  , Flag "pgmc"           (HasArg (upd . setPgmc)) Supported
  , Flag "pgmm"           (HasArg (upd . setPgmm)) Supported
  , Flag "pgms"           (HasArg (upd . setPgms)) Supported
  , Flag "pgma"           (HasArg (upd . setPgma)) Supported
  , Flag "pgml"           (HasArg (upd . setPgml)) Supported
  , Flag "pgmdll"         (HasArg (upd . setPgmdll)) Supported
  , Flag "pgmwindres"     (HasArg (upd . setPgmwindres)) Supported

    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , Flag "optlo"          (HasArg (upd . addOptlo)) Supported
  , Flag "optlc"          (HasArg (upd . addOptlc)) Supported

  , Flag "optL"           (HasArg (upd . addOptL)) Supported
  , Flag "optP"           (HasArg (upd . addOptP)) Supported
  , Flag "optF"           (HasArg (upd . addOptF)) Supported
  , Flag "optc"           (HasArg (upd . addOptc)) Supported
  , Flag "optm"           (HasArg (upd . addOptm)) Supported
  , Flag "opta"           (HasArg (upd . addOpta)) Supported
  , Flag "optl"           (HasArg (upd . addOptl)) Supported
  , Flag "optwindres"     (HasArg (upd . addOptwindres)) Supported

  , Flag "split-objs"
         (NoArg (if can_split then setDynFlag Opt_SplitObjs else return ()))
         Supported

        -------- ghc -M -----------------------------------------------------
  , Flag "dep-suffix"               (HasArg (upd . addDepSuffix)) Supported
  , Flag "optdep-s"                 (HasArg (upd . addDepSuffix))
         (Deprecated "Use -dep-suffix instead")
  , Flag "dep-makefile"             (HasArg (upd . setDepMakefile)) Supported
  , Flag "optdep-f"                 (HasArg (upd . setDepMakefile))
         (Deprecated "Use -dep-makefile instead")
  , Flag "optdep-w"                 (NoArg  (return ()))
         (Deprecated "-optdep-w doesn't do anything")
  , Flag "include-pkg-deps" (NoArg  (upd (setDepIncludePkgDeps True))) Supported
  , Flag "optdep--include-prelude"  (NoArg  (upd (setDepIncludePkgDeps True)))
         (Deprecated "Use -include-pkg-deps instead")
  , Flag "optdep--include-pkg-deps" (NoArg  (upd (setDepIncludePkgDeps True)))
         (Deprecated "Use -include-pkg-deps instead")
  , Flag "exclude-module"           (HasArg (upd . addDepExcludeMod)) Supported
  , Flag "optdep--exclude-module"   (HasArg (upd . addDepExcludeMod))
         (Deprecated "Use -exclude-module instead")
  , Flag "optdep-x"                 (HasArg (upd . addDepExcludeMod))
         (Deprecated "Use -exclude-module instead")

        -------- Linking ----------------------------------------------------
  , Flag "no-link"        (NoArg (upd $ \d -> d{ ghcLink=NoLink } ))
         Supported
  , Flag "shared"         (NoArg (upd $ \d -> d{ ghcLink=LinkDynLib } ))
         Supported
  , Flag "dynload"        (HasArg (upd . parseDynLibLoaderMode))
         Supported
  , Flag "dylib-install-name" (HasArg (upd . setDylibInstallName)) Supported

        ------- Libraries ---------------------------------------------------
  , Flag "L"              (Prefix addLibraryPath ) Supported
  , Flag "l"              (AnySuffix (\s -> do upd (addOptl s))) Supported

        ------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  , Flag "framework-path" (HasArg addFrameworkPath ) Supported
  , Flag "framework"      (HasArg (upd . addCmdlineFramework)) Supported

        ------- Output Redirection ------------------------------------------
  , Flag "odir"           (HasArg (upd . setObjectDir)) Supported
  , Flag "o"              (SepArg (upd . setOutputFile . Just)) Supported
  , Flag "ohi"            (HasArg (upd . setOutputHi   . Just )) Supported
  , Flag "osuf"           (HasArg (upd . setObjectSuf)) Supported
  , Flag "hcsuf"          (HasArg (upd . setHcSuf)) Supported
  , Flag "hisuf"          (HasArg (upd . setHiSuf)) Supported
  , Flag "hidir"          (HasArg (upd . setHiDir)) Supported
  , Flag "tmpdir"         (HasArg (upd . setTmpDir)) Supported
  , Flag "stubdir"        (HasArg (upd . setStubDir)) Supported
  , Flag "outputdir"      (HasArg (upd . setOutputDir)) Supported
  , Flag "ddump-file-prefix" (HasArg (upd . setDumpPrefixForce . Just))
         Supported

        ------- Keeping temporary files -------------------------------------
     -- These can be singular (think ghc -c) or plural (think ghc --make)
  , Flag "keep-hc-file"     (NoArg (setDynFlag Opt_KeepHcFiles)) Supported
  , Flag "keep-hc-files"    (NoArg (setDynFlag Opt_KeepHcFiles)) Supported
  , Flag "keep-s-file"      (NoArg (setDynFlag Opt_KeepSFiles)) Supported
  , Flag "keep-s-files"     (NoArg (setDynFlag Opt_KeepSFiles)) Supported
  , Flag "keep-raw-s-file"  (NoArg (setDynFlag Opt_KeepRawSFiles)) Supported
  , Flag "keep-raw-s-files" (NoArg (setDynFlag Opt_KeepRawSFiles)) Supported
  , Flag "keep-llvm-file"   (NoArg (setDynFlag Opt_KeepLlvmFiles)) Supported
  , Flag "keep-llvm-files"  (NoArg (setDynFlag Opt_KeepLlvmFiles)) Supported
     -- This only makes sense as plural
  , Flag "keep-tmp-files"   (NoArg (setDynFlag Opt_KeepTmpFiles)) Supported

        ------- Miscellaneous ----------------------------------------------
  , Flag "no-auto-link-packages" (NoArg (unSetDynFlag Opt_AutoLinkPackages)) Supported
  , Flag "no-hs-main"     (NoArg (setDynFlag Opt_NoHsMain)) Supported
  , Flag "with-rtsopts"   (HasArg setRtsOpts) Supported
  , Flag "rtsopts"        (NoArg (setDynFlag Opt_RtsOptsEnabled)) Supported
  , Flag "no-rtsopts"     (NoArg (unSetDynFlag Opt_RtsOptsEnabled)) Supported
  , Flag "main-is"        (SepArg setMainIs ) Supported
  , Flag "haddock"        (NoArg (setDynFlag Opt_Haddock)) Supported
  , Flag "haddock-opts"   (HasArg (upd . addHaddockOpts)) Supported
  , Flag "hpcdir"         (SepArg setOptHpcDir) Supported

        ------- recompilation checker --------------------------------------
  , Flag "recomp"         (NoArg (unSetDynFlag Opt_ForceRecomp))
         (Deprecated "Use -fno-force-recomp instead")
  , Flag "no-recomp"      (NoArg (setDynFlag   Opt_ForceRecomp))
         (Deprecated "Use -fforce-recomp instead")

        ------ HsCpp opts ---------------------------------------------------
  , Flag "D"              (AnySuffix (upd . addOptP)) Supported
  , Flag "U"              (AnySuffix (upd . addOptP)) Supported

        ------- Include/Import Paths ----------------------------------------
  , Flag "I"              (Prefix    addIncludePath) Supported
  , Flag "i"              (OptPrefix addImportPath ) Supported

        ------ Debugging ----------------------------------------------------
  , Flag "dstg-stats"     (NoArg (setDynFlag Opt_StgStats)) Supported

  , Flag "ddump-cmm"               (setDumpFlag Opt_D_dump_cmm)
         Supported
  , Flag "ddump-cmmz"              (setDumpFlag Opt_D_dump_cmmz)
         Supported
  , Flag "ddump-cmmz-pretty"       (setDumpFlag Opt_D_dump_cmmz_pretty)
         Supported
  , Flag "ddump-cps-cmm"           (setDumpFlag Opt_D_dump_cps_cmm)
         Supported
  , Flag "ddump-cvt-cmm"           (setDumpFlag Opt_D_dump_cvt_cmm)
         Supported
  , Flag "ddump-asm"               (setDumpFlag Opt_D_dump_asm)
         Supported
  , Flag "ddump-asm-native"        (setDumpFlag Opt_D_dump_asm_native)
         Supported
  , Flag "ddump-asm-liveness"      (setDumpFlag Opt_D_dump_asm_liveness)
         Supported
  , Flag "ddump-asm-coalesce"      (setDumpFlag Opt_D_dump_asm_coalesce)
         Supported
  , Flag "ddump-asm-regalloc"      (setDumpFlag Opt_D_dump_asm_regalloc)
         Supported
  , Flag "ddump-asm-conflicts"     (setDumpFlag Opt_D_dump_asm_conflicts)
         Supported
  , Flag "ddump-asm-regalloc-stages"
                                 (setDumpFlag Opt_D_dump_asm_regalloc_stages)
         Supported
  , Flag "ddump-asm-stats"         (setDumpFlag Opt_D_dump_asm_stats)
         Supported
  , Flag "ddump-asm-expanded"      (setDumpFlag Opt_D_dump_asm_expanded)
         Supported
  , Flag "ddump-llvm"              (NoArg (do { setObjTarget HscLlvm
                                              ; setDumpFlag' Opt_D_dump_llvm}))
         Supported
  , Flag "ddump-cpranal"           (setDumpFlag Opt_D_dump_cpranal)
         Supported
  , Flag "ddump-deriv"             (setDumpFlag Opt_D_dump_deriv)
         Supported
  , Flag "ddump-ds"                (setDumpFlag Opt_D_dump_ds)
         Supported
  , Flag "ddump-flatC"             (setDumpFlag Opt_D_dump_flatC)
         Supported
  , Flag "ddump-foreign"           (setDumpFlag Opt_D_dump_foreign)
         Supported
  , Flag "ddump-inlinings"         (setDumpFlag Opt_D_dump_inlinings)
         Supported
  , Flag "ddump-rule-firings"      (setDumpFlag Opt_D_dump_rule_firings)
         Supported
  , Flag "ddump-occur-anal"        (setDumpFlag Opt_D_dump_occur_anal)
         Supported
  , Flag "ddump-parsed"            (setDumpFlag Opt_D_dump_parsed)
         Supported
  , Flag "ddump-rn"                (setDumpFlag Opt_D_dump_rn)
         Supported
  , Flag "ddump-simpl"             (setDumpFlag Opt_D_dump_simpl)
         Supported
  , Flag "ddump-simpl-iterations"  (setDumpFlag Opt_D_dump_simpl_iterations)
         Supported
  , Flag "ddump-simpl-phases"      (OptPrefix setDumpSimplPhases)
         Supported
  , Flag "ddump-spec"              (setDumpFlag Opt_D_dump_spec)
         Supported
  , Flag "ddump-prep"              (setDumpFlag Opt_D_dump_prep)
         Supported
  , Flag "ddump-stg"               (setDumpFlag Opt_D_dump_stg)
         Supported
  , Flag "ddump-stranal"           (setDumpFlag Opt_D_dump_stranal)
         Supported
  , Flag "ddump-tc"                (setDumpFlag Opt_D_dump_tc)
         Supported
  , Flag "ddump-types"             (setDumpFlag Opt_D_dump_types)
         Supported
  , Flag "ddump-rules"             (setDumpFlag Opt_D_dump_rules)
         Supported
  , Flag "ddump-cse"               (setDumpFlag Opt_D_dump_cse)
         Supported
  , Flag "ddump-worker-wrapper"    (setDumpFlag Opt_D_dump_worker_wrapper)
         Supported
  , Flag "ddump-rn-trace"          (setDumpFlag Opt_D_dump_rn_trace)
         Supported
  , Flag "ddump-if-trace"          (setDumpFlag Opt_D_dump_if_trace)
         Supported
  , Flag "ddump-tc-trace"          (setDumpFlag Opt_D_dump_tc_trace)
         Supported
  , Flag "ddump-splices"           (setDumpFlag Opt_D_dump_splices)
         Supported
  , Flag "ddump-rn-stats"          (setDumpFlag Opt_D_dump_rn_stats)
         Supported
  , Flag "ddump-opt-cmm"           (setDumpFlag Opt_D_dump_opt_cmm)
         Supported
  , Flag "ddump-simpl-stats"       (setDumpFlag Opt_D_dump_simpl_stats)
         Supported
  , Flag "ddump-bcos"              (setDumpFlag Opt_D_dump_BCOs)
         Supported
  , Flag "dsource-stats"           (setDumpFlag Opt_D_source_stats)
         Supported
  , Flag "dverbose-core2core"      (NoArg (do { setVerbosity (Just 2)
                                              ; setVerboseCore2Core }))
         Supported
  , Flag "dverbose-stg2stg"        (setDumpFlag Opt_D_verbose_stg2stg)
         Supported
  , Flag "ddump-hi"                (setDumpFlag Opt_D_dump_hi)
         Supported
  , Flag "ddump-minimal-imports"   (setDumpFlag Opt_D_dump_minimal_imports)
         Supported
  , Flag "ddump-vect"              (setDumpFlag Opt_D_dump_vect)
         Supported
  , Flag "ddump-hpc"               (setDumpFlag Opt_D_dump_hpc)
         Supported
  , Flag "ddump-mod-cycles"        (setDumpFlag Opt_D_dump_mod_cycles)
         Supported
  , Flag "ddump-view-pattern-commoning" (setDumpFlag Opt_D_dump_view_pattern_commoning)
         Supported
  , Flag "ddump-to-file"           (setDumpFlag Opt_DumpToFile)
         Supported
  , Flag "ddump-hi-diffs"          (setDumpFlag Opt_D_dump_hi_diffs)
         Supported
  , Flag "ddump-rtti"      	   (setDumpFlag Opt_D_dump_rtti)
         Supported

  , Flag "dcore-lint"              (NoArg (setDynFlag Opt_DoCoreLinting))
         Supported
  , Flag "dstg-lint"               (NoArg (setDynFlag Opt_DoStgLinting))
         Supported
  , Flag "dcmm-lint"               (NoArg (setDynFlag Opt_DoCmmLinting))
         Supported
  , Flag "dasm-lint"               (NoArg (setDynFlag Opt_DoAsmLinting))
         Supported
  , Flag "dshow-passes"
         (NoArg (do forceRecompile
                    setVerbosity (Just 2)))
         Supported
  , Flag "dfaststring-stats"       (NoArg (setDynFlag Opt_D_faststring_stats))
         Supported

        ------ Machine dependant (-m<blah>) stuff ---------------------------

  , Flag "monly-2-regs" (NoArg (upd (\s -> s{stolen_x86_regs = 2}) ))
         Supported
  , Flag "monly-3-regs" (NoArg (upd (\s -> s{stolen_x86_regs = 3}) ))
         Supported
  , Flag "monly-4-regs" (NoArg (upd (\s -> s{stolen_x86_regs = 4}) ))
         Supported

  , Flag "msse2" (NoArg (setDynFlag Opt_SSE2))
         Supported

     ------ Warning opts -------------------------------------------------
  , Flag "W"      (NoArg (mapM_ setDynFlag   minusWOpts))
         Supported
  , Flag "Werror" (NoArg (setDynFlag         Opt_WarnIsError))
         Supported
  , Flag "Wwarn"  (NoArg (unSetDynFlag       Opt_WarnIsError))
         Supported
  , Flag "Wall"   (NoArg (mapM_ setDynFlag   minusWallOpts))
         Supported
  , Flag "Wnot"   (NoArg (mapM_ unSetDynFlag minusWallOpts))
         (Deprecated "Use -w instead")
  , Flag "w"      (NoArg (mapM_ unSetDynFlag minuswRemovesOpts))
         Supported

        ------ Optimisation flags ------------------------------------------
  , Flag "O"      (NoArg (upd (setOptLevel 1))) Supported
  , Flag "Onot"   (NoArg (upd (setOptLevel 0)))
         (Deprecated "Use -O0 instead")
  , Flag "Odph"   (NoArg (upd setDPHOpt)) Supported
  , Flag "O"      (OptIntSuffix (\mb_n -> upd (setOptLevel (mb_n `orElse` 1))))
         Supported
                -- If the number is missing, use 1

  , Flag "fsimplifier-phases"
         (IntSuffix (\n -> upd (\dfs -> dfs{ simplPhases = n })))
         Supported
  , Flag "fmax-simplifier-iterations"
         (IntSuffix (\n -> upd (\dfs -> dfs{ maxSimplIterations = n })))
         Supported

  , Flag "fspec-constr-threshold"
         (IntSuffix (\n -> upd (\dfs -> dfs{ specConstrThreshold = Just n })))
         Supported
  , Flag "fno-spec-constr-threshold"
         (NoArg (upd (\dfs -> dfs{ specConstrThreshold = Nothing })))
         Supported
  , Flag "fspec-constr-count"
         (IntSuffix (\n -> upd (\dfs -> dfs{ specConstrCount = Just n })))
         Supported
  , Flag "fno-spec-constr-count"
         (NoArg (upd (\dfs -> dfs{ specConstrCount = Nothing })))
         Supported
  , Flag "fliberate-case-threshold"
         (IntSuffix (\n -> upd (\dfs -> dfs{ liberateCaseThreshold = Just n })))
         Supported
  , Flag "fno-liberate-case-threshold"
         (NoArg (upd (\dfs -> dfs{ liberateCaseThreshold = Nothing })))
         Supported

  , Flag "frule-check"
         (SepArg (\s -> upd (\dfs -> dfs{ ruleCheck = Just s })))
         Supported
  , Flag "fcontext-stack"
         (IntSuffix $ \n -> upd $ \dfs -> dfs{ ctxtStkDepth = n })
         Supported

  , Flag "fstrictness-before"
         (IntSuffix (\n -> upd (\dfs -> dfs{ strictnessBefore = n : strictnessBefore dfs })))
         Supported

        ------ Profiling ----------------------------------------------------

  -- XXX Should the -f* flags be deprecated?
  -- They don't seem to be documented
  , Flag "fauto-sccs-on-all-toplevs"
         (NoArg (setDynFlag Opt_AutoSccsOnAllToplevs))
         Supported
  , Flag "auto-all"
         (NoArg (setDynFlag Opt_AutoSccsOnAllToplevs))
         Supported
  , Flag "no-auto-all"
         (NoArg (unSetDynFlag Opt_AutoSccsOnAllToplevs))
         Supported
  , Flag "fauto-sccs-on-exported-toplevs"
         (NoArg (setDynFlag Opt_AutoSccsOnExportedToplevs))
         Supported
  , Flag "auto"
         (NoArg (setDynFlag Opt_AutoSccsOnExportedToplevs))
         Supported
  , Flag "no-auto"
         (NoArg (unSetDynFlag Opt_AutoSccsOnExportedToplevs))
         Supported
  , Flag "fauto-sccs-on-individual-cafs"
         (NoArg (setDynFlag Opt_AutoSccsOnIndividualCafs))
         Supported
  , Flag "caf-all"
         (NoArg (setDynFlag Opt_AutoSccsOnIndividualCafs))
         Supported
  , Flag "no-caf-all"
         (NoArg (unSetDynFlag Opt_AutoSccsOnIndividualCafs))
         Supported

        ------ DPH flags ----------------------------------------------------

  , Flag "fdph-seq"
         (NoArg (setDPHBackend DPHSeq))
         Supported
  , Flag "fdph-par"
         (NoArg (setDPHBackend DPHPar))
         Supported
  , Flag "fdph-this"
         (NoArg (setDPHBackend DPHThis))
         Supported

        ------ Compiler flags -----------------------------------------------

  , Flag "fasm"             (NoArg (setObjTarget HscAsm)) Supported
  , Flag "fvia-c"           (NoArg (setObjTarget HscC))
         (Deprecated "The -fvia-c flag will be removed in a future GHC release")
  , Flag "fvia-C"           (NoArg (setObjTarget HscC))
         (Deprecated "The -fvia-C flag will be removed in a future GHC release")
  , Flag "fllvm"            (NoArg (setObjTarget HscLlvm)) Supported

  , Flag "fno-code"         (NoArg (do upd $ \d -> d{ ghcLink=NoLink }
                                       setTarget HscNothing))
                                   Supported
  , Flag "fbyte-code"       (NoArg (setTarget HscInterpreted)) Supported
  , Flag "fobject-code"     (NoArg (setTarget defaultHscTarget)) Supported

  , Flag "fglasgow-exts"    (NoArg enableGlasgowExts)
         Supported
  , Flag "fno-glasgow-exts" (NoArg disableGlasgowExts)
         Supported
 ]
 ++ map (mkFlag True  "f"    setDynFlag  ) fFlags
 ++ map (mkFlag False "fno-" unSetDynFlag) fFlags
 ++ map (mkFlag True  "f"    setExtensionFlag  ) fLangFlags
 ++ map (mkFlag False "fno-" unSetExtensionFlag) fLangFlags
 ++ map (mkFlag True  "X"    setExtensionFlag  ) xFlags
 ++ map (mkFlag False "XNo"  unSetExtensionFlag) xFlags

package_flags :: [Flag DynP]
package_flags = [
        ------- Packages ----------------------------------------------------
    Flag "package-conf"   (HasArg extraPkgConf_) Supported
  , Flag "no-user-package-conf" (NoArg (unSetDynFlag Opt_ReadUserPackageConf))
         Supported
  , Flag "package-name"   (HasArg (upd . setPackageName)) Supported
  , Flag "package-id"     (HasArg exposePackageId) Supported
  , Flag "package"        (HasArg exposePackage) Supported
  , Flag "hide-package"   (HasArg hidePackage) Supported
  , Flag "hide-all-packages" (NoArg (setDynFlag Opt_HideAllPackages))
         Supported
  , Flag "ignore-package" (HasArg ignorePackage)
         Supported
  , Flag "syslib"         (HasArg exposePackage)
         (Deprecated "Use -package instead")
  ]

mkFlag :: Bool                  -- ^ True <=> it should be turned on
       -> String                -- ^ The flag prefix
       -> (flag -> DynP ())
       -> (String, flag, Bool -> Deprecated)
       -> Flag DynP
mkFlag turnOn flagPrefix f (name, flag, deprecated)
    = Flag (flagPrefix ++ name) (NoArg (f flag)) (deprecated turnOn)

deprecatedForExtension :: String -> Bool -> Deprecated
deprecatedForExtension lang turn_on
    = Deprecated ("use -X"  ++ flag ++ " or pragma {-# LANGUAGE " ++ flag ++ " #-} instead")
    where 
      flag | turn_on    = lang
           | otherwise = "No"++lang

useInstead :: String -> Bool -> Deprecated
useInstead flag turn_on
  = Deprecated ("Use -f" ++ no ++ flag ++ " instead")
  where
    no = if turn_on then "" else "no-"

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fFlags :: [(String, DynFlag, Bool -> Deprecated)]
fFlags = [
  ( "warn-dodgy-foreign-imports",       Opt_WarnDodgyForeignImports, const Supported ),
  ( "warn-dodgy-exports",               Opt_WarnDodgyExports, const Supported ),
  ( "warn-dodgy-imports",               Opt_WarnDodgyImports, const Supported ),
  ( "warn-duplicate-exports",           Opt_WarnDuplicateExports, const Supported ),
  ( "warn-hi-shadowing",                Opt_WarnHiShadows, const Supported ),
  ( "warn-implicit-prelude",            Opt_WarnImplicitPrelude, const Supported ),
  ( "warn-incomplete-patterns",         Opt_WarnIncompletePatterns, const Supported ),
  ( "warn-incomplete-record-updates",   Opt_WarnIncompletePatternsRecUpd, const Supported ),
  ( "warn-missing-fields",              Opt_WarnMissingFields, const Supported ),
  ( "warn-missing-import-lists",        Opt_WarnMissingImportList, const Supported ),
  ( "warn-missing-methods",             Opt_WarnMissingMethods, const Supported ),
  ( "warn-missing-signatures",          Opt_WarnMissingSigs, const Supported ),
  ( "warn-name-shadowing",              Opt_WarnNameShadowing, const Supported ),
  ( "warn-overlapping-patterns",        Opt_WarnOverlappingPatterns, const Supported ),
  ( "warn-simple-patterns",             Opt_WarnSimplePatterns, const Supported ),
  ( "warn-type-defaults",               Opt_WarnTypeDefaults, const Supported ),
  ( "warn-monomorphism-restriction",    Opt_WarnMonomorphism, const Supported ),
  ( "warn-unused-binds",                Opt_WarnUnusedBinds, const Supported ),
  ( "warn-unused-imports",              Opt_WarnUnusedImports, const Supported ),
  ( "warn-unused-matches",              Opt_WarnUnusedMatches, const Supported ),
  ( "warn-warnings-deprecations",       Opt_WarnWarningsDeprecations, const Supported ),
  ( "warn-deprecations",                Opt_WarnWarningsDeprecations, const Supported ),
  ( "warn-deprecated-flags",            Opt_WarnDeprecatedFlags, const Supported ),
  ( "warn-orphans",                     Opt_WarnOrphans, const Supported ),
  ( "warn-tabs",                        Opt_WarnTabs, const Supported ),
  ( "warn-unrecognised-pragmas",        Opt_WarnUnrecognisedPragmas, const Supported ),
  ( "warn-lazy-unlifted-bindings",      Opt_WarnLazyUnliftedBindings,
    const $ Deprecated "lazy unlifted bindings will be an error in GHC 6.14, and this flag will no longer exist"),
  ( "warn-unused-do-bind",              Opt_WarnUnusedDoBind, const Supported ),
  ( "warn-wrong-do-bind",               Opt_WarnWrongDoBind, const Supported ),
  ( "warn-alternative-layout-rule-transitional", Opt_WarnAlternativeLayoutRuleTransitional, const Supported ),
  ( "print-explicit-foralls",           Opt_PrintExplicitForalls, const Supported ),
  ( "strictness",                       Opt_Strictness, const Supported ),
  ( "specialise",                       Opt_Specialise, const Supported ),
  ( "float-in",                         Opt_FloatIn, const Supported ),
  ( "static-argument-transformation",   Opt_StaticArgumentTransformation, const Supported ),
  ( "full-laziness",                    Opt_FullLaziness, const Supported ),
  ( "liberate-case",                    Opt_LiberateCase, const Supported ),
  ( "spec-constr",                      Opt_SpecConstr, const Supported ),
  ( "cse",                              Opt_CSE, const Supported ),
  ( "ignore-interface-pragmas",         Opt_IgnoreInterfacePragmas, const Supported ),
  ( "omit-interface-pragmas",           Opt_OmitInterfacePragmas, const Supported ),
  ( "expose-all-unfoldings",            Opt_ExposeAllUnfoldings, const Supported ),
  ( "do-lambda-eta-expansion",          Opt_DoLambdaEtaExpansion, const Supported ),
  ( "ignore-asserts",                   Opt_IgnoreAsserts, const Supported ),
  ( "do-eta-reduction",                 Opt_DoEtaReduction, const Supported ),
  ( "case-merge",                       Opt_CaseMerge, const Supported ),
  ( "unbox-strict-fields",              Opt_UnboxStrictFields, const Supported ),
  ( "method-sharing",                   Opt_MethodSharing, const Supported ),
  ( "dicts-cheap",                      Opt_DictsCheap, const Supported ),
  ( "excess-precision",                 Opt_ExcessPrecision, const Supported ),
  ( "eager-blackholing",                Opt_EagerBlackHoling, const Supported ),
  ( "asm-mangling",                     Opt_DoAsmMangling, const Supported ),
  ( "print-bind-result",                Opt_PrintBindResult, const Supported ),
  ( "force-recomp",                     Opt_ForceRecomp, const Supported ),
  ( "hpc-no-auto",                      Opt_Hpc_No_Auto, const Supported ),
  ( "rewrite-rules",                    Opt_EnableRewriteRules, useInstead "enable-rewrite-rules" ),
  ( "enable-rewrite-rules",             Opt_EnableRewriteRules, const Supported ),
  ( "break-on-exception",               Opt_BreakOnException, const Supported ),
  ( "break-on-error",                   Opt_BreakOnError, const Supported ),
  ( "print-evld-with-show",             Opt_PrintEvldWithShow, const Supported ),
  ( "print-bind-contents",              Opt_PrintBindContents, const Supported ),
  ( "run-cps",                          Opt_RunCPS, const Supported ),
  ( "run-cpsz",                         Opt_RunCPSZ, const Supported ),
  ( "new-codegen",                      Opt_TryNewCodeGen, const Supported ),
  ( "convert-to-zipper-and-back",       Opt_ConvertToZipCfgAndBack, const Supported ),
  ( "vectorise",                        Opt_Vectorise, const Supported ),
  ( "regs-graph",                       Opt_RegsGraph, const Supported ),
  ( "regs-iterative",                   Opt_RegsIterative, const Supported ),
  ( "gen-manifest",                     Opt_GenManifest, const Supported ),
  ( "embed-manifest",                   Opt_EmbedManifest, const Supported ),
  ( "ext-core",                         Opt_EmitExternalCore, const Supported ),
  ( "shared-implib",                    Opt_SharedImplib, const Supported ),
  ( "building-cabal-package",           Opt_BuildingCabalPackage, const Supported ),
  ( "implicit-import-qualified",        Opt_ImplicitImportQualified, const Supported )
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fLangFlags :: [(String, ExtensionFlag, Bool -> Deprecated)]
fLangFlags = [
  ( "th",                               Opt_TemplateHaskell,
    deprecatedForExtension "TemplateHaskell" ),
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

supportedExtensions :: [String]
supportedExtensions = [ name' | (name, _, _) <- xFlags, name' <- [name, "No" ++ name] ]

-- This may contain duplicates
extensionOptions :: [ExtensionFlag]
extensionOptions = [ langFlag | (_, langFlag, _) <- xFlags ]

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [(String, ExtensionFlag, Bool -> Deprecated)]
xFlags = [
  ( "CPP",                              Opt_Cpp, const Supported ),
  ( "PostfixOperators",                 Opt_PostfixOperators, const Supported ),
  ( "TupleSections",                    Opt_TupleSections, const Supported ),
  ( "PatternGuards",                    Opt_PatternGuards, const Supported ),
  ( "UnicodeSyntax",                    Opt_UnicodeSyntax, const Supported ),
  ( "MagicHash",                        Opt_MagicHash, const Supported ),
  ( "PolymorphicComponents",            Opt_PolymorphicComponents, const Supported ),
  ( "ExistentialQuantification",        Opt_ExistentialQuantification, const Supported ),
  ( "KindSignatures",                   Opt_KindSignatures, const Supported ),
  ( "EmptyDataDecls",                   Opt_EmptyDataDecls, const Supported ),
  ( "ParallelListComp",                 Opt_ParallelListComp, const Supported ),
  ( "TransformListComp",                Opt_TransformListComp, const Supported ),
  ( "ForeignFunctionInterface",         Opt_ForeignFunctionInterface, const Supported ),
  ( "UnliftedFFITypes",                 Opt_UnliftedFFITypes, const Supported ),
  ( "GHCForeignImportPrim",             Opt_GHCForeignImportPrim, const Supported ),
  ( "LiberalTypeSynonyms",              Opt_LiberalTypeSynonyms, const Supported ),
  ( "Rank2Types",                       Opt_Rank2Types, const Supported ),
  ( "RankNTypes",                       Opt_RankNTypes, const Supported ),
  ( "ImpredicativeTypes",               Opt_ImpredicativeTypes, 
        const $ Deprecated "impredicative polymorphism will be simplified or removed in GHC 6.14" ),
  ( "TypeOperators",                    Opt_TypeOperators, const Supported ),
  ( "RecursiveDo",                      Opt_RecursiveDo,
    deprecatedForExtension "DoRec"),
  ( "DoRec",                            Opt_DoRec, const Supported ),
  ( "Arrows",                           Opt_Arrows, const Supported ),
  ( "PArr",                             Opt_PArr, const Supported ),
  ( "TemplateHaskell",                  Opt_TemplateHaskell, const Supported ),
  ( "QuasiQuotes",                      Opt_QuasiQuotes, const Supported ),
  ( "Generics",                         Opt_Generics, const Supported ),
  -- On by default:
  ( "ImplicitPrelude",                  Opt_ImplicitPrelude, const Supported ),
  ( "RecordWildCards",                  Opt_RecordWildCards, const Supported ),
  ( "NamedFieldPuns",                   Opt_RecordPuns, const Supported ),
  ( "RecordPuns",                       Opt_RecordPuns,
    deprecatedForExtension "NamedFieldPuns" ),
  ( "DisambiguateRecordFields",         Opt_DisambiguateRecordFields, const Supported ),
  ( "OverloadedStrings",                Opt_OverloadedStrings, const Supported ),
  ( "GADTs",                            Opt_GADTs, const Supported ),
  ( "ViewPatterns",                     Opt_ViewPatterns, const Supported ),
  ( "TypeFamilies",                     Opt_TypeFamilies, const Supported ),
  ( "BangPatterns",                     Opt_BangPatterns, const Supported ),
  -- On by default:
  ( "MonomorphismRestriction",          Opt_MonomorphismRestriction, const Supported ),
  -- On by default:
  ( "NPlusKPatterns",                   Opt_NPlusKPatterns, const Supported ),
  -- On by default (which is not strictly H98):
  ( "MonoPatBinds",                     Opt_MonoPatBinds, const Supported ),
  ( "ExplicitForAll",                   Opt_ExplicitForAll, const Supported ),
  ( "AlternativeLayoutRule",            Opt_AlternativeLayoutRule, const Supported ),
  ( "AlternativeLayoutRuleTransitional",Opt_AlternativeLayoutRuleTransitional, const Supported ),
  -- On by default:
  ( "DatatypeContexts",                 Opt_DatatypeContexts, const Supported ),
  ( "MonoLocalBinds",                   Opt_MonoLocalBinds, const Supported ),
  ( "RelaxedPolyRec",                   Opt_RelaxedPolyRec, const Supported ),
  ( "ExtendedDefaultRules",             Opt_ExtendedDefaultRules, const Supported ),
  ( "ImplicitParams",                   Opt_ImplicitParams, const Supported ),
  ( "ScopedTypeVariables",              Opt_ScopedTypeVariables, const Supported ),

  ( "PatternSignatures",                Opt_ScopedTypeVariables, 
    deprecatedForExtension "ScopedTypeVariables" ),

  ( "UnboxedTuples",                    Opt_UnboxedTuples, const Supported ),
  ( "StandaloneDeriving",               Opt_StandaloneDeriving, const Supported ),
  ( "DeriveDataTypeable",               Opt_DeriveDataTypeable, const Supported ),
  ( "DeriveFunctor",                    Opt_DeriveFunctor, const Supported ),
  ( "DeriveTraversable",                Opt_DeriveTraversable, const Supported ),
  ( "DeriveFoldable",                   Opt_DeriveFoldable, const Supported ),
  ( "TypeSynonymInstances",             Opt_TypeSynonymInstances, const Supported ),
  ( "FlexibleContexts",                 Opt_FlexibleContexts, const Supported ),
  ( "FlexibleInstances",                Opt_FlexibleInstances, const Supported ),
  ( "ConstrainedClassMethods",          Opt_ConstrainedClassMethods, const Supported ),
  ( "MultiParamTypeClasses",            Opt_MultiParamTypeClasses, const Supported ),
  ( "FunctionalDependencies",           Opt_FunctionalDependencies, const Supported ),
  ( "GeneralizedNewtypeDeriving",       Opt_GeneralizedNewtypeDeriving, const Supported ),
  ( "OverlappingInstances",             Opt_OverlappingInstances, const Supported ),
  ( "UndecidableInstances",             Opt_UndecidableInstances, const Supported ),
  ( "IncoherentInstances",              Opt_IncoherentInstances, const Supported ),
  ( "PackageImports",                   Opt_PackageImports, const Supported ),
  ( "NewQualifiedOperators",            Opt_NewQualifiedOperators,
    const $ Deprecated "The new qualified operator syntax was rejected by Haskell'" )
  ]

impliedFlags :: [(ExtensionFlag, ExtensionFlag)]
impliedFlags
  = [ (Opt_RankNTypes,                Opt_ExplicitForAll)
    , (Opt_Rank2Types,                Opt_ExplicitForAll)
    , (Opt_ScopedTypeVariables,       Opt_ExplicitForAll)
    , (Opt_LiberalTypeSynonyms,       Opt_ExplicitForAll)
    , (Opt_ExistentialQuantification, Opt_ExplicitForAll)
    , (Opt_PolymorphicComponents,     Opt_ExplicitForAll)

    , (Opt_GADTs,               Opt_RelaxedPolyRec)  -- We want type-sig variables to
                                                     --      be completely rigid for GADTs

    , (Opt_TypeFamilies,        Opt_RelaxedPolyRec)  -- Trac #2944 gives a nice example
    , (Opt_TypeFamilies,        Opt_KindSignatures)  -- Type families use kind signatures
      						     -- all over the place

    , (Opt_ScopedTypeVariables, Opt_RelaxedPolyRec)  -- Ditto for scoped type variables; see
                                                     --      Note [Scoped tyvars] in TcBinds
    , (Opt_ImpredicativeTypes,  Opt_RankNTypes)

	-- Record wild-cards implies field disambiguation
	-- Otherwise if you write (C {..}) you may well get
	-- stuff like " 'a' not in scope ", which is a bit silly
 	-- if the compiler has just filled in field 'a' of constructor 'C'
    , (Opt_RecordWildCards,     Opt_DisambiguateRecordFields)
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
           , Opt_GADTs
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
           , Opt_GeneralizedNewtypeDeriving
           , Opt_TypeFamilies ]

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
  let (pic_warns, dflags2) =
        if opt_PIC && hscTarget dflags1 == HscC && cGhcUnregisterised == "NO"
          then ([L noSrcSpan $ "Warning: -fvia-C is incompatible with -fPIC; ignoring -fvia-C"],
                dflags1{ hscTarget = HscAsm })
          else ([], dflags1)

  return (dflags2, leftover, pic_warns ++ warns)

type DynP = CmdLineP DynFlags

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = do
   dfs <- getCmdLineState
   putCmdLineState $! (f dfs)

--------------------------
setDynFlag, unSetDynFlag :: DynFlag -> DynP ()
setDynFlag f = upd (\dfs -> dopt_set dfs f)
unSetDynFlag f = upd (\dfs -> dopt_unset dfs f)

--------------------------
setExtensionFlag, unSetExtensionFlag :: ExtensionFlag -> DynP ()
setExtensionFlag f = do { upd (\dfs -> lopt_set dfs f)
                        ; mapM_ setExtensionFlag deps }
  where
    deps = [ d | (f', d) <- impliedFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags
        -- When you un-set f, however, we don't un-set the things it implies
        --      (except for -fno-glasgow-exts, which is treated specially)

unSetExtensionFlag f = upd (\dfs -> lopt_unset dfs f)

--------------------------
setDumpFlag :: DynFlag -> OptKind DynP
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

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
forceRecompile = do { dfs <- getCmdLineState
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
machdepCCOpts _dflags
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
#if darwin_TARGET_OS
                      -- By default, gcc on OS X will generate SSE
                      -- instructions, which need things 16-byte aligned,
                      -- but we don't 16-byte align things. Thus drop
                      -- back to generic i686 compatibility. Trac #2983.
                      --
                      -- Since Snow Leopard (10.6), gcc defaults to x86_64.
                      ["-march=i686", "-m32"],
#else
                      [ if opt_Static then "-DDONT_WANT_WIN32_DLL_SUPPORT" else ""
                      ],
#endif
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
#if darwin_TARGET_OS
            ["-m64"],
#else
            [],
#endif
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
                ("Support SMP",                 String cGhcWithSMP),
                ("Unregisterised",              String cGhcUnregisterised),
                ("Tables next to code",         String cGhcEnableTablesNextToCode),
                ("RTS ways",                    String cGhcRTSWays),
                ("Leading underscore",          String cLeadingUnderscore),
                ("Debug on",                    String (show debugIsOn)),
                ("LibDir",                      FromDynFlags topDir),
                ("Global Package DB",           FromDynFlags systemPackageConfig)
               ]

