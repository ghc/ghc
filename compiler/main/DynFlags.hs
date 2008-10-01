
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
        DynFlags(..),
        HscTarget(..), isObjectTarget, defaultObjectTarget,
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..),
        Option(..),
        DynLibLoader(..),
        fFlags, xFlags,
        dphPackage,

        -- ** Manipulating DynFlags
        defaultDynFlags,                -- DynFlags
        initDynFlags,                   -- DynFlags -> IO DynFlags

        dopt,                           -- DynFlag -> DynFlags -> Bool
        dopt_set, dopt_unset,           -- DynFlags -> DynFlag -> DynFlags
        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlag,
        getMainFun,
        updOptLevel,
        setTmpDir,
        setPackageName,

        -- ** Parsing DynFlags
        parseDynamicFlags,
        parseDynamicNoPackageFlags,
        allFlags,

        supportedLanguages, languageOptions,

        -- ** DynFlag C compiler options
        machdepCCOpts, picCCOpts,

        -- * Configuration of the core-to-core passes
        CoreToDo(..),
        SimplifierMode(..),
        SimplifierSwitch(..),
        FloatOutSwitches(..),
        getCoreToDo,

        -- * Configuration of the stg-to-stg passes
        StgToDo(..),
        getStgToDo,

        -- * Compiler configuration suitable for display to the user
        compilerInfo
  ) where

#include "HsVersions.h"

import Module
import PackageConfig
import PrelNames        ( mAIN, main_RDR_Unqual )
import RdrName          ( RdrName, mkRdrUnqual )
import OccName          ( mkVarOccFS )
#ifdef i386_TARGET_ARCH
import StaticFlags      ( opt_Static )
#endif
import StaticFlags      ( opt_PIC, WayName(..), v_Ways, v_Build_tag,
                          v_RTS_Build_tag )
import {-# SOURCE #-} Packages (PackageState)
import DriverPhases     ( Phase(..), phaseInputExt )
import Config
import CmdLineParser
import Constants        ( mAX_CONTEXT_REDUCTION_DEPTH )
import Panic
import UniqFM           ( UniqFM )
import Util
import Maybes           ( orElse )
import SrcLoc
import FastString
import Outputable
import {-# SOURCE #-} ErrUtils ( Severity(..), Message, mkLocMessage )

import Data.IORef       ( readIORef )
import Control.Monad    ( when )

import Data.Char
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
   | Opt_WarnDodgyImports
   | Opt_WarnOrphans
   | Opt_WarnTabs
   | Opt_WarnUnrecognisedPragmas
   | Opt_WarnDodgyForeignImports

   -- language opts
   | Opt_OverlappingInstances
   | Opt_UndecidableInstances
   | Opt_IncoherentInstances
   | Opt_MonomorphismRestriction
   | Opt_MonoPatBinds
   | Opt_ExtendedDefaultRules           -- Use GHC's extended rules for defaulting
   | Opt_ForeignFunctionInterface
   | Opt_UnliftedFFITypes
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
   | Opt_StandaloneDeriving
   | Opt_DeriveDataTypeable
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
   | Opt_PostfixOperators
   | Opt_PatternGuards
   | Opt_LiberalTypeSynonyms
   | Opt_Rank2Types
   | Opt_RankNTypes
   | Opt_ImpredicativeTypes
   | Opt_TypeOperators
   | Opt_PackageImports
   | Opt_NewQualifiedOperators

   | Opt_PrintExplicitForalls

   -- optimisation opts
   | Opt_Strictness
   | Opt_FullLaziness
   | Opt_StaticArgumentTransformation
   | Opt_CSE
   | Opt_LiberateCase
   | Opt_SpecConstr
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_DoLambdaEtaExpansion
   | Opt_IgnoreAsserts
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_UnboxStrictFields
   | Opt_MethodSharing
   | Opt_DictsCheap
   | Opt_InlineIfEnoughArgs
   | Opt_EnableRewriteRules		-- Apply rewrite rules during simplification
   | Opt_Vectorise
   | Opt_RegsGraph                      -- do graph coloring register allocation
   | Opt_RegsIterative                  -- do iterative coalescing graph coloring register allocation

   -- misc opts
   | Opt_Cpp
   | Opt_Pp
   | Opt_ForceRecomp
   | Opt_DryRun
   | Opt_DoAsmMangling
   | Opt_ExcessPrecision
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
   | Opt_RunCPSZ
   | Opt_ConvertToZipCfgAndBack
   | Opt_AutoLinkPackages
   | Opt_ImplicitImportQualified

   -- keeping stuff
   | Opt_KeepHiDiffs
   | Opt_KeepHcFiles
   | Opt_KeepSFiles
   | Opt_KeepRawSFiles
   | Opt_KeepTmpFiles
   | Opt_KeepRawTokenStream

   deriving (Eq, Show)

-- | Contains not only a collection of 'DynFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  ghcMode               :: GhcMode,
  ghcLink               :: GhcLink,
  coreToDo              :: Maybe [CoreToDo], -- reserved for -Ofile
  stgToDo               :: Maybe [StgToDo],  -- similarly
  hscTarget             :: HscTarget,
  hscOutName            :: String,      -- ^ Name of the output file
  extCoreName           :: String,      -- ^ Name of the .hcr output file
  verbosity             :: Int,         -- ^ Verbosity level: see "DynFlags#verbosity_levels"
  optLevel              :: Int,         -- ^ Optimisation level
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  shouldDumpSimplPhase  :: SimplifierMode -> Bool,
  ruleCheck             :: Maybe String,

  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  liberateCaseThreshold :: Maybe Int,   -- ^ Threshold for LiberateCase

  stolen_x86_regs       :: Int,
  cmdlineHcIncludes     :: [String],    -- ^ @\-\#includes@
  importPaths           :: [FilePath],
  mainModIs             :: Module,
  mainFunIs             :: Maybe String,
  ctxtStkDepth          :: Int,         -- ^ Typechecker context stack depth

  dphBackend            :: DPHBackend,

  thisPackage           :: PackageId,   -- ^ name of package currently being compiled

  -- ways
  wayNames              :: [WayName],   -- ^ Way flags from the command line
  buildTag              :: String,      -- ^ The global \"way\" (e.g. \"p\" for prof)
  rtsBuildTag           :: String,      -- ^ The RTS \"way\"

  -- paths etc.
  objectDir             :: Maybe String,
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

  --  For ghc -M
  depMakefile           :: FilePath,
  depIncludePkgDeps     :: Bool,
  depExcludeMods        :: [ModuleName],
  depSuffixes           :: [String],
  depWarnings           :: Bool,

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
  pkgDatabase           :: Maybe (UniqFM PackageConfig),
  pkgState              :: PackageState,

  -- hsc dynamic flags
  flags                 :: [DynFlag],

  -- | Message output action: use "ErrUtils" instead of this if you can
  log_action            :: Severity -> SrcSpan -> PprStyle -> Message -> IO (),

  haddockOptions :: Maybe String
 }

data HscTarget
  = HscC
  | HscAsm
  | HscJava
  | HscInterpreted
  | HscNothing
  deriving (Eq, Show)

-- | Will this target result in an object file on the disk?
isObjectTarget :: HscTarget -> Bool
isObjectTarget HscC     = True
isObjectTarget HscAsm   = True
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
  | LinkInMemory        -- ^ Use the in-memory dynamic linker
  | LinkDynLib          -- ^ Link objects into a dynamic lib (DLL on Windows, DSO on ELF platforms)
  deriving (Eq, Show)

isNoLink :: GhcLink -> Bool
isNoLink NoLink = True
isNoLink _      = False

data PackageFlag
  = ExposePackage  String
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
 build_tag <- readIORef v_Build_tag
 rts_build_tag <- readIORef v_RTS_Build_tag
 return dflags{
        wayNames        = ways,
        buildTag        = build_tag,
        rtsBuildTag     = rts_build_tag
        }

-- | The normal 'DynFlags'. Note that they is not suitable for use in this form
-- and must be fully initialized by 'GHC.newSession' first.
defaultDynFlags :: DynFlags
defaultDynFlags =
     DynFlags {
        ghcMode                 = CompManager,
        ghcLink                 = LinkBinary,
        coreToDo                = Nothing,
        stgToDo                 = Nothing,
        hscTarget               = defaultHscTarget,
        hscOutName              = "",
        extCoreName             = "",
        verbosity               = 0,
        optLevel                = 0,
        simplPhases             = 2,
        maxSimplIterations      = 4,
        shouldDumpSimplPhase    = const False,
        ruleCheck               = Nothing,
        specConstrThreshold     = Just 200,
        specConstrCount         = Just 3,
        liberateCaseThreshold   = Just 200,
        stolen_x86_regs         = 4,
        cmdlineHcIncludes       = [],
        importPaths             = ["."],
        mainModIs               = mAIN,
        mainFunIs               = Nothing,
        ctxtStkDepth            = mAX_CONTEXT_REDUCTION_DEPTH,

        dphBackend              = DPHPar,

        thisPackage             = mainPackageId,

        objectDir               = Nothing,
        hiDir                   = Nothing,
        stubDir                 = Nothing,

        objectSuf               = phaseInputExt StopLn,
        hcSuf                   = phaseInputExt HCc,
        hiSuf                   = "hi",

        outputFile              = Nothing,
        outputHi                = Nothing,
        dynLibLoader            = Deployable,
        dumpPrefix              = Nothing,
        dumpPrefixForce         = Nothing,
        includePaths            = [],
        libraryPaths            = [],
        frameworkPaths          = [],
        cmdlineFrameworks       = [],
        tmpDir                  = cDEFAULT_TMPDIR,

        hpcDir                  = ".hpc",

        opt_L                   = [],
        opt_P                   = (if opt_PIC
                                   then ["-D__PIC__"]
                                   else []),
        opt_F                   = [],
        opt_c                   = [],
        opt_a                   = [],
        opt_m                   = [],
        opt_l                   = [],
        opt_windres             = [],

        extraPkgConfs           = [],
        packageFlags            = [],
        pkgDatabase             = Nothing,
        pkgState                = panic "no package state yet: call GHC.setSessionDynFlags",
        wayNames                = panic "defaultDynFlags: No wayNames",
        buildTag                = panic "defaultDynFlags: No buildTag",
        rtsBuildTag             = panic "defaultDynFlags: No rtsBuildTag",
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
        -- end of initSysTools values
        -- ghc -M values
        depMakefile       = "Makefile",
        depIncludePkgDeps = False,
        depExcludeMods    = [],
        depSuffixes       = [],
        depWarnings       = True,
        -- end of ghc -M values
        haddockOptions = Nothing,
        flags = [
            Opt_AutoLinkPackages,
            Opt_ReadUserPackageConf,

            Opt_MonoPatBinds,   -- Experimentally, I'm making this non-standard
                                -- behaviour the default, to see if anyone notices
                                -- SLPJ July 06

            Opt_ImplicitPrelude,
            Opt_MonomorphismRestriction,

            Opt_MethodSharing,

            Opt_DoAsmMangling,

            Opt_GenManifest,
            Opt_EmbedManifest,
            Opt_PrintBindContents
            ]
            ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
                    -- The default -O0 options
            ++ standardWarnings,

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
    #verbosity_levels#
    Verbosity levels:

    0   |   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"
-}

-- | Test whether a 'DynFlag' is set
dopt :: DynFlag -> DynFlags -> Bool
dopt f dflags  = f `elem` (flags dflags)

-- | Set a 'DynFlag'
dopt_set :: DynFlags -> DynFlag -> DynFlags
dopt_set dfs f = dfs{ flags = f : flags dfs }

-- | Unset a 'DynFlag'
dopt_unset :: DynFlags -> DynFlag -> DynFlags
dopt_unset dfs f = dfs{ flags = filter (/= f) (flags dfs) }

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

setObjectDir, setHiDir, setStubDir, setOutputDir,
         setObjectSuf, setHiSuf, setHcSuf, parseDynLibLoaderMode,
         setPgmP, setPgmL, setPgmF, setPgmc, setPgmm, setPgms, setPgma, setPgml, setPgmdll, setPgmwindres,
         addOptL, addOptP, addOptF, addOptc, addOptm, addOpta, addOptl, addOptwindres,
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

addOptL   f d = d{ opt_L   = f : opt_L d}
addOptP   f d = d{ opt_P   = f : opt_P d}
addOptF   f d = d{ opt_F   = f : opt_F d}
addOptc   f d = d{ opt_c   = f : opt_c d}
addOptm   f d = d{ opt_m   = f : opt_m d}
addOpta   f d = d{ opt_a   = f : opt_a d}
addOptl   f d = d{ opt_l   = f : opt_l d}
addOptwindres f d = d{ opt_windres = f : opt_windres d}

setDepMakefile :: FilePath -> DynFlags -> DynFlags
setDepMakefile f d = d { depMakefile = deOptDep f }

setDepIncludePkgDeps :: Bool -> DynFlags -> DynFlags
setDepIncludePkgDeps b d = d { depIncludePkgDeps = b }

addDepExcludeMod :: String -> DynFlags -> DynFlags
addDepExcludeMod m d
    = d { depExcludeMods = mkModuleName (deOptDep m) : depExcludeMods d }

addDepSuffix :: FilePath -> DynFlags -> DynFlags
addDepSuffix s d = d { depSuffixes = deOptDep s : depSuffixes d }

setDepWarnings :: Bool -> DynFlags -> DynFlags
setDepWarnings b d = d { depWarnings = b }

-- XXX Legacy code:
-- We used to use "-optdep-flag -optdeparg", so for legacy applications
-- we need to strip the "-optdep" off of the arg
deOptDep :: String -> String
deOptDep x = case maybePrefixMatch "-optdep" x of
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
        Opt_WarnDodgyForeignImports
      ]

minusWOpts :: [DynFlag]
minusWOpts
    = standardWarnings ++
      [ Opt_WarnUnusedBinds,
        Opt_WarnUnusedMatches,
        Opt_WarnUnusedImports,
        Opt_WarnIncompletePatterns,
        Opt_WarnDodgyImports
      ]

minusWallOpts :: [DynFlag]
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSigs,
        Opt_WarnHiShadows,
        Opt_WarnOrphans
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
-- CoreToDo:  abstraction of core-to-core passes to run.

data CoreToDo           -- These are diff core-to-core passes,
                        -- which may be invoked in any order,
                        -- as many times as you like.

  = CoreDoSimplify      -- The core-to-core simplifier.
        SimplifierMode
        [SimplifierSwitch]
                        -- Each run of the simplifier can take a different
                        -- set of simplifier-specific flags.
  | CoreDoFloatInwards
  | CoreDoFloatOutwards FloatOutSwitches
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoStrictness
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoSpecConstr
  | CoreDoOldStrictness
  | CoreDoGlomBinds
  | CoreCSE
  | CoreDoRuleCheck Int{-CompilerPhase-} String -- Check for non-application of rules
                                                -- matching this string
  | CoreDoVectorisation PackageId
  | CoreDoNothing                -- Useful when building up
  | CoreDoPasses [CoreToDo]      -- lists of these things

data SimplifierMode             -- See comments in SimplMonad
  = SimplGently
  | SimplPhase Int [String]

data SimplifierSwitch
  = MaxSimplifierIterations Int
  | NoCaseOfCase

data FloatOutSwitches
  = FloatOutSw  Bool    -- True <=> float lambdas to top level
                Bool    -- True <=> float constants to top level,
                        --          even if they do not escape a lambda


-- The core-to-core pass ordering is derived from the DynFlags:
runWhen :: Bool -> CoreToDo -> CoreToDo
runWhen True  do_this = do_this
runWhen False _       = CoreDoNothing

runMaybe :: Maybe a -> (a -> CoreToDo) -> CoreToDo
runMaybe (Just x) f = f x
runMaybe Nothing  _ = CoreDoNothing

getCoreToDo :: DynFlags -> [CoreToDo]
getCoreToDo dflags
  | Just todo <- coreToDo dflags = todo -- set explicitly by user
  | otherwise = core_todo
  where
    opt_level     = optLevel dflags
    phases        = simplPhases dflags
    max_iter      = maxSimplIterations dflags
    strictness    = dopt Opt_Strictness dflags
    full_laziness = dopt Opt_FullLaziness dflags
    cse           = dopt Opt_CSE dflags
    spec_constr   = dopt Opt_SpecConstr dflags
    liberate_case = dopt Opt_LiberateCase dflags
    rule_check    = ruleCheck dflags
    static_args   = dopt Opt_StaticArgumentTransformation dflags

    maybe_rule_check phase = runMaybe rule_check (CoreDoRuleCheck phase)

    simpl_phase phase names iter
      = CoreDoPasses
          [ CoreDoSimplify (SimplPhase phase names) [
              MaxSimplifierIterations iter
            ],
            maybe_rule_check phase
          ]

    vectorisation
      = runWhen (dopt Opt_Vectorise dflags)
        $ CoreDoPasses [ simpl_gently, CoreDoVectorisation (dphPackage dflags) ]


                -- By default, we have 2 phases before phase 0.

                -- Want to run with inline phase 2 after the specialiser to give
                -- maximum chance for fusion to work before we inline build/augment
                -- in phase 1.  This made a difference in 'ansi' where an
                -- overloaded function wasn't inlined till too late.

                -- Need phase 1 so that build/augment get
                -- inlined.  I found that spectral/hartel/genfft lost some useful
                -- strictness in the function sumcode' if augment is not inlined
                -- before strictness analysis runs
    simpl_phases = CoreDoPasses [ simpl_phase phase ["main"] max_iter
                                  | phase <- [phases, phases-1 .. 1] ]


        -- initial simplify: mk specialiser happy: minimum effort please
    simpl_gently = CoreDoSimplify SimplGently [
                        --      Simplify "gently"
                        -- Don't inline anything till full laziness has bitten
                        -- In particular, inlining wrappers inhibits floating
                        -- e.g. ...(case f x of ...)...
                        --  ==> ...(case (case x of I# x# -> fw x#) of ...)...
                        --  ==> ...(case x of I# x# -> case fw x# of ...)...
                        -- and now the redex (f x) isn't floatable any more
                        -- Similarly, don't apply any rules until after full
                        -- laziness.  Notably, list fusion can prevent floating.

            NoCaseOfCase,       -- Don't do case-of-case transformations.
                                -- This makes full laziness work better
            MaxSimplifierIterations max_iter
        ]

    core_todo =
     if opt_level == 0 then
       [vectorisation,
        simpl_phase 0 ["final"] max_iter]
     else {- opt_level >= 1 -} [

    -- We want to do the static argument transform before full laziness as it
    -- may expose extra opportunities to float things outwards. However, to fix
    -- up the output of the transformation we need at do at least one simplify
    -- after this before anything else
        runWhen static_args (CoreDoPasses [ simpl_gently, CoreDoStaticArgs ]),

        -- We run vectorisation here for now, but we might also try to run
        -- it later
        vectorisation,

        -- initial simplify: mk specialiser happy: minimum effort please
        simpl_gently,

        -- Specialisation is best done before full laziness
        -- so that overloaded functions have all their dictionary lambdas manifest
        CoreDoSpecialising,

        runWhen full_laziness (CoreDoFloatOutwards (FloatOutSw False False)),

        CoreDoFloatInwards,

        simpl_phases,

                -- Phase 0: allow all Ids to be inlined now
                -- This gets foldr inlined before strictness analysis

                -- At least 3 iterations because otherwise we land up with
                -- huge dead expressions because of an infelicity in the
                -- simpifier.
                --      let k = BIG in foldr k z xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
                -- Don't stop now!
        simpl_phase 0 ["main"] (max max_iter 3),


#ifdef OLD_STRICTNESS
        CoreDoOldStrictness,
#endif
        runWhen strictness (CoreDoPasses [
                CoreDoStrictness,
                CoreDoWorkerWrapper,
                CoreDoGlomBinds,
                simpl_phase 0 ["post-worker-wrapper"] max_iter
                ]),

        runWhen full_laziness
          (CoreDoFloatOutwards (FloatOutSw False    -- Not lambdas
                                           True)),  -- Float constants
                -- nofib/spectral/hartel/wang doubles in speed if you
                -- do full laziness late in the day.  It only happens
                -- after fusion and other stuff, so the early pass doesn't
                -- catch it.  For the record, the redex is
                --        f_el22 (f_el21 r_midblock)


        runWhen cse CoreCSE,
                -- We want CSE to follow the final full-laziness pass, because it may
                -- succeed in commoning up things floated out by full laziness.
                -- CSE used to rely on the no-shadowing invariant, but it doesn't any more

        CoreDoFloatInwards,

        maybe_rule_check 0,

                -- Case-liberation for -O2.  This should be after
                -- strictness analysis and the simplification which follows it.
        runWhen liberate_case (CoreDoPasses [
            CoreLiberateCase,
            simpl_phase 0 ["post-liberate-case"] max_iter
            ]),         -- Run the simplifier after LiberateCase to vastly
                        -- reduce the possiblility of shadowing
                        -- Reason: see Note [Shadowing] in SpecConstr.lhs

        runWhen spec_constr CoreDoSpecConstr,

        maybe_rule_check 0,

        -- Final clean-up simplification:
        simpl_phase 0 ["final"] max_iter
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
  | Just todo <- stgToDo dflags = todo -- set explicitly by user
  | otherwise = todo2
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
           map ("X"++) supportedLanguages ++
           map ("XNo"++) supportedLanguages
    where ok (PrefixPred _ _) = False
          ok _ = True
          flags = [ name | (name, _, _) <- fFlags ]

dynamic_flags :: [Flag DynP]
dynamic_flags = [
    Flag "n"              (NoArg  (setDynFlag Opt_DryRun)) Supported
  , Flag "cpp"            (NoArg  (setDynFlag Opt_Cpp)) Supported
  , Flag "F"              (NoArg  (setDynFlag Opt_Pp)) Supported
  , Flag "#include"       (HasArg (addCmdlineHCInclude)) Supported
  , Flag "v"              (OptIntSuffix setVerbosity) Supported

        ------- Specific phases  --------------------------------------------
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
  , Flag "optdep-w"                 (NoArg  (upd (setDepWarnings False)))
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
  , Flag "c"              (NoArg (upd $ \d -> d{ ghcLink=NoLink } ))
         Supported
  , Flag "no-link"        (NoArg (upd $ \d -> d{ ghcLink=NoLink } ))
         (Deprecated "Use -c instead")
  , Flag "shared"         (NoArg (upd $ \d -> d{ ghcLink=LinkDynLib } ))
         Supported
  , Flag "dynload"        (HasArg (upd . parseDynLibLoaderMode))
         Supported

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
     -- This only makes sense as plural
  , Flag "keep-tmp-files"   (NoArg (setDynFlag Opt_KeepTmpFiles)) Supported

        ------- Miscellaneous ----------------------------------------------
  , Flag "no-auto-link-packages" (NoArg (unSetDynFlag Opt_AutoLinkPackages)) Supported
  , Flag "no-hs-main"     (NoArg (setDynFlag Opt_NoHsMain)) Supported
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
  , Flag "dverbose-core2core"      (NoArg setVerboseCore2Core)
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
  , Flag "fvia-c"           (NoArg (setObjTarget HscC)) Supported
  , Flag "fvia-C"           (NoArg (setObjTarget HscC)) Supported

  , Flag "fno-code"         (NoArg (setTarget HscNothing)) Supported
  , Flag "fbyte-code"       (NoArg (setTarget HscInterpreted)) Supported
  , Flag "fobject-code"     (NoArg (setTarget defaultHscTarget)) Supported

  , Flag "fglasgow-exts"    (NoArg (mapM_ setDynFlag   glasgowExtsFlags))
         Supported
  , Flag "fno-glasgow-exts" (NoArg (mapM_ unSetDynFlag glasgowExtsFlags))
         Supported
 ]
 ++ map (mkFlag True  "f"    setDynFlag  ) fFlags
 ++ map (mkFlag False "fno-" unSetDynFlag) fFlags
 ++ map (mkFlag True  "X"    setDynFlag  ) xFlags
 ++ map (mkFlag False "XNo"  unSetDynFlag) xFlags

package_flags :: [Flag DynP]
package_flags = [
        ------- Packages ----------------------------------------------------
    Flag "package-conf"   (HasArg extraPkgConf_) Supported
  , Flag "no-user-package-conf" (NoArg (unSetDynFlag Opt_ReadUserPackageConf))
         Supported
  , Flag "package-name"   (HasArg (upd . setPackageName)) Supported
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
       -> (DynFlag -> DynP ())
       -> (String, DynFlag, Bool -> Deprecated)
       -> Flag DynP
mkFlag turnOn flagPrefix f (name, dynflag, deprecated)
    = Flag (flagPrefix ++ name) (NoArg (f dynflag)) (deprecated turnOn)

deprecatedForLanguage :: String -> Bool -> Deprecated
deprecatedForLanguage lang turn_on
    = Deprecated ("use -X"  ++ flag ++ " or pragma {-# LANGUAGE " ++ flag ++ "#-} instead")
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
  ( "warn-dodgy-imports",               Opt_WarnDodgyImports, const Supported ),
  ( "warn-duplicate-exports",           Opt_WarnDuplicateExports, const Supported ),
  ( "warn-hi-shadowing",                Opt_WarnHiShadows, const Supported ),
  ( "warn-implicit-prelude",            Opt_WarnImplicitPrelude, const Supported ),
  ( "warn-incomplete-patterns",         Opt_WarnIncompletePatterns, const Supported ),
  ( "warn-incomplete-record-updates",   Opt_WarnIncompletePatternsRecUpd, const Supported ),
  ( "warn-missing-fields",              Opt_WarnMissingFields, const Supported ),
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
  ( "print-explicit-foralls",           Opt_PrintExplicitForalls, const Supported ),
  ( "strictness",                       Opt_Strictness, const Supported ),
  ( "static-argument-transformation",   Opt_StaticArgumentTransformation, const Supported ),
  ( "full-laziness",                    Opt_FullLaziness, const Supported ),
  ( "liberate-case",                    Opt_LiberateCase, const Supported ),
  ( "spec-constr",                      Opt_SpecConstr, const Supported ),
  ( "cse",                              Opt_CSE, const Supported ),
  ( "ignore-interface-pragmas",         Opt_IgnoreInterfacePragmas, const Supported ),
  ( "omit-interface-pragmas",           Opt_OmitInterfacePragmas, const Supported ),
  ( "do-lambda-eta-expansion",          Opt_DoLambdaEtaExpansion, const Supported ),
  ( "ignore-asserts",                   Opt_IgnoreAsserts, const Supported ),
  ( "do-eta-reduction",                 Opt_DoEtaReduction, const Supported ),
  ( "case-merge",                       Opt_CaseMerge, const Supported ),
  ( "unbox-strict-fields",              Opt_UnboxStrictFields, const Supported ),
  ( "method-sharing",                   Opt_MethodSharing, const Supported ),
  ( "dicts-cheap",                      Opt_DictsCheap, const Supported ),
  ( "inline-if-enough-args",            Opt_InlineIfEnoughArgs, const Supported ),
  ( "excess-precision",                 Opt_ExcessPrecision, const Supported ),
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
  ( "run-cps",                          Opt_RunCPSZ, const Supported ),
  ( "convert-to-zipper-and-back",       Opt_ConvertToZipCfgAndBack, const Supported ),
  ( "vectorise",                        Opt_Vectorise, const Supported ),
  ( "regs-graph",                       Opt_RegsGraph, const Supported ),
  ( "regs-iterative",                   Opt_RegsIterative, const Supported ),
  ( "th",                               Opt_TemplateHaskell,
    deprecatedForLanguage "TemplateHaskell" ),
  ( "fi",                               Opt_ForeignFunctionInterface,
    deprecatedForLanguage "ForeignFunctionInterface" ),
  ( "ffi",                              Opt_ForeignFunctionInterface,
    deprecatedForLanguage "ForeignFunctionInterface" ),
  ( "arrows",                           Opt_Arrows,
    deprecatedForLanguage "Arrows" ),
  ( "generics",                         Opt_Generics,
    deprecatedForLanguage "Generics" ),
  ( "implicit-prelude",                 Opt_ImplicitPrelude,
    deprecatedForLanguage "ImplicitPrelude" ),
  ( "bang-patterns",                    Opt_BangPatterns,
    deprecatedForLanguage "BangPatterns" ),
  ( "monomorphism-restriction",         Opt_MonomorphismRestriction,
    deprecatedForLanguage "MonomorphismRestriction" ),
  ( "mono-pat-binds",                   Opt_MonoPatBinds,
    deprecatedForLanguage "MonoPatBinds" ),
  ( "extended-default-rules",           Opt_ExtendedDefaultRules,
    deprecatedForLanguage "ExtendedDefaultRules" ),
  ( "implicit-params",                  Opt_ImplicitParams,
    deprecatedForLanguage "ImplicitParams" ),
  ( "scoped-type-variables",            Opt_ScopedTypeVariables,
    deprecatedForLanguage "ScopedTypeVariables" ),
  ( "parr",                             Opt_PArr,
    deprecatedForLanguage "PArr" ),
  ( "allow-overlapping-instances",      Opt_OverlappingInstances,
    deprecatedForLanguage "OverlappingInstances" ),
  ( "allow-undecidable-instances",      Opt_UndecidableInstances,
    deprecatedForLanguage "UndecidableInstances" ),
  ( "allow-incoherent-instances",       Opt_IncoherentInstances,
    deprecatedForLanguage "IncoherentInstances" ),
  ( "gen-manifest",                     Opt_GenManifest, const Supported ),
  ( "embed-manifest",                   Opt_EmbedManifest, const Supported ),
  ( "implicit-import-qualified",        Opt_ImplicitImportQualified, const Supported )
  ]

supportedLanguages :: [String]
supportedLanguages = [ name | (name, _, _) <- xFlags ]

-- This may contain duplicates
languageOptions :: [DynFlag]
languageOptions = [ dynFlag | (_, dynFlag, _) <- xFlags ]

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [(String, DynFlag, Bool -> Deprecated)]
xFlags = [
  ( "CPP",                              Opt_Cpp, const Supported ),
  ( "PostfixOperators",                 Opt_PostfixOperators, const Supported ),
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
  ( "LiberalTypeSynonyms",              Opt_LiberalTypeSynonyms, const Supported ),
  ( "Rank2Types",                       Opt_Rank2Types, const Supported ),
  ( "RankNTypes",                       Opt_RankNTypes, const Supported ),
  ( "ImpredicativeTypes",               Opt_ImpredicativeTypes, const Supported ),
  ( "TypeOperators",                    Opt_TypeOperators, const Supported ),
  ( "RecursiveDo",                      Opt_RecursiveDo, const Supported ),
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
    deprecatedForLanguage "NamedFieldPuns" ),
  ( "DisambiguateRecordFields",         Opt_DisambiguateRecordFields, const Supported ),
  ( "OverloadedStrings",                Opt_OverloadedStrings, const Supported ),
  ( "GADTs",                            Opt_GADTs, const Supported ),
  ( "ViewPatterns",                     Opt_ViewPatterns, const Supported ),
  ( "TypeFamilies",                     Opt_TypeFamilies, const Supported ),
  ( "BangPatterns",                     Opt_BangPatterns, const Supported ),
  -- On by default:
  ( "MonomorphismRestriction",          Opt_MonomorphismRestriction, const Supported ),
  -- On by default (which is not strictly H98):
  ( "MonoPatBinds",                     Opt_MonoPatBinds, const Supported ),
  ( "RelaxedPolyRec",                   Opt_RelaxedPolyRec, const Supported ),
  ( "ExtendedDefaultRules",             Opt_ExtendedDefaultRules, const Supported ),
  ( "ImplicitParams",                   Opt_ImplicitParams, const Supported ),
  ( "ScopedTypeVariables",              Opt_ScopedTypeVariables, const Supported ),

  ( "PatternSignatures",                Opt_ScopedTypeVariables, 
    deprecatedForLanguage "ScopedTypeVariables" ),

  ( "UnboxedTuples",                    Opt_UnboxedTuples, const Supported ),
  ( "StandaloneDeriving",               Opt_StandaloneDeriving, const Supported ),
  ( "DeriveDataTypeable",               Opt_DeriveDataTypeable, const Supported ),
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
  ( "NewQualifiedOperators",            Opt_NewQualifiedOperators, const Supported )
  ]

impliedFlags :: [(DynFlag, DynFlag)]
impliedFlags
  = [ (Opt_GADTs,               Opt_RelaxedPolyRec)  -- We want type-sig variables to
                                                     --      be completely rigid for GADTs

    , (Opt_ScopedTypeVariables, Opt_RelaxedPolyRec)  -- Ditto for scoped type variables; see
                                                     --      Note [Scoped tyvars] in TcBinds
  ]

glasgowExtsFlags :: [DynFlag]
glasgowExtsFlags = [
             Opt_PrintExplicitForalls
           , Opt_ForeignFunctionInterface
           , Opt_UnliftedFFITypes
           , Opt_GADTs
           , Opt_ImplicitParams
           , Opt_ScopedTypeVariables
           , Opt_UnboxedTuples
           , Opt_TypeSynonymInstances
           , Opt_StandaloneDeriving
           , Opt_DeriveDataTypeable
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
           , Opt_ImpredicativeTypes
           , Opt_TypeOperators
           , Opt_RecursiveDo
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
parseDynamicFlags_ dflags args pkg_flags = do
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

  let ((leftover, errs, warns), dflags')
          = runCmdLine (processArgs flag_spec args') dflags
  when (not (null errs)) $ ghcError $ errorsToGhcException errs
  return (dflags', leftover, warns)

type DynP = CmdLineP DynFlags

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = do
   dfs <- getCmdLineState
   putCmdLineState $! (f dfs)

--------------------------
setDynFlag, unSetDynFlag :: DynFlag -> DynP ()
setDynFlag f = do { upd (\dfs -> dopt_set dfs f)
		  ; mapM_ setDynFlag deps }
  where
    deps = [ d | (f', d) <- impliedFlags, f' == f ]
        -- When you set f, set the ones it implies
	-- NB: use setDynFlag recursively, in case the implied flags
	--     implies further flags
        -- When you un-set f, however, we don't un-set the things it implies
        --      (except for -fno-glasgow-exts, which is treated specially)

unSetDynFlag f = upd (\dfs -> dopt_unset dfs f)

--------------------------
setDumpFlag :: DynFlag -> OptKind DynP
setDumpFlag dump_flag
  = NoArg (setDynFlag dump_flag >> when want_recomp forceRecompile)
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
setVerboseCore2Core = do setDynFlag Opt_D_verbose_core2core 
		         forceRecompile
                         upd (\s -> s { shouldDumpSimplPhase = const True })

setDumpSimplPhases :: String -> DynP ()
setDumpSimplPhases s = do forceRecompile
                          upd (\s -> s { shouldDumpSimplPhase = spec })
  where
    spec :: SimplifierMode -> Bool
    spec = join (||)
         . map (join (&&) . map match . split ':')
         . split ','
         $ case s of
             '=' : s' -> s'
             _        -> s

    join :: (Bool -> Bool -> Bool)
         -> [SimplifierMode -> Bool]
         -> SimplifierMode -> Bool
    join _  [] = const True
    join op ss = foldr1 (\f g x -> f x `op` g x) ss

    match :: String -> SimplifierMode -> Bool
    match "" = const True
    match s  = case reads s of
                [(n,"")] -> phase_num  n
                _        -> phase_name s

    phase_num :: Int -> SimplifierMode -> Bool
    phase_num n (SimplPhase k _) = n == k
    phase_num _ _                = False

    phase_name :: String -> SimplifierMode -> Bool
    phase_name s SimplGently       = s == "gentle"
    phase_name s (SimplPhase _ ss) = s `elem` ss

setVerbosity :: Maybe Int -> DynP ()
setVerbosity mb_n = upd (\dfs -> dfs{ verbosity = mb_n `orElse` 3 })

addCmdlineHCInclude :: String -> DynP ()
addCmdlineHCInclude a = upd (\s -> s{cmdlineHcIncludes =  a : cmdlineHcIncludes s})

extraPkgConf_ :: FilePath -> DynP ()
extraPkgConf_  p = upd (\s -> s{ extraPkgConfs = p : extraPkgConfs s })

exposePackage, hidePackage, ignorePackage :: String -> DynP ()
exposePackage p =
  upd (\s -> s{ packageFlags = ExposePackage p : packageFlags s })
hidePackage p =
  upd (\s -> s{ packageFlags = HidePackage p : packageFlags s })
ignorePackage p =
  upd (\s -> s{ packageFlags = IgnorePackage p : packageFlags s })

setPackageName :: String -> DynFlags -> DynFlags
setPackageName p
  | Nothing <- unpackPackageId pid
  = ghcError (CmdLineError ("cannot parse \'" ++ p ++ "\' as a package identifier"))
  | otherwise
  = \s -> s{ thisPackage = pid }
  where
        pid = stringToPackageId p

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
--    -fno-spec-constr-threshold        run SpecConstr even for big loops
--    -fno-spec-constr-count            SpecConstr as much as possible
--    -finline-enough-args              hack to prevent excessive inlining
--
setDPHOpt :: DynFlags -> DynFlags
setDPHOpt dflags = setOptLevel 2 (dflags { maxSimplIterations  = 20
                                         , specConstrThreshold = Nothing
                                         , specConstrCount     = Nothing
                                         })
                   `dopt_set`   Opt_DictsCheap
                   `dopt_unset` Opt_MethodSharing
                   `dopt_set`   Opt_InlineIfEnoughArgs

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

-- | Get the unqualified name of the function to use as the \"main\" for the main module.
-- Either returns the default name or the one configured on the command line with -main-is
getMainFun :: DynFlags -> RdrName
getMainFun dflags = case (mainFunIs dflags) of
    Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn))
    Nothing -> main_RDR_Unqual

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
               sta = opt_Static
           in
                    ( [ if sta then "-DDONT_WANT_WIN32_DLL_SUPPORT" else ""
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
        = ( [], ["-fomit-frame-pointer",
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
        = ["-fno-common", "-D__PIC__"]
    | otherwise
        = ["-mdynamic-no-pic"]
#elif mingw32_TARGET_OS
      -- no -fPIC for Windows
    | opt_PIC
        = ["-D__PIC__"]
    | otherwise
        = []
#else
    | opt_PIC
        = ["-fPIC", "-D__PIC__"]
    | otherwise
        = []
#endif

-- -----------------------------------------------------------------------------
-- Splitting

can_split :: Bool
can_split = cSplitObjs == "YES"

-- -----------------------------------------------------------------------------
-- Compiler Info

compilerInfo :: [(String, String)]
compilerInfo = [("Project name",                cProjectName),
                ("Project version",             cProjectVersion),
                ("Booter version",              cBooterVersion),
                ("Stage",                       cStage),
                ("Interface file version",      cHscIfaceFileVersion),
                ("Have interpreter",            cGhcWithInterpreter),
                ("Object splitting",            cSplitObjs),
                ("Have native code generator",  cGhcWithNativeCodeGen),
                ("Support SMP",                 cGhcWithSMP),
                ("Unregisterised",              cGhcUnregisterised),
                ("Tables next to code",         cGhcEnableTablesNextToCode),
                ("Win32 DLLs",                  cEnableWin32DLLs),
                ("RTS ways",                    cGhcRTSWays),
                ("Leading underscore",          cLeadingUnderscore),
                ("Debug on",                    show debugIsOn)
               ]

