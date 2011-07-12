%
% (c) The University of Glasgow, 2006
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
-- | Types for the per-module compiler
module HscTypes ( 
	-- * compilation state
        HscEnv(..), hscEPS,
	FinderCache, FindResult(..), ModLocationCache,
	Target(..), TargetId(..), pprTarget, pprTargetId,
	ModuleGraph, emptyMG,

        -- * Information about modules
	ModDetails(..),	emptyModDetails,
        ModGuts(..), CgGuts(..), ForeignStubs(..), appendStubC,
        ImportedMods, ImportedModsVal,

	ModSummary(..), ms_imps, ms_mod_name, showModMsg, isBootSummary,
	msHsFilePath, msHiFilePath, msObjFilePath,

        -- * Information about the module being compiled
	HscSource(..), isHsBoot, hscSourceString,	-- Re-exported from DriverPhases
	
	-- * State relating to modules in this package
	HomePackageTable, HomeModInfo(..), emptyHomePackageTable,
        hptInstances, hptRules, hptVectInfo,
        hptObjs,

	-- * State relating to known packages
	ExternalPackageState(..), EpsStats(..), addEpsInStats,
	PackageTypeEnv, PackageIfaceTable, emptyPackageIfaceTable,
	lookupIfaceByModule, emptyModIface,
	
	PackageInstEnv, PackageRuleBase,


        -- * Annotations
        prepareAnnotations,

        -- * Interactive context
	InteractiveContext(..), emptyInteractiveContext, 
	icPrintUnqual, extendInteractiveContext,
        substInteractiveContext,
        mkPrintUnqualified, pprModulePrefix,

	-- * Interfaces
	ModIface(..), mkIfaceWarnCache, mkIfaceHashCache, mkIfaceFixCache,
	emptyIfaceWarnCache,

        -- * Fixity
	FixityEnv, FixItem(..), lookupFixity, emptyFixityEnv,

        -- * TyThings and type environments
	TyThing(..),
	tyThingClass, tyThingTyCon, tyThingDataCon, tyThingId, tyThingCoAxiom,
	implicitTyThings, implicitTyConThings, implicitClassThings, isImplicitTyThing,
	
	TypeEnv, lookupType, lookupTypeHscEnv, mkTypeEnv, emptyTypeEnv,
	extendTypeEnv, extendTypeEnvList, extendTypeEnvWithIds, lookupTypeEnv,
	typeEnvElts, typeEnvClasses, typeEnvTyCons, typeEnvIds,
	typeEnvDataCons, typeEnvCoAxioms,

        -- * MonadThings
        MonadThings(..),

        -- * Information on imports and exports
	WhetherHasOrphans, IsBootInterface, Usage(..), 
	Dependencies(..), noDependencies,
	NameCache(..), OrigNameCache, OrigIParamCache,
	Avails, availsToNameSet, availsToNameEnv, availName, availNames,
	GenAvailInfo(..), AvailInfo, RdrAvailInfo, 
	IfaceExport,

	-- * Warnings
	Warnings(..), WarningTxt(..), plusWarns,

	-- * Linker stuff
        Linkable(..), isObjectLinkable, linkableObjs,
	Unlinked(..), CompiledByteCode,
	isObject, nameOfObject, isInterpretable, byteCodeOfObject,
        
        -- * Program coverage
        HpcInfo(..), emptyHpcInfo, isHpcUsed, AnyHpcUsage,

        -- * Breakpoints
        ModBreaks (..), BreakIndex, emptyModBreaks,

        -- * Vectorisation information
        VectInfo(..), IfaceVectInfo(..), noVectInfo, plusVectInfo, 
        noIfaceVectInfo,

        -- * Safe Haskell information
        IfaceTrustInfo, getSafeMode, setSafeMode, noIfaceTrustInfo,
        trustInfoToNum, numToTrustInfo, IsSafeImport,

        -- * Compilation errors and warnings
        SourceError, GhcApiError, mkSrcErr, srcErrorMessages, mkApiErr,
        throwOneError, handleSourceError,
        handleFlagWarnings, printOrThrowWarnings,
    ) where

#include "HsVersions.h"

#ifdef GHCI
import ByteCodeAsm      ( CompiledByteCode )
import {-# SOURCE #-}  InteractiveEval ( Resume )
#endif

import HsSyn
import RdrName
import Name
import NameEnv
import NameSet  
import Module
import InstEnv          ( InstEnv, Instance )
import FamInstEnv       ( FamInstEnv, FamInst )
import Rules            ( RuleBase )
import CoreSyn          ( CoreBind )
import VarEnv
import VarSet
import Var
import Id
import Type             

import Annotations
import Class		( Class, classAllSelIds, classATs, classTyCon )
import TyCon
import DataCon		( DataCon, dataConImplicitIds, dataConWrapId )
import PrelNames	( gHC_PRIM )
import Packages hiding ( Version(..) )
import DynFlags		( DynFlags(..), isOneShot, HscTarget (..), dopt,
                          DynFlag(..), SafeHaskellMode(..), dynFlagDependencies )
import DriverPhases	( HscSource(..), isHsBoot, hscSourceString, Phase )
import BasicTypes	( IPName, defaultFixity, WarningTxt(..) )
import OptimizationFuel	( OptFuelState )
import IfaceSyn
import CoreSyn		( CoreRule, CoreVect )
import Maybes		( orElse, expectJust, catMaybes )
import Outputable
import BreakArray
import SrcLoc
import UniqFM		( lookupUFM, eltsUFM, emptyUFM )
import UniqSupply	( UniqSupply )
import FastString
import StringBuffer	( StringBuffer )
import Fingerprint
import MonadUtils
import Bag
import ErrUtils

import System.FilePath
import System.Time	( ClockTime )
import Data.IORef
import Data.Array       ( Array, array )
import Data.List
import Data.Map (Map)
import Data.Word
import Control.Monad    ( mplus, guard, liftM, when )
import Exception
import Data.Typeable    ( Typeable )

-- -----------------------------------------------------------------------------
-- Source Errors

-- When the compiler (HscMain) discovers errors, it throws an
-- exception in the IO monad.

mkSrcErr :: ErrorMessages -> SourceError
srcErrorMessages :: SourceError -> ErrorMessages
mkApiErr :: SDoc -> GhcApiError

throwOneError :: MonadIO m => ErrMsg -> m ab
throwOneError err = liftIO $ throwIO $ mkSrcErr $ unitBag err

-- | A source error is an error that is caused by one or more errors in the
-- source code.  A 'SourceError' is thrown by many functions in the
-- compilation pipeline.  Inside GHC these errors are merely printed via
-- 'log_action', but API clients may treat them differently, for example,
-- insert them into a list box.  If you want the default behaviour, use the
-- idiom:
--
-- > handleSourceError printExceptionAndWarnings $ do
-- >   ... api calls that may fail ...
--
-- The 'SourceError's error messages can be accessed via 'srcErrorMessages'.
-- This list may be empty if the compiler failed due to @-Werror@
-- ('Opt_WarnIsError').
--
-- See 'printExceptionAndWarnings' for more information on what to take care
-- of when writing a custom error handler.
newtype SourceError = SourceError ErrorMessages
  deriving Typeable

instance Show SourceError where
  show (SourceError msgs) = unlines . map show . bagToList $ msgs
    -- ToDo: is there some nicer way to print this?

instance Exception SourceError

mkSrcErr = SourceError

-- | Perform the given action and call the exception handler if the action
-- throws a 'SourceError'.  See 'SourceError' for more information.
handleSourceError :: (ExceptionMonad m) =>
                     (SourceError -> m a) -- ^ exception handler
                  -> m a -- ^ action to perform
                  -> m a
handleSourceError handler act =
  gcatch act (\(e :: SourceError) -> handler e)

srcErrorMessages (SourceError msgs) = msgs

-- | XXX: what exactly is an API error?
newtype GhcApiError = GhcApiError SDoc
 deriving Typeable

instance Show GhcApiError where
  show (GhcApiError msg) = showSDoc msg

instance Exception GhcApiError

mkApiErr = GhcApiError

-- | Given a bag of warnings, turn them into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings dflags warns
  | dopt Opt_WarnIsError dflags
  = when (not (isEmptyBag warns)) $ do
      throwIO $ mkSrcErr $ warns `snocBag` warnIsErrorMsg
  | otherwise
  = printBagOfWarnings dflags warns

handleFlagWarnings :: DynFlags -> [Located String] -> IO ()
handleFlagWarnings dflags warns
 = when (dopt Opt_WarnDeprecatedFlags dflags) $ do
        -- It would be nicer if warns :: [Located Message], but that
        -- has circular import problems.
      let bag = listToBag [ mkPlainWarnMsg loc (text warn) 
                          | L loc warn <- warns ]

      printOrThrowWarnings dflags bag
\end{code}

\begin{code}
-- | Hscenv is like 'Session', except that some of the fields are immutable.
-- An HscEnv is used to compile a single module from plain Haskell source
-- code (after preprocessing) to either C, assembly or C--.  Things like
-- the module graph don't change during a single compilation.
--
-- Historical note: \"hsc\" used to be the name of the compiler binary,
-- when there was a separate driver and compiler.  To compile a single
-- module, the driver would invoke hsc on the source code... so nowadays
-- we think of hsc as the layer of the compiler that deals with compiling
-- a single module.
data HscEnv 
  = HscEnv { 
	hsc_dflags :: DynFlags,
		-- ^ The dynamic flag settings

	hsc_targets :: [Target],
		-- ^ The targets (or roots) of the current session

	hsc_mod_graph :: ModuleGraph,
		-- ^ The module graph of the current session

	hsc_IC :: InteractiveContext,
		-- ^ The context for evaluating interactive statements

	hsc_HPT    :: HomePackageTable,
		-- ^ The home package table describes already-compiled
		-- home-package modules, /excluding/ the module we 
		-- are compiling right now.
		-- (In one-shot mode the current module is the only
		--  home-package module, so hsc_HPT is empty.  All other
		--  modules count as \"external-package\" modules.
		--  However, even in GHCi mode, hi-boot interfaces are
		--  demand-loaded into the external-package table.)
		--
		-- 'hsc_HPT' is not mutable because we only demand-load 
		-- external packages; the home package is eagerly 
		-- loaded, module by module, by the compilation manager.
		--	
		-- The HPT may contain modules compiled earlier by @--make@
		-- but not actually below the current module in the dependency
		-- graph.

		-- (This changes a previous invariant: changed Jan 05.)
	
	hsc_EPS	:: {-# UNPACK #-} !(IORef ExternalPackageState),
	        -- ^ Information about the currently loaded external packages.
	        -- This is mutable because packages will be demand-loaded during
	        -- a compilation run as required.
	
	hsc_NC	:: {-# UNPACK #-} !(IORef NameCache),
		-- ^ As with 'hsc_EPS', this is side-effected by compiling to
		-- reflect sucking in interface files.  They cache the state of
		-- external interface files, in effect.

	hsc_FC   :: {-# UNPACK #-} !(IORef FinderCache),
	        -- ^ The cached result of performing finding in the file system
	hsc_MLC  :: {-# UNPACK #-} !(IORef ModLocationCache),
		-- ^ This caches the location of modules, so we don't have to 
		-- search the filesystem multiple times. See also 'hsc_FC'.

        hsc_OptFuel :: OptFuelState,
                -- ^ Settings to control the use of \"optimization fuel\":
                -- by limiting the number of transformations,
                -- we can use binary search to help find compiler bugs.

        hsc_type_env_var :: Maybe (Module, IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'TcRnTypes.tcg_type_env_var' for 
                -- 'TcRunTypes.TcGblEnv'
 }

hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

-- | A compilation target.
--
-- A target may be supplied with the actual text of the
-- module.  If so, use this instead of the file contents (this
-- is for use in an IDE where the file hasn't been saved by
-- the user yet).
data Target = Target
      { targetId           :: TargetId  -- ^ module or filename
      , targetAllowObjCode :: Bool      -- ^ object code allowed?
      , targetContents     :: Maybe (StringBuffer,ClockTime)
                                        -- ^ in-memory text buffer?
      }

data TargetId
  = TargetModule ModuleName
	-- ^ A module name: search for the file
  | TargetFile FilePath (Maybe Phase)
	-- ^ A filename: preprocess & parse it to find the module name.
	-- If specified, the Phase indicates how to compile this file
	-- (which phase to start from).  Nothing indicates the starting phase
	-- should be determined from the suffix of the filename.
  deriving Eq

pprTarget :: Target -> SDoc
pprTarget (Target id obj _) = 
   (if obj then char '*' else empty) <> pprTargetId id

instance Outputable Target where
    ppr = pprTarget

pprTargetId :: TargetId -> SDoc
pprTargetId (TargetModule m) = ppr m
pprTargetId (TargetFile f _) = text f

instance Outputable TargetId where
    ppr = pprTargetId

-- | Helps us find information about modules in the home package
type HomePackageTable  = ModuleNameEnv HomeModInfo
	-- Domain = modules in the home package that have been fully compiled
	-- "home" package name cached here for convenience

-- | Helps us find information about modules in the imported packages
type PackageIfaceTable = ModuleEnv ModIface
	-- Domain = modules in the imported packages

emptyHomePackageTable :: HomePackageTable
emptyHomePackageTable  = emptyUFM

emptyPackageIfaceTable :: PackageIfaceTable
emptyPackageIfaceTable = emptyModuleEnv

-- | Information about modules in the package being compiled
data HomeModInfo 
  = HomeModInfo {
      hm_iface    :: !ModIface,
        -- ^ The basic loaded interface file: every loaded module has one of
        -- these, even if it is imported from another package
      hm_details  :: !ModDetails,
        -- ^ Extra information that has been created from the 'ModIface' for
	-- the module, typically during typechecking
      hm_linkable :: !(Maybe Linkable)
        -- ^ The actual artifact we would like to link to access things in
	-- this module.
	--
	-- 'hm_linkable' might be Nothing:
	--
	--   1. If this is an .hs-boot module
	--
	--   2. Temporarily during compilation if we pruned away
	--      the old linkable because it was out of date.
	--
	-- After a complete compilation ('GHC.load'), all 'hm_linkable' fields
	-- in the 'HomePackageTable' will be @Just@.
	--
	-- When re-linking a module ('HscMain.HscNoRecomp'), we construct the
	-- 'HomeModInfo' by building a new 'ModDetails' from the old
	-- 'ModIface' (only).
    }

-- | Find the 'ModIface' for a 'Module', searching in both the loaded home
-- and external package module information
lookupIfaceByModule
	:: DynFlags
	-> HomePackageTable
	-> PackageIfaceTable
	-> Module
	-> Maybe ModIface
lookupIfaceByModule dflags hpt pit mod
  | modulePackageId mod == thisPackage dflags
  = 	-- The module comes from the home package, so look first
	-- in the HPT.  If it's not from the home package it's wrong to look
	-- in the HPT, because the HPT is indexed by *ModuleName* not Module
    fmap hm_iface (lookupUFM hpt (moduleName mod)) 
    `mplus` lookupModuleEnv pit mod

  | otherwise = lookupModuleEnv pit mod		-- Look in PIT only 

-- If the module does come from the home package, why do we look in the PIT as well?
-- (a) In OneShot mode, even home-package modules accumulate in the PIT
-- (b) Even in Batch (--make) mode, there is *one* case where a home-package
--     module is in the PIT, namely GHC.Prim when compiling the base package.
-- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
-- of its own, but it doesn't seem worth the bother.
\end{code}


\begin{code}
hptInstances :: HscEnv -> (ModuleName -> Bool) -> ([Instance], [FamInst])
-- ^ Find all the instance declarations (of classes and families) that are in
-- modules imported by this one, directly or indirectly, and are in the Home
-- Package Table.  This ensures that we don't see instances from modules @--make@
-- compiled before this one, but which are not below this one.
hptInstances hsc_env want_this_module
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                guard (want_this_module (moduleName (mi_module (hm_iface mod_info))))
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (concat insts, concat famInsts)

hptVectInfo :: HscEnv -> VectInfo
-- ^ Get the combined VectInfo of all modules in the home package table.  In
-- contrast to instances and rules, we don't care whether the modules are
-- \"below\" us in the dependency sense.  The VectInfo of those modules not \"below\" 
-- us does not affect the compilation of the current module.
hptVectInfo = concatVectInfo . hptAllThings ((: []) . md_vect_info . hm_details)

hptRules :: HscEnv -> [(ModuleName, IsBootInterface)] -> [CoreRule]
-- ^ Get rules from modules \"below\" this one (in the dependency sense)
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


hptAnns :: HscEnv -> Maybe [(ModuleName, IsBootInterface)] -> [Annotation]
-- ^ Get annotations from modules \"below\" this one (in the dependency sense)
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsUFM (hsc_HPT hsc_env))

hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> [(ModuleName, IsBootInterface)] -> [a]
-- Get things from modules \"below\" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
 | isOneShot (ghcMode (hsc_dflags hsc_env)) = []
  | otherwise
  = let 
	hpt = hsc_HPT hsc_env
    in
    [ thing
    |	-- Find each non-hi-boot module below me
      (mod, is_boot_mod) <- deps
    , include_hi_boot || not is_boot_mod

	-- unsavoury: when compiling the base package with --make, we
	-- sometimes try to look up RULES etc for GHC.Prim.  GHC.Prim won't
	-- be in the HPT, because we never compile it; it's in the EPT
	-- instead.  ToDo: clean up, and remove this slightly bogus
	-- filter:
    , mod /= moduleName gHC_PRIM

	-- Look it up in the HPT
    , let things = case lookupUFM hpt mod of
		    Just info -> extract info
		    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg [] 
	  msg = vcat [ptext (sLit "missing module") <+> ppr mod,
		      ptext (sLit "Probable cause: out-of-date interface files")]
			-- This really shouldn't happen, but see Trac #962

	-- And get its dfuns
    , thing <- things ]

hptObjs :: HomePackageTable -> [FilePath]
hptObjs hpt = concat (map (maybe [] linkableObjs . hm_linkable) (eltsUFM hpt))
\end{code}

%************************************************************************
%*									*
\subsection{Dealing with Annotations}
%*									*
%************************************************************************

\begin{code}
prepareAnnotations :: HscEnv -> Maybe ModGuts -> IO AnnEnv
-- ^ Deal with gathering annotations in from all possible places 
--   and combining them into a single 'AnnEnv'
prepareAnnotations hsc_env mb_guts
  = do { eps <- hscEPS hsc_env
       ; let -- Extract annotations from the module being compiled if supplied one
            mb_this_module_anns = fmap (mkAnnEnv . mg_anns) mb_guts
        -- Extract dependencies of the module if we are supplied one,
        -- otherwise load annotations from all home package table
        -- entries regardless of dependency ordering.
            home_pkg_anns  = (mkAnnEnv . hptAnns hsc_env) $ fmap (dep_mods . mg_deps) mb_guts
            other_pkg_anns = eps_ann_env eps
            ann_env        = foldl1' plusAnnEnv $ catMaybes [mb_this_module_anns, 
                                                             Just home_pkg_anns, 
                                                             Just other_pkg_anns]

       ; return ann_env }
\end{code}

%************************************************************************
%*									*
\subsection{The Finder cache}
%*									*
%************************************************************************

\begin{code}
-- | The 'FinderCache' maps home module names to the result of
-- searching for that module.  It records the results of searching for
-- modules along the search path.  On @:load@, we flush the entire
-- contents of this cache.
--
-- Although the @FinderCache@ range is 'FindResult' for convenience ,
-- in fact it will only ever contain 'Found' or 'NotFound' entries.
--
type FinderCache = ModuleNameEnv FindResult

-- | The result of searching for an imported module.
data FindResult
  = Found ModLocation Module
	-- ^ The module was found
  | NoPackage PackageId
	-- ^ The requested package was not found
  | FoundMultiple [PackageId]
	-- ^ _Error_: both in multiple packages

  | NotFound          -- Not found
      { fr_paths       :: [FilePath]       -- Places where I looked

      , fr_pkg         :: Maybe PackageId  -- Just p => module is in this package's
                                           --           manifest, but couldn't find
                                           --           the .hi file

      , fr_mods_hidden :: [PackageId]      -- Module is in these packages,
                                           --   but the *module* is hidden

      , fr_pkgs_hidden :: [PackageId]      -- Module is in these packages,
                                           --   but the *package* is hidden

      , fr_suggestions :: [Module]         -- Possible mis-spelled modules
      }

-- | Cache that remembers where we found a particular module.  Contains both
-- home modules and package modules.  On @:load@, only home modules are
-- purged from this cache.
type ModLocationCache = ModuleEnv ModLocation
\end{code}

%************************************************************************
%*									*
\subsection{Symbol tables and Module details}
%*									*
%************************************************************************

\begin{code}
-- | A 'ModIface' plus a 'ModDetails' summarises everything we know 
-- about a compiled module.  The 'ModIface' is the stuff *before* linking,
-- and can be written out to an interface file. The 'ModDetails is after 
-- linking and can be completely recovered from just the 'ModIface'.
-- 
-- When we read an interface file, we also construct a 'ModIface' from it,
-- except that we explicitly make the 'mi_decls' and a few other fields empty;
-- as when reading we consolidate the declarations etc. into a number of indexed
-- maps and environments in the 'ExternalPackageState'.
data ModIface 
   = ModIface {
        mi_module   :: !Module,             -- ^ Name of the module we are for
        mi_iface_hash :: !Fingerprint,      -- ^ Hash of the whole interface
        mi_mod_hash :: !Fingerprint,	    -- ^ Hash of the ABI only

        mi_orphan   :: !WhetherHasOrphans,  -- ^ Whether this module has orphans
        mi_finsts   :: !WhetherHasFamInst,  -- ^ Whether this module has family instances
	mi_boot	    :: !IsBootInterface,    -- ^ Read from an hi-boot file?

	mi_deps	    :: Dependencies,
	        -- ^ The dependencies of the module.  This is
		-- consulted for directly-imported modules, but not
		-- for anything else (hence lazy)

        mi_usages   :: [Usage],
                -- ^ Usages; kept sorted so that it's easy to decide
		-- whether to write a new iface file (changing usages
		-- doesn't affect the hash of this module)
        
		-- NOT STRICT!  we read this field lazily from the interface file
		-- It is *only* consulted by the recompilation checker

		-- Exports
		-- Kept sorted by (mod,occ), to make version comparisons easier
        mi_exports  :: ![IfaceExport],
                -- ^ Records the modules that are the declaration points for things
                -- exported by this module, and the 'OccName's of those things
        
        mi_exp_hash :: !Fingerprint,	-- ^ Hash of export list

        mi_fixities :: [(OccName,Fixity)],
                -- ^ Fixities
        
		-- NOT STRICT!  we read this field lazily from the interface file

	mi_warns  :: Warnings,
		-- ^ Warnings
		
		-- NOT STRICT!  we read this field lazily from the interface file

	mi_anns  :: [IfaceAnnotation],
	        -- ^ Annotations
	
		-- NOT STRICT!  we read this field lazily from the interface file

		-- Type, class and variable declarations
		-- The hash of an Id changes if its fixity or deprecations change
		--	(as well as its type of course)
		-- Ditto data constructors, class operations, except that 
		-- the hash of the parent class/tycon changes
	mi_decls :: [(Fingerprint,IfaceDecl)],	-- ^ Sorted type, variable, class etc. declarations

        mi_globals  :: !(Maybe GlobalRdrEnv),
		-- ^ Binds all the things defined at the top level in
		-- the /original source/ code for this module. which
		-- is NOT the same as mi_exports, nor mi_decls (which
		-- may contains declarations for things not actually
		-- defined by the user).  Used for GHCi and for inspecting
		-- the contents of modules via the GHC API only.
		--
		-- (We need the source file to figure out the
		-- top-level environment, if we didn't compile this module
		-- from source then this field contains @Nothing@).
		--
		-- Strictly speaking this field should live in the
		-- 'HomeModInfo', but that leads to more plumbing.

		-- Instance declarations and rules
	mi_insts     :: [IfaceInst],			-- ^ Sorted class instance
	mi_fam_insts :: [IfaceFamInst],			-- ^ Sorted family instances
	mi_rules     :: [IfaceRule],			-- ^ Sorted rules
	mi_orphan_hash :: !Fingerprint,	-- ^ Hash for orphan rules and 
					-- class and family instances
					-- combined

        mi_vect_info :: !IfaceVectInfo, -- ^ Vectorisation information

		-- Cached environments for easy lookup
		-- These are computed (lazily) from other fields
		-- and are not put into the interface file
	mi_warn_fn  :: Name -> Maybe WarningTxt,        -- ^ Cached lookup for 'mi_warns'
	mi_fix_fn  :: OccName -> Fixity,	        -- ^ Cached lookup for 'mi_fixities'
	mi_hash_fn :: OccName -> Maybe (OccName, Fingerprint),
                        -- ^ Cached lookup for 'mi_decls'.
			-- The @Nothing@ in 'mi_hash_fn' means that the thing
			-- isn't in decls. It's useful to know that when
			-- seeing if we are up to date wrt. the old interface.
                        -- The 'OccName' is the parent of the name, if it has one.
	mi_hpc    :: !AnyHpcUsage,
	        -- ^ True if this program uses Hpc at any point in the program.
	mi_trust  :: !IfaceTrustInfo,
	        -- ^ Safe Haskell Trust information for this module.
	mi_trust_pkg :: !Bool
	        -- ^ Do we require the package this module resides in be trusted
	        -- to trust this module? This is used for the situation where a
	        -- module is Safe (so doesn't require the package be trusted
	        -- itself) but imports some trustworthy modules from its own
	        -- package (which does require its own package be trusted).
                -- See Note [RnNames . Trust Own Package]
     }

-- | The 'ModDetails' is essentially a cache for information in the 'ModIface'
-- for home modules only. Information relating to packages will be loaded into
-- global environments in 'ExternalPackageState'.
data ModDetails
   = ModDetails {
	-- The next two fields are created by the typechecker
	md_exports   :: [AvailInfo],
        md_types     :: !TypeEnv,       -- ^ Local type environment for this particular module
        md_insts     :: ![Instance],    -- ^ 'DFunId's for the instances in this module
        md_fam_insts :: ![FamInst],
        md_rules     :: ![CoreRule],    -- ^ Domain may include 'Id's from other modules
        md_anns      :: ![Annotation],  -- ^ Annotations present in this module: currently 
                                        -- they only annotate things also declared in this module
        md_vect_info :: !VectInfo       -- ^ Module vectorisation information
     }

emptyModDetails :: ModDetails
emptyModDetails = ModDetails { md_types = emptyTypeEnv,
			       md_exports = [],
			       md_insts     = [],
			       md_rules     = [],
			       md_fam_insts = [],
                               md_anns      = [],
                               md_vect_info = noVectInfo
                             } 

-- | Records the modules directly imported by a module for extracting e.g. usage information
type ImportedMods = ModuleEnv [ImportedModsVal]
type ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)

-- TODO: we are not actually using the codomain of this type at all, so it can be
-- replaced with ModuleEnv ()

-- | A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a 'ModIface' and 
-- 'ModDetails' are extracted and the ModGuts is discarded.
data ModGuts
  = ModGuts {
        mg_module    :: !Module,         -- ^ Module being compiled
	mg_boot      :: IsBootInterface, -- ^ Whether it's an hs-boot module
	mg_exports   :: ![AvailInfo],	 -- ^ What it exports
	mg_deps	     :: !Dependencies,	 -- ^ What it depends on, directly or
	                                 -- otherwise
	mg_dir_imps  :: !ImportedMods,	 -- ^ Directly-imported modules; used to
					 -- generate initialisation code
	mg_used_names:: !NameSet,	 -- ^ What the module needed (used in 'MkIface.mkIface')

        mg_rdr_env   :: !GlobalRdrEnv,	 -- ^ Top-level lexical environment

	-- These fields all describe the things **declared in this module**
	mg_fix_env   :: !FixityEnv,	 -- ^ Fixities declared in this module
	                                 -- TODO: I'm unconvinced this is actually used anywhere
	mg_types     :: !TypeEnv,        -- ^ Types declared in this module
	mg_insts     :: ![Instance],	 -- ^ Class instances declared in this module
	mg_fam_insts :: ![FamInst],	 -- ^ Family instances declared in this module
        mg_rules     :: ![CoreRule],	 -- ^ Before the core pipeline starts, contains 
		     			 -- See Note [Overall plumbing for rules] in Rules.lhs
	mg_binds     :: ![CoreBind],	 -- ^ Bindings for this module
	mg_foreign   :: !ForeignStubs,   -- ^ Foreign exports declared in this module
	mg_warns     :: !Warnings,	 -- ^ Warnings declared in the module
        mg_anns      :: [Annotation],    -- ^ Annotations declared in this module
        mg_hpc_info  :: !HpcInfo,        -- ^ Coverage tick boxes in the module
        mg_modBreaks :: !ModBreaks,      -- ^ Breakpoints for the module
        mg_vect_decls:: ![CoreVect],     -- ^ Vectorisation declarations in this module
                                         --   (produced by desugarer & consumed by vectoriser)
        mg_vect_info :: !VectInfo,       -- ^ Pool of vectorised declarations in the module

	-- The next two fields are unusual, because they give instance
	-- environments for *all* modules in the home package, including
	-- this module, rather than for *just* this module.  
	-- Reason: when looking up an instance we don't want to have to
	--	  look at each module in the home package in turn
	mg_inst_env     :: InstEnv,
        -- ^ Class instance environment from /home-package/ modules (including
	-- this one); c.f. 'tcg_inst_env'
	mg_fam_inst_env :: FamInstEnv,
        -- ^ Type-family instance enviroment for /home-package/ modules
	-- (including this one); c.f. 'tcg_fam_inst_env'
        mg_trust_pkg :: Bool
        -- ^ Do we need to trust our own package for Safe Haskell?
        -- See Note [RnNames . Trust Own Package]
    }

-- The ModGuts takes on several slightly different forms:
--
-- After simplification, the following fields change slightly:
--	mg_rules	Orphan rules only (local ones now attached to binds)
--	mg_binds	With rules attached

-- The ModGuts takes on several slightly different forms:
--
-- After simplification, the following fields change slightly:
--	mg_rules	Orphan rules only (local ones now attached to binds)
--	mg_binds	With rules attached


---------------------------------------------------------
-- The Tidy pass forks the information about this module: 
--	* one lot goes to interface file generation (ModIface)
--	  and later compilations (ModDetails)
--	* the other lot goes to code generation (CgGuts)

-- | A restricted form of 'ModGuts' for code generation purposes
data CgGuts 
  = CgGuts {
	cg_module   :: !Module, -- ^ Module being compiled

	cg_tycons   :: [TyCon],
		-- ^ Algebraic data types (including ones that started
		-- life as classes); generate constructors and info
		-- tables. Includes newtypes, just for the benefit of
		-- External Core

	cg_binds    :: [CoreBind],
		-- ^ The tidied main bindings, including
		-- previously-implicit bindings for record and class
		-- selectors, and data construtor wrappers.  But *not*
		-- data constructor workers; reason: we we regard them
		-- as part of the code-gen of tycons

        cg_foreign  :: !ForeignStubs,   -- ^ Foreign export stubs
	cg_dep_pkgs :: ![PackageId],	-- ^ Dependent packages, used to 
	                                -- generate #includes for C code gen
        cg_hpc_info :: !HpcInfo,        -- ^ Program coverage tick box information
        cg_modBreaks :: !ModBreaks      -- ^ Module breakpoints
    }

-----------------------------------
-- | Foreign export stubs
data ForeignStubs = NoStubs             -- ^ We don't have any stubs
		  | ForeignStubs
			SDoc 		
			SDoc 		
		   -- ^ There are some stubs. Parameters:
		   --
		   --  1) Header file prototypes for
                   --     "foreign exported" functions
                   --
                   --  2) C stubs to use when calling
                   --     "foreign exported" functions

appendStubC :: ForeignStubs -> SDoc -> ForeignStubs
appendStubC NoStubs            c_code = ForeignStubs empty c_code
appendStubC (ForeignStubs h c) c_code = ForeignStubs h (c $$ c_code)
\end{code}

\begin{code}
emptyModIface :: Module -> ModIface
emptyModIface mod
  = ModIface { mi_module   = mod,
	       mi_iface_hash = fingerprint0,
	       mi_mod_hash = fingerprint0,
	       mi_orphan   = False,
	       mi_finsts   = False,
	       mi_boot	   = False,
	       mi_deps     = noDependencies,
	       mi_usages   = [],
	       mi_exports  = [],
	       mi_exp_hash = fingerprint0,
	       mi_fixities = [],
	       mi_warns    = NoWarnings,
	       mi_anns     = [],
	       mi_insts     = [],
	       mi_fam_insts = [],
	       mi_rules     = [],
	       mi_decls     = [],
	       mi_globals   = Nothing,
	       mi_orphan_hash = fingerprint0,
               mi_vect_info = noIfaceVectInfo,
	       mi_warn_fn    = emptyIfaceWarnCache,
	       mi_fix_fn    = emptyIfaceFixCache,
	       mi_hash_fn   = emptyIfaceHashCache,
	       mi_hpc       = False,
	       mi_trust     = noIfaceTrustInfo,
               mi_trust_pkg = False
    }		
\end{code}


%************************************************************************
%*									*
\subsection{The interactive context}
%*									*
%************************************************************************

\begin{code}
-- | Interactive context, recording information about the state of the
-- context in which statements are executed in a GHC session.
--
data InteractiveContext 
  = InteractiveContext { 
         -- These two fields are only stored here so that the client
         -- can retrieve them with GHC.getContext.  GHC itself doesn't
         -- use them, but it does reset them to empty sometimes (such
         -- as before a GHC.load).  The context is set with GHC.setContext.
         ic_toplev_scope :: [Module],
             -- ^ The context includes the "top-level" scope of
             -- these modules
         ic_imports :: [ImportDecl RdrName],
             -- ^ The context is extended with these import declarations

         ic_rn_gbl_env :: GlobalRdrEnv,
             -- ^ The contexts' cached 'GlobalRdrEnv', built by
             -- 'InteractiveEval.setContext'

         ic_tmp_ids :: [Id],
             -- ^ Names bound during interaction with the user.  Later
             -- Ids shadow earlier ones with the same OccName
             -- Expressions are typed with these Ids in the envt For
             -- runtime-debugging, these Ids may have free TcTyVars of
             -- RuntimUnkSkol flavour, but no free TyVars (because the
             -- typechecker doesn't expect that)

#ifdef GHCI
         ic_resume :: [Resume],
             -- ^ The stack of breakpoint contexts
#endif

         ic_cwd :: Maybe FilePath
             -- virtual CWD of the program
    }


emptyInteractiveContext :: InteractiveContext
emptyInteractiveContext
  = InteractiveContext { ic_toplev_scope = [],
                         ic_imports = [],
			 ic_rn_gbl_env = emptyGlobalRdrEnv,
			 ic_tmp_ids = []
#ifdef GHCI
                         , ic_resume = []
#endif
                         , ic_cwd = Nothing
                       }

icPrintUnqual :: DynFlags -> InteractiveContext -> PrintUnqualified
icPrintUnqual dflags ictxt = mkPrintUnqualified dflags (ic_rn_gbl_env ictxt)


extendInteractiveContext
        :: InteractiveContext
        -> [Id]
        -> InteractiveContext
extendInteractiveContext ictxt ids
  = ictxt { ic_tmp_ids =  snub ((ic_tmp_ids ictxt \\ ids) ++ ids)
                          -- NB. must be this way around, because we want
                          -- new ids to shadow existing bindings.
          }
    where snub = map head . group . sort

substInteractiveContext :: InteractiveContext -> TvSubst -> InteractiveContext
substInteractiveContext ictxt subst | isEmptyTvSubst subst = ictxt
substInteractiveContext ictxt@InteractiveContext{ic_tmp_ids=ids} subst 
  = ictxt { ic_tmp_ids = map subst_ty ids }
  where
   subst_ty id = id `setIdType` substTy subst (idType id)
\end{code}

%************************************************************************
%*									*
        Building a PrintUnqualified		
%*									*
%************************************************************************

Note [Printing original names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding how to print names is pretty tricky.  We are given a name
P:M.T, where P is the package name, M is the defining module, and T is
the occurrence name, and we have to decide in which form to display
the name given a GlobalRdrEnv describing the current scope.

Ideally we want to display the name in the form in which it is in
scope.  However, the name might not be in scope at all, and that's
where it gets tricky.  Here are the cases:

 1. T uniquely maps to  P:M.T      --->  "T"      NameUnqual
 2. There is an X for which X.T 
       uniquely maps to  P:M.T     --->  "X.T"    NameQual X
 3. There is no binding for "M.T"  --->  "M.T"    NameNotInScope1
 4. Otherwise                      --->  "P:M.T"  NameNotInScope2

(3) and (4) apply when the entity P:M.T is not in the GlobalRdrEnv at
all. In these cases we still want to refer to the name as "M.T", *but*
"M.T" might mean something else in the current scope (e.g. if there's
an "import X as M"), so to avoid confusion we avoid using "M.T" if
there's already a binding for it.  Instead we write P:M.T.

There's one further subtlety: in case (3), what if there are two
things around, P1:M.T and P2:M.T?  Then we don't want to print both of
them as M.T!  However only one of the modules P1:M and P2:M can be
exposed (say P2), so we use M.T for that, and P1:M.T for the other one.
This is handled by the qual_mod component of PrintUnqualified, inside
the (ppr mod) of case (3), in Name.pprModulePrefix

\begin{code}
-- | Creates some functions that work out the best ways to format
-- names for the user according to a set of heuristics
mkPrintUnqualified :: DynFlags -> GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualified dflags env = (qual_name, qual_mod)
  where
  qual_name mod occ	-- The (mod,occ) pair is the original name of the thing
        | [gre] <- unqual_gres, right_name gre = NameUnqual
		-- If there's a unique entity that's in scope unqualified with 'occ'
		-- AND that entity is the right one, then we can use the unqualified name

        | [gre] <- qual_gres = NameQual (get_qual_mod (gre_prov gre))

        | null qual_gres = 
              if null (lookupGRE_RdrName (mkRdrQual (moduleName mod) occ) env)
                   then NameNotInScope1
                   else NameNotInScope2

	| otherwise = panic "mkPrintUnqualified"
      where
	right_name gre = nameModule_maybe (gre_name gre) == Just mod

        unqual_gres = lookupGRE_RdrName (mkRdrUnqual occ) env
        qual_gres   = filter right_name (lookupGlobalRdrEnv env occ)

	get_qual_mod LocalDef      = moduleName mod
	get_qual_mod (Imported is) = ASSERT( not (null is) ) is_as (is_decl (head is))

    -- we can mention a module P:M without the P: qualifier iff
    -- "import M" would resolve unambiguously to P:M.  (if P is the
    -- current package we can just assume it is unqualified).

  qual_mod mod
     | modulePackageId mod == thisPackage dflags = False

     | [pkgconfig] <- [pkg | (pkg,exposed_module) <- lookup, 
                             exposed pkg && exposed_module],
       packageConfigId pkgconfig == modulePackageId mod
        -- this says: we are given a module P:M, is there just one exposed package
        -- that exposes a module M, and is it package P?
     = False

     | otherwise = True
     where lookup = lookupModuleInAllPackages dflags (moduleName mod)
\end{code}


%************************************************************************
%*									*
		TyThing
%*									*
%************************************************************************

\begin{code}
-- | Determine the 'TyThing's brought into scope by another 'TyThing'
-- /other/ than itself. For example, Id's don't have any implicit TyThings
-- as they just bring themselves into scope, but classes bring their
-- dictionary datatype, type constructor and some selector functions into
-- scope, just for a start!

-- N.B. the set of TyThings returned here *must* match the set of
-- names returned by LoadIface.ifaceDeclSubBndrs, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in LoadIface.loadDecl (see note [Tricky iface loop])
-- The order of the list does not matter.
implicitTyThings :: TyThing -> [TyThing]
implicitTyThings (AnId _)       = []
implicitTyThings (ACoAxiom _cc) = []
implicitTyThings (ATyCon tc)    = implicitTyConThings tc
implicitTyThings (AClass cl)    = implicitClassThings cl
implicitTyThings (ADataCon dc)  = map AnId (dataConImplicitIds dc)
    -- For data cons add the worker and (possibly) wrapper
    
implicitClassThings :: Class -> [TyThing]
implicitClassThings cl 
  = -- Does not include default methods, because those Ids may have
    --    their own pragmas, unfoldings etc, not derived from the Class object
    -- Dictionary datatype:
    --    [extras_plus:]
    --      type constructor 
    --    [recursive call:]
    --      (possibly) newtype coercion; definitely no family coercion here
    --      data constructor
    --      worker
    --      (no wrapper by invariant)
    extras_plus (ATyCon (classTyCon cl)) ++
    -- associated types 
    --    No extras_plus (recursive call) for the classATs, because they
    --    are only the family decls; they have no implicit things
    map ATyCon (classATs cl) ++
    -- superclass and operation selectors
    map AnId (classAllSelIds cl)

implicitTyConThings :: TyCon -> [TyThing]
implicitTyConThings tc 
  =   -- fields (names of selectors)
      -- (possibly) implicit coercion and family coercion
      --   depending on whether it's a newtype or a family instance or both
    implicitCoTyCon tc ++
      -- for each data constructor in order,
      --   the contructor, worker, and (possibly) wrapper
    concatMap (extras_plus . ADataCon) (tyConDataCons tc)


-- add a thing and recursive call
extras_plus :: TyThing -> [TyThing]
extras_plus thing = thing : implicitTyThings thing

-- For newtypes and indexed data types (and both),
-- add the implicit coercion tycon
implicitCoTyCon :: TyCon -> [TyThing]
implicitCoTyCon tc 
  = map ACoAxiom . catMaybes $ [-- Just if newtype, Nothing if not
                              newTyConCo_maybe tc,
                              -- Just if family instance, Nothing if not
			      tyConFamilyCoercion_maybe tc] 

-- sortByOcc = sortBy (\ x -> \ y -> getOccName x < getOccName y)


-- | Returns @True@ if there should be no interface-file declaration
-- for this thing on its own: either it is built-in, or it is part
-- of some other declaration, or it is generated implicitly by some
-- other declaration.
isImplicitTyThing :: TyThing -> Bool
isImplicitTyThing (ADataCon {}) = True
isImplicitTyThing (AnId id)     = isImplicitId id
isImplicitTyThing (AClass {})   = False
isImplicitTyThing (ATyCon tc)   = isImplicitTyCon tc
isImplicitTyThing (ACoAxiom {}) = True

extendTypeEnvWithIds :: TypeEnv -> [Id] -> TypeEnv
extendTypeEnvWithIds env ids
  = extendNameEnvList env [(getName id, AnId id) | id <- ids]
\end{code}

%************************************************************************
%*									*
		TypeEnv
%*									*
%************************************************************************

\begin{code}
-- | A map from 'Name's to 'TyThing's, constructed by typechecking
-- local declarations or interface files
type TypeEnv = NameEnv TyThing

emptyTypeEnv    :: TypeEnv
typeEnvElts     :: TypeEnv -> [TyThing]
typeEnvClasses  :: TypeEnv -> [Class]
typeEnvTyCons   :: TypeEnv -> [TyCon]
typeEnvCoAxioms :: TypeEnv -> [CoAxiom]
typeEnvIds      :: TypeEnv -> [Id]
typeEnvDataCons :: TypeEnv -> [DataCon]
lookupTypeEnv   :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv 	    = emptyNameEnv
typeEnvElts     env = nameEnvElts env
typeEnvClasses  env = [cl | AClass cl   <- typeEnvElts env]
typeEnvTyCons   env = [tc | ATyCon tc   <- typeEnvElts env] 
typeEnvCoAxioms env = [ax | ACoAxiom ax <- typeEnvElts env] 
typeEnvIds      env = [id | AnId id     <- typeEnvElts env] 
typeEnvDataCons env = [dc | ADataCon dc <- typeEnvElts env] 

mkTypeEnv :: [TyThing] -> TypeEnv
mkTypeEnv things = extendTypeEnvList emptyTypeEnv things
		
lookupTypeEnv = lookupNameEnv

-- Extend the type environment
extendTypeEnv :: TypeEnv -> TyThing -> TypeEnv
extendTypeEnv env thing = extendNameEnv env (getName thing) thing 

extendTypeEnvList :: TypeEnv -> [TyThing] -> TypeEnv
extendTypeEnvList env things = foldl extendTypeEnv env things
\end{code}

\begin{code}
-- | Find the 'TyThing' for the given 'Name' by using all the resources
-- at our disposal: the compiled modules in the 'HomePackageTable' and the
-- compiled modules in other packages that live in 'PackageTypeEnv'. Note
-- that this does NOT look up the 'TyThing' in the module being compiled: you
-- have to do that yourself, if desired
lookupType :: DynFlags
	   -> HomePackageTable
	   -> PackageTypeEnv
	   -> Name
	   -> Maybe TyThing

lookupType dflags hpt pte name
  -- in one-shot, we don't use the HPT
  | not (isOneShot (ghcMode dflags)) && modulePackageId mod == this_pkg 
  = do hm <- lookupUFM hpt (moduleName mod) -- Maybe monad
       lookupNameEnv (md_types (hm_details hm)) name
  | otherwise
  = lookupNameEnv pte name
  where mod = ASSERT( isExternalName name ) nameModule name
	this_pkg = thisPackage dflags

-- | As 'lookupType', but with a marginally easier-to-use interface
-- if you have a 'HscEnv'
lookupTypeHscEnv :: HscEnv -> Name -> IO (Maybe TyThing)
lookupTypeHscEnv hsc_env name = do
    eps <- readIORef (hsc_EPS hsc_env)
    return $! lookupType dflags hpt (eps_PTE eps) name
  where 
    dflags = hsc_dflags hsc_env
    hpt = hsc_HPT hsc_env
\end{code}

\begin{code}
-- | Get the 'TyCon' from a 'TyThing' if it is a type constructor thing. Panics otherwise
tyThingTyCon :: TyThing -> TyCon
tyThingTyCon (ATyCon tc) = tc
tyThingTyCon other	 = pprPanic "tyThingTyCon" (pprTyThing other)

-- | Get the 'CoAxiom' from a 'TyThing' if it is a coercion axiom thing. Panics otherwise
tyThingCoAxiom :: TyThing -> CoAxiom
tyThingCoAxiom (ACoAxiom ax) = ax
tyThingCoAxiom other	     = pprPanic "tyThingCoAxiom" (pprTyThing other)

-- | Get the 'Class' from a 'TyThing' if it is a class thing. Panics otherwise
tyThingClass :: TyThing -> Class
tyThingClass (AClass cls) = cls
tyThingClass other	  = pprPanic "tyThingClass" (pprTyThing other)

-- | Get the 'DataCon' from a 'TyThing' if it is a data constructor thing. Panics otherwise
tyThingDataCon :: TyThing -> DataCon
tyThingDataCon (ADataCon dc) = dc
tyThingDataCon other	     = pprPanic "tyThingDataCon" (pprTyThing other)

-- | Get the 'Id' from a 'TyThing' if it is a id *or* data constructor thing. Panics otherwise
tyThingId :: TyThing -> Id
tyThingId (AnId id)     = id
tyThingId (ADataCon dc) = dataConWrapId dc
tyThingId other         = pprPanic "tyThingId" (pprTyThing other)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{MonadThings and friends}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Class that abstracts out the common ability of the monads in GHC
-- to lookup a 'TyThing' in the monadic environment by 'Name'. Provides
-- a number of related convenience functions for accessing particular
-- kinds of 'TyThing'
class Monad m => MonadThings m where
        lookupThing :: Name -> m TyThing

        lookupId :: Name -> m Id
        lookupId = liftM tyThingId . lookupThing

        lookupDataCon :: Name -> m DataCon
        lookupDataCon = liftM tyThingDataCon . lookupThing

        lookupTyCon :: Name -> m TyCon
        lookupTyCon = liftM tyThingTyCon . lookupThing

        lookupClass :: Name -> m Class
        lookupClass = liftM tyThingClass . lookupThing
\end{code}

\begin{code}
-- | Constructs cache for the 'mi_hash_fn' field of a 'ModIface'
mkIfaceHashCache :: [(Fingerprint,IfaceDecl)]
                 -> (OccName -> Maybe (OccName, Fingerprint))
mkIfaceHashCache pairs 
  = \occ -> lookupOccEnv env occ
  where
    env = foldr add_decl emptyOccEnv pairs
    add_decl (v,d) env0 = foldr add_imp env1 (ifaceDeclSubBndrs d)
      where
          decl_name = ifName d
          env1 = extendOccEnv env0 decl_name (decl_name, v)
          add_imp bndr env = extendOccEnv env bndr (decl_name, v)

emptyIfaceHashCache :: OccName -> Maybe (OccName, Fingerprint)
emptyIfaceHashCache _occ = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Auxiliary types}
%*									*
%************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere

\begin{code}
------------------ Warnings -------------------------
-- | Warning information for a module
data Warnings
  = NoWarnings                          -- ^ Nothing deprecated
  | WarnAll WarningTxt	                -- ^ Whole module deprecated
  | WarnSome [(OccName,WarningTxt)]     -- ^ Some specific things deprecated

     -- Only an OccName is needed because
     --    (1) a deprecation always applies to a binding
     --        defined in the module in which the deprecation appears.
     --    (2) deprecations are only reported outside the defining module.
     --        this is important because, otherwise, if we saw something like
     --
     --        {-# DEPRECATED f "" #-}
     --        f = ...
     --        h = f
     --        g = let f = undefined in f
     --
     --        we'd need more information than an OccName to know to say something
     --        about the use of f in h but not the use of the locally bound f in g
     --
     --        however, because we only report about deprecations from the outside,
     --        and a module can only export one value called f,
     --        an OccName suffices.
     --
     --        this is in contrast with fixity declarations, where we need to map
     --        a Name to its fixity declaration.
  deriving( Eq )

-- | Constructs the cache for the 'mi_warn_fn' field of a 'ModIface'
mkIfaceWarnCache :: Warnings -> Name -> Maybe WarningTxt
mkIfaceWarnCache NoWarnings  = \_ -> Nothing
mkIfaceWarnCache (WarnAll t) = \_ -> Just t
mkIfaceWarnCache (WarnSome pairs) = lookupOccEnv (mkOccEnv pairs) . nameOccName

emptyIfaceWarnCache :: Name -> Maybe WarningTxt
emptyIfaceWarnCache _ = Nothing

plusWarns :: Warnings -> Warnings -> Warnings
plusWarns d NoWarnings = d
plusWarns NoWarnings d = d
plusWarns _ (WarnAll t) = WarnAll t
plusWarns (WarnAll t) _ = WarnAll t
plusWarns (WarnSome v1) (WarnSome v2) = WarnSome (v1 ++ v2)
\end{code}
\begin{code}
-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails	  = [AvailInfo]
-- | 'Name'd things that are available
type AvailInfo    = GenAvailInfo Name
-- | 'RdrName'd things that are available
type RdrAvailInfo = GenAvailInfo OccName

-- | Records what things are "available", i.e. in scope
data GenAvailInfo name	= Avail name	 -- ^ An ordinary identifier in scope
			| AvailTC name
				  [name] -- ^ A type or class in scope. Parameters:
				         --
				         --  1) The name of the type or class
				         --
				         --  2) The available pieces of type or class.
					 --     NB: If the type or class is itself
					 --     to be in scope, it must be in this list.
					 --     Thus, typically: @AvailTC Eq [Eq, ==, \/=]@
			deriving( Eq )
			-- Equality used when deciding if the interface has changed

-- | The original names declared of a certain module that are exported
type IfaceExport = (Module, [GenAvailInfo OccName])

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = addListToNameSet set (availNames avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: GenAvailInfo name -> name
availName (Avail n)     = n
availName (AvailTC n _) = n

-- | All names made available by the availability information
availNames :: GenAvailInfo name -> [name]
availNames (Avail n)      = [n]
availNames (AvailTC _ ns) = ns

instance Outputable n => Outputable (GenAvailInfo n) where
   ppr = pprAvail

pprAvail :: Outputable n => GenAvailInfo n -> SDoc
pprAvail (Avail n)      = ppr n
pprAvail (AvailTC n ns) = ppr n <> braces (hsep (punctuate comma (map ppr ns)))
\end{code}

\begin{code}
-- | Creates cached lookup for the 'mi_fix_fn' field of 'ModIface'
mkIfaceFixCache :: [(OccName, Fixity)] -> OccName -> Fixity
mkIfaceFixCache pairs 
  = \n -> lookupOccEnv env n `orElse` defaultFixity
  where
   env = mkOccEnv pairs

emptyIfaceFixCache :: OccName -> Fixity
emptyIfaceFixCache _ = defaultFixity

-- | Fixity environment mapping names to their fixities
type FixityEnv = NameEnv FixItem

-- | Fixity information for an 'Name'. We keep the OccName in the range 
-- so that we can generate an interface from it
data FixItem = FixItem OccName Fixity

instance Outputable FixItem where
  ppr (FixItem occ fix) = ppr fix <+> ppr occ

emptyFixityEnv :: FixityEnv
emptyFixityEnv = emptyNameEnv

lookupFixity :: FixityEnv -> Name -> Fixity
lookupFixity env n = case lookupNameEnv env n of
			Just (FixItem _ fix) -> fix
			Nothing	      	-> defaultFixity
\end{code}


%************************************************************************
%*									*
\subsection{WhatsImported}
%*									*
%************************************************************************

\begin{code}
-- | Records whether a module has orphans. An \"orphan\" is one of:
--
-- * An instance declaration in a module other than the definition
--   module for one of the type constructors or classes in the instance head
--
-- * A transformation rule in a module other than the one defining
--   the function in the head of the rule
type WhetherHasOrphans   = Bool

-- | Does this module define family instances?
type WhetherHasFamInst = Bool

-- | Did this module originate from a *-boot file?
type IsBootInterface = Bool

-- | Dependency information about modules and packages below this one
-- in the import hierarchy.
--
-- Invariant: the dependencies of a module @M@ never includes @M@.
--
-- Invariant: none of the lists contain duplicates.
data Dependencies
  = Deps { dep_mods   :: [(ModuleName, IsBootInterface)]
                        -- ^ Home-package module dependencies
         , dep_pkgs   :: [(PackageId, Bool)]
                       -- ^ External package dependencies. The bool indicates
                        -- if the package is required to be trusted when the
                        -- module is imported as a safe import (Safe Haskell).
                        -- See Note [RnNames . Tracking Trust Transitively]
         , dep_orphs  :: [Module]
                        -- ^ Orphan modules (whether home or external pkg),
                        -- *not* including family instance orphans as they
                        -- are anyway included in 'dep_finsts'
         , dep_finsts :: [Module]
                        -- ^ Modules that contain family instances (whether the
                        -- instances are from the home or an external package)
         }
  deriving( Eq )
        -- Equality used only for old/new comparison in MkIface.addVersionInfo
        -- See 'TcRnTypes.ImportAvails' for details on dependencies.

noDependencies :: Dependencies
noDependencies = Deps [] [] [] []

-- | Records modules that we depend on by making a direct import from
data Usage
  = UsagePackageModule {
        usg_mod      :: Module,
           -- ^ External package module depended on
        usg_mod_hash :: Fingerprint,
	    -- ^ Cached module fingerprint
        usg_safe :: IsSafeImport
            -- ^ Was this module imported as a safe import
    }                                           -- ^ Module from another package
  | UsageHomeModule {
        usg_mod_name :: ModuleName,
            -- ^ Name of the module
	usg_mod_hash :: Fingerprint,
	    -- ^ Cached module fingerprint
	usg_entities :: [(OccName,Fingerprint)],
            -- ^ Entities we depend on, sorted by occurrence name and fingerprinted.
            -- NB: usages are for parent names only, e.g. type constructors 
            -- but not the associated data constructors.
	usg_exports  :: Maybe Fingerprint,
            -- ^ Fingerprint for the export list we used to depend on this module,
            -- if we depend on the export list
        usg_safe :: IsSafeImport
            -- ^ Was this module imported as a safe import
    }                                           -- ^ Module from the current package
    deriving( Eq )
	-- The export list field is (Just v) if we depend on the export list:
	--	i.e. we imported the module directly, whether or not we
	--	     enumerated the things we imported, or just imported 
        --           everything
	-- We need to recompile if M's exports change, because 
	-- if the import was	import M, 	we might now have a name clash
        --                                      in the importing module.
	-- if the import was	import M(x)	M might no longer export x
	-- The only way we don't depend on the export list is if we have
	--			import M()
	-- And of course, for modules that aren't imported directly we don't
	-- depend on their export lists
\end{code}


%************************************************************************
%*									*
		The External Package State
%*									*
%************************************************************************

\begin{code}
type PackageTypeEnv    = TypeEnv
type PackageRuleBase   = RuleBase
type PackageInstEnv    = InstEnv
type PackageFamInstEnv = FamInstEnv
type PackageVectInfo   = VectInfo
type PackageAnnEnv     = AnnEnv

-- | Information about other packages that we have slurped in by reading
-- their interface files
data ExternalPackageState
  = EPS {
	eps_is_boot :: !(ModuleNameEnv (ModuleName, IsBootInterface)),
		-- ^ In OneShot mode (only), home-package modules
		-- accumulate in the external package state, and are
		-- sucked in lazily.  For these home-pkg modules
		-- (only) we need to record which are boot modules.
		-- We set this field after loading all the
		-- explicitly-imported interfaces, but before doing
		-- anything else
		--
		-- The 'ModuleName' part is not necessary, but it's useful for
		-- debug prints, and it's convenient because this field comes
		-- direct from 'TcRnTypes.imp_dep_mods'

	eps_PIT :: !PackageIfaceTable,
		-- ^ The 'ModIface's for modules in external packages
		-- whose interfaces we have opened.
		-- The declarations in these interface files are held in the
		-- 'eps_decls', 'eps_inst_env', 'eps_fam_inst_env' and 'eps_rules'
		-- fields of this record, not in the 'mi_decls' fields of the 
		-- interface we have sucked in.
		--
		-- What /is/ in the PIT is:
		--
		-- * The Module
		--
		-- * Fingerprint info
		--
		-- * Its exports
		--
		-- * Fixities
		--
		-- * Deprecations and warnings

	eps_PTE :: !PackageTypeEnv,	   
	        -- ^ Result of typechecking all the external package
	        -- interface files we have sucked in. The domain of
	        -- the mapping is external-package modules
	        
	eps_inst_env     :: !PackageInstEnv,   -- ^ The total 'InstEnv' accumulated
					       -- from all the external-package modules
	eps_fam_inst_env :: !PackageFamInstEnv,-- ^ The total 'FamInstEnv' accumulated
					       -- from all the external-package modules
	eps_rule_base    :: !PackageRuleBase,  -- ^ The total 'RuleEnv' accumulated
					       -- from all the external-package modules
	eps_vect_info    :: !PackageVectInfo,  -- ^ The total 'VectInfo' accumulated
					       -- from all the external-package modules
        eps_ann_env      :: !PackageAnnEnv,    -- ^ The total 'AnnEnv' accumulated
					       -- from all the external-package modules

        eps_mod_fam_inst_env :: !(ModuleEnv FamInstEnv), -- ^ The family instances accumulated from external
                                                         -- packages, keyed off the module that declared them

	eps_stats :: !EpsStats                 -- ^ Stastics about what was loaded from external packages
  }

-- | Accumulated statistics about what we are putting into the 'ExternalPackageState'.
-- \"In\" means stuff that is just /read/ from interface files,
-- \"Out\" means actually sucked in and type-checked
data EpsStats = EpsStats { n_ifaces_in
			 , n_decls_in, n_decls_out 
			 , n_rules_in, n_rules_out
			 , n_insts_in, n_insts_out :: !Int }

addEpsInStats :: EpsStats -> Int -> Int -> Int -> EpsStats
-- ^ Add stats for one newly-read interface
addEpsInStats stats n_decls n_insts n_rules
  = stats { n_ifaces_in = n_ifaces_in stats + 1
	  , n_decls_in  = n_decls_in stats + n_decls
	  , n_insts_in  = n_insts_in stats + n_insts
	  , n_rules_in  = n_rules_in stats + n_rules }
\end{code}

Names in a NameCache are always stored as a Global, and have the SrcLoc 
of their binding locations.

Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.

\begin{code}
-- | The NameCache makes sure that there is just one Unique assigned for
-- each original name; i.e. (module-name, occ-name) pair and provides
-- something of a lookup mechanism for those names.
data NameCache
 = NameCache {  nsUniqs :: UniqSupply,
		-- ^ Supply of uniques
		nsNames :: OrigNameCache,
		-- ^ Ensures that one original name gets one unique
		nsIPs   :: OrigIParamCache
		-- ^ Ensures that one implicit parameter name gets one unique
   }

-- | Per-module cache of original 'OccName's given 'Name's
type OrigNameCache   = ModuleEnv (OccEnv Name)

-- | Module-local cache of implicit parameter 'OccName's given 'Name's
type OrigIParamCache = Map (IPName OccName) (IPName Name)
\end{code}



%************************************************************************
%*									*
		The module graph and ModSummary type
	A ModSummary is a node in the compilation manager's
	dependency graph, and it's also passed to hscMain
%*									*
%************************************************************************

\begin{code}
-- | A ModuleGraph contains all the nodes from the home package (only).
-- There will be a node for each source module, plus a node for each hi-boot
-- module.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'Digraph.flattenSCC' to achieve this.
type ModuleGraph = [ModSummary]

emptyMG :: ModuleGraph
emptyMG = []

-- | A single node in a 'ModuleGraph. The nodes of the module graph are one of:
--
-- * A regular Haskell source module
--
-- * A hi-boot source module
--
-- * An external-core source module
data ModSummary
   = ModSummary {
        ms_mod          :: Module,		-- ^ Identity of the module
	ms_hsc_src      :: HscSource,		-- ^ The module source either plain Haskell, hs-boot or external core
        ms_location     :: ModLocation,		-- ^ Location of the various files belonging to the module
        ms_hs_date      :: ClockTime,		-- ^ Timestamp of source file
	ms_obj_date     :: Maybe ClockTime,	-- ^ Timestamp of object, if we have one
        ms_srcimps      :: [Located (ImportDecl RdrName)],	-- ^ Source imports of the module
        ms_textual_imps :: [Located (ImportDecl RdrName)],	-- ^ Non-source imports of the module from the module *text*
        ms_hspp_file    :: FilePath,		-- ^ Filename of preprocessed source file
        ms_hspp_opts    :: DynFlags,            -- ^ Cached flags from @OPTIONS@, @INCLUDE@
                                                -- and @LANGUAGE@ pragmas in the modules source code
	ms_hspp_buf     :: Maybe StringBuffer   -- ^ The actual preprocessed source, if we have it
     }

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

ms_imps :: ModSummary -> [Located (ImportDecl RdrName)]
ms_imps ms = ms_textual_imps ms ++ map mk_additional_import (dynFlagDependencies (ms_hspp_opts ms))
  where
    -- This is a not-entirely-satisfactory means of creating an import that corresponds to an
    -- import that did not occur in the program text, such as those induced by the use of
    -- plugins (the -plgFoo flag)
    mk_additional_import mod_nm = noLoc $ ImportDecl {
      ideclName = noLoc mod_nm,
      ideclPkgQual = Nothing,
      ideclSource = False,
      ideclQualified = False,
      ideclAs = Nothing,
      ideclHiding = Nothing,
      ideclSafe = False
    }

-- The ModLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just 
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.

-- The ModLocation is stable over successive up-sweeps in GHCi, wheres
-- the ms_hs_date and imports can, of course, change

msHsFilePath, msHiFilePath, msObjFilePath :: ModSummary -> FilePath
msHsFilePath  ms = expectJust "msHsFilePath" (ml_hs_file  (ms_location ms))
msHiFilePath  ms = ml_hi_file  (ms_location ms)
msObjFilePath ms = ml_obj_file (ms_location ms)

-- | Did this 'ModSummary' originate from a hs-boot file?
isBootSummary :: ModSummary -> Bool
isBootSummary ms = isHsBoot (ms_hsc_src ms)

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms) 
				<> text (hscSourceString (ms_hsc_src ms)) <> comma,
                          text "ms_textual_imps =" <+> ppr (ms_textual_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

showModMsg :: HscTarget -> Bool -> ModSummary -> String
showModMsg target recomp mod_summary
  = showSDoc $
        hsep [text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' '),
              char '(', text (normalise $ msHsFilePath mod_summary) <> comma,
              case target of
                  HscInterpreted | recomp 
                             -> text "interpreted"
                  HscNothing -> text "nothing"
                  _          -> text (normalise $ msObjFilePath mod_summary),
              char ')']
 where 
    mod     = moduleName (ms_mod mod_summary)
    mod_str = showSDoc (ppr mod) ++ hscSourceString (ms_hsc_src mod_summary)
\end{code}


%************************************************************************
%*									*
\subsection{Hpc Support}
%*									*
%************************************************************************

\begin{code}
-- | Information about a modules use of Haskell Program Coverage
data HpcInfo
  = HpcInfo 
     { hpcInfoTickCount :: Int
     , hpcInfoHash      :: Int
     }
  | NoHpcInfo 
     { hpcUsed          :: AnyHpcUsage  -- ^ Is hpc used anywhere on the module \*tree\*?
     }

-- | This is used to signal if one of my imports used HPC instrumentation
-- even if there is no module-local HPC usage
type AnyHpcUsage = Bool

emptyHpcInfo :: AnyHpcUsage -> HpcInfo
emptyHpcInfo = NoHpcInfo 

-- | Find out if HPC is used by this module or any of the modules
-- it depends upon
isHpcUsed :: HpcInfo -> AnyHpcUsage
isHpcUsed (HpcInfo {})     		 = True
isHpcUsed (NoHpcInfo { hpcUsed = used }) = used
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Vectorisation Support}
%*                                                                      *
%************************************************************************

The following information is generated and consumed by the vectorisation
subsystem.  It communicates the vectorisation status of declarations from one
module to another.

Why do we need both f and f_v in the ModGuts/ModDetails/EPS version VectInfo
below?  We need to know `f' when converting to IfaceVectInfo.  However, during
vectorisation, we need to know `f_v', whose `Var' we cannot lookup based
on just the OccName easily in a Core pass.

\begin{code}
-- |Vectorisation information for 'ModGuts', 'ModDetails' and 'ExternalPackageState'; see also
-- documentation at 'Vectorise.Env.GlobalEnv'.
data VectInfo      
  = VectInfo
    { vectInfoVar          :: VarEnv  (Var    , Var  )    -- ^ @(f, f_v)@ keyed on @f@
    , vectInfoTyCon        :: NameEnv (TyCon  , TyCon)    -- ^ @(T, T_v)@ keyed on @T@
    , vectInfoDataCon      :: NameEnv (DataCon, DataCon)  -- ^ @(C, C_v)@ keyed on @C@
    , vectInfoPADFun       :: NameEnv (TyCon  , Var)      -- ^ @(T_v, paT)@ keyed on @T_v@
    , vectInfoIso          :: NameEnv (TyCon  , Var)      -- ^ @(T, isoT)@ keyed on @T@
    , vectInfoScalarVars   :: VarSet                      -- ^ set of purely scalar variables
    , vectInfoScalarTyCons :: NameSet                     -- ^ set of scalar type constructors
    }

-- |Vectorisation information for 'ModIface'; i.e, the vectorisation information propagated 
-- across module boundaries.
--
data IfaceVectInfo 
  = IfaceVectInfo 
    { ifaceVectInfoVar          :: [Name]  -- ^ All variables in here have a vectorised variant
    , ifaceVectInfoTyCon        :: [Name]  -- ^ All 'TyCon's in here have a vectorised variant;
                                           -- the name of the vectorised variant and those of its
                                           -- data constructors are determined by
                                           -- 'OccName.mkVectTyConOcc' and 
                                           -- 'OccName.mkVectDataConOcc'; the names of the
                                           -- isomorphisms are determined by 'OccName.mkVectIsoOcc'
    , ifaceVectInfoTyConReuse   :: [Name]  -- ^ The vectorised form of all the 'TyCon's in here
                                           -- coincides with the unconverted form; the name of the
                                           -- isomorphisms is determined by 'OccName.mkVectIsoOcc'
    , ifaceVectInfoScalarVars   :: [Name]  -- iface version of 'vectInfoScalarVar'
    , ifaceVectInfoScalarTyCons :: [Name]  -- iface version of 'vectInfoScalarTyCon'
    }

noVectInfo :: VectInfo
noVectInfo 
  = VectInfo emptyVarEnv emptyNameEnv emptyNameEnv emptyNameEnv emptyNameEnv emptyVarSet
             emptyNameSet

plusVectInfo :: VectInfo -> VectInfo -> VectInfo
plusVectInfo vi1 vi2 = 
  VectInfo (vectInfoVar          vi1 `plusVarEnv`    vectInfoVar          vi2)
           (vectInfoTyCon        vi1 `plusNameEnv`   vectInfoTyCon        vi2)
           (vectInfoDataCon      vi1 `plusNameEnv`   vectInfoDataCon      vi2)
           (vectInfoPADFun       vi1 `plusNameEnv`   vectInfoPADFun       vi2)
           (vectInfoIso          vi1 `plusNameEnv`   vectInfoIso          vi2)
           (vectInfoScalarVars   vi1 `unionVarSet`   vectInfoScalarVars   vi2)
           (vectInfoScalarTyCons vi1 `unionNameSets` vectInfoScalarTyCons vi2)

concatVectInfo :: [VectInfo] -> VectInfo
concatVectInfo = foldr plusVectInfo noVectInfo

noIfaceVectInfo :: IfaceVectInfo
noIfaceVectInfo = IfaceVectInfo [] [] [] [] []
\end{code}

%************************************************************************
%*									*
\subsection{Safe Haskell Support}
%*									*
%************************************************************************

This stuff here is related to supporting the Safe Haskell extension,
primarily about storing under what trust type a module has been compiled.

\begin{code}
-- | Is an import a safe import?
type IsSafeImport = Bool

-- | Safe Haskell information for 'ModIface'
-- Simply a wrapper around SafeHaskellMode to sepperate iface and flags
newtype IfaceTrustInfo = TrustInfo SafeHaskellMode

getSafeMode :: IfaceTrustInfo -> SafeHaskellMode
getSafeMode (TrustInfo x) = x

setSafeMode :: SafeHaskellMode -> IfaceTrustInfo
setSafeMode = TrustInfo

noIfaceTrustInfo :: IfaceTrustInfo
noIfaceTrustInfo = setSafeMode Sf_None

trustInfoToNum :: IfaceTrustInfo -> Word8
trustInfoToNum it
  = case getSafeMode it of
            Sf_None -> 0
            Sf_SafeImports -> 1
            Sf_SafeLanguage -> 2
            Sf_Trustworthy -> 3
            Sf_TrustworthyWithSafeLanguage -> 4
            Sf_Safe -> 5

numToTrustInfo :: Word8 -> IfaceTrustInfo
numToTrustInfo 0 = setSafeMode Sf_None
numToTrustInfo 1 = setSafeMode Sf_SafeImports
numToTrustInfo 2 = setSafeMode Sf_SafeLanguage
numToTrustInfo 3 = setSafeMode Sf_Trustworthy
numToTrustInfo 4 = setSafeMode Sf_TrustworthyWithSafeLanguage
numToTrustInfo 5 = setSafeMode Sf_Safe
numToTrustInfo n = error $ "numToTrustInfo: bad input number! (" ++ show n ++ ")"

instance Outputable IfaceTrustInfo where
    ppr (TrustInfo Sf_None)         = ptext $ sLit "none"
    ppr (TrustInfo Sf_SafeImports)  = ptext $ sLit "safe-imports"
    ppr (TrustInfo Sf_SafeLanguage) = ptext $ sLit "safe-language"
    ppr (TrustInfo Sf_Trustworthy)  = ptext $ sLit "trustworthy"
    ppr (TrustInfo Sf_TrustworthyWithSafeLanguage)
                                    = ptext $ sLit "trustworthy + safe-language"
    ppr (TrustInfo Sf_Safe)         = ptext $ sLit "safe"
\end{code}

%************************************************************************
%*									*
\subsection{Linkable stuff}
%*									*
%************************************************************************

This stuff is in here, rather than (say) in Linker.lhs, because the Linker.lhs
stuff is the *dynamic* linker, and isn't present in a stage-1 compiler

\begin{code}
-- | Information we can use to dynamically link modules into the compiler
data Linkable = LM {
  linkableTime     :: ClockTime,	-- ^ Time at which this linkable was built
					-- (i.e. when the bytecodes were produced,
					--	 or the mod date on the files)
  linkableModule   :: Module,           -- ^ The linkable module itself
  linkableUnlinked :: [Unlinked]
    -- ^ Those files and chunks of code we have yet to link.
    --
    -- INVARIANT: A valid linkable always has at least one 'Unlinked' item.
    -- If this list is empty, the Linkable represents a fake linkable, which
    -- is generated in HscNothing mode to avoid recompiling modules.
    --
    -- XXX: Do items get removed from this list when they get linked?
 }

isObjectLinkable :: Linkable -> Bool
isObjectLinkable l = not (null unlinked) && all isObject unlinked
  where unlinked = linkableUnlinked l
	-- A linkable with no Unlinked's is treated as a BCO.  We can
	-- generate a linkable with no Unlinked's as a result of
	-- compiling a module in HscNothing mode, and this choice
	-- happens to work well with checkStability in module GHC.

linkableObjs :: Linkable -> [FilePath]
linkableObjs l = [ f | DotO f <- linkableUnlinked l ]

instance Outputable Linkable where
   ppr (LM when_made mod unlinkeds)
      = (text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod)
        $$ nest 3 (ppr unlinkeds)

-------------------------------------------

-- | Objects which have yet to be linked by the compiler
data Unlinked
   = DotO FilePath      -- ^ An object file (.o)
   | DotA FilePath      -- ^ Static archive file (.a)
   | DotDLL FilePath    -- ^ Dynamically linked library file (.so, .dll, .dylib)
   | BCOs CompiledByteCode ModBreaks    -- ^ A byte-code object, lives only in memory

#ifndef GHCI
data CompiledByteCode = CompiledByteCodeUndefined
_unused :: CompiledByteCode
_unused = CompiledByteCodeUndefined
#endif

instance Outputable Unlinked where
   ppr (DotO path)   = text "DotO" <+> text path
   ppr (DotA path)   = text "DotA" <+> text path
   ppr (DotDLL path) = text "DotDLL" <+> text path
#ifdef GHCI
   ppr (BCOs bcos _) = text "BCOs" <+> ppr bcos
#else
   ppr (BCOs _ _)    = text "No byte code"
#endif

-- | Is this an actual file on disk we can link in somehow?
isObject :: Unlinked -> Bool
isObject (DotO _)   = True
isObject (DotA _)   = True
isObject (DotDLL _) = True
isObject _          = False

-- | Is this a bytecode linkable with no file on disk?
isInterpretable :: Unlinked -> Bool
isInterpretable = not . isObject

-- | Retrieve the filename of the linkable if possible. Panic if it is a byte-code object
nameOfObject :: Unlinked -> FilePath
nameOfObject (DotO fn)   = fn
nameOfObject (DotA fn)   = fn
nameOfObject (DotDLL fn) = fn
nameOfObject other       = pprPanic "nameOfObject" (ppr other)

-- | Retrieve the compiled byte-code if possible. Panic if it is a file-based linkable
byteCodeOfObject :: Unlinked -> CompiledByteCode
byteCodeOfObject (BCOs bc _) = bc
byteCodeOfObject other       = pprPanic "byteCodeOfObject" (ppr other)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Breakpoint Support}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Breakpoint index
type BreakIndex = Int

-- | All the information about the breakpoints for a given module
data ModBreaks
   = ModBreaks
   { modBreaks_flags :: BreakArray
        -- ^ The array of flags, one per breakpoint, 
        -- indicating which breakpoints are enabled.
   , modBreaks_locs :: !(Array BreakIndex SrcSpan)
        -- ^ An array giving the source span of each breakpoint.
   , modBreaks_vars :: !(Array BreakIndex [OccName])
        -- ^ An array giving the names of the free variables at each breakpoint.
   , modBreaks_decls :: !(Array BreakIndex [String])
        -- ^ An array giving the names of the declarations enclosing each breakpoint.
   }

emptyModBreaks :: ModBreaks
emptyModBreaks = ModBreaks
   { modBreaks_flags = error "ModBreaks.modBreaks_array not initialised"
         -- Todo: can we avoid this? 
   , modBreaks_locs  = array (0,-1) []
   , modBreaks_vars  = array (0,-1) []
   , modBreaks_decls = array (0,-1) []
   }
\end{code}
