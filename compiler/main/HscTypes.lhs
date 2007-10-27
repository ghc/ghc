%
% (c) The University of Glasgow, 2006
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes ( 
	-- * Sessions and compilation state
	Session(..), withSession, modifySession, 
        HscEnv(..), hscEPS,
	FinderCache, FindResult(..), ModLocationCache,
	Target(..), TargetId(..), pprTarget, pprTargetId,
	ModuleGraph, emptyMG,

	ModDetails(..),	emptyModDetails,
	ModGuts(..), CoreModule(..), CgGuts(..), ModImports(..), ForeignStubs(..),

	ModSummary(..), ms_mod_name, showModMsg, isBootSummary,
	msHsFilePath, msHiFilePath, msObjFilePath, 

	HscSource(..), isHsBoot, hscSourceString,	-- Re-exported from DriverPhases
	
	HomePackageTable, HomeModInfo(..), emptyHomePackageTable,
	hptInstances, hptRules, hptVectInfo,

	ExternalPackageState(..), EpsStats(..), addEpsInStats,
	PackageTypeEnv, PackageIfaceTable, emptyPackageIfaceTable,
	lookupIfaceByModule, emptyModIface,

	InteractiveContext(..), emptyInteractiveContext, 
	icPrintUnqual, mkPrintUnqualified, extendInteractiveContext,
        substInteractiveContext,

	ModIface(..), mkIfaceDepCache, mkIfaceVerCache, mkIfaceFixCache,
	emptyIfaceDepCache,

	FixityEnv, FixItem(..), lookupFixity, emptyFixityEnv,

	implicitTyThings, isImplicitTyThing,

	TyThing(..), tyThingClass, tyThingTyCon, tyThingDataCon, tyThingId,
	TypeEnv, lookupType, mkTypeEnv, emptyTypeEnv,
	extendTypeEnv, extendTypeEnvList, extendTypeEnvWithIds, lookupTypeEnv,
	typeEnvElts, typeEnvClasses, typeEnvTyCons, typeEnvIds,
	typeEnvDataCons,

	WhetherHasOrphans, IsBootInterface, Usage(..), 
	Dependencies(..), noDependencies,
	NameCache(..), OrigNameCache, OrigIParamCache,
	Avails, availsToNameSet, availsToNameEnv, availName, availNames,
	GenAvailInfo(..), AvailInfo, RdrAvailInfo, 
	IfaceExport,

	Deprecations(..), DeprecTxt, plusDeprecs,

	PackageInstEnv, PackageRuleBase,

	-- Linker stuff
	Linkable(..), isObjectLinkable,
	Unlinked(..), CompiledByteCode,
	isObject, nameOfObject, isInterpretable, byteCodeOfObject,
        HpcInfo(..), emptyHpcInfo, isHpcUsed, AnyHpcUsage,

        -- Breakpoints
        ModBreaks (..), BreakIndex, emptyModBreaks,

        -- Vectorisation information
        VectInfo(..), IfaceVectInfo(..), noVectInfo, plusVectInfo, 
        noIfaceVectInfo
    ) where

#include "HsVersions.h"

#ifdef GHCI
import ByteCodeAsm	( CompiledByteCode )
import {-# SOURCE #-}  InteractiveEval ( Resume )
#endif

import RdrName
import Name		( Name, NamedThing, getName, nameOccName, nameModule )
import NameEnv
import NameSet	
import OccName		( OccName, OccEnv, lookupOccEnv, mkOccEnv, emptyOccEnv, 
			  extendOccEnv )
import Module
import InstEnv		( InstEnv, Instance )
import FamInstEnv	( FamInstEnv, FamInst )
import Rules		( RuleBase )
import CoreSyn		( CoreBind )
import VarEnv
import VarSet
import Var       hiding ( setIdType )
import Id
import Type		

import Class		( Class, classSelIds, classATs, classTyCon )
import TyCon
import DataCon		( DataCon, dataConImplicitIds )
import PrelNames	( gHC_PRIM )
import Packages hiding ( Version(..) )
import DynFlags		( DynFlags(..), isOneShot, HscTarget (..) )
import DriverPhases	( HscSource(..), isHsBoot, hscSourceString, Phase )
import BasicTypes	( Version, initialVersion, IPName, 
			  Fixity, defaultFixity, DeprecTxt )
import IfaceSyn
import FiniteMap	( FiniteMap )
import CoreSyn		( CoreRule )
import Maybes		( orElse, expectJust, catMaybes, seqMaybe )
import Outputable
import BreakArray
import SrcLoc		( SrcSpan, Located )
import UniqFM		( lookupUFM, eltsUFM, emptyUFM )
import UniqSupply	( UniqSupply )
import FastString	( FastString )
import StringBuffer	( StringBuffer )

import System.Time	( ClockTime )
import Data.IORef
import Data.Array       ( Array, array )
import Data.List
\end{code}


%************************************************************************
%*									*
\subsection{Compilation environment}
%*									*
%************************************************************************


\begin{code}
-- | The Session is a handle to the complete state of a compilation
-- session.  A compilation session consists of a set of modules
-- constituting the current program or library, the context for
-- interactive evaluation, and various caches.
newtype Session = Session (IORef HscEnv)

withSession :: Session -> (HscEnv -> IO a) -> IO a
withSession (Session ref) f = do h <- readIORef ref; f h

modifySession :: Session -> (HscEnv -> HscEnv) -> IO ()
modifySession (Session ref) f = do h <- readIORef ref; writeIORef ref $! f h
\end{code}

HscEnv is like Session, except that some of the fields are immutable.
An HscEnv is used to compile a single module from plain Haskell source
code (after preprocessing) to either C, assembly or C--.  Things like
the module graph don't change during a single compilation.

Historical note: "hsc" used to be the name of the compiler binary,
when there was a separate driver and compiler.  To compile a single
module, the driver would invoke hsc on the source code... so nowadays
we think of hsc as the layer of the compiler that deals with compiling
a single module.

\begin{code}
data HscEnv 
  = HscEnv { 
	hsc_dflags :: DynFlags,
		-- The dynamic flag settings

	hsc_targets :: [Target],
		-- The targets (or roots) of the current session

	hsc_mod_graph :: ModuleGraph,
		-- The module graph of the current session

	hsc_IC :: InteractiveContext,
		-- The context for evaluating interactive statements

	hsc_HPT    :: HomePackageTable,
		-- The home package table describes already-compiled
		-- home-packge modules, *excluding* the module we 
		-- are compiling right now.
		-- (In one-shot mode the current module is the only
		--  home-package module, so hsc_HPT is empty.  All other
		--  modules count as "external-package" modules.
		--  However, even in GHCi mode, hi-boot interfaces are
		--  demand-loadeded into the external-package table.)
		--
		-- hsc_HPT is not mutable because we only demand-load 
		-- external packages; the home package is eagerly 
		-- loaded, module by module, by the compilation manager.
		--	
		-- The HPT may contain modules compiled earlier by --make
		-- but not actually below the current module in the dependency
		-- graph.  (This changes a previous invariant: changed Jan 05.)
	
	hsc_EPS	:: {-# UNPACK #-} !(IORef ExternalPackageState),
	hsc_NC	:: {-# UNPACK #-} !(IORef NameCache),
		-- These are side-effected by compiling to reflect
		-- sucking in interface files.  They cache the state of
		-- external interface files, in effect.

	hsc_FC   :: {-# UNPACK #-} !(IORef FinderCache),
	hsc_MLC  :: {-# UNPACK #-} !(IORef ModLocationCache),
		-- The finder's cache.  This caches the location of modules,
		-- so we don't have to search the filesystem multiple times.

        hsc_global_rdr_env :: GlobalRdrEnv,
        hsc_global_type_env :: TypeEnv
 }

hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

-- | A compilation target.
--
-- A target may be supplied with the actual text of the
-- module.  If so, use this instead of the file contents (this
-- is for use in an IDE where the file hasn't been saved by
-- the user yet).
data Target = Target TargetId (Maybe (StringBuffer,ClockTime))

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
pprTarget (Target id _) = pprTargetId id

instance Outputable Target where
    ppr = pprTarget

pprTargetId :: TargetId -> SDoc
pprTargetId (TargetModule m) = ppr m
pprTargetId (TargetFile f _) = text f

instance Outputable TargetId where
    ppr = pprTargetId

type HomePackageTable  = ModuleNameEnv HomeModInfo
	-- Domain = modules in the home package
	-- "home" package name cached here for convenience
type PackageIfaceTable = ModuleEnv ModIface
	-- Domain = modules in the imported packages

emptyHomePackageTable :: HomePackageTable
emptyHomePackageTable  = emptyUFM

emptyPackageIfaceTable :: PackageIfaceTable
emptyPackageIfaceTable = emptyModuleEnv

data HomeModInfo 
  = HomeModInfo { hm_iface    :: !ModIface,
		  hm_details  :: !ModDetails,
		  hm_linkable :: !(Maybe Linkable) }
		-- hm_linkable might be Nothing if:
		--   a) this is an .hs-boot module
		--   b) temporarily during compilation if we pruned away
		--      the old linkable because it was out of date.
		-- after a complete compilation (GHC.load), all hm_linkable
		-- fields in the HPT will be Just.
		--
		-- When re-linking a module (hscNoRecomp), we construct
		-- the HomModInfo by building a new ModDetails from the
		-- old ModIface (only).

-- | Find the 'ModIface' for a 'Module'
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
    `seqMaybe` lookupModuleEnv pit mod

  | otherwise = lookupModuleEnv pit mod		-- Look in PIT only 

-- If the module does come from the home package, why do we look in the PIT as well?
-- (a) In OneShot mode, even home-package modules accumulate in the PIT
-- (b) Even in Batch (--make) mode, there is *one* case where a home-package
--     module is in the PIT, namely GHC.Prim when compiling the base package.
-- We could eliminate (b) if we wanted, by making GHC.Prim belong to a packake
-- of its own, but it doesn't seem worth the bother.
\end{code}


\begin{code}
hptInstances :: HscEnv -> (ModuleName -> Bool) -> ([Instance], [FamInst])
-- Find all the instance declarations (of classes and families) that are in
-- modules imported by this one, directly or indirectly, and are in the Home
-- Package Table.  This ensures that we don't see instances from modules --make
-- compiled before this one, but which are not below this one.
hptInstances hsc_env want_this_module
  = let (insts, famInsts) = unzip
          [ (md_insts details, md_fam_insts details)
          | mod_info <- eltsUFM (hsc_HPT hsc_env)
          , want_this_module (moduleName (mi_module (hm_iface mod_info)))
          , let details = hm_details mod_info ]
    in
    (concat insts, concat famInsts)

hptRules :: HscEnv -> [(ModuleName, IsBootInterface)] -> [CoreRule]
-- Get rules from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptRules hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []
  | otherwise
  = let 
	hpt = hsc_HPT hsc_env
    in
    [ rule
    |	-- Find each non-hi-boot module below me
      (mod, False) <- deps

	-- unsavoury: when compiling the base package with --make, we
	-- sometimes try to look up RULES for GHC.Prim.  GHC.Prim won't
	-- be in the HPT, because we never compile it; it's in the EPT
	-- instead.  ToDo: clean up, and remove this slightly bogus
	-- filter:
    , mod /= moduleName gHC_PRIM

	-- Look it up in the HPT
    , let rules = case lookupUFM hpt mod of
		    Just info -> md_rules (hm_details info)
		    Nothing -> pprTrace "WARNING in hptRules" msg [] 
	  msg = vcat [ptext SLIT("missing module") <+> ppr mod,
		      ptext SLIT("Probable cause: out-of-date interface files")]
			-- This really shouldn't happen, but see Trac #962

	-- And get its dfuns
    , rule <- rules ]

hptVectInfo :: HscEnv -> VectInfo
-- Get the combined VectInfo of all modules in the home package table.  In
-- contrast to instances and rules, we don't care whether the modules are
-- "below" or us.  The VectInfo of those modules not "below" us does not
-- affect the compilation of the current module.
hptVectInfo hsc_env 
  = foldr plusVectInfo noVectInfo [ md_vect_info (hm_details mod_info)
                                  | mod_info <- eltsUFM (hsc_HPT hsc_env)]
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
	-- the module was found
  | NoPackage PackageId
	-- the requested package was not found
  | FoundMultiple [PackageId]
	-- *error*: both in multiple packages
  | PackageHidden PackageId
	-- for an explicit source import: the package containing the module is
	-- not exposed.
  | ModuleHidden  PackageId
	-- for an explicit source import: the package containing the module is
	-- exposed, but the module itself is hidden.
  | NotFound [FilePath] (Maybe PackageId)
	-- the module was not found, the specified places were searched
  | NotFoundInPackage PackageId
	-- the module was not found in this package

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

A @ModIface@ plus a @ModDetails@ summarises everything we know 
about a compiled module.  The @ModIface@ is the stuff *before* linking,
and can be written out to an interface file.  (The @ModDetails@ is after 
linking; it is the "linked" form of the mi_decls field.)

When we *read* an interface file, we also construct a @ModIface@ from it,
except that the mi_decls part is empty; when reading we consolidate
the declarations into a single indexed map in the @PersistentRenamerState@.

\begin{code}
data ModIface 
   = ModIface {
        mi_module   :: !Module,
        mi_mod_vers :: !Version,	    -- Module version: changes when anything changes

        mi_orphan   :: !WhetherHasOrphans,  -- Whether this module has orphans
        mi_finsts   :: !WhetherHasFamInst,  -- Whether module has family insts
	mi_boot	    :: !IsBootInterface,    -- Read from an hi-boot file?

	mi_deps	    :: Dependencies,
		-- This is consulted for directly-imported modules,
		-- but not for anything else (hence lazy)

		-- Usages; kept sorted so that it's easy to decide
		-- whether to write a new iface file (changing usages
		-- doesn't affect the version of this module)
        mi_usages   :: [Usage],
		-- NOT STRICT!  we read this field lazily from the interface file
		-- It is *only* consulted by the recompilation checker

		-- Exports
		-- Kept sorted by (mod,occ), to make version comparisons easier
        mi_exports  :: ![IfaceExport],
        mi_exp_vers :: !Version,	-- Version number of export list

		-- Fixities
        mi_fixities :: [(OccName,Fixity)],
		-- NOT STRICT!  we read this field lazily from the interface file

		-- Deprecations
	mi_deprecs  :: Deprecations,
		-- NOT STRICT!  we read this field lazily from the interface file

		-- Type, class and variable declarations
		-- The version of an Id changes if its fixity or deprecations change
		--	(as well as its type of course)
		-- Ditto data constructors, class operations, except that 
		-- the version of the parent class/tycon changes
	mi_decls :: [(Version,IfaceDecl)],	-- Sorted

        mi_globals  :: !(Maybe GlobalRdrEnv),
		-- Binds all the things defined at the top level in
		-- the *original source* code for this module. which
		-- is NOT the same as mi_exports, nor mi_decls (which
		-- may contains declarations for things not actually
		-- defined by the user).  Used for GHCi and for inspecting
		-- the contents of modules via the GHC API only.
		--
		-- (We need the source file to figure out the
		-- top-level environment, if we didn't compile this module
		-- from source then this field contains Nothing).
		--
		-- Strictly speaking this field should live in the
		-- HomeModInfo, but that leads to more plumbing.

		-- Instance declarations and rules
	mi_insts     :: [IfaceInst],			-- Sorted
	mi_fam_insts :: [IfaceFamInst],			-- Sorted
	mi_rules     :: [IfaceRule],			-- Sorted
	mi_rule_vers :: !Version,	-- Version number for rules and 
					-- instances (for classes and families)
					-- combined

                -- Vectorisation information
        mi_vect_info :: !IfaceVectInfo,

		-- Cached environments for easy lookup
		-- These are computed (lazily) from other fields
		-- and are not put into the interface file
	mi_dep_fn  :: Name -> Maybe DeprecTxt,	-- Cached lookup for mi_deprecs
	mi_fix_fn  :: OccName -> Fixity,	-- Cached lookup for mi_fixities
	mi_ver_fn  :: OccName -> Maybe (OccName, Version),
                        -- Cached lookup for mi_decls
			-- The Nothing in mi_ver_fn means that the thing
			-- isn't in decls. It's useful to know that when
			-- seeing if we are up to date wrt the old interface
                        -- The 'OccName' is the parent of the name, if it has one.
	mi_hpc    :: !AnyHpcUsage
	  -- True if this program uses Hpc at any point in the program.
     }

-- Should be able to construct ModDetails from mi_decls in ModIface
data ModDetails
   = ModDetails {
	-- The next two fields are created by the typechecker
	md_exports   :: [AvailInfo],
        md_types     :: !TypeEnv,
        md_insts     :: ![Instance],  -- Dfun-ids for the instances in this module
        md_fam_insts :: ![FamInst],
        md_rules     :: ![CoreRule],  -- Domain may include Ids from other modules
        md_vect_info :: !VectInfo     -- Vectorisation information
     }

emptyModDetails :: ModDetails
emptyModDetails = ModDetails { md_types = emptyTypeEnv,
			       md_exports = [],
			       md_insts     = [],
			       md_rules     = [],
			       md_fam_insts = [],
                               md_vect_info = noVectInfo
                             } 

-- A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a ModIface and 
-- ModDetails are extracted and the ModGuts is dicarded.

data ModGuts
  = ModGuts {
        mg_module    :: !Module,
	mg_boot      :: IsBootInterface, -- Whether it's an hs-boot module
	mg_exports   :: ![AvailInfo],	 -- What it exports
	mg_deps	     :: !Dependencies,	 -- What is below it, directly or
					 --   otherwise 
	mg_dir_imps  :: ![Module],	 -- Directly-imported modules; used to
					 --	generate initialisation code
	mg_usages    :: ![Usage],	 -- Version info for what it needed

        mg_rdr_env   :: !GlobalRdrEnv,	 -- Top-level lexical environment

	-- These fields all describe the things **declared in this module**
	mg_fix_env   :: !FixityEnv,	 -- Fixities
	mg_types     :: !TypeEnv,
	mg_insts     :: ![Instance],	 -- Instances 
	mg_fam_insts :: ![FamInst],	 -- Instances 
        mg_rules     :: ![CoreRule],	 -- Rules from this module
	mg_binds     :: ![CoreBind],	 -- Bindings for this module
	mg_foreign   :: !ForeignStubs,
	mg_deprecs   :: !Deprecations,	 -- Deprecations declared in the module
	mg_hpc_info  :: !HpcInfo,        -- info about coverage tick boxes
        mg_modBreaks :: !ModBreaks,
        mg_vect_info :: !VectInfo,        -- Pool of vectorised declarations

	-- The next two fields are unusual, because they give instance
	-- environments for *all* modules in the home package, including
	-- this module, rather than for *just* this module.  
	-- Reason: when looking up an instance we don't want to have to
	--	  look at each module in the home package in turn
	mg_inst_env     :: InstEnv,	 -- Class instance enviroment fro
					 -- *home-package* modules (including
					 -- this one); c.f. tcg_inst_env
	mg_fam_inst_env :: FamInstEnv	 -- Type-family instance enviroment
					 -- for *home-package* modules (including
					 -- this one); c.f. tcg_fam_inst_env
    }

-- A CoreModule consists of just the fields of a ModGuts that are needed for
-- the compileToCoreModule interface.
data CoreModule
  = CoreModule {
      -- Module name
      cm_module   :: !Module,
      -- Type environment for types declared in this module
      cm_types    :: !TypeEnv,
      -- Declarations
      cm_binds    :: [CoreBind]
    }

instance Outputable CoreModule where
   ppr (CoreModule {cm_module = mn, cm_types = te, cm_binds = cb}) =
      text "%module" <+> ppr mn <+> ppr te $$ vcat (map ppr cb)

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
data CgGuts 
  = CgGuts {
	cg_module   :: !Module,

	cg_tycons   :: [TyCon],
		-- Algebraic data types (including ones that started
		-- life as classes); generate constructors and info
		-- tables Includes newtypes, just for the benefit of
		-- External Core

	cg_binds    :: [CoreBind],
		-- The tidied main bindings, including
		-- previously-implicit bindings for record and class
		-- selectors, and data construtor wrappers.  But *not*
		-- data constructor workers; reason: we we regard them
		-- as part of the code-gen of tycons

	cg_dir_imps :: ![Module],
		-- Directly-imported modules; used to generate
		-- initialisation code

	cg_foreign  :: !ForeignStubs,	
	cg_dep_pkgs :: ![PackageId],	-- Used to generate #includes for C code gen
        cg_hpc_info :: !HpcInfo,         -- info about coverage tick boxes
        cg_modBreaks :: !ModBreaks
    }

-----------------------------------
data ModImports
  = ModImports {
	imp_direct     :: ![(Module,Bool)],	-- Explicitly-imported modules
						-- Boolean is true if we imported the whole
						--	module (apart, perhaps, from hiding some)
	imp_pkg_mods   :: !ModuleSet,		-- Non-home-package modules on which we depend,
						--	directly or indirectly
	imp_home_names :: !NameSet		-- Home package things on which we depend,
						--	directly or indirectly
    }

-----------------------------------
data ForeignStubs = NoStubs
		  | ForeignStubs
			SDoc 		-- Header file prototypes for
                                      	-- 	"foreign exported" functions
			SDoc 		-- C stubs to use when calling
                                        -- 	"foreign exported" functions
			[FastString] 	-- Headers that need to be included
				        -- 	into C code generated for this module

\end{code}

\begin{code}
emptyModIface :: Module -> ModIface
emptyModIface mod
  = ModIface { mi_module   = mod,
	       mi_mod_vers = initialVersion,
	       mi_orphan   = False,
	       mi_finsts   = False,
	       mi_boot	   = False,
	       mi_deps     = noDependencies,
	       mi_usages   = [],
	       mi_exports  = [],
	       mi_exp_vers = initialVersion,
	       mi_fixities = [],
	       mi_deprecs  = NoDeprecs,
	       mi_insts     = [],
	       mi_fam_insts = [],
	       mi_rules     = [],
	       mi_decls     = [],
	       mi_globals   = Nothing,
	       mi_rule_vers = initialVersion,
               mi_vect_info = noIfaceVectInfo,
	       mi_dep_fn = emptyIfaceDepCache,
	       mi_fix_fn = emptyIfaceFixCache,
	       mi_ver_fn = emptyIfaceVerCache,
	       mi_hpc    = False
    }		
\end{code}


%************************************************************************
%*									*
\subsection{The interactive context}
%*									*
%************************************************************************

\begin{code}
data InteractiveContext 
  = InteractiveContext { 
	ic_toplev_scope :: [Module],	-- Include the "top-level" scope of
					-- these modules

	ic_exports :: [Module],		-- Include just the exports of these
					-- modules

	ic_rn_gbl_env :: GlobalRdrEnv,	-- The cached GlobalRdrEnv, built from
					-- ic_toplev_scope and ic_exports

	ic_tmp_ids :: [Id],             -- Names bound during interaction.
                                        -- Later Ids shadow
                                        -- earlier ones with the same OccName.

        ic_tyvars :: TyVarSet           -- skolem type variables free in
                                        -- ic_tmp_ids.  These arise at
                                        -- breakpoints in a polymorphic 
                                        -- context, where we have only partial
                                        -- type information.

#ifdef GHCI
        , ic_resume :: [Resume]         -- the stack of breakpoint contexts
#endif
    }


emptyInteractiveContext :: InteractiveContext
emptyInteractiveContext
  = InteractiveContext { ic_toplev_scope = [],
			 ic_exports = [],
			 ic_rn_gbl_env = emptyGlobalRdrEnv,
			 ic_tmp_ids = [],
                         ic_tyvars = emptyVarSet
#ifdef GHCI
                         , ic_resume = []
#endif
                       }

icPrintUnqual :: DynFlags -> InteractiveContext -> PrintUnqualified
icPrintUnqual dflags ictxt = mkPrintUnqualified dflags (ic_rn_gbl_env ictxt)


extendInteractiveContext
        :: InteractiveContext
        -> [Id]
        -> TyVarSet
        -> InteractiveContext
extendInteractiveContext ictxt ids tyvars
  = ictxt { ic_tmp_ids =  ic_tmp_ids ictxt ++ ids,
                          -- NB. must be this way around, because we want
                          -- new ids to shadow existing bindings.
            ic_tyvars   = ic_tyvars ictxt `unionVarSet` tyvars }


substInteractiveContext :: InteractiveContext -> TvSubst -> InteractiveContext
substInteractiveContext ictxt subst | isEmptyTvSubst subst = ictxt
substInteractiveContext ictxt@InteractiveContext{ic_tmp_ids=ids} subst =
   let ids'     = map (\id -> id `setIdType` substTy subst (idType id)) ids
       subst_dom= varEnvKeys$ getTvSubstEnv subst
       subst_ran= varEnvElts$ getTvSubstEnv subst
       new_tvs  = [ tv | Just tv <- map getTyVar_maybe subst_ran]  
       ic_tyvars'= (`delVarSetListByKey` subst_dom) 
                 . (`extendVarSetList`   new_tvs)
                   $ ic_tyvars ictxt
    in ictxt { ic_tmp_ids = ids'
             , ic_tyvars   = ic_tyvars' }

          where delVarSetListByKey = foldl' delVarSetByKey
\end{code}

%************************************************************************
%*									*
        Building a PrintUnqualified		
%*									*
%************************************************************************

Deciding how to print names is pretty tricky.  We are given a name
P:M.T, where P is the package name, M is the defining module, and T is
the occurrence name, and we have to decide in which form to display
the name given a GlobalRdrEnv describing the current scope.

Ideally we want to display the name in the form in which it is in
scope.  However, the name might not be in scope at all, and that's
where it gets tricky.  Here are the cases:

 1. T   uniquely maps to  P:M.T                         --->  "T"
 2. there is an X for which X.T uniquely maps to  P:M.T --->  "X.T"
 3. there is no binding for "M.T"                       --->  "M.T"
 4. otherwise                                           --->  "P:M.T"

3 and 4 apply when P:M.T is not in scope.  In these cases we want to
refer to the name as "M.T", but "M.T" might mean something else in the
current scope (e.g. if there's an "import X as M"), so to avoid
confusion we avoid using "M.T" if there's already a binding for it.

There's one further subtlety: if the module M cannot be imported
because it is not exposed by any package, then we must refer to it as
"P:M".  This is handled by the qual_mod component of PrintUnqualified.

\begin{code}
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
	right_name gre = nameModule (gre_name gre) == mod

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
-- N.B. the set of TyThings returned here *must* match the set of
-- names returned by LoadIface.ifaceDeclSubBndrs, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in LoadIface.loadDecl (see note [Tricky iface loop])
-- The order of the list does not matter.
implicitTyThings :: TyThing -> [TyThing]

-- For data and newtype declarations:
implicitTyThings (ATyCon tc) = 
    -- fields (names of selectors)
    map AnId (tyConSelIds tc) ++ 
    -- (possibly) implicit coercion and family coercion
    --   depending on whether it's a newtype or a family instance or both
    implicitCoTyCon tc ++
    -- for each data constructor in order,
    --   the contructor, worker, and (possibly) wrapper
    concatMap (extras_plus . ADataCon) (tyConDataCons tc)
		     
implicitTyThings (AClass cl) 
  = -- dictionary datatype:
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
    map AnId (classSelIds cl)

implicitTyThings (ADataCon dc) = 
    -- For data cons add the worker and (possibly) wrapper
    map AnId (dataConImplicitIds dc)

implicitTyThings (AnId _)   = []

-- add a thing and recursive call
extras_plus :: TyThing -> [TyThing]
extras_plus thing = thing : implicitTyThings thing

-- For newtypes and indexed data types (and both),
-- add the implicit coercion tycon
implicitCoTyCon :: TyCon -> [TyThing]
implicitCoTyCon tc 
  = map ATyCon . catMaybes $ [-- Just if newtype, Nothing if not
                              newTyConCo_maybe tc, 
                              -- Just if family instance, Nothing if not
			        tyConFamilyCoercion_maybe tc] 

-- sortByOcc = sortBy (\ x -> \ y -> getOccName x < getOccName y)


-- | returns 'True' if there should be no interface-file declaration
-- for this thing on its own: either it is built-in, or it is part
-- of some other declaration, or it is generated implicitly by some
-- other declaration.
isImplicitTyThing :: TyThing -> Bool
isImplicitTyThing (ADataCon _)  = True
isImplicitTyThing (AnId     id) = isImplicitId id
isImplicitTyThing (AClass   _)  = False
isImplicitTyThing (ATyCon   tc) = isImplicitTyCon tc

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
type TypeEnv = NameEnv TyThing

emptyTypeEnv    :: TypeEnv
typeEnvElts     :: TypeEnv -> [TyThing]
typeEnvClasses  :: TypeEnv -> [Class]
typeEnvTyCons   :: TypeEnv -> [TyCon]
typeEnvIds      :: TypeEnv -> [Id]
typeEnvDataCons :: TypeEnv -> [DataCon]
lookupTypeEnv   :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv 	    = emptyNameEnv
typeEnvElts     env = nameEnvElts env
typeEnvClasses  env = [cl | AClass cl   <- typeEnvElts env]
typeEnvTyCons   env = [tc | ATyCon tc   <- typeEnvElts env] 
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
  where mod = nameModule name
	this_pkg = thisPackage dflags
\end{code}

\begin{code}
tyThingTyCon :: TyThing -> TyCon
tyThingTyCon (ATyCon tc) = tc
tyThingTyCon other	 = pprPanic "tyThingTyCon" (pprTyThing other)

tyThingClass :: TyThing -> Class
tyThingClass (AClass cls) = cls
tyThingClass other	  = pprPanic "tyThingClass" (pprTyThing other)

tyThingDataCon :: TyThing -> DataCon
tyThingDataCon (ADataCon dc) = dc
tyThingDataCon other	     = pprPanic "tyThingDataCon" (pprTyThing other)

tyThingId :: TyThing -> Id
tyThingId (AnId id) = id
tyThingId other     = pprPanic "tyThingId" (pprTyThing other)
\end{code}

%************************************************************************
%*									*
\subsection{Auxiliary types}
%*									*
%************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere

\begin{code}
mkIfaceVerCache :: [(Version,IfaceDecl)]
                -> (OccName -> Maybe (OccName, Version))
mkIfaceVerCache pairs 
  = \occ -> lookupOccEnv env occ
  where
    env = foldr add_decl emptyOccEnv pairs
    add_decl (v,d) env0 = foldr add_imp env1 (ifaceDeclSubBndrs d)
      where
          decl_name = ifName d
          env1 = extendOccEnv env0 decl_name (decl_name, v)
          add_imp bndr env = extendOccEnv env bndr (decl_name, v)

emptyIfaceVerCache :: OccName -> Maybe (OccName, Version)
emptyIfaceVerCache _occ = Nothing

------------------ Deprecations -------------------------
data Deprecations
  = NoDeprecs
  | DeprecAll DeprecTxt	        -- Whole module deprecated
  | DeprecSome [(OccName,DeprecTxt)] -- Some specific things deprecated
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

mkIfaceDepCache :: Deprecations -> Name -> Maybe DeprecTxt
mkIfaceDepCache NoDeprecs     	  = \_ -> Nothing
mkIfaceDepCache (DeprecAll t) 	  = \_ -> Just t
mkIfaceDepCache (DeprecSome pairs) = lookupOccEnv (mkOccEnv pairs) . nameOccName

emptyIfaceDepCache :: Name -> Maybe DeprecTxt
emptyIfaceDepCache _ = Nothing

plusDeprecs :: Deprecations -> Deprecations -> Deprecations
plusDeprecs d NoDeprecs = d
plusDeprecs NoDeprecs d = d
plusDeprecs _ (DeprecAll t) = DeprecAll t
plusDeprecs (DeprecAll t) _ = DeprecAll t
plusDeprecs (DeprecSome v1) (DeprecSome v2) = DeprecSome (v1 ++ v2)
\end{code}


\begin{code}
type Avails	  = [AvailInfo]
type AvailInfo    = GenAvailInfo Name
type RdrAvailInfo = GenAvailInfo OccName

data GenAvailInfo name	= Avail name	 -- An ordinary identifier
			| AvailTC name 	 -- The name of the type or class
				  [name] -- The available pieces of type/class.
					 -- NB: If the type or class is itself
					 -- to be in scope, it must be in this list.
					 -- Thus, typically: AvailTC Eq [Eq, ==, /=]
			deriving( Eq )
			-- Equality used when deciding if the interface has changed

type IfaceExport = (Module, [GenAvailInfo OccName])

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = addListToNameSet set (availNames avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

availName :: GenAvailInfo name -> name
availName (Avail n)     = n
availName (AvailTC n _) = n

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
mkIfaceFixCache :: [(OccName, Fixity)] -> OccName -> Fixity
mkIfaceFixCache pairs 
  = \n -> lookupOccEnv env n `orElse` defaultFixity
  where
   env = mkOccEnv pairs

emptyIfaceFixCache :: OccName -> Fixity
emptyIfaceFixCache _ = defaultFixity

-- This fixity environment is for source code only
type FixityEnv = NameEnv FixItem

-- We keep the OccName in the range so that we can generate an interface from it
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
type WhetherHasOrphans   = Bool
	-- An "orphan" is 
	-- 	* an instance decl in a module other than the defn module for 
	--		one of the tycons or classes in the instance head
	--	* a transformation rule in a module other than the one defining
	--		the function in the head of the rule.

type WhetherHasFamInst = Bool	     -- This module defines family instances?

type IsBootInterface = Bool

-- Dependency info about modules and packages below this one
-- in the import hierarchy.  See TcRnTypes.ImportAvails for details.
-- The orphan modules in `dep_orphs' do *not* include family instance orphans,
-- as they are anyway included in `dep_finsts'.
--
-- Invariant: the dependencies of a module M never includes M
-- Invariant: the lists are unordered, with no duplicates
data Dependencies
  = Deps { dep_mods   :: [(ModuleName,      -- Home-package module dependencies
			   IsBootInterface)]
	 , dep_pkgs   :: [PackageId] 	    -- External package dependencies
	 , dep_orphs  :: [Module]	    -- Orphan modules (whether home or
					    -- external pkg)
         , dep_finsts :: [Module]	    -- Modules that contain family
					    -- instances (whether home or
					    -- external pkg)
         }
  deriving( Eq )
	-- Equality used only for old/new comparison in MkIface.addVersionInfo

noDependencies :: Dependencies
noDependencies = Deps [] [] [] []
 	  
data Usage
  = Usage { usg_name     :: ModuleName,			-- Name of the module
	    usg_mod      :: Version,			-- Module version
	    usg_entities :: [(OccName,Version)],	-- Sorted by occurrence name
                -- NB. usages are for parent names only, eg. tycon but not constructors.
	    usg_exports  :: Maybe Version,		-- Export-list version, if we depend on it
	    usg_rules    :: Version 			-- Orphan-rules version (for non-orphan
							-- modules this will always be initialVersion)
    }	    deriving( Eq )
	-- This type doesn't let you say "I imported f but none of the rules in
	-- the module". If you use anything in the module you get its rule version
	-- So if the rules change, you'll recompile, even if you don't use them.
	-- This is easy to implement, and it's safer: you might not have used the rules last
	-- time round, but if someone has added a new rule you might need it this time

	-- The export list field is (Just v) if we depend on the export list:
	--	i.e. we imported the module directly, whether or not we
	--	     enumerated the things we imported, or just imported everything
	-- We need to recompile if M's exports change, because 
	-- if the import was	import M, 	we might now have a name clash in the 
	--					importing module.
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

data ExternalPackageState
  = EPS {
	eps_is_boot :: !(ModuleNameEnv (ModuleName, IsBootInterface)),
		-- In OneShot mode (only), home-package modules
		-- accumulate in the external package state, and are
		-- sucked in lazily.  For these home-pkg modules
		-- (only) we need to record which are boot modules.
		-- We set this field after loading all the
		-- explicitly-imported interfaces, but before doing
		-- anything else
		--
		-- The ModuleName part is not necessary, but it's useful for
		-- debug prints, and it's convenient because this field comes
		-- direct from TcRnTypes.ImportAvails.imp_dep_mods

	eps_PIT :: !PackageIfaceTable,
		-- The ModuleIFaces for modules in external packages
		-- whose interfaces we have opened
		-- The declarations in these interface files are held in
		-- eps_decls, eps_inst_env, eps_fam_inst_env, eps_rules
		-- (below), not in the mi_decls fields of the iPIT.  
		-- What _is_ in the iPIT is:
		--	* The Module 
		--	* Version info
		--	* Its exports
		--	* Fixities
		--	* Deprecations

	eps_PTE :: !PackageTypeEnv,	   -- Domain = external-package modules

	eps_inst_env     :: !PackageInstEnv,   -- The total InstEnv accumulated
					       -- from all the external-package
					       -- modules 
	eps_fam_inst_env :: !PackageFamInstEnv,-- Ditto FamInstEnv
	eps_rule_base    :: !PackageRuleBase,  -- Ditto RuleEnv
        eps_vect_info    :: !PackageVectInfo,  -- Ditto VectInfo

        eps_mod_fam_inst_env :: !(ModuleEnv FamInstEnv), -- identifies family
						       -- instances of each mod 
	eps_stats :: !EpsStats
  }

-- "In" means read from iface files
-- "Out" means actually sucked in and type-checked
data EpsStats = EpsStats { n_ifaces_in
			 , n_decls_in, n_decls_out 
			 , n_rules_in, n_rules_out
			 , n_insts_in, n_insts_out :: !Int }

addEpsInStats :: EpsStats -> Int -> Int -> Int -> EpsStats
-- Add stats for one newly-read interface
addEpsInStats stats n_decls n_insts n_rules
  = stats { n_ifaces_in = n_ifaces_in stats + 1
	  , n_decls_in  = n_decls_in stats + n_decls
	  , n_insts_in  = n_insts_in stats + n_insts
	  , n_rules_in  = n_rules_in stats + n_rules }
\end{code}

The NameCache makes sure that there is just one Unique assigned for
each original name; i.e. (module-name, occ-name) pair.  The Name is
always stored as a Global, and has the SrcLoc of its binding location.
Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.

\begin{code}
data NameCache
 = NameCache {  nsUniqs :: UniqSupply,
		-- Supply of uniques
		nsNames :: OrigNameCache,
		-- Ensures that one original name gets one unique
		nsIPs   :: OrigIParamCache
		-- Ensures that one implicit parameter name gets one unique
   }

type OrigNameCache   = ModuleEnv (OccEnv Name)
type OrigIParamCache = FiniteMap (IPName OccName) (IPName Name)
\end{code}



%************************************************************************
%*									*
		The module graph and ModSummary type
	A ModSummary is a node in the compilation manager's
	dependency graph, and it's also passed to hscMain
%*									*
%************************************************************************

A ModuleGraph contains all the nodes from the home package (only).  
There will be a node for each source module, plus a node for each hi-boot
module.

\begin{code}
type ModuleGraph = [ModSummary]  -- The module graph, 
				 -- NOT NECESSARILY IN TOPOLOGICAL ORDER

emptyMG :: ModuleGraph
emptyMG = []

-- The nodes of the module graph are
-- 	EITHER a regular Haskell source module
-- 	OR     a hi-boot source module

data ModSummary
   = ModSummary {
        ms_mod       :: Module,			-- Identity of the module
	ms_hsc_src   :: HscSource,		-- Source is Haskell, hs-boot, external core
        ms_location  :: ModLocation,		-- Location
        ms_hs_date   :: ClockTime,		-- Timestamp of source file
	ms_obj_date  :: Maybe ClockTime,	-- Timestamp of object, maybe
        ms_srcimps   :: [Located ModuleName],	-- Source imports
        ms_imps      :: [Located ModuleName],	-- Non-source imports
        ms_hspp_file :: FilePath,		-- Filename of preprocessed source.
        ms_hspp_opts :: DynFlags,               -- Cached flags from OPTIONS, INCLUDE
                                                -- and LANGUAGE pragmas.
	ms_hspp_buf  :: Maybe StringBuffer    	-- The actual preprocessed source, maybe.
     }

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

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

isBootSummary :: ModSummary -> Bool
isBootSummary ms = isHsBoot (ms_hsc_src ms)

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms) 
				<> text (hscSourceString (ms_hsc_src ms)) <> comma,
                          text "ms_imps =" <+> ppr (ms_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

showModMsg :: HscTarget -> Bool -> ModSummary -> String
showModMsg target recomp mod_summary
  = showSDoc (hsep [text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' '),
	            char '(', text (msHsFilePath mod_summary) <> comma,
		    case target of
                      HscInterpreted | recomp 
                                 -> text "interpreted"
                      HscNothing -> text "nothing"
                      _other     -> text (msObjFilePath mod_summary),
		    char ')'])
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
data HpcInfo 
  = HpcInfo 
     { hpcInfoTickCount :: Int 
     , hpcInfoHash      :: Int  
     }
  | NoHpcInfo 
     { hpcUsed          :: AnyHpcUsage  -- is hpc used anywhere on the module tree?
     }

-- This is used to mean there is no module-local hpc usage,
-- but one of my imports used hpc instrumentation.

type AnyHpcUsage = Bool

emptyHpcInfo :: AnyHpcUsage -> HpcInfo
emptyHpcInfo = NoHpcInfo 

isHpcUsed :: HpcInfo -> AnyHpcUsage
isHpcUsed (HpcInfo {})     		 = True
isHpcUsed (NoHpcInfo { hpcUsed = used }) = used
\end{code}

%************************************************************************
%*									*
\subsection{Vectorisation Support}
%*									*
%************************************************************************

The following information is generated and consumed by the vectorisation
subsystem.  It communicates the vectorisation status of declarations from one
module to another.

Why do we need both f and f_v in the ModGuts/ModDetails/EPS version VectInfo
below?  We need to know `f' when converting to IfaceVectInfo.  However, during
vectorisation, we need to know `f_v', whose `Var' we cannot lookup based
on just the OccName easily in a Core pass.

\begin{code}
-- ModGuts/ModDetails/EPS version
data VectInfo      
  = VectInfo {
      vectInfoVar     :: VarEnv  (Var    , Var  ),   -- (f, f_v) keyed on f
      vectInfoTyCon   :: NameEnv (TyCon  , TyCon),   -- (T, T_v) keyed on T
      vectInfoDataCon :: NameEnv (DataCon, DataCon), -- (C, C_v) keyed on C
      vectInfoPADFun  :: NameEnv (TyCon  , Var),     -- (T_v, paT) keyed on T_v
      vectInfoIso     :: NameEnv (TyCon  , Var)      -- (T, isoT) keyed on T
    }
    -- all of this is always tidy, even in ModGuts

-- ModIface version
data IfaceVectInfo 
  = IfaceVectInfo {
      ifaceVectInfoVar        :: [Name],
        -- all variables in here have a vectorised variant;
        -- the name of the vectorised variant is determined by `mkCloVect'
      ifaceVectInfoTyCon      :: [Name],
        -- all tycons in here have a vectorised variant;
        -- the name of the vectorised variant and those of its
        -- data constructors are determined by `mkVectTyConOcc'
        -- and `mkVectDataConOcc'; the names of
        -- the isomorphisms is determined by `mkVectIsoOcc'
      ifaceVectInfoTyConReuse :: [Name]              
        -- the vectorised form of all the tycons in here coincids with
        -- the unconverted from; the names of the isomorphisms is determined
        -- by `mkVectIsoOcc'
    }

noVectInfo :: VectInfo
noVectInfo = VectInfo emptyVarEnv emptyNameEnv emptyNameEnv emptyNameEnv emptyNameEnv

plusVectInfo :: VectInfo -> VectInfo -> VectInfo
plusVectInfo vi1 vi2 = 
  VectInfo (vectInfoVar     vi1 `plusVarEnv`  vectInfoVar     vi2)
           (vectInfoTyCon   vi1 `plusNameEnv` vectInfoTyCon   vi2)
           (vectInfoDataCon vi1 `plusNameEnv` vectInfoDataCon vi2)
           (vectInfoPADFun  vi1 `plusNameEnv` vectInfoPADFun  vi2)
           (vectInfoIso     vi1 `plusNameEnv` vectInfoIso     vi2)

noIfaceVectInfo :: IfaceVectInfo
noIfaceVectInfo = IfaceVectInfo [] [] []
\end{code}

%************************************************************************
%*									*
\subsection{Linkable stuff}
%*									*
%************************************************************************

This stuff is in here, rather than (say) in Linker.lhs, because the Linker.lhs
stuff is the *dynamic* linker, and isn't present in a stage-1 compiler

\begin{code}
data Linkable = LM {
  linkableTime     :: ClockTime,	-- Time at which this linkable was built
					-- (i.e. when the bytecodes were produced,
					--	 or the mod date on the files)
  linkableModule   :: Module,		-- Should be Module, but see below
  linkableUnlinked :: [Unlinked]
 }

isObjectLinkable :: Linkable -> Bool
isObjectLinkable l = not (null unlinked) && all isObject unlinked
  where unlinked = linkableUnlinked l
	-- A linkable with no Unlinked's is treated as a BCO.  We can
	-- generate a linkable with no Unlinked's as a result of
	-- compiling a module in HscNothing mode, and this choice
	-- happens to work well with checkStability in module GHC.

instance Outputable Linkable where
   ppr (LM when_made mod unlinkeds)
      = (text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod)
        $$ nest 3 (ppr unlinkeds)

-------------------------------------------
data Unlinked
   = DotO FilePath
   | DotA FilePath
   | DotDLL FilePath
   | BCOs CompiledByteCode ModBreaks

#ifndef GHCI
data CompiledByteCode
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

isObject :: Unlinked -> Bool
isObject (DotO _)   = True
isObject (DotA _)   = True
isObject (DotDLL _) = True
isObject _          = False

isInterpretable :: Unlinked -> Bool
isInterpretable = not . isObject

nameOfObject :: Unlinked -> FilePath
nameOfObject (DotO fn)   = fn
nameOfObject (DotA fn)   = fn
nameOfObject (DotDLL fn) = fn
nameOfObject other       = pprPanic "nameOfObject" (ppr other)

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
type BreakIndex = Int

-- | all the information about the breakpoints for a given module
data ModBreaks
   = ModBreaks
   { modBreaks_flags :: BreakArray
        -- The array of flags, one per breakpoint, 
        -- indicating which breakpoints are enabled.
   , modBreaks_locs :: !(Array BreakIndex SrcSpan)
        -- An array giving the source span of each breakpoint.
   , modBreaks_vars :: !(Array BreakIndex [OccName])
        -- An array giving the names of the free variables at each breakpoint.
   }

emptyModBreaks :: ModBreaks
emptyModBreaks = ModBreaks
   { modBreaks_flags = error "ModBreaks.modBreaks_array not initialised"
         -- Todo: can we avoid this? 
   , modBreaks_locs = array (0,-1) []
   , modBreaks_vars = array (0,-1) []
   }
\end{code}
