
% (c) The University of Glasgow, 2000
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes ( 
	-- * Sessions and compilation state
	Session(..), HscEnv(..), hscEPS,
	FinderCache, FinderCacheEntry,
	Target(..), TargetId(..), pprTarget, pprTargetId,
	ModuleGraph, emptyMG,

	ModDetails(..),	emptyModDetails,
	ModGuts(..), ModImports(..), ForeignStubs(..),

	ModSummary(..), showModMsg, isBootSummary,
	msHsFilePath, msHiFilePath, msObjFilePath, 

	HscSource(..), isHsBoot, hscSourceString,	-- Re-exported from DriverPhases
	
	HomePackageTable, HomeModInfo(..), emptyHomePackageTable,
	hptInstances, hptRules,

	ExternalPackageState(..), EpsStats(..), addEpsInStats,
	PackageTypeEnv, PackageIfaceTable, emptyPackageIfaceTable,
	lookupIface, lookupIfaceByModule, emptyModIface,

	InteractiveContext(..), emptyInteractiveContext, 
	icPrintUnqual, unQualInScope,

	ModIface(..), mkIfaceDepCache, mkIfaceVerCache, mkIfaceFixCache,
	emptyIfaceDepCache, 

	Deprecs(..), IfaceDeprecs,

	FixityEnv, FixItem(..), lookupFixity, emptyFixityEnv,

	implicitTyThings, 

	TyThing(..), tyThingClass, tyThingTyCon, tyThingDataCon, tyThingId,
	TypeEnv, lookupType, mkTypeEnv, emptyTypeEnv,
	extendTypeEnv, extendTypeEnvList, extendTypeEnvWithIds, lookupTypeEnv,
	typeEnvElts, typeEnvClasses, typeEnvTyCons, typeEnvIds,

	WhetherHasOrphans, IsBootInterface, Usage(..), 
	Dependencies(..), noDependencies,
	InstPool, Gated, addInstsToPool, 
	RulePool, addRulesToPool, 
	NameCache(..), OrigNameCache, OrigIParamCache,
	Avails, availsToNameSet, availName, availNames,
	GenAvailInfo(..), AvailInfo, RdrAvailInfo, 
	IfaceExport,

	Deprecations, DeprecTxt, lookupDeprec, plusDeprecs,

	InstEnv, DFunId,
	PackageInstEnv, PackageRuleBase,

	-- Linker stuff
	Linkable(..), isObjectLinkable,
	Unlinked(..), CompiledByteCode,
	isObject, nameOfObject, isInterpretable, byteCodeOfObject
    ) where

#include "HsVersions.h"

#ifdef GHCI
import ByteCodeAsm	( CompiledByteCode )
#endif

import RdrName		( GlobalRdrEnv, emptyGlobalRdrEnv,
			  LocalRdrEnv, emptyLocalRdrEnv,
			  GlobalRdrElt(..), mkRdrUnqual, lookupGRE_RdrName )
import Name		( Name, NamedThing, getName, nameOccName, nameModule )
import NameEnv
import NameSet	
import OccName		( OccName, OccEnv, lookupOccEnv, mkOccEnv, emptyOccEnv, 
			  extendOccEnv )
import Module
import InstEnv		( InstEnv, DFunId )
import Rules		( RuleBase )
import CoreSyn		( CoreBind )
import Id		( Id )
import Type		( TyThing(..) )

import Class		( Class, classSelIds, classTyCon )
import TyCon		( TyCon, tyConSelIds, tyConDataCons )
import DataCon		( dataConImplicitIds )
import Packages		( PackageIdH, PackageId, PackageConfig )
import DynFlags		( DynFlags(..), isOneShot )
import DriverPhases	( HscSource(..), isHsBoot, hscSourceString )
import BasicTypes	( Version, initialVersion, IPName, 
			  Fixity, defaultFixity, DeprecTxt )

import IfaceSyn		( IfaceInst, IfaceRule, IfaceDecl(ifName) )

import FiniteMap	( FiniteMap )
import CoreSyn		( IdCoreRule )
import Maybes		( orElse, fromJust, expectJust )
import Outputable
import SrcLoc		( SrcSpan )
import UniqSupply	( UniqSupply )
import FastString	( FastString )

import DATA_IOREF	( IORef, readIORef )
import StringBuffer	( StringBuffer )
import Time		( ClockTime )
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

	hsc_FC  :: {-# UNPACK #-} !(IORef FinderCache)
		-- The finder's cache.  This caches the location of modules,
		-- so we don't have to search the filesystem multiple times.
 }

hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

-- | A compilation target.
--
-- A target may be supplied with the actual text of the
-- module.  If so, use this instead of the file contents (this
-- is for use in an IDE where the file hasn't been saved by
-- the user yet).
data Target = Target TargetId (Maybe StringBuffer)

data TargetId
  = TargetModule Module	   -- | A module name: search for the file
  | TargetFile   FilePath  -- | A filename: parse it to find the module name.


pprTarget :: Target -> SDoc
pprTarget (Target id _) = pprTargetId id

pprTargetId (TargetModule m) = ppr m
pprTargetId (TargetFile f)   = text f

type FinderCache = ModuleEnv FinderCacheEntry
type FinderCacheEntry = (ModLocation, Maybe (PackageConfig,Bool))
	-- The finder's cache (see module Finder)

type HomePackageTable  = ModuleEnv HomeModInfo
	-- Domain = modules in the home package
type PackageIfaceTable = ModuleEnv ModIface
	-- Domain = modules in the imported packages

emptyHomePackageTable  = emptyModuleEnv
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
\end{code}

Simple lookups in the symbol table.

\begin{code}
lookupIface :: HomePackageTable -> PackageIfaceTable -> Module -> Maybe ModIface
-- We often have two IfaceTables, and want to do a lookup
lookupIface hpt pit mod
  = case lookupModuleEnv hpt mod of
	Just mod_info -> Just (hm_iface mod_info)
	Nothing       -> lookupModuleEnv pit mod

lookupIfaceByModule :: HomePackageTable -> PackageIfaceTable -> Module -> Maybe ModIface
-- We often have two IfaceTables, and want to do a lookup
lookupIfaceByModule hpt pit mod
  = case lookupModuleEnv hpt mod of
	Just mod_info -> Just (hm_iface mod_info)
	Nothing       -> lookupModuleEnv pit mod
\end{code}


\begin{code}
hptInstances :: HscEnv -> (Module -> Bool) -> [DFunId]
-- Find all the instance declarations that are in modules imported 
-- by this one, directly or indirectly, and are in the Home Package Table
-- This ensures that we don't see instances from modules --make compiled 
-- before this one, but which are not below this one
hptInstances hsc_env want_this_module
  = [ dfun 
    | mod_info <- moduleEnvElts (hsc_HPT hsc_env)
    , want_this_module (mi_module (hm_iface mod_info))
    , dfun <- md_insts (hm_details mod_info) ]

hptRules :: HscEnv -> [(Module, IsBootInterface)] -> [IdCoreRule]
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

	-- Look it up in the HPT
    , let mod_info = ASSERT( mod `elemModuleEnv` hpt )
		     fromJust (lookupModuleEnv hpt mod)

	-- And get its dfuns
    , rule <- md_rules (hm_details mod_info) ]
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
	mi_package  :: !PackageIdH,	    -- Which package the module comes from
        mi_module   :: !Module,
        mi_mod_vers :: !Version,	    -- Module version: changes when anything changes

        mi_orphan   :: !WhetherHasOrphans,  -- Whether this module has orphans
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
	mi_deprecs  :: IfaceDeprecs,
		-- NOT STRICT!  we read this field lazily from the interface file

		-- Type, class and variable declarations
		-- The version of an Id changes if its fixity or deprecations change
		--	(as well as its type of course)
		-- Ditto data constructors, class operations, except that 
		-- the version of the parent class/tycon changes
	mi_decls :: [(Version,IfaceDecl)],	-- Sorted

        mi_globals  :: !(Maybe GlobalRdrEnv),
		-- Its top level environment or Nothing if we read this
		-- interface from an interface file.  (We need the source
		-- file to figure out the top-level environment.)

		-- Instance declarations and rules
	mi_insts     :: [IfaceInst],	-- Sorted
	mi_rules     :: [IfaceRule],	-- Sorted
	mi_rule_vers :: !Version,	-- Version number for rules and instances combined

		-- Cached environments for easy lookup
		-- These are computed (lazily) from other fields
		-- and are not put into the interface file
	mi_dep_fn  :: Name -> Maybe DeprecTxt,	-- Cached lookup for mi_deprecs
	mi_fix_fn  :: OccName -> Fixity,	-- Cached lookup for mi_fixities
	mi_ver_fn  :: OccName -> Maybe Version	-- Cached lookup for mi_decls
			-- The Nothing in mi_ver_fn means that the thing
			-- isn't in decls. It's useful to know that when
			-- seeing if we are up to date wrt the old interface
     }

-- Should be able to construct ModDetails from mi_decls in ModIface
data ModDetails
   = ModDetails {
	-- The next three fields are created by the typechecker
        md_types    :: !TypeEnv,
        md_insts    :: ![DFunId],	-- Dfun-ids for the instances in this module
        md_rules    :: ![IdCoreRule]	-- Domain may include Ids from other modules
     }

emptyModDetails = ModDetails { md_types = emptyTypeEnv,
			       md_insts = [],
			       md_rules = [] }

-- A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a ModIface and 
-- ModDetails are extracted and the ModGuts is dicarded.

data ModGuts
  = ModGuts {
        mg_module   :: !Module,
	mg_boot     :: IsBootInterface, -- Whether it's an hs-boot module
	mg_exports  :: !NameSet,	-- What it exports
	mg_deps	    :: !Dependencies,	-- What is below it, directly or otherwise
	mg_dir_imps :: ![Module],	-- Directly-imported modules; used to
					--	generate initialisation code
	mg_usages   :: ![Usage],	-- Version info for what it needed

        mg_rdr_env  :: !GlobalRdrEnv,	-- Top-level lexical environment
	mg_fix_env  :: !FixityEnv,	-- Fixity env, for things declared in this module
	mg_deprecs  :: !Deprecations,	-- Deprecations declared in the module

	mg_types    :: !TypeEnv,
	mg_insts    :: ![DFunId],	-- Instances 
        mg_rules    :: ![IdCoreRule],	-- Rules from this module
	mg_binds    :: ![CoreBind],	-- Bindings for this module
	mg_foreign  :: !ForeignStubs
    }

-- The ModGuts takes on several slightly different forms:
--
-- After simplification, the following fields change slightly:
--	mg_rules	Orphan rules only (local ones now attached to binds)
--	mg_binds	With rules attached
--
-- After CoreTidy, the following fields change slightly:
--	mg_types	Now contains Ids as well, replete with final IdInfo
--			   The Ids are only the ones that are visible from
--			   importing modules.  Without -O that means only
--			   exported Ids, but with -O importing modules may
--			   see ids mentioned in unfoldings of exported Ids
--
--	mg_insts	Same DFunIds as before, but with final IdInfo,
--			   and the unique might have changed; remember that
--			   CoreTidy links up the uniques of old and new versions
--
--	mg_rules	All rules for exported things, substituted with final Ids
--
--	mg_binds	Tidied



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

data ForeignStubs = NoStubs
		  | ForeignStubs
			SDoc 		-- Header file prototypes for
                                      	-- 	"foreign exported" functions
			SDoc 		-- C stubs to use when calling
                                        -- 	"foreign exported" functions
			[FastString] 	-- Headers that need to be included
				        -- 	into C code generated for this module
			[Id]		-- Foreign-exported binders
					-- 	we have to generate code to register these

\end{code}

\begin{code}
emptyModIface :: PackageIdH -> Module -> ModIface
emptyModIface pkg mod
  = ModIface { mi_package  = pkg,
	       mi_module   = mod,
	       mi_mod_vers = initialVersion,
	       mi_orphan   = False,
	       mi_boot	   = False,
	       mi_deps     = noDependencies,
	       mi_usages   = [],
	       mi_exports  = [],
	       mi_exp_vers = initialVersion,
	       mi_fixities = [],
	       mi_deprecs  = NoDeprecs,
	       mi_insts = [],
	       mi_rules = [],
	       mi_decls = [],
	       mi_globals  = Nothing,
	       mi_rule_vers = initialVersion,
	       mi_dep_fn = emptyIfaceDepCache,
	       mi_fix_fn = emptyIfaceFixCache,
	       mi_ver_fn = emptyIfaceVerCache
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

	ic_rn_local_env :: LocalRdrEnv,	-- Lexical context for variables bound
					-- during interaction

	ic_type_env :: TypeEnv		-- Ditto for types
    }

emptyInteractiveContext
  = InteractiveContext { ic_toplev_scope = [],
			 ic_exports = [],
			 ic_rn_gbl_env = emptyGlobalRdrEnv,
			 ic_rn_local_env = emptyLocalRdrEnv,
			 ic_type_env = emptyTypeEnv }

icPrintUnqual :: InteractiveContext -> PrintUnqualified
icPrintUnqual ictxt = unQualInScope (ic_rn_gbl_env ictxt)
\end{code}

@unQualInScope@ returns a function that takes a @Name@ and tells whether
its unqualified name is in scope.  This is put as a boolean flag in
the @Name@'s provenance to guide whether or not to print the name qualified
in error messages.

\begin{code}
unQualInScope :: GlobalRdrEnv -> PrintUnqualified
-- True if 'f' is in scope, and has only one binding,
-- and the thing it is bound to is the name we are looking for
-- (i.e. false if A.f and B.f are both in scope as unqualified 'f')
--
-- [Out of date] Also checks for built-in syntax, which is always 'in scope'
unQualInScope env mod occ
  = case lookupGRE_RdrName (mkRdrUnqual occ) env of
	[gre] -> nameModule (gre_name gre) == mod
	other -> False
\end{code}


%************************************************************************
%*									*
		TyThing
%*									*
%************************************************************************

\begin{code}
implicitTyThings :: TyThing -> [TyThing]
implicitTyThings (AnId id)   = []

	-- For type constructors, add the data cons (and their extras),
	-- and the selectors and generic-programming Ids too
	--
	-- Newtypes don't have a worker Id, so don't generate that?
implicitTyThings (ATyCon tc) = map AnId (tyConSelIds tc) ++ 
			       concatMap (extras_plus . ADataCon) (tyConDataCons tc)
		     
	-- For classes, add the class TyCon too (and its extras)
	-- and the class selector Ids
implicitTyThings (AClass cl) = map AnId (classSelIds cl) ++
			       extras_plus (ATyCon (classTyCon cl))
			 

	-- For data cons add the worker and wrapper (if any)
implicitTyThings (ADataCon dc) = map AnId (dataConImplicitIds dc)

extras_plus thing = thing : implicitTyThings thing

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

emptyTypeEnv   :: TypeEnv
typeEnvElts    :: TypeEnv -> [TyThing]
typeEnvClasses :: TypeEnv -> [Class]
typeEnvTyCons  :: TypeEnv -> [TyCon]
typeEnvIds     :: TypeEnv -> [Id]
lookupTypeEnv  :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv 	   = emptyNameEnv
typeEnvElts    env = nameEnvElts env
typeEnvClasses env = [cl | AClass cl <- typeEnvElts env]
typeEnvTyCons  env = [tc | ATyCon tc <- typeEnvElts env] 
typeEnvIds     env = [id | AnId id   <- typeEnvElts env] 

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
lookupType :: HomePackageTable -> PackageTypeEnv -> Name -> Maybe TyThing
lookupType hpt pte name
  = case lookupModuleEnv hpt (nameModule name) of
	Just details -> lookupNameEnv (md_types (hm_details details)) name
	Nothing	     -> lookupNameEnv pte name
\end{code}


\begin{code}
tyThingTyCon (ATyCon tc) = tc
tyThingTyCon other	 = pprPanic "tyThingTyCon" (ppr other)

tyThingClass (AClass cls) = cls
tyThingClass other	  = pprPanic "tyThingClass" (ppr other)

tyThingDataCon (ADataCon dc) = dc
tyThingDataCon other	     = pprPanic "tyThingDataCon" (ppr other)

tyThingId (AnId id) = id
tyThingId other     = pprPanic "tyThingId" (ppr other)
\end{code}

%************************************************************************
%*									*
\subsection{Auxiliary types}
%*									*
%************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere

\begin{code}
mkIfaceVerCache :: [(Version,IfaceDecl)] -> OccName -> Maybe Version
mkIfaceVerCache pairs 
  = \occ -> lookupOccEnv env occ
  where
    env = foldl add emptyOccEnv pairs
    add env (v,d) = extendOccEnv env (ifName d) v

emptyIfaceVerCache :: OccName -> Maybe Version
emptyIfaceVerCache occ = Nothing

------------------ Deprecations -------------------------
data Deprecs a
  = NoDeprecs
  | DeprecAll DeprecTxt	-- Whole module deprecated
  | DeprecSome a	-- Some specific things deprecated
  deriving( Eq )

type IfaceDeprecs = Deprecs [(OccName,DeprecTxt)]
type Deprecations = Deprecs (NameEnv (OccName,DeprecTxt))

mkIfaceDepCache:: IfaceDeprecs -> Name -> Maybe DeprecTxt
mkIfaceDepCache NoDeprecs     	  = \n -> Nothing
mkIfaceDepCache (DeprecAll t) 	  = \n -> Just t
mkIfaceDepCache (DeprecSome pairs) = lookupOccEnv (mkOccEnv pairs) . nameOccName

emptyIfaceDepCache :: Name -> Maybe DeprecTxt
emptyIfaceDepCache n = Nothing

lookupDeprec :: Deprecations -> Name -> Maybe DeprecTxt
lookupDeprec NoDeprecs        name = Nothing
lookupDeprec (DeprecAll  txt) name = Just txt
lookupDeprec (DeprecSome env) name = case lookupNameEnv env name of
					    Just (_, txt) -> Just txt
					    Nothing	  -> Nothing

plusDeprecs :: Deprecations -> Deprecations -> Deprecations
plusDeprecs d NoDeprecs = d
plusDeprecs NoDeprecs d = d
plusDeprecs d (DeprecAll t) = DeprecAll t
plusDeprecs (DeprecAll t) d = DeprecAll t
plusDeprecs (DeprecSome v1) (DeprecSome v2) = DeprecSome (v1 `plusNameEnv` v2)
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
availsToNameSet avails = foldl add emptyNameSet avails
		       where
			 add set avail = addListToNameSet set (availNames avail)

availName :: GenAvailInfo name -> name
availName (Avail n)     = n
availName (AvailTC n _) = n

availNames :: GenAvailInfo name -> [name]
availNames (Avail n)      = [n]
availNames (AvailTC n ns) = ns

instance Outputable n => Outputable (GenAvailInfo n) where
   ppr = pprAvail

pprAvail :: Outputable n => GenAvailInfo n -> SDoc
pprAvail (AvailTC n ns) = ppr n <> case {- filter (/= n) -} ns of
					[]  -> empty
					ns' -> braces (hsep (punctuate comma (map ppr ns')))

pprAvail (Avail n) = ppr n
\end{code}

\begin{code}
mkIfaceFixCache :: [(OccName, Fixity)] -> OccName -> Fixity
mkIfaceFixCache pairs 
  = \n -> lookupOccEnv env n `orElse` defaultFixity
  where
   env = mkOccEnv pairs

emptyIfaceFixCache :: OccName -> Fixity
emptyIfaceFixCache n = defaultFixity

-- This fixity environment is for source code only
type FixityEnv = NameEnv FixItem

-- We keep the OccName in the range so that we can generate an interface from it
data FixItem = FixItem OccName Fixity SrcSpan

instance Outputable FixItem where
  ppr (FixItem occ fix loc) = ppr fix <+> ppr occ <+> parens (ppr loc)

emptyFixityEnv :: FixityEnv
emptyFixityEnv = emptyNameEnv

lookupFixity :: FixityEnv -> Name -> Fixity
lookupFixity env n = case lookupNameEnv env n of
			Just (FixItem _ fix _) -> fix
			Nothing	      	       -> defaultFixity
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

type IsBootInterface = Bool

-- Dependency info about modules and packages below this one
-- in the import hierarchy.  See TcRnTypes.ImportAvails for details.
--
-- Invariant: the dependencies of a module M never includes M
-- Invariant: the lists are unordered, with no duplicates
data Dependencies
  = Deps { dep_mods  :: [(Module,IsBootInterface)],	-- Home-package module dependencies
	   dep_pkgs  :: [PackageId], 			-- External package dependencies
	   dep_orphs :: [Module] }			-- Orphan modules (whether home or external pkg)
  deriving( Eq )
	-- Equality used only for old/new comparison in MkIface.addVersionInfo

noDependencies :: Dependencies
noDependencies = Deps [] [] []
 	  
data Usage
  = Usage { usg_name     :: Module,			-- Name of the module
	    usg_mod      :: Version,			-- Module version
	    usg_entities :: [(OccName,Version)],	-- Sorted by occurrence name
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
	--	i.e. we imported the module without saying exactly what we imported
	-- We need to recompile if the module exports changes, because we might
	-- now have a name clash in the importing module.
\end{code}


%************************************************************************
%*									*
		The External Package State
%*									*
%************************************************************************

\begin{code}
type PackageTypeEnv  = TypeEnv
type PackageRuleBase = RuleBase
type PackageInstEnv  = InstEnv

data ExternalPackageState
  = EPS {
	eps_is_boot :: !(ModuleEnv (Module, IsBootInterface)),
		-- In OneShot mode (only), home-package modules accumulate in the
		-- external package state, and are sucked in lazily.
		-- For these home-pkg modules (only) we need to record which are
		-- boot modules.  We set this field after loading all the 
		-- explicitly-imported interfaces, but before doing anything else
		--
		-- The Module part is not necessary, but it's useful for
		-- debug prints, and it's convenient because this field comes
		-- direct from TcRnTypes.ImportAvails.imp_dep_mods

	eps_PIT :: !PackageIfaceTable,
		-- The ModuleIFaces for modules in external packages
		-- whose interfaces we have opened
		-- The declarations in these interface files are held in
		-- eps_decls, eps_insts, eps_rules (below), not in the 
		-- mi_decls fields of the iPIT.  
		-- What _is_ in the iPIT is:
		--	* The Module 
		--	* Version info
		--	* Its exports
		--	* Fixities
		--	* Deprecations

	eps_PTE :: !PackageTypeEnv,		-- Domain = external-package modules

	eps_inst_env :: !PackageInstEnv,	-- The total InstEnv accumulated from
						--   all the external-package modules
	eps_rule_base :: !PackageRuleBase,	-- Ditto RuleEnv


	-- Holding pens for stuff that has been read in from file,
	-- but not yet slurped into the renamer
	eps_insts :: !InstPool,
		-- The as-yet un-slurped instance decls
		-- Decls move from here to eps_inst_env
		-- Each instance is 'gated' by the names that must be 
		-- available before this instance decl is needed.

	eps_rules :: !RulePool,
		-- The as-yet un-slurped rules

	eps_stats :: !EpsStats
  }

-- "In" means read from iface files
-- "Out" means actually sucked in and type-checked
data EpsStats = EpsStats { n_ifaces_in
			 , n_decls_in, n_decls_out 
			 , n_rules_in, n_rules_out
			 , n_insts_in, n_insts_out :: !Int }
\end{code}

The NameCache makes sure that there is just one Unique assigned for
each original name; i.e. (module-name, occ-name) pair.  The Name is
always stored as a Global, and has the SrcLoc of its binding location.
Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.

Exactly the same is true of the Module stored in the Name.  When we first
encounter the occurrence, we may not know the details of the module, so
we just store junk.  Then when we find the binding site, we fix it up.

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

\begin{code}
type Gated d = ([Name], (Module, SDoc, d))
	-- The [Name] 'gate' the declaration; always non-empty
	-- Module records which module this decl belongs to
	-- SDoc records the pathname of the file, or similar err-ctxt info

type RulePool = [Gated IfaceRule]

addRulesToPool :: RulePool
	      -> [Gated IfaceRule]
	      -> RulePool
addRulesToPool rules new_rules = new_rules ++ rules

-------------------------
addEpsInStats :: EpsStats -> Int -> Int -> Int -> EpsStats
-- Add stats for one newly-read interface
addEpsInStats stats n_decls n_insts n_rules
  = stats { n_ifaces_in = n_ifaces_in stats + 1
	  , n_decls_in  = n_decls_in stats + n_decls
	  , n_insts_in  = n_insts_in stats + n_insts
	  , n_rules_in  = n_rules_in stats + n_rules }

-------------------------
type InstPool = NameEnv [Gated IfaceInst]
	-- The key of the Pool is the Class
	-- The Names are the TyCons in the instance head
	-- For example, suppose this is in an interface file
	--	instance C T where ...
	-- We want to slurp this decl if both C and T are "visible" in 
	-- the importing module.  See "The gating story" in RnIfaces for details.


addInstsToPool :: InstPool -> [(Name, Gated IfaceInst)] -> InstPool
addInstsToPool insts new_insts
  = foldr add insts new_insts
  where
    add :: (Name, Gated IfaceInst) -> NameEnv [Gated IfaceInst] -> NameEnv [Gated IfaceInst]
    add (cls,new_inst) insts = extendNameEnv_C combine insts cls [new_inst]
	where
	  combine old_insts _ = new_inst : old_insts
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
        ms_mod       :: Module,			-- Name of the module
	ms_hsc_src   :: HscSource,		-- Source is Haskell, hs-boot, external core
        ms_location  :: ModLocation,		-- Location
        ms_hs_date   :: ClockTime,		-- Timestamp of source file
	ms_obj_date  :: Maybe ClockTime,	-- Timestamp of object, maybe
        ms_srcimps   :: [Module],		-- Source imports
        ms_imps      :: [Module],		-- Non-source imports
        ms_hspp_file :: Maybe FilePath,		-- Filename of preprocessed source,
						-- once we have preprocessed it.
	ms_hspp_buf  :: Maybe StringBuffer	-- The actual preprocessed source, maybe.
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

showModMsg :: Bool -> ModSummary -> String
showModMsg use_object mod_summary
  = showSDoc (hsep [text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' '),
	            char '(', text (msHsFilePath mod_summary) <> comma,
		    if use_object then text (msObjFilePath mod_summary)
			      else text "interpreted",
		    char ')'])
 where 
    mod     = ms_mod mod_summary 
    mod_str = moduleUserString mod ++ hscSourceString (ms_hsc_src mod_summary)
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
isObjectLinkable l = all isObject (linkableUnlinked l)

instance Outputable Linkable where
   ppr (LM when_made mod unlinkeds)
      = (text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod)
        $$ nest 3 (ppr unlinkeds)

-------------------------------------------
data Unlinked
   = DotO FilePath
   | DotA FilePath
   | DotDLL FilePath
   | BCOs CompiledByteCode

#ifndef GHCI
data CompiledByteCode = NoByteCode
#endif

instance Outputable Unlinked where
   ppr (DotO path)   = text "DotO" <+> text path
   ppr (DotA path)   = text "DotA" <+> text path
   ppr (DotDLL path) = text "DotDLL" <+> text path
#ifdef GHCI
   ppr (BCOs bcos)   = text "BCOs" <+> ppr bcos
#else
   ppr (BCOs bcos)   = text "No byte code"
#endif

isObject (DotO _)   = True
isObject (DotA _)   = True
isObject (DotDLL _) = True
isObject _          = False

isInterpretable = not . isObject

nameOfObject (DotO fn)   = fn
nameOfObject (DotA fn)   = fn
nameOfObject (DotDLL fn) = fn

byteCodeOfObject (BCOs bc) = bc
\end{code}



