%
% (c) The University of Glasgow, 2000
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes ( 
	HscEnv(..), 
	GhciMode(..),

	ModDetails(..),	ModIface(..), 
	ModGuts(..), ModImports(..), ForeignStubs(..),
	ParsedIface(..), IfaceDeprecs,

	HomePackageTable, HomeModInfo(..), emptyHomePackageTable,

	ExternalPackageState(..), 
	PackageTypeEnv, PackageIfaceTable, emptyPackageIfaceTable,
	lookupIface, lookupIfaceByModName, moduleNameToModule,
	emptyModIface,

	InteractiveContext(..), emptyInteractiveContext, icPrintUnqual,

	IfaceDecls, mkIfaceDecls, dcl_tycl, dcl_rules, dcl_insts,

	VersionInfo(..), initialVersionInfo, lookupVersion,
	FixityEnv, lookupFixity, collectFixities, emptyFixityEnv,

	TyThing(..), isTyClThing, implicitTyThingIds,

	TypeEnv, lookupType, mkTypeEnv, emptyTypeEnv,
	extendTypeEnvList, extendTypeEnvWithIds,
	typeEnvElts, typeEnvClasses, typeEnvTyCons, typeEnvIds,

	ImportedModuleInfo, WhetherHasOrphans, ImportVersion, WhatsImported(..),
	IsBootInterface, DeclsMap,
	IfaceInsts, IfaceRules, GatedDecl, GatedDecls, GateFn, 
	NameCache(..), OrigNameCache, OrigIParamCache,
	Avails, availsToNameSet, availName, availNames,
	GenAvailInfo(..), AvailInfo, RdrAvailInfo, 
	ExportItem, RdrExportItem,

	PersistentCompilerState(..),

	Deprecations(..), lookupDeprec, plusDeprecs,

	InstEnv, ClsInstEnv, DFunId,
	PackageInstEnv, PackageRuleBase,

	GlobalRdrEnv, GlobalRdrElt(..), emptyGlobalRdrEnv, pprGlobalRdrEnv,
	LocalRdrEnv, extendLocalRdrEnv, isLocalGRE, unQualInScope,
	
	-- Linker stuff
	Linkable(..), isObjectLinkable,
	Unlinked(..), CompiledByteCode,
	isObject, nameOfObject, isInterpretable, byteCodeOfObject,

	-- Provenance
	Provenance(..), ImportReason(..), 
        pprNameProvenance, hasBetterProv

    ) where

#include "HsVersions.h"

#ifdef GHCI
import ByteCodeAsm	( CompiledByteCode )
#endif

import RdrName		( RdrName, mkRdrUnqual, 
			  RdrNameEnv, addListToRdrEnv, foldRdrEnv, isUnqual,
			  rdrEnvToList, emptyRdrEnv )
import Name		( Name, NamedThing, getName, nameOccName, nameModule, nameSrcLoc )
import NameEnv
import NameSet	
import OccName		( OccName )
import Module
import InstEnv		( InstEnv, ClsInstEnv, DFunId )
import Rules		( RuleBase )
import CoreSyn		( CoreBind )
import Id		( Id )
import Class		( Class, classSelIds )
import TyCon		( TyCon, isNewTyCon, tyConGenIds, tyConSelIds, tyConDataCons_maybe )
import DataCon		( dataConWorkId, dataConWrapId )
import Packages		( PackageName, preludePackage )
import CmdLineOpts	( DynFlags )

import BasicTypes	( Version, initialVersion, IPName,
			  Fixity, FixitySig(..), defaultFixity )

import HsSyn		( DeprecTxt, TyClDecl, InstDecl, RuleDecl, 
			  tyClDeclName, ifaceRuleDeclName, tyClDeclNames )
import RnHsSyn		( RenamedTyClDecl, RenamedRuleDecl, RenamedInstDecl )

import CoreSyn		( IdCoreRule )
import PrelNames	( isBuiltInSyntaxName )

import FiniteMap
import Bag		( Bag )
import Maybes		( orElse )
import Outputable
import SrcLoc		( SrcLoc, isGoodSrcLoc )
import Util		( thenCmp, sortLt )
import UniqSupply	( UniqSupply )
import Maybe		( fromJust )
import FastString	( FastString )

import Time		( ClockTime )
\end{code}


%************************************************************************
%*									*
\subsection{Compilation environment}
%*									*
%************************************************************************

The HscEnv gives the environment in which to compile a chunk of code.

\begin{code}
data HscEnv = HscEnv { hsc_mode   :: GhciMode,
		       hsc_dflags :: DynFlags,
		       hsc_HPT    :: HomePackageTable }
\end{code}

The GhciMode is self-explanatory:

\begin{code}
data GhciMode = Batch | Interactive | OneShot 
	      deriving Eq
\end{code}

\begin{code}
type HomePackageTable  = ModuleEnv HomeModInfo	-- Domain = modules in the home package
type PackageIfaceTable = ModuleEnv ModIface	-- Domain = modules in the imported packages

emptyHomePackageTable  = emptyModuleEnv
emptyPackageIfaceTable = emptyModuleEnv

data HomeModInfo = HomeModInfo { hm_iface    :: ModIface,
				 hm_details  :: ModDetails,
				 hm_linkable :: Linkable }
\end{code}

Simple lookups in the symbol table.

\begin{code}
lookupIface :: HomePackageTable -> PackageIfaceTable -> Name -> Maybe ModIface
-- We often have two IfaceTables, and want to do a lookup
lookupIface hpt pit name
  = case lookupModuleEnv hpt mod of
	Just mod_info -> Just (hm_iface mod_info)
	Nothing       -> lookupModuleEnv pit mod
  where
    mod = nameModule name

lookupIfaceByModName :: HomePackageTable -> PackageIfaceTable -> ModuleName -> Maybe ModIface
-- We often have two IfaceTables, and want to do a lookup
lookupIfaceByModName hpt pit mod
  = case lookupModuleEnvByName hpt mod of
	Just mod_info -> Just (hm_iface mod_info)
	Nothing       -> lookupModuleEnvByName pit mod
\end{code}

\begin{code}
-- Use instead of Finder.findModule if possible: this way doesn't
-- require filesystem operations, and it is guaranteed not to fail
-- when the IfaceTables are properly populated (i.e. after the renamer).
moduleNameToModule :: HomePackageTable -> PackageIfaceTable -> ModuleName -> Module
moduleNameToModule hpt pit mod 
   = mi_module (fromJust (lookupIfaceByModName hpt pit mod))
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
	mi_package  :: !PackageName,	    -- Which package the module comes from
        mi_version  :: !VersionInfo,	    -- Module version number
        mi_orphan   :: !WhetherHasOrphans,  -- Whether this module has orphans
	mi_boot	    :: !IsBootInterface,    -- Read from an hi-boot file?

        mi_usages   :: [ImportVersion Name],
		-- Usages; kept sorted so that it's easy to decide
		-- whether to write a new iface file (changing usages
		-- doesn't affect the version of this module)
		-- NOT STRICT!  we read this field lazily from the interface file

        mi_exports  :: ![ExportItem],
		-- What it exports Kept sorted by (mod,occ), to make
		-- version comparisons easier

        mi_globals  :: !(Maybe GlobalRdrEnv),
		-- Its top level environment or Nothing if we read this
		-- interface from an interface file.  (We need the source
		-- file to figure out the top-level environment.)

        mi_fixities :: !FixityEnv,	    -- Fixities
	mi_deprecs  :: Deprecations,	    -- Deprecations
		-- NOT STRICT!  we read this field lazilly from the interface file

	mi_decls    :: IfaceDecls	    -- The RnDecls form of ModDetails
		-- NOT STRICT!  we fill this field with _|_ sometimes
     }

-- Should be able to construct ModDetails from mi_decls in ModIface
data ModDetails
   = ModDetails {
	-- The next three fields are created by the typechecker
        md_types    :: !TypeEnv,
        md_insts    :: ![DFunId],	-- Dfun-ids for the instances in this module
        md_rules    :: ![IdCoreRule]	-- Domain may include Ids from other modules
     }



-- A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a ModIface and 
-- ModDetails are extracted and the ModGuts is dicarded.

data ModGuts
  = ModGuts {
        mg_module   :: !Module,
	mg_exports  :: !Avails,			-- What it exports
	mg_usages   :: ![ImportVersion Name],	-- What it imports, directly or otherwise
						-- ...exactly as in ModIface
	mg_dir_imps :: ![Module],		-- Directly imported modules

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


data IfaceDecls = IfaceDecls { dcl_tycl  :: [RenamedTyClDecl],	-- Sorted
			       dcl_rules :: [RenamedRuleDecl],	-- Sorted
			       dcl_insts :: [RenamedInstDecl] }	-- Unsorted

mkIfaceDecls :: [RenamedTyClDecl] -> [RenamedRuleDecl] -> [RenamedInstDecl] -> IfaceDecls
mkIfaceDecls tycls rules insts
  = IfaceDecls { dcl_tycl  = sortLt lt_tycl tycls,
		 dcl_rules = sortLt lt_rule rules,
		 dcl_insts = insts }
  where
    d1 `lt_tycl` d2 = tyClDeclName      d1 < tyClDeclName      d2
    r1 `lt_rule` r2 = ifaceRuleDeclName r1 < ifaceRuleDeclName r2
\end{code}

\begin{code}
emptyModIface :: Module -> ModIface
emptyModIface mod
  = ModIface { mi_module   = mod,
	       mi_package  = preludePackage, -- XXX fully bogus
	       mi_version  = initialVersionInfo,
	       mi_usages   = [],
	       mi_orphan   = False,
	       mi_boot	   = False,
	       mi_exports  = [],
	       mi_fixities = emptyNameEnv,
	       mi_globals  = Nothing,
	       mi_deprecs  = NoDeprecs,
	       mi_decls    = panic "emptyModIface: decls"
    }		
\end{code}


%************************************************************************
%*									*
		Parsed interface files
%*									*
%************************************************************************

A ParsedIface is exactly as read from an interface file.

\begin{code}
type IfaceDeprecs = Maybe (Either DeprecTxt [(RdrName,DeprecTxt)])
	-- Nothing	  => NoDeprecs
	-- Just (Left t)  => DeprecAll
	-- Just (Right p) => DeprecSome

data ParsedIface
  = ParsedIface {
      pi_mod	   :: ModuleName,
      pi_pkg       :: PackageName,
      pi_vers	   :: Version,		 		-- Module version number
      pi_orphan    :: WhetherHasOrphans,		-- Whether this module has orphans
      pi_usages	   :: [ImportVersion OccName],		-- Usages
      pi_exports   :: (Version, [RdrExportItem]),	-- Exports
      pi_decls	   :: [(Version, TyClDecl RdrName)],	-- Local definitions
      pi_fixity	   :: [FixitySig RdrName],		-- Local fixity declarations,
      pi_insts	   :: [InstDecl RdrName],		-- Local instance declarations
      pi_rules	   :: (Version, [RuleDecl RdrName]),	-- Rules, with their version
      pi_deprecs   :: IfaceDeprecs			-- Deprecations
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
			 ic_rn_gbl_env = emptyRdrEnv,
			 ic_rn_local_env = emptyRdrEnv,
			 ic_type_env = emptyTypeEnv }

icPrintUnqual :: InteractiveContext -> PrintUnqualified
icPrintUnqual ictxt = unQualInScope (ic_rn_gbl_env ictxt)
\end{code}


%************************************************************************
%*									*
\subsection{Type environment stuff}
%*									*
%************************************************************************

\begin{code}
data TyThing = AnId   Id
	     | ATyCon TyCon
	     | AClass Class

isTyClThing :: TyThing -> Bool
isTyClThing (ATyCon _) = True
isTyClThing (AClass _) = True
isTyClThing (AnId   _) = False

instance NamedThing TyThing where
  getName (AnId id)   = getName id
  getName (ATyCon tc) = getName tc
  getName (AClass cl) = getName cl

instance Outputable TyThing where
  ppr (AnId   id) = ptext SLIT("AnId")   <+> ppr id
  ppr (ATyCon tc) = ptext SLIT("ATyCon") <+> ppr tc
  ppr (AClass cl) = ptext SLIT("AClass") <+> ppr cl


typeEnvElts    :: TypeEnv -> [TyThing]
typeEnvClasses :: TypeEnv -> [Class]
typeEnvTyCons  :: TypeEnv -> [TyCon]
typeEnvIds     :: TypeEnv -> [Id]

typeEnvElts    env = nameEnvElts env
typeEnvClasses env = [cl | AClass cl <- typeEnvElts env]
typeEnvTyCons  env = [tc | ATyCon tc <- typeEnvElts env] 
typeEnvIds     env = [id | AnId id   <- typeEnvElts env] 

implicitTyThingIds :: [TyThing] -> [Id]
-- Add the implicit data cons and selectors etc 
implicitTyThingIds things
  = concat (map go things)
  where
    go (AnId f)    = []
    go (AClass cl) = classSelIds cl
    go (ATyCon tc) = tyConGenIds tc ++
		     tyConSelIds tc ++
		     [ n | dc <- tyConDataCons_maybe tc `orElse` [],
			   n  <- implicitConIds tc dc]
		-- Synonyms return empty list of constructors and selectors

    implicitConIds tc dc	-- Newtypes have a constructor wrapper,
				-- but no worker
	| isNewTyCon tc = [dataConWrapId dc]
	| otherwise     = [dataConWorkId dc, dataConWrapId dc]
\end{code}


\begin{code}
type TypeEnv = NameEnv TyThing

emptyTypeEnv = emptyNameEnv

mkTypeEnv :: [TyThing] -> TypeEnv
mkTypeEnv things = extendTypeEnvList emptyTypeEnv things
		
extendTypeEnvList :: TypeEnv -> [TyThing] -> TypeEnv
extendTypeEnvList env things
  = extendNameEnvList env [(getName thing, thing) | thing <- things]

extendTypeEnvWithIds :: TypeEnv -> [Id] -> TypeEnv
extendTypeEnvWithIds env ids
  = extendNameEnvList env [(getName id, AnId id) | id <- ids]
\end{code}

\begin{code}
lookupType :: HomePackageTable -> PackageTypeEnv -> Name -> Maybe TyThing
lookupType hpt pte name
  = case lookupModuleEnv hpt (nameModule name) of
	Just details -> lookupNameEnv (md_types (hm_details details)) name
	Nothing	     -> lookupNameEnv pte name
\end{code}

%************************************************************************
%*									*
\subsection{Auxiliary types}
%*									*
%************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere

\begin{code}
data VersionInfo 
  = VersionInfo {
	vers_module  :: Version,	-- Changes when anything changes
	vers_exports :: Version,	-- Changes when export list changes
	vers_rules   :: Version,	-- Changes when any rule changes
	vers_decls   :: NameEnv Version
		-- Versions for "big" names only (not data constructors, class ops)
		-- The version of an Id changes if its fixity changes
		-- Ditto data constructors, class operations, except that the version of
		-- the parent class/tycon changes
		--
		-- If a name isn't in the map, it means 'initialVersion'
    }

initialVersionInfo :: VersionInfo
initialVersionInfo = VersionInfo { vers_module  = initialVersion,
				   vers_exports = initialVersion,
				   vers_rules   = initialVersion,
				   vers_decls   = emptyNameEnv
			}

lookupVersion :: NameEnv Version -> Name -> Version
lookupVersion env name = lookupNameEnv env name `orElse` initialVersion

data Deprecations = NoDeprecs
	 	  | DeprecAll DeprecTxt				-- Whole module deprecated
		  | DeprecSome (NameEnv (Name,DeprecTxt))	-- Some things deprecated
								-- Just "big" names
		-- We keep the Name in the range, so we can print them out

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

instance Eq Deprecations where
  -- Used when checking whether we need write a new interface
  NoDeprecs       == NoDeprecs	     = True
  (DeprecAll t1)  == (DeprecAll t2)  = t1 == t2
  (DeprecSome e1) == (DeprecSome e2) = nameEnvElts e1 == nameEnvElts e2
  d1		  == d2		     = False
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

type RdrExportItem = (ModuleName, [RdrAvailInfo])
type ExportItem    = (ModuleName, [AvailInfo])

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
type FixityEnv = NameEnv (FixitySig Name)
	-- We keep the whole fixity sig so that we
	-- can report line-number info when there is a duplicate
	-- fixity declaration

emptyFixityEnv :: FixityEnv
emptyFixityEnv = emptyNameEnv

lookupFixity :: FixityEnv -> Name -> Fixity
lookupFixity env n = case lookupNameEnv env n of
			Just (FixitySig _ fix _) -> fix
			Nothing			 -> defaultFixity

collectFixities :: FixityEnv -> [TyClDecl Name] -> [FixitySig Name]
-- Collect fixities for the specified declarations
collectFixities env decls
  = [ fix
    | d <- decls, (n,_) <- tyClDeclNames d,
      Just fix <- [lookupNameEnv env n]
    ]
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

type IsBootInterface     = Bool

type ImportVersion name  = (ModuleName, WhetherHasOrphans, IsBootInterface, WhatsImported name)

data WhatsImported name  = NothingAtAll			-- The module is below us in the
							-- hierarchy, but we import nothing
							-- Used for orphan modules, so they appear
							-- in the usage list

			 | Everything Version		-- Used for modules from other packages;
							-- we record only the module's version number

			 | Specifically 
				Version			-- Module version
				(Maybe Version)		-- Export-list version, if we depend on it
				[(name,Version)]	-- List guaranteed non-empty
				Version			-- Rules version

			 deriving( Eq )
	-- 'Specifically' doesn't let you say "I imported f but none of the rules in
	-- the module". If you use anything in the module you get its rule version
	-- So if the rules change, you'll recompile, even if you don't use them.
	-- This is easy to implement, and it's safer: you might not have used the rules last
	-- time round, but if someone has added a new rule you might need it this time

	-- The export list field is (Just v) if we depend on the export list:
	--	we imported the module without saying exactly what we imported
	-- We need to recompile if the module exports changes, because we might
	-- now have a name clash in the importing module.
\end{code}


%************************************************************************
%*									*
\subsection{The persistent compiler state}
%*									*
%************************************************************************

The @PersistentCompilerState@ persists across successive calls to the
compiler.

\begin{code}
data PersistentCompilerState 
   = PCS {
	pcs_nc  :: !NameCache,
        pcs_EPS :: !ExternalPackageState
     }
\end{code}


\begin{code}
type PackageTypeEnv  = TypeEnv
type PackageRuleBase = RuleBase
type PackageInstEnv  = InstEnv

data ExternalPackageState
  = EPS {
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

	eps_imp_mods :: !ImportedModuleInfo,
		-- Modules that we know something about, because they are mentioned
		-- in interface files, BUT which we have not loaded yet.  
		-- No module is both in here and in the PIT

	eps_PTE :: !PackageTypeEnv,		-- Domain = external-package modules

	eps_inst_env :: !PackageInstEnv,	-- The total InstEnv accumulated from
						--   all the external-package modules
	eps_rule_base :: !PackageRuleBase,	-- Ditto RuleEnv


	-- Holding pens for stuff that has been read in from file,
	-- but not yet slurped into the renamer
	eps_decls      :: !DeclsMap,
		-- A single, global map of Names to unslurped decls
	eps_insts      :: !IfaceInsts,
		-- The as-yet un-slurped instance decls; this bag is depleted when we
		-- slurp an instance decl so that we don't slurp the same one twice.
		-- Each is 'gated' by the names that must be available before
		-- this instance decl is needed.
	eps_rules      :: !IfaceRules,
		-- Similar to instance decls, only for rules

	eps_inst_gates :: !NameSet	-- Gates for instance decls
		-- The instance gates must accumulate across
		-- all invocations of the renamer; 
		-- see "the gating story" in RnIfaces.lhs
		-- These names should all be from other packages;
		-- for the home package we have all the instance
		-- declarations anyhow
  }
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

type OrigNameCache   = FiniteMap (ModuleName,OccName) Name
type OrigIParamCache = FiniteMap (IPName RdrName) (IPName Name)
\end{code}

@ImportedModuleInfo@ contains info ONLY about modules that have not yet 
been loaded into the iPIT.  These modules are mentioned in interfaces we've
already read, so we know a tiny bit about them, but we havn't yet looked
at the interface file for the module itself.  It needs to persist across 
invocations of the renamer, at least from Rename.checkOldIface to Rename.renameSource.
And there's no harm in it persisting across multiple compilations.

\begin{code}
type ImportedModuleInfo 
    = FiniteMap ModuleName (WhetherHasOrphans, IsBootInterface)
\end{code}

A DeclsMap contains a binding for each Name in the declaration
including the constructors of a type decl etc.  The Bool is True just
for the 'main' Name.

\begin{code}
type DeclsMap = (NameEnv (AvailInfo, Bool, (Module, TyClDecl RdrName)), Int)
						-- The Int says how many have been sucked in

type IfaceInsts = GatedDecls (InstDecl RdrName)
type IfaceRules = GatedDecls (RuleDecl RdrName)

type GatedDecls d = (Bag (GatedDecl d), Int)	-- The Int says how many have been sucked in
type GatedDecl  d = (GateFn, (Module, d))
type GateFn       = (Name -> Bool) -> Bool	-- Returns True <=> gate is open
						-- The (Name -> Bool) fn returns True for visible Names
	-- For example, suppose this is in an interface file
	--	instance C T where ...
	-- We want to slurp this decl if both C and T are "visible" in 
	-- the importing module.  See "The gating story" in RnIfaces for details.
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
  linkableModName  :: ModuleName,	-- Should be Module, but see below
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


%************************************************************************
%*									*
\subsection{Provenance and export info}
%*									*
%************************************************************************

A LocalRdrEnv is used for local bindings (let, where, lambda, case)
Also used in 

\begin{code}
type LocalRdrEnv = RdrNameEnv Name

extendLocalRdrEnv :: LocalRdrEnv -> [Name] -> LocalRdrEnv
extendLocalRdrEnv env names
  = addListToRdrEnv env [(mkRdrUnqual (nameOccName n), n) | n <- names]
\end{code}

The GlobalRdrEnv gives maps RdrNames to Names.  There is a separate
one for each module, corresponding to that module's top-level scope.

\begin{code}
type GlobalRdrEnv = RdrNameEnv [GlobalRdrElt]
	-- The list is because there may be name clashes
	-- These only get reported on lookup, not on construction

emptyGlobalRdrEnv = emptyRdrEnv

data GlobalRdrElt 
  = GRE { gre_name   :: Name,
	  gre_parent :: Name,	-- Name of the "parent" structure
				-- 	* the tycon of a data con
				--	* the class of a class op
				-- For others it's just the same as gre_name
	  gre_prov   :: Provenance,		-- Why it's in scope
	  gre_deprec :: Maybe DeprecTxt		-- Whether this name is deprecated
    }

instance Outputable GlobalRdrElt where
  ppr gre = ppr (gre_name gre) <+> 
	    parens (hsep [text "parent:" <+> ppr (gre_parent gre) <> comma,
			  pprNameProvenance gre])
pprGlobalRdrEnv env
  = vcat (map pp (rdrEnvToList env))
  where
    pp (rn, gres) = ppr rn <> colon <+> 
		    vcat [ ppr (gre_name gre) <+> pprNameProvenance gre
			 | gre <- gres]

isLocalGRE :: GlobalRdrElt -> Bool
isLocalGRE (GRE {gre_prov = LocalDef}) = True
isLocalGRE other    		       = False
\end{code}

@unQualInScope@ returns a function that takes a @Name@ and tells whether
its unqualified name is in scope.  This is put as a boolean flag in
the @Name@'s provenance to guide whether or not to print the name qualified
in error messages.

\begin{code}
unQualInScope :: GlobalRdrEnv -> Name -> Bool
-- True if 'f' is in scope, and has only one binding,
-- and the thing it is bound to is the name we are looking for
-- (i.e. false if A.f and B.f are both in scope as unqualified 'f')
--
-- Also checks for built-in syntax, which is always 'in scope'
--
-- This fn is only efficient if the shared 
-- partial application is used a lot.
unQualInScope env
  = \n -> n `elemNameSet` unqual_names || isBuiltInSyntaxName n
  where
    unqual_names :: NameSet
    unqual_names = foldRdrEnv add emptyNameSet env
    add rdr_name [gre] unquals | isUnqual rdr_name = addOneToNameSet unquals (gre_name gre)
    add _        _     unquals		 	   = unquals
\end{code}

The "provenance" of something says how it came to be in scope.

\begin{code}
data Provenance
  = LocalDef			-- Defined locally

  | NonLocalDef  		-- Defined non-locally
	ImportReason

-- Just used for grouping error messages (in RnEnv.warnUnusedBinds)
instance Eq Provenance where
  p1 == p2 = case p1 `compare` p2 of EQ -> True; _ -> False

instance Eq ImportReason where
  p1 == p2 = case p1 `compare` p2 of EQ -> True; _ -> False

instance Ord Provenance where
   compare LocalDef LocalDef = EQ
   compare LocalDef (NonLocalDef _) = LT
   compare (NonLocalDef _) LocalDef = GT

   compare (NonLocalDef reason1) (NonLocalDef reason2) 
      = compare reason1 reason2

instance Ord ImportReason where
   compare ImplicitImport ImplicitImport = EQ
   compare ImplicitImport (UserImport _ _ _) = LT
   compare (UserImport _ _ _) ImplicitImport = GT
   compare (UserImport m1 loc1 _) (UserImport m2 loc2 _) 
      = (m1 `compare` m2) `thenCmp` (loc1 `compare` loc2)


data ImportReason
  = UserImport Module SrcLoc Bool	-- Imported from module M on line L
					-- Note the M may well not be the defining module
					-- for this thing!
	-- The Bool is true iff the thing was named *explicitly* in the import spec,
	-- rather than being imported as part of a group; e.g.
	--	import B
	--	import C( T(..) )
	-- Here, everything imported by B, and the constructors of T
	-- are not named explicitly; only T is named explicitly.
	-- This info is used when warning of unused names.

  | ImplicitImport			-- Imported implicitly for some other reason
\end{code}

\begin{code}
hasBetterProv :: Provenance -> Provenance -> Bool
-- Choose 
--	a local thing		      over an	imported thing
--	a user-imported thing	      over a	non-user-imported thing
-- 	an explicitly-imported thing  over an	implicitly imported thing
hasBetterProv LocalDef 				  _			       = True
hasBetterProv (NonLocalDef (UserImport _ _ _   )) (NonLocalDef ImplicitImport) = True
hasBetterProv _					  _			       = False

pprNameProvenance :: GlobalRdrElt -> SDoc
pprNameProvenance (GRE {gre_name = name, gre_prov = prov})
  = case prov of
	LocalDef	-> ptext SLIT("defined at") <+> ppr (nameSrcLoc name)
	NonLocalDef why ->  sep [ppr_reason why, 
				 nest 2 (ppr_defn (nameSrcLoc name))]

ppr_reason ImplicitImport	  = ptext SLIT("implicitly imported")
ppr_reason (UserImport mod loc _) = ptext SLIT("imported from") <+> ppr mod <+> ptext SLIT("at") <+> ppr loc

ppr_defn loc | isGoodSrcLoc loc = parens (ptext SLIT("at") <+> ppr loc)
	     | otherwise	= empty
\end{code}
