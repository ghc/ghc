%
% (c) The University of Glasgow, 2000
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes ( 
	ModuleLocation(..),

	ModDetails(..),	ModIface(..), 
	HomeSymbolTable, PackageTypeEnv,
	HomeIfaceTable, PackageIfaceTable, emptyIfaceTable,
	lookupIface, lookupIfaceByModName,
	emptyModIface,

	IfaceDecls, mkIfaceDecls, dcl_tycl, dcl_rules, dcl_insts,

	VersionInfo(..), initialVersionInfo,

	TyThing(..), isTyClThing,

	TypeEnv, lookupType, mkTypeEnv, extendTypeEnvList, 
	typeEnvClasses, typeEnvTyCons,

	WhetherHasOrphans, ImportVersion, WhatsImported(..),
	PersistentRenamerState(..), IsBootInterface, Avails, DeclsMap,
	IfaceInsts, IfaceRules, GatedDecl, IsExported,
	OrigNameEnv(..), OrigNameNameEnv, OrigNameIParamEnv,
	AvailEnv, AvailInfo, GenAvailInfo(..),
	PersistentCompilerState(..),

	Deprecations(..), lookupDeprec,

	InstEnv, ClsInstEnv, DFunId,
	PackageInstEnv, PackageRuleBase,

	GlobalRdrEnv, RdrAvailInfo, pprGlobalRdrEnv,

	-- Provenance
	Provenance(..), ImportReason(..), 
        pprNameProvenance, hasBetterProv

    ) where

#include "HsVersions.h"

import RdrName		( RdrNameEnv, emptyRdrEnv, rdrEnvToList )
import Name		( Name, NamedThing, getName, nameModule, nameSrcLoc )
import Name -- Env
import OccName		( OccName )
import Module		( Module, ModuleName, ModuleEnv,
			  lookupModuleEnv, lookupModuleEnvByName, emptyModuleEnv
			)
import InstEnv		( InstEnv, ClsInstEnv, DFunId )
import Rules		( RuleBase )
import Id		( Id )
import Class		( Class )
import TyCon		( TyCon )

import BasicTypes	( Version, initialVersion, Fixity )

import HsSyn		( DeprecTxt, tyClDeclName, ifaceRuleDeclName )
import RdrHsSyn		( RdrNameInstDecl, RdrNameRuleDecl, RdrNameTyClDecl )
import RnHsSyn		( RenamedTyClDecl, RenamedRuleDecl, RenamedInstDecl )

import CoreSyn		( IdCoreRule )

import FiniteMap	( FiniteMap )
import Bag		( Bag )
import Maybes		( seqMaybe )
import Outputable
import SrcLoc		( SrcLoc, isGoodSrcLoc )
import Util		( thenCmp, sortLt )
import UniqSupply	( UniqSupply )
\end{code}

%************************************************************************
%*									*
\subsection{Module locations}
%*									*
%************************************************************************

\begin{code}
data ModuleLocation
   = ModuleLocation {
        ml_hs_file   :: Maybe FilePath,
        ml_hspp_file :: Maybe FilePath,  -- path of preprocessed source
        ml_hi_file   :: Maybe FilePath,
        ml_obj_file  :: Maybe FilePath
     }
     deriving Show

instance Outputable ModuleLocation where
   ppr = text . show
\end{code}

For a module in another package, the hs_file and obj_file
components of ModuleLocation are undefined.  

The locations specified by a ModuleLocation may or may not
correspond to actual files yet: for example, even if the object
file doesn't exist, the ModuleLocation still contains the path to
where the object file will reside if/when it is created.


%************************************************************************
%*									*
\subsection{Symbol tables and Module details}
%*									*
%************************************************************************

A @ModIface@ plus a @ModDetails@ summarises everything we know 
about a compiled module.  The @ModIface@ is the stuff *before* linking,
and can be written out to an interface file.  The @ModDetails@ is after
linking; it is the "linked" form of the mi_decls field.

\begin{code}
data ModIface 
   = ModIface {
        mi_module   :: Module,			-- Complete with package info
        mi_version  :: VersionInfo,		-- Module version number
        mi_orphan   :: WhetherHasOrphans,       -- Whether this module has orphans
	mi_boot	    :: IsBootInterface,		-- Whether this interface was read from an hi-boot file

        mi_usages   :: [ImportVersion Name],	-- Usages; kept sorted so that it's easy
						-- to decide whether to write a new iface file
						-- (changing usages doesn't affect the version of
						--  this module)

        mi_exports  :: [(ModuleName,Avails)],	-- What it exports
						-- Kept sorted by (mod,occ),
						-- to make version comparisons easier

        mi_globals  :: GlobalRdrEnv,		-- Its top level environment

        mi_fixities :: NameEnv Fixity,		-- Fixities
	mi_deprecs  :: Deprecations,		-- Deprecations

	mi_decls    :: IfaceDecls		-- The RnDecls form of ModDetails
     }

data IfaceDecls = IfaceDecls { dcl_tycl  :: [RenamedTyClDecl],	-- Sorted
			       dcl_rules :: [RenamedRuleDecl],	-- Sorted
			       dcl_insts :: [RenamedInstDecl] }	-- Unsorted

mkIfaceDecls :: [RenamedTyClDecl] -> [RenamedRuleDecl] -> [RenamedInstDecl] -> IfaceDecls
mkIfaceDecls tycls rules insts
  = IfaceDecls { dcl_tycl  = sortLt lt_tycl tycls,
		 dcl_rules = sortLt lt_rule rules,
		 dcl_insts = insts }
  where
    d1 `lt_tycl` d2 = nameOccName (tyClDeclName      d1) < nameOccName (tyClDeclName      d2)
    r1 `lt_rule` r2 = nameOccName (ifaceRuleDeclName r1) < nameOccName (ifaceRuleDeclName r2)

	-- I wanted to sort just by the Name, but there's a problem: we are comparing
	-- the old version of an interface with the new version.  The latter will use
	-- local names like 'lvl23' that were constructed not by the renamer but by
	-- the simplifier.  So the unqiues aren't going to line up.
	--
	-- It's ok to compare by OccName because this comparison only drives the
	-- computation of new version numbers.
	--
	-- Better solutions: 	Compare in a way that is insensitive to the name used
	--			for local things.  This would decrease the wobbles due
	--			to 'lvl23' changing to 'lvl24'.
	--
	-- NB: there's a related comparision on MkIface.diffDecls!  




-- typechecker should only look at this, not ModIface
-- Should be able to construct ModDetails from mi_decls in ModIface
data ModDetails
   = ModDetails {
	-- The next three fields are created by the typechecker
        md_types    :: TypeEnv,
        md_insts    :: [DFunId],	-- Dfun-ids for the instances in this module
        md_rules    :: [IdCoreRule]	-- Domain may include Ids from other modules
     }
\end{code}

\begin{code}
emptyModDetails :: ModDetails
emptyModDetails
  = ModDetails { md_types = emptyTypeEnv,
                 md_insts = [],
                 md_rules = []
    }

emptyModIface :: Module -> ModIface
emptyModIface mod
  = ModIface { mi_module   = mod,
	       mi_version  = initialVersionInfo,
	       mi_usages   = [],
	       mi_orphan   = False,
	       mi_boot	   = False,
	       mi_exports  = [],
	       mi_fixities = emptyNameEnv,
	       mi_globals  = emptyRdrEnv,
	       mi_deprecs  = NoDeprecs,
	       mi_decls    = panic "emptyModIface: decls"
    }		
\end{code}

Symbol tables map modules to ModDetails:

\begin{code}
type SymbolTable	= ModuleEnv ModDetails
type IfaceTable		= ModuleEnv ModIface

type HomeIfaceTable     = IfaceTable
type PackageIfaceTable  = IfaceTable

type HomeSymbolTable    = SymbolTable	-- Domain = modules in the home package

emptyIfaceTable :: IfaceTable
emptyIfaceTable = emptyModuleEnv
\end{code}

Simple lookups in the symbol table.

\begin{code}
lookupIface :: HomeIfaceTable -> PackageIfaceTable -> Name -> Maybe ModIface
-- We often have two IfaceTables, and want to do a lookup
lookupIface hit pit name
  = lookupModuleEnv hit mod `seqMaybe` lookupModuleEnv pit mod
  where
    mod = nameModule name

lookupIfaceByModName :: HomeIfaceTable -> PackageIfaceTable -> ModuleName -> Maybe ModIface
-- We often have two IfaceTables, and want to do a lookup
lookupIfaceByModName hit pit mod
  = lookupModuleEnvByName hit mod `seqMaybe` lookupModuleEnvByName pit mod
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

typeEnvClasses env = [cl | AClass cl <- nameEnvElts env]
typeEnvTyCons  env = [tc | ATyCon tc <- nameEnvElts env] 

\end{code}


\begin{code}
type TypeEnv = NameEnv TyThing

emptyTypeEnv = emptyNameEnv

mkTypeEnv :: [TyThing] -> TypeEnv
mkTypeEnv things = extendTypeEnvList emptyTypeEnv things
		
extendTypeEnvList :: TypeEnv -> [TyThing] -> TypeEnv
extendTypeEnvList env things
  = foldl add_thing env things
  where
    add_thing :: TypeEnv -> TyThing -> TypeEnv
    add_thing env thing = extendNameEnv env (getName thing) thing
\end{code}

\begin{code}
lookupType :: HomeSymbolTable -> PackageTypeEnv -> Name -> Maybe TyThing
lookupType hst pte name
  = case lookupModuleEnv hst (nameModule name) of
	Just details -> lookupNameEnv (md_types details) name
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
    }

initialVersionInfo :: VersionInfo
initialVersionInfo = VersionInfo { vers_module  = initialVersion,
				   vers_exports = initialVersion,
				   vers_rules   = initialVersion,
				   vers_decls   = emptyNameEnv }

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

type AvailEnv	  = NameEnv AvailInfo	-- Maps a Name to the AvailInfo that contains it
				
instance Outputable n => Outputable (GenAvailInfo n) where
   ppr = pprAvail

pprAvail :: Outputable n => GenAvailInfo n -> SDoc
pprAvail (AvailTC n ns) = ppr n <> case {- filter (/= n) -} ns of
					[]  -> empty
					ns' -> braces (hsep (punctuate comma (map ppr ns')))

pprAvail (Avail n) = ppr n
\end{code}


%************************************************************************
%*									*
\subsection{ModIface}
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

data WhatsImported name  = NothingAtAll				-- The module is below us in the
								-- hierarchy, but we import nothing

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

type IsExported = Name -> Bool		-- True for names that are exported from this module
\end{code}


%************************************************************************
%*									*
\subsection{The persistent compiler state}
%*									*
%************************************************************************

\begin{code}
data PersistentCompilerState 
   = PCS {
        pcs_PIT :: PackageIfaceTable,	-- Domain = non-home-package modules
					--   the mi_decls component is empty

        pcs_PTE :: PackageTypeEnv,	-- Domain = non-home-package modules
					--   except that the InstEnv components is empty

	pcs_insts :: PackageInstEnv,	-- The total InstEnv accumulated from all
					--   the non-home-package modules

	pcs_rules :: PackageRuleBase,	-- Ditto RuleEnv

        pcs_PRS :: PersistentRenamerState
     }

\end{code}

The @PersistentRenamerState@ persists across successive calls to the
compiler.

It contains:
  * A name supply, which deals with allocating unique names to
    (Module,OccName) original names, 
 
  * An accumulated TypeEnv from all the modules in imported packages

  * An accumulated InstEnv from all the modules in imported packages
    The point is that we don't want to keep recreating it whenever
    we compile a new module.  The InstEnv component of pcPST is empty.
    (This means we might "see" instances that we shouldn't "really" see;
    but the Haskell Report is vague on what is meant to be visible, 
    so we just take the easy road here.)

  * Ditto for rules

  * A "holding pen" for declarations that have been read out of
    interface files but not yet sucked in, renamed, and typechecked

\begin{code}
type PackageTypeEnv  = TypeEnv
type PackageRuleBase = RuleBase
type PackageInstEnv  = InstEnv

data PersistentRenamerState
  = PRS { prsOrig  :: OrigNameEnv,
	  prsDecls :: DeclsMap,
	  prsInsts :: IfaceInsts,
	  prsRules :: IfaceRules,
	  prsNS    :: UniqSupply
    }
\end{code}

The OrigNameEnv makes sure that there is just one Unique assigned for
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
data OrigNameEnv
 = Orig { origNames  :: OrigNameNameEnv,
		-- Ensures that one original name gets one unique
	  origIParam :: OrigNameIParamEnv
		-- Ensures that one implicit parameter name gets one unique
   }

type OrigNameNameEnv   = FiniteMap (ModuleName,OccName) Name
type OrigNameIParamEnv = FiniteMap OccName Name
\end{code}


A DeclsMap contains a binding for each Name in the declaration
including the constructors of a type decl etc.  The Bool is True just
for the 'main' Name.

\begin{code}
type DeclsMap = (NameEnv (AvailInfo, Bool, (Module, RdrNameTyClDecl)), Int)
						-- The Int says how many have been sucked in

type IfaceInsts = GatedDecls RdrNameInstDecl
type IfaceRules = GatedDecls RdrNameRuleDecl

type GatedDecls d = (Bag (GatedDecl d), Int)	-- The Int says how many have been sucked in
type GatedDecl  d = ([Name], (Module, d))
\end{code}


%************************************************************************
%*									*
\subsection{Provenance and export info}
%*									*
%************************************************************************

The GlobalRdrEnv gives maps RdrNames to Names.  There is a separate
one for each module, corresponding to that module's top-level scope.

\begin{code}
type GlobalRdrEnv = RdrNameEnv [(Name,Provenance)]	-- The list is because there may be name clashes
							-- These only get reported on lookup,
							-- not on construction

pprGlobalRdrEnv env
  = vcat (map pp (rdrEnvToList env))
  where
    pp (rn, nps) = ppr rn <> colon <+> 
		   vcat [ppr n <+> pprNameProvenance n p | (n,p) <- nps]
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

pprNameProvenance :: Name -> Provenance -> SDoc
pprNameProvenance name LocalDef   	 = ptext SLIT("defined at") <+> ppr (nameSrcLoc name)
pprNameProvenance name (NonLocalDef why) = sep [ppr_reason why, 
					        nest 2 (parens (ppr_defn (nameSrcLoc name)))]

ppr_reason ImplicitImport	  = ptext SLIT("implicitly imported")
ppr_reason (UserImport mod loc _) = ptext SLIT("imported from") <+> ppr mod <+> ptext SLIT("at") <+> ppr loc

ppr_defn loc | isGoodSrcLoc loc = ptext SLIT("at") <+> ppr loc
	     | otherwise	= empty
\end{code}
