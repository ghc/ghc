%
% (c) The University of Glasgow, 2000
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes ( 
	ModDetails(..),	GlobalSymbolTable, 
	HomeSymbolTable, PackageSymbolTable,

	TyThing(..), lookupTypeEnv,

	WhetherHasOrphans, ImportVersion, ExportItem,
	PersistentRenamerState(..), IsBootInterface, Avails, DeclsMap,
	IfaceInsts, IfaceRules, DeprecationEnv, OrigNameEnv, 
	AvailEnv, AvailInfo, GenAvailInfo(..),
	PersistentCompilerState(..),

	InstEnv, ClsInstEnv, DFunId,

	GlobalRdrEnv, RdrAvailInfo,

	CompResult(..), HscResult(..),

	-- Provenance
	Provenance(..), ImportReason(..), PrintUnqualified,
        pprNameProvenance, hasBetterProv

    ) where

#include "HsVersions.h"

import Name		( Name, NameEnv, NamedThing,
			  unitNameEnv, extendNameEnv, plusNameEnv, 
			  lookupNameEnv, emptyNameEnv, getName, nameModule,
			  nameSrcLoc )
import Module		( Module, ModuleName,
			  extendModuleEnv, lookupModuleEnv )
import Class		( Class )
import OccName		( OccName )
import RdrName		( RdrNameEnv, emptyRdrEnv )
import Outputable	( SDoc )
import UniqFM 		( UniqFM )
import FiniteMap	( FiniteMap, emptyFM, addToFM, lookupFM, foldFM )
import Bag		( Bag )
import Id		( Id )
import VarEnv		( IdEnv, emptyVarEnv )
import BasicTypes	( Version, Fixity, defaultFixity )
import TyCon		( TyCon )
import ErrUtils		( ErrMsg, WarnMsg )
import CmLink		( Linkable )
import RdrHsSyn		( RdrNameInstDecl, RdrNameRuleDecl, RdrNameHsDecl,
			  RdrNameDeprecation, RdrNameFixitySig )
import InterpSyn	( UnlinkedIBind )
import UniqSupply	( UniqSupply )
import HsDecls		( DeprecTxt )
import CoreSyn		( CoreRule )
import NameSet		( NameSet )
import Type		( Type )
import VarSet		( TyVarSet )
import Panic		( panic )
import Outputable
import SrcLoc		( SrcLoc, isGoodSrcLoc )
\end{code}

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
data ModDetails
   = ModDetails {
        md_module   :: Module,			-- Complete with package info
        md_version  :: VersionInfo,		-- Module version number
        md_orphan   :: WhetherHasOrphans,       -- Whether this module has orphans
        md_usages   :: [ImportVersion Name],	-- Usages

        md_exports  :: Avails,			-- What it exports
        md_globals  :: GlobalRdrEnv,		-- Its top level environment

        md_fixities :: NameEnv Fixity,		-- Fixities
	md_deprecs  :: NameEnv DeprecTxt,	-- Deprecations

	-- The next three fields are created by the typechecker
        md_types    :: TypeEnv,
        md_insts    :: [DFunId],	-- Dfun-ids for the instances in this module
        md_rules    :: RuleEnv		-- Domain may include Ids from other modules
     }

-- ModIFace is nearly the same as RnMonad.ParsedIface.
-- Right now it's identical :)
data ModIFace 
   = ModIFace {
        mi_mod       :: Module,                   -- Complete with package info
        mi_vers      :: Version,                  -- Module version number
        mi_orphan    :: WhetherHasOrphans,        -- Whether this module has orphans
        mi_usages    :: [ImportVersion OccName],  -- Usages
        mi_exports   :: [ExportItem],             -- Exports
        mi_insts     :: [RdrNameInstDecl],        -- Local instance declarations
        mi_decls     :: [(Version, RdrNameHsDecl)],    -- Local definitions
        mi_fixity    :: (Version, [RdrNameFixitySig]), -- Local fixity declarations, 
                                                       -- with their version
        mi_rules     :: (Version, [RdrNameRuleDecl]),  -- Rules, with their version
        mi_deprecs   :: [RdrNameDeprecation]           -- Deprecations
     }

\end{code}

\begin{code}
emptyModDetails :: Module -> ModDetails
emptyModDetails mod
  = ModDetails { md_module   = mod,
		 md_exports  = [],
		 md_globals  = emptyRdrEnv,
		 md_fixities = emptyNameEnv,
		 md_deprecs  = emptyNameEnv,
		 md_types    = emptyNameEnv,
		 md_insts    = [],
    		 md_rules    = emptyRuleEnv
    }		
\end{code}

Symbol tables map modules to ModDetails:

\begin{code}
type SymbolTable	= ModuleEnv ModDetails
type HomeSymbolTable    = SymbolTable	-- Domain = modules in the home package
type PackageSymbolTable = SymbolTable	-- Domain = modules in the some other package
type GlobalSymbolTable  = SymbolTable	-- Domain = all modules
\end{code}

Simple lookups in the symbol table.

\begin{code}
lookupFixityEnv :: SymbolTable -> Name -> Maybe Fixity
	-- Returns defaultFixity if there isn't an explicit fixity
lookupFixityEnv tbl name
  = case lookupModuleEnv tbl (nameModule name) of
	Nothing	     -> Nothing
	Just details -> lookupNameEnv (md_fixities details) name
\end{code}


%************************************************************************
%*									*
\subsection{Type environment stuff}
%*									*
%************************************************************************

\begin{code}
type TypeEnv = NameEnv TyThing

data TyThing = AnId   Id
	     | ATyCon TyCon
	     | AClass Class

instance NamedThing TyThing where
  getName (AnId id)   = getName id
  getName (ATyCon tc) = getName tc
  getName (AClass cl) = getName cl
\end{code}


\begin{code}
lookupTypeEnv :: SymbolTable -> Name -> Maybe TyThing
lookupTypeEnv tbl name
  = case lookupModuleEnv tbl (nameModule name) of
	Just details -> lookupNameEnv (md_types details) name
	Nothing	     -> Nothing


groupTyThings :: [TyThing] -> FiniteMap Module TypeEnv
  -- Finite map because we want the range too
groupTyThings things
  = foldl add emptyFM things
  where
    add :: FiniteMap Module TypeEnv -> TyThing -> FiniteMap Module TypeEnv
    add tbl thing = addToFM tbl mod new_env
		  where
		    name    = getName thing
		    mod     = nameModule name
		    new_env = case lookupFM tbl mod of
				Nothing  -> unitNameEnv name thing
				Just env -> extendNameEnv env name thing
		
extendTypeEnv :: SymbolTable -> FiniteMap Module TypeEnv -> SymbolTable
extendTypeEnv tbl things
  = foldFM add tbl things
  where
    add mod type_env tbl
	= panic "extendTypeEnv" --extendModuleEnv mod new_details
	where
	  new_details 
             = case lookupModuleEnv tbl mod of
                  Nothing      -> (emptyModDetails mod) {md_types = type_env}
                  Just details -> details {md_types = md_types details 
                                                     `plusNameEnv` type_env}
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
	modVers  :: Version,
	fixVers  :: Version,
	ruleVers :: Version,
	declVers :: NameEnv Version
    }

type DeprecationEnv = NameEnv DeprecTxt		-- Give reason for deprecation

type InstEnv    = UniqFM ClsInstEnv		-- Maps Class to instances for that class
type ClsInstEnv = [(TyVarSet, [Type], DFunId)]	-- The instances for a particular class
type DFunId	= Id

type RuleEnv    = IdEnv [CoreRule]

emptyRuleEnv    = emptyVarEnv
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
\end{code}


%************************************************************************
%*									*
\subsection{ModIface}
%*									*
%************************************************************************

\begin{code}
type ExportItem		 = (ModuleName, [RdrAvailInfo])

type ImportVersion name  = (ModuleName, WhetherHasOrphans, IsBootInterface, WhatsImported name)

type ModVersionInfo	= (Version,		-- Version of the whole module
			   Version,		-- Version number for all fixity decls together
			   Version)		-- ...ditto all rules together

type WhetherHasOrphans   = Bool
	-- An "orphan" is 
	-- 	* an instance decl in a module other than the defn module for 
	--		one of the tycons or classes in the instance head
	--	* a transformation rule in a module other than the one defining
	--		the function in the head of the rule.

type IsBootInterface     = Bool

data WhatsImported name  = NothingAtAll				-- The module is below us in the
								-- hierarchy, but we import nothing

			 | Everything Version			-- The module version

			 | Specifically Version			-- Module version
					Version			-- Fixity version
					Version			-- Rules version
					[(name,Version)]	-- List guaranteed non-empty
			 deriving( Eq )
	-- 'Specifically' doesn't let you say "I imported f but none of the fixities in
	-- the module". If you use anything in the module you get its fixity and rule version
	-- So if the fixities or rules change, you'll recompile, even if you don't use either.
	-- This is easy to implement, and it's safer: you might not have used the rules last
	-- time round, but if someone has added a new rule you might need it this time

	-- 'Everything' means there was a "module M" in 
	-- this module's export list, so we just have to go by M's version,
	-- not the list of (name,version) pairs
\end{code}


%************************************************************************
%*									*
\subsection{The persistent compiler state}
%*									*
%************************************************************************

\begin{code}
data PersistentCompilerState 
   = PCS {
        pcs_PST :: PackageSymbolTable,	-- Domain = non-home-package modules
					--   except that the InstEnv components is empty
	pcs_insts :: InstEnv,		-- The total InstEnv accumulated from all
					--   the non-home-package modules
	pcs_rules :: RuleEnv,		-- Ditto RuleEnv

        pcs_PRS :: PersistentRenamerState
     }
\end{code}

The @PersistentRenamerState@ persists across successive calls to the
compiler.

It contains:
  * A name supply, which deals with allocating unique names to
    (Module,OccName) original names, 
 
  * An accumulated InstEnv from all the modules in pcs_PST
    The point is that we don't want to keep recreating it whenever
    we compile a new module.  The InstEnv component of pcPST is empty.
    (This means we might "see" instances that we shouldn't "really" see;
    but the Haskell Report is vague on what is meant to be visible, 
    so we just take the easy road here.)

  * Ditto for rules

  * A "holding pen" for declarations that have been read out of
    interface files but not yet sucked in, renamed, and typechecked

\begin{code}
data PersistentRenamerState
  = PRS { prsOrig  :: OrigNameEnv,
	  prsDecls :: DeclsMap,
	  prsInsts :: IfaceInsts,
	  prsRules :: IfaceRules
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
 = Orig { origNames  :: FiniteMap (ModuleName,OccName) Name,	-- Ensures that one original name gets one unique
	  origIParam :: FiniteMap OccName Name			-- Ensures that one implicit parameter name gets one unique
   }
\end{code}


A DeclsMap contains a binding for each Name in the declaration
including the constructors of a type decl etc.  The Bool is True just
for the 'main' Name.

\begin{code}
type DeclsMap = NameEnv (AvailInfo, Bool, (Module, RdrNameHsDecl))

type IfaceInsts = Bag GatedDecl
type IfaceRules = Bag GatedDecl

type GatedDecl = (NameSet, (Module, RdrNameHsDecl))
\end{code}


%************************************************************************
%*									*
\subsection{The result of compiling one module}
%*									*
%************************************************************************

\begin{code}
data CompResult
   = CompOK   ModDetails  -- new details (HST additions)
              (Maybe (ModIFace, Linkable))
                       -- summary and code; Nothing => compilation not reqd
                       -- (old summary and code are still valid)
              PersistentCompilerState	-- updated PCS
              (Bag WarnMsg) 		-- warnings

   | CompErrs PersistentCompilerState	-- updated PCS
              (Bag ErrMsg)		-- errors
              (Bag WarnMsg)             -- warnings


-- The driver sits between 'compile' and 'hscMain', translating calls
-- to the former into calls to the latter, and results from the latter
-- into results from the former.  It does things like preprocessing
-- the .hs file if necessary, and compiling up the .stub_c files to
-- generate Linkables.

data HscResult
   = HscOK   ModDetails  	     -- new details (HomeSymbolTable additions)
	     (Maybe ModIFace)	     -- new iface (if any compilation was done)
	     (Maybe String)  	     -- generated stub_h filename (in /tmp)
	     (Maybe String)  	     -- generated stub_c filename (in /tmp)
	     (Maybe [UnlinkedIBind]) -- interpreted code, if any
             PersistentCompilerState -- updated PCS
             (Bag WarnMsg) 		-- warnings

   | HscErrs PersistentCompilerState -- updated PCS
             (Bag ErrMsg)		-- errors
             (Bag WarnMsg)             -- warnings

-- These two are only here to avoid recursion between CmCompile and
-- CompManager.  They really ought to be in the latter.
type ModuleEnv a = UniqFM a   -- Domain is Module

type HomeModMap         = FiniteMap ModuleName Module -- domain: home mods only
type HomeInterfaceTable = ModuleEnv ModIFace
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
\end{code}

The "provenance" of something says how it came to be in scope.

\begin{code}
data Provenance
  = LocalDef			-- Defined locally

  | NonLocalDef  		-- Defined non-locally
	ImportReason
	PrintUnqualified

{-
Moved here from Name.
pp_prov (LocalDef _ Exported)          = char 'x'
pp_prov (LocalDef _ NotExported)       = char 'l'
pp_prov (NonLocalDef ImplicitImport _) = char 'j'
pp_prov (NonLocalDef (UserImport _ _ True ) _) = char 'I'	-- Imported by name
pp_prov (NonLocalDef (UserImport _ _ False) _) = char 'i'	-- Imported by ..
pp_prov SystemProv	     	       = char 's'
-}

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
			

type PrintUnqualified = Bool	-- True <=> the unqualified name of this thing is
				-- in scope in this module, so print it 
				-- unqualified in error messages
\end{code}

\begin{code}
hasBetterProv :: Provenance -> Provenance -> Bool
-- Choose 
--	a local thing		      over an	imported thing
--	a user-imported thing	      over a	non-user-imported thing
-- 	an explicitly-imported thing  over an	implicitly imported thing
hasBetterProv LocalDef 				    _				   = True
hasBetterProv (NonLocalDef (UserImport _ _ True) _) _				   = True
hasBetterProv (NonLocalDef (UserImport _ _ _   ) _) (NonLocalDef ImplicitImport _) = True
hasBetterProv _					    _				   = False

pprNameProvenance :: Name -> Provenance -> SDoc
pprNameProvenance name LocalDef   	       = ptext SLIT("defined at") <+> ppr (nameSrcLoc name)
pprNameProvenance name (NonLocalDef why _) = sep [ppr_reason why, 
					      nest 2 (parens (ppr_defn (nameSrcLoc name)))]

ppr_reason ImplicitImport	  = ptext SLIT("implicitly imported")
ppr_reason (UserImport mod loc _) = ptext SLIT("imported from") <+> ppr mod <+> ptext SLIT("at") <+> ppr loc

ppr_defn loc | isGoodSrcLoc loc = ptext SLIT("at") <+> ppr loc
	     | otherwise	= empty
\end{code}
