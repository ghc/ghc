%
% (c) The University of Glasgow, 2000
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes ( TyThing(..) )
where

#include "HsVersions.h"

import Name		( Name, NameEnv, NamedThing,
			  unitNameEnv, extendNameEnv, plusNameEnv, 
			  lookupNameEnv, emptyNameEnv, getName, nameModule )
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
import VarEnv		( IdEnv )
import BasicTypes	( Version, Fixity, defaultFixity )
import TyCon		( TyCon )
import ErrUtils		( ErrMsg, WarnMsg )
import CmLink		( Linkable )
import RdrHsSyn		( RdrNameInstDecl, RdrNameRuleDecl, RdrNameHsDecl,
			  RdrNameDeprecation, RdrNameFixitySig )
import UniqSupply	( UniqSupply )
import HsDecls		( DeprecTxt )
import CoreSyn		( CoreRule )
import NameSet		( NameSet )
import Type		( Type )
import VarSet		( TyVarSet )
import {-# SOURCE #-} TcInstUtil ( emptyInstEnv )
import Panic		( panic )
\end{code}

%************************************************************************
%*									*
\subsection{Symbol tables and Module details}
%*									*
%************************************************************************

A @ModDetails@ summarises everything we know about a compiled module.

\begin{code}
data ModDetails
   = ModDetails {
	moduleId      :: Module,
        moduleExports :: Avails,		-- What it exports
        moduleEnv     :: GlobalRdrEnv,		-- Its top level environment

        fixityEnv     :: NameEnv Fixity,
	deprecEnv     :: NameEnv DeprecTxt,
        typeEnv       :: TypeEnv,

        instEnv       :: InstEnv,
        ruleEnv       :: RuleEnv		-- Domain may include Id from other modules
     }

emptyModDetails :: Module -> ModDetails
emptyModDetails mod
  = ModDetails { moduleId      = mod,
		 moduleExports = [],
		 moduleEnv     = emptyRdrEnv,
		 fixityEnv     = emptyNameEnv,
		 deprecEnv     = emptyNameEnv,
		 typeEnv       = emptyNameEnv,
		 instEnv       = emptyInstEnv,
    		 ruleEnv       = emptyRuleEnv
    }		
emptyRuleEnv = panic "emptyRuleEnv"
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
lookupFixityEnv :: SymbolTable -> Name -> Fixity
	-- Returns defaultFixity if there isn't an explicit fixity
lookupFixityEnv tbl name
  = case lookupModuleEnv tbl (nameModule name) of
	Nothing	     -> defaultFixity
	Just details -> case lookupNameEnv (fixityEnv details) name of
				Just fixity -> fixity
				Nothing	    -> defaultFixity
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
	Just details -> lookupNameEnv (typeEnv details) name
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
                  Nothing      -> (emptyModDetails mod) {typeEnv = type_env}
                  Just details -> details {typeEnv = typeEnv details 
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
type DeprecationEnv = NameEnv DeprecTxt		-- Give reason for deprecation

type GlobalRdrEnv = RdrNameEnv [Name]	-- The list is because there may be name clashes
					-- These only get reported on lookup,
					-- not on construction

type InstEnv    = UniqFM ClsInstEnv		-- Maps Class to instances for that class
type ClsInstEnv = [(TyVarSet, [Type], Id)]	-- The instances for a particular class

type RuleEnv    = IdEnv [CoreRule]
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
        pcsPST :: PackageSymbolTable,		-- Domain = non-home-package modules
						--   except that the InstEnv components is empty
	pcsInsts :: InstEnv,			-- The total InstEnv accumulated from all
						--   the non-home-package modules
	pcsRules :: RuleEnv,			-- Ditto RuleEnv

        pcsPRS :: PersistentRenamerState
     }
\end{code}

The @PersistentRenamerState@ persists across successive calls to the
compiler.

It contains:
  * A name supply, which deals with allocating unique names to
    (Module,OccName) original names, 
 
  * An accumulated InstEnv from all the modules in pcsPST
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

data OrigNameEnv
 = Orig { origNames  :: FiniteMap (Module,OccName) Name, -- Ensures that one original name gets one unique
	  origIParam :: FiniteMap OccName Name		 -- Ensures that one implicit parameter name gets one unique
   }

type DeclsMap = NameEnv (Version, AvailInfo, Bool, (Module, RdrNameHsDecl))
		-- A DeclsMap contains a binding for each Name in the declaration
		-- including the constructors of a type decl etc.
		-- The Bool is True just for the 'main' Name.

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
   = HscOK   ModDetails  		-- new details (HomeSymbolTable additions)
	     (Maybe ModIFace)		-- new iface (if any compilation was done)
	     (Maybe String)  		-- generated stub_h
	     (Maybe String)  		-- generated stub_c
             PersistentCompilerState 	-- updated PCS
             [SDoc]                  	-- warnings

   | HscErrs PersistentCompilerState 	-- updated PCS
             [SDoc]                  	-- errors
             [SDoc]                  	-- warnings

	
-- These two are only here to avoid recursion between CmCompile and
-- CompManager.  They really ought to be in the latter.
type ModuleEnv a = UniqFM a   -- Domain is Module

type HomeModMap         = FiniteMap ModuleName Module -- domain: home mods only
type HomeInterfaceTable = ModuleEnv ModIFace
\end{code}


