%
% (c) The University of Glasgow, 2000
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}
module HscTypes  ( )
where

#include "HsVersions.h"

\end{code}

%************************************************************************
%*									*
\subsection{Symbol tables and Module details}
%*									*
%************************************************************************

A @ModDetails@ summarises everything we know about a compiled module

\begin{code}
data ModDetails
   = ModDetails {
        moduleExports :: Avails,		-- What it exports
        moduleEnv     :: GlobalRdrEnv,		-- Its top level environment

        fixityEnv     :: NameEnv Fixity,
	deprecEnv     :: NameEnv DeprecTxt,
        typeEnv       :: NameEnv TyThing,	-- TyThing is in TcEnv.lhs

        instEnv       :: InstEnv,
        ruleEnv       :: IdEnv [CoreRule]	-- Domain includes Ids from other modules
     }
\end{code}

Symbol tables map modules to ModDetails:

\begin{code}
type SymbolTable	= ModuleEnv ModDetails
type HomeSymbolTable    = SymbolTable	-- Domain = modules in the home package
type PackageSymbolTable = SymbolTable	-- Domain = modules in the some other package
type GlobalSymbolTable  = SymbolTable	-- Domain = all modules
\end{code}


Simple lookups in the symbol table

\begin{code}
lookupFixityEnv :: SymbolTable -> Name -> Fixity
	-- Returns defaultFixity if there isn't an explicit fixity
lookupFixityEnv tbl name
  = case lookupModuleEnv tbl (nameModule name) of
	Nothing	     -> defaultFixity
	Just details -> case lookupNameEnv (fixityEnv details) name of
				Just fixity -> fixity
				Nothing	    -> defaultFixity

lookupTypeEnv :: SymbolTable -> Name -> Maybe TyThing
lookupTypeEnv tbl name
  = case lookupModuleEnv tbl (nameModule name) of
	Just details -> lookupNameEnv (typeEnv details) name
	Nothing	     -> Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Auxiliary types}
%*									*
%************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere

\begin{code}
data TyThing = AnId   Id
	     | ATyCon TyCon
	     | AClass Class

type DeprecationEnv = NameEnv DeprecTxt		-- Give reason for deprecation

type GlobalRdrEnv = RdrNameEnv [Name]	-- The list is because there may be name clashes
					-- These only get reported on lookup,
					-- not on construction

type InstEnv    = UniqFM ClsInstEnv		-- Maps Class to instances for that class
type ClsInstEnv = [(TyVarSet, [Type], Id)]	-- The instances for a particular class
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
        pcsPRS :: PersistentRenamerState
     }
\end{code}

The @PersistentRenamerState@ persists across successive calls to the
compiler.

It contains:
  * a name supply, which deals with allocating unique names to
    (Module,OccName) original names, 
 
  * a "holding pen" for declarations that have been read out of
    interface files but not yet sucked in, renamed, and typechecked

\begin{code}
data PersistentRenamerState
  = PRS { prsNS	   :: NameSupply,
	  prsDecls :: DeclsMap,
	  prsInsts :: IfaceInsts,
	  prsRules :: IfaceRules,
    }

data NameSupply
 = NS { nsUniqs  :: UniqSupply,
	nsNames  :: FiniteMap (Module,OccName) Name	-- Ensures that one original name gets one unique
	nsIParam :: FiniteMap OccName Name		-- Ensures that one implicit parameter name gets one unique
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
	     Maybe ModIFace		-- new iface (if any compilation was done)
	     Maybe String  		-- generated stub_h
	     Maybe String  		-- generated stub_c
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


