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
\subsection{Module details}
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

Auxiliary definitions

\begin{code}
type DeprecationEnv = NameEnv DeprecTxt		-- Give reason for deprecation

type GlobalRdrEnv = RdrNameEnv [Name]	-- The list is because there may be name clashes
					-- These only get reported on lookup,
					-- not on construction

data GenAvailInfo name	= Avail name	 -- An ordinary identifier
			| AvailTC name 	 -- The name of the type or class
				  [name] -- The available pieces of type/class.
					 -- NB: If the type or class is itself
					 -- to be in scope, it must be in this list.
					 -- Thus, typically: AvailTC Eq [Eq, ==, /=]
			deriving( Eq )
			-- Equality used when deciding if the interface has changed

type AvailEnv	  = NameEnv AvailInfo	-- Maps a Name to the AvailInfo that contains it
type AvailInfo    = GenAvailInfo Name
type RdrAvailInfo = GenAvailInfo OccName
type Avails	  = [AvailInfo]
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
        pcsPST    :: PackageSymbolTable,	-- Domain = non-home-package modules
        pcsHP     :: HoldingPen, 		-- Pre-slurped interface bits and pieces
	pcsNS	  :: NameSupply			-- Allocate uniques for names
     }

type PackageSymbolTable = ModuleEnv ModDetails

data NameSupply
 = NS { nsUniqs  :: UniqSupply,
	nsNames  :: FiniteMap (Module,OccName) Name	-- Ensures that one original name gets one unique
	nsIParam :: FiniteMap OccName Name		-- Ensures that one implicit parameter name gets one unique
   }
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
              PersistentCompilerState -- updated PCS
              [SDoc]                  -- warnings

   | CompErrs PersistentCompilerState -- updated PCS
              [SDoc]                  -- errors
              [SDoc]                  -- warnings


-- These two are only here to avoid recursion between CmCompile and
-- CompManager.  They really ought to be in the latter.
type ModuleEnv a = UniqFM a   -- Domain is Module

type HomeModMap         = FiniteMap ModuleName Module -- domain: home mods only
type HomeSymbolTable    = ModuleEnv ModDetails        -- ditto
type HomeInterfaceTable = ModuleEnv ModIFace

\end{code}


