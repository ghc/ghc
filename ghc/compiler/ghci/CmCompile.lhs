%
% (c) The University of Glasgow, 2000
%
\section[CmCompile]{Compiler for GHCI}

\begin{code}
module CmCompile ( cmCompile,
                   ModDetails,         -- abstract
                   ModIFace,           -- abstract
                   PersistentCompilerState, emptyPCS,    -- abstract
                   HomeSymbolTable,    -- not abstract (CM needs to see it)
                   HomeInterfaceTable, -- ditto
                   CompResult(..)
                 )
where

#include "HsVersions.h"

-- many of these need to be moved to HscTypes
--import CmLink		( Linkable(..) )
--import Outputable	( SDoc )
--import CmFind		( Finder )
--import CmSummarise	( ModSummary, name_of_summary )
--import FiniteMap	( FiniteMap, emptyFM )

--import Module		( Module )
--import RnMonad		( Avails, GlobalRdrEnv, DeclsMap, 
--                          WhetherHasOrphans, ImportVersion,
--                          IfaceInsts, IfaceRules, ExportItem )
--import TcEnv		( TyThing, InstEnv )
--import Name		( Name, OccName )
--import BasicTypes	( Fixity, Version )
--import Id		( Id )
--import CoreSyn		( CoreRule )
--import RdrHsSyn		( RdrNameDeprecation, RdrNameRuleDecl, RdrNameFixitySig,
--                          RdrNameHsDecl, RdrNameInstDecl )

import HscTypes		( )

\end{code}


%************************************************************************
%*									*
\subsection{The main compiler interface}
%*									*
%************************************************************************


\begin{code}
cmCompile :: Finder                  -- to find modules
          -> ModSummary              -- summary, including source
          -> Maybe ModIFace          -- old interface, if available
          -> HomeModMap              -- ModuleName -> Module
          -> HomeSymbolTable         -- for home module ModDetails          
          -> PersistentCompilerState -- IN: persistent compiler state
          -> IO CompResult

cmCompile finder summary old_iface hst pcs
   = do putStrLn ("cmCompile: compiling " ++ name_of_summary summary)
        return (CompOK (error "cmCompile:modDetails")
                       (Just (error "cmCompile:modIFace", 
                              --error "cmCompile:Linkable"
                              --LM (name_of_summary summary) [] 
                              LM (name_of_summary summary) []
                              ))
                       pcs
                       []
               )

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


%************************************************************************
%*									*
\subsection{Module details}
%*									*
%************************************************************************

A @ModDetails@ summarises everything we know about a compiled module

Auxiliary definitions

\begin{code}
{- I DONT think this should be here -- should be in HscTypes 
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
-}
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
        pcsHP     :: RnMonad.HoldingPen, 	-- Pre-slurped interface bits and pieces
	pcsNS	  :: NameSupply			-- Allocate uniques for names
     }

type PackageSymbolTable = ModuleEnv ModDetails

data NameSupply
 = NS { nsUniqs  :: UniqSupply,
	nsNames  :: FiniteMap (Module,OccName) Name	-- Ensures that one original name gets one unique
	nsIParam :: FiniteMap OccName Name		-- Ensures that one implicit parameter name gets one unique
   }
=======
>>>>>>> 1.9
=======

-- should be somewhere else?
emptyPCS :: IO PersistentCompilerState
emptyPCS = return (PersistentCompilerState 
                      { pcs_modmap = emptyFM,
                        pcs_pit    = emptyPIT,
                        pcs_pst    = emptyPST,
                        pcs_hp     = emptyHoldingPen })
>>>>>>> 1.10
\end{code}

