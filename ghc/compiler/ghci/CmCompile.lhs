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

-- should be somewhere else?
emptyPCS :: IO PersistentCompilerState
emptyPCS = return (PersistentCompilerState 
                      { pcs_modmap = emptyFM,
                        pcs_pit    = emptyPIT,
                        pcs_pst    = emptyPST,
                        pcs_hp     = emptyHoldingPen })
\end{code}

