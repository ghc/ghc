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

import CmLink		( Linkable(..) )
import Outputable	( SDoc )
import CmFind		( Finder )
import CmSummarise	( ModSummary, name_of_summary )
import FiniteMap	( FiniteMap, emptyFM )

import Module		( Module )
import RnMonad		( Avails, GlobalRdrEnv, DeclsMap, 
                          WhetherHasOrphans, ImportVersion,
                          IfaceInsts, IfaceRules, ExportItem )
import TcEnv		( TyThing, InstEnv )
import Name		( Name, OccName )
import BasicTypes	( Fixity, Version )
import Id		( Id )
import CoreSyn		( CoreRule )
import RdrHsSyn		( RdrNameDeprecation, RdrNameRuleDecl, RdrNameFixitySig,
                          RdrNameHsDecl, RdrNameInstDecl )


\end{code}
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

emptyPCS :: IO PersistentCompilerState
emptyPCS = return (PersistentCompilerState 
                      { pcs_modmap = emptyFM,
                        pcs_pit    = emptyPIT,
                        pcs_pst    = emptyPST,
                        pcs_hp     = emptyHoldingPen })

-- These two are only here to avoid recursion between CmCompile and
-- CompManager.  They really ought to be in the latter.
type ModuleEnv a = UniqFM a   -- Domain is Module

type HomeModMap         = FiniteMap ModuleName Module -- domain: home mods only
type HomeSymbolTable    = ModuleEnv ModDetails        -- ditto
type HomeInterfaceTable = ModuleEnv ModIFace

data PersistentCompilerState 
   = PersistentCompilerState {
        pcs_modmap :: PackageModMap,         -- domain: package mods only
        pcs_pit    :: PackageInterfaceTable, -- Package interface table
        pcs_pst    :: PackageSymbolTable,    -- Package symbol table
        pcs_hp     :: HoldingPen             -- pre slurped interface bits and pieces
     }

type PackageModMap         = FiniteMap ModuleName Module
type PackageInterfaceTable = ModuleEnv ModIFace
type PackageSymbolTable    = ModuleEnv ModDetails

emptyPIT :: PackageInterfaceTable
emptyPIT = emptyFM

emptyPST :: PackageSymbolTable
emptyPST = emptyFM

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

data ModDetails
   = ModDetails {
        moduleExports :: Avails,
        moduleEnv     :: GlobalRdrEnv,           -- == FM RdrName [Name]
        typeEnv       :: FiniteMap Name TyThing, -- TyThing is in TcEnv.lhs
        instEnv       :: InstEnv,
        fixityEnv     :: FiniteMap Name Fixity,
        ruleEnv       :: FiniteMap Id [CoreRule]
     }

-- This should eventually replace RnMonad.Ifaces
data HoldingPen
   = HoldingPen {
        iDecls :: DeclsMap,     -- A single, global map of Names to decls

        iInsts :: IfaceInsts,
        -- The as-yet un-slurped instance decls; this bag is depleted when we
        -- slurp an instance decl so that we don't slurp the same one twice.
        -- Each is 'gated' by the names that must be available before
        -- this instance decl is needed.

        iRules :: IfaceRules
        -- Similar to instance decls, only for rules
     }

emptyHoldingPen :: HoldingPen
emptyHoldingPen = error "emptyHoldingPen:unimp"
\end{code}
