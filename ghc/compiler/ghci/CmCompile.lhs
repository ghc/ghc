%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmCompile]{Compiler for GHCI}

\begin{code}
module CmCompile ( cmCompile,
                   ModDetails,       -- abstract
                   ModIFace,         -- abstract
                   PCS, emptyPCS,    -- abstract
                   HST,              -- not abstract (CM needs to see it)
                   HIT,              -- ditto
                   CompResult(..)
                 )
where

#include "HsVersions.h"

import CmLink		( Linkable )
import Outputable	( SDoc )
import CmFind		( Finder )
import CmSummarise	( ModSummary )
import CmStaticInfo	( SI )
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
cmCompile :: SI               -- obvious
          -> Finder           -- to find modules
          -> ModSummary       -- summary, including source
          -> Maybe ModIFace   -- old interface, if available
          -> HST              -- for home module ModDetails
          -> PCS              -- IN: persistent compiler state
          -> IO CompResult

cmCompile flags finder summary old_iface hst pcs
   = return (error "cmCompile:unimp")


data CompResult
   = CompOK   ModDetails  -- new details (HST additions)
              (Maybe (ModIFace, Linkable))
                       -- summary and code; Nothing => compilation not reqd
                       -- (old summary and code are still valid)
              PCS      -- updated PCS
              [SDoc]   -- warnings

   | CompErrs PCS      -- updated PCS
              [SDoc]   -- warnings and errors

emptyPCS :: IO PCS
emptyPCS = return (MkPCS emptyPIT emptyPST emptyHoldingPen)


-- These two are only here to avoid recursion between CmCompile and
-- CompManager.  They really ought to be in the latter.
type HST = FiniteMap {-really:Module-} String{- == ModName-} ModDetails
type HIT = FiniteMap {-really:Module-} String{- == ModName-} ModIFace


data PCS = MkPCS PIT         -- Package interface table
                 PST         -- Package symbol table
                 HoldingPen  -- pre slurped interface bits and pieces

type PIT = FiniteMap Module ModIFace
type PST = FiniteMap Module ModDetails

emptyPIT :: PIT
emptyPIT = emptyFM

emptyPST :: PST
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
