%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CompManager]{The Compilation Manager}

\begin{code}
module CompManager ( cmInit, cmLoadModule, 
                     cmGetExpr, cmRunExpr,
                     CmState  -- abstract
                   )
where

#include "HsVersions.h"

import Outputable	( SDoc )

import CmStaticInfo 	( FLAGS, PCI, SI, mkSI )
import CmFind 		( Finder, ModName )
import CmSummarise 	( )
import CmCompile 	( PCS, HST, HIT )
import CmLink 		( PLS, HValue, Linkable )



cmInit :: FLAGS 
       -> PCI
       -> IO CmState
cmInit flags pkginfo
   = return (error "cmInit:unimp")

cmLoadModule :: CmState 
             -> ModName
             -> IO (CmState, Either [SDoc] ModHandle)
cmLoadModule cmstate modname
   = return (error "cmLoadModule:unimp")

cmGetExpr :: CmState
          -> ModHandle
          -> String
          -> IO (CmState, Either [SDoc] HValue)
cmGetExpr cmstate modhdl expr
   = return (error "cmGetExpr:unimp")

cmRunExpr :: HValue -> IO ()
cmRunExpr hval
   = return (error "cmRunExpr:unimp")

type ModHandle = String   -- ToDo: do better?


-- Persistent state just for CM, excluding link & compile subsystems
data PCMS
   = PCMS HST   -- home symbol table
          HIT   -- home interface table
          UI    -- the unlinked images
          MG    -- the module graph

-- Persistent state for the entire system
data CmState
   = CmState PCMS      -- CM's persistent state
             PCS       -- compile's persistent state
             PLS       -- link's persistent state
             SI        -- static info, never changes
             Finder    -- the module finder

-- CM internal types
type UI = [Linkable]	-- the unlinked images (should be a set, really)
data MG = MG            -- the module graph




\end{code}
