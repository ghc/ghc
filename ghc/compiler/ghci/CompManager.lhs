%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CompManager]{The Compilation Manager}

\begin{code}
module CompManager ( cmInit, cmLoadModule, 
                     cmGetExpr, cmRunExpr,
                     CmState, emptyCmState  -- abstract
                   )
where

#include "HsVersions.h"

import List		( nub )
import Maybe		( catMaybes )
import Outputable	( SDoc )
import FiniteMap	( emptyFM )

import CmStaticInfo 	( FLAGS, PCI, SI, mkSI )
import CmFind 		( Finder, newFinder, 
			  ModName, ml_modname, isPackageLoc )
import CmSummarise 	( summarise, ModSummary(..), mi_name )
import CmCompile 	( PCS, emptyPCS, HST, HIT )
import CmLink 		( PLS, emptyPLS, HValue, Linkable )



cmInit :: FLAGS 
       -> PCI
       -> IO CmState
cmInit flags pkginfo
   = emptyCmState flags pkginfo

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
   = PCMS { 
        hst :: HST,   -- home symbol table
        hit :: HIT,   -- home interface table
        ui  :: UI,    -- the unlinked images
        mg  :: MG    -- the module graph
     }

emptyPCMS :: PCMS
emptyPCMS = PCMS { hst = emptyHST,
                   hit = emptyHIT,
                   ui  = emptyUI,
                   mg  = emptyMG }

emptyHIT :: HIT
emptyHIT = emptyFM

emptyHST :: HST
emptyHST = emptyFM



-- Persistent state for the entire system
data CmState
   = CmState {
        pcms   :: PCMS,      -- CM's persistent state
        pcs    :: PCS,       -- compile's persistent state
        pls    :: PLS,       -- link's persistent state
        si     :: SI,        -- static info, never changes
        finder :: Finder    -- the module finder
     }

emptyCmState :: FLAGS -> PCI -> IO CmState
emptyCmState flags pci
    = do let pcms = emptyPCMS
         pcs     <- emptyPCS
         pls     <- emptyPLS
         let si   = mkSI flags pci
         finder  <- newFinder pci
         return (CmState { pcms   = pcms,
                           pcs    = pcs,
                           pls    =   pls,
                           si     = si,
                           finder = finder })

-- CM internal types
type UI = [Linkable]	-- the unlinked images (should be a set, really)
emptyUI :: UI
emptyUI = []


type MG = [[ModSummary]]            -- the module graph
emptyMG :: MG
emptyMG = []

\end{code}

The real business of the compilation manager: given a system state and
a module name, try and bring the module up to date, probably changing
the system state at the same time.

\begin{code}
cmLoadModule :: CmState 
             -> ModName
             -> IO (CmState, Either [SDoc] ModHandle)

cmLoadModule cmstate modname
   = do putStr "cmLoadModule: downsweep begins\n"
        let find  = finder cmstate
        mgNew <- downsweep modname find
        putStrLn ( "after chasing:\n\n" ++ unlines (map show mgNew))
        return (error "cmLoadModule:unimp")

downsweep :: ModName          -- module to chase from
          -> Finder
          -> IO [ModSummary]
downsweep rootNm finder
   = do rootLoc <- getSummary rootNm
        loop [rootLoc]
     where
        getSummary :: ModName -> IO ModSummary
        getSummary nm
           = do loc     <- finder nm
                summary <- summarise loc
                return summary

        -- loop invariant: homeSummaries doesn't contain package modules
        loop :: [ModSummary] -> IO [ModSummary]
        loop homeSummaries
           = do let allImps   -- all imports
                       = (nub . map mi_name . concat . catMaybes . map ms_imports)
                         homeSummaries
                let allHome   -- all modules currently in homeSummaries
                       = map (ml_modname.ms_loc) homeSummaries
                let neededImps
                       = filter (`notElem` allHome) allImps
                neededSummaries
                       <- mapM getSummary neededImps
                let newHomeSummaries
                       = filter (not.isPackageLoc.ms_loc) neededSummaries
                if null newHomeSummaries
                 then return homeSummaries
                 else loop (newHomeSummaries ++ homeSummaries)
                 
\end{code}
