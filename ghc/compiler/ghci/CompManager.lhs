%
% (c) The University of Glasgow, 2000
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
import Maybe		( catMaybes, maybeToList )
import Outputable	( SDoc )
import FiniteMap	( emptyFM, filterFM )
import Digraph		( SCC(..), stronglyConnComp )
import Panic		( panic )

import CmStaticInfo 	( FLAGS, PCI, SI(..), mkSI )
import CmFind 		( Finder, newFinder, 
			  ModName, ml_modname, isPackageLoc )
import CmSummarise 	( summarise, ModSummary(..), 
			  mi_name, ms_get_imports )
import CmCompile 	( PCS, emptyPCS, HST, HIT, CompResult(..) )
import CmLink 		( PLS, emptyPLS, Linkable, 
			  link, LinkResult(..), 
			  filterModuleLinkables, modname_of_linkable,
			  is_package_linkable )
import InterpSyn	( HValue )

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
        mg  :: MG     -- the module graph
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
        finder :: Finder     -- the module finder
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


type MG = [SCC ModSummary]  -- the module graph, topologically sorted
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

cmLoadModule cmstate1 modname
   = do -- version 1's are the original, before downsweep

        let pci1  = pci  (si cmstate1)
        let pcms1 = pcms cmstate1
        let pls1  = pls  cmstate1
        let pcs1  = pcs  cmstate1
        let mg1   = mg  pcms1
        let hst1  = hst pcms1
        let hit1  = hit pcms1
        let ui1   = ui  pcms1

        -- do the downsweep to reestablish the module graph
        -- then generate version 2's by removing from HIT,HST,UI any
        -- modules in the old MG which are not in the new one.

        putStr "cmLoadModule: downsweep begins\n"
        mg2unsorted <- downsweep modname (finder cmstate1)
        putStrLn ( "after chasing:\n\n" ++ unlines (map show mg2unsorted))

        let modnames1   = map name_of_summary (flattenMG mg1)
        let modnames2   = map name_of_summary mg2unsorted
        let mods_to_zap = filter (`notElem` modnames2) modnames1

        let (hst2, hit2, ui2)
               = filterTopLevelEnvs (`notElem` mods_to_zap) 
                                    (hst1, hit2, ui2)

        let mg2 = topological_sort mg2unsorted

        putStrLn ( "after tsort:\n\n" 
                   ++ unlines (map show (flattenMG mg2)))

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

        let threaded2 = ModThreaded pcs1 hst2 hit2

        (threaded3, sccOKs, newLis, errs, warns)
           <- upsweep_sccs threaded2 [] [] [] [] mg2

        let ui3 = add_to_ui ui2 newLis
        let (ModThreaded pcs3 hst3 hit3) = threaded3

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if null errs

         then 
           do let mods_to_relink = upwards_closure mg2 
                                      (map modname_of_linkable newLis)
              let sccs_to_relink = group_uis ui3 mg2 mods_to_relink
              linkresult <- link pci1 sccs_to_relink pls1
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (1)"
                 LinkOK pls3 
                    -> do let pcms3 
                                 = PCMS { hst=hst3, hit=hit3, ui=ui3, mg=mg2 }
                          let cmstate3 
                                 = CmState { pcms=pcms3, pcs=pcs3, pls=pls3,
                                             si     = si cmstate1,
                                             finder = finder cmstate1
                                   }
                          return (cmstate3, Right modname)

         else 
           do let mods_to_relink = downwards_closure mg2 
                                      (map name_of_summary (flattenMG sccOKs))
              let sccs_to_relink = group_uis ui3 mg2 mods_to_relink
              linkresult <- link pci1 sccs_to_relink pls1
              let (hst4, hit4, ui4) 
                     = filterTopLevelEnvs (`notElem` mods_to_relink)
                                          (hst3,hit3,ui3)
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (2)"
                 LinkOK pls4
                    -> do let pcms4 
                                 = PCMS { hst=hst4, hit=hit4, ui=ui4, mg=mg2 }
                          let cmstate4 
                                 = CmState { pcms=pcms4, pcs=pcs3, pls=pls4,
                                             si     = si cmstate1,
                                             finder = finder cmstate1
                                   }
                          return (cmstate4, Right modname)


flattenMG :: [SCC ModSummary] -> [ModSummary]
flattenMG = concatMap flatten

flatten (AcyclicSCC v) = [v]
flatten (CyclicSCC vs) = vs

-- For each module in mods_to_group, extract the relevant linkable
-- out of UI, and arrange these linkables in SCCs as defined by modGraph.
-- All this is so that we can pass SCCified Linkable groups to the
-- linker.  A constraint that should be recorded somewhere is that
-- all sccs should either be all-interpreted or all-object, not a mixture.
group_uis :: UI -> [SCC ModSummary] -> [ModName] -> [SCC Linkable]
group_uis ui modGraph mods_to_group
   = map extract (cleanup (fishOut modGraph mods_to_group))
     where
        fishOut :: [SCC ModSummary] -> [ModName] -> [(Bool,[ModName])]
        fishOut [] unused
           | null unused = []
           | otherwise   = panic "group_uis: modnames not in modgraph"
        fishOut ((AcyclicSCC ms):sccs) unused
           = case split (== (name_of_summary ms)) unused of
                (eq, not_eq) -> (False, eq) : fishOut sccs not_eq
        fishOut ((CyclicSCC mss):sccs) unused
           = case split (`elem` (map name_of_summary mss)) unused of
                (eq, not_eq) -> (True, eq) : fishOut sccs not_eq

        cleanup :: [(Bool,[ModName])] -> [SCC ModName]
        cleanup [] = []
        cleanup ((isRec,names):rest)
           | null names = cleanup rest
           | isRec      = CyclicSCC names : cleanup rest
           | not isRec  = case names of [name] -> AcyclicSCC name : cleanup rest
                                        other  -> panic "group_uis(cleanup)"

        extract :: SCC ModName -> SCC Linkable
        extract (AcyclicSCC nm) = AcyclicSCC (getLi nm)
        extract (CyclicSCC nms) = CyclicSCC (map getLi nms)

        getLi nm = case [li | li <- ui, not (is_package_linkable li),
                                        nm == modname_of_linkable li] of
                      [li]  -> li
                      other -> panic "group_uis:getLi"

        split f xs = (filter f xs, filter (not.f) xs)


-- Add the given (LM-form) Linkables to the UI, overwriting previous
-- versions if they exist.
add_to_ui :: UI -> [Linkable] -> UI
add_to_ui ui lis 
   = foldr add1 ui lis
     where
        add1 :: Linkable -> UI -> UI
        add1 li ui
           = li : filter (\li2 -> not (for_same_module li li2)) ui

        for_same_module :: Linkable -> Linkable -> Bool
        for_same_module li1 li2 
           = not (is_package_linkable li1)
             && not (is_package_linkable li2)
             && modname_of_linkable li1 == modname_of_linkable li2
                                  

-- Compute upwards and downwards closures in the (home-) module graph.
downwards_closure,
 upwards_closure :: [SCC ModSummary] -> [ModName] -> [ModName]

upwards_closure   = up_down_closure True
downwards_closure = up_down_closure False

up_down_closure :: Bool -> [SCC ModSummary] -> [ModName] -> [ModName]
up_down_closure up modGraph roots
   = let mgFlat = flattenMG modGraph
         nodes  = map name_of_summary mgFlat

         fwdEdges, backEdges  :: [(ModName, [ModName])] 
                   -- have an entry for each mod in mgFlat, and do not
                   -- mention edges leading out of the home package
         fwdEdges 
            = map mkEdge mgFlat
         backEdges -- Only calculated if needed, which is just as well!
            = [(n, [m | (m, m_imports) <- fwdEdges, n `elem` m_imports])
               | (n, n_imports) <- fwdEdges]

         iterate :: [(ModName,[ModName])] -> [ModName] -> [ModName]
         iterate graph set
            = let set2 = nub (concatMap dsts set)
                  dsts :: ModName -> [ModName]
                  dsts node = case lookup node graph of
                                 Just ds -> ds
                                 Nothing -> panic "up_down_closure"
              in
                  if length set == length set2 then set else iterate graph set2

         mkEdge summ
            = (name_of_summary summ, 
               -- ignore imports not from the home package
               filter (`elem` nodes) (deps_of_summary summ))
     in
         (if up then iterate backEdges else iterate fwdEdges) (nub roots)


data ModThreaded  -- stuff threaded through individual module compilations
   = ModThreaded PCS HST HIT

-- Compile multiple SCCs, stopping as soon as an error appears
upsweep_sccs :: ModThreaded           -- PCS & HST & HIT
             -> [SCC ModSummary]      -- accum: SCCs which succeeded
             -> [Linkable]            -- accum: new Linkables
             -> [SDoc]                -- accum: error messages
             -> [SDoc]                -- accum: warnings
             -> [SCC ModSummary]      -- SCCs to do (the worklist)
                                      -- ...... RETURNING ......
             -> IO (ModThreaded,
                    [SCC ModSummary], -- SCCs which succeeded
                    [Linkable],       -- new linkables
                    [SDoc],           -- error messages
                    [SDoc])           -- warnings

upsweep_sccs threaded sccOKs newLis errs warns []
   = -- No more SCCs to do.
     return (threaded, sccOKs, newLis, errs, warns)

upsweep_sccs threaded sccOKs newLis errs warns (scc:sccs)
   = -- Start work on a new SCC.
     do (threaded2, lisM, errsM, warnsM) 
           <- upsweep_mods threaded (flatten scc)
        if    null errsM
         then -- all the modules in the scc were ok
              -- move on to the next SCC
              upsweep_sccs threaded2 (scc:sccOKs) (lisM++newLis) 
                           errs (warnsM++warns) sccs
         else -- we got a compilation error; give up now
              return 
                 (threaded2, sccOKs, 
                 lisM++newLis, errsM++errs, warnsM++warns)

-- Compile multiple modules (one SCC), stopping as soon as an error appears
upsweep_mods :: ModThreaded
             -> [ModSummary]
             -> IO (ModThreaded, [Linkable], [SDoc], [SDoc])
upsweep_mods threaded []
   = return (threaded, [], [], [])
upsweep_mods threaded (mod:mods)
   = do (threaded1, maybe_linkable, errsM, warnsM) <- upsweep_mod threaded mod
        if null errsM
         then -- No errors; get contribs from the rest
              do (threaded2, linkables, errsMM, warnsMM)
                    <- upsweep_mods threaded1 mods
                 return
                    (threaded2, maybeToList maybe_linkable ++ linkables,
                     errsM++errsMM, warnsM++warnsMM)
         else -- Errors; give up _now_
              return (threaded1, [], errsM, warnsM)

-- Compile a single module.
upsweep_mod :: ModThreaded
            -> ModSummary
            -> IO (ModThreaded, Maybe Linkable, [SDoc], [SDoc])
upsweep_mod = error "upsweep_mod"



         
filterTopLevelEnvs :: (ModName -> Bool) -> (HST, HIT, UI) -> (HST, HIT, UI)
filterTopLevelEnvs p (hst, hit, ui)
   = (filterFM (\k v -> p k) hst,
      filterFM (\k v -> p k) hit,
      filterModuleLinkables p ui
     )

name_of_summary :: ModSummary -> ModName
name_of_summary = ml_modname . ms_loc

deps_of_summary :: ModSummary -> [ModName]
deps_of_summary = map mi_name . ms_get_imports

topological_sort :: [ModSummary] -> [SCC ModSummary]
topological_sort summaries
   = let 
         toEdge :: ModSummary -> (ModSummary,ModName,[ModName])
         toEdge summ
             = (summ, name_of_summary summ, deps_of_summary summ)
         
         mash_edge :: (ModSummary,ModName,[ModName]) -> (ModSummary,Int,[Int])
         mash_edge (summ, m, m_imports)
            = case lookup m key_map of
                 Nothing -> panic "reverse_topological_sort"
                 Just mk -> (summ, mk, 
                                -- ignore imports not from the home package
                                catMaybes (map (flip lookup key_map) m_imports))

         edges     = map toEdge summaries
         key_map   = zip [nm | (s,nm,imps) <- edges] [1 ..] :: [(ModName,Int)]
         scc_input = map mash_edge edges
         sccs      = stronglyConnComp scc_input
     in
         sccs

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
                       = (nub . map mi_name . concat . map ms_get_imports)
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
