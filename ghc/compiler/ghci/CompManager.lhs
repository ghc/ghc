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
import Maybe		( catMaybes, maybeToList, fromMaybe )
import Outputable
import FiniteMap	( emptyFM, filterFM, lookupFM, addToFM )
import Digraph		( SCC(..), stronglyConnComp )
import Panic		( panic )

import CmStaticInfo 	( PCI(..), mkPCI, Package(..) )
import Finder 		( Finder, newFinder, 
			  ModName, ml_modname, isPackageLoc,
			  PkgName, Path )
import CmSummarise 	( summarise, ModSummary(..), 
			  mi_name, ms_get_imports,
			  name_of_summary, deps_of_summary )
--import CmCompile 	( PCS, emptyPCS, HST, HIT, CompResult(..), cmCompile )
import CmLink 		( PLS, emptyPLS, Linkable(..), 
			  link, LinkResult(..), 
			  filterModuleLinkables, modname_of_linkable,
			  is_package_linkable )
import InterpSyn	( HValue )


cmInit :: String{-temp debugging hack-}
       -> [Package]
       -> IO CmState
cmInit path raw_package_info
   = emptyCmState path raw_package_info

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
data PersistentCMState
   = PersistentCMState {
        hst :: HomeSymbolTable,    -- home symbol table
        hit :: HomeInterfaceTable, -- home interface table
        ui  :: UnlinkedImages,     -- the unlinked images
        mg  :: ModuleGraph         -- the module graph
     }

emptyPCMS :: PersistentCMState
emptyPCMS = PersistentCMState
               { hmm = emptyHMM,
                 hst = emptyHST, hit = emptyHIT,
                 ui  = emptyUI,  mg  = emptyMG }

emptyHIT :: HomeInterfaceTable
emptyHIT = emptyFM
emptyHST :: HomeSymbolTable
emptyHST = emptyFM



-- Persistent state for the entire system
data CmState
   = CmState {
        pcms   :: PersistentCMState,       -- CM's persistent state
        pcs    :: PersistentCompilerState, -- compile's persistent state
        pls    :: PersistentLinkerState,   -- link's persistent state
        pci    :: PackageConfigInfo,       -- package config info, never changes
        finder :: Finder                   -- the module finder
     }

emptyCmState :: String{-temp debugging hack-}
             -> [Package] -> IO CmState
emptyCmState path_TMP_DEBUGGING_HACK raw_package_info
    = do let pcms = emptyPCMS
         pcs     <- emptyPCS
         pls     <- emptyPLS
         pci     <- mkPCI raw_package_info
         finder  <- newFinder path_TMP_DEBUGGING_HACK pci
         return (CmState { pcms   = pcms,
                           pcs    = pcs,
                           pls    = pls,
                           pci    = pci,
                           finder = finder })

-- CM internal types
type UnlinkedImage = [Linkable]	-- the unlinked images (should be a set, really)
emptyUI :: UnlinkedImage
emptyUI = []

type ModuleGraph = [SCC ModSummary]  -- the module graph, topologically sorted
emptyMG :: ModuleGraph
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
        let pcms1   = pcms   cmstate1
        let pls1    = pls    cmstate1
        let pcs1    = pcs    cmstate1
        let mg1     = mg     pcms1
        let hst1    = hst    pcms1
        let hit1    = hit    pcms1
        let ui1     = ui     pcms1
        -- these aren't numbered since they don't change
        let pcii    = pci    cmstate1
        let finderr = finder cmstate1

        -- do the downsweep to reestablish the module graph
        -- then generate version 2's by removing from HIT,HST,UI any
        -- modules in the old MG which are not in the new one.

        putStr "cmLoadModule: downsweep begins\n"
        mg2unsorted <- downsweep modname finderr
        putStrLn (showSDoc (vcat (map ppr mg2unsorted)))

        let modnames1   = map name_of_summary (flattenSCCs mg1)
        let modnames2   = map name_of_summary mg2unsorted
        let mods_to_zap = filter (`notElem` modnames2) modnames1

        let (hst2, hit2, ui2)
               = filterTopLevelEnvs (`notElem` mods_to_zap) 
                                    (hst1, hit1, ui1)

        let mg2 = topological_sort mg2unsorted

        putStrLn "after tsort:\n"
        putStrLn (showSDoc (vcat (map ppr (flattenSCCs mg2))))

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

        let threaded2 = ModThreaded pcs1 hst2 hit2

        (threaded3, sccOKs, newLis, errs, warns)
           <- upsweep_sccs finderr threaded2 [] [] [] [] mg2

        let ui3 = add_to_ui ui2 newLis
        let (ModThreaded pcs3 hst3 hit3) = threaded3

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if null errs

         then 
           do let mods_to_relink = upwards_closure mg2 
                                      (map modname_of_linkable newLis)
              let pkg_linkables = find_pkg_linkables_for pcii mg2 mods_to_relink
              putStrLn ("needed package modules =\n" 
                        ++ showSDoc (vcat (map ppr pkg_linkables)))
              let sccs_to_relink = group_uis ui3 mg2 mods_to_relink
              let all_to_relink  = map AcyclicSCC pkg_linkables 
                                   ++ sccs_to_relink
              linkresult <- link pcii all_to_relink pls1
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (1)"
                 LinkOK pls3 
                    -> do let pcms3 
                                 = PCMS { hst=hst3, hit=hit3, ui=ui3, mg=mg2 }
                          let cmstate3 
                                 = CmState { pcms=pcms3, pcs=pcs3, pls=pls3,
                                             pci=pcii, finder=finderr }
                          return (cmstate3, Right modname)

         else 
           do let mods_to_relink = downwards_closure mg2 
                                      (map name_of_summary (flattenSCCs sccOKs))
              let pkg_linkables = find_pkg_linkables_for pcii mg2 mods_to_relink
              let sccs_to_relink = group_uis ui3 mg2 mods_to_relink
              let all_to_relink  = map AcyclicSCC pkg_linkables 
                                   ++ sccs_to_relink
              linkresult <- link pcii all_to_relink pls1
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
                                             pci=pcii, finder=finderr }
                          return (cmstate4, Right modname)

-- Given a (home) module graph and a bunch of names of (home) modules
-- within that graph, return the names of any packages needed by the
-- named modules.  Do this by looking at their imports.  Assumes, and
-- checks, that all of "mods" are mentioned in "mg".
-- 
-- Then, having found the packages directly needed by "mods",
-- (1) round up, by looking in "pci", all packages they directly or
-- indirectly depend on, and (2) put these packages in topological
-- order, since that's important for some linkers.  Since cycles in
-- the package dependency graph aren't allowed, we can just return
-- the list of (package) linkables, rather than a list of SCCs.
find_pkg_linkables_for :: PCI -> [SCC ModSummary] -> [ModName] -> [Linkable]
find_pkg_linkables_for pcii mg mods
   = let mg_summaries = flattenSCCs mg
         mg_names     = map name_of_summary mg_summaries
     in
     if   not (all (`elem` mg_names) mods)
     then panic "find_packages_for"
     else 
     let all_imports
            = concat 
                 [deps_of_summary summ
                 | summ <- mg_summaries, name_of_summary summ `elem` mods]
         imports_not_in_home  -- imports which must be from packages
            = nub (filter (`notElem` mg_names) all_imports)
         mod_tab :: [(ModName, PkgName, Path)]
         mod_tab = module_table pcii
         home_pkgs_needed -- the packages directly needed by the home modules
            = nub [pkg_nm | (mod_nm, pkg_nm, path) <- mod_tab, 
                            mod_nm `elem` imports_not_in_home]

         -- Discover the package dependency graph, and use it to find the
         -- transitive closure of all the needed packages
         pkg_depend_graph :: [(PkgName,[PkgName])]
         pkg_depend_graph = map (\raw -> (name raw, package_deps raw)) 
                                (raw_package_info pcii)

         all_pkgs_needed = simple_transitive_closure 
                              pkg_depend_graph home_pkgs_needed

         -- Make a graph, in the style which Digraph.stronglyConnComp expects,
         -- containing entries only for the needed packages.
         needed_graph
            = concat
                 [if srcP `elem` all_pkgs_needed
                  then [(srcP, srcP, dstsP)] 
                  else []
                 | (srcP, dstsP) <- pkg_depend_graph]
         tsorted = flattenSCCs (stronglyConnComp needed_graph)
     in
         map LP tsorted


simple_transitive_closure :: Eq a => [(a,[a])] -> [a] -> [a]
simple_transitive_closure graph set
   = let set2      = nub (concatMap dsts set ++ set)
         dsts node = fromMaybe [] (lookup node graph)
     in
         if   length set == length set2 
         then set 
         else simple_transitive_closure graph set2


flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flatten

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
   = let mgFlat = flattenSCCs modGraph
         nodes  = map name_of_summary mgFlat

         fwdEdges, backEdges  :: [(ModName, [ModName])] 
                   -- have an entry for each mod in mgFlat, and do not
                   -- mention edges leading out of the home package
         fwdEdges 
            = map mkEdge mgFlat
         backEdges -- Only calculated if needed, which is just as well!
            = [(n, [m | (m, m_imports) <- fwdEdges, n `elem` m_imports])
               | (n, n_imports) <- fwdEdges]

         mkEdge summ
            = (name_of_summary summ, 
               -- ignore imports not from the home package
               filter (`elem` nodes) (deps_of_summary summ))
     in
         simple_transitive_closure
            (if up then backEdges else fwdEdges) (nub roots)


data ModThreaded  -- stuff threaded through individual module compilations
   = ModThreaded PCS HST HIT

-- Compile multiple SCCs, stopping as soon as an error appears
upsweep_sccs :: Finder                -- the finder
             -> ModThreaded           -- PCS & HST & HIT
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

upsweep_sccs finder threaded sccOKs newLis errs warns []
   = -- No more SCCs to do.
     return (threaded, sccOKs, newLis, errs, warns)

upsweep_sccs finder threaded sccOKs newLis errs warns (scc:sccs)
   = -- Start work on a new SCC.
     do (threaded2, lisM, errsM, warnsM) 
           <- upsweep_mods finder threaded (flatten scc)
        if    null errsM
         then -- all the modules in the scc were ok
              -- move on to the next SCC
              upsweep_sccs finder threaded2 
                           (scc:sccOKs) (lisM++newLis) 
                           errs (warnsM++warns) sccs
         else -- we got a compilation error; give up now
              return 
                 (threaded2, sccOKs, 
                 lisM++newLis, errsM++errs, warnsM++warns)

-- Compile multiple modules (one SCC), stopping as soon as an error appears
upsweep_mods :: Finder
             -> ModThreaded
             -> [ModSummary]
             -> IO (ModThreaded, [Linkable], [SDoc], [SDoc])
upsweep_mods finder threaded []
   = return (threaded, [], [], [])
upsweep_mods finder threaded (mod:mods)
   = do (threaded1, maybe_linkable, errsM, warnsM) 
           <- upsweep_mod finder threaded mod
        if null errsM
         then -- No errors; get contribs from the rest
              do (threaded2, linkables, errsMM, warnsMM)
                    <- upsweep_mods finder threaded1 mods
                 return
                    (threaded2, maybeToList maybe_linkable ++ linkables,
                     errsM++errsMM, warnsM++warnsMM)
         else -- Errors; give up _now_
              return (threaded1, [], errsM, warnsM)

-- Compile a single module.
upsweep_mod :: Finder
            -> ModThreaded
            -> ModSummary
            -> IO (ModThreaded, Maybe Linkable, [SDoc], [SDoc])

upsweep_mod finder threaded1 summary1
   = do let mod_name = name_of_summary summary1
        let (ModThreaded pcs1 hst1 hit1) = threaded1
        let old_iface = lookupFM hit1 (name_of_summary summary1)
        compresult <- cmCompile finder summary1 old_iface hst1 pcs1

        case compresult of

           -- Compilation "succeeded", but didn't return a new iface or
           -- linkable, meaning that compilation wasn't needed, and the
           -- new details were manufactured from the old iface.
           CompOK details Nothing pcs2 warns
              -> let hst2      = addToFM hst1 mod_name details
                     hit2      = hit1
                     threaded2 = ModThreaded pcs2 hst2 hit2
                 in  return (threaded2, Nothing, [], warns)

           -- Compilation really did happen, and succeeded.  A new
           -- details, iface and linkable are returned.
           CompOK details (Just (new_iface, new_linkable)) pcs2 warns
              -> let hst2      = addToFM hst1 mod_name details
                     hit2      = addToFM hit1 mod_name new_iface
                     threaded2 = ModThreaded pcs2 hst2 hit2
                 in  return (threaded2, Just new_linkable, [], warns)

           -- Compilation failed.  compile may still have updated
           -- the PCS, tho.
           CompErrs pcs2 errs warns
              -> let threaded2 = ModThreaded pcs2 hst1 hit1
                 in  return (threaded2, Nothing, errs, warns)
         
filterTopLevelEnvs :: (ModName -> Bool) -> (HST, HIT, UI) -> (HST, HIT, UI)
filterTopLevelEnvs p (hst, hit, ui)
   = (filterFM (\k v -> p k) hst,
      filterFM (\k v -> p k) hit,
      filterModuleLinkables p ui
     )

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
           = do found <- finder nm
		case found of
		   Just (mod, location) -> summarise mod location
		   Nothing -> panic ("CompManager: can't find module `" ++ 
					showSDoc (ppr nm) ++ "'")

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
