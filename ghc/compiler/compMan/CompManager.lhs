%
% (c) The University of Glasgow, 2000
%
\section[CompManager]{The Compilation Manager}

\begin{code}
module CompManager ( cmInit, cmLoadModule, 
                     cmGetExpr, cmRunExpr,
                     CmState, emptyCmState,  -- abstract
		     cmLookupSymbol --tmp
                   )
where

#include "HsVersions.h"

import List		( nub )
import Maybe		( catMaybes, maybeToList, fromMaybe )
import Maybes		( maybeToBool )
import Outputable
import UniqFM		( emptyUFM, lookupUFM, addToUFM, delListFromUFM,
			  UniqFM, listToUFM )
import Unique		( Uniquable )
import Digraph		( SCC(..), stronglyConnComp, flattenSCC, flattenSCCs )

import CmLink
import CmTypes
import HscTypes
import Interpreter	( HValue )
import Module		( ModuleName, moduleName, packageOfModule, 
			  isModuleInThisPackage, PackageName, moduleEnvElts,
			  moduleNameUserString )
import CmStaticInfo	( Package(..), PackageConfigInfo, GhciMode(..) )
import DriverPipeline
import GetImports
import HscTypes		( HomeSymbolTable, HomeIfaceTable, 
			  PersistentCompilerState, ModDetails(..) )
import Name		( lookupNameEnv )
import RdrName
import Module
import PrelNames	( mainName )
import HscMain		( initPersistentCompilerState )
import Finder		( findModule, emptyHomeDirCache )
import DriverUtil	( BarfKind(..) )
import Util
import Panic		( panic )

import Exception	( throwDyn )
import IO
import Time             ( ClockTime )
import Directory        ( getModificationTime, doesFileExist )

\end{code}



\begin{code}
cmInit :: PackageConfigInfo -> GhciMode -> IO CmState
cmInit raw_package_info gmode
   = emptyCmState raw_package_info gmode

cmGetExpr :: CmState
          -> ModuleName
          -> String
          -> IO (CmState, Either [SDoc] HValue)
cmGetExpr cmstate modhdl expr
   = return (panic "cmGetExpr:unimp")

cmRunExpr :: HValue -> IO ()
cmRunExpr hval
   = return (panic "cmRunExpr:unimp")


-- Persistent state just for CM, excluding link & compile subsystems
data PersistentCMState
   = PersistentCMState {
        hst   :: HomeSymbolTable,    -- home symbol table
        hit   :: HomeIfaceTable,     -- home interface table
        ui    :: UnlinkedImage,      -- the unlinked images
        mg    :: ModuleGraph,        -- the module graph
        pci   :: PackageConfigInfo,  -- NEVER CHANGES
        gmode :: GhciMode            -- NEVER CHANGES
     }

emptyPCMS :: PackageConfigInfo -> GhciMode -> PersistentCMState
emptyPCMS pci gmode
  = PersistentCMState { hst = emptyHST, hit = emptyHIT,
                        ui  = emptyUI,  mg  = emptyMG, 
                        pci = pci, gmode = gmode }

emptyHIT :: HomeIfaceTable
emptyHIT = emptyUFM
emptyHST :: HomeSymbolTable
emptyHST = emptyUFM



-- Persistent state for the entire system
data CmState
   = CmState {
        pcms   :: PersistentCMState,       -- CM's persistent state
        pcs    :: PersistentCompilerState, -- compile's persistent state
        pls    :: PersistentLinkerState    -- link's persistent state
     }

emptyCmState :: PackageConfigInfo -> GhciMode -> IO CmState
emptyCmState pci gmode
    = do let pcms = emptyPCMS pci gmode
         pcs     <- initPersistentCompilerState
         pls     <- emptyPLS
         return (CmState { pcms   = pcms,
                           pcs    = pcs,
                           pls    = pls })

-- CM internal types
type UnlinkedImage = [Linkable]	-- the unlinked images (should be a set, really)
emptyUI :: UnlinkedImage
emptyUI = []

type ModuleGraph = [ModSummary]  -- the module graph, topologically sorted
emptyMG :: ModuleGraph
emptyMG = []

\end{code}

The real business of the compilation manager: given a system state and
a module name, try and bring the module up to date, probably changing
the system state at the same time.

\begin{code}
cmLoadModule :: CmState 
             -> ModuleName
             -> IO (CmState, Maybe ModuleName)

cmLoadModule cmstate1 rootname
   = do -- version 1's are the original, before downsweep
        let pcms1     = pcms   cmstate1
        let pls1      = pls    cmstate1
        let pcs1      = pcs    cmstate1
        let mg1       = mg     pcms1
        let hst1      = hst    pcms1
        let hit1      = hit    pcms1
        let ui1       = ui     pcms1
   
        let pcii      = pci   pcms1 -- this never changes
        let ghci_mode = gmode pcms1 -- ToDo: fix!

        -- During upsweep, look at new summaries to see if source has
        -- changed.  Here's a function to pass down; it takes a new
        -- summary.
        let source_changed :: ModSummary -> Bool
            source_changed = summary_indicates_source_changed mg1

        -- Do the downsweep to reestablish the module graph
        -- then generate version 2's by removing from HIT,HST,UI any
        -- modules in the old MG which are not in the new one.

        -- Throw away the old home dir cache
        emptyHomeDirCache

        hPutStr stderr "cmLoadModule: downsweep begins\n"
        mg2unsorted <- downsweep [rootname]

        let modnames1   = map name_of_summary mg1
        let modnames2   = map name_of_summary mg2unsorted
        let mods_to_zap = filter (`notElem` modnames2) modnames1

        let (hst2, hit2, ui2)
               = removeFromTopLevelEnvs mods_to_zap (hst1, hit1, ui1)
        -- should be cycle free; ignores 'import source's
        let mg2 = topological_sort False mg2unsorted
        -- ... whereas this takes them into account.  Used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hst/hit all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps = topological_sort True mg2unsorted
      
        let reachable_from :: ModuleName -> [ModuleName]
            reachable_from = downwards_closure_of_module mg2unsorted

        hPutStrLn stderr "after tsort:\n"
        hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Because we don't take into account source imports when doing
        -- the topological sort, there shouldn't be any cycles in mg2.
        -- If there is, we complain and give up -- the user needs to
        -- break the cycle using a boot file.

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

        let threaded2 = CmThreaded pcs1 hst2 hit2

        (upsweep_complete_success, threaded3, modsDone, newLis)
           <- upsweep_mods ghci_mode ui2 reachable_from source_changed threaded2 mg2

        let ui3 = add_to_ui ui2 newLis
        let (CmThreaded pcs3 hst3 hit3) = threaded3

        -- At this point, modsDone and newLis should have the same
        -- length, so there is one new (or old) linkable for each 
        -- mod which was processed (passed to compile).

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if upsweep_complete_success

         then 
           -- Easy; just relink it all.
           do hPutStrLn stderr "UPSWEEP COMPLETELY SUCCESSFUL"
              linkresult 
                 <- link ghci_mode (any exports_main (moduleEnvElts hst3)) 
                         newLis pls1
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (1)"
                 LinkOK pls3 
                    -> do let pcms3 = PersistentCMState { hst=hst3, hit=hit3, 
                                                          ui=ui3, mg=modsDone, 
                                                          pci=pcii, gmode=ghci_mode }
                          let cmstate3 
                                 = CmState { pcms=pcms3, pcs=pcs3, pls=pls3 }
                          return (cmstate3, Just rootname)

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do hPutStrLn stderr "UPSWEEP PARTIALLY SUCCESSFUL"

              let modsDone_names
                     = map name_of_summary modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names mg2_with_srcimps
              let (hst4, hit4, ui4) 
                     = removeFromTopLevelEnvs mods_to_zap_names (hst3,hit3,ui3)
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).name_of_summary) modsDone
              let mods_to_keep_names 
                     = map name_of_summary mods_to_keep
              -- we could get the relevant linkables by filtering newLis, but
              -- it seems easier to drag them out of the updated, cleaned-up UI
              let linkables_to_link 
                     = map (findModuleLinkable ui4) mods_to_keep_names

              linkresult <- link ghci_mode False linkables_to_link pls1
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (2)"
                 LinkOK pls4
                    -> do let pcms4 = PersistentCMState { hst=hst4, hit=hit4, 
                                                          ui=ui4, mg=mods_to_keep,
                                                          pci=pcii, gmode=ghci_mode }
                          let cmstate4 
                                 = CmState { pcms=pcms4, pcs=pcs3, pls=pls4 }
                          return (cmstate4, 
                                  -- choose rather arbitrarily who to return
                                  if null mods_to_keep then Nothing 
                                     else Just (last mods_to_keep_names))


-- Given a bunch of old summaries and a new summary, try and
-- find the corresponding old summary, and, if found, compare
-- its source timestamp with that of the new summary.  If in
-- doubt say True.
summary_indicates_source_changed :: [ModSummary] -> ModSummary -> Bool
summary_indicates_source_changed old_summaries new_summary
   = case [old | old <- old_summaries, 
                 name_of_summary old == name_of_summary new_summary] of

        (_:_:_) -> panic "summary_indicates_newer_source"
                   
        []      -> -- can't find a corresponding old summary, so
                   -- compare source and iface dates in the new summary.
                   trace (showSDoc (text "SISC: no old summary, new =" 
                                    <+> pprSummaryTimes new_summary)) (
                   case (ms_hs_date new_summary, ms_hi_date new_summary) of
                      (Just hs_t, Just hi_t) -> hs_t > hi_t
                      other                  -> True
                   )

        [old]   -> -- found old summary; compare source timestamps
                   trace (showSDoc (text "SISC: old =" 
                                    <+> pprSummaryTimes old
                                    <+> pprSummaryTimes new_summary)) (
                   case (ms_hs_date old, ms_hs_date new_summary) of
                      (Just old_t, Just new_t) -> new_t > old_t
                      other                    -> True
                   )

-- Return (names of) all those in modsDone who are part of a cycle
-- as defined by theGraph.
findPartiallyCompletedCycles :: [ModuleName] -> [SCC ModSummary] -> [ModuleName]
findPartiallyCompletedCycles modsDone theGraph
   = chew theGraph
     where
        chew [] = []
        chew ((AcyclicSCC v):rest) = chew rest    -- acyclic?  not interesting.
        chew ((CyclicSCC vs):rest)
           = let names_in_this_cycle = nub (map name_of_summary vs)
                 mods_in_this_cycle  
                    = nub ([done | done <- modsDone, 
                                   done `elem` names_in_this_cycle])
                 chewed_rest = chew rest
             in 
             if   not (null mods_in_this_cycle) 
                  && length mods_in_this_cycle < length names_in_this_cycle
             then mods_in_this_cycle ++ chewed_rest
             else chewed_rest


-- Does this ModDetails export Main.main?
exports_main :: ModDetails -> Bool
exports_main md
   = maybeToBool (lookupNameEnv (md_types md) mainName)


-- Add the given (LM-form) Linkables to the UI, overwriting previous
-- versions if they exist.
add_to_ui :: UnlinkedImage -> [Linkable] -> UnlinkedImage
add_to_ui ui lis
   = foldr add1 ui lis
     where
        add1 :: Linkable -> UnlinkedImage -> UnlinkedImage
        add1 li ui
           = li : filter (\li2 -> not (for_same_module li li2)) ui

        for_same_module :: Linkable -> Linkable -> Bool
        for_same_module li1 li2 
           = not (is_package_linkable li1)
             && not (is_package_linkable li2)
             && modname_of_linkable li1 == modname_of_linkable li2
                                  

data CmThreaded  -- stuff threaded through individual module compilations
   = CmThreaded PersistentCompilerState HomeSymbolTable HomeIfaceTable


-- Compile multiple modules, stopping as soon as an error appears.
-- There better had not be any cyclic groups here -- we check for them.
upsweep_mods :: GhciMode
             -> UnlinkedImage         -- old linkables
             -> (ModuleName -> [ModuleName])  -- to construct downward closures
             -> (ModSummary -> Bool)  -- has source changed?
             -> CmThreaded            -- PCS & HST & HIT
             -> [SCC ModSummary]      -- mods to do (the worklist)
                                      -- ...... RETURNING ......
             -> IO (Bool{-complete success?-},
                    CmThreaded,
                    [ModSummary],     -- mods which succeeded
                    [Linkable])       -- new linkables

upsweep_mods ghci_mode oldUI reachable_from source_changed threaded []
   = return (True, threaded, [], [])

upsweep_mods ghci_mode oldUI reachable_from source_changed threaded ((CyclicSCC ms):_)
   = do hPutStrLn stderr ("ghc: module imports form a cycle for modules:\n\t" ++
                          unwords (map (moduleNameUserString.name_of_summary) ms))
        return (False, threaded, [], [])

upsweep_mods ghci_mode oldUI reachable_from source_changed threaded ((AcyclicSCC mod):mods)
   = do (threaded1, maybe_linkable) 
           <- upsweep_mod ghci_mode oldUI threaded mod 
                          (reachable_from (name_of_summary mod)) 
                          (source_changed mod)
        case maybe_linkable of
           Just linkable 
              -> -- No errors; do the rest
                 do (restOK, threaded2, modOKs, linkables) 
                       <- upsweep_mods ghci_mode oldUI reachable_from source_changed threaded1 mods
                    return (restOK, threaded2, mod:modOKs, linkable:linkables)
           Nothing -- we got a compilation error; give up now
              -> return (False, threaded1, [], [])


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: GhciMode 
            -> UnlinkedImage
            -> CmThreaded
            -> ModSummary
            -> [ModuleName]
            -> Bool
            -> IO (CmThreaded, Maybe Linkable)

upsweep_mod ghci_mode oldUI threaded1 summary1 
            reachable_from_here source_might_have_changed
   = do let mod_name = name_of_summary summary1
        let (CmThreaded pcs1 hst1 hit1) = threaded1
        let old_iface = lookupUFM hit1 (name_of_summary summary1)

        -- We *have* to compile it if we're in batch mode and we can't see
        -- a previous linkable for it on disk.
        compilation_mandatory 
           <- if ghci_mode /= Batch then return False 
              else case ml_obj_file (ms_location summary1) of
                      Nothing     -> do --putStrLn "cmcm: object?!"
                                        return True
                      Just obj_fn -> do --putStrLn ("cmcm: old obj " ++ obj_fn)
                                        b <- doesFileExist obj_fn
                                        return (not b)

        let compilation_might_be_needed 
               = source_might_have_changed || compilation_mandatory
            source_unchanged
               = not compilation_might_be_needed
            (hst1_strictDC, hit1_strictDC)
               = retainInTopLevelEnvs reachable_from_here (hst1,hit1)

        compresult <- compile ghci_mode summary1 source_unchanged
                         old_iface hst1_strictDC hit1_strictDC pcs1

        --putStrLn ( "UPSWEEP_MOD: smhc = " ++ show source_might_have_changed 
        --           ++ ",  cman = " ++ show compilation_mandatory)

        case compresult of

           -- Compilation "succeeded", but didn't return a new iface or
           -- linkable, meaning that compilation wasn't needed, and the
           -- new details were manufactured from the old iface.
           CompOK details Nothing pcs2
              -> let hst2         = addToUFM hst1 mod_name details
                     hit2         = hit1
                     threaded2    = CmThreaded pcs2 hst2 hit2
                     old_linkable 
                        | ghci_mode == Interactive 
                        = findModuleLinkable oldUI mod_name
                        | otherwise
                        = LM mod_name
                             [DotO (unJust (ml_obj_file (ms_location summary1)) 
                                    "upsweep_mod")]
                 in  return (threaded2, Just old_linkable)

           -- Compilation really did happen, and succeeded.  A new
           -- details, iface and linkable are returned.
           CompOK details (Just (new_iface, new_linkable)) pcs2
              -> let hst2      = addToUFM hst1 mod_name details
                     hit2      = addToUFM hit1 mod_name new_iface
                     threaded2 = CmThreaded pcs2 hst2 hit2
                 in  return (threaded2, Just new_linkable)

           -- Compilation failed.  compile may still have updated
           -- the PCS, tho.
           CompErrs pcs2
              -> let threaded2 = CmThreaded pcs2 hst1 hit1
                 in  return (threaded2, Nothing)


-- Remove unwanted modules from the top level envs (HST, HIT, UI).
removeFromTopLevelEnvs :: [ModuleName]
                       -> (HomeSymbolTable, HomeIfaceTable, UnlinkedImage)
                       -> (HomeSymbolTable, HomeIfaceTable, UnlinkedImage)
removeFromTopLevelEnvs zap_these (hst, hit, ui)
   = (delListFromUFM hst zap_these,
      delListFromUFM hit zap_these,
      filterModuleLinkables (`notElem` zap_these) ui
     )

retainInTopLevelEnvs :: [ModuleName]
                        -> (HomeSymbolTable, HomeIfaceTable)
                        -> (HomeSymbolTable, HomeIfaceTable)
retainInTopLevelEnvs keep_these (hst, hit)
   = (retainInUFM hst keep_these,
      retainInUFM hit keep_these
     )
     where
        retainInUFM :: Uniquable key => UniqFM elt -> [key] -> UniqFM elt
        retainInUFM ufm keys_to_keep
           = listToUFM (concatMap (maybeLookupUFM ufm) keys_to_keep)
        maybeLookupUFM ufm u 
           = case lookupUFM ufm u of Nothing -> []; Just val -> [(u, val)] 

-- Needed to clean up HIT and HST so that we don't get duplicates in inst env
downwards_closure_of_module :: [ModSummary] -> ModuleName -> [ModuleName]
downwards_closure_of_module summaries root
   = let toEdge :: ModSummary -> (ModuleName,[ModuleName])
         toEdge summ
             = (name_of_summary summ, ms_srcimps summ ++ ms_imps summ)
         res = simple_transitive_closure (map toEdge summaries) [root]             
     in
         trace (showSDoc (text "DC of mod" <+> ppr root
                          <+> text "=" <+> ppr res)) (
         res
         )

-- Calculate transitive closures from a set of roots given an adjacency list
simple_transitive_closure :: Eq a => [(a,[a])] -> [a] -> [a]
simple_transitive_closure graph set 
   = let set2      = nub (concatMap dsts set ++ set)
         dsts node = fromMaybe [] (lookup node graph)
     in
         if   length set == length set2
         then set
         else simple_transitive_closure graph set2


-- Calculate SCCs of the module graph, with or without taking into
-- account source imports.
topological_sort :: Bool -> [ModSummary] -> [SCC ModSummary]
topological_sort include_source_imports summaries
   = let 
         toEdge :: ModSummary -> (ModSummary,ModuleName,[ModuleName])
         toEdge summ
             = (summ, name_of_summary summ, 
                      (if include_source_imports 
                       then ms_srcimps summ else []) ++ ms_imps summ)
        
         mash_edge :: (ModSummary,ModuleName,[ModuleName]) -> (ModSummary,Int,[Int])
         mash_edge (summ, m, m_imports)
            = case lookup m key_map of
                 Nothing -> panic "reverse_topological_sort"
                 Just mk -> (summ, mk, 
                                -- ignore imports not from the home package
                                catMaybes (map (flip lookup key_map) m_imports))

         edges     = map toEdge summaries
         key_map   = zip [nm | (s,nm,imps) <- edges] [1 ..] :: [(ModuleName,Int)]
         scc_input = map mash_edge edges
         sccs      = stronglyConnComp scc_input
     in
         sccs


-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.
downsweep :: [ModuleName] -> IO [ModSummary]
downsweep rootNm
   = do rootSummaries <- mapM getSummary rootNm
        loop (filter (isModuleInThisPackage.ms_mod) rootSummaries)
     where
        getSummary :: ModuleName -> IO ModSummary
        getSummary nm
           | trace ("getSummary: "++ showSDoc (ppr nm)) True
           = do found <- findModule nm
		case found of
		   Just (mod, location) -> summarise mod location
		   Nothing -> throwDyn (OtherError 
                                   ("no signs of life for module `" 
                                     ++ showSDoc (ppr nm) ++ "'"))
                                 
        -- loop invariant: homeSummaries doesn't contain package modules
        loop :: [ModSummary] -> IO [ModSummary]
        loop homeSummaries
           = do let allImps :: [ModuleName]
                    allImps = (nub . concatMap ms_imps) homeSummaries
                let allHome   -- all modules currently in homeSummaries
                       = map (moduleName.ms_mod) homeSummaries
                let neededImps
                       = filter (`notElem` allHome) allImps
                neededSummaries
                       <- mapM getSummary neededImps
                let newHomeSummaries
                       = filter (isModuleInThisPackage.ms_mod) neededSummaries
                if null newHomeSummaries
                 then return homeSummaries
                 else loop (newHomeSummaries ++ homeSummaries)


-- Summarise a module, and pick and source and interface timestamps.
summarise :: Module -> ModuleLocation -> IO ModSummary
summarise mod location
   | isModuleInThisPackage mod
   = do let hs_fn = unJust (ml_hs_file location) "summarise"
        hspp_fn <- preprocess hs_fn
        modsrc <- readFile hspp_fn
        let (srcimps,imps) = getImports modsrc

        maybe_src_timestamp
           <- case ml_hs_file location of 
                 Nothing     -> return Nothing
                 Just src_fn -> maybe_getModificationTime src_fn
        maybe_iface_timestamp
           <- case ml_hi_file location of 
                 Nothing     -> return Nothing
                 Just if_fn  -> maybe_getModificationTime if_fn

        return (ModSummary mod location{ml_hspp_file=Just hspp_fn} 
                               srcimps imps
                               maybe_src_timestamp maybe_iface_timestamp)
   | otherwise
   = return (ModSummary mod location [] [] Nothing Nothing)

   where
      maybe_getModificationTime :: FilePath -> IO (Maybe ClockTime)
      maybe_getModificationTime fn
         = (do time <- getModificationTime fn
               return (Just time)) 
           `catch`
           (\err -> return Nothing)

cmLookupSymbol :: RdrName -> CmState -> Maybe HValue
cmLookupSymbol nm CmState{ pls = pls } = lookupClosure nm pls
\end{code}
