%
% (c) The University of Glasgow, 2000
%
\section[CompManager]{The Compilation Manager}

\begin{code}
module CompManager ( cmInit, cmLoadModule, cmUnload,
#ifdef GHCI
                     cmGetExpr, cmRunExpr,
#endif
                     CmState, emptyCmState  -- abstract
                   )
where

#include "HsVersions.h"

import CmLink
import CmTypes
import HscTypes
import Module		( ModuleName, moduleName,
			  isHomeModule, moduleEnvElts,
			  moduleNameUserString )
import CmStaticInfo	( GhciMode(..) )
import DriverPipeline
import GetImports
import HscTypes		( HomeSymbolTable, HomeIfaceTable, 
			  PersistentCompilerState, ModDetails(..) )
import Name		( lookupNameEnv )
import Module
import PrelNames	( mainName )
import HscMain		( initPersistentCompilerState )
import Finder
import UniqFM		( emptyUFM, lookupUFM, addToUFM, delListFromUFM,
			  UniqFM, listToUFM )
import Unique		( Uniquable )
import Digraph		( SCC(..), stronglyConnComp )
import DriverFlags	( getDynFlags )
import DriverPhases
import DriverUtil	( BarfKind(..), splitFilename3 )
import ErrUtils		( showPass )
import Util
import DriverUtil
import Outputable
import Panic		( panic )
import CmdLineOpts	( DynFlags(..) )

#ifdef GHCI
import Interpreter	( HValue )
import HscMain		( hscExpr )
import RdrName
import Type		( Type )
import PrelGHC		( unsafeCoerce# )
#endif

-- lang
import Exception	( throwDyn )

-- std
import Time             ( ClockTime )
import Directory        ( getModificationTime, doesFileExist )
import IO
import Monad
import List		( nub )
import Maybe		( catMaybes, fromMaybe, isJust )
\end{code}


\begin{code}
cmInit :: GhciMode -> IO CmState
cmInit gmode
   = emptyCmState gmode

#ifdef GHCI
cmGetExpr :: CmState
	  -> DynFlags
          -> ModuleName
          -> String
          -> IO (CmState, Maybe (HValue, PrintUnqualified, Type))
cmGetExpr cmstate dflags modname expr
   = do (new_pcs, maybe_stuff) <- 
	   hscExpr dflags hst hit pcs (mkHomeModule modname) expr
        case maybe_stuff of
	   Nothing     -> return (cmstate{ pcs=new_pcs }, Nothing)
	   Just (uiexpr, print_unqual, ty) -> do
		hValue <- linkExpr pls uiexpr
	        return (cmstate{ pcs=new_pcs }, 
			Just (hValue, print_unqual, ty))

   -- ToDo: check that the module we passed in is sane/exists?
   where
       CmState{ pcs=pcs, pcms=pcms, pls=pls } = cmstate
       PersistentCMState{ hst=hst, hit=hit } = pcms

-- The HValue should represent a value of type IO () (Perhaps IO a?)
cmRunExpr :: HValue -> IO ()
cmRunExpr hval
   = do unsafeCoerce# hval :: IO ()
	-- putStrLn "done."
#endif

-- Persistent state just for CM, excluding link & compile subsystems
data PersistentCMState
   = PersistentCMState {
        hst   :: HomeSymbolTable,    -- home symbol table
        hit   :: HomeIfaceTable,     -- home interface table
        ui    :: UnlinkedImage,      -- the unlinked images
        mg    :: ModuleGraph,        -- the module graph
        gmode :: GhciMode            -- NEVER CHANGES
     }

emptyPCMS :: GhciMode -> PersistentCMState
emptyPCMS gmode
  = PersistentCMState { hst = emptyHST, hit = emptyHIT,
                        ui  = emptyUI,  mg  = emptyMG, 
                        gmode = gmode }

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

emptyCmState :: GhciMode -> IO CmState
emptyCmState gmode
    = do let pcms = emptyPCMS gmode
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

Unload the compilation manager's state: everything it knows about the
current collection of modules in the Home package.

\begin{code}
cmUnload :: CmState -> IO CmState
cmUnload state 
 = do -- Throw away the old home dir cache
      emptyHomeDirCache
      -- Throw away the HIT and the HST
      return state{ pcms=pcms{ hst=new_hst, hit=new_hit } }
   where
     CmState{ pcms=pcms } = state
     PersistentCMState{ hst=hst, hit=hit } = pcms
     (new_hst, new_hit) = retainInTopLevelEnvs [] (hst,hit)
\end{code}

The real business of the compilation manager: given a system state and
a module name, try and bring the module up to date, probably changing
the system state at the same time.

\begin{code}
cmLoadModule :: CmState 
             -> FilePath
             -> IO (CmState,		-- new state
		    Bool, 		-- was successful
		    [ModuleName])	-- list of modules loaded

cmLoadModule cmstate1 rootname
   = do -- version 1's are the original, before downsweep
        let pcms1     = pcms   cmstate1
        let pls1      = pls    cmstate1
        let pcs1      = pcs    cmstate1
        let mg1       = mg     pcms1
        let hst1      = hst    pcms1
        let hit1      = hit    pcms1
        let ui1       = ui     pcms1
   
        let ghci_mode = gmode pcms1 -- this never changes

        -- Do the downsweep to reestablish the module graph
        -- then generate version 2's by removing from HIT,HST,UI any
        -- modules in the old MG which are not in the new one.

	dflags <- getDynFlags
        let verb = verbosity dflags

	showPass dflags "Chasing dependencies"
        when (verb >= 1 && ghci_mode == Batch) $
           hPutStrLn stderr (prog_name ++ ": chasing modules from: " ++ rootname)

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

        --hPutStrLn stderr "after tsort:\n"
        --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Because we don't take into account source imports when doing
        -- the topological sort, there shouldn't be any cycles in mg2.
        -- If there is, we complain and give up -- the user needs to
        -- break the cycle using a boot file.

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

        let threaded2 = CmThreaded pcs1 hst2 hit2

        (upsweep_complete_success, threaded3, modsDone, newLis)
           <- upsweep_mods ghci_mode dflags ui2 reachable_from threaded2 mg2

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
           do when (verb >= 2) $ 
		hPutStrLn stderr "Upsweep completely successful."
              linkresult 
                 <- link ghci_mode dflags 
			(any exports_main (moduleEnvElts hst3)) 
                        newLis pls1
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (1)"
                 LinkOK pls3 
                    -> do let pcms3 = PersistentCMState { hst=hst3, hit=hit3, 
                                                          ui=ui3, mg=modsDone, 
                                                          gmode=ghci_mode }
                          let cmstate3 
                                 = CmState { pcms=pcms3, pcs=pcs3, pls=pls3 }
                          return (cmstate3, True, map name_of_summary modsDone)

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do when (verb >= 2) $
		hPutStrLn stderr "Upsweep partially successful."

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
                     = map (unJust "linkables_to_link" . findModuleLinkable_maybe ui4)
                           mods_to_keep_names

              linkresult <- link ghci_mode dflags False linkables_to_link pls1
              case linkresult of
                 LinkErrs _ _
                    -> panic "cmLoadModule: link failed (2)"
                 LinkOK pls4
                    -> do let pcms4 = PersistentCMState { hst=hst4, hit=hit4, 
                                                          ui=ui4, mg=mods_to_keep,
                                                          gmode=ghci_mode }
                          let cmstate4 
                                 = CmState { pcms=pcms4, pcs=pcs3, pls=pls4 }
                          return (cmstate4, False, mods_to_keep_names)


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
   = isJust (lookupNameEnv (md_types md) mainName)


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
	     -> DynFlags
             -> UnlinkedImage         -- old linkables
             -> (ModuleName -> [ModuleName])  -- to construct downward closures
             -> CmThreaded            -- PCS & HST & HIT
             -> [SCC ModSummary]      -- mods to do (the worklist)
                                      -- ...... RETURNING ......
             -> IO (Bool{-complete success?-},
                    CmThreaded,
                    [ModSummary],     -- mods which succeeded
                    [Linkable])       -- new linkables

upsweep_mods ghci_mode dflags oldUI reachable_from threaded 
     []
   = return (True, threaded, [], [])

upsweep_mods ghci_mode dflags oldUI reachable_from threaded 
     ((CyclicSCC ms):_)
   = do hPutStrLn stderr ("Module imports form a cycle for modules:\n\t" ++
                          unwords (map (moduleNameUserString.name_of_summary) ms))
        return (False, threaded, [], [])

upsweep_mods ghci_mode dflags oldUI reachable_from threaded 
     ((AcyclicSCC mod):mods)
   = do (threaded1, maybe_linkable) 
           <- upsweep_mod ghci_mode dflags oldUI threaded mod 
                          (reachable_from (name_of_summary mod)) 
        case maybe_linkable of
           Just linkable 
              -> -- No errors; do the rest
                 do (restOK, threaded2, modOKs, linkables) 
                       <- upsweep_mods ghci_mode dflags oldUI reachable_from 
                                       threaded1 mods
                    return (restOK, threaded2, mod:modOKs, linkable:linkables)
           Nothing -- we got a compilation error; give up now
              -> return (False, threaded1, [], [])


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
maybe_getFileLinkable :: ModuleName -> FilePath -> IO (Maybe Linkable)
maybe_getFileLinkable mod_name obj_fn
   = do obj_exist <- doesFileExist obj_fn
        if not obj_exist 
         then return Nothing 
         else 
         do let stub_fn = case splitFilename3 obj_fn of
                             (dir, base, ext) -> dir ++ "/" ++ base ++ ".stub_o"
            stub_exist <- doesFileExist stub_fn
            obj_time <- getModificationTime obj_fn
            if stub_exist
             then return (Just (LM obj_time mod_name [DotO obj_fn, DotO stub_fn]))
             else return (Just (LM obj_time mod_name [DotO obj_fn]))


upsweep_mod :: GhciMode 
	    -> DynFlags
            -> UnlinkedImage
            -> CmThreaded
            -> ModSummary
            -> [ModuleName]
            -> IO (CmThreaded, Maybe Linkable)

upsweep_mod ghci_mode dflags oldUI threaded1 summary1 reachable_from_here
   = do 
        let mod_name = name_of_summary summary1
	let verb = verbosity dflags

        when (verb == 1) $
	   if (ghci_mode == Batch)
		then hPutStr stderr (prog_name ++ ": module " 
                       	++ moduleNameUserString mod_name
			++ ": ")
		else hPutStr stderr ("Compiling "
		       	++ moduleNameUserString mod_name
		 	++ " ... ")

        let (CmThreaded pcs1 hst1 hit1) = threaded1
        let old_iface = lookupUFM hit1 mod_name

        let maybe_oldUI_linkable = findModuleLinkable_maybe oldUI mod_name
        maybe_oldDisk_linkable
           <- case ml_obj_file (ms_location summary1) of
                 Nothing -> return Nothing
                 Just obj_fn -> maybe_getFileLinkable mod_name obj_fn

        -- The most recent of the old UI linkable or whatever we could
        -- find on disk.  Is returned as the linkable if compile
        -- doesn't think we need to recompile.        
        let maybe_old_linkable
               = case (maybe_oldUI_linkable, maybe_oldDisk_linkable) of
                    (Nothing, Nothing) -> Nothing
                    (Nothing, Just di) -> Just di
                    (Just ui, Nothing) -> Just ui
                    (Just ui, Just di)
                       | linkableTime ui >= linkableTime di -> Just ui
                       | otherwise                          -> Just di

        let compilation_mandatory
               = case maybe_old_linkable of
                    Nothing -> True
                    Just li -> case ms_hs_date summary1 of
                                  Nothing -> panic "compilation_mandatory:no src date"
                                  Just src_date -> src_date >= linkableTime li
            source_unchanged
               = not compilation_mandatory

            (hst1_strictDC, hit1_strictDC)
               = retainInTopLevelEnvs reachable_from_here (hst1,hit1)

            old_linkable 
               = unJust "upsweep_mod:old_linkable" maybe_old_linkable

        compresult <- compile ghci_mode summary1 source_unchanged
                         old_iface hst1_strictDC hit1_strictDC pcs1

        case compresult of

           -- Compilation "succeeded", but didn't return a new
           -- linkable, meaning that compilation wasn't needed, and the
           -- new details were manufactured from the old iface.
           CompOK pcs2 new_details new_iface Nothing
              -> do let hst2         = addToUFM hst1 mod_name new_details
                        hit2         = addToUFM hit1 mod_name new_iface
                        threaded2    = CmThreaded pcs2 hst2 hit2

		    if ghci_mode == Interactive && verb >= 1 then
		      -- if we're using an object file, tell the user
		      case maybe_old_linkable of
			Just (LM _ _ objs@(DotO _:_))
			   -> do hPutStr stderr (showSDoc (space <> 
				   parens (hsep (text "using": 
					punctuate comma 
					  [ text o | DotO o <- objs ]))))
			         when (verb > 1) $ hPutStrLn stderr ""
			_ -> return ()
		      else
			return ()

		    when (verb == 1) $ hPutStrLn stderr ""
                    return (threaded2, Just old_linkable)

           -- Compilation really did happen, and succeeded.  A new
           -- details, iface and linkable are returned.
           CompOK pcs2 new_details new_iface (Just new_linkable)
              -> do let hst2      = addToUFM hst1 mod_name new_details
                        hit2      = addToUFM hit1 mod_name new_iface
                        threaded2 = CmThreaded pcs2 hst2 hit2

		    when (verb == 1) $ hPutStrLn stderr ""
	            return (threaded2, Just new_linkable)

           -- Compilation failed.  compile may still have updated
           -- the PCS, tho.
           CompErrs pcs2
	      -> do let threaded2 = CmThreaded pcs2 hst1 hit1
		    when (verb == 1) $ hPutStrLn stderr ""
                    return (threaded2, Nothing)

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
         --trace (showSDoc (text "DC of mod" <+> ppr root
         --                 <+> text "=" <+> ppr res)) (
         res
         --)

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
downsweep :: [FilePath] -> IO [ModSummary]
downsweep rootNm
   = do rootSummaries <- mapM getRootSummary rootNm
        loop (filter (isHomeModule.ms_mod) rootSummaries)
     where
	getRootSummary :: FilePath -> IO ModSummary
	getRootSummary file
	   | haskellish_file file
	   = do exists <- doesFileExist file
		if exists then summariseFile file else do
		throwDyn (OtherError ("can't find file `" ++ file ++ "'"))	
	   | otherwise
 	   = do exists <- doesFileExist hs_file
		if exists then summariseFile hs_file else do
		exists <- doesFileExist lhs_file
		if exists then summariseFile lhs_file else do
		getSummary (mkModuleName file)
           where 
		 hs_file = file ++ ".hs"
		 lhs_file = file ++ ".lhs"

        getSummary :: ModuleName -> IO ModSummary
        getSummary nm
           -- | trace ("getSummary: "++ showSDoc (ppr nm)) True
           = do found <- findModule nm
		case found of
                   -- Be sure not to use the mod and location passed in to 
                   -- summarise for any other purpose -- summarise may change
                   -- the module names in them if name of module /= name of file,
                   -- and put the changed versions in the returned summary.
                   -- These will then conflict with the passed-in versions.
		   Just (mod, location) -> summarise mod location
		   Nothing -> throwDyn (OtherError 
                                   ("can't find module `" 
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
                       = filter (isHomeModule.ms_mod) neededSummaries
                if null newHomeSummaries
                 then return homeSummaries
                 else loop (newHomeSummaries ++ homeSummaries)


-----------------------------------------------------------------------------
-- Summarising modules

-- We have two types of summarisation:
--
--    * Summarise a file.  This is used for the root module passed to
--	cmLoadModule.  The file is read, and used to determine the root
--	module name.  The module name may differ from the filename.
--
--    * Summarise a module.  We are given a module name, and must provide
--	a summary.  The finder is used to locate the file in which the module
--	resides.

summariseFile :: FilePath -> IO ModSummary
summariseFile file
   = do hspp_fn <- preprocess file
        modsrc <- readFile hspp_fn

        let (srcimps,imps,mod_name) = getImports modsrc
	    (path, basename, ext) = splitFilename3 file

	Just (mod, location)
	   <- mkHomeModuleLocn mod_name (path ++ '/':basename) file
	   
        maybe_src_timestamp
           <- case ml_hs_file location of 
                 Nothing     -> return Nothing
                 Just src_fn -> maybe_getModificationTime src_fn

        return (ModSummary mod
                           location{ml_hspp_file=Just hspp_fn}
                           srcimps imps
                           maybe_src_timestamp)

-- Summarise a module, and pick up source and interface timestamps.
summarise :: Module -> ModuleLocation -> IO ModSummary
summarise mod location
   | isHomeModule mod
   = do let hs_fn = unJust "summarise" (ml_hs_file location)
        hspp_fn <- preprocess hs_fn
        modsrc <- readFile hspp_fn
        let (srcimps,imps,mod_name) = getImports modsrc

        maybe_src_timestamp
           <- case ml_hs_file location of 
                 Nothing     -> return Nothing
                 Just src_fn -> maybe_getModificationTime src_fn

	if mod_name == moduleName mod
		then return ()
		else throwDyn (OtherError 
			(showSDoc (text "file name does not match module name: "
			   <+> ppr (moduleName mod) <+> text "vs" 
			   <+> ppr mod_name)))

        return (ModSummary mod location{ml_hspp_file=Just hspp_fn} 
                               srcimps imps
                               maybe_src_timestamp)

   | otherwise
   = return (ModSummary mod location [] [] Nothing)

maybe_getModificationTime :: FilePath -> IO (Maybe ClockTime)
maybe_getModificationTime fn
   = (do time <- getModificationTime fn
         return (Just time)) 
     `catch`
     (\err -> return Nothing)
\end{code}
