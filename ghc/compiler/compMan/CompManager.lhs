%
% (c) The University of Glasgow, 2002
%
% The Compilation Manager
%
\begin{code}
{-# OPTIONS -fvia-C #-}
module CompManager ( 
    ModuleGraph, 

    CmState, emptyCmState,  -- abstract

    cmInit, 	   -- :: GhciMode -> IO CmState

    cmDepAnal,	   -- :: CmState -> DynFlags -> [FilePath] -> IO ModuleGraph

    cmLoadModules, -- :: CmState -> DynFlags -> ModuleGraph
		   --	 -> IO (CmState, [String])

    cmUnload,	   -- :: CmState -> DynFlags -> IO CmState

    cmSetContext,  -- :: CmState -> String -> IO CmState

    cmGetContext,  -- :: CmState -> IO String

#ifdef GHCI
    cmInfoThing,   -- :: CmState -> DynFlags -> String -> IO (Maybe TyThing)

    CmRunResult(..),
    cmRunStmt,	   -- :: CmState -> DynFlags -> String
		   --	 -> IO (CmState, CmRunResult)

    cmTypeOfExpr,  -- :: CmState -> DynFlags -> String
		   -- 	-> IO (CmState, Maybe String)

    cmTypeOfName,  -- :: CmState -> Name -> IO (Maybe String)

    cmCompileExpr, -- :: CmState -> DynFlags -> String 
		   -- 	-> IO (CmState, Maybe HValue)#endif
#endif
  )
where

#include "HsVersions.h"

import CmLink
import CmTypes
import DriverPipeline
import DriverState	( v_Output_file )
import DriverPhases
import DriverUtil
import Finder
#ifdef GHCI
import HscMain		( initPersistentCompilerState, hscThing )
#else
import HscMain		( initPersistentCompilerState )
#endif
import HscTypes
import Name		( Name, NamedThing(..), nameRdrName, nameModule,
			  isHomePackageName )
import RdrName		( emptyRdrEnv )
import Module
import GetImports
import UniqFM
import Unique		( Uniquable )
import Digraph		( SCC(..), stronglyConnComp, flattenSCC, flattenSCCs )
import ErrUtils		( showPass )
import SysTools		( cleanTempFilesExcept )
import Util
import Outputable
import Panic
import CmdLineOpts	( DynFlags(..), getDynFlags )

import IOExts

#ifdef GHCI
import RdrName		( lookupRdrEnv )
import Id		( idType, idName )
import NameEnv
import Type		( tidyType )
import VarEnv		( emptyTidyEnv )
import RnEnv		( unQualInScope, mkIfaceGlobalRdrEnv )
import BasicTypes	( Fixity, defaultFixity )
import Interpreter	( HValue )
import HscMain		( hscStmt )
import PrelGHC		( unsafeCoerce# )

import Foreign
import CForeign
import Exception	( Exception, try )
#endif

-- lang
import Exception	( throwDyn )

-- std
import Directory        ( getModificationTime, doesFileExist )
import IO
import Monad
import List		( nub )
import Maybe
\end{code}


\begin{code}
-- Persistent state for the entire system
data CmState
   = CmState {
        hst   :: HomeSymbolTable,    -- home symbol table
        hit   :: HomeIfaceTable,     -- home interface table
        ui    :: UnlinkedImage,      -- the unlinked images
        mg    :: ModuleGraph,        -- the module graph
        gmode :: GhciMode,           -- NEVER CHANGES
	ic    :: InteractiveContext, -- command-line binding info

        pcs    :: PersistentCompilerState, -- compile's persistent state
        pls    :: PersistentLinkerState    -- link's persistent state
     }

emptyCmState :: GhciMode -> Module -> IO CmState
emptyCmState gmode mod
    = do pcs     <- initPersistentCompilerState
         pls     <- emptyPLS
         return (CmState { hst    = emptySymbolTable,
                           hit    = emptyIfaceTable,
                           ui     = emptyUI,
                           mg     = emptyMG, 
                           gmode  = gmode,
			   ic     = emptyInteractiveContext mod,
                           pcs    = pcs,
                           pls    = pls })

emptyInteractiveContext mod
  = InteractiveContext { ic_module = mod, 
			 ic_rn_env = emptyRdrEnv,
			 ic_type_env = emptyTypeEnv }

defaultCurrentModuleName = mkModuleName "Prelude"
GLOBAL_VAR(defaultCurrentModule, error "no defaultCurrentModule", Module)

-- CM internal types
type UnlinkedImage = [Linkable]	-- the unlinked images (should be a set, really)
emptyUI :: UnlinkedImage
emptyUI = []

type ModuleGraph = [ModSummary]  -- the module graph, topologically sorted
emptyMG :: ModuleGraph
emptyMG = []

-----------------------------------------------------------------------------
-- Produce an initial CmState.

cmInit :: GhciMode -> IO CmState
cmInit mode = do
   prel <- moduleNameToModule defaultCurrentModuleName
   writeIORef defaultCurrentModule prel
   emptyCmState mode prel

-----------------------------------------------------------------------------
-- Setting the context doesn't throw away any bindings; the bindings
-- we've built up in the InteractiveContext simply move to the new
-- module.  They always shadow anything in scope in the current context.

cmSetContext :: CmState -> String -> IO CmState
cmSetContext cmstate str
   = do let mn = mkModuleName str
	    modules_loaded = [ (name_of_summary s, ms_mod s) | s <- mg cmstate ]

        m <- case lookup mn modules_loaded of
		Just m  -> return m
		Nothing -> do
		   mod <- moduleNameToModule mn
		   if isHomeModule mod 
			then throwDyn (CmdLineError (showSDoc 
				(quotes (ppr (moduleName mod))
 				  <+> text "is not currently loaded")))
		   	else return mod

	return cmstate{ ic = (ic cmstate){ic_module=m} }
		
cmGetContext :: CmState -> IO String
cmGetContext cmstate = return (moduleUserString (ic_module (ic cmstate)))

moduleNameToModule :: ModuleName -> IO Module
moduleNameToModule mn
 = do maybe_stuff <- findModule mn
      case maybe_stuff of
	Nothing -> throwDyn (CmdLineError ("can't find module `"
				    ++ moduleNameUserString mn ++ "'"))
	Just (m,_) -> return m

-----------------------------------------------------------------------------
-- cmInfoThing: convert a String to a TyThing

-- A string may refer to more than one TyThing (eg. a constructor,
-- and type constructor), so we return a list of all the possible TyThings.

#ifdef GHCI
cmInfoThing :: CmState -> DynFlags -> String 
	-> IO (CmState, PrintUnqualified, [(TyThing,Fixity)])
cmInfoThing cmstate dflags id
   = do (new_pcs, things) <- hscThing dflags hst hit pcs icontext id
	let pairs = map (\x -> (x, getFixity new_pcs (getName x))) things
	return (cmstate{ pcs=new_pcs }, unqual, pairs)
   where 
     CmState{ hst=hst, hit=hit, pcs=pcs, pls=pls, ic=icontext } = cmstate
     unqual = getUnqual pcs hit icontext

     getFixity :: PersistentCompilerState -> Name -> Fixity
     getFixity pcs name
	| Just iface  <- lookupModuleEnv iface_table (nameModule name),
	  Just fixity <- lookupNameEnv (mi_fixities iface) name
	  = fixity
	| otherwise
	  = defaultFixity
	where iface_table | isHomePackageName name = hit
			  | otherwise              = pcs_PIT pcs
#endif

-----------------------------------------------------------------------------
-- cmRunStmt:  Run a statement/expr.

#ifdef GHCI
data CmRunResult
  = CmRunOk [Name] 		-- names bound by this evaluation
  | CmRunFailed 
  | CmRunDeadlocked		-- statement deadlocked
  | CmRunException Exception	-- statement raised an exception

cmRunStmt :: CmState -> DynFlags -> String -> IO (CmState, CmRunResult)		
cmRunStmt cmstate@CmState{ hst=hst, hit=hit, pcs=pcs, pls=pls, ic=icontext }
          dflags expr
   = do 
	let InteractiveContext { 
	       	ic_rn_env = rn_env, 
	       	ic_type_env = type_env } = icontext

        (new_pcs, maybe_stuff) 
	    <- hscStmt dflags hst hit pcs icontext expr False{-stmt-}

        case maybe_stuff of
	   Nothing -> return (cmstate{ pcs=new_pcs }, CmRunFailed)
	   Just (ids, _, bcos) -> do

		-- update the interactive context
	        let 
		    names = map idName ids

		    -- these names have just been shadowed
		    shadowed = [ n | r <- map nameRdrName names,
				     Just n <- [lookupRdrEnv rn_env r] ]
		    
		    new_rn_env   = extendLocalRdrEnv rn_env names

		    -- remove any shadowed bindings from the type_env
		    filtered_type_env = delListFromNameEnv type_env shadowed

		    new_type_env = extendNameEnvList filtered_type_env 	
			      		[ (getName id, AnId id)	| id <- ids]

		    new_ic = icontext { ic_rn_env   = new_rn_env, 
			  	  	ic_type_env = new_type_env }

		-- link it
		hval <- linkExpr pls bcos

		-- run it!
		let thing_to_run = unsafeCoerce# hval :: IO [HValue]
		either_hvals <- sandboxIO thing_to_run
		case either_hvals of
		   Left err
			| err == dEADLOCKED
			-> return ( cmstate{ pcs=new_pcs, ic=new_ic }, 
				    CmRunDeadlocked )
			| otherwise
			-> do hPutStrLn stderr ("unknown failure, code " ++ show err)
			      return ( cmstate{ pcs=new_pcs, ic=new_ic }, CmRunFailed )

		   Right maybe_hvals ->
		     case maybe_hvals of
			Left e -> 
			    return ( cmstate{ pcs=new_pcs, ic=new_ic }, 
				     CmRunException e )
			Right hvals -> do
			     -- Get the newly bound things, and bind them.  
			     -- Don't forget to delete any shadowed bindings from the
			     -- closure_env, lest we end up with a space leak.
			     pls <- delListFromClosureEnv pls shadowed
			     new_pls <- addListToClosureEnv pls (zip names hvals)
	     
			     return (cmstate{ pcs=new_pcs, pls=new_pls, ic=new_ic }, 
				     CmRunOk names)

-- We run the statement in a "sandbox", which amounts to calling into
-- the RTS to request a new main thread.  The main benefit is that we
-- get to detect a deadlock this way, but also there's no danger that
-- exceptions raised by the expression can affect the interpreter.

sandboxIO :: IO a -> IO (Either Int (Either Exception a))
sandboxIO thing = do
  st_thing <- newStablePtr (Exception.try thing)
  alloca $ \ p_st_result -> do
    stat <- rts_evalStableIO st_thing p_st_result
    freeStablePtr st_thing
    if stat == 1
	then do st_result <- peek p_st_result
		result <- deRefStablePtr st_result
		freeStablePtr st_result
		return (Right result)
	else do
		return (Left (fromIntegral stat))

-- ToDo: slurp this in from ghc/includes/RtsAPI.h somehow
dEADLOCKED = 4 :: Int

foreign import "rts_evalStableIO"  {- safe -}
  rts_evalStableIO :: StablePtr (IO a) -> Ptr (StablePtr a) -> IO CInt
  -- more informative than the C type!
#endif

-----------------------------------------------------------------------------
-- cmTypeOfExpr: returns a string representing the type of an expression

#ifdef GHCI
cmTypeOfExpr :: CmState -> DynFlags -> String -> IO (CmState, Maybe String)
cmTypeOfExpr cmstate dflags expr
   = do (new_pcs, maybe_stuff) 
	  <- hscStmt dflags hst hit pcs ic expr True{-just an expr-}

	let new_cmstate = cmstate{pcs = new_pcs}

	case maybe_stuff of
	   Nothing -> return (new_cmstate, Nothing)
	   Just (_, ty, _) -> return (new_cmstate, Just str)
 	     where 
		str = showSDocForUser unqual (ppr tidy_ty)
		unqual  = getUnqual pcs hit ic
		tidy_ty = tidyType emptyTidyEnv ty
   where
       CmState{ hst=hst, hit=hit, pcs=pcs, ic=ic } = cmstate

getUnqual pcs hit ic
   = case lookupIfaceByModName hit pit modname of
	Nothing    -> alwaysQualify
	Just iface -> 
	   case mi_globals iface of
	      Just env -> unQualInScope env
	      Nothing  -> unQualInScope (mkIfaceGlobalRdrEnv (mi_exports iface))
  where
    pit = pcs_PIT pcs
    modname = moduleName (ic_module ic)
#endif

-----------------------------------------------------------------------------
-- cmTypeOfName: returns a string representing the type of a name.

#ifdef GHCI
cmTypeOfName :: CmState -> Name -> IO (Maybe String)
cmTypeOfName CmState{ hit=hit, pcs=pcs, ic=ic } name
 = case lookupNameEnv (ic_type_env ic) name of
	Nothing -> return Nothing
	Just (AnId id) -> return (Just str)
	   where
	     unqual = getUnqual pcs hit ic
	     ty = tidyType emptyTidyEnv (idType id)
	     str = showSDocForUser unqual (ppr ty)

	_ -> panic "cmTypeOfName"
#endif

-----------------------------------------------------------------------------
-- cmCompileExpr: compile an expression and deliver an HValue

#ifdef GHCI
cmCompileExpr :: CmState -> DynFlags -> String -> IO (CmState, Maybe HValue)
cmCompileExpr cmstate dflags expr
   = do 
	let InteractiveContext { 
	       	ic_rn_env = rn_env, 
	       	ic_type_env = type_env,
	       	ic_module   = this_mod } = icontext

        (new_pcs, maybe_stuff) 
	    <- hscStmt dflags hst hit pcs icontext 
		  ("let __cmCompileExpr = "++expr) False{-stmt-}

        case maybe_stuff of
	   Nothing -> return (cmstate{ pcs=new_pcs }, Nothing)
	   Just (ids, _, bcos) -> do

		-- link it
		hval <- linkExpr pls bcos

		-- run it!
		let thing_to_run = unsafeCoerce# hval :: IO [HValue]
		hvals <- thing_to_run

		case (ids,hvals) of
		  ([id],[hv]) -> return (cmstate{ pcs=new_pcs }, Just hv)
		  _ -> panic "cmCompileExpr"

   where
       CmState{ hst=hst, hit=hit, pcs=pcs, pls=pls, ic=icontext } = cmstate
#endif

-----------------------------------------------------------------------------
-- Unload the compilation manager's state: everything it knows about the
-- current collection of modules in the Home package.

cmUnload :: CmState -> DynFlags -> IO CmState
cmUnload state@CmState{ gmode=mode, pls=pls, pcs=pcs } dflags
 = do -- Throw away the old home dir cache
      emptyHomeDirCache

      -- Unload everything the linker knows about
      new_pls <- CmLink.unload mode dflags [] pls 

      -- Start with a fresh CmState, but keep the PersistentCompilerState
      new_state <- cmInit mode
      return new_state{ pcs=pcs, pls=new_pls }


-----------------------------------------------------------------------------
-- Trace dependency graph

-- This is a seperate pass so that the caller can back off and keep
-- the current state if the downsweep fails.

cmDepAnal :: CmState -> DynFlags -> [FilePath] -> IO ModuleGraph
cmDepAnal cmstate dflags rootnames
  = do showPass dflags "Chasing dependencies"
       when (verbosity dflags >= 1 && gmode cmstate == Batch) $
           hPutStrLn stderr (showSDoc (hcat [
	     text progName, text ": chasing modules from: ",
	     hcat (punctuate comma (map text rootnames))]))
       downsweep rootnames (mg cmstate)

-----------------------------------------------------------------------------
-- The real business of the compilation manager: given a system state and
-- a module name, try and bring the module up to date, probably changing
-- the system state at the same time.

cmLoadModules :: CmState 
	     -> DynFlags
             -> ModuleGraph
             -> IO (CmState,		-- new state
		    Bool, 		-- was successful
		    [String])		-- list of modules loaded

cmLoadModules cmstate1 dflags mg2unsorted
   = do -- version 1's are the original, before downsweep
        let pls1      = pls    cmstate1
        let pcs1      = pcs    cmstate1
        let hst1      = hst    cmstate1
        let hit1      = hit    cmstate1
	-- similarly, ui1 is the (complete) set of linkables from
	-- the previous pass, if any.
        let ui1       = ui     cmstate1

        let ghci_mode = gmode cmstate1 -- this never changes

        -- Do the downsweep to reestablish the module graph
        let verb = verbosity dflags

	-- Find out if we have a Main module
        let a_root_is_Main 
               = any ((=="Main").moduleNameUserString.name_of_summary) 
                     mg2unsorted

        let mg2unsorted_names = map name_of_summary mg2unsorted

        -- reachable_from follows source as well as normal imports
        let reachable_from :: ModuleName -> [ModuleName]
            reachable_from = downwards_closure_of_module mg2unsorted
 
        -- should be cycle free; ignores 'import source's
        let mg2 = topological_sort False mg2unsorted
        -- ... whereas this takes them into account.  Used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hst/hit all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps = topological_sort True mg2unsorted

	-- Sort out which linkables we wish to keep in the unlinked image.
	-- See getValidLinkables below for details.
	valid_linkables <- getValidLinkables ui1 mg2unsorted_names 
				mg2_with_srcimps
	-- when (verb >= 2) $
        --    putStrLn (showSDoc (text "Valid linkables:" 
        -- 			 <+> ppr valid_linkables))

        -- Figure out a stable set of modules which can be retained
        -- the top level envs, to avoid upsweeping them.  Goes to a
        -- bit of trouble to avoid upsweeping module cycles.
        --
        -- Construct a set S of stable modules like this:
        -- Travel upwards, over the sccified graph.  For each scc
        -- of modules ms, add ms to S only if:
        -- 1.  All home imports of ms are either in ms or S
        -- 2.  A valid linkable exists for each module in ms

        stable_mods <- preUpsweep valid_linkables hit1 
		 		  mg2unsorted_names [] mg2_with_srcimps

        let stable_summaries
               = concatMap (findInSummaries mg2unsorted) stable_mods

	    stable_linkables
	       = filter (\m -> linkableModName m `elem` stable_mods) 
		    valid_linkables

        when (verb >= 2) $
           putStrLn (showSDoc (text "Stable modules:" 
                               <+> sep (map (text.moduleNameUserString) stable_mods)))

	-- unload any modules which aren't going to be re-linked this
	-- time around.
	pls2 <- CmLink.unload ghci_mode dflags stable_linkables pls1

        -- We could at this point detect cycles which aren't broken by
        -- a source-import, and complain immediately, but it seems better
        -- to let upsweep_mods do this, so at least some useful work gets
        -- done before the upsweep is abandoned.
        let upsweep_these
               = filter (\scc -> any (`notElem` stable_mods) 
                                     (map name_of_summary (flattenSCC scc)))
                        mg2

        --hPutStrLn stderr "after tsort:\n"
        --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Because we don't take into account source imports when doing
        -- the topological sort, there shouldn't be any cycles in mg2.
        -- If there is, we complain and give up -- the user needs to
        -- break the cycle using a boot file.

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

        let threaded2 = CmThreaded pcs1 hst1 hit1

	-- clean up between compilations
	let cleanup = cleanTempFilesExcept verb 
			  (ppFilesFromSummaries (flattenSCCs upsweep_these))

        (upsweep_complete_success, threaded3, modsUpswept, newLis)
           <- upsweep_mods ghci_mode dflags valid_linkables reachable_from 
                           threaded2 cleanup upsweep_these

        let ui3 = add_to_ui valid_linkables newLis
        let (CmThreaded pcs3 hst3 hit3) = threaded3

        -- At this point, modsUpswept and newLis should have the same
        -- length, so there is one new (or old) linkable for each 
        -- mod which was processed (passed to compile).

	-- Make modsDone be the summaries for each home module now
	-- available; this should equal the domains of hst3 and hit3.
	-- (NOT STRICTLY TRUE if an interactive session was started
	--  with some object on disk ???)
        -- Get in in a roughly top .. bottom order (hence reverse).

        let modsDone = reverse modsUpswept ++ stable_summaries

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if upsweep_complete_success

         then 
           -- Easy; just relink it all.
           do when (verb >= 2) $ 
		 hPutStrLn stderr "Upsweep completely successful."

	      -- clean up after ourselves
	      cleanTempFilesExcept verb (ppFilesFromSummaries modsDone)

	      -- issue a warning for the confusing case where the user said '-o foo'
	      -- but we're not going to do any linking.
	      ofile <- readIORef v_Output_file
	      when (ghci_mode == Batch && isJust ofile && not a_root_is_Main
		     && verb > 0) $
	         hPutStrLn stderr "Warning: output was redirected with -o, but no output will be generated\nbecause there is no Main module."

	      -- link everything together
              linkresult <- link ghci_mode dflags a_root_is_Main ui3 pls2

	      cmLoadFinish True linkresult 
			hst3 hit3 ui3 modsDone ghci_mode pcs3

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do when (verb >= 2) $
		hPutStrLn stderr "Upsweep partially successful."

              let modsDone_names
                     = map name_of_summary modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).name_of_summary) 
			  modsDone

              let (hst4, hit4, ui4)
                     = retainInTopLevelEnvs (map name_of_summary mods_to_keep) 
                                            (hst3,hit3,ui3)

	      -- clean up after ourselves
	      cleanTempFilesExcept verb (ppFilesFromSummaries mods_to_keep)

	      -- link everything together
              linkresult <- link ghci_mode dflags False ui4 pls2

	      cmLoadFinish False linkresult 
		    hst4 hit4 ui4 mods_to_keep ghci_mode pcs3


-- Finish up after a cmLoad.

-- If the link failed, unload everything and return.
cmLoadFinish ok (LinkFailed pls) hst hit ui mods ghci_mode pcs = do
  dflags <- getDynFlags
  new_pls <- CmLink.unload ghci_mode dflags [] pls 
  new_state <- cmInit ghci_mode
  return (new_state{ pcs=pcs, pls=new_pls }, False, [])

-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
cmLoadFinish ok (LinkOK pls) hst hit ui mods ghci_mode pcs
  = do def_mod <- readIORef defaultCurrentModule
       let current_mod = case mods of 
				[]    -> def_mod
				(x:_) -> ms_mod x

       	   new_ic = emptyInteractiveContext current_mod

           new_cmstate = CmState{ hst=hst, hit=hit, ui=ui, mg=mods,
                                  gmode=ghci_mode, pcs=pcs, pls=pls,
				  ic = new_ic }
           mods_loaded = map (moduleNameUserString.name_of_summary) mods

       return (new_cmstate, ok, mods_loaded)

-- used to fish out the preprocess output files for the purposes
-- of cleaning up.
ppFilesFromSummaries summaries
  = [ fn | Just fn <- map toPpFile summaries ]
  where
   toPpFile sum
     | hspp /= ml_hs_file loc = hspp
     | otherwise              = Nothing
    where
      loc  = ms_location sum
      hspp = ml_hspp_file loc


-----------------------------------------------------------------------------
-- getValidLinkables

-- For each module (or SCC of modules), we take:
--
--	- an on-disk linkable, if this is the first time around and one
--	  is available.
--
--	- the old linkable, otherwise (and if one is available).
--
-- and we throw away the linkable if it is older than the source
-- file.  We ignore the on-disk linkables unless all of the dependents
-- of this SCC also have on-disk linkables.
--
-- If a module has a valid linkable, then it may be STABLE (see below),
-- and it is classified as SOURCE UNCHANGED for the purposes of calling
-- compile.
--
-- ToDo: this pass could be merged with the preUpsweep.

getValidLinkables
	:: [Linkable]		-- old linkables
	-> [ModuleName]		-- all home modules
	-> [SCC ModSummary]	-- all modules in the program, dependency order
	-> IO [Linkable]	-- still-valid linkables 

getValidLinkables old_linkables all_home_mods module_graph
  = foldM (getValidLinkablesSCC old_linkables all_home_mods) [] module_graph

getValidLinkablesSCC old_linkables all_home_mods new_linkables scc0
   = let 
	  scc             = flattenSCC scc0
          scc_names       = map name_of_summary scc
	  home_module m   = m `elem` all_home_mods && m `notElem` scc_names
          scc_allhomeimps = nub (filter home_module (concatMap ms_imps scc))
		-- NOTE: ms_imps, not ms_allimps above.  We don't want to
		-- force a module's SOURCE imports to be already compiled for
		-- its object linkable to be valid.

	  has_object m = case findModuleLinkable_maybe new_linkables m of
			    Nothing -> False
			    Just l  -> isObjectLinkable l

          objects_allowed = all has_object scc_allhomeimps
     in do

     these_linkables 
	<- foldM (getValidLinkable old_linkables objects_allowed) [] scc

	-- since an scc can contain only all objects or no objects at all,
	-- we have to check whether we got all objects or not, and re-do
	-- the linkable check if not.
     adjusted_linkables 
	<- if objects_allowed && not (all isObjectLinkable these_linkables)
	      then foldM (getValidLinkable old_linkables False) [] scc
	      else return these_linkables

     return (adjusted_linkables ++ new_linkables)


getValidLinkable :: [Linkable] -> Bool -> [Linkable] -> ModSummary 
	-> IO [Linkable]
getValidLinkable old_linkables objects_allowed new_linkables summary 
  = do let mod_name = name_of_summary summary

       maybe_disk_linkable
          <- if (not objects_allowed)
		then return Nothing
		else case ml_obj_file (ms_location summary) of
                 	Just obj_fn -> maybe_getFileLinkable mod_name obj_fn
                 	Nothing -> return Nothing

       let old_linkable = findModuleLinkable_maybe old_linkables mod_name
	   maybe_old_linkable =
	  	case old_linkable of
		    Just l | not (isObjectLinkable l) || stillThere l 
				-> old_linkable
				-- ToDo: emit a warning if not (stillThere l)
                    other -> Nothing

	   -- make sure that if we had an old disk linkable around, that it's
	   -- still there on the disk (in case we need to re-link it).
	   stillThere l = 
		case maybe_disk_linkable of
		   Nothing    -> False
		   Just l_disk -> linkableTime l == linkableTime l_disk

    	   -- we only look for objects on disk the first time around;
    	   -- if the user compiles a module on the side during a GHCi session,
    	   -- it won't be picked up until the next ":load".  This is what the
    	   -- "null old_linkables" test below is.
           linkable | null old_linkables = maybeToList maybe_disk_linkable
		    | otherwise          = maybeToList maybe_old_linkable

           -- only linkables newer than the source code are valid
           src_date = ms_hs_date summary

	   valid_linkable
	      =  filter (\l -> linkableTime l >= src_date) linkable
		-- why '>=' rather than '>' above?  If the filesystem stores
		-- times to the nearset second, we may occasionally find that
		-- the object & source have the same modification time, 
		-- especially if the source was automatically generated
		-- and compiled.  Using >= is slightly unsafe, but it matches
		-- make's behaviour.

       return (valid_linkable ++ new_linkables)


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


-----------------------------------------------------------------------------
-- Do a pre-upsweep without use of "compile", to establish a 
-- (downward-closed) set of stable modules for which we won't call compile.

-- a stable module:
--	* has a valid linkable (see getValidLinkables above)
--	* depends only on stable modules
--	* has an interface in the HIT (interactive mode only)

preUpsweep :: [Linkable]	-- new valid linkables
	   -> HomeIfaceTable
           -> [ModuleName]      -- names of all mods encountered in downsweep
           -> [ModuleName]      -- accumulating stable modules
           -> [SCC ModSummary]  -- scc-ified mod graph, including src imps
           -> IO [ModuleName]	-- stable modules

preUpsweep valid_lis hit all_home_mods stable []  = return stable
preUpsweep valid_lis hit all_home_mods stable (scc0:sccs)
   = do let scc = flattenSCC scc0
            scc_allhomeimps :: [ModuleName]
            scc_allhomeimps 
               = nub (filter (`elem` all_home_mods) (concatMap ms_allimps scc))
            all_imports_in_scc_or_stable
               = all in_stable_or_scc scc_allhomeimps
            scc_names
               = map name_of_summary scc
            in_stable_or_scc m
               = m `elem` scc_names || m `elem` stable

	    -- now we check for valid linkables: each module in the SCC must 
	    -- have a valid linkable (see getValidLinkables above).
	    has_valid_linkable new_summary
   	      = isJust (findModuleLinkable_maybe valid_lis modname)
	       where modname = name_of_summary new_summary

	    has_interface summary = ms_mod summary `elemUFM` hit

	    scc_is_stable = all_imports_in_scc_or_stable
			  && all has_valid_linkable scc
			  && all has_interface scc

        if scc_is_stable
         then preUpsweep valid_lis hit all_home_mods (scc_names++stable) sccs
         else preUpsweep valid_lis hit all_home_mods stable sccs


-- Helper for preUpsweep.  Assuming that new_summary's imports are all
-- stable (in the sense of preUpsweep), determine if new_summary is itself
-- stable, and, if so, in batch mode, return its linkable.
findInSummaries :: [ModSummary] -> ModuleName -> [ModSummary]
findInSummaries old_summaries mod_name
   = [s | s <- old_summaries, name_of_summary s == mod_name]

findModInSummaries :: [ModSummary] -> Module -> Maybe ModSummary
findModInSummaries old_summaries mod
   = case [s | s <- old_summaries, ms_mod s == mod] of
	 [] -> Nothing
	 (s:_) -> Just s

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


-- Add the given (LM-form) Linkables to the UI, overwriting previous
-- versions if they exist.
add_to_ui :: UnlinkedImage -> [Linkable] -> UnlinkedImage
add_to_ui ui lis
   = filter (not_in lis) ui ++ lis
     where
        not_in :: [Linkable] -> Linkable -> Bool
        not_in lis li
           = all (\l -> linkableModName l /= mod) lis
           where mod = linkableModName li
                                  

data CmThreaded  -- stuff threaded through individual module compilations
   = CmThreaded PersistentCompilerState HomeSymbolTable HomeIfaceTable


-- Compile multiple modules, stopping as soon as an error appears.
-- There better had not be any cyclic groups here -- we check for them.
upsweep_mods :: GhciMode
	     -> DynFlags
             -> UnlinkedImage         -- valid linkables
             -> (ModuleName -> [ModuleName])  -- to construct downward closures
             -> CmThreaded            -- PCS & HST & HIT
	     -> IO ()		      -- how to clean up unwanted tmp files
             -> [SCC ModSummary]      -- mods to do (the worklist)
                                      -- ...... RETURNING ......
             -> IO (Bool{-complete success?-},
                    CmThreaded,
                    [ModSummary],     -- mods which succeeded
                    [Linkable])       -- new linkables

upsweep_mods ghci_mode dflags oldUI reachable_from threaded cleanup
     []
   = return (True, threaded, [], [])

upsweep_mods ghci_mode dflags oldUI reachable_from threaded cleanup
     ((CyclicSCC ms):_)
   = do hPutStrLn stderr ("Module imports form a cycle for modules:\n\t" ++
                          unwords (map (moduleNameUserString.name_of_summary) ms))
        return (False, threaded, [], [])

upsweep_mods ghci_mode dflags oldUI reachable_from threaded cleanup
     ((AcyclicSCC mod):mods)
   = do --case threaded of
        --   CmThreaded pcsz hstz hitz
        --      -> putStrLn ("UPSWEEP_MOD: hit = " ++ show (map (moduleNameUserString.moduleName.mi_module) (eltsUFM hitz)))

        (threaded1, maybe_linkable) 
           <- upsweep_mod ghci_mode dflags oldUI threaded mod 
                          (reachable_from (name_of_summary mod))

	-- remove unwanted tmp files between compilations
	cleanup

        case maybe_linkable of
           Just linkable 
              -> -- No errors; do the rest
                 do (restOK, threaded2, modOKs, linkables) 
                       <- upsweep_mods ghci_mode dflags oldUI reachable_from 
                                       threaded1 cleanup mods
                    return (restOK, threaded2, mod:modOKs, linkable:linkables)
           Nothing -- we got a compilation error; give up now
              -> return (False, threaded1, [], [])


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: GhciMode 
	    -> DynFlags
            -> UnlinkedImage
            -> CmThreaded
            -> ModSummary
            -> [ModuleName]
            -> IO (CmThreaded, Maybe Linkable)

upsweep_mod ghci_mode dflags oldUI threaded1 summary1 reachable_inc_me
   = do 
        let mod_name = name_of_summary summary1

        let (CmThreaded pcs1 hst1 hit1) = threaded1
        let old_iface = lookupUFM hit1 mod_name

        let maybe_old_linkable = findModuleLinkable_maybe oldUI mod_name

            source_unchanged = isJust maybe_old_linkable

	    reachable_only = filter (/= (name_of_summary summary1)) 
				reachable_inc_me

	   -- in interactive mode, all home modules below us *must* have an
	   -- interface in the HIT.  We never demand-load home interfaces in
	   -- interactive mode.
            (hst1_strictDC, hit1_strictDC, [])
               = ASSERT(ghci_mode == Batch || 
			all (`elemUFM` hit1) reachable_only)
		 retainInTopLevelEnvs reachable_only (hst1,hit1,[])

            old_linkable 
               = unJust "upsweep_mod:old_linkable" maybe_old_linkable

	    have_object 
	       | Just l <- maybe_old_linkable, isObjectLinkable l = True
	       | otherwise = False

        compresult <- compile ghci_mode summary1 source_unchanged
			 have_object old_iface hst1_strictDC hit1_strictDC pcs1

        case compresult of

           -- Compilation "succeeded", and may or may not have returned a new
           -- linkable (depending on whether compilation was actually performed
	   -- or not).
           CompOK pcs2 new_details new_iface maybe_new_linkable
              -> do let hst2      = addToUFM hst1 mod_name new_details
                        hit2      = addToUFM hit1 mod_name new_iface
                        threaded2 = CmThreaded pcs2 hst2 hit2

                    return (threaded2, if isJust maybe_new_linkable
					  then maybe_new_linkable
					  else Just old_linkable)

           -- Compilation failed.  compile may still have updated
           -- the PCS, tho.
           CompErrs pcs2
	      -> do let threaded2 = CmThreaded pcs2 hst1 hit1
                    return (threaded2, Nothing)

-- Filter modules in the top level envs (HST, HIT, UI).
retainInTopLevelEnvs :: [ModuleName]
                        -> (HomeSymbolTable, HomeIfaceTable, UnlinkedImage)
                        -> (HomeSymbolTable, HomeIfaceTable, UnlinkedImage)
retainInTopLevelEnvs keep_these (hst, hit, ui)
   = (retainInUFM hst keep_these,
      retainInUFM hit keep_these,
      filterModuleLinkables (`elem` keep_these) ui
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
         toEdge summ = (name_of_summary summ, 
			filter (`elem` all_mods) (ms_allimps summ))

	 all_mods = map name_of_summary summaries

         res = simple_transitive_closure (map toEdge summaries) [root]
     in
--         trace (showSDoc (text "DC of mod" <+> ppr root
--                          <+> text "=" <+> ppr res)) $
         res

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


-----------------------------------------------------------------------------
-- Downsweep (dependency analysis)

-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.

-- We pass in the previous collection of summaries, which is used as a
-- cache to avoid recalculating a module summary if the source is
-- unchanged.

downsweep :: [FilePath] -> [ModSummary] -> IO [ModSummary]
downsweep roots old_summaries
   = do rootSummaries <- mapM getRootSummary roots
        all_summaries
           <- loop (concat (map ms_imps rootSummaries))
		(mkModuleEnv [ (mod, s) | s <- rootSummaries, 
					  let mod = ms_mod s, isHomeModule mod 
			     ])
        return all_summaries
     where
	getRootSummary :: FilePath -> IO ModSummary
	getRootSummary file
	   | haskellish_src_file file
	   = do exists <- doesFileExist file
		if exists then summariseFile file else do
		throwDyn (CmdLineError ("can't find file `" ++ file ++ "'"))	
	   | otherwise
 	   = do exists <- doesFileExist hs_file
		if exists then summariseFile hs_file else do
		exists <- doesFileExist lhs_file
		if exists then summariseFile lhs_file else do
		let mod_name = mkModuleName file
		maybe_summary <- getSummary mod_name
		case maybe_summary of
		   Nothing -> packageModErr mod_name
		   Just s  -> return s
           where 
		 hs_file = file ++ ".hs"
		 lhs_file = file ++ ".lhs"

        getSummary :: ModuleName -> IO (Maybe ModSummary)
        getSummary nm
           = do found <- findModule nm
		case found of
		   Just (mod, location) -> do
			let old_summary = findModInSummaries old_summaries mod
			summarise mod location old_summary

		   Nothing -> throwDyn (CmdLineError 
                                   ("can't find module `" 
                                     ++ showSDoc (ppr nm) ++ "'"))

        -- loop invariant: env doesn't contain package modules
        loop :: [ModuleName] -> ModuleEnv ModSummary -> IO [ModSummary]
	loop [] env = return (moduleEnvElts env)
        loop imps env
           = do -- imports for modules we don't already have
                let needed_imps = nub (filter (not . (`elemUFM` env)) imps)

		-- summarise them
                needed_summaries <- mapM getSummary needed_imps

		-- get just the "home" modules
                let new_home_summaries = [ s | Just s <- needed_summaries ]

		-- loop, checking the new imports
		let new_imps = concat (map ms_imps new_home_summaries)
                loop new_imps (extendModuleEnvList env 
				[ (ms_mod s, s) | s <- new_home_summaries ])

-----------------------------------------------------------------------------
-- Summarising modules

-- We have two types of summarisation:
--
--    * Summarise a file.  This is used for the root module(s) passed to
--	cmLoadModules.  The file is read, and used to determine the root
--	module name.  The module name may differ from the filename.
--
--    * Summarise a module.  We are given a module name, and must provide
--	a summary.  The finder is used to locate the file in which the module
--	resides.

summariseFile :: FilePath -> IO ModSummary
summariseFile file
   = do hspp_fn <- preprocess file
        (srcimps,imps,mod_name) <- getImportsFromFile hspp_fn

        let (path, basename, _ext) = splitFilename3 file

	(mod, location)
	   <- mkHomeModuleLocn mod_name (path ++ '/':basename) file

        src_timestamp
           <- case ml_hs_file location of 
                 Nothing     -> noHsFileErr mod_name
                 Just src_fn -> getModificationTime src_fn

        return (ModSummary mod
                           location{ml_hspp_file=Just hspp_fn}
                           srcimps imps src_timestamp)

-- Summarise a module, and pick up source and timestamp.
summarise :: Module -> ModuleLocation -> Maybe ModSummary
	 -> IO (Maybe ModSummary)
summarise mod location old_summary
   | not (isHomeModule mod) = return Nothing
   | otherwise
   = do let hs_fn = unJust "summarise" (ml_hs_file location)

        case ml_hs_file location of {
           Nothing -> noHsFileErr mod;
           Just src_fn -> do

        src_timestamp <- getModificationTime src_fn

	-- return the cached summary if the source didn't change
	case old_summary of {
	   Just s | ms_hs_date s == src_timestamp -> return (Just s);
	   _ -> do

        hspp_fn <- preprocess hs_fn
        (srcimps,imps,mod_name) <- getImportsFromFile hspp_fn

	when (mod_name /= moduleName mod) $
		throwDyn (ProgramError 
		   (showSDoc (text hs_fn
			      <>  text ": file name does not match module name"
			      <+> quotes (ppr (moduleName mod)))))

        return (Just (ModSummary mod location{ml_hspp_file=Just hspp_fn} 
                                 srcimps imps src_timestamp))
        }
      }


noHsFileErr mod
  = throwDyn (CmdLineError (showSDoc (text "no source file for module" <+> quotes (ppr mod))))

packageModErr mod
  = throwDyn (CmdLineError (showSDoc (text "module" <+>
				   quotes (ppr mod) <+>
				   text "is a package module")))
\end{code}
