%
% (c) The University of Glasgow, 2002
%
% The Compilation Manager
%
\begin{code}
{-# OPTIONS -fvia-C #-}
module CompManager ( 
    ModuleGraph, ModSummary(..),

    CmState, 		-- abstract

    cmInit, 	   -- :: GhciMode -> DynFlags -> IO CmState

    cmDepAnal,	   -- :: CmState -> DynFlags -> [FilePath] -> IO ModuleGraph

    cmLoadModules, -- :: CmState -> DynFlags -> ModuleGraph
		   --	 -> IO (CmState, Bool, [String])

    cmUnload,	   -- :: CmState -> DynFlags -> IO CmState

#ifdef GHCI
    cmModuleIsInterpreted, -- :: CmState -> String -> IO Bool

    cmSetContext,  -- :: CmState -> DynFlags -> [String] -> [String] -> IO CmState
    cmGetContext,  -- :: CmState -> IO ([String],[String])

    cmInfoThing,   -- :: CmState -> DynFlags -> String
		   --	-> IO (CmState, [(TyThing,Fixity)])

    cmBrowseModule, -- :: CmState -> IO [TyThing]

    CmRunResult(..),
    cmRunStmt,	   -- :: CmState -> DynFlags -> String
		   --	 -> IO (CmState, CmRunResult)

    cmTypeOfExpr,  -- :: CmState -> DynFlags -> String
		   -- 	-> IO (CmState, Maybe String)

    cmTypeOfName,  -- :: CmState -> Name -> IO (Maybe String)

    HValue,
    cmCompileExpr, -- :: CmState -> DynFlags -> String 
		   --	-> IO (CmState, Maybe HValue)

    cmGetModInfo,		-- :: CmState -> (ModuleGraph, HomePackageTable)

    cmSetDFlags,
    cmGetBindings, 	-- :: CmState -> [TyThing]
    cmGetPrintUnqual,	-- :: CmState -> PrintUnqualified
#endif
  )
where

#include "HsVersions.h"

import DriverPipeline	( CompResult(..), preprocess, compile, link )
import HscMain		( newHscEnv )
import DriverState	( v_Output_file, v_NoHsMain )
import DriverPhases
import Finder
import HscTypes
import PrelNames        ( gHC_PRIM_Name )
import Module		( Module, ModuleName, moduleName, mkModuleName, isHomeModule,
			  ModuleEnv, lookupModuleEnvByName, mkModuleEnv, moduleEnvElts,
			  extendModuleEnvList, extendModuleEnv,
			  moduleNameUserString,
			  ModLocation(..) )
import RdrName		( GlobalRdrEnv, plusGlobalRdrEnv )
import GetImports
import UniqFM
import Digraph		( SCC(..), stronglyConnComp, flattenSCC, flattenSCCs )
import ErrUtils		( showPass )
import SysTools		( cleanTempFilesExcept )
import BasicTypes	( SuccessFlag(..), succeeded, failed )
import Util
import Outputable
import Panic
import CmdLineOpts	( DynFlags(..), getDynFlags )
import Maybes		( expectJust, orElse, mapCatMaybes )

import DATA_IOREF	( readIORef )

#ifdef GHCI
import HscMain		( hscThing, hscStmt, hscTcExpr )
import TcRnDriver	( mkExportEnv, getModuleContents )
import IfaceSyn		( IfaceDecl )
import Name		( Name )
import NameEnv
import Id		( idType )
import Type		( tidyType )
import VarEnv		( emptyTidyEnv )
import BasicTypes	( Fixity )
import Linker		( HValue, unload, extendLinkEnv )
import GHC.Exts		( unsafeCoerce# )
import Foreign
import Control.Exception as Exception ( Exception, try )
#endif

import EXCEPTION	( throwDyn )

-- std
import Directory        ( getModificationTime, doesFileExist )
import IO
import Monad
import List		( nub )
import Maybe
import Time		( ClockTime )
\end{code}


\begin{code}
-- Persistent state for the entire system
data CmState
   = CmState {
	cm_hsc :: HscEnv,		-- Includes the home-package table
	cm_mg  :: ModuleGraph,		-- The module graph
  	cm_ic  :: InteractiveContext 	-- Command-line binding info
     }

#ifdef GHCI
cmGetModInfo	 cmstate = (cm_mg cmstate, hsc_HPT (cm_hsc cmstate))
cmGetBindings    cmstate = nameEnvElts (ic_type_env (cm_ic cmstate))
cmGetPrintUnqual cmstate = icPrintUnqual (cm_ic cmstate)
cmHPT		 cmstate = hsc_HPT (cm_hsc cmstate)
#endif

cmInit :: GhciMode -> DynFlags -> IO CmState
cmInit ghci_mode dflags
   = do { hsc_env <- newHscEnv ghci_mode dflags
	; return (CmState { cm_hsc = hsc_env,
			    cm_mg  = emptyMG, 
			    cm_ic  = emptyInteractiveContext })}

discardCMInfo :: CmState -> CmState
-- Forget the compilation manager's state, including the home package table
-- but retain the persistent info in HscEnv
discardCMInfo cm_state
  = cm_state { cm_mg = emptyMG, cm_ic = emptyInteractiveContext,
	       cm_hsc = (cm_hsc cm_state) { hsc_HPT = emptyHomePackageTable } }

-------------------------------------------------------------------
--			The unlinked image
-- 
-- The compilation manager keeps a list of compiled, but as-yet unlinked
-- binaries (byte code or object code).  Even when it links bytecode
-- it keeps the unlinked version so it can re-link it later without
-- recompiling.

type UnlinkedImage = [Linkable]	-- the unlinked images (should be a set, really)

findModuleLinkable_maybe :: [Linkable] -> ModuleName -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        many -> pprPanic "findModuleLinkable" (ppr mod)
\end{code}


%************************************************************************
%*									*
	GHCI stuff
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
-----------------------------------------------------------------------------
-- Setting the context doesn't throw away any bindings; the bindings
-- we've built up in the InteractiveContext simply move to the new
-- module.  They always shadow anything in scope in the current context.

cmSetContext
	:: CmState
	-> [String]		-- take the top-level scopes of these modules
	-> [String]		-- and the just the exports from these
	-> IO CmState
cmSetContext cmstate toplevs exports = do 
  let old_ic  = cm_ic cmstate
      hsc_env = cm_hsc cmstate
      hpt     = hsc_HPT hsc_env

  export_env  <- mkExportEnv hsc_env (map mkModuleName exports)
  toplev_envs <- mapM (mkTopLevEnv hpt) toplevs

  let all_env = foldr plusGlobalRdrEnv export_env toplev_envs
  return cmstate{ cm_ic = old_ic { ic_toplev_scope = toplevs,
		     		   ic_exports      = exports,
			       	   ic_rn_gbl_env   = all_env } }

mkTopLevEnv :: HomePackageTable -> String -> IO GlobalRdrEnv
mkTopLevEnv hpt mod
 = case lookupModuleEnvByName hpt (mkModuleName mod) of
      Nothing      -> throwDyn (ProgramError ("mkTopLevEnv: not a home module " ++ mod))
      Just details -> case hm_globals details of
			Nothing  -> throwDyn (ProgramError ("mkTopLevEnv: not interpreted " ++ mod))
			Just env -> return env

cmGetContext :: CmState -> IO ([String],[String])
cmGetContext CmState{cm_ic=ic} = 
  return (ic_toplev_scope ic, ic_exports ic)

cmModuleIsInterpreted :: CmState -> String -> IO Bool
cmModuleIsInterpreted cmstate str 
 = case lookupModuleEnvByName (cmHPT cmstate) (mkModuleName str) of
      Just details       -> return (isJust (hm_globals details))
      _not_a_home_module -> return False

-----------------------------------------------------------------------------
cmSetDFlags :: CmState -> DynFlags -> CmState
cmSetDFlags cm_state dflags 
  = cm_state { cm_hsc = (cm_hsc cm_state) { hsc_dflags = dflags } }

-----------------------------------------------------------------------------
-- cmInfoThing: convert a String to a TyThing

-- A string may refer to more than one TyThing (eg. a constructor,
-- and type constructor), so we return a list of all the possible TyThings.

cmInfoThing :: CmState -> String -> IO [(IfaceDecl,Fixity)]
cmInfoThing cmstate id
   = hscThing (cm_hsc cmstate) (cm_ic cmstate) id

-- ---------------------------------------------------------------------------
-- cmBrowseModule: get all the TyThings defined in a module

cmBrowseModule :: CmState -> String -> Bool -> IO [IfaceDecl]
cmBrowseModule cmstate str exports_only
  = do { mb_decls <- getModuleContents (cm_hsc cmstate) (cm_ic cmstate) 
		     		       (mkModuleName str) exports_only
       ; case mb_decls of
	   Nothing -> return []		-- An error of some kind
	   Just ds -> return ds
   }


-----------------------------------------------------------------------------
-- cmRunStmt:  Run a statement/expr.

data CmRunResult
  = CmRunOk [Name] 		-- names bound by this evaluation
  | CmRunFailed 
  | CmRunException Exception	-- statement raised an exception

cmRunStmt :: CmState -> String -> IO (CmState, CmRunResult)		
cmRunStmt cmstate@CmState{ cm_hsc=hsc_env, cm_ic=icontext } expr
   = do 
        maybe_stuff <- hscStmt hsc_env icontext expr

        case maybe_stuff of
	   Nothing -> return (cmstate, CmRunFailed)
	   Just (new_ic, names, hval) -> do

		let thing_to_run = unsafeCoerce# hval :: IO [HValue]
		either_hvals <- sandboxIO thing_to_run

		case either_hvals of
		    Left e -> do
			-- on error, keep the *old* interactive context,
			-- so that 'it' is not bound to something
			-- that doesn't exist.
		        return ( cmstate, CmRunException e )

		    Right hvals -> do
			-- Get the newly bound things, and bind them.  
			-- Don't need to delete any shadowed bindings;
			-- the new ones override the old ones. 
			extendLinkEnv (zip names hvals)
	     		
			return (cmstate{ cm_ic=new_ic }, 
				CmRunOk names)


-- We run the statement in a "sandbox" to protect the rest of the
-- system from anything the expression might do.  For now, this
-- consists of just wrapping it in an exception handler, but see below
-- for another version.

sandboxIO :: IO a -> IO (Either Exception a)
sandboxIO thing = Exception.try thing

{-
-- This version of sandboxIO runs the expression in a completely new
-- RTS main thread.  It is disabled for now because ^C exceptions
-- won't be delivered to the new thread, instead they'll be delivered
-- to the (blocked) GHCi main thread.

-- SLPJ: when re-enabling this, reflect a wrong-stat error as an exception

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

foreign import "rts_evalStableIO"  {- safe -}
  rts_evalStableIO :: StablePtr (IO a) -> Ptr (StablePtr a) -> IO CInt
  -- more informative than the C type!
-}

-----------------------------------------------------------------------------
-- cmTypeOfExpr: returns a string representing the type of an expression

cmTypeOfExpr :: CmState -> String -> IO (Maybe String)
cmTypeOfExpr cmstate expr
   = do maybe_stuff <- hscTcExpr (cm_hsc cmstate) (cm_ic cmstate) expr

	case maybe_stuff of
	   Nothing -> return Nothing
	   Just ty -> return (Just str)
 	     where 
		str     = showSDocForUser unqual (text expr <+> dcolon <+> ppr tidy_ty)
		unqual  = icPrintUnqual (cm_ic cmstate)
		tidy_ty = tidyType emptyTidyEnv ty


-----------------------------------------------------------------------------
-- cmTypeOfName: returns a string representing the type of a name.

cmTypeOfName :: CmState -> Name -> IO (Maybe String)
cmTypeOfName CmState{ cm_ic=ic } name
 = do 
    hPutStrLn stderr ("cmTypeOfName: " ++ showSDoc (ppr name))
    case lookupNameEnv (ic_type_env ic) name of
	Nothing        -> return Nothing
	Just (AnId id) -> return (Just str)
	   where
	     unqual = icPrintUnqual ic
	     ty = tidyType emptyTidyEnv (idType id)
	     str = showSDocForUser unqual (ppr ty)

	_ -> panic "cmTypeOfName"

-----------------------------------------------------------------------------
-- cmCompileExpr: compile an expression and deliver an HValue

cmCompileExpr :: CmState -> String -> IO (Maybe HValue)
cmCompileExpr cmstate expr
   = do 
        maybe_stuff 
	    <- hscStmt (cm_hsc cmstate) (cm_ic cmstate)
		       ("let __cmCompileExpr = "++expr)

        case maybe_stuff of
	   Nothing -> return Nothing
	   Just (new_ic, names, hval) -> do

			-- Run it!
		hvals <- (unsafeCoerce# hval) :: IO [HValue]

		case (names,hvals) of
		  ([n],[hv]) -> return (Just hv)
		  _ 	     -> panic "cmCompileExpr"

#endif /* GHCI */
\end{code}


%************************************************************************
%*									*
	Loading and unloading
%*									*
%************************************************************************

\begin{code}
-----------------------------------------------------------------------------
-- Unload the compilation manager's state: everything it knows about the
-- current collection of modules in the Home package.

cmUnload :: CmState -> IO CmState
cmUnload state@CmState{ cm_hsc = hsc_env }
 = do -- Throw away the old home dir cache
      flushFinderCache

      -- Unload everything the linker knows about
      cm_unload hsc_env []

      -- Start with a fresh CmState, but keep the PersistentCompilerState
      return (discardCMInfo state)

cm_unload hsc_env linkables
  = case hsc_mode hsc_env of
	Batch -> return ()
#ifdef GHCI
	Interactive -> Linker.unload (hsc_dflags hsc_env) linkables
#else
	Interactive -> panic "unload: no interpreter"
#endif
    

-----------------------------------------------------------------------------
-- Trace dependency graph

-- This is a seperate pass so that the caller can back off and keep
-- the current state if the downsweep fails.  Typically the caller
-- might go	cmDepAnal
--		cmUnload
--		cmLoadModules
-- He wants to do the dependency analysis before the unload, so that
-- if the former fails he can use the later

cmDepAnal :: CmState -> [FilePath] -> IO ModuleGraph
cmDepAnal cmstate rootnames
  = do showPass dflags "Chasing dependencies"
       when (verbosity dflags >= 1 && gmode == Batch) $
           hPutStrLn stderr (showSDoc (hcat [
	     text "Chasing modules from: ",
	     hcat (punctuate comma (map text rootnames))]))
       downsweep rootnames (cm_mg cmstate)
  where
    hsc_env = cm_hsc cmstate
    dflags  = hsc_dflags hsc_env
    gmode   = hsc_mode hsc_env

-----------------------------------------------------------------------------
-- The real business of the compilation manager: given a system state and
-- a module name, try and bring the module up to date, probably changing
-- the system state at the same time.

cmLoadModules :: CmState 		-- The HPT may not be as up to date
              -> ModuleGraph		-- Bang up to date
              -> IO (CmState,		-- new state
		     SuccessFlag,	-- was successful
		     [String])		-- list of modules loaded

cmLoadModules cmstate1 mg2unsorted
   = do -- version 1's are the original, before downsweep
	let hsc_env   = cm_hsc cmstate1
        let hpt1      = hsc_HPT hsc_env
        let ghci_mode = hsc_mode   hsc_env -- this never changes
        let dflags    = hsc_dflags hsc_env -- this never changes

        -- Do the downsweep to reestablish the module graph
        let verb = verbosity dflags

	-- Find out if we have a Main module
        let a_root_is_Main 
               = any ((=="Main").moduleNameUserString.modSummaryName) 
                     mg2unsorted

        let mg2unsorted_names = map modSummaryName mg2unsorted

        -- reachable_from follows source as well as normal imports
        let reachable_from :: ModuleName -> [ModuleName]
            reachable_from = downwards_closure_of_module mg2unsorted
 
        -- should be cycle free; ignores 'import source's
        let mg2 = topological_sort False mg2unsorted
        -- ... whereas this takes them into account.  Used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hpt all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps = topological_sort True mg2unsorted

	-- Sort out which linkables we wish to keep in the unlinked image.
	-- See getValidLinkables below for details.
	(valid_old_linkables, new_linkables)
	    <- getValidLinkables ghci_mode (hptLinkables hpt1)
		  mg2unsorted_names mg2_with_srcimps

	-- putStrLn (showSDoc (vcat [ppr valid_old_linkables, ppr new_linkables]))

		-- Uniq of ModuleName is the same as Module, fortunately...
	let hpt2 = delListFromUFM hpt1 (map linkableModName new_linkables)
            hsc_env2 = hsc_env { hsc_HPT = hpt2 }

	-- When (verb >= 2) $
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
        -- 2.  A valid old linkable exists for each module in ms

        stable_mods <- preUpsweep valid_old_linkables
		 		  mg2unsorted_names [] mg2_with_srcimps

        let stable_summaries
               = concatMap (findInSummaries mg2unsorted) stable_mods

	    stable_linkables
	       = filter (\m -> linkableModName m `elem` stable_mods) 
		    valid_old_linkables

        when (verb >= 2) $
           hPutStrLn stderr (showSDoc (text "Stable modules:" 
                               <+> sep (map (text.moduleNameUserString) stable_mods)))

	-- Unload any modules which are going to be re-linked this
	-- time around.
	cm_unload hsc_env2 stable_linkables

	-- we can now glom together our linkable sets
	let valid_linkables = valid_old_linkables ++ new_linkables

        -- We could at this point detect cycles which aren't broken by
        -- a source-import, and complain immediately, but it seems better
        -- to let upsweep_mods do this, so at least some useful work gets
        -- done before the upsweep is abandoned.
        let upsweep_these
               = filter (\scc -> any (`notElem` stable_mods) 
                                     (map modSummaryName (flattenSCC scc)))
                        mg2

        --hPutStrLn stderr "after tsort:\n"
        --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Because we don't take into account source imports when doing
        -- the topological sort, there shouldn't be any cycles in mg2.
        -- If there is, we complain and give up -- the user needs to
        -- break the cycle using a boot file.

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

	-- clean up between compilations
	let cleanup = cleanTempFilesExcept verb 
			  (ppFilesFromSummaries (flattenSCCs mg2))

        (upsweep_ok, hsc_env3, modsUpswept)
           <- upsweep_mods hsc_env2 valid_linkables reachable_from 
                           cleanup upsweep_these

        -- At this point, modsUpswept and newLis should have the same
        -- length, so there is one new (or old) linkable for each 
        -- mod which was processed (passed to compile).

	-- Make modsDone be the summaries for each home module now
	-- available; this should equal the domain of hpt3.
	-- (NOT STRICTLY TRUE if an interactive session was started
	--  with some object on disk ???)
        -- Get in in a roughly top .. bottom order (hence reverse).

        let modsDone = reverse modsUpswept ++ stable_summaries

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if succeeded upsweep_ok

         then 
           -- Easy; just relink it all.
           do when (verb >= 2) $ 
		 hPutStrLn stderr "Upsweep completely successful."

	      -- clean up after ourselves
	      cleanTempFilesExcept verb (ppFilesFromSummaries modsDone)

	      ofile <- readIORef v_Output_file
	      no_hs_main <- readIORef v_NoHsMain

	      -- Issue a warning for the confusing case where the user
	      -- said '-o foo' but we're not going to do any linking.
	      -- We attempt linking if either (a) one of the modules is
	      -- called Main, or (b) the user said -no-hs-main, indicating
	      -- that main() is going to come from somewhere else.
	      --
	      let do_linking = a_root_is_Main || no_hs_main
	      when (ghci_mode == Batch && isJust ofile && not do_linking
		     && verb > 0) $
	         hPutStrLn stderr "Warning: output was redirected with -o, but no output will be generated\nbecause there is no Main module."

	      -- link everything together
              linkresult <- link ghci_mode dflags do_linking (hsc_HPT hsc_env3)

	      let cmstate3 = cmstate1 { cm_mg = modsDone, cm_hsc = hsc_env3 }
	      cmLoadFinish Succeeded linkresult cmstate3

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do when (verb >= 2) $
		hPutStrLn stderr "Upsweep partially successful."

              let modsDone_names
                     = map modSummaryName modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).modSummaryName) 
			  modsDone

              let hpt4 = retainInTopLevelEnvs (map modSummaryName mods_to_keep) 
					      (hsc_HPT hsc_env3)

	      -- Clean up after ourselves
	      cleanTempFilesExcept verb (ppFilesFromSummaries mods_to_keep)

	      -- Link everything together
              linkresult <- link ghci_mode dflags False hpt4

	      let cmstate3 = cmstate1 { cm_mg = mods_to_keep,
					cm_hsc = hsc_env3 { hsc_HPT = hpt4 } }
	      cmLoadFinish Failed linkresult cmstate3


-- Finish up after a cmLoad.

-- If the link failed, unload everything and return.
cmLoadFinish ok Failed cmstate
  = do cm_unload (cm_hsc cmstate) []
       return (discardCMInfo cmstate, Failed, [])

-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
cmLoadFinish ok Succeeded cmstate
  = do let new_cmstate = cmstate { cm_ic = emptyInteractiveContext }
           mods_loaded = map (moduleNameUserString.modSummaryName) 
			     (cm_mg cmstate)

       return (new_cmstate, ok, mods_loaded)

-- used to fish out the preprocess output files for the purposes of
-- cleaning up.  The preprocessed file *might* be the same as the
-- source file, but that doesn't do any harm.
ppFilesFromSummaries summaries
  = [ fn | Just fn <- map (ml_hspp_file.ms_location) summaries ]

-----------------------------------------------------------------------------
-- getValidLinkables

-- For each module (or SCC of modules), we take:
--
--	- an on-disk linkable, if this is the first time around and one
--	  is available.
--
--	- the old linkable, otherwise (and if one is available).
--
-- and we throw away the linkable if it is older than the source file.
-- In interactive mode, we also ignore the on-disk linkables unless
-- all of the dependents of this SCC also have on-disk linkables (we
-- can't have dynamically loaded objects that depend on interpreted
-- modules in GHCi).
--
-- If a module has a valid linkable, then it may be STABLE (see below),
-- and it is classified as SOURCE UNCHANGED for the purposes of calling
-- compile.
--
-- ToDo: this pass could be merged with the preUpsweep.

getValidLinkables
	:: GhciMode
	-> [Linkable]		-- old linkables
	-> [ModuleName]		-- all home modules
	-> [SCC ModSummary]	-- all modules in the program, dependency order
	-> IO ( [Linkable],	-- still-valid linkables 
		[Linkable] 	-- new linkables we just found
	      )

getValidLinkables mode old_linkables all_home_mods module_graph = do
  ls <- foldM (getValidLinkablesSCC mode old_linkables all_home_mods) 
		[] module_graph
  return (partition_it ls [] [])
 where
  partition_it []         valid new = (valid,new)
  partition_it ((l,b):ls) valid new 
	| b         = partition_it ls valid (l:new)
	| otherwise = partition_it ls (l:valid) new


getValidLinkablesSCC mode old_linkables all_home_mods new_linkables scc0
   = let 
	  scc             = flattenSCC scc0
          scc_names       = map modSummaryName scc
	  home_module m   = m `elem` all_home_mods && m `notElem` scc_names
          scc_allhomeimps = nub (filter home_module (concatMap ms_imps scc))
		-- NB. ms_imps, not ms_allimps above.  We don't want to
		-- force a module's SOURCE imports to be already compiled for
		-- its object linkable to be valid.

	  has_object m = 
		case findModuleLinkable_maybe (map fst new_linkables) m of
		    Nothing -> False
		    Just l  -> isObjectLinkable l

          objects_allowed = mode == Batch || all has_object scc_allhomeimps
     in do

     new_linkables'
	<- foldM (getValidLinkable old_linkables objects_allowed) [] scc

	-- since an scc can contain only all objects or no objects at all,
	-- we have to check whether we got all objects or not, and re-do
	-- the linkable check if not.
     new_linkables' <- 
        if objects_allowed
	     && not (all isObjectLinkable (map fst new_linkables'))
	  then foldM (getValidLinkable old_linkables False) [] scc
	  else return new_linkables'

     return (new_linkables ++ new_linkables')


getValidLinkable :: [Linkable] -> Bool -> [(Linkable,Bool)] -> ModSummary 
	-> IO [(Linkable,Bool)]
	-- True <=> linkable is new; i.e. freshly discovered on the disk
	--				  presumably generated 'on the side'
	--				  by a separate GHC run
getValidLinkable old_linkables objects_allowed new_linkables summary 
	-- 'objects_allowed' says whether we permit this module to
	-- have a .o-file linkable.  We only permit it if all the
	-- modules it depends on also have .o files; a .o file can't
	-- link to a bytecode module
   = do let mod_name = modSummaryName summary

	maybe_disk_linkable
          <- if (not objects_allowed)
		then return Nothing

		else findLinkable mod_name (ms_location summary)

	let old_linkable = findModuleLinkable_maybe old_linkables mod_name

	    new_linkables' = 
	     case (old_linkable, maybe_disk_linkable) of
		(Nothing, Nothing)			-> []

		-- new object linkable just appeared
		(Nothing, Just l)			-> up_to_date l True

		(Just l,  Nothing)
		  | isObjectLinkable l			-> []
		    -- object linkable disappeared!  In case we need to
		    -- relink the module, disregard the old linkable and
		    -- just interpret the module from now on.
		  | otherwise				-> up_to_date l False
		    -- old byte code linkable

		(Just l, Just l') 
		  | not (isObjectLinkable l)		-> up_to_date l  False
		    -- if the previous linkable was interpreted, then we
		    -- ignore a newly compiled version, because the version
		    -- numbers in the interface file will be out-of-sync with
		    -- our internal ones.
		  | linkableTime l' >  linkableTime l   -> up_to_date l' True
		  | linkableTime l' == linkableTime l   -> up_to_date l  False
		  | otherwise			        -> []
		    -- on-disk linkable has been replaced by an older one!
		    -- again, disregard the previous one.

	    up_to_date l b
		| linkableTime l < ms_hs_date summary = []
		| otherwise = [(l,b)]
		-- why '<' rather than '<=' above?  If the filesystem stores
		-- times to the nearset second, we may occasionally find that
		-- the object & source have the same modification time, 
		-- especially if the source was automatically generated
		-- and compiled.  Using >= is slightly unsafe, but it matches
		-- make's behaviour.

	return (new_linkables' ++ new_linkables)


hptLinkables :: HomePackageTable -> [Linkable]
-- Get all the linkables from the home package table, one for each module
-- Once the HPT is up to date, these are the ones we should link
hptLinkables hpt = map hm_linkable (moduleEnvElts hpt)


-----------------------------------------------------------------------------
-- Do a pre-upsweep without use of "compile", to establish a 
-- (downward-closed) set of stable modules for which we won't call compile.

-- a stable module:
--	* has a valid linkable (see getValidLinkables above)
--	* depends only on stable modules
--	* has an interface in the HPT (interactive mode only)

preUpsweep :: [Linkable]	-- new valid linkables
           -> [ModuleName]	-- names of all mods encountered in downsweep
           -> [ModuleName]	-- accumulating stable modules
           -> [SCC ModSummary]  -- scc-ified mod graph, including src imps
           -> IO [ModuleName]	-- stable modules

preUpsweep valid_lis all_home_mods stable []  = return stable
preUpsweep valid_lis all_home_mods stable (scc0:sccs)
   = do let scc = flattenSCC scc0
            scc_allhomeimps :: [ModuleName]
            scc_allhomeimps 
               = nub (filter (`elem` all_home_mods) (concatMap ms_allimps scc))
            all_imports_in_scc_or_stable
               = all in_stable_or_scc scc_allhomeimps
            scc_names
		= map modSummaryName scc
            in_stable_or_scc m
               = m `elem` scc_names || m `elem` stable

	    -- now we check for valid linkables: each module in the SCC must 
	    -- have a valid linkable (see getValidLinkables above).
	    has_valid_linkable new_summary
   	      = isJust (findModuleLinkable_maybe valid_lis modname)
	       where modname = modSummaryName new_summary

	    scc_is_stable = all_imports_in_scc_or_stable
			  && all has_valid_linkable scc

        if scc_is_stable
         then preUpsweep valid_lis all_home_mods (scc_names++stable) sccs
         else preUpsweep valid_lis all_home_mods stable sccs


-- Helper for preUpsweep.  Assuming that new_summary's imports are all
-- stable (in the sense of preUpsweep), determine if new_summary is itself
-- stable, and, if so, in batch mode, return its linkable.
findInSummaries :: [ModSummary] -> ModuleName -> [ModSummary]
findInSummaries old_summaries mod_name
   = [s | s <- old_summaries, modSummaryName s == mod_name]

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
           = let names_in_this_cycle = nub (map modSummaryName vs)
                 mods_in_this_cycle  
                    = nub ([done | done <- modsDone, 
                                   done `elem` names_in_this_cycle])
                 chewed_rest = chew rest
             in 
             if   notNull mods_in_this_cycle
                  && length mods_in_this_cycle < length names_in_this_cycle
             then mods_in_this_cycle ++ chewed_rest
             else chewed_rest


-- Compile multiple modules, stopping as soon as an error appears.
-- There better had not be any cyclic groups here -- we check for them.
upsweep_mods :: HscEnv			-- Includes up-to-date HPT
             -> [Linkable]		-- Valid linkables
             -> (ModuleName -> [ModuleName])  -- to construct downward closures
	     -> IO ()		      -- how to clean up unwanted tmp files
             -> [SCC ModSummary]      -- mods to do (the worklist)
                                      -- ...... RETURNING ......
             -> IO (SuccessFlag,
                    HscEnv,		-- With an updated HPT
                    [ModSummary])	-- Mods which succeeded

upsweep_mods hsc_env oldUI reachable_from cleanup
     []
   = return (Succeeded, hsc_env, [])

upsweep_mods hsc_env oldUI reachable_from cleanup
     ((CyclicSCC ms):_)
   = do hPutStrLn stderr ("Module imports form a cycle for modules:\n\t" ++
                          unwords (map (moduleNameUserString.modSummaryName) ms))
        return (Failed, hsc_env, [])

upsweep_mods hsc_env oldUI reachable_from cleanup
     ((AcyclicSCC mod):mods)
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++ 
	--	     show (map (moduleNameUserString.moduleName.mi_module.hm_iface) (eltsUFM (hsc_HPT hsc_env)))

        (ok_flag, hsc_env1) <- upsweep_mod hsc_env oldUI mod 
                  	    		    (reachable_from (modSummaryName mod))

	cleanup		-- Remove unwanted tmp files between compilations

        if failed ok_flag then
	     return (Failed, hsc_env1, [])
	  else do 
	     (restOK, hsc_env2, modOKs) 
                       <- upsweep_mods hsc_env1 oldUI reachable_from cleanup mods
             return (restOK, hsc_env2, mod:modOKs)


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> UnlinkedImage
            -> ModSummary
            -> [ModuleName]
            -> IO (SuccessFlag, 
		   HscEnv)		-- With updated HPT

upsweep_mod hsc_env oldUI summary1 reachable_inc_me
   = do 
        let this_mod = ms_mod summary1
	    location = ms_location summary1
	    mod_name = moduleName this_mod
	    hpt1     = hsc_HPT hsc_env

        let mb_old_iface = case lookupModuleEnvByName hpt1 mod_name of
			     Just mod_info -> Just (hm_iface mod_info)
			     Nothing	   -> Nothing

        let maybe_old_linkable = findModuleLinkable_maybe oldUI mod_name
            source_unchanged   = isJust maybe_old_linkable

	    reachable_only = filter (/= mod_name) reachable_inc_me

	   -- In interactive mode, all home modules below us *must* have an
	   -- interface in the HPT.  We never demand-load home interfaces in
	   -- interactive mode.
            hpt1_strictDC
               = ASSERT(hsc_mode hsc_env == Batch || all (`elemUFM` hpt1) reachable_only)
		 retainInTopLevelEnvs reachable_only hpt1
	    hsc_env_strictDC = hsc_env { hsc_HPT = hpt1_strictDC }

            old_linkable = expectJust "upsweep_mod:old_linkable" maybe_old_linkable

	    have_object 
	       | Just l <- maybe_old_linkable, isObjectLinkable l = True
	       | otherwise = False

        compresult <- compile hsc_env_strictDC this_mod location 
			      source_unchanged have_object mb_old_iface

        case compresult of

           -- Compilation "succeeded", and may or may not have returned a new
           -- linkable (depending on whether compilation was actually performed
	   -- or not).
           CompOK new_details new_globals new_iface maybe_new_linkable
              -> do let 
			new_linkable = maybe_new_linkable `orElse` old_linkable
			new_info = HomeModInfo { hm_iface = new_iface,
						 hm_globals = new_globals,
						 hm_details = new_details,
						 hm_linkable = new_linkable }
			hpt2      = extendModuleEnv hpt1 this_mod new_info

                    return (Succeeded, hsc_env { hsc_HPT = hpt2 })

           -- Compilation failed.  Compile may still have updated the PCS, tho.
           CompErrs -> return (Failed, hsc_env)

-- Filter modules in the HPT
retainInTopLevelEnvs :: [ModuleName] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = listToUFM (concatMap (maybeLookupUFM hpt) keep_these)
   where
     maybeLookupUFM ufm u  = case lookupUFM ufm u of 
				Nothing  -> []
				Just val -> [(u, val)] 

-- Needed to clean up HPT so that we don't get duplicates in inst env
downwards_closure_of_module :: [ModSummary] -> ModuleName -> [ModuleName]
downwards_closure_of_module summaries root
   = let toEdge :: ModSummary -> (ModuleName,[ModuleName])
         toEdge summ = (modSummaryName summ, 
			filter (`elem` all_mods) (ms_allimps summ))

	 all_mods = map modSummaryName summaries

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
             = (summ, modSummaryName summ, 
                      (if include_source_imports 
                       then ms_srcimps summ else []) ++ ms_imps summ)
        
         mash_edge :: (ModSummary,ModuleName,[ModuleName]) -> (ModSummary,Int,[Int])
         mash_edge (summ, m, m_imports)
            = case lookup m key_map of
                 Nothing -> panic "reverse_topological_sort"
                 Just mk -> (summ, mk, 
                                -- ignore imports not from the home package
                             mapCatMaybes (flip lookup key_map) m_imports)

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
	checkDuplicates rootSummaries
        all_summaries
           <- loop (concat (map (\ m -> zip (repeat (fromMaybe "<unknown>" (ml_hs_file (ms_location m))))
	   				    (ms_imps m)) rootSummaries))
		(mkModuleEnv [ (mod, s) | s <- rootSummaries, 
					  let mod = ms_mod s, isHomeModule mod 
			     ])
        return all_summaries
     where
	getRootSummary :: FilePath -> IO ModSummary
	getRootSummary file
	   | isHaskellSrcFilename file
	   = do exists <- doesFileExist file
		if exists then summariseFile file else do
		throwDyn (CmdLineError ("can't find file `" ++ file ++ "'"))	
	   | otherwise
 	   = do exists <- doesFileExist hs_file
		if exists then summariseFile hs_file else do
		exists <- doesFileExist lhs_file
		if exists then summariseFile lhs_file else do
		let mod_name = mkModuleName file
		maybe_summary <- getSummary (file, mod_name)
		case maybe_summary of
		   Nothing -> packageModErr mod_name
		   Just s  -> return s
           where 
		 hs_file = file ++ ".hs"
		 lhs_file = file ++ ".lhs"

	-- In a root module, the filename is allowed to diverge from the module
	-- name, so we have to check that there aren't multiple root files
	-- defining the same module (otherwise the duplicates will be silently
 	-- ignored, leading to confusing behaviour).
	checkDuplicates :: [ModSummary] -> IO ()
	checkDuplicates summaries = mapM_ check summaries
  	  where check summ = 
		  case dups of
			[]     -> return ()
			[_one] -> return ()
			many   -> multiRootsErr modl many
		   where modl = ms_mod summ
			 dups = 
			   [ fromJust (ml_hs_file (ms_location summ'))
			   | summ' <- summaries, ms_mod summ' == modl ]

        getSummary :: (FilePath,ModuleName) -> IO (Maybe ModSummary)
        getSummary (currentMod,nm)
           = do found <- findModule nm
		case found of
		   Right (mod, location) -> do
			let old_summary = findModInSummaries old_summaries mod
			summarise mod location old_summary

		   Left files -> do
			dflags <- getDynFlags
			throwDyn (noModError dflags currentMod nm files)

        -- loop invariant: env doesn't contain package modules
        loop :: [(FilePath,ModuleName)] -> ModuleEnv ModSummary -> IO [ModSummary]
	loop [] env = return (moduleEnvElts env)
        loop imps env
           = do -- imports for modules we don't already have
                let needed_imps = nub (filter (not . (`elemUFM` env).snd) imps)

		-- summarise them
                needed_summaries <- mapM getSummary needed_imps

		-- get just the "home" modules
                let new_home_summaries = [ s | Just s <- needed_summaries ]

		-- loop, checking the new imports
		let new_imps = concat (map (\ m -> zip (repeat (fromMaybe "<unknown>" (ml_hs_file (ms_location m))))
						       (ms_imps m)) new_home_summaries)
                loop new_imps (extendModuleEnvList env 
				[ (ms_mod s, s) | s <- new_home_summaries ])

-- ToDo: we don't have a proper line number for this error
noModError dflags loc mod_nm files = ProgramError (showSDoc (
  hang (text loc <> colon) 4 $
    (text "Can't find module" <+> quotes (ppr mod_nm) $$ extra)
  ))
  where
   extra
    | verbosity dflags < 3 =
        text "(use -v to see a list of the files searched for)"
    | otherwise =
        hang (ptext SLIT("locations searched:")) 4 (vcat (map text files))

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

        let -- GHC.Prim doesn't exist physically, so don't go looking for it.
            the_imps = filter (/= gHC_PRIM_Name) imps

	(mod, location) <- mkHomeModLocation mod_name file

        src_timestamp
           <- case ml_hs_file location of 
                 Nothing     -> noHsFileErr mod_name
                 Just src_fn -> getModificationTime src_fn

        return (ModSummary { ms_mod = mod, 
                             ms_location = location{ml_hspp_file=Just hspp_fn},
                             ms_srcimps = srcimps, ms_imps = the_imps,
			     ms_hs_date = src_timestamp })

-- Summarise a module, and pick up source and timestamp.
summarise :: Module -> ModLocation -> Maybe ModSummary
	 -> IO (Maybe ModSummary)
summarise mod location old_summary
   | not (isHomeModule mod) = return Nothing
   | otherwise
   = do let hs_fn = expectJust "summarise" (ml_hs_file location)

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
	let
	     -- GHC.Prim doesn't exist physically, so don't go looking for it.
           the_imps = filter (/= gHC_PRIM_Name) imps

	when (mod_name /= moduleName mod) $
		throwDyn (ProgramError 
		   (showSDoc (text hs_fn
			      <>  text ": file name does not match module name"
			      <+> quotes (ppr (moduleName mod)))))

        return (Just (ModSummary mod location{ml_hspp_file=Just hspp_fn} 
                                 srcimps the_imps src_timestamp))
        }
      }


noHsFileErr mod
  = throwDyn (CmdLineError (showSDoc (text "no source file for module" <+> quotes (ppr mod))))

packageModErr mod
  = throwDyn (CmdLineError (showSDoc (text "module" <+>
				   quotes (ppr mod) <+>
				   text "is a package module")))

multiRootsErr mod files
  = throwDyn (ProgramError (showSDoc (
	text "module" <+> quotes (ppr mod) <+> 
	text "is defined in multiple files:" <+>
	sep (map text files))))
\end{code}


%************************************************************************
%*									*
		The ModSummary Type
%*									*
%************************************************************************

\begin{code}
-- The ModLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just 
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.


type ModuleGraph = [ModSummary]  -- the module graph, topologically sorted

emptyMG :: ModuleGraph
emptyMG = []

data ModSummary
   = ModSummary {
        ms_mod      :: Module,			-- name, package
        ms_location :: ModLocation,		-- location
        ms_srcimps  :: [ModuleName],		-- source imports
        ms_imps     :: [ModuleName],		-- non-source imports
        ms_hs_date  :: ClockTime		-- timestamp of summarised file
     }

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms) <> comma,
                          text "ms_imps =" <+> ppr (ms_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

ms_allimps ms = ms_srcimps ms ++ ms_imps ms

modSummaryName :: ModSummary -> ModuleName
modSummaryName = moduleName . ms_mod
\end{code}
