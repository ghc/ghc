%
% (c) The University of Glasgow, 2002
%
% The Compilation Manager
%
\begin{code}
module CompManager ( 
    ModSummary,		-- Abstract
    ModuleGraph, 	-- All the modules from the home package

    CmState, 		-- Abstract

    cmInit, 	   -- :: GhciMode -> IO CmState

    cmDepAnal,	   -- :: CmState -> [FilePath] -> IO ModuleGraph
    cmDownsweep,   
    cmTopSort,	   -- :: Bool -> ModuleGraph -> [SCC ModSummary]
    cyclicModuleErr,	-- :: [ModSummary] -> String	-- Used by DriverMkDepend

    cmLoadModules, -- :: CmState -> ModuleGraph
		   --	 -> IO (CmState, Bool, [String])

    cmUnload,	   -- :: CmState -> IO CmState


#ifdef GHCI
    cmModuleIsInterpreted, -- :: CmState -> String -> IO Bool

    cmSetContext,  -- :: CmState -> [String] -> [String] -> IO CmState
    cmGetContext,  -- :: CmState -> IO ([String],[String])

    cmGetInfo,    -- :: CmState -> String -> IO (CmState, [(TyThing,Fixity)])
    GetInfoResult,
    cmBrowseModule, -- :: CmState -> IO [TyThing]
    cmShowModule,

    CmRunResult(..),
    cmRunStmt,		-- :: CmState -> String -> IO (CmState, CmRunResult)

    cmTypeOfExpr,	-- :: CmState -> String -> IO (CmState, Maybe String)
    cmKindOfType,	-- :: CmState -> String -> IO (CmState, Maybe String)
    cmTypeOfName,	-- :: CmState -> Name -> IO (Maybe String)

    HValue,
    cmCompileExpr,	-- :: CmState -> String -> IO (CmState, Maybe HValue)
    cmGetModuleGraph,	-- :: CmState -> ModuleGraph
    cmSetDFlags,
    cmGetDFlags,

    cmGetBindings, 	-- :: CmState -> [TyThing]
    cmGetPrintUnqual,	-- :: CmState -> PrintUnqualified
#endif
  )
where

#include "HsVersions.h"

import Packages		( isHomePackage )
import DriverPipeline	( CompResult(..), preprocess, compile, link )
import HscMain		( newHscEnv )
import DriverState	( v_Output_file, v_NoHsMain, v_MainModIs )
import DriverPhases	( HscSource(..), isHsBoot, hscSourceString, isHaskellSrcFilename )
import Finder		( findModule, findLinkable, addHomeModuleToFinder,
			  flushFinderCache, findPackageModule,
			  mkHomeModLocation, FindResult(..), cantFindError )
import HscTypes		( ModSummary(..), HomeModInfo(..), ModIface(..), msHsFilePath,
			  HscEnv(..), GhciMode(..), 
			  InteractiveContext(..), emptyInteractiveContext, 
			  HomePackageTable, emptyHomePackageTable, IsBootInterface,
			  Linkable(..), isObjectLinkable )
import Module		( Module, mkModule, delModuleEnv, delModuleEnvList, mkModuleEnv,
			  lookupModuleEnv, moduleEnvElts, extendModuleEnv, filterModuleEnv,
			  moduleUserString, addBootSuffixLocn, 
			  ModLocation(..) )
import GetImports	( getImports )
import Digraph		( SCC(..), stronglyConnComp, flattenSCC, flattenSCCs )
import ErrUtils		( showPass )
import SysTools		( cleanTempFilesExcept )
import BasicTypes	( SuccessFlag(..), succeeded )
import StringBuffer	( hGetStringBuffer )
import Type		( dropForAlls )
import Util
import Outputable
import Panic
import CmdLineOpts	( DynFlags(..), dopt )
import Maybes		( expectJust, orElse, mapCatMaybes )
import FiniteMap

import DATA_IOREF	( readIORef )

#ifdef GHCI
import HscMain		( hscGetInfo, GetInfoResult, hscStmt, hscTcExpr, hscKcType )
import HscTypes		( TyThing(..), icPrintUnqual, showModMsg )
import TcRnDriver	( mkExportEnv, getModuleContents )
import IfaceSyn		( IfaceDecl )
import RdrName		( GlobalRdrEnv, plusGlobalRdrEnv )
import Name		( Name )
import NameEnv
import Id		( idType )
import Type		( tidyType )
import VarEnv		( emptyTidyEnv )
import Linker		( HValue, unload, extendLinkEnv )
import GHC.Exts		( unsafeCoerce# )
import Foreign
import Control.Exception as Exception ( Exception, try )
import CmdLineOpts	( DynFlag(..), dopt_unset )
#endif

import EXCEPTION	( throwDyn )

-- std
import Directory        ( getModificationTime, doesFileExist )
import IO
import Monad
import List		( nub )
import Maybe
\end{code}


%************************************************************************
%*									*
		The module dependency graph
		ModSummary, ModGraph, NodeKey, NodeMap
%*									*
%************************************************************************

The nodes of the module graph are
	EITHER a regular Haskell source module
	OR     a hi-boot source module

A ModuleGraph contains all the nodes from the home package (only).  
There will be a node for each source module, plus a node for each hi-boot
module.

\begin{code}
type ModuleGraph = [ModSummary]  -- The module graph, 
				 -- NOT NECESSARILY IN TOPOLOGICAL ORDER

emptyMG :: ModuleGraph
emptyMG = []

--------------------
ms_allimps :: ModSummary -> [Module]
ms_allimps ms = ms_srcimps ms ++ ms_imps ms

--------------------
type NodeKey   = (Module, HscSource)	  -- The nodes of the graph are 
type NodeMap a = FiniteMap NodeKey a	  -- keyed by (mod, src_file_type) pairs

msKey :: ModSummary -> NodeKey
msKey (ModSummary { ms_mod = mod, ms_hsc_src = boot }) = (mod,boot)

emptyNodeMap :: NodeMap a
emptyNodeMap = emptyFM

mkNodeMap :: [ModSummary] -> NodeMap ModSummary
mkNodeMap summaries = listToFM [ (msKey s, s) | s <- summaries]
	
nodeMapElts :: NodeMap a -> [a]
nodeMapElts = eltsFM
\end{code}


%************************************************************************
%*									*
		The compilation manager state
%*									*
%************************************************************************


\begin{code}
-- Persistent state for the entire system
data CmState
   = CmState {
	cm_hsc :: HscEnv,		-- Includes the home-package table
	cm_mg  :: ModuleGraph,		-- The module graph
  	cm_ic  :: InteractiveContext 	-- Command-line binding info
     }

#ifdef GHCI
cmGetModuleGraph cmstate = cm_mg cmstate
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

findModuleLinkable_maybe :: [Linkable] -> Module -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        many -> pprPanic "findModuleLinkable" (ppr mod)

delModuleLinkable :: [Linkable] -> Module -> [Linkable]
delModuleLinkable ls mod = [ l | l@(LM _ nm _) <- ls, nm /= mod ]
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

  let export_mods = map mkModule exports
  mapM_ (checkModuleExists (hsc_dflags hsc_env) hpt) export_mods
  export_env  <- mkExportEnv hsc_env export_mods
  toplev_envs <- mapM (mkTopLevEnv hpt) toplevs

  let all_env = foldr plusGlobalRdrEnv export_env toplev_envs
  return cmstate{ cm_ic = old_ic { ic_toplev_scope = toplevs,
 		     		   ic_exports      = exports,
			       	   ic_rn_gbl_env   = all_env } }

checkModuleExists :: DynFlags -> HomePackageTable -> Module -> IO ()
checkModuleExists dflags hpt mod = 
  case lookupModuleEnv hpt mod of
    Just mod_info -> return ()
    _not_a_home_module -> do
	  res <- findPackageModule dflags mod True
	  case res of
	    Found _ _ -> return  ()
	    err -> let msg = cantFindError dflags mod err in
		   throwDyn (CmdLineError (showSDoc msg))

mkTopLevEnv :: HomePackageTable -> String -> IO GlobalRdrEnv
mkTopLevEnv hpt mod
 = case lookupModuleEnv hpt (mkModule mod) of
      Nothing      -> throwDyn (ProgramError ("mkTopLevEnv: not a home module " ++ mod))
      Just details -> case mi_globals (hm_iface details) of
			Nothing  -> throwDyn (ProgramError ("mkTopLevEnv: not interpreted " ++ mod))
			Just env -> return env

cmGetContext :: CmState -> IO ([String],[String])
cmGetContext CmState{cm_ic=ic} = 
  return (ic_toplev_scope ic, ic_exports ic)

cmModuleIsInterpreted :: CmState -> String -> IO Bool
cmModuleIsInterpreted cmstate str 
 = case lookupModuleEnv (cmHPT cmstate) (mkModule str) of
      Just details       -> return (isJust (mi_globals (hm_iface details)))
      _not_a_home_module -> return False

-----------------------------------------------------------------------------

cmSetDFlags :: CmState -> DynFlags -> CmState
cmSetDFlags cm_state dflags 
  = cm_state { cm_hsc = (cm_hsc cm_state) { hsc_dflags = dflags } }

cmGetDFlags :: CmState -> DynFlags
cmGetDFlags cm_state = hsc_dflags (cm_hsc cm_state)

-----------------------------------------------------------------------------
-- cmInfoThing: convert a String to a TyThing

-- A string may refer to more than one TyThing (eg. a constructor,
-- and type constructor), so we return a list of all the possible TyThings.

cmGetInfo :: CmState -> String -> IO [GetInfoResult]
cmGetInfo cmstate id = hscGetInfo (cm_hsc cmstate) (cm_ic cmstate) id

-- ---------------------------------------------------------------------------
-- cmBrowseModule: get all the TyThings defined in a module

cmBrowseModule :: CmState -> String -> Bool -> IO [IfaceDecl]
cmBrowseModule cmstate str exports_only
  = do { mb_decls <- getModuleContents (cm_hsc cmstate) (cm_ic cmstate) 
		     		       (mkModule str) exports_only
       ; case mb_decls of
	   Nothing -> return []		-- An error of some kind
	   Just ds -> return ds
   }


-----------------------------------------------------------------------------
cmShowModule :: CmState -> ModSummary -> String
cmShowModule cmstate mod_summary
  = case lookupModuleEnv hpt (ms_mod mod_summary) of
	Nothing	      -> panic "missing linkable"
	Just mod_info -> showModMsg obj_linkable mod_summary
		      where
			 obj_linkable = isObjectLinkable (hm_linkable mod_info)
  where
    hpt  = hsc_HPT (cm_hsc cmstate)

-----------------------------------------------------------------------------
-- cmRunStmt:  Run a statement/expr.

data CmRunResult
  = CmRunOk [Name] 		-- names bound by this evaluation
  | CmRunFailed 
  | CmRunException Exception	-- statement raised an exception

cmRunStmt :: CmState -> String -> IO (CmState, CmRunResult)		
cmRunStmt cmstate@CmState{ cm_hsc=hsc_env, cm_ic=icontext } expr
   = do 
	-- Turn off -fwarn-unused-bindings when running a statement, to hide
	-- warnings about the implicit bindings we introduce.
	let dflags'  = dopt_unset (hsc_dflags hsc_env) Opt_WarnUnusedBinds
	    hsc_env' = hsc_env{ hsc_dflags = dflags' }

        maybe_stuff <- hscStmt hsc_env' icontext expr

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
	   Just ty -> return (Just (showSDocForUser unqual doc))
 	     where 
		doc     = text expr <+> dcolon <+> ppr final_ty
		unqual  = icPrintUnqual (cm_ic cmstate)
		tidy_ty = tidyType emptyTidyEnv ty
		dflags  = hsc_dflags (cm_hsc cmstate)
		-- if -fglasgow-exts is on we show the foralls, otherwise
		-- we don't.
		final_ty
		  | dopt Opt_GlasgowExts dflags = tidy_ty
		  | otherwise			= dropForAlls tidy_ty

-----------------------------------------------------------------------------
-- cmKindOfType: returns a string representing the kind of a type

cmKindOfType :: CmState -> String -> IO (Maybe String)
cmKindOfType cmstate str
   = do maybe_stuff <- hscKcType (cm_hsc cmstate) (cm_ic cmstate) str
	case maybe_stuff of
	   Nothing -> return Nothing
	   Just kind -> return (Just res_str)
 	     where 
		res_str = showSDocForUser unqual (text str <+> dcolon <+> ppr kind)
		unqual  = icPrintUnqual (cm_ic cmstate)

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

cm_unload hsc_env stable_linkables	-- Unload everthing *except* 'stable_linkables'
  = case hsc_mode hsc_env of
	Batch -> return ()
#ifdef GHCI
	Interactive -> Linker.unload (hsc_dflags hsc_env) stable_linkables
#else
	Interactive -> panic "cm_unload: no interpreter"
#endif
	other -> panic "cm_unload: strange mode"
    

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
       cmDownsweep dflags rootnames (cm_mg cmstate) []
  where
    hsc_env = cm_hsc cmstate
    dflags  = hsc_dflags hsc_env
    gmode   = hsc_mode hsc_env

-----------------------------------------------------------------------------
-- The real business of the compilation manager: given a system state and
-- a module name, try and bring the module up to date, probably changing
-- the system state at the same time.

cmLoadModules :: CmState 		-- The HPT may not be as up to date
              -> ModuleGraph		-- Bang up to date; but may contain hi-boot no
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
	mb_main_mod <- readIORef v_MainModIs
        let 
	    main_mod = mb_main_mod `orElse` "Main"
	    a_root_is_Main 
               = any ((==main_mod).moduleUserString.ms_mod) 
                     mg2unsorted

        let mg2unsorted_names = map ms_mod mg2unsorted

        -- mg2 should be cycle free; but it includes hi-boot ModSummary nodes
        let mg2 :: [SCC ModSummary]
	    mg2 = cmTopSort False mg2unsorted

        -- mg2_with_srcimps drops the hi-boot nodes, returning a 
	-- graph with cycles.  Among other things, it is used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hpt all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps :: [SCC ModSummary]
	    mg2_with_srcimps = cmTopSort True mg2unsorted

	-- Sort out which linkables we wish to keep in the unlinked image.
	-- See getValidLinkables below for details.
	(valid_old_linkables, new_linkables)
	    <- getValidLinkables ghci_mode (hptLinkables hpt1)
		  mg2unsorted_names mg2_with_srcimps

	-- putStrLn (showSDoc (vcat [ppr valid_old_linkables, ppr new_linkables]))

	-- The new_linkables are .o files we found on the disk, presumably
	-- as a result of a GHC run "on the side".  So we'd better forget
	-- everything we know abouut those modules!
	let old_hpt = delModuleEnvList hpt1 (map linkableModule new_linkables)

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

	-- mg2_with_srcimps has no hi-boot nodes, 
	-- and hence neither does stable_mods 
        stable_summaries <- preUpsweep valid_old_linkables
		 		       mg2unsorted_names [] mg2_with_srcimps
        let stable_mods      = map ms_mod stable_summaries
	    stable_linkables = filter (\m -> linkableModule m `elem` stable_mods) 
				      valid_old_linkables

	    stable_hpt = filterModuleEnv is_stable_hm hpt1
	    is_stable_hm hm_info = mi_module (hm_iface hm_info) `elem` stable_mods

            upsweep_these
               = filter (\scc -> any (`notElem` stable_mods) 
                                     (map ms_mod (flattenSCC scc)))
                        mg2

        when (verb >= 2) $
           hPutStrLn stderr (showSDoc (text "Stable modules:" 
                               <+> sep (map (text.moduleUserString) stable_mods)))

	-- Unload any modules which are going to be re-linked this time around.
	cm_unload hsc_env stable_linkables

	-- We can now glom together our linkable sets
	let valid_linkables = valid_old_linkables ++ new_linkables

        -- We could at this point detect cycles which aren't broken by
        -- a source-import, and complain immediately, but it seems better
        -- to let upsweep_mods do this, so at least some useful work gets
        -- done before the upsweep is abandoned.
        --hPutStrLn stderr "after tsort:\n"
        --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Because we don't take into account source imports when doing
        -- the topological sort, there shouldn't be any cycles in mg2.
        -- If there is, we complain and give up -- the user needs to
        -- break the cycle using a boot file.

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

	-- clean up between compilations
	let cleanup = cleanTempFilesExcept dflags
			  (ppFilesFromSummaries (flattenSCCs mg2))

        (upsweep_ok, hsc_env3, modsUpswept)
           <- upsweep_mods (hsc_env { hsc_HPT = stable_hpt })
			   (old_hpt, valid_linkables)
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
	      cleanTempFilesExcept dflags (ppFilesFromSummaries modsDone)

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
	         hPutStrLn stderr ("Warning: output was redirected with -o, but no output will be generated\nbecause there is no " ++ main_mod ++ " module.")

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
                     = map ms_mod modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).ms_mod) 
			  modsDone

              let hpt4 = retainInTopLevelEnvs (map ms_mod mods_to_keep) 
					      (hsc_HPT hsc_env3)

	      -- Clean up after ourselves
	      cleanTempFilesExcept dflags (ppFilesFromSummaries mods_to_keep)

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
           mods_loaded = map (moduleUserString.ms_mod) 
			     (cm_mg cmstate)

       return (new_cmstate, ok, mods_loaded)

-- used to fish out the preprocess output files for the purposes of
-- cleaning up.  The preprocessed file *might* be the same as the
-- source file, but that doesn't do any harm.
ppFilesFromSummaries summaries = [ fn | Just fn <- map ms_hspp_file summaries ]

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
	-> [Module]		-- all home modules
	-> [SCC ModSummary]	-- all modules in the program, dependency order
	-> IO ( [Linkable],	-- still-valid linkables 
		[Linkable] 	-- new linkables we just found on the disk
				-- presumably generated by separate run of ghc
	      )

getValidLinkables mode old_linkables all_home_mods module_graph
  = do	{ 	-- Process the SCCs in bottom-to-top order
		-- (foldM works left-to-right)
	  ls <- foldM (getValidLinkablesSCC mode old_linkables all_home_mods) 
	  	      [] module_graph
	; return (partition_it ls [] []) }
 where
  partition_it []         valid new = (valid,new)
  partition_it ((l,b):ls) valid new 
	| b         = partition_it ls valid (l:new)
	| otherwise = partition_it ls (l:valid) new


getValidLinkablesSCC
	:: GhciMode
	-> [Linkable]		-- old linkables
	-> [Module]		-- all home modules
	-> [(Linkable,Bool)]
	-> SCC ModSummary
	-> IO [(Linkable,Bool)]

getValidLinkablesSCC mode old_linkables all_home_mods new_linkables scc0
   = let 
	  scc             = flattenSCC scc0
          scc_names       = map ms_mod scc
	  home_module m   = m `elem` all_home_mods && m `notElem` scc_names
          scc_allhomeimps = nub (filter home_module (concatMap ms_imps scc))
		-- NB. ms_imps, not ms_allimps above.  We don't want to
		-- force a module's SOURCE imports to be already compiled for
		-- its object linkable to be valid.

		-- The new_linkables is only the *valid* linkables below here
	  has_object m = case findModuleLinkable_maybe (map fst new_linkables) m of
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
   = do let mod_name = ms_mod summary

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
           -> [Module]		-- names of all mods encountered in downsweep
           -> [ModSummary]	-- accumulating stable modules
           -> [SCC ModSummary]  -- scc-ified mod graph, including src imps
           -> IO [ModSummary]	-- stable modules

preUpsweep valid_lis all_home_mods stable []  = return stable
preUpsweep valid_lis all_home_mods stable (scc0:sccs)
   = do let scc = flattenSCC scc0
            scc_allhomeimps :: [Module]
            scc_allhomeimps 
               = nub (filter (`elem` all_home_mods) (concatMap ms_allimps scc))
            all_imports_in_scc_or_stable
               = all in_stable_or_scc scc_allhomeimps
	    scc_mods     = map ms_mod scc
            stable_names = scc_mods ++ map ms_mod stable
            in_stable_or_scc m = m `elem` stable_names

	    -- now we check for valid linkables: each module in the SCC must 
	    -- have a valid linkable (see getValidLinkables above).
	    has_valid_linkable scc_mod
   	      = isJust (findModuleLinkable_maybe valid_lis scc_mod)

	    scc_is_stable = all_imports_in_scc_or_stable
			  && all has_valid_linkable scc_mods

        if scc_is_stable
         then preUpsweep valid_lis all_home_mods (scc ++ stable) sccs
         else preUpsweep valid_lis all_home_mods stable 	 sccs


-- Return (names of) all those in modsDone who are part of a cycle
-- as defined by theGraph.
findPartiallyCompletedCycles :: [Module] -> [SCC ModSummary] -> [Module]
findPartiallyCompletedCycles modsDone theGraph
   = chew theGraph
     where
        chew [] = []
        chew ((AcyclicSCC v):rest) = chew rest    -- acyclic?  not interesting.
        chew ((CyclicSCC vs):rest)
           = let names_in_this_cycle = nub (map ms_mod vs)
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
upsweep_mods :: HscEnv				-- Includes initially-empty HPT
             -> (HomePackageTable, [Linkable])	-- HPT and valid linkables from last time round
	     -> IO ()				-- How to clean up unwanted tmp files
             -> [SCC ModSummary]		-- Mods to do (the worklist)
             -> IO (SuccessFlag,
                    HscEnv,		-- With an updated HPT
                    [ModSummary])	-- Mods which succeeded

upsweep_mods hsc_env oldUI cleanup
     []
   = return (Succeeded, hsc_env, [])

upsweep_mods hsc_env oldUI cleanup
     (CyclicSCC ms:_)
   = do hPutStrLn stderr (showSDoc (cyclicModuleErr ms))
        return (Failed, hsc_env, [])

upsweep_mods hsc_env oldUI@(old_hpt, old_linkables) cleanup
     (AcyclicSCC mod:mods)
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++ 
	--	     show (map (moduleUserString.moduleName.mi_module.hm_iface) 
	--		       (moduleEnvElts (hsc_HPT hsc_env)))

        mb_mod_info <- upsweep_mod hsc_env oldUI mod 

	cleanup		-- Remove unwanted tmp files between compilations

        case mb_mod_info of
	    Nothing -> return (Failed, hsc_env, [])
	    Just mod_info -> do 
		{ let this_mod = ms_mod mod

			-- Add new info to hsc_env
		      hpt1     = extendModuleEnv (hsc_HPT hsc_env) this_mod mod_info
		      hsc_env1 = hsc_env { hsc_HPT = hpt1 }

			-- Space-saving: delete the old HPT entry and linkable for mod
			-- BUT if mod is a hs-boot node, don't delete it
			-- For the linkable this is dead right: the linkable relates only
			-- to the main Haskell source file. 
			-- For the interface, the HPT entry is probaby for the main Haskell
			-- source file.  Deleting it would force 
		      oldUI1 | isHsBoot (ms_hsc_src mod) = oldUI
			     | otherwise
			     = (delModuleEnv old_hpt this_mod, 
				  delModuleLinkable old_linkables this_mod)

		; (restOK, hsc_env2, modOKs) <- upsweep_mods hsc_env1 oldUI1 cleanup mods
		; return (restOK, hsc_env2, mod:modOKs) }


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> (HomePackageTable, UnlinkedImage)
            -> ModSummary
            -> IO (Maybe HomeModInfo)	-- Nothing => Failed

upsweep_mod hsc_env (old_hpt, old_linkables) summary
   = do 
        let this_mod = ms_mod summary

	-- The old interface is ok if it's in the old HPT 
	--	a) we're compiling a source file, and the old HPT entry is for a source file
	--	b) we're compiling a hs-boot file
	-- Case (b) allows an hs-boot file to get the interface of its real source file
	-- on the second iteration of the compilation manager, but that does no harm.
	-- Otherwise the hs-boot file will always be recompiled
            mb_old_iface 
		= case lookupModuleEnv old_hpt this_mod of
		     Nothing	 				  -> Nothing
		     Just hm_info | isHsBoot (ms_hsc_src summary) -> Just iface
				  | not (mi_boot iface) 	  -> Just iface
				  | otherwise		  	  -> Nothing
				   where 
				     iface = hm_iface hm_info

            maybe_old_linkable = findModuleLinkable_maybe old_linkables this_mod
            source_unchanged   = isJust maybe_old_linkable

            old_linkable = expectJust "upsweep_mod:old_linkable" maybe_old_linkable

	    have_object 
	       | Just l <- maybe_old_linkable, isObjectLinkable l = True
	       | otherwise = False

        compresult <- compile hsc_env summary source_unchanged have_object mb_old_iface

        case compresult of

           -- Compilation "succeeded", and may or may not have returned a new
           -- linkable (depending on whether compilation was actually performed
	   -- or not).
           CompOK new_details new_iface maybe_new_linkable
              -> do let 
			new_linkable = maybe_new_linkable `orElse` old_linkable
			new_info = HomeModInfo { hm_iface = new_iface,
						 hm_details = new_details,
						 hm_linkable = new_linkable }
                    return (Just new_info)

           -- Compilation failed.  Compile may still have updated the PCS, tho.
           CompErrs -> return Nothing

-- Filter modules in the HPT
retainInTopLevelEnvs :: [Module] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = mkModuleEnv [ (mod, fromJust mb_mod_info)
		 | mod <- keep_these
		 , let mb_mod_info = lookupModuleEnv hpt mod
		 , isJust mb_mod_info ]

-----------------------------------------------------------------------------
cmTopSort :: Bool 		-- Drop hi-boot nodes? (see below)
	  -> [ModSummary]
	  -> [SCC ModSummary]
-- Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
--
-- Drop hi-boot nodes (first boolean arg)? 
--
--   False:	treat the hi-boot summaries as nodes of the graph,
--		so the graph must be acyclic
--
--   True:	eliminate the hi-boot nodes, and instead pretend
--		the a source-import of Foo is an import of Foo
--		The resulting graph has no hi-boot nodes, but can by cyclic

cmTopSort drop_hs_boot_nodes summaries
   = stronglyConnComp nodes
   where
	-- Drop hs-boot nodes by using HsSrcFile as the key
	hs_boot_key | drop_hs_boot_nodes = HsSrcFile
		    | otherwise		 = HsBootFile	

	-- We use integers as the keys for the SCC algorithm
	nodes :: [(ModSummary, Int, [Int])]	
	nodes = [(s, fromJust (lookup_key (ms_hsc_src s) (ms_mod s)), 
		     out_edge_keys hs_boot_key (ms_srcimps s) ++
		     out_edge_keys HsSrcFile   (ms_imps s)    )
		| s <- summaries
		, not (ms_hsc_src s == HsBootFile && drop_hs_boot_nodes) ]
		-- Drop the hi-boot ones if told to do so

	key_map :: NodeMap Int
	key_map = listToFM ([(ms_mod s, ms_hsc_src s) | s <- summaries]
			   `zip` [1..])

	lookup_key :: HscSource -> Module -> Maybe Int
	lookup_key hs_src mod = lookupFM key_map (mod, hs_src)

	out_edge_keys :: HscSource -> [Module] -> [Int]
        out_edge_keys hi_boot ms = mapCatMaybes (lookup_key hi_boot) ms
		-- If we want keep_hi_boot_nodes, then we do lookup_key with
		-- the IsBootInterface parameter True; else False


-----------------------------------------------------------------------------
-- Downsweep (dependency analysis)

-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.

-- We pass in the previous collection of summaries, which is used as a
-- cache to avoid recalculating a module summary if the source is
-- unchanged.
--
-- The returned list of [ModSummary] nodes has one node for each home-package
-- module.  The imports of these nodes are all there, including the imports
-- of non-home-package modules.

cmDownsweep :: DynFlags
	    -> [FilePath]	-- Roots
	    -> [ModSummary]	-- Old summaries
	    -> [Module]		-- Ignore dependencies on these; treat them as
				-- if they were package modules
	    -> IO [ModSummary]
cmDownsweep dflags roots old_summaries excl_mods
   = do rootSummaries <- mapM getRootSummary roots
	checkDuplicates rootSummaries
        loop (concatMap msImports rootSummaries) 
	     (mkNodeMap rootSummaries)
     where
	old_summary_map :: NodeMap ModSummary
	old_summary_map = mkNodeMap old_summaries

	getRootSummary :: FilePath -> IO ModSummary
	getRootSummary file
	   | isHaskellSrcFilename file
	   = do exists <- doesFileExist file
		if exists then summariseFile dflags file else do
		throwDyn (CmdLineError ("can't find file `" ++ file ++ "'"))	
	   | otherwise
 	   = do exists <- doesFileExist hs_file
		if exists then summariseFile dflags hs_file else do
		exists <- doesFileExist lhs_file
		if exists then summariseFile dflags lhs_file else do
		let mod_name = mkModule file
		maybe_summary <- summarise dflags emptyNodeMap Nothing False 
					   mod_name excl_mods
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

	loop :: [(FilePath,Module,IsBootInterface)]	-- Work list: process these modules
	     -> NodeMap ModSummary 	-- Visited set
	     -> IO [ModSummary]		-- The result includes the worklist, except 
					-- for those mentioned in the visited set
	loop [] done 	  = return (nodeMapElts done)
	loop ((cur_path, wanted_mod, is_boot) : ss) done 
	  | key `elemFM` done = loop ss done
	  | otherwise	      = do { mb_s <- summarise dflags old_summary_map 
						 (Just cur_path) is_boot 
						 wanted_mod excl_mods
				   ; case mb_s of
					Nothing -> loop ss done
					Just s  -> loop (msImports s ++ ss) 
							(addToFM done key s) }
	  where
	    key = (wanted_mod, if is_boot then HsBootFile else HsSrcFile)

msImports :: ModSummary -> [(FilePath, 		-- Importing module
			     Module,	 	-- Imported module
			     IsBootInterface)]	 -- {-# SOURCE #-} import or not
msImports s =  [(f, m,True)  | m <- ms_srcimps s] 
	    ++ [(f, m,False) | m <- ms_imps    s] 
	where
	  f = msHsFilePath s	-- Keep the importing module for error reporting


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

summariseFile :: DynFlags -> FilePath -> IO ModSummary
-- Used for Haskell source only, I think
-- We know the file name, and we know it exists,
-- but we don't necessarily know the module name (might differ)
summariseFile dflags file
   = do (dflags', hspp_fn) <- preprocess dflags file
		-- The dflags' contains the OPTIONS pragmas

	-- Read the file into a buffer.  We're going to cache
	-- this buffer in the ModLocation (ml_hspp_buf) so that it
	-- doesn't have to be slurped again when hscMain parses the
	-- file later.
	buf <- hGetStringBuffer hspp_fn
        (srcimps,the_imps,mod) <- getImports dflags' buf hspp_fn

	-- Make a ModLocation for this file
	location <- mkHomeModLocation mod file

	-- Tell the Finder cache where it is, so that subsequent calls
	-- to findModule will find it, even if it's not on any search path
	addHomeModuleToFinder mod location

        src_timestamp <- getModificationTime file
        return (ModSummary { ms_mod = mod, ms_hsc_src = HsSrcFile,
			     ms_location = location,
                             ms_hspp_file = Just hspp_fn,
			     ms_hspp_buf  = Just buf,
                             ms_srcimps = srcimps, ms_imps = the_imps,
			     ms_hs_date = src_timestamp })

-- Summarise a module, and pick up source and timestamp.
summarise :: DynFlags 
	  -> NodeMap ModSummary	-- Map of old summaries
	  -> Maybe FilePath	-- Importing module (for error messages)
	  -> IsBootInterface	-- True <=> a {-# SOURCE #-} import
	  -> Module 		-- Imported module to be summarised
	  -> [Module]		-- Modules to exclude
	  -> IO (Maybe ModSummary)	-- Its new summary

summarise dflags old_summary_map cur_mod is_boot wanted_mod excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- lookupFM old_summary_map (wanted_mod, hsc_src)
  = do	{ 	-- Find its new timestamp; all the 
		-- ModSummaries in the old map have valid ml_hs_files
	   let location = ms_location old_summary
	       src_fn = fromJust (ml_hs_file location)

	;  src_timestamp <- getModificationTime src_fn

		-- return the cached summary if the source didn't change
	; if ms_hs_date old_summary == src_timestamp 
	  then return (Just old_summary)
	  else new_summary location
	}

  | otherwise
  = do	{ found <- findModule dflags wanted_mod True {-explicit-}
	; case found of
	     Found location pkg 
		| not (isHomePackage pkg)      -> return Nothing	-- Drop external-pkg
		| isJust (ml_hs_file location) -> new_summary location	-- Home package
	     err        -> noModError dflags cur_mod wanted_mod err	-- Not found
	}
  where
    hsc_src = if is_boot then HsBootFile else HsSrcFile

    new_summary location
      = do { 	-- Adjust location to point to the hs-boot source file, 
		-- hi file, object file, when is_boot says so
	  let location' | is_boot   = addBootSuffixLocn location
			| otherwise = location
	      src_fn = fromJust (ml_hs_file location')

		-- Check that it exists
		-- It might have been deleted since the Finder last found it
	; exists <- doesFileExist src_fn
	; if exists then return () else noHsFileErr cur_mod src_fn

	-- Preprocess the source file and get its imports
	-- The dflags' contains the OPTIONS pragmas
	; (dflags', hspp_fn) <- preprocess dflags src_fn
	; buf <- hGetStringBuffer hspp_fn
        ; (srcimps, the_imps, mod_name) <- getImports dflags' buf hspp_fn

	; when (mod_name /= wanted_mod) $
		throwDyn (ProgramError 
		   (showSDoc (text src_fn
			      <>  text ": file name does not match module name"
			      <+> quotes (ppr mod_name))))

		-- Find its timestamp, and return the summary
        ; src_timestamp <- getModificationTime src_fn
	; return (Just ( ModSummary { ms_mod       = wanted_mod, 
				      ms_hsc_src   = hsc_src,
				      ms_location  = location',
				      ms_hspp_file = Just hspp_fn,
				      ms_hspp_buf  = Just buf,
				      ms_srcimps   = srcimps,
				      ms_imps      = the_imps,
				      ms_hs_date   = src_timestamp }))
    	}


-----------------------------------------------------------------------------
-- 			Error messages
-----------------------------------------------------------------------------

noModError :: DynFlags -> Maybe FilePath -> Module -> FindResult -> IO ab
-- ToDo: we don't have a proper line number for this error
noModError dflags cur_mod wanted_mod err
  = throwDyn $ ProgramError $ showSDoc $
    vcat [cantFindError dflags wanted_mod err,
	  nest 2 (parens (pp_where cur_mod))]
				
noHsFileErr cur_mod path
  = throwDyn $ CmdLineError $ showSDoc $
    vcat [text "Can't find" <+> text path,
	  nest 2 (parens (pp_where cur_mod))]
 
pp_where Nothing  = text "one of the roots of the dependency analysis"
pp_where (Just p) = text "imported from" <+> text p

packageModErr mod
  = throwDyn (CmdLineError (showSDoc (text "module" <+>
				   quotes (ppr mod) <+>
				   text "is a package module")))

multiRootsErr mod files
  = throwDyn (ProgramError (showSDoc (
	text "module" <+> quotes (ppr mod) <+> 
	text "is defined in multiple files:" <+>
	sep (map text files))))

cyclicModuleErr :: [ModSummary] -> SDoc
cyclicModuleErr ms
  = hang (ptext SLIT("Module imports form a cycle for modules:"))
       2 (vcat (map show_one ms))
  where
    show_one ms = sep [ show_mod (ms_hsc_src ms) (ms_mod ms),
			nest 2 $ ptext SLIT("imports:") <+> 
				   (pp_imps HsBootFile (ms_srcimps ms)
				   $$ pp_imps HsSrcFile  (ms_imps ms))]
    show_mod hsc_src mod = ppr mod <> text (hscSourceString hsc_src)
    pp_imps src mods = fsep (map (show_mod src) mods)
\end{code}

