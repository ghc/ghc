%
% (c) The University of Glasgow, 2001
%
\section[CmLink]{The compilation manager's linker}

\begin{code}
module CmLink (
	LinkResult(..),	link, unload,

	filterModuleLinkables,
	findModuleLinkable_maybe,

        PersistentLinkerState{-abstractly!-}, emptyPLS,

#ifdef GHCI
	delListFromClosureEnv,
	addListToClosureEnv,
	linkExpr
#endif
  ) where


#ifdef GHCI
import ByteCodeLink	( linkIModules, linkIExpr )
#endif

import Interpreter
import DriverPipeline
import CmTypes
import HscTypes		( GhciMode(..) )
import Outputable	( SDoc )
import Name		( Name )
import Module		( ModuleName )
import FiniteMap
import Outputable
import ErrUtils		( showPass )
import CmdLineOpts	( DynFlags(..) )
import Util

import Exception	( block )
import IOExts
import List
import Monad
import IO

#include "HsVersions.h"

-- ---------------------------------------------------------------------------
-- The Linker's state

-- The PersistentLinkerState maps Names to actual closures (for
-- interpreted code only), for use during linking.

data PersistentLinkerState
   = PersistentLinkerState {

#ifdef GHCI
	-- Current global mapping from RdrNames to closure addresses
        closure_env :: ClosureEnv,

	-- the current global mapping from RdrNames of DataCons to
	-- info table addresses.
	-- When a new Unlinked is linked into the running image, or an existing
	-- module in the image is replaced, the itbl_env must be updated
	-- appropriately.
        itbl_env    :: ItblEnv,

	-- the currently loaded interpreted modules
	bcos_loaded :: [Linkable]

#else
	dummy :: ()	--  sigh, can't have an empty record
#endif

     }

emptyPLS :: IO PersistentLinkerState
#ifdef GHCI
emptyPLS = return (PersistentLinkerState { closure_env = emptyFM,
                                           itbl_env    = emptyFM,
					   bcos_loaded = [] })
#else
emptyPLS = return (PersistentLinkerState {})
#endif

-- We also keep track of which object modules are currently loaded
-- into the dynamic linker, so that we can unload them again later.
--
-- This state *must* match the actual state of the dyanmic linker at
-- all times, which is why we keep it private here and don't
-- put it in the PersistentLinkerState.
--
GLOBAL_VAR(v_ObjectsLoaded, [], [Linkable])


-- ---------------------------------------------------------------------------
-- Utils

findModuleLinkable_maybe :: [Linkable] -> ModuleName -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        many -> pprPanic "findModuleLinkable" (ppr mod)

filterModuleLinkables :: (ModuleName -> Bool) -> [Linkable] -> [Linkable]
filterModuleLinkables p [] = []
filterModuleLinkables p (li:lis)
   = case li of
        LM _ modnm _ -> if p modnm then retain else dump
     where
        dump   = filterModuleLinkables p lis
        retain = li : dump

linkableInSet :: Linkable -> [Linkable] -> Bool
linkableInSet l objs_loaded =
  case findModuleLinkable_maybe objs_loaded (linkableModName l) of
	Nothing -> False
	Just m  -> linkableTime l == linkableTime m

-- These two are used to add/remove entries from the closure env for
-- new bindings made at the prompt.
#ifdef GHCI
delListFromClosureEnv :: PersistentLinkerState -> [Name]
  	-> IO PersistentLinkerState
delListFromClosureEnv pls names
  = return pls{ closure_env = delListFromFM (closure_env pls) names }

addListToClosureEnv :: PersistentLinkerState -> [(Name,HValue)]
	-> IO PersistentLinkerState
addListToClosureEnv pls new_bindings
  = return pls{ closure_env = addListToFM (closure_env pls) new_bindings }
#endif

-- ---------------------------------------------------------------------------
-- Unloading old objects ready for a new compilation sweep.
--
-- The compilation manager provides us with a list of linkables that it
-- considers "stable", i.e. won't be recompiled this time around.  For
-- each of the modules current linked in memory,
--
--	* if the linkable is stable (and it's the same one - the
--	  user may have recompiled the module on the side), we keep it,
--
--	* otherwise, we unload it.
--
--      * we also implicitly unload all temporary bindings at this point.

unload :: GhciMode
       -> DynFlags
       -> [Linkable]		-- stable linkables
       -> PersistentLinkerState
       -> IO PersistentLinkerState

unload Batch dflags linkables pls = return pls

#ifdef GHCI
unload Interactive dflags linkables pls
  = block $ do -- block, so we're safe from Ctrl-C in here
	objs_loaded  <- readIORef v_ObjectsLoaded
	objs_loaded' <- filterM (maybeUnload objs_to_keep) objs_loaded
	writeIORef v_ObjectsLoaded objs_loaded'

        bcos_loaded' <- filterM (maybeUnload bcos_to_keep) (bcos_loaded pls)

       	let objs_retained = map linkableModName objs_loaded'
	    bcos_retained = map linkableModName bcos_loaded'
	    itbl_env'     = filterNameMap bcos_retained (itbl_env pls)
            closure_env'  = filterNameMap bcos_retained (closure_env pls)

       	let verb = verbosity dflags
       	when (verb >= 3) $ do
	    hPutStrLn stderr (showSDoc
		(text "CmLink.unload: retaining objs" <+> ppr objs_retained))
	    hPutStrLn stderr (showSDoc
		(text "CmLink.unload: retaining bcos" <+> ppr bcos_retained))

       	return pls{ itbl_env = itbl_env',
	            closure_env = closure_env',
		    bcos_loaded = bcos_loaded' }
  where
	(objs_to_keep, bcos_to_keep) = partition isObjectLinkable linkables

	maybeUnload :: [Linkable] -> Linkable -> IO Bool
	maybeUnload keep_linkables l@(LM time mod objs)
	   | linkableInSet l linkables
		= return True
	   | otherwise
		= do mapM unloadObj [ f | DotO f <- objs ]
		     return False
#else
unload Interactive dflags linkables pls = panic "CmLink.unload: no interpreter"
#endif

-----------------------------------------------------------------------------
-- Linking

data LinkResult
   = LinkOK   PersistentLinkerState
   | LinkErrs PersistentLinkerState [SDoc]

link :: GhciMode		-- interactive or batch
     -> DynFlags		-- dynamic flags
     -> Bool			-- attempt linking in batch mode?
     -> [Linkable]
     -> PersistentLinkerState
     -> IO LinkResult

-- For the moment, in the batch linker, we don't bother to tell doLink
-- which packages to link -- it just tries all that are available.
-- batch_attempt_linking should only be *looked at* in batch mode.  It
-- should only be True if the upsweep was successful and someone
-- exports main, i.e., we have good reason to believe that linking
-- will succeed.

-- There will be (ToDo: are) two lists passed to link.  These
-- correspond to
--
--	1. The list of all linkables in the current home package.  This is
--	   used by the batch linker to link the program, and by the interactive
--	   linker to decide which modules from the previous link it can
--	   throw away.
--	2. The list of modules on which we just called "compile".  This list
--	   is used by the interactive linker to decide which modules need
--	   to be actually linked this time around (or unlinked and re-linked
--	   if the module was recompiled).

link mode dflags batch_attempt_linking linkables pls1
   = do let verb = verbosity dflags
        when (verb >= 3) $ do
	     hPutStrLn stderr "CmLink.link: linkables are ..."
             hPutStrLn stderr (showSDoc (vcat (map ppr linkables)))
	res <- link' mode dflags batch_attempt_linking linkables pls1
        when (verb >= 3) $
	     hPutStrLn stderr "CmLink.link: done"
	return res

link' Batch dflags batch_attempt_linking linkables pls1
   | batch_attempt_linking
   = do let o_files = concatMap getOfiles linkables
        when (verb >= 1) $
             hPutStrLn stderr "ghc: linking ..."
	-- don't showPass in Batch mode; doLink will do that for us.
        doLink o_files
	-- doLink only returns if it succeeds
        return (LinkOK pls1)
   | otherwise
   = do when (verb >= 3) $ do
	    hPutStrLn stderr "CmLink.link(batch): upsweep (partially) failed OR"
            hPutStrLn stderr "   Main.main not exported; not linking."
        return (LinkOK pls1)
   where
      verb = verbosity dflags
      getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)

#ifdef GHCI
link' Interactive dflags batch_attempt_linking linkables pls
    = do showPass dflags "Linking"
	 block $ do -- don't want to be interrupted by ^C in here

	    -- Always load objects first.  Objects aren't allowed to
	    -- depend on BCOs.
	    let (objs, bcos) = partition isObjectLinkable linkables

	    objs_loaded  <- readIORef v_ObjectsLoaded
	    objs_loaded' <- linkObjs objs objs_loaded
	    writeIORef v_ObjectsLoaded objs_loaded'

	    -- resolve symbols within the object files
	    resolveObjs

	    -- finally link the interpreted linkables
	    linkBCOs bcos [] pls
#endif

-----------------------------------------------------------------------------
-- Linker for interactive mode

#ifdef GHCI
linkObjs [] objs_loaded = return objs_loaded
linkObjs (l@(LM _ m uls) : ls) objs_loaded
   | linkableInSet l objs_loaded  = linkObjs ls objs_loaded -- already loaded
   | otherwise = do mapM_ loadObj [ file | DotO file <- uls ]
	  	    linkObjs ls (l:objs_loaded)

linkBCOs [] ul_trees pls = linkFinish pls ul_trees
linkBCOs (l@(LM _ m uls) : ls) ul_trees pls
   | linkableInSet l (bcos_loaded pls)
	= linkBCOs ls ul_trees pls
   | otherwise
	= linkBCOs ls (uls++ul_trees) pls{bcos_loaded = l : bcos_loaded pls}

-- link all the interpreted code in one go.
linkFinish pls ul_bcos = do

   let stuff = [ (bcos,itbls) | BCOs bcos itbls <- ul_bcos ]

   (ibinds, new_itbl_env, new_closure_env) <-
	linkIModules (itbl_env pls) (closure_env pls) stuff

   let new_pls = pls { closure_env = new_closure_env,
		       itbl_env    = new_itbl_env
		     }
   return (LinkOK new_pls)
#endif

-- ---------------------------------------------------------------------------
-- Link a single expression

#ifdef GHCI
linkExpr :: PersistentLinkerState -> UnlinkedBCOExpr -> IO HValue
linkExpr PersistentLinkerState{ itbl_env = ie, closure_env = ce } bcos
  = linkIExpr ie ce bcos
#endif
\end{code}
