%
% (c) The University of Glasgow, 2000
%
\section[CmLink]{Linker for GHCI}

\begin{code}
module CmLink ( Linkable(..),  Unlinked(..),
		filterModuleLinkables, 
		findModuleLinkable_maybe,
		LinkResult(..),
                link, 
		unload,
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
import CmStaticInfo	( GhciMode(..) )
import Outputable	( SDoc )
import Digraph		( SCC(..), flattenSCC )
import Name		( Name )
import Module		( ModuleName )
import FiniteMap
import Outputable
import ErrUtils		( showPass )
import CmdLineOpts	( DynFlags(..) )
import Panic		( panic )

import List
import Monad
import IO

#include "HsVersions.h"
\end{code}

\begin{code}
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

	-- list of objects we've loaded (we'll need to unload them again
	-- before re-loading the same module), together with the ClockTime
	-- of the linkable they were loaded from.
	objects_loaded :: [Linkable]

	-- notionally here, but really lives in the C part of the linker:
	--            object_symtab :: FiniteMap String Addr
#else
	dummy :: ()	--  sigh, can't have an empty record
#endif

     }

data LinkResult 
   = LinkOK   PersistentLinkerState
   | LinkErrs PersistentLinkerState [SDoc]

findModuleLinkable_maybe :: [Linkable] -> ModuleName -> Maybe Linkable
findModuleLinkable_maybe lis mod 
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        many -> pprPanic "findModuleLinkable" (ppr mod)


emptyPLS :: IO PersistentLinkerState
#ifdef GHCI
emptyPLS = return (PersistentLinkerState { closure_env = emptyFM, 
                                           itbl_env    = emptyFM,
					   objects_loaded = [] })
#else
emptyPLS = return (PersistentLinkerState {})
#endif

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

-----------------------------------------------------------------------------
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
  = do new_loaded <- filterM maybeUnload (objects_loaded pls)
       let mods_retained = map linkableModName new_loaded
	   itbl_env'     = filterNameMap mods_retained (itbl_env pls)
           closure_env'  = filterNameMap mods_retained (closure_env pls)

       let verb = verbosity dflags
       when (verb >= 3) $ do
	    hPutStrLn stderr (showSDoc 
		(text "CmLink.unload: retaining" <+> ppr mods_retained))

       return pls{ objects_loaded = new_loaded,
		   itbl_env = itbl_env',
	           closure_env = closure_env' }
  where
	maybeUnload :: Linkable -> IO Bool
	maybeUnload (LM time mod objs) = do
	  case findModuleLinkable_maybe linkables mod of
		Nothing -> do unloadObjs; return False
		Just l | linkableTime l /= time -> do unloadObjs; return False
		       | otherwise              -> return True
	  where
	     unloadObjs = mapM unloadObj [ f | DotO f <- objs ]
#else
unload Interactive dflags linkables pls = panic "CmLink.unload: no interpreter"
#endif
-----------------------------------------------------------------------------
-- Linking

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

link' Interactive dflags batch_attempt_linking linkables pls
    = do showPass dflags "Linking"
	 let (objs, bcos) = partition (isObject.head.linkableUnlinked) linkables
	 linkObjs (objs ++ bcos) pls
	   -- get the objects first

filterModuleLinkables :: (ModuleName -> Bool) -> [Linkable] -> [Linkable]
filterModuleLinkables p [] = []
filterModuleLinkables p (li:lis)
   = case li of
        LM _ modnm _ -> if p modnm then retain else dump
     where
        dump   = filterModuleLinkables p lis
        retain = li : dump

-----------------------------------------------------------------------------
-- Linker for interactive mode

#ifndef GHCI
linkObjs      = panic "CmLink.linkObjs: no interpreter"
#else
linkObjs [] pls = linkFinish pls []
linkObjs (l@(LM _ m uls) : ls) pls
   | all isObject uls = do
	if isLoaded l pls then linkObjs ls pls else do
	let objs = [ file | DotO file <- uls ] 
	mapM_ loadObj objs
	linkObjs ls pls{objects_loaded = l : objects_loaded pls}
   | all isInterpretable uls  = linkInterpretedCode (l:ls) [] pls
   | otherwise                = invalidLinkable

isLoaded :: Linkable -> PersistentLinkerState -> Bool
isLoaded l pls = 
  case findModuleLinkable_maybe (objects_loaded pls) (linkableModName l) of
	Nothing -> False
	Just m  -> linkableTime l == linkableTime m
 
linkInterpretedCode [] ul_trees pls = linkFinish pls ul_trees
linkInterpretedCode (l@(LM _ m uls) : ls) ul_trees pls
   | all isInterpretable uls = 
	if isLoaded l pls then linkInterpretedCode ls ul_trees pls else
	linkInterpretedCode ls (uls++ul_trees) 
		pls{objects_loaded = l : objects_loaded pls}
   | any isObject uls
        = panic "linkInterpretedCode: trying to link object code to interpreted code"
   | otherwise = invalidLinkable

invalidLinkable = panic "CmLink: linkable doesn't contain entirely objects or interpreted code"


-- link all the interpreted code in one go.
linkFinish pls ul_bcos = do
   resolveObjs

   let stuff = [ (bcos,itbls) | BCOs bcos itbls <- ul_bcos ]

   (ibinds, new_itbl_env, new_closure_env) <-
	linkIModules (itbl_env pls) (closure_env pls) stuff

   let new_pls = pls { closure_env = new_closure_env,
		       itbl_env    = new_itbl_env
		     }
   return (LinkOK new_pls)

linkExpr :: PersistentLinkerState -> UnlinkedBCOExpr -> IO HValue
linkExpr PersistentLinkerState{ itbl_env = ie, closure_env = ce } bcos
  = linkIExpr ie ce bcos
#endif
\end{code}
