%
% (c) The University of Glasgow, 2000
%
\section[CmLink]{Linker for GHCI}

\begin{code}
module CmLink ( Linkable(..),  Unlinked(..),
		filterModuleLinkables, 
		findModuleLinkable_maybe,
		modname_of_linkable, is_package_linkable,
		LinkResult(..),
                link, 
		unload,
                PersistentLinkerState{-abstractly!-}, emptyPLS,
#ifdef GHCI
		linkExpr
#endif
  ) where


import Interpreter
import DriverPipeline
import CmTypes
import CmStaticInfo	( GhciMode(..) )
import Outputable	( SDoc )
import Digraph		( SCC(..), flattenSCC )
import DriverUtil
import Module		( ModuleName )
import RdrName
import FiniteMap
import Outputable
import ErrUtils		( showPass )
import CmdLineOpts	( DynFlags(..) )
import Panic		( panic )

import Exception
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
        itbl_env    :: ItblEnv

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
                                           itbl_env    = emptyFM })
#else
emptyPLS = return (PersistentLinkerState {})
#endif
\end{code}

\begin{code}
link :: GhciMode		-- interactive or batch
     -> DynFlags		-- dynamic flags
     -> Bool			-- attempt linking in batch mode?
     -> [Linkable] 		-- only contains LMs, not LPs
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
      getOfiles (LP _)    = panic "CmLink.link(getOfiles): shouldn't get package linkables"
      getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)

link' Interactive dflags batch_attempt_linking linkables pls1
    = do showPass dflags "Linking"
	 pls2 <- unload pls1
	 linkObjs linkables pls2


ppLinkableSCC :: SCC Linkable -> SDoc
ppLinkableSCC = ppr . flattenSCC


modname_of_linkable (LM _ nm _) = nm
modname_of_linkable (LP _)      = panic "modname_of_linkable: package"

is_package_linkable (LP _)     = True
is_package_linkable (LM _ _ _) = False

filterModuleLinkables :: (ModuleName -> Bool) 
                      -> [Linkable] 
                      -> [Linkable]
filterModuleLinkables p [] = []
filterModuleLinkables p (li:lis)
   = case li of
        LP _         -> retain
        LM _ modnm _ -> if p modnm then retain else dump
     where
        dump   = filterModuleLinkables p lis
        retain = li : dump

-----------------------------------------------------------------------------
-- Linker for interactive mode

#ifndef GHCI
linkObjs      = panic "CmLink.linkObjs: no interpreter"
unload        = panic "CmLink.unload: no interpreter"
lookupClosure = panic "CmLink.lookupClosure: no interpreter"
#else
linkObjs [] pls = linkFinish pls [] []
linkObjs (l@(LM _ _ uls) : ls) pls
   | all isObject uls = do
	mapM_ loadObj [ file | DotO file <- uls ] 
	linkObjs ls pls
   | all isInterpretable uls  = linkInterpretedCode (l:ls) [] [] pls
   | otherwise                = invalidLinkable
linkObjs _ pls = 
   throwDyn (OtherError "CmLink.linkObjs: found package linkable")

 
linkInterpretedCode [] mods ul_trees pls = linkFinish pls mods ul_trees
linkInterpretedCode (LM _ m uls : ls) mods ul_trees pls
   | all isInterpretable uls = 
	linkInterpretedCode ls (m:mods) (uls++ul_trees) pls
        
   | any isObject uls
        = throwDyn (OtherError "can't link object code that depends on interpreted code")
   | otherwise = invalidLinkable
linkInterpretedCode _ _ _ pls = 
   throwDyn (OtherError "CmLink.linkInterpretedCode: found package linkable")

invalidLinkable = throwDyn (OtherError "linkable doesn't contain entirely objects interpreted code")


-- link all the interpreted code in one go.  We first remove from the
-- various environments any previous versions of these modules.
linkFinish pls mods ul_trees = do
   resolveObjs
   let itbl_env'    = filterNameMap mods (itbl_env pls)
       closure_env' = filterNameMap mods (closure_env pls)
       stuff        = [ (trees,itbls) | Trees trees itbls <- ul_trees ]

   (ibinds, new_itbl_env, new_closure_env) <-
	linkIModules itbl_env' closure_env' stuff

   let new_pls = PersistentLinkerState {
				  closure_env = new_closure_env,
				  itbl_env    = new_itbl_env
			}
   return (LinkOK new_pls)

-- purge the current "linked image"
unload :: PersistentLinkerState -> IO PersistentLinkerState
unload pls = return pls{ closure_env = emptyFM, itbl_env = emptyFM }

linkExpr :: PersistentLinkerState -> UnlinkedIExpr -> IO HValue
linkExpr PersistentLinkerState{ itbl_env = ie, closure_env = ce } expr
  = iExprToHValue ie ce expr
#endif
\end{code}
