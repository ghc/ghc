%
% (c) The University of Glasgow, 2000
%
\section[CmLink]{Linker for GHCI}

\begin{code}
module CmLink ( Linkable(..),  Unlinked(..),
		filterModuleLinkables, 
		modname_of_linkable, is_package_linkable,
		LinkResult(..),
                link, 
                PersistentLinkerState{-abstractly!-}, emptyPLS )
where

import StgInterp	( linkIModules, ClosureEnv, ItblEnv )
import Linker

import CmStaticInfo	( PackageConfigInfo )
import Module		( ModuleName, PackageName )
import InterpSyn	( UnlinkedIBind, HValue, binder )
import Module		( Module )
import Outputable	( SDoc )
import FiniteMap	( FiniteMap, emptyFM )
import RdrName		( RdrName )
import Digraph		( SCC(..) )
import Addr		( Addr )
import Outputable
import Panic		( panic )

#include "HsVersions.h"
\end{code}

\begin{code}
data PersistentLinkerState 
   = PersistentLinkerState {
        closure_env :: ClosureEnv,
        itbl_env    :: ItblEnv
	-- notionally here, but really lives in the C part of the linker:
	--            object_symtab :: FiniteMap String Addr
     }

data LinkResult 
   = LinkOK   PersistentLinkerState
   | LinkErrs PersistentLinkerState [SDoc]

data Unlinked
   = DotO FilePath
   | DotA FilePath
   | DotDLL FilePath
   | Trees [UnlinkedIBind]	-- bunch of interpretable bindings

instance Outputable Unlinked where
   ppr (DotO path)   = text "DotO" <+> text path
   ppr (DotA path)   = text "DotA" <+> text path
   ppr (DotDLL path) = text "DotDLL" <+> text path
   ppr (Trees binds) = text "Trees" <+> ppr (map binder binds)


isObject (DotO _) = True
isObject (DotA _) = True
isObject (DotDLL _) = True
isObject _ = False

isInterpretable (Trees _) = True
isInterpretable _ = False

data Linkable
   = LM ModuleName [Unlinked]
   | LP PackageName

instance Outputable Linkable where
   ppr (LM mod_nm unlinkeds) = text "LinkableM" <+> ppr mod_nm <+> ppr unlinkeds
   ppr (LP package_nm)       = text "LinkableP" <+> ptext package_nm

emptyPLS :: IO PersistentLinkerState
emptyPLS = return (PersistentLinkerState { closure_env = emptyFM, 
                                           itbl_env    = emptyFM })
\end{code}

\begin{code}
link :: PackageConfigInfo 
     -> [SCC Linkable] 
     -> PersistentLinkerState 
     -> IO LinkResult

#ifndef GHCI_NOTYET
--link = panic "CmLink.link: not implemented"
link pci groups pls1
   = do putStrLn "Hello from the Linker!"
        putStrLn (showSDoc (vcat (map ppLinkableSCC groups)))
        putStrLn "Bye-bye from the Linker!"
        return (LinkOK pls1)

ppLinkableSCC :: SCC Linkable -> SDoc
ppLinkableSCC (CyclicSCC xs) = ppr xs
ppLinkableSCC (AcyclicSCC x) = ppr [x]


#else
link pci [] pls = return (LinkOK pls)
link pci (group:groups) pls = do
   -- the group is either all objects or all interpretable, for now
   if all isObject group
	then do mapM loadObj [ file | DotO file <- group ]
	        resolveObjs
		link pci groups pls
    else if all isInterpretable group
	then do (new_closure_env, new_itbl_env) <-
		   linkIModules	(closure_env pls)
				(itbl_env pls)
				[ trees | Trees trees <- group ]
	        link pci groups (PersistentLinkerState{
				   closure_env=new_closure_env,
				   itbl_env=new_itbl_env})
    else
	return (LinkErrs pls (ptext SLIT("linker: group must contain all objects or all interpreted modules")))
#endif

modname_of_linkable (LM nm _) = nm
modname_of_linkable (LP _)    = panic "modname_of_linkable: package"

is_package_linkable (LP _)   = True
is_package_linkable (LM _ _) = False

filterModuleLinkables :: (ModuleName -> Bool) 
                      -> [Linkable] 
                      -> [Linkable]
filterModuleLinkables p [] = []
filterModuleLinkables p (li:lis)
   = case li of
        LP _       -> retain
        LM modnm _ -> if p modnm then retain else dump
     where
        dump   = filterModuleLinkables p lis
        retain = li : dump
\end{code}
