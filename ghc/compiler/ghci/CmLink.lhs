%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmLink]{Linker for GHCI}

\begin{code}
module CmLink ( Linkable(..), 
		filterModuleLinkables, modname_of_linkable,
		LinkResult(..),
                HValue,
                link, 
                PLS{-abstractly!-}, emptyPLS )
		  
where

import CmStaticInfo	( PCI )
import CmFind		( Path, PkgName )
import Module		( Module )
import Outputable	( SDoc )
import FiniteMap	( FiniteMap, emptyFM )
import RdrName		( RdrName )
import Digraph		( SCC )
import Addr		( Addr )
import Panic		( panic )

#include "HsVersions.h"

\end{code}

\begin{code}
data PLS 
   = MkPLS {
        source_symtab :: FiniteMap RdrName HValue,
        object_symtab :: FiniteMap String Addr
     }

data HValue = HValue -- fix this ... just temporary?


link :: PCI -> [SCC Linkable] -> PLS -> IO LinkResult
link pci linkabless pls
   = return (error "link:unimp")

data LinkResult 
   = LinkOK   PLS
   | LinkErrs PLS [SDoc]

data Unlinked
   = DotO Path
   | DotA Path
   | DotDLL Path
   -- | Trees [StgTree RdrName]

data Linkable
   = LM {-should be:Module-} String{- == ModName-} [Unlinked]
   | LP PkgName

modname_of_linkable (LM nm _) = nm
modname_of_linkable (LP _)    = panic "modname_of_linkable: package"

filterModuleLinkables :: (String{- ==ModName-} -> Bool) 
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

emptyPLS :: IO PLS
emptyPLS = return (MkPLS { source_symtab = emptyFM, 
                           object_symtab = emptyFM })
\end{code}
