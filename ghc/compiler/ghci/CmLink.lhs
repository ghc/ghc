%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmLink]{Linker for GHCI}

\begin{code}
module CmLink ( Linkable(..), LinkResult(..),
                HValue,
                link, 
                PLS{-abstractly!-}, emptyPLS )
		  
where

import CmStaticInfo	( PCI )
import CmFind		( Path, PkgName )
import Module		( Module )
import Outputable	( SDoc )
import FiniteMap	( FiniteMap )
import RdrName		( RdrName )
import Addr		( Addr )

#include "HsVersions.h"

\end{code}

\begin{code}
data PLS 
   = MkPLS {
        source_symtab :: FiniteMap RdrName HValue,
        object_symtab :: FiniteMap String Addr
     }

data HValue = HValue -- fix this ... just temporary?


link :: PCI -> [[Linkable]] -> PLS -> IO LinkResult
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
   = LM Module [Unlinked]
   | LP PkgName

emptyPLS :: IO PLS
emptyPLS = return (error "emptyPLS:unimp")
\end{code}
