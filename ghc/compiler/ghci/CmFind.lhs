%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmFind]{Module finder for GHCI}

\begin{code}
module CmFind ( Path, ModName, PkgName,
                ModLocation(..), Finder, newFinder )
where

#include "HsVersions.h"

import Module		( Module )
import CmStaticInfo	( PCI )
\end{code}

\begin{code}
type Path = String
type ModName = String
type PkgName = String

data ModLocation 
   = SourceOnly Module Path        -- .hs
   | ObjectCode Module Path Path   -- .o, .hi
   | InPackage  Module PkgName

type Finder = ModName -> IO ModLocation

newFinder :: PCI -> IO Finder
newFinder pci = return (error "newFinder:unimp")
\end{code}
