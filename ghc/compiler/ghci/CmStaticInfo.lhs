%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( FLAGS, PCI, 
                      mkSI, SI -- abstract
                    )
where

#include "HsVersions.h"

\end{code}

\begin{code}
type FLAGS = [String]       -- or some such fiction
type PCI = [PkgConfig]
data PkgConfig = PkgConfig   -- add details here

data SI = MkSI FLAGS PCI

mkSI :: FLAGS -> PCI -> SI
mkSI = MkSI


\end{code}
