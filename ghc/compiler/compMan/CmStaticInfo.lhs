%
% (c) The University of Glasgow, 2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( GhciMode(..), PackageConfig(..), defaultPackageConfig )
where

#include "HsVersions.h"

\end{code}

\begin{code}
data GhciMode = Batch | Interactive | OneShot 
     deriving Eq

#include "../utils/ghc-pkg/Package.hs"
\end{code}
