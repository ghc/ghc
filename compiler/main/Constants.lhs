%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Constants]{Info about this compilation}

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Constants (module Constants) where

import Config

#include "ghc_boot_platform.h"

#include "../includes/HaskellConstants.hs"

hiVersion :: Integer
hiVersion = read (cProjectVersionInt ++ cProjectPatchLevel) :: Integer
\end{code}
