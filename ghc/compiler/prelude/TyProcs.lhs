%
% (c) The GRASP Project, Glasgow University, 1992
%
\section[TyProcessor]{The processor datatypes}

This is used only for ``Data Parallel Haskell.''

\begin{code}
#include "HsVersions.h"

module TyProcs where

import PrelFuns		-- help functions, types and things
import PrelUniqs

import AbsUniType	( applyTyCon, mkProcessorTyCon )
import Util

mkProcessorTy :: [UniType] -> UniType -> UniType
mkProcessorTy tys ty
 = applyTyCon (mkProcessorTyCon (length tys)) (tys++[ty])

processor1TyCon = mkProcessorTyCon (1::Int)
processor2TyCon = mkProcessorTyCon (2::Int)
processor3TyCon = mkProcessorTyCon (3::Int)
\end{code}
