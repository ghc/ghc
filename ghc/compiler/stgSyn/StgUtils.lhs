x%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[StgUtils]{Utility functions for @STG@ programs}

\begin{code}
#include "HsVersions.h"

module StgUtils ( mapStgBindeesRhs ) where

import StgSyn

import UniqSet

import Util
\end{code}

This utility function simply applies the given function to every
bindee in the program.

\begin{code}
mapStgBindeesBind :: (Id -> Id) -> StgBinding -> StgBinding

mapStgBindeesBind fn (StgNonRec b rhs) = StgNonRec b (mapStgBindeesRhs fn rhs)
mapStgBindeesBind fn (StgRec pairs)    = StgRec [ (b, mapStgBindeesRhs fn r) | (b, r) <- pairs ]

------------------
mapStgBindeesRhs :: (Id -> Id) -> StgRhs -> StgRhs

mapStgBindeesRhs fn (StgRhsClosure cc bi fvs u args expr)
  = StgRhsClosure
	cc bi
	(map fn fvs)
	u
	(map fn args)
	(mapStgBindeesExpr fn expr)

mapStgBindeesRhs fn (StgRhsCon cc con atoms)
  = StgRhsCon cc con (map (mapStgBindeesAtom fn) atoms)

------------------
mapStgBindeesExpr :: (Id -> Id) -> StgExpr -> StgExpr

mapStgBindeesExpr fn (StgApp f args lvs)
  = StgApp (mapStgBindeesAtom fn f)
	   (map (mapStgBindeesAtom fn) args)
	   (mapUniqSet fn lvs)

mapStgBindeesExpr fn (StgCon con atoms lvs)
  = StgCon con (map (mapStgBindeesAtom fn) atoms) (mapUniqSet fn lvs)

mapStgBindeesExpr fn (StgPrim op atoms lvs)
  = StgPrim op (map (mapStgBindeesAtom fn) atoms) (mapUniqSet fn lvs)

mapStgBindeesExpr fn (StgLet bind expr)
  = StgLet (mapStgBindeesBind fn bind) (mapStgBindeesExpr fn expr)

mapStgBindeesExpr fn (StgLetNoEscape lvs rhss_lvs bind body)
  = StgLetNoEscape (mapUniqSet fn lvs) (mapUniqSet fn rhss_lvs)
		   (mapStgBindeesBind fn bind) (mapStgBindeesExpr fn body)

mapStgBindeesExpr fn (StgSCC ty label expr)
  = StgSCC ty label (mapStgBindeesExpr fn expr)

mapStgBindeesExpr fn (StgCase expr lvs1 lvs2 uniq alts)
  = StgCase (mapStgBindeesExpr fn expr)
	    (mapUniqSet fn lvs1)
	    (mapUniqSet fn lvs2)
	    uniq
	    (mapStgBindeesAlts alts)
  where
    mapStgBindeesAlts (StgAlgAlts ty alts deflt)
      = StgAlgAlts ty (map mapStgBindeesBoxed_alt alts) (mapStgBindeesDeflt deflt)
      where
	mapStgBindeesBoxed_alt (c,ps,use_mask,expr) = (c,ps,use_mask,mapStgBindeesExpr fn expr)

    mapStgBindeesAlts (StgPrimAlts ty alts deflt)
      = StgPrimAlts ty (map mapStgBindeesunboxed_alt alts) (mapStgBindeesDeflt deflt)
      where
	mapStgBindeesunboxed_alt (l,expr) = (l,mapStgBindeesExpr fn expr)

    mapStgBindeesDeflt StgNoDefault		    = StgNoDefault
    mapStgBindeesDeflt (StgBindDefault b used expr) = StgBindDefault b used (mapStgBindeesExpr fn expr)

------------------
mapStgBindeesAtom :: (Id -> Id) -> StgArg -> StgArg

mapStgBindeesAtom fn a@(StgLitArg _)	= a
mapStgBindeesAtom fn a@(StgVarArg id)  = StgVarArg (fn id)
\end{code}
