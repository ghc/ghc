%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%************************************************************************
%*									*
\section[SAT]{Static Argument Transformation pass}
%*									*
%************************************************************************

May be seen as removing invariants from loops:
Arguments of recursive functions that do not change in recursive
calls are removed from the recursion, which is done locally
and only passes the arguments which effectively change.

Example:
map = /\ ab -> \f -> \xs -> case xs of
			     []	   -> []
			     (a:b) -> f a : map f b

as map is recursively called with the same argument f (unmodified)
we transform it to

map = /\ ab -> \f -> \xs -> let map' ys = case ys of
					   []	 -> []
					   (a:b) -> f a : map' b
			    in map' xs

Notice that for a compiler that uses lambda lifting this is
useless as map' will be transformed back to what map was.

\begin{code}
#include "HsVersions.h"

module StgSAT (
	doStaticArgs,

	-- and to make the interface self-sufficient...
	PlainStgProgram(..), StgExpr, StgBinding, Id
    ) where

import IdEnv
import Maybes		( Maybe(..) )
import StgSyn
import SATMonad		( SATEnv(..), SATInfo(..), Arg(..), updSAEnv, insSAEnv,
                          SatM(..), initSAT, thenSAT, thenSAT_,
                          emptyEnvSAT, returnSAT, mapSAT )
import StgSATMonad
import SplitUniq
import Util
\end{code}

\begin{code}
doStaticArgs :: PlainStgProgram -> SplitUniqSupply -> PlainStgProgram

doStaticArgs binds
  = initSAT (mapSAT sat_bind binds)
  where
    sat_bind (StgNonRec binder expr)
      = emptyEnvSAT  `thenSAT_`
	satRhs expr `thenSAT` (\ expr' ->
	returnSAT (StgNonRec binder expr'))
    sat_bind (StgRec [(binder,rhs)])
      = emptyEnvSAT			  `thenSAT_`
	insSAEnv binder (getArgLists rhs) `thenSAT_`
	satRhs rhs			  `thenSAT` (\ rhs' ->
	saTransform binder rhs')
    sat_bind (StgRec pairs)
      = emptyEnvSAT		`thenSAT_`
	mapSAT satRhs rhss	`thenSAT` \ rhss' ->
	returnSAT (StgRec (binders `zip` rhss'))
      where
	(binders, rhss)	= unzip pairs
\end{code}

\begin{code}
satAtom (StgVarAtom v)
  = updSAEnv (Just (v,([],[]))) `thenSAT_`
    returnSAT ()

satAtom _ = returnSAT ()
\end{code}

\begin{code}
satExpr :: PlainStgExpr -> SatM PlainStgExpr

satExpr e@(StgConApp con args lvs)
  = mapSAT satAtom args	    `thenSAT_`
    returnSAT e

satExpr e@(StgPrimApp op args lvs)
  = mapSAT satAtom args	    `thenSAT_`
    returnSAT e

satExpr e@(StgApp (StgLitAtom _) _ _)
  = returnSAT e

satExpr e@(StgApp (StgVarAtom v) args _)
  = updSAEnv (Just (v,([],map tagArg args)))	`thenSAT_`
    mapSAT satAtom args				`thenSAT_`
    returnSAT e
  where 
    tagArg (StgVarAtom v) = Static v
    tagArg _              = NotStatic
    
satExpr (StgCase expr lv1 lv2 uniq alts)
  = satExpr expr	`thenSAT` \ expr' ->
    sat_alts alts	`thenSAT` \ alts' ->
    returnSAT (StgCase expr' lv1 lv2 uniq alts')
  where
    sat_alts (StgAlgAlts ty alts deflt)
      = mapSAT satAlgAlt alts	    `thenSAT` \ alts' ->
	sat_default deflt	    `thenSAT` \ deflt' ->
	returnSAT (StgAlgAlts ty alts' deflt')
      where
	satAlgAlt (con, params, use_mask, rhs)
	  = satExpr rhs		 `thenSAT` \ rhs' ->
	    returnSAT (con, params, use_mask, rhs')

    sat_alts (StgPrimAlts ty alts deflt)
      = mapSAT satPrimAlt alts	    `thenSAT` \ alts' ->
	sat_default deflt	    `thenSAT` \ deflt' ->
	returnSAT (StgPrimAlts ty alts' deflt')
      where
	satPrimAlt (lit, rhs)
	  = satExpr rhs `thenSAT` \ rhs' ->
	    returnSAT (lit, rhs')

    sat_default StgNoDefault
      = returnSAT StgNoDefault
    sat_default (StgBindDefault binder used rhs)
      = satExpr rhs		     `thenSAT` \ rhs' ->
	returnSAT (StgBindDefault binder used rhs')

satExpr (StgLetNoEscape lv1 lv2 (StgNonRec binder rhs) body)
  = satExpr body		`thenSAT` \ body' ->
    satRhs rhs			`thenSAT` \ rhs' ->
    returnSAT (StgLetNoEscape lv1 lv2 (StgNonRec binder rhs') body')

satExpr (StgLetNoEscape lv1 lv2 (StgRec [(binder,rhs)]) body)
  = satExpr body		      `thenSAT` \ body' ->
    insSAEnv binder (getArgLists rhs) `thenSAT_`
    satRhs rhs			      `thenSAT` \ rhs' ->
    saTransform binder rhs'	      `thenSAT` \ binding ->
    returnSAT (StgLetNoEscape lv1 lv2 binding body')

satExpr (StgLetNoEscape lv1 lv2 (StgRec binds) body)
  = let (binders, rhss) = unzip binds
    in
    satExpr body		    `thenSAT` \ body' ->
    mapSAT satRhs rhss		    `thenSAT` \ rhss' ->
    returnSAT (StgLetNoEscape lv1 lv2 (StgRec (binders `zip` rhss')) body')

satExpr (StgLet (StgNonRec binder rhs) body)
  = satExpr body		`thenSAT` \ body' ->
    satRhs rhs			`thenSAT` \ rhs' ->
    returnSAT (StgLet (StgNonRec binder rhs') body')

satExpr (StgLet (StgRec [(binder,rhs)]) body)
  = satExpr body		      `thenSAT` \ body' ->
    insSAEnv binder (getArgLists rhs) `thenSAT_`
    satRhs rhs			      `thenSAT` \ rhs' ->
    saTransform binder rhs'	      `thenSAT` \ binding ->
    returnSAT (StgLet binding body')

satExpr (StgLet (StgRec binds) body)
  = let (binders, rhss) = unzip binds
    in
    satExpr body		    `thenSAT` \ body' ->
    mapSAT satRhs rhss		    `thenSAT` \ rhss' ->
    returnSAT (StgLet (StgRec (binders `zip` rhss')) body')

satExpr (StgSCC ty cc expr)
  = satExpr expr		    `thenSAT` \ expr' ->
    returnSAT (StgSCC ty cc expr')

-- ToDo: DPH stuff
\end{code}

\begin{code}
satRhs rhs@(StgRhsCon cc v args) = returnSAT rhs
satRhs (StgRhsClosure cc bi fvs upd args body) 
  = satExpr body		`thenSAT` \ body' ->
    returnSAT (StgRhsClosure cc bi fvs upd args body')

\end{code}

