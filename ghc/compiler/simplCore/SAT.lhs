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

We could possibly do the same for big lambdas, but we don't as
they will eventually be removed in later stages of the compiler,
therefore there is no penalty in keeping them.

Experimental Evidence: Heap: +/- 7%
                       Instrs: Always improves for 2 or more Static Args.

\begin{code}
#include "HsVersions.h"

module SAT (
	doStaticArgs,

	-- and to make the interface self-sufficient...
	PlainCoreProgram(..), CoreExpr, CoreBinding, Id
    ) where

import IdEnv
import Maybes		( Maybe(..) )
import PlainCore
import SATMonad
import SplitUniq
import Util
\end{code}

\begin{code}
doStaticArgs :: PlainCoreProgram -> SplitUniqSupply -> PlainCoreProgram

doStaticArgs binds
  = initSAT (mapSAT sat_bind binds)
  where
    sat_bind (CoNonRec binder expr)
      = emptyEnvSAT  `thenSAT_`
	satExpr expr `thenSAT` (\ expr' ->
	returnSAT (CoNonRec binder expr') )
    sat_bind (CoRec [(binder,rhs)])
      = emptyEnvSAT			  `thenSAT_`
	insSAEnv binder (getArgLists rhs) `thenSAT_`
	satExpr rhs			  `thenSAT` (\ rhs' ->
	saTransform binder rhs')
    sat_bind (CoRec pairs)
      = emptyEnvSAT		`thenSAT_`
	mapSAT satExpr rhss	`thenSAT` \ rhss' ->
	returnSAT (CoRec (binders `zip` rhss'))
      where
	(binders, rhss)	= unzip pairs
\end{code}

\begin{code}
satAtom (CoVarAtom v)
  = updSAEnv (Just (v,([],[]))) `thenSAT_`
    returnSAT ()

satAtom _ = returnSAT ()
\end{code}

\begin{code}
satExpr :: PlainCoreExpr -> SatM PlainCoreExpr

satExpr var@(CoVar v)
  = updSAEnv (Just (v,([],[]))) `thenSAT_`
    returnSAT var

satExpr lit@(CoLit _) = returnSAT lit

satExpr e@(CoCon con types args)
  = mapSAT satAtom args	    `thenSAT_`
    returnSAT e

satExpr e@(CoPrim prim ty args)
  = mapSAT satAtom args	    `thenSAT_`
    returnSAT e

satExpr (CoLam binders body)
  = satExpr body		`thenSAT` \ body' ->
    returnSAT (CoLam binders body')

satExpr (CoTyLam tyvar body)
  = satExpr body	   `thenSAT` (\ body' ->
    returnSAT (CoTyLam tyvar body') )

satExpr app@(CoApp _ _)
  = getAppArgs app

satExpr app@(CoTyApp _ _)
  = getAppArgs app

satExpr (CoCase expr alts)
  = satExpr expr	`thenSAT` \ expr' ->
    sat_alts alts	`thenSAT` \ alts' ->
    returnSAT (CoCase expr' alts')
  where
    sat_alts (CoAlgAlts alts deflt)
      = mapSAT satAlgAlt alts	    `thenSAT` \ alts' ->
	sat_default deflt	    `thenSAT` \ deflt' ->
	returnSAT (CoAlgAlts alts' deflt')
      where
	satAlgAlt (con, params, rhs)
	  = satExpr rhs		 `thenSAT` \ rhs' ->
	    returnSAT (con, params, rhs')

    sat_alts (CoPrimAlts alts deflt)
      = mapSAT satPrimAlt alts	    `thenSAT` \ alts' ->
	sat_default deflt	    `thenSAT` \ deflt' ->
	returnSAT (CoPrimAlts alts' deflt')
      where
	satPrimAlt (lit, rhs)
	  = satExpr rhs `thenSAT` \ rhs' ->
	    returnSAT (lit, rhs')

    sat_default CoNoDefault
      = returnSAT CoNoDefault
    sat_default (CoBindDefault binder rhs)
      = satExpr rhs		     `thenSAT` \ rhs' ->
	returnSAT (CoBindDefault binder rhs')

satExpr (CoLet (CoNonRec binder rhs) body)
  = satExpr body		`thenSAT` \ body' ->
    satExpr rhs			`thenSAT` \ rhs' ->
    returnSAT (CoLet (CoNonRec binder rhs') body')

satExpr (CoLet (CoRec [(binder,rhs)]) body)
  = satExpr body		      `thenSAT` \ body' ->
    insSAEnv binder (getArgLists rhs) `thenSAT_`
    satExpr rhs			      `thenSAT` \ rhs' ->
    saTransform binder rhs'	      `thenSAT` \ binding ->
    returnSAT (CoLet binding body')

satExpr (CoLet (CoRec binds) body)
  = let
	(binders, rhss) = unzip binds
    in
    satExpr body		    `thenSAT` \ body' ->
    mapSAT satExpr rhss		    `thenSAT` \ rhss' ->
    returnSAT (CoLet (CoRec (binders `zip` rhss')) body')

satExpr (CoSCC cc expr)
  = satExpr expr		    `thenSAT` \ expr2 ->
    returnSAT (CoSCC cc expr2)

-- ToDo: DPH stuff
\end{code}

\begin{code}
getAppArgs :: PlainCoreExpr -> SatM PlainCoreExpr

getAppArgs app
  = get app		`thenSAT` \ (app',result) ->
    updSAEnv result	`thenSAT_`
    returnSAT app'
  where
    get :: PlainCoreExpr
	-> SatM (PlainCoreExpr, Maybe (Id, SATInfo))

    get (CoTyApp e ty)
      = get e		`thenSAT` \ (e',result) ->
	returnSAT (
	  CoTyApp e' ty,
	  case result of
	    Nothing	     -> Nothing
	    Just (v,(tv,lv)) -> Just (v,(tv++[Static ty],lv))
	)

    get (CoApp e a)
      = get e		`thenSAT` \ (e', result) ->
	satAtom a	`thenSAT_`
	let si = case a of
		   (CoVarAtom v) -> Static v
		   _	         -> NotStatic
	in
	  returnSAT (
	    CoApp e' a,
	    case result of
		Just (v,(tv,lv)) -> Just (v,(tv,lv++[si]))
		Nothing	    	 -> Nothing
	  )

    get var@(CoVar v)
      = returnSAT (var, Just (v,([],[])))

    get e
      = satExpr e	`thenSAT` \ e2 ->
	returnSAT (e2, Nothing)
\end{code}

