%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[SAT]{Static Argument Transformation pass}
%*									*
%************************************************************************

96/03: We aren't using the static-argument transformation right now.

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
module SAT ( doStaticArgs ) where

#include "HsVersions.h"

import Panic	( panic )

doStaticArgs = panic "SAT.doStaticArgs (ToDo)"

{- LATER: to end of file:

import SATMonad
import Util
\end{code}

\begin{code}
doStaticArgs :: [CoreBind] -> UniqSupply -> [CoreBind]

doStaticArgs binds
  = do {
	showPass "Static argument";
	let { binds' = initSAT (mapSAT sat_bind binds) };
	endPass "Static argument" 
		False		-- No specific flag for dumping SAT
		binds'
    }
  where
    sat_bind (NonRec binder expr)
      = emptyEnvSAT  `thenSAT_`
	satExpr expr `thenSAT` (\ expr' ->
	returnSAT (NonRec binder expr') )
    sat_bind (Rec [(binder,rhs)])
      = emptyEnvSAT			  `thenSAT_`
	insSAEnv binder (getArgLists rhs) `thenSAT_`
	satExpr rhs			  `thenSAT` (\ rhs' ->
	saTransform binder rhs')
    sat_bind (Rec pairs)
      = emptyEnvSAT		`thenSAT_`
	mapSAT satExpr rhss	`thenSAT` \ rhss' ->
	returnSAT (Rec (zipEqual "doStaticArgs" binders rhss'))
      where
	(binders, rhss)	= unzip pairs
\end{code}

\begin{code}
satAtom (VarArg v)
  = updSAEnv (Just (v,([],[]))) `thenSAT_`
    returnSAT ()

satAtom _ = returnSAT ()
\end{code}

\begin{code}
satExpr :: CoreExpr -> SatM CoreExpr

satExpr var@(Var v)
  = updSAEnv (Just (v,([],[]))) `thenSAT_`
    returnSAT var

satExpr lit@(Lit _) = returnSAT lit

satExpr e@(Prim prim ty args)
  = mapSAT satAtom args	    `thenSAT_`
    returnSAT e

satExpr (Lam binders body)
  = satExpr body		`thenSAT` \ body' ->
    returnSAT (Lam binders body')

satExpr (CoTyLam tyvar body)
  = satExpr body	   `thenSAT` (\ body' ->
    returnSAT (CoTyLam tyvar body') )

satExpr app@(App _ _)
  = getAppArgs app

satExpr app@(CoTyApp _ _)
  = getAppArgs app

satExpr (Case expr alts)
  = satExpr expr	`thenSAT` \ expr' ->
    sat_alts alts	`thenSAT` \ alts' ->
    returnSAT (Case expr' alts')
  where
    sat_alts (AlgAlts alts deflt)
      = mapSAT satAlgAlt alts	    `thenSAT` \ alts' ->
	sat_default deflt	    `thenSAT` \ deflt' ->
	returnSAT (AlgAlts alts' deflt')
      where
	satAlgAlt (con, params, rhs)
	  = satExpr rhs		 `thenSAT` \ rhs' ->
	    returnSAT (con, params, rhs')

    sat_alts (PrimAlts alts deflt)
      = mapSAT satPrimAlt alts	    `thenSAT` \ alts' ->
	sat_default deflt	    `thenSAT` \ deflt' ->
	returnSAT (PrimAlts alts' deflt')
      where
	satPrimAlt (lit, rhs)
	  = satExpr rhs `thenSAT` \ rhs' ->
	    returnSAT (lit, rhs')

    sat_default NoDefault
      = returnSAT NoDefault
    sat_default (BindDefault binder rhs)
      = satExpr rhs		     `thenSAT` \ rhs' ->
	returnSAT (BindDefault binder rhs')

satExpr (Let (NonRec binder rhs) body)
  = satExpr body		`thenSAT` \ body' ->
    satExpr rhs			`thenSAT` \ rhs' ->
    returnSAT (Let (NonRec binder rhs') body')

satExpr (Let (Rec [(binder,rhs)]) body)
  = satExpr body		      `thenSAT` \ body' ->
    insSAEnv binder (getArgLists rhs) `thenSAT_`
    satExpr rhs			      `thenSAT` \ rhs' ->
    saTransform binder rhs'	      `thenSAT` \ binding ->
    returnSAT (Let binding body')

satExpr (Let (Rec binds) body)
  = let
	(binders, rhss) = unzip binds
    in
    satExpr body		    `thenSAT` \ body' ->
    mapSAT satExpr rhss		    `thenSAT` \ rhss' ->
    returnSAT (Let (Rec (zipEqual "satExpr:Rec" binders rhss')) body')

satExpr (Note note expr)
  = satExpr expr		    `thenSAT` \ expr2 ->
    returnSAT (Note note expr2)
\end{code}

\begin{code}
getAppArgs :: CoreExpr -> SatM CoreExpr

getAppArgs app
  = get app		`thenSAT` \ (app',result) ->
    updSAEnv result	`thenSAT_`
    returnSAT app'
  where
    get :: CoreExpr
	-> SatM (CoreExpr, Maybe (Id, SATInfo))

    get (CoTyApp e ty)
      = get e		`thenSAT` \ (e',result) ->
	returnSAT (
	  CoTyApp e' ty,
	  case result of
	    Nothing	     -> Nothing
	    Just (v,(tv,lv)) -> Just (v,(tv++[Static ty],lv))
	)

    get (App e a)
      = get e		`thenSAT` \ (e', result) ->
	satAtom a	`thenSAT_`
	let si = case a of
		   (VarArg v) -> Static v
		   _	         -> NotStatic
	in
	  returnSAT (
	    App e' a,
	    case result of
		Just (v,(tv,lv)) -> Just (v,(tv,lv++[si]))
		Nothing	    	 -> Nothing
	  )

    get var@(Var v)
      = returnSAT (var, Just (v,([],[])))

    get e
      = satExpr e	`thenSAT` \ e2 ->
	returnSAT (e2, Nothing)
-}
\end{code}
