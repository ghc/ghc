%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
module FreeVars (
	-- Cheap and cheerful variant...
	exprFreeVars, exprFreeTyVars,

	-- Complicated and expensive variant for float-out
	freeVars,
	freeVarsOf, freeTyVarsOf,
	CoreExprWithFVs,		-- For the above functions
	AnnCoreExpr,			-- Dito
	FVInfo(..), LeakInfo(..)
    ) where

#include "HsVersions.h"

import AnnCoreSyn	-- output

import CoreSyn
import CoreUtils	( idSpecVars )
import Id		( idType, getIdArity, isBottomingId,
			  emptyIdSet, unitIdSet, mkIdSet, unionIdSets,
			  elementOfIdSet, minusIdSet, unionManyIdSets,
			  IdSet, Id
			)
import IdInfo		( ArityInfo(..) )
import PrimOp		( PrimOp(CCallOp) )
import Type		( tyVarsOfType, Type )
import TyVar		( emptyTyVarSet, unitTyVarSet, minusTyVarSet,
			  intersectTyVarSets, unionManyTyVarSets,
			  TyVarSet, TyVar
			)
import BasicTypes	( Unused )

import UniqSet		( unionUniqSets, addOneToUniqSet, delOneFromUniqSet )
import Util		( panic, assertPanic )

\end{code}

%************************************************************************
%*									*
\section[freevars-everywhere]{Attaching free variables to every sub-expression
%*									*
%************************************************************************

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.

The ``free type variables'' are defined to be those which are mentioned
in type applications, {\em not} ones which lie buried in the types of Ids.

*** ALAS: we *do* need to collect tyvars from lambda-bound ids. ***
I've half-convinced myself we don't for case- and letrec bound ids
but I might be wrong. (SLPJ, date unknown)

\begin{code}
type CoreExprWithFVs =  AnnCoreExpr Id Id Unused FVInfo

type TyVarCands = TyVarSet  -- for when we carry around lists of
type IdCands	= IdSet	    -- "candidate" TyVars/Ids.
noTyVarCands    = emptyTyVarSet
noIdCands       = emptyIdSet

data FVInfo
  = FVInfo  IdSet	-- Free ids
	    TyVarSet    -- Free tyvars
	    LeakInfo

noFreeIds      = emptyIdSet
noFreeTyVars   = emptyTyVarSet
noFreeAnything = (noFreeIds, noFreeTyVars)
aFreeId i      = unitIdSet i
aFreeTyVar t   = unitTyVarSet t
is_among       = elementOfIdSet
munge_id_ty  i = tyVarsOfType (idType i)
combine	       = unionUniqSets -- used both for {Id,TyVar}Sets
without	       = delOneFromUniqSet
add	       = addOneToUniqSet

combineFVInfo (FVInfo fvs1 tfvs1 leak1) (FVInfo fvs2 tfvs2 leak2)
  = FVInfo (fvs1  `combine` fvs2)
	   (tfvs1 `combine` tfvs2)
	   (leak1 `orLeak`  leak2)
\end{code}

Leak-free-ness is based only on the value, not the type.  In
particular, nested collections of constructors are guaranteed leak
free.  Function applications are not, except for PAPs.

Applications of error gets (LeakFree bigArity) -- a hack!

\begin{code}
data LeakInfo
  = MightLeak
  | LeakFree Int    -- Leak free, and guarantees to absorb this # of
		    -- args before becoming leaky.

lEAK_FREE_0   = LeakFree 0
lEAK_FREE_BIG = LeakFree bigArity
	      where
		bigArity = 1000::Int	-- NB: arbitrary

orLeak :: LeakInfo -> LeakInfo -> LeakInfo
orLeak MightLeak     _           = MightLeak
orLeak _             MightLeak   = MightLeak
orLeak (LeakFree n) (LeakFree m) = LeakFree (n `min` m)
\end{code}

Main public interface:
\begin{code}
freeVars :: CoreExpr -> CoreExprWithFVs

freeVars expr = fvExpr noIdCands noTyVarCands expr

\end{code}

%************************************************************************
%*									*
\subsection{Free variables (and types)}
%*									*
%************************************************************************

We do the free-variable stuff by passing around ``candidates lists''
of @Ids@ and @TyVars@ that may be considered free.  This is useful,
e.g., to avoid considering top-level binders as free variables---don't
put them on the candidates list.

\begin{code}

fvExpr :: IdCands	    -- In-scope Ids
       -> TyVarCands	    -- In-scope tyvars
       -> CoreExpr
       -> CoreExprWithFVs

fvExpr id_cands tyvar_cands (Var v)
  = (FVInfo fvs noFreeTyVars leakiness, AnnVar v)
  where
    {-
     ToDo: insert motivating example for why we *need*
     to include the idSpecVars in the FV list.
    -}
    fvs = fvs_v `unionIdSets` mkIdSet (idSpecVars v)

    fvs_v
     | v `is_among` id_cands = aFreeId v
     | otherwise	     = noFreeIds
     
    leakiness
      | isBottomingId v = lEAK_FREE_BIG	-- Hack
      | otherwise       = case getIdArity v of
			    UnknownArity       -> lEAK_FREE_0
			    ArityAtLeast arity -> LeakFree arity
			    ArityExactly arity -> LeakFree arity

fvExpr id_cands tyvar_cands (Lit k)
  = (FVInfo noFreeIds noFreeTyVars lEAK_FREE_0, AnnLit k)

fvExpr id_cands tyvar_cands (Con c args)
  = (FVInfo args_fvs tfvs lEAK_FREE_0, AnnCon c args)
  where
    (args_fvs, tfvs) = freeArgs id_cands tyvar_cands args

fvExpr id_cands tyvar_cands (Prim op args)
  = (FVInfo args_fvs tfvs lEAK_FREE_0, AnnPrim op args)
  where
    (args_fvs, tfvs) = freeArgs id_cands tyvar_cands args_to_use{-NB-}
    args_to_use
      = case op of
	  CCallOp _ _ _ _ _ res_ty -> TyArg res_ty : args
	  _			   -> args

-- this Lam stuff could probably be improved by rewriting (WDP 96/03)

fvExpr id_cands tyvar_cands (Lam b@(ValBinder binder) body)
  = (FVInfo (freeVarsOf body2   `minusIdSet` unitIdSet binder)
	    (freeTyVarsOf body2 `combine`    munge_id_ty binder)
	    leakiness,
     AnnLam b body2)
  where
	-- We need to collect free tyvars from the binders
    body2 = fvExpr (unitIdSet binder `combine` id_cands) tyvar_cands body

    leakiness = case leakinessOf body2 of
		  MightLeak  -> LeakFree 1
		  LeakFree n -> LeakFree (n + 1)

fvExpr id_cands tyvar_cands (Lam b@(TyBinder tyvar) body)
  = (FVInfo (freeVarsOf body2)
	    (freeTyVarsOf body2 `minusTyVarSet` aFreeTyVar tyvar)
	    (leakinessOf body2),
     AnnLam b body2)
  where
    body2 = fvExpr id_cands (aFreeTyVar tyvar `combine` tyvar_cands) body

-- ditto on rewriting this App stuff (WDP 96/03)

fvExpr id_cands tyvar_cands (App fun arg)
  = (FVInfo (freeVarsOf fun2   `combine` fvs_arg)
	    (freeTyVarsOf fun2 `combine` tfvs_arg)
	    leakiness,
     AnnApp fun2 arg)
  where
    fun2 = fvExpr id_cands tyvar_cands fun
    fun2_leakiness = leakinessOf fun2

    (fvs_arg, tfvs_arg) = freeArgs id_cands tyvar_cands [arg]

    leakiness = if (notValArg arg) then
		    fun2_leakiness
		else
		    case fun2_leakiness of
		       LeakFree n | n>1 -> LeakFree (n-1) -- Note > not >=
		       other            -> MightLeak

fvExpr id_cands tyvar_cands (Case expr alts)
  = (combineFVInfo expr_fvinfo alts_fvinfo,
     AnnCase expr2 alts')
  where
    expr2@(expr_fvinfo,_) = fvExpr id_cands tyvar_cands expr
    (alts_fvinfo, alts') = annotate_alts alts

    annotate_alts (AlgAlts alts deflt)
      = (fvinfo, AnnAlgAlts alts' deflt')
      where
    	(alts_fvinfo_s, alts') = unzip (map ann_boxed_alt alts)
    	(deflt_fvinfo, deflt') = annotate_default deflt
	fvinfo = foldr combineFVInfo deflt_fvinfo alts_fvinfo_s

	ann_boxed_alt (con, params, rhs)
	  = (FVInfo (freeVarsOf rhs' `minusIdSet` mkIdSet params)
		    (freeTyVarsOf rhs' `combine` param_ftvs)
		    (leakinessOf rhs'),
	     (con, params, rhs'))
    	  where
	    rhs' = fvExpr (mkIdSet params `combine` id_cands) tyvar_cands rhs
	    param_ftvs = foldr (combine . munge_id_ty) noFreeTyVars params
		-- We need to collect free tyvars from the binders

    annotate_alts (PrimAlts alts deflt)
      = (fvinfo, AnnPrimAlts alts' deflt')
      where
    	(alts_fvinfo_s, alts') = unzip (map ann_unboxed_alt alts)
    	(deflt_fvinfo, deflt') = annotate_default deflt
	fvinfo  = foldr combineFVInfo deflt_fvinfo alts_fvinfo_s

	ann_unboxed_alt (lit, rhs) = (rhs_info, (lit, rhs'))
    	  where
	    rhs'@(rhs_info,_) = fvExpr id_cands tyvar_cands rhs

    annotate_default NoDefault = (FVInfo noFreeIds noFreeTyVars lEAK_FREE_BIG,
				    AnnNoDefault)

    annotate_default (BindDefault binder rhs)
      = (FVInfo (freeVarsOf   rhs' `minusIdSet` aFreeId binder)
		(freeTyVarsOf rhs' `combine` binder_ftvs)
		(leakinessOf rhs'),
	 AnnBindDefault binder rhs')
      where
	rhs' = fvExpr (aFreeId binder `combine` id_cands) tyvar_cands rhs
	binder_ftvs = munge_id_ty binder
	    -- We need to collect free tyvars from the binder

-- Don't forget to notice that the idSpecVars of the binder
-- are free in the whole expression; albeit not in the RHS or body

fvExpr id_cands tyvar_cands (Let (NonRec binder rhs) body)
  = (FVInfo (freeVarsOf rhs'   `combine` body_fvs `combine` mkIdSet (idSpecVars binder))
	    (freeTyVarsOf rhs' `combine` freeTyVarsOf body2 `combine` binder_ftvs)
	    (leakinessOf rhs' `orLeak` leakinessOf body2),
     AnnLet (AnnNonRec binder rhs') body2)
  where
    rhs'	= fvRhs id_cands tyvar_cands (binder, rhs)
    body2	= fvExpr (aFreeId binder `combine` id_cands) tyvar_cands body
    body_fvs	= freeVarsOf body2 `minusIdSet` aFreeId binder
    binder_ftvs = munge_id_ty binder
	-- We need to collect free tyvars from the binder

fvExpr id_cands tyvar_cands (Let (Rec binds) body)
  = (FVInfo (binds_fvs `combine` body_fvs)
	    (rhss_tfvs `combine` freeTyVarsOf body2 `combine` binders_ftvs)
	    (leakiness_of_rhss `orLeak` leakinessOf body2),
     AnnLet (AnnRec (binders `zip` rhss')) body2)
  where
    (binders, rhss)   = unzip binds
    new_id_cands      = binders_set `combine` id_cands
    binders_set	      = mkIdSet binders
    rhss'	      = map (fvRhs new_id_cands tyvar_cands) binds

    FVInfo rhss_fvs rhss_tfvs leakiness_of_rhss
	= foldr1 combineFVInfo [info | (info,_) <- rhss']

	-- Don't forget to notice that the idSpecVars of the binder
	-- are free in the whole expression; albeit not in the RHS or body
    binds_fvs	      = (foldr (unionIdSets . mkIdSet . idSpecVars) rhss_fvs binders)
			`minusIdSet`
			binders_set

    body2	      = fvExpr new_id_cands tyvar_cands body
    body_fvs	      = freeVarsOf body2 `minusIdSet` binders_set
    binders_ftvs      = foldr (combine . munge_id_ty) noFreeTyVars binders
	-- We need to collect free tyvars from the binders

fvExpr id_cands tyvar_cands (Note (Coerce to_ty from_ty) expr)
  = (FVInfo (freeVarsOf   expr2)
	    (freeTyVarsOf expr2 `combine` tfvs1 `combine` tfvs2)
	    (leakinessOf  expr2),
     AnnNote (Coerce to_ty from_ty) expr2)
  where
    expr2 = fvExpr id_cands tyvar_cands expr
    tfvs1  = freeTy tyvar_cands from_ty
    tfvs2  = freeTy tyvar_cands to_ty

fvExpr id_cands tyvar_cands (Note other_note expr)
  = (fvinfo, AnnNote other_note expr2)
  where
    expr2@(fvinfo,_) = fvExpr id_cands tyvar_cands expr

fvRhs id_cands tyvar_cands (bndr,rhs)
  = fvExpr id_cands tyvar_cands rhs
\end{code}

\begin{code}
freeArgs :: IdCands -> TyVarCands
	 -> [CoreArg]
	 -> (IdSet, TyVarSet)

freeArgs icands tcands [] = noFreeAnything
freeArgs icands tcands (arg:args)
  -- this code is written this funny way only for "efficiency" purposes
  = let
	free_first_arg@(arg_fvs, tfvs) = free_arg arg
    in
    if (null args) then
	free_first_arg
    else
	case (freeArgs icands tcands args) of { (irest, trest) ->
	(arg_fvs `combine` irest, tfvs `combine` trest) }
  where
    free_arg (LitArg   _)   = noFreeAnything
    free_arg (TyArg   ty)   = (noFreeIds, freeTy tcands ty)
    free_arg (VarArg   v)
      | v `is_among` icands = (aFreeId v, noFreeTyVars)
      | otherwise	    = noFreeAnything

---------
freeTy :: TyVarCands -> Type -> TyVarSet

freeTy cands ty = tyVarsOfType ty `intersectTyVarSets` cands

freeVarsOf :: CoreExprWithFVs -> IdSet
freeVarsOf (FVInfo free_vars _ _, _) = free_vars

freeTyVarsOf :: CoreExprWithFVs -> TyVarSet
freeTyVarsOf (FVInfo _ free_tyvars _, _) = free_tyvars

leakinessOf :: CoreExprWithFVs -> LeakInfo
leakinessOf (FVInfo _ _ leakiness, _) = leakiness
\end{code}


%************************************************************************
%*									*
\section{Finding the free variables of an expression}
%*									*
%************************************************************************

This function simply finds the free variables of an expression.

\begin{code}
type InterestingIdFun
  =  Id		-- The Id being looked at
  -> Bool	-- True <=> interesting

exprFreeVars :: InterestingIdFun -> CoreExpr -> IdSet
exprFreeVars fv_cand e = expr_fvs fv_cand emptyIdSet e
\end{code}


\begin{code}
expr_fvs :: InterestingIdFun	-- "Interesting id" predicate
	 -> IdSet		-- In scope ids
	 -> CoreExpr
	 -> IdSet

expr_fvs fv_cand in_scope (Var v)        = id_fvs fv_cand in_scope v
expr_fvs fv_cand in_scope (Lit lit)      = noFreeIds
expr_fvs fv_cand in_scope (Con con args) = args_fvs fv_cand in_scope args
expr_fvs fv_cand in_scope (Prim op args) = args_fvs fv_cand in_scope args
expr_fvs fv_cand in_scope (Note _ expr)  = expr_fvs fv_cand in_scope expr
expr_fvs fv_cand in_scope (App fun arg)  = expr_fvs fv_cand in_scope fun `combine`
					   arg_fvs fv_cand in_scope arg


expr_fvs fv_cand in_scope (Lam (ValBinder b) body)
  = (expr_fvs fv_cand (in_scope `add` b) body)
expr_fvs fv_cand in_scope (Lam (TyBinder b) body)
  = expr_fvs fv_cand in_scope body

expr_fvs fv_cand in_scope (Case scrut alts)
  = expr_fvs fv_cand in_scope scrut `combine` alts_fvs
  where
    alts_fvs
      = case alts of
	  AlgAlts alg_alts deflt -> unionManyIdSets (deflt_fvs : alt_fvs)
	    where
	      alt_fvs   = map do_alg_alt alg_alts
	      deflt_fvs = do_deflt deflt

	  PrimAlts prim_alts deflt -> unionManyIdSets (deflt_fvs : alt_fvs)
	    where
	      alt_fvs   = map do_prim_alt prim_alts
	      deflt_fvs = do_deflt deflt

    do_alg_alt :: (Id, [Id], CoreExpr) -> IdSet
    do_alg_alt (con, args, rhs) = expr_fvs fv_cand new_in_scope rhs
      where
	new_in_scope = in_scope `combine` mkIdSet args

    do_prim_alt (lit, rhs) = expr_fvs fv_cand in_scope rhs

    do_deflt NoDefault		 = noFreeIds
    do_deflt (BindDefault b rhs) = expr_fvs fv_cand (in_scope `add` b) rhs

expr_fvs fv_cand in_scope (Let (NonRec b r) body)
  = expr_fvs fv_cand in_scope r `combine`
    expr_fvs fv_cand (in_scope `add` b) body

expr_fvs fv_cand in_scope (Let (Rec pairs) body)
  = foldr (combine . expr_fvs fv_cand in_scope' . snd) noFreeIds pairs `combine`
    expr_fvs fv_cand in_scope' body
  where
    in_scope' = in_scope `combine` mkIdSet (map fst pairs)




--------------------------------------
arg_fvs fv_cand in_scope (VarArg v) = id_fvs fv_cand in_scope v
arg_fvs fv_cand in_scope other_arg  = noFreeIds

--------------------------------------
args_fvs fv_cand in_scope args = foldr (combine . arg_fvs fv_cand in_scope) noFreeIds args


--------------------------------------
id_fvs fv_cand in_scope v
  | v `elementOfIdSet` in_scope = noFreeIds
  | fv_cand v		        = aFreeId v
  | otherwise			= noFreeIds
\end{code}


\begin{code}
exprFreeTyVars ::  CoreExpr -> TyVarSet
exprFreeTyVars = expr_ftvs

expr_ftvs :: CoreExpr -> TyVarSet
expr_ftvs (Var v)        = noFreeTyVars
expr_ftvs (Lit lit)      = noFreeTyVars
expr_ftvs (Con con args) = args_ftvs args
expr_ftvs (Prim op args) = args_ftvs args
expr_ftvs (Note _ expr)  = expr_ftvs expr
expr_ftvs (App fun arg)  = expr_ftvs fun `combine` arg_ftvs arg

expr_ftvs (Lam (ValBinder b) body) = expr_ftvs body
expr_ftvs (Lam (TyBinder b)  body) = expr_ftvs body `without` b

expr_ftvs (Case scrut alts)
  = expr_ftvs scrut `combine` alts_ftvs
  where
    alts_ftvs
      = case alts of
	  AlgAlts alg_alts deflt -> unionManyTyVarSets (deflt_ftvs : alt_ftvs)
	    where
	      alt_ftvs   = map do_alg_alt alg_alts
	      deflt_ftvs = do_deflt deflt

	  PrimAlts prim_alts deflt -> unionManyTyVarSets (deflt_ftvs : alt_ftvs)
	    where
	      alt_ftvs   = map do_prim_alt prim_alts
	      deflt_ftvs = do_deflt deflt

    do_alg_alt :: (Id, [Id], CoreExpr) -> TyVarSet
    do_alg_alt (con, args, rhs) = expr_ftvs rhs

    do_prim_alt (lit, rhs) = expr_ftvs rhs

    do_deflt NoDefault		 = noFreeTyVars
    do_deflt (BindDefault b rhs) = expr_ftvs rhs

expr_ftvs (Let (NonRec b r) body)
  = bind_ftvs (b,r) `combine` expr_ftvs body

expr_ftvs (Let (Rec pairs) body)
  = foldr (combine . bind_ftvs) noFreeTyVars pairs `combine`
    expr_ftvs body

--------------------------------------
bind_ftvs (b,e) = tyVarsOfType (idType b) `combine` expr_ftvs e

--------------------------------------
arg_ftvs (TyArg ty) = tyVarsOfType ty
arg_ftvs other_arg  = noFreeTyVars

--------------------------------------
args_ftvs args = foldr (combine . arg_ftvs) noFreeTyVars args
\end{code}
