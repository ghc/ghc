%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
#include "HsVersions.h"

module FreeVars (
	freeVars,

	-- cheap and cheerful variant...
	addTopBindsFVs, addExprFVs,

	freeVarsOf, freeTyVarsOf,
	SYN_IE(FVCoreExpr), SYN_IE(FVCoreBinding),

	SYN_IE(CoreExprWithFVs),		-- For the above functions
	SYN_IE(AnnCoreExpr),		-- Dito
	FVInfo(..), LeakInfo(..)
    ) where

IMP_Ubiq(){-uitous-}

import AnnCoreSyn	-- output

import CoreSyn
import Id		( idType, getIdArity, isBottomingId,
			  emptyIdSet, unitIdSet, mkIdSet,
			  elementOfIdSet, minusIdSet, unionManyIdSets,
			  SYN_IE(IdSet)
			)
import IdInfo		( ArityInfo(..) )
import PrimOp		( PrimOp(..) )
import Type		( tyVarsOfType )
import TyVar		( emptyTyVarSet, unitTyVarSet, minusTyVarSet,
			  intersectTyVarSets,
			  SYN_IE(TyVarSet)
			)
import UniqSet		( unionUniqSets )
import Usage		( SYN_IE(UVar) )
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
type CoreExprWithFVs =  AnnCoreExpr Id Id TyVar UVar FVInfo

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
  = (FVInfo (if (v `is_among` id_cands)
	     then aFreeId v
	     else noFreeIds)
	    noFreeTyVars
	    leakiness,
     AnnVar v)
  where
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
	  CCallOp _ _ _ _ res_ty -> TyArg res_ty : args
	  _			 -> args

-- this Lam stuff could probably be improved by rewriting (WDP 96/03)

fvExpr id_cands tyvar_cands (Lam (UsageBinder uvar) body)
  = panic "fvExpr:Lam UsageBinder"

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

fvExpr id_cands tyvar_cands (Let (NonRec binder rhs) body)
  = (FVInfo (freeVarsOf rhs'   `combine` body_fvs)
	    (freeTyVarsOf rhs' `combine` freeTyVarsOf body2 `combine` binder_ftvs)
	    (leakinessOf rhs' `orLeak` leakinessOf body2),
     AnnLet (AnnNonRec binder rhs') body2)
  where
    rhs'	= fvExpr id_cands tyvar_cands rhs
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
    rhss'	      = map (fvExpr new_id_cands tyvar_cands) rhss

    FVInfo rhss_fvs rhss_tfvs leakiness_of_rhss
	= foldr1 combineFVInfo [info | (info,_) <- rhss']

    binds_fvs	      = rhss_fvs `minusIdSet` binders_set
    body2	      = fvExpr new_id_cands tyvar_cands body
    body_fvs	      = freeVarsOf body2 `minusIdSet` binders_set
    binders_ftvs      = foldr (combine . munge_id_ty) noFreeTyVars binders
	-- We need to collect free tyvars from the binders

fvExpr id_cands tyvar_cands (SCC label expr)
  = (fvinfo, AnnSCC label expr2)
  where
    expr2@(fvinfo,_) = fvExpr id_cands tyvar_cands expr

fvExpr id_cands tyvar_cands (Coerce c ty expr)
  = (FVInfo (freeVarsOf   expr2)
	    (freeTyVarsOf expr2 `combine` tfvs)
	    (leakinessOf  expr2),
     AnnCoerce c ty expr2)
  where
    expr2 = fvExpr id_cands tyvar_cands expr
    tfvs  = freeTy tyvar_cands ty
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
    free_arg (LitArg   _) = noFreeAnything
    free_arg (UsageArg _) = noFreeAnything
    free_arg (TyArg   ty) = (noFreeIds, freeTy tcands ty)
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
\section[freevars-binders]{Attaching free variables to binders
%*									*
%************************************************************************


Here's an variant of the free-variable pass, which pins free-variable
information on {\em binders} rather than every single jolly
expression!
\begin{itemize}
\item
  The free vars attached to a lambda binder are the free vars of the
  whole lambda abstraction.  If there are multiple binders, they are
  each given the same free-var set.
\item
  The free vars attached to a let(rec) binder are the free vars of the
  rhs of the binding.  In the case of letrecs, this set excludes the
  binders themselves.
\item
  The free vars attached to a case alternative binder are the free
  vars of the alternative, excluding the alternative's binders.
\end{itemize}

There's a predicate carried in which tells what is a free-var
candidate. It is passed the Id and a set of in-scope Ids.

(Global) constructors used on the rhs in a Con are also treated as
potential free-var candidates (though they will not be recorded in the
in-scope set). The predicate must decide if they are to be recorded as
free-vars.

As it happens this is only ever used by the Specialiser!

\begin{code}
type FVCoreBinder  = (Id, IdSet)
type FVCoreExpr    = GenCoreExpr    FVCoreBinder Id TyVar UVar
type FVCoreBinding = GenCoreBinding FVCoreBinder Id TyVar UVar

type InterestingIdFun
  =  IdSet	-- Non-top-level in-scope variables
  -> Id		-- The Id being looked at
  -> Bool	-- True <=> interesting
\end{code}

\begin{code}
addExprFVs :: InterestingIdFun	-- "Interesting id" predicate
	   -> IdSet		-- In scope ids
	   -> CoreExpr
	   -> (FVCoreExpr, IdSet)

addExprFVs fv_cand in_scope (Var v)
  = (Var v, if fv_cand in_scope v
	      then aFreeId v
	      else noFreeIds)

addExprFVs fv_cand in_scope (Lit lit) = (Lit lit, noFreeIds)

addExprFVs fv_cand in_scope (Con con args)
  = (Con con args,
     if fv_cand in_scope con
     then aFreeId con
     else noFreeIds `combine` fvsOfArgs fv_cand in_scope args)

addExprFVs fv_cand in_scope (Prim op args)
  = (Prim op args, fvsOfArgs fv_cand in_scope args)

addExprFVs fv_cand in_scope (Lam binder body)
  = (Lam new_binder new_body, lam_fvs)
  where
    (new_binder, binder_set)
      = case binder of
	  TyBinder    t -> (TyBinder t, emptyIdSet)
	  UsageBinder u -> (UsageBinder u, emptyIdSet)
          ValBinder   b -> (ValBinder (b, lam_fvs),
			    unitIdSet b)

    new_in_scope	 = in_scope `combine` binder_set
    (new_body, body_fvs) = addExprFVs fv_cand new_in_scope body
    lam_fvs	         = body_fvs `minusIdSet` binder_set

addExprFVs fv_cand in_scope (App fun arg)
  = (App fun2 arg, fun_fvs `combine` fvsOfArgs fv_cand in_scope [arg])
  where
    (fun2, fun_fvs) = addExprFVs fv_cand in_scope fun

addExprFVs fv_cand in_scope (Case scrut alts)
  = (Case scrut' alts', scrut_fvs `combine` alts_fvs)
  where
    (scrut', scrut_fvs) = addExprFVs fv_cand in_scope scrut

    (alts', alts_fvs)
      = case alts of
	  AlgAlts alg_alts deflt -> (AlgAlts alg_alts' deflt', fvs)
	    where
	      (alg_alts', alt_fvs) = unzip (map do_alg_alt alg_alts)
	      (deflt', deflt_fvs) = do_deflt deflt
	      fvs = unionManyIdSets (deflt_fvs : alt_fvs)

	  PrimAlts prim_alts deflt -> (PrimAlts prim_alts' deflt', fvs)
	    where
	      (prim_alts', alt_fvs) = unzip (map do_prim_alt prim_alts)
	      (deflt', deflt_fvs) = do_deflt deflt
	      fvs = unionManyIdSets (deflt_fvs : alt_fvs)

    do_alg_alt :: (Id, [Id], CoreExpr)
	       -> ((Id, [FVCoreBinder], FVCoreExpr), IdSet)

    do_alg_alt (con, args, rhs) = ((con, args `zip` (repeat fvs), rhs'), fvs)
      where
	new_in_scope = in_scope `combine` arg_set
	(rhs', rhs_fvs) = addExprFVs fv_cand new_in_scope rhs
	fvs = rhs_fvs `minusIdSet` arg_set
	arg_set = mkIdSet args

    do_prim_alt (lit, rhs) = ((lit, rhs'), rhs_fvs)
      where
	(rhs', rhs_fvs) = addExprFVs fv_cand in_scope rhs

    do_deflt NoDefault = (NoDefault, noFreeIds)
    do_deflt (BindDefault var rhs)
      = (BindDefault (var,fvs) rhs', fvs)
      where
	new_in_scope = in_scope `combine` var_set
	(rhs', rhs_fvs) = addExprFVs fv_cand new_in_scope rhs
	fvs = rhs_fvs `minusIdSet` var_set
	var_set = aFreeId var

addExprFVs fv_cand in_scope (Let binds body)
  = (Let binds' body2, fvs_binds `combine` (fvs_body `minusIdSet` binder_set))
  where
    (binds', fvs_binds, new_in_scope, binder_set)
      = addBindingFVs fv_cand in_scope binds

    (body2, fvs_body)  = addExprFVs fv_cand new_in_scope body

addExprFVs fv_cand in_scope (SCC label expr)
  = (SCC label expr2, expr_fvs)
  where
    (expr2, expr_fvs) = addExprFVs fv_cand in_scope expr

addExprFVs fv_cand in_scope (Coerce c ty expr)
  = (Coerce c ty expr2, expr_fvs)
  where
    (expr2, expr_fvs) = addExprFVs fv_cand in_scope expr
\end{code}

\begin{code}
addBindingFVs
	    :: InterestingIdFun	-- "Interesting id" predicate
	    -> IdSet		-- In scope ids
	    -> CoreBinding
	    -> (FVCoreBinding,
		IdSet,		-- Free vars of binding group
		IdSet,		-- Augmented in-scope Ids
		IdSet)		-- Set of Ids bound by this binding

addBindingFVs fv_cand in_scope (NonRec binder rhs)
  = (NonRec binder' rhs', fvs, new_in_scope, binder_set)
  where
    ((binder', rhs'), fvs) = do_pair fv_cand in_scope binder_set (binder, rhs)
    new_in_scope = in_scope `combine` binder_set
    binder_set = aFreeId binder

addBindingFVs fv_cand in_scope (Rec pairs)
  = (Rec pairs', unionManyIdSets fvs_s, new_in_scope, binder_set)
  where
    binders = [binder | (binder,_) <- pairs]
    binder_set = mkIdSet binders
    new_in_scope = in_scope `combine` binder_set
    (pairs', fvs_s) = unzip (map (do_pair fv_cand new_in_scope binder_set) pairs)
\end{code}

\begin{code}
addTopBindsFVs
	    :: InterestingIdFun	-- "Interesting id" predicate
	    -> [CoreBinding]
	    -> ([FVCoreBinding],
		IdSet)

addTopBindsFVs fv_cand [] = ([], noFreeIds)
addTopBindsFVs fv_cand (b:bs)
  = let
      (b',  fvs_b, _, _) = addBindingFVs fv_cand noFreeIds b
      (bs', fvs_bs)      = addTopBindsFVs fv_cand bs
    in
    (b' : bs', fvs_b `combine` fvs_bs)
\end{code}

\begin{code}
fvsOfArgs   :: InterestingIdFun	-- "Interesting id" predicate
	    -> IdSet		-- In scope ids
	    -> [CoreArg]
	    -> IdSet

fvsOfArgs _ _ [] = noFreeIds

fvsOfArgs fv_cand in_scope [VarArg v] -- this is only a short-cut...
  = if (fv_cand in_scope v) then aFreeId v else noFreeIds
fvsOfArgs _	  _	   [ _ ] = noFreeIds

fvsOfArgs fv_cand in_scope args
  = mkIdSet [ v | (VarArg v) <- args, fv_cand in_scope v ]
    -- all other types of args are uninteresting here...

----------
do_pair	:: InterestingIdFun -- "Interesting id" predicate
	-> IdSet   	    -- In scope ids
	-> IdSet
	-> (Id, CoreExpr)
	-> ((FVCoreBinder, FVCoreExpr), IdSet)

do_pair fv_cand in_scope binder_set (binder,rhs)
 = (((binder, fvs), rhs'), fvs)
 where
   (rhs', rhs_fvs) = addExprFVs fv_cand in_scope rhs
   fvs = rhs_fvs `minusIdSet` binder_set
\end{code}
