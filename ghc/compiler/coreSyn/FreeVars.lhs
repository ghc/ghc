%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
#include "HsVersions.h"

module FreeVars (
	freeVars,

	-- cheap and cheerful variant...
	addTopBindsFVs,

	freeVarsOf, freeTyVarsOf,
	FVCoreExpr(..), FVCoreBinding(..),

	CoreExprWithFVs(..),		-- For the above functions
	AnnCoreExpr(..),		-- Dito
	FVInfo(..), LeakInfo(..)

	-- and to make the interface self-sufficient...
    ) where


import AnnCoreSyn	-- output

import PrelInfo		( PrimOp(..), PrimRep -- for CCallOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import Type		( extractTyVarsFromTy )
import Id		( idType, getIdArity, toplevelishId, isBottomingId )
import IdInfo		-- Wanted for arityMaybe, but it seems you have
			-- to import it all...  (Death to the Instance Virus!)
import Maybes
import UniqSet
import Util
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
type CoreExprWithFVs =  AnnCoreExpr Id Id FVInfo

type TyVarCands = TyVarSet  -- for when we carry around lists of
type IdCands	= IdSet	    -- "candidate" TyVars/Ids.
noTyVarCands    = emptyUniqSet
noIdCands       = emptyUniqSet

data FVInfo = FVInfo
		IdSet	    -- Free ids
		TyVarSet    -- Free tyvars
		LeakInfo

noFreeIds      = emptyUniqSet
noFreeTyVars   = emptyUniqSet
aFreeId i      = singletonUniqSet i
aFreeTyVar t   = singletonUniqSet t
is_among       = elementOfUniqSet
combine	       = unionUniqSets
munge_id_ty  i = mkUniqSet (extractTyVarsFromTy (idType i))

combineFVInfo (FVInfo fvs1 tfvs1 leak1) (FVInfo fvs2 tfvs2 leak2)
  = FVInfo (fvs1  `combine` fvs2)
	   (tfvs1 `combine` tfvs2)
	   (leak1 `orLeak`	  leak2)
\end{code}

Leak-free-ness is based only on the value, not the type.
In particular, nested collections of constructors are guaranteed leak free.
Function applications are not, except for PAPs.

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

\subsection{Free variables (and types)}

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
     AnnCoVar v)
  where
    leakiness
      | isBottomingId v = lEAK_FREE_BIG	-- Hack
      | otherwise       = case arityMaybe (getIdArity v) of
			    Nothing    -> lEAK_FREE_0
			    Just arity -> LeakFree arity

fvExpr id_cands tyvar_cands (Lit k)
  = (FVInfo noFreeIds noFreeTyVars lEAK_FREE_0, AnnCoLit k)

fvExpr id_cands tyvar_cands (Con c tys args)
  = (FVInfo args_fvs tfvs lEAK_FREE_0, AnnCoCon c tys args)
  where
    args_fvs = foldr (combine . freeAtom id_cands)  noFreeIds    args
    tfvs     = foldr (combine . freeTy tyvar_cands) noFreeTyVars tys

fvExpr id_cands tyvar_cands (Prim op@(CCallOp _ _ _ _ res_ty) tys args)
  = ASSERT (null tys)
    (FVInfo args_fvs tfvs lEAK_FREE_0, AnnCoPrim op tys args)
  where
    args_fvs = foldr (combine . freeAtom id_cands)  noFreeIds    args
    tfvs     = foldr (combine . freeTy tyvar_cands) noFreeTyVars (res_ty:tys)

fvExpr id_cands tyvar_cands (Prim op tys args)
  = (FVInfo args_fvs tfvs lEAK_FREE_0, AnnCoPrim op tys args)
  where
    args_fvs = foldr (combine . freeAtom id_cands)  noFreeIds    args
    tfvs     = foldr (combine . freeTy tyvar_cands) noFreeTyVars tys

fvExpr id_cands tyvar_cands (Lam binder body)
  = (FVInfo (freeVarsOf body2   `minusUniqSet`  singletonUniqSet binder)
	    (freeTyVarsOf body2 `combine` munge_id_ty binder)
	    leakiness,
     AnnCoLam binder body2)
  where
	-- We need to collect free tyvars from the binders
    body2 = fvExpr (singletonUniqSet binder `combine` id_cands) tyvar_cands body

    leakiness = case leakinessOf body2 of
		  MightLeak  -> LeakFree 1
		  LeakFree n -> LeakFree (n + 1)

fvExpr id_cands tyvar_cands (CoTyLam tyvar body)
  = (FVInfo (freeVarsOf body2)
	    (freeTyVarsOf body2 `minusUniqSet` aFreeTyVar tyvar)
	    (leakinessOf body2),
     AnnCoTyLam tyvar body2)
  where
    body2 = fvExpr id_cands (aFreeTyVar tyvar `combine` tyvar_cands) body

fvExpr id_cands tyvar_cands (App fun arg)
  = (FVInfo (freeVarsOf fun2 `combine` fvs_arg)
	    (freeTyVarsOf fun2)
	    leakiness,
     AnnCoApp fun2 arg)
  where
    fun2 = fvExpr id_cands tyvar_cands fun
    fvs_arg = freeAtom id_cands arg

    leakiness = case leakinessOf fun2 of
		   LeakFree n | n>1 -> LeakFree (n-1)	-- Note > not >=
		   other            -> MightLeak

fvExpr id_cands tyvar_cands (CoTyApp expr ty)
  = (FVInfo (freeVarsOf expr2)
	    (freeTyVarsOf expr2 `combine` tfvs_arg)
	    (leakinessOf expr2),
     AnnCoTyApp expr2 ty)
  where
    expr2    = fvExpr id_cands tyvar_cands expr
    tfvs_arg = freeTy tyvar_cands ty

fvExpr id_cands tyvar_cands (Case expr alts)
  = (combineFVInfo expr_fvinfo alts_fvinfo,
     AnnCoCase expr2 alts')
  where
    expr2@(expr_fvinfo,_) = fvExpr id_cands tyvar_cands expr
    (alts_fvinfo, alts') = annotate_alts alts

    annotate_alts (AlgAlts alts deflt)
      = (fvinfo, AnnCoAlgAlts alts' deflt')
      where
    	(alts_fvinfo_s, alts') = unzip (map ann_boxed_alt alts)
    	(deflt_fvinfo, deflt') = annotate_default deflt
	fvinfo = foldr combineFVInfo deflt_fvinfo alts_fvinfo_s

	ann_boxed_alt (con, params, rhs)
	  = (FVInfo (freeVarsOf rhs' `minusUniqSet` mkUniqSet params)
		    (freeTyVarsOf rhs' `combine` param_ftvs)
		    (leakinessOf rhs'),
	     (con, params, rhs'))
    	  where
	    rhs' = fvExpr (mkUniqSet params `combine` id_cands) tyvar_cands rhs
	    param_ftvs = foldr (combine . munge_id_ty) noFreeTyVars params
		-- We need to collect free tyvars from the binders

    annotate_alts (PrimAlts alts deflt)
      = (fvinfo, AnnCoPrimAlts alts' deflt')
      where
    	(alts_fvinfo_s, alts') = unzip (map ann_unboxed_alt alts)
    	(deflt_fvinfo, deflt') = annotate_default deflt
	fvinfo  = foldr combineFVInfo deflt_fvinfo alts_fvinfo_s

	ann_unboxed_alt (lit, rhs) = (rhs_info, (lit, rhs'))
    	  where
	    rhs'@(rhs_info,_) = fvExpr id_cands tyvar_cands rhs

    annotate_default NoDefault = (FVInfo noFreeIds noFreeTyVars lEAK_FREE_BIG,
				    AnnCoNoDefault)

    annotate_default (BindDefault binder rhs)
      = (FVInfo (freeVarsOf   rhs' `minusUniqSet` aFreeId binder)
		(freeTyVarsOf rhs' `combine` binder_ftvs)
		(leakinessOf rhs'),
	 AnnCoBindDefault binder rhs')
      where
	rhs' = fvExpr (aFreeId binder `combine` id_cands) tyvar_cands rhs
	binder_ftvs = munge_id_ty binder
	    -- We need to collect free tyvars from the binder

fvExpr id_cands tyvar_cands (Let (NonRec binder rhs) body)
  = (FVInfo (freeVarsOf rhs'   `combine` body_fvs)
	    (freeTyVarsOf rhs' `combine` freeTyVarsOf body2 `combine` binder_ftvs)
	    (leakinessOf rhs' `orLeak` leakinessOf body2),
     AnnCoLet (AnnCoNonRec binder rhs') body2)
  where
    rhs'	= fvExpr id_cands tyvar_cands rhs
    body2	= fvExpr (aFreeId binder `combine` id_cands) tyvar_cands body
    body_fvs	= freeVarsOf body2 `minusUniqSet` aFreeId binder
    binder_ftvs = munge_id_ty binder
	-- We need to collect free tyvars from the binder

fvExpr id_cands tyvar_cands (Let (Rec binds) body)
  = (FVInfo (binds_fvs `combine` body_fvs)
	    (rhss_tfvs `combine` freeTyVarsOf body2 `combine` binders_ftvs)
	    (leakiness_of_rhss `orLeak` leakinessOf body2),
     AnnCoLet (AnnCoRec (binders `zip` rhss')) body2)
  where
    (binders, rhss)   = unzip binds
    new_id_cands      = binders_set `combine` id_cands
    binders_set	      = mkUniqSet binders
    rhss'	      = map (fvExpr new_id_cands tyvar_cands) rhss

    FVInfo rhss_fvs rhss_tfvs leakiness_of_rhss
	= foldr1 combineFVInfo [info | (info,_) <- rhss']

    binds_fvs	      = rhss_fvs `minusUniqSet` binders_set
    body2	      = fvExpr new_id_cands tyvar_cands body
    body_fvs	      = freeVarsOf body2 `minusUniqSet` binders_set
    binders_ftvs      = foldr (combine . munge_id_ty) noFreeTyVars binders
	-- We need to collect free tyvars from the binders

fvExpr id_cands tyvar_cands (SCC label expr)
  = (fvinfo, AnnCoSCC label expr2)
  where
    expr2@(fvinfo,_) = fvExpr id_cands tyvar_cands expr
\end{code}

\begin{code}
freeAtom :: IdCands -> CoreArg ->  IdSet

freeAtom cands (LitArg k) = noFreeIds
freeAtom cands (VarArg v) | v `is_among` cands = aFreeId v
			     | otherwise	  = noFreeIds

freeTy :: TyVarCands -> Type -> TyVarSet

freeTy cands ty = mkUniqSet (extractTyVarsFromTy ty) `intersectUniqSets` cands

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
type FVCoreExpr    = GenCoreExpr    FVCoreBinder Id
type FVCoreBinding = GenCoreBinding FVCoreBinder Id

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

addExprFVs fv_cand in_scope (Con con tys args)
  = (Con con tys args,
     if fv_cand in_scope con
     then aFreeId con
     else noFreeIds
	`combine`
     unionManyUniqSets (map (fvsOfAtom fv_cand in_scope) args))

addExprFVs fv_cand in_scope (Prim op tys args)
  = (Prim op tys args,
     unionManyUniqSets (map (fvsOfAtom fv_cand in_scope) args))

addExprFVs fv_cand in_scope (Lam binder body)
  = (Lam (binder,lam_fvs) new_body, lam_fvs)
  where
    binder_set = singletonUniqSet binder
    new_in_scope = in_scope `combine` binder_set
    (new_body, body_fvs) = addExprFVs fv_cand new_in_scope body
    lam_fvs = body_fvs `minusUniqSet` binder_set

addExprFVs fv_cand in_scope (CoTyLam tyvar body)
  = (CoTyLam tyvar body2, body_fvs)
  where
    (body2, body_fvs) = addExprFVs fv_cand in_scope body

addExprFVs fv_cand in_scope (App fun arg)
  = (App fun2 arg, fun_fvs `combine` fvsOfAtom fv_cand in_scope arg)
  where
    (fun2, fun_fvs) = addExprFVs fv_cand in_scope fun

addExprFVs fv_cand in_scope (CoTyApp fun ty)
  = (CoTyApp fun2 ty, fun_fvs)
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
	      fvs = unionManyUniqSets (deflt_fvs : alt_fvs)

	  PrimAlts prim_alts deflt -> (PrimAlts prim_alts' deflt', fvs)
	    where
	      (prim_alts', alt_fvs) = unzip (map do_prim_alt prim_alts)
	      (deflt', deflt_fvs) = do_deflt deflt
	      fvs = unionManyUniqSets (deflt_fvs : alt_fvs)

    do_alg_alt :: (Id, [Id], CoreExpr)
	       -> ((Id, [FVCoreBinder], FVCoreExpr), IdSet)

    do_alg_alt (con, args, rhs) = ((con, args `zip` (repeat fvs), rhs'), fvs)
      where
	new_in_scope = in_scope `combine` arg_set
	(rhs', rhs_fvs) = addExprFVs fv_cand new_in_scope rhs
	fvs = rhs_fvs `minusUniqSet` arg_set
	arg_set = mkUniqSet args

    do_prim_alt (lit, rhs) = ((lit, rhs'), rhs_fvs)
      where
	(rhs', rhs_fvs) = addExprFVs fv_cand in_scope rhs

    do_deflt NoDefault = (NoDefault, noFreeIds)
    do_deflt (BindDefault var rhs)
      = (BindDefault (var,fvs) rhs', fvs)
      where
	new_in_scope = in_scope `combine` var_set
	(rhs', rhs_fvs) = addExprFVs fv_cand new_in_scope rhs
	fvs = rhs_fvs `minusUniqSet` var_set
	var_set = aFreeId var

addExprFVs fv_cand in_scope (Let binds body)
  = (Let binds' body2, fvs_binds `combine` (fvs_body `minusUniqSet` binder_set))
  where
    (binds', fvs_binds, new_in_scope, binder_set)
      = addBindingFVs fv_cand in_scope binds

    (body2, fvs_body)  = addExprFVs fv_cand new_in_scope body

addExprFVs fv_cand in_scope (SCC label expr)
  = (SCC label expr2, expr_fvs)
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
  = (Rec pairs', unionManyUniqSets fvs_s, new_in_scope, binder_set)
  where
    binders = [binder | (binder,_) <- pairs]
    binder_set = mkUniqSet binders
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
fvsOfAtom   :: InterestingIdFun	-- "Interesting id" predicate
	    -> IdSet		-- In scope ids
	    -> CoreArg
	    -> IdSet

fvsOfAtom fv_cand in_scope (VarArg v)
  = if fv_cand in_scope v
    then aFreeId v
    else noFreeIds
fvsOfAtom _ _ _ = noFreeIds -- if a literal...

do_pair	:: InterestingIdFun -- "Interesting id" predicate
	-> IdSet   	    -- In scope ids
	-> IdSet
	-> (Id, CoreExpr)
	-> ((FVCoreBinder, FVCoreExpr), IdSet)

do_pair fv_cand in_scope binder_set (binder,rhs)
 = (((binder, fvs), rhs'), fvs)
 where
   (rhs', rhs_fvs) = addExprFVs fv_cand in_scope rhs
   fvs = rhs_fvs `minusUniqSet` binder_set
\end{code}
