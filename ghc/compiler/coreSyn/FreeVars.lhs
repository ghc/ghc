%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
module FreeVars (
	freeVars,
	freeVarsOf,
	CoreExprWithFVs, CoreBindWithFVs
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreUtils	( idFreeVars )
import Id		( Id )
import VarSet
import Var		( IdOrTyVar, isId )
import Name		( isLocallyDefined )
import Type		( tyVarsOfType, Type )
import Util		( mapAndUnzip )
\end{code}

%************************************************************************
%*									*
\section[freevars-everywhere]{Attaching free variables to every sub-expression
%*									*
%************************************************************************

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.

\begin{code}
type CoreBindWithFVs = AnnBind Id IdOrTyVarSet
type CoreExprWithFVs = AnnExpr Id IdOrTyVarSet
	-- Every node annotated with its free variables,
	-- both Ids and TyVars

freeVarsOf :: CoreExprWithFVs -> IdSet
freeVarsOf (free_vars, _) = free_vars

noFVs    = emptyVarSet
aFreeVar = unitVarSet
unionFVs = unionVarSet

filters :: IdOrTyVar -> IdOrTyVarSet -> IdOrTyVarSet

-- (b `filters` s) removes the binder b from the free variable set s,
-- but *adds* to s
--	(a) the free variables of b's type
--	(b) the idSpecVars of b
--
-- This is really important for some lambdas:
-- 	In (\x::a -> x) the only mention of "a" is in the binder.
--
-- Also in
--	let x::a = b in ...
-- we should really note that "a" is free in this expression.
-- It'll be pinned inside the /\a by the binding for b, but
-- it seems cleaner to make sure that a is in the free-var set 
-- when it is mentioned.
--
-- This also shows up in recursive bindings.  Consider:
--	/\a -> letrec x::a = x in E
-- Now, there are no explicit free type variables in the RHS of x,
-- but nevertheless "a" is free in its definition.  So we add in
-- the free tyvars of the types of the binders, and include these in the
-- free vars of the group, attached to the top level of each RHS.
--
-- This actually happened in the defn of errorIO in IOBase.lhs:
--	errorIO (ST io) = case (errorIO# io) of
--	    		    _ -> bottom
--			  where
--			    bottom = bottom -- Never evaluated

filters b s | isId b    = (s `delVarSet` b) `unionFVs` idFreeVars b
	    | otherwise = s `delVarSet` b
\end{code}


%************************************************************************
%*									*
\subsection{Free variables (and types)}
%*									*
%************************************************************************

\begin{code}
freeVars :: CoreExpr -> CoreExprWithFVs

freeVars (Var v)
  = (fvs, AnnVar v)
  where
	-- ToDo: insert motivating example for why we *need*
	-- to include the idSpecVars in the FV list.
	--	Actually [June 98] I don't think it's necessary
	-- fvs = fvs_v `unionVarSet` idSpecVars v

    fvs | isLocallyDefined v = aFreeVar v
	| otherwise	     = noFVs

freeVars (Con con args)
  = (foldr (unionFVs . freeVarsOf) noFVs args2, AnnCon con args2)
  where
    args2 = map freeVars args

freeVars (Lam b body)
  = (b `filters` freeVarsOf body', AnnLam b body')
  where
    body' = freeVars body

freeVars (App fun arg)
  = (freeVarsOf fun2 `unionFVs` freeVarsOf arg2, AnnApp fun2 arg2)
  where
    fun2 = freeVars fun
    arg2 = freeVars arg

freeVars (Case scrut bndr alts)
  = ((bndr `filters` alts_fvs) `unionFVs` freeVarsOf scrut2,
     AnnCase scrut2 bndr alts2)
  where
    scrut2 = freeVars scrut

    (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
    alts_fvs 		= foldr1 unionFVs alts_fvs_s

    fv_alt (con,args,rhs) = (foldr filters (freeVarsOf rhs2) args,
			     (con, args, rhs2))
		    	  where
			     rhs2 = freeVars rhs

freeVars (Let (NonRec binder rhs) body)
  = (freeVarsOf rhs2 `unionFVs` body_fvs,
     AnnLet (AnnNonRec binder rhs2) body2)
  where
    rhs2     = freeVars rhs
    body2    = freeVars body
    body_fvs = binder `filters` freeVarsOf body2

freeVars (Let (Rec binds) body)
  = (foldl delVarSet group_fvs binders,
	-- The "filters" part may have added one of the binders
	-- via the idSpecVars part, so we must delete it again
     AnnLet (AnnRec (binders `zip` rhss2)) body2)
  where
    (binders, rhss) = unzip binds

    rhss2     = map freeVars rhss
    all_fvs   = foldr (unionFVs . fst) body_fvs rhss2
    group_fvs = foldr filters all_fvs binders

    body2     = freeVars body
    body_fvs  = freeVarsOf body2

freeVars (Note (Coerce to_ty from_ty) expr)
  = (freeVarsOf expr2 `unionFVs` tfvs1 `unionFVs` tfvs2,
     AnnNote (Coerce to_ty from_ty) expr2)
  where
    expr2  = freeVars expr
    tfvs1  = tyVarsOfType from_ty
    tfvs2  = tyVarsOfType to_ty

freeVars (Note other_note expr)
  = (freeVarsOf expr2, AnnNote other_note expr2)
  where
    expr2 = freeVars expr

freeVars (Type ty) = (tyVarsOfType ty, AnnType ty)
\end{code}

