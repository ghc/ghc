%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
module CoreFVs (
	exprFreeVars,	-- CoreExpr -> VarSet	-- Find all locally-defined free Ids or tyvars
	exprsFreeVars,	-- [CoreExpr] -> VarSet

	exprSomeFreeVars, exprsSomeFreeVars,

	idRuleVars, idFreeVars, idFreeTyVars,
	ruleRhsFreeVars, ruleLhsFreeNames, ruleLhsFreeIds, 

	CoreExprWithFVs,	-- = AnnExpr Id VarSet
	CoreBindWithFVs,	-- = AnnBind Id VarSet
	freeVars,		-- CoreExpr -> CoreExprWithFVs
	freeVarsOf		-- CoreExprWithFVs -> IdSet
    ) where

#include "HsVersions.h"

import CoreSyn
import Id		( Id, idType, idSpecialisation )
import NameSet
import VarSet
import Var		( Var, isId, isLocalVar, varName )
import Type		( tyVarsOfType )
import TcType		( tyClsNamesOfType )
import Util		( mapAndUnzip )
import Outputable
\end{code}


%************************************************************************
%*									*
\section{Finding the free variables of an expression}
%*									*
%************************************************************************

This function simply finds the free variables of an expression.
So far as type variables are concerned, it only finds tyvars that are

	* free in type arguments, 
	* free in the type of a binder,

but not those that are free in the type of variable occurrence.

\begin{code}
exprFreeVars :: CoreExpr -> VarSet	-- Find all locally-defined free Ids or tyvars
exprFreeVars = exprSomeFreeVars isLocalVar

exprsFreeVars :: [CoreExpr] -> VarSet
exprsFreeVars = foldr (unionVarSet . exprFreeVars) emptyVarSet

exprSomeFreeVars :: InterestingVarFun 	-- Says which Vars are interesting
		 -> CoreExpr
		 -> VarSet
exprSomeFreeVars fv_cand e = expr_fvs e fv_cand emptyVarSet

exprsSomeFreeVars :: InterestingVarFun 	-- Says which Vars are interesting
		  -> [CoreExpr]
		  -> VarSet
exprsSomeFreeVars fv_cand = foldr (unionVarSet . exprSomeFreeVars fv_cand) emptyVarSet

type InterestingVarFun = Var -> Bool	-- True <=> interesting
\end{code}


\begin{code}
type FV = InterestingVarFun 
	  -> VarSet		-- In scope
	  -> VarSet		-- Free vars

union :: FV -> FV -> FV
union fv1 fv2 fv_cand in_scope = fv1 fv_cand in_scope `unionVarSet` fv2 fv_cand in_scope

noVars :: FV
noVars fv_cand in_scope = emptyVarSet

-- At a variable occurrence, add in any free variables of its rule rhss
-- Curiously, we gather the Id's free *type* variables from its binding
-- site, but its free *rule-rhs* variables from its usage sites.  This
-- is a little weird.  The reason is that the former is more efficient,
-- but the latter is more fine grained, and a makes a difference when
-- a variable mentions itself one of its own rule RHSs
oneVar :: Id -> FV
oneVar var fv_cand in_scope
  = ASSERT( isId var ) 
    foldVarSet add_rule_var var_itself_set (idRuleVars var)
  where
    var_itself_set | keep_it fv_cand in_scope var = unitVarSet var
	           | otherwise		      = emptyVarSet
    add_rule_var var set | keep_it fv_cand in_scope var = extendVarSet set var
			 | otherwise			= set

someVars :: VarSet -> FV
someVars vars fv_cand in_scope
  = filterVarSet (keep_it fv_cand in_scope) vars

keep_it fv_cand in_scope var
  | var `elemVarSet` in_scope = False
  | fv_cand var		      = True
  | otherwise		      = False


addBndr :: CoreBndr -> FV -> FV
addBndr bndr fv fv_cand in_scope
  | isId bndr = inside_fvs `unionVarSet` someVars (idFreeTyVars bndr) fv_cand in_scope
  | otherwise = inside_fvs
  where
    inside_fvs = fv fv_cand (in_scope `extendVarSet` bndr) 

addBndrs :: [CoreBndr] -> FV -> FV
addBndrs bndrs fv = foldr addBndr fv bndrs
\end{code}


\begin{code}
expr_fvs :: CoreExpr -> FV

expr_fvs (Type ty) 	 = someVars (tyVarsOfType ty)
expr_fvs (Var var) 	 = oneVar var
expr_fvs (Lit lit)	 = noVars
expr_fvs (Note _ expr)   = expr_fvs expr
expr_fvs (App fun arg)   = expr_fvs fun `union` expr_fvs arg
expr_fvs (Lam bndr body) = addBndr bndr (expr_fvs body)

-- gaw 2004
expr_fvs (Case scrut bndr ty alts)
  = expr_fvs scrut `union` someVars (tyVarsOfType ty) `union` addBndr bndr  
      (foldr (union . alt_fvs) noVars alts)
  where
    alt_fvs (con, bndrs, rhs) = addBndrs bndrs (expr_fvs rhs)

expr_fvs (Let (NonRec bndr rhs) body)
  = expr_fvs rhs `union` addBndr bndr (expr_fvs body)

expr_fvs (Let (Rec pairs) body)
  = addBndrs bndrs (foldr (union . expr_fvs) (expr_fvs body) rhss)
  where
    (bndrs,rhss) = unzip pairs
\end{code}


%************************************************************************
%*									*
\section{Free names}
%*									*
%************************************************************************

exprFreeNames finds the free *names* of an expression, notably
including the names of type constructors (which of course do not show
up in exprFreeVars).  Similarly ruleLhsFreeNames.  The latter is used
when deciding whether a rule is an orphan.  In particular, suppose that
T is defined in this module; we want to avoid declaring that a rule like
	fromIntegral T = fromIntegral_T
is an orphan.  Of course it isn't, an declaring it an orphan would
make the whole module an orphan module, which is bad.

\begin{code}
ruleLhsFreeNames :: IdCoreRule -> NameSet
ruleLhsFreeNames (IdCoreRule fn _ (BuiltinRule _ _)) = unitNameSet (varName fn)
ruleLhsFreeNames (IdCoreRule fn _ (Rule _ _ tpl_vars tpl_args rhs))
  = addOneToNameSet (exprsFreeNames tpl_args `del_binders` tpl_vars) (varName fn)

exprFreeNames :: CoreExpr -> NameSet
exprFreeNames (Var v) 	  = unitNameSet (varName v)
exprFreeNames (Lit _) 	  = emptyNameSet
exprFreeNames (Type ty)   = tyClsNamesOfType ty	-- Don't need free tyvars
exprFreeNames (App e1 e2) = exprFreeNames e1 `unionNameSets` exprFreeNames e2
exprFreeNames (Lam v e)   = exprFreeNames e `delFromNameSet` varName v
exprFreeNames (Note n e)  = exprFreeNames e

exprFreeNames (Let (NonRec b r) e) = (exprFreeNames e `delFromNameSet` varName b)
				     `unionNameSets` exprFreeNames r

exprFreeNames (Let (Rec prs) e) = (exprsFreeNames rs `unionNameSets` exprFreeNames e)
				  `del_binders` bs
				where
				  (bs, rs) = unzip prs

-- gaw 2004
exprFreeNames (Case e b ty as) = exprFreeNames e `unionNameSets` tyClsNamesOfType ty 
                                 `unionNameSets`
			         (unionManyNameSets (map altFreeNames as) `delFromNameSet` varName b)

-- Helpers
altFreeNames (_,bs,r) = exprFreeNames r `del_binders` bs

exprsFreeNames es = foldr (unionNameSets . exprFreeNames) emptyNameSet es

del_binders :: NameSet -> [Var] -> NameSet
del_binders names bndrs = foldl (\s b -> delFromNameSet s (varName b)) names bndrs
\end{code}

%************************************************************************
%*									*
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
%*									*
%************************************************************************


\begin{code}
ruleRhsFreeVars :: CoreRule -> VarSet
ruleRhsFreeVars (BuiltinRule _ _) = noFVs
ruleRhsFreeVars (Rule str _ tpl_vars tpl_args rhs)
  = rule_fvs isLocalVar emptyVarSet
  where
    rule_fvs = addBndrs tpl_vars (expr_fvs rhs)

ruleLhsFreeIds :: CoreRule -> VarSet
-- This finds all locally-defined free Ids on the LHS of the rule
ruleLhsFreeIds (BuiltinRule _ _) = noFVs
ruleLhsFreeIds (Rule _ _ tpl_vars tpl_args rhs)
  = foldl delVarSet (exprsFreeVars tpl_args) tpl_vars
\end{code}


%************************************************************************
%*									*
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
%*									*
%************************************************************************

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.

\begin{code}
type CoreBindWithFVs = AnnBind Id VarSet
type CoreExprWithFVs = AnnExpr Id VarSet
	-- Every node annotated with its free variables,
	-- both Ids and TyVars

freeVarsOf :: CoreExprWithFVs -> IdSet
freeVarsOf (free_vars, _) = free_vars

noFVs    = emptyVarSet
aFreeVar = unitVarSet
unionFVs = unionVarSet

delBindersFV :: [Var] -> VarSet -> VarSet
delBindersFV bs fvs = foldr delBinderFV fvs bs

delBinderFV :: Var -> VarSet -> VarSet
-- This way round, so we can do it multiple times using foldr

-- (b `delBinderFV` s) removes the binder b from the free variable set s,
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

delBinderFV b s | isId b    = (s `delVarSet` b) `unionFVs` idFreeVars b
	        | otherwise = s `delVarSet` b

idFreeVars :: Id -> VarSet
idFreeVars id = ASSERT( isId id) idRuleVars id `unionVarSet` idFreeTyVars id

idFreeTyVars :: Id -> TyVarSet
-- Only local Ids conjured up locally, can have free type variables.
-- (During type checking top-level Ids can have free tyvars)
idFreeTyVars id = tyVarsOfType (idType id)
--  | isLocalId id = tyVarsOfType (idType id)
--  | otherwise    = emptyVarSet

idRuleVars ::Id -> VarSet
idRuleVars id = ASSERT( isId id) rulesRhsFreeVars (idSpecialisation id)
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

    fvs | isLocalVar v = aFreeVar v
	| otherwise    = noFVs

freeVars (Lit lit) = (noFVs, AnnLit lit)
freeVars (Lam b body)
  = (b `delBinderFV` freeVarsOf body', AnnLam b body')
  where
    body' = freeVars body

freeVars (App fun arg)
  = (freeVarsOf fun2 `unionFVs` freeVarsOf arg2, AnnApp fun2 arg2)
  where
    fun2 = freeVars fun
    arg2 = freeVars arg

freeVars (Case scrut bndr ty alts)
-- gaw 2004
  = ((bndr `delBinderFV` alts_fvs) `unionFVs` freeVarsOf scrut2 `unionFVs` tyVarsOfType ty,
     AnnCase scrut2 bndr ty alts2)
  where
    scrut2 = freeVars scrut

    (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
    alts_fvs 		= foldr1 unionFVs alts_fvs_s

    fv_alt (con,args,rhs) = (delBindersFV args (freeVarsOf rhs2),
			     (con, args, rhs2))
		    	  where
			     rhs2 = freeVars rhs

freeVars (Let (NonRec binder rhs) body)
  = (freeVarsOf rhs2 `unionFVs` body_fvs,
     AnnLet (AnnNonRec binder rhs2) body2)
  where
    rhs2     = freeVars rhs
    body2    = freeVars body
    body_fvs = binder `delBinderFV` freeVarsOf body2

freeVars (Let (Rec binds) body)
  = (foldl delVarSet group_fvs binders,
	-- The "delBinderFV" part may have added one of the binders
	-- via the idSpecVars part, so we must delete it again
     AnnLet (AnnRec (binders `zip` rhss2)) body2)
  where
    (binders, rhss) = unzip binds

    rhss2     = map freeVars rhss
    all_fvs   = foldr (unionFVs . fst) body_fvs rhss2
    group_fvs = delBindersFV binders all_fvs

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

