%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
module CoreFVs (
	exprFreeVars,	-- CoreExpr   -> VarSet	-- Find all locally-defined free Ids or tyvars
	exprsFreeVars,	-- [CoreExpr] -> VarSet
	bindFreeVars, 	-- CoreBind   -> VarSet

	exprSomeFreeVars, exprsSomeFreeVars,
	exprFreeNames, exprsFreeNames,

	idRuleVars, idFreeVars, varTypeTyVars, 
	ruleRhsFreeVars, rulesRhsFreeVars,
	ruleLhsFreeNames, ruleLhsFreeIds, 

	CoreExprWithFVs,	-- = AnnExpr Id VarSet
	CoreBindWithFVs,	-- = AnnBind Id VarSet
	freeVars,		-- CoreExpr -> CoreExprWithFVs
	freeVarsOf		-- CoreExprWithFVs -> IdSet
    ) where

#include "HsVersions.h"

import CoreSyn
import Id
import IdInfo
import NameSet
import UniqFM
import Name
import VarSet
import Var
import TcType
import Util
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

bindFreeVars :: CoreBind -> VarSet
bindFreeVars (NonRec b r) = exprFreeVars r
bindFreeVars (Rec prs)    = addBndrs (map fst prs) 
				     (foldr (union . rhs_fvs) noVars prs)
				     isLocalVar emptyVarSet

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

--	Comment about obselete code
-- We used to gather the free variables the RULES at a variable occurrence
-- with the following cryptic comment:
--     "At a variable occurrence, add in any free variables of its rule rhss
--     Curiously, we gather the Id's free *type* variables from its binding
--     site, but its free *rule-rhs* variables from its usage sites.  This
--     is a little weird.  The reason is that the former is more efficient,
--     but the latter is more fine grained, and a makes a difference when
--     a variable mentions itself one of its own rule RHSs"
-- Not only is this "weird", but it's also pretty bad because it can make
-- a function seem more recursive than it is.  Suppose
--	f  = ...g...
--	g  = ...
--         RULE g x = ...f...
-- Then f is not mentioned in its own RHS, and needn't be a loop breaker
-- (though g may be).  But if we collect the rule fvs from g's occurrence,
-- it looks as if f mentions itself.  (This bites in the eftInt/eftIntFB
-- code in GHC.Enum.)
-- 
-- Anyway, it seems plain wrong.  The RULE is like an extra RHS for the
-- function, so its free variables belong at the definition site.
--
-- Deleted code looked like
--     foldVarSet add_rule_var var_itself_set (idRuleVars var)
--     add_rule_var var set | keep_it fv_cand in_scope var = extendVarSet set var
--			    | otherwise			   = set
-- 	SLPJ Feb06

oneVar :: Id -> FV
oneVar var fv_cand in_scope
  = ASSERT( isId var ) 
    if keep_it fv_cand in_scope var 
    then unitVarSet var
    else emptyVarSet

someVars :: VarSet -> FV
someVars vars fv_cand in_scope
  = filterVarSet (keep_it fv_cand in_scope) vars

keep_it fv_cand in_scope var
  | var `elemVarSet` in_scope = False
  | fv_cand var		      = True
  | otherwise		      = False


addBndr :: CoreBndr -> FV -> FV
addBndr bndr fv fv_cand in_scope
  = someVars (varTypeTyVars bndr) fv_cand in_scope
	-- Include type varibles in the binder's type
	-- 	(not just Ids; coercion variables too!)
    `unionVarSet`  fv fv_cand (in_scope `extendVarSet` bndr) 

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
expr_fvs (Cast expr co)  = expr_fvs expr `union` someVars (tyVarsOfType co)

expr_fvs (Case scrut bndr ty alts)
  = expr_fvs scrut `union` someVars (tyVarsOfType ty) `union` addBndr bndr  
      (foldr (union . alt_fvs) noVars alts)
  where
    alt_fvs (con, bndrs, rhs) = addBndrs bndrs (expr_fvs rhs)

expr_fvs (Let (NonRec bndr rhs) body)
  = rhs_fvs (bndr, rhs) `union` addBndr bndr (expr_fvs body)

expr_fvs (Let (Rec pairs) body)
  = addBndrs (map fst pairs) 
	     (foldr (union . rhs_fvs) (expr_fvs body) pairs)

---------
rhs_fvs (bndr, rhs) = expr_fvs rhs `union` someVars (idRuleVars bndr)
	-- Treat any RULES as extra RHSs of the binding

---------
exprs_fvs exprs = foldr (union . expr_fvs) noVars exprs
\end{code}


%************************************************************************
%*									*
\section{Free names}
%*									*
%************************************************************************

exprFreeNames finds the free *external* *names* of an expression, notably
including the names of type constructors (which of course do not show
up in exprFreeVars).  Similarly ruleLhsFreeNames.  The latter is used
when deciding whether a rule is an orphan.  In particular, suppose that
T is defined in this module; we want to avoid declaring that a rule like
	fromIntegral T = fromIntegral_T
is an orphan.  Of course it isn't, an declaring it an orphan would
make the whole module an orphan module, which is bad.

There's no need to delete local binders, because they will all
be *internal* names.

\begin{code}
ruleLhsFreeNames :: CoreRule -> NameSet
ruleLhsFreeNames (BuiltinRule { ru_fn = fn }) = unitNameSet fn
ruleLhsFreeNames (Rule { ru_fn = fn, ru_bndrs = tpl_vars, ru_args = tpl_args })
  = addOneToNameSet (exprsFreeNames tpl_args) fn

exprFreeNames :: CoreExpr -> NameSet
-- Find the free *external* names of an expression
exprFreeNames e
  = go e
  where
    go (Var v) 
      | isExternalName n    = unitNameSet n
      | otherwise	    = emptyNameSet
      where n = idName v
    go (Lit _) 	   	    = emptyNameSet
    go (Type ty)   	    = tyClsNamesOfType ty	-- Don't need free tyvars
    go (App e1 e2) 	    = go e1 `unionNameSets` go e2
    go (Lam v e)   	    = go e `delFromNameSet` idName v
    go (Note n e)  	    = go e  
    go (Cast e co)          = go e `unionNameSets` tyClsNamesOfType co
    go (Let (NonRec b r) e) = go e `unionNameSets` go r
    go (Let (Rec prs) e)    = exprsFreeNames (map snd prs) `unionNameSets` go e
    go (Case e b ty as)     = go e `unionNameSets` tyClsNamesOfType ty 
                              `unionNameSets` unionManyNameSets (map go_alt as)

    go_alt (_,_,r) = go r

exprsFreeNames es = foldr (unionNameSets . exprFreeNames) emptyNameSet es
\end{code}

%************************************************************************
%*									*
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
%*									*
%************************************************************************


\begin{code}
ruleRhsFreeVars :: CoreRule -> VarSet
ruleRhsFreeVars (BuiltinRule {}) = noFVs
ruleRhsFreeVars (Rule { ru_fn = fn, ru_bndrs = bndrs, ru_rhs = rhs })
  = delFromUFM fvs fn
	-- Hack alert!
	-- Don't include the Id in its own rhs free-var set.
	-- Otherwise the occurrence analyser makes bindings recursive
	-- that shoudn't be.  E.g.
	--	RULE:  f (f x y) z  ==>  f x (f y z)
  where
    fvs = addBndrs bndrs (expr_fvs rhs) isLocalVar emptyVarSet

rulesRhsFreeVars :: [CoreRule] -> VarSet
rulesRhsFreeVars rules
  = foldr (unionVarSet . ruleRhsFreeVars) emptyVarSet rules

ruleLhsFreeIds :: CoreRule -> VarSet
-- This finds all locally-defined free Ids on the LHS of the rule
ruleLhsFreeIds (BuiltinRule {}) = noFVs
ruleLhsFreeIds (Rule { ru_bndrs = bndrs, ru_args = args })
  = addBndrs bndrs (exprs_fvs args) isLocalId emptyVarSet
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

delBinderFV b s = (s `delVarSet` b) `unionFVs` varTypeTyVars b
	-- Include coercion variables too!

varTypeTyVars :: Var -> TyVarSet
-- Find the type variables free in the type of the variable
-- Remember, coercion variables can mention type variables...
varTypeTyVars var
  | isLocalId var || isCoVar var = tyVarsOfType (idType var)
  | otherwise = emptyVarSet	-- Global Ids and non-coercion TyVars

idFreeVars :: Id -> VarSet
idFreeVars id = ASSERT( isId id) idRuleVars id `unionVarSet` varTypeTyVars id

idRuleVars ::Id -> VarSet
idRuleVars id = ASSERT( isId id) specInfoFreeVars (idSpecialisation id)
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
  = (freeVarsOf rhs2 `unionFVs` body_fvs `unionFVs` idRuleVars binder,
		-- Remember any rules; cf rhs_fvs above
     AnnLet (AnnNonRec binder rhs2) body2)
  where
    rhs2     = freeVars rhs
    body2    = freeVars body
    body_fvs = binder `delBinderFV` freeVarsOf body2

freeVars (Let (Rec binds) body)
  = (delBindersFV binders all_fvs,
     AnnLet (AnnRec (binders `zip` rhss2)) body2)
  where
    (binders, rhss) = unzip binds

    rhss2     = map freeVars rhss
    rhs_body_fvs = foldr (unionFVs . freeVarsOf) body_fvs rhss2
    all_fvs      = foldr (unionFVs . idRuleVars) rhs_body_fvs binders
	-- The "delBinderFV" happens after adding the idSpecVars,
	-- since the latter may add some of the binders as fvs

    body2     = freeVars body
    body_fvs  = freeVarsOf body2


freeVars (Cast expr co)
  = (freeVarsOf expr2 `unionFVs` cfvs, AnnCast expr2 co)
  where
    expr2 = freeVars expr
    cfvs  = tyVarsOfType co

freeVars (Note other_note expr)
  = (freeVarsOf expr2, AnnNote other_note expr2)
  where
    expr2 = freeVars expr

freeVars (Type ty) = (tyVarsOfType ty, AnnType ty)
\end{code}

