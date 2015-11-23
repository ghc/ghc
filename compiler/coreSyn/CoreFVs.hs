{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Taken quite directly from the Peyton Jones/Lester paper.
-}

{-# LANGUAGE CPP #-}

-- | A module concerned with finding the free variables of an expression.
module CoreFVs (
        -- * Free variables of expressions and binding groups
        exprFreeVars,   -- CoreExpr   -> VarSet -- Find all locally-defined free Ids or tyvars
        exprFreeDVars,  -- CoreExpr   -> DVarSet -- Find all locally-defined free Ids or tyvars
        exprFreeIds,    -- CoreExpr   -> IdSet  -- Find all locally-defined free Ids
        exprsFreeVars,  -- [CoreExpr] -> VarSet
        bindFreeVars,   -- CoreBind   -> VarSet

        -- * Selective free variables of expressions
        InterestingVarFun,
        exprSomeFreeVars, exprsSomeFreeVars,

        -- * Free variables of Rules, Vars and Ids
        varTypeTyVars,
        varTypeTyVarsAcc,
        idUnfoldingVars, idFreeVars, idRuleAndUnfoldingVars,
        idFreeVarsAcc,
        idRuleVars, idRuleRhsVars, stableUnfoldingVars,
        ruleRhsFreeVars, ruleFreeVars, rulesFreeVars,
        rulesFreeDVars,
        ruleLhsFreeIds, exprsOrphNames,
        vectsFreeVars,

        expr_fvs,

        -- * Core syntax tree annotation with free variables
        CoreExprWithFVs,        -- = AnnExpr Id DVarSet
        CoreBindWithFVs,        -- = AnnBind Id DVarSet
        freeVars,               -- CoreExpr -> CoreExprWithFVs
        freeVarsOf              -- CoreExprWithFVs -> DIdSet
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
import TypeRep
import Coercion
import Maybes( orElse )
import Util
import BasicTypes( Activation )
import Outputable
import FV

{-
************************************************************************
*                                                                      *
\section{Finding the free variables of an expression}
*                                                                      *
************************************************************************

This function simply finds the free variables of an expression.
So far as type variables are concerned, it only finds tyvars that are

        * free in type arguments,
        * free in the type of a binder,

but not those that are free in the type of variable occurrence.
-}

-- | Find all locally-defined free Ids or type variables in an expression
exprFreeVars :: CoreExpr -> VarSet
exprFreeVars = runFVSet . filterFV isLocalVar . expr_fvs

exprFreeDVars :: CoreExpr -> DVarSet
exprFreeDVars = runFVDSet . filterFV isLocalVar . expr_fvs


-- | Find all locally-defined free Ids in an expression
exprFreeIds :: CoreExpr -> IdSet        -- Find all locally-defined free Ids
exprFreeIds = exprSomeFreeVars isLocalId

-- | Find all locally-defined free Ids or type variables in several expressions
exprsFreeVars :: [CoreExpr] -> VarSet
exprsFreeVars = mapUnionVarSet exprFreeVars

-- | Find all locally defined free Ids in a binding group
bindFreeVars :: CoreBind -> VarSet
bindFreeVars (NonRec b r) = runFVSet $ filterFV isLocalVar $ rhs_fvs (b,r)
bindFreeVars (Rec prs)    = runFVSet $ filterFV isLocalVar $
                                addBndrs (map fst prs)
                                     (foldr (unionFV . rhs_fvs) noVars prs)

-- | Finds free variables in an expression selected by a predicate
exprSomeFreeVars :: InterestingVarFun   -- ^ Says which 'Var's are interesting
                 -> CoreExpr
                 -> VarSet
exprSomeFreeVars fv_cand e = runFVSet $ filterFV fv_cand $ expr_fvs e

-- | Finds free variables in several expressions selected by a predicate
exprsSomeFreeVars :: InterestingVarFun  -- Says which 'Var's are interesting
                  -> [CoreExpr]
                  -> VarSet
exprsSomeFreeVars fv_cand es =
  runFVSet $ filterFV fv_cand $ foldr (unionFV . expr_fvs) noVars es

--      Comment about obselete code
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
--      f  = ...g...
--      g  = ...
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
--                          | otherwise                    = set
--      SLPJ Feb06

-- XXX move to FV
someVars :: [Var] -> FV
someVars vars = foldr (unionFV . oneVar) noVars vars


addBndr :: CoreBndr -> FV -> FV
addBndr bndr fv fv_cand in_scope acc
  = (varTypeTyVarsAcc bndr `unionFV`
        -- Include type varibles in the binder's type
        --      (not just Ids; coercion variables too!)
     FV.delFV bndr fv) fv_cand in_scope acc

addBndrs :: [CoreBndr] -> FV -> FV
addBndrs bndrs fv = foldr addBndr fv bndrs

expr_fvs :: CoreExpr -> FV

expr_fvs (Type ty) fv_cand in_scope acc =
  tyVarsOfTypeAcc ty fv_cand in_scope acc
expr_fvs (Coercion co) fv_cand in_scope acc =
  tyCoVarsOfCoAcc co fv_cand in_scope acc
expr_fvs (Var var) fv_cand in_scope acc = oneVar var fv_cand in_scope acc
expr_fvs (Lit _) fv_cand in_scope acc = noVars fv_cand in_scope acc
expr_fvs (Tick t expr) fv_cand in_scope acc =
  (tickish_fvs t `unionFV` expr_fvs expr) fv_cand in_scope acc
expr_fvs (App fun arg) fv_cand in_scope acc =
  (expr_fvs fun `unionFV` expr_fvs arg) fv_cand in_scope acc
expr_fvs (Lam bndr body) fv_cand in_scope acc =
  addBndr bndr (expr_fvs body) fv_cand in_scope acc
expr_fvs (Cast expr co) fv_cand in_scope acc =
  (expr_fvs expr `unionFV` tyCoVarsOfCoAcc co) fv_cand in_scope acc

expr_fvs (Case scrut bndr ty alts) fv_cand in_scope acc
  = (expr_fvs scrut `unionFV` tyVarsOfTypeAcc ty `unionFV` addBndr bndr
      (foldr (unionFV . alt_fvs) noVars alts)) fv_cand in_scope acc
  where
    alt_fvs (_, bndrs, rhs) = addBndrs bndrs (expr_fvs rhs)

expr_fvs (Let (NonRec bndr rhs) body) fv_cand in_scope acc
  = (rhs_fvs (bndr, rhs) `unionFV` addBndr bndr (expr_fvs body))
      fv_cand in_scope acc

expr_fvs (Let (Rec pairs) body) fv_cand in_scope acc
  = addBndrs (map fst pairs)
             (foldr (unionFV . rhs_fvs) (expr_fvs body) pairs)
               fv_cand in_scope acc

---------
rhs_fvs :: (Id, CoreExpr) -> FV
rhs_fvs (bndr, rhs) = expr_fvs rhs `unionFV`
                      bndrRuleAndUnfoldingVarsAcc bndr -- XXX: FIXME
        -- Treat any RULES as extra RHSs of the binding

---------
exprs_fvs :: [CoreExpr] -> FV
exprs_fvs exprs = foldr (unionFV . expr_fvs) noVars exprs

tickish_fvs :: Tickish Id -> FV
tickish_fvs (Breakpoint _ ids) = someVars ids
tickish_fvs _ = noVars

{-
************************************************************************
*                                                                      *
\section{Free names}
*                                                                      *
************************************************************************
-}

-- | Finds the free /external/ names of an expression, notably
-- including the names of type constructors (which of course do not show
-- up in 'exprFreeVars').
exprOrphNames :: CoreExpr -> NameSet
-- There's no need to delete local binders, because they will all
-- be /internal/ names.
exprOrphNames e
  = go e
  where
    go (Var v)
      | isExternalName n    = unitNameSet n
      | otherwise           = emptyNameSet
      where n = idName v
    go (Lit _)              = emptyNameSet
    go (Type ty)            = orphNamesOfType ty        -- Don't need free tyvars
    go (Coercion co)        = orphNamesOfCo co
    go (App e1 e2)          = go e1 `unionNameSet` go e2
    go (Lam v e)            = go e `delFromNameSet` idName v
    go (Tick _ e)           = go e
    go (Cast e co)          = go e `unionNameSet` orphNamesOfCo co
    go (Let (NonRec _ r) e) = go e `unionNameSet` go r
    go (Let (Rec prs) e)    = exprsOrphNames (map snd prs) `unionNameSet` go e
    go (Case e _ ty as)     = go e `unionNameSet` orphNamesOfType ty
                              `unionNameSet` unionNameSets (map go_alt as)

    go_alt (_,_,r) = go r

-- | Finds the free /external/ names of several expressions: see 'exprOrphNames' for details
exprsOrphNames :: [CoreExpr] -> NameSet
exprsOrphNames es = foldr (unionNameSet . exprOrphNames) emptyNameSet es

{-
************************************************************************
*                                                                      *
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
*                                                                      *
************************************************************************
-}

-- | Those variables free in the right hand side of a rule
ruleRhsFreeVars :: CoreRule -> VarSet
ruleRhsFreeVars (BuiltinRule {}) = noFVs
ruleRhsFreeVars (Rule { ru_fn = _, ru_bndrs = bndrs, ru_rhs = rhs })
  = runFVSet $ filterFV isLocalVar $ addBndrs bndrs (expr_fvs rhs)
      -- See Note [Rule free var hack]

-- | Those variables free in the both the left right hand sides of a rule
ruleFreeVars :: CoreRule -> VarSet
ruleFreeVars (BuiltinRule {}) = noFVs
ruleFreeVars (Rule { ru_fn = _do_not_include  -- See Note [Rule free var hack]
                   , ru_bndrs = bndrs
                   , ru_rhs = rhs, ru_args = args })
  = runFVSet $ filterFV isLocalVar $ addBndrs bndrs (exprs_fvs (rhs:args))

ruleFreeVarsAcc :: CoreRule -> FV
ruleFreeVarsAcc (BuiltinRule {}) =
  noVars
ruleFreeVarsAcc (Rule { ru_fn = _do_not_include  -- See Note [Rule free var hack]
                      , ru_bndrs = bndrs
                      , ru_rhs = rhs, ru_args = args })
  = addBndrs bndrs (exprs_fvs (rhs:args))

rulesFreeVarsAcc :: [CoreRule] -> FV
rulesFreeVarsAcc (rule:rules) = ruleFreeVarsAcc rule `unionFV` rulesFreeVarsAcc rules
rulesFreeVarsAcc [] = noVars

rulesFreeDVars :: [CoreRule] -> DVarSet
rulesFreeDVars rules = runFVDSet $ filterFV isLocalVar $ rulesFreeVarsAcc rules


idRuleRhsVars :: (Activation -> Bool) -> Id -> VarSet
-- Just the variables free on the *rhs* of a rule
idRuleRhsVars is_active id
  = mapUnionVarSet get_fvs (idCoreRules id)
  where
    get_fvs (Rule { ru_fn = fn, ru_bndrs = bndrs
                  , ru_rhs = rhs, ru_act = act })
      | is_active act
            -- See Note [Finding rule RHS free vars] in OccAnal.hs
      = delFromUFM fvs fn        -- Note [Rule free var hack]
      where
        fvs = runFVSet $ filterFV isLocalVar $ addBndrs bndrs (expr_fvs rhs)
    get_fvs _ = noFVs

-- | Those variables free in the right hand side of several rules
rulesFreeVars :: [CoreRule] -> VarSet
rulesFreeVars rules = mapUnionVarSet ruleFreeVars rules

ruleLhsFreeIds :: CoreRule -> VarSet
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
ruleLhsFreeIds (BuiltinRule {}) = noFVs
ruleLhsFreeIds (Rule { ru_bndrs = bndrs, ru_args = args })
  = runFVSet $ filterFV isLocalId $ addBndrs bndrs (exprs_fvs args)

{-
Note [Rule free var hack]  (Not a hack any more)
~~~~~~~~~~~~~~~~~~~~~~~~~
We used not to include the Id in its own rhs free-var set.
Otherwise the occurrence analyser makes bindings recursive:
        f x y = x+y
        RULE:  f (f x y) z  ==>  f x (f y z)
However, the occurrence analyser distinguishes "non-rule loop breakers"
from "rule-only loop breakers" (see BasicTypes.OccInfo).  So it will
put this 'f' in a Rec block, but will mark the binding as a non-rule loop
breaker, which is perfectly inlinable.
-}

-- |Free variables of a vectorisation declaration
vectsFreeVars :: [CoreVect] -> VarSet
vectsFreeVars = mapUnionVarSet vectFreeVars
  where
    vectFreeVars (Vect   _ rhs)   = runFVSet $ filterFV isLocalId $ expr_fvs rhs
    vectFreeVars (NoVect _)       = noFVs
    vectFreeVars (VectType _ _ _) = noFVs
    vectFreeVars (VectClass _)    = noFVs
    vectFreeVars (VectInst _)     = noFVs
      -- this function is only concerned with values, not types

{-
************************************************************************
*                                                                      *
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
*                                                                      *
************************************************************************

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.
-}

-- | Every node in a binding group annotated with its
-- (non-global) free variables, both Ids and TyVars
type CoreBindWithFVs = AnnBind Id DVarSet
-- | Every node in an expression annotated with its
-- (non-global) free variables, both Ids and TyVars
type CoreExprWithFVs = AnnExpr Id DVarSet

freeVarsOf :: CoreExprWithFVs -> DIdSet
-- ^ Inverse function to 'freeVars'
freeVarsOf (free_vars, _) = free_vars

noFVs :: VarSet
noFVs = emptyVarSet

aFreeVar :: Var -> DVarSet
aFreeVar = unitDVarSet

unionFVs :: DVarSet -> DVarSet -> DVarSet
unionFVs = unionDVarSet

delBindersFV :: [Var] -> DVarSet -> DVarSet
delBindersFV bs fvs = foldr delBinderFV fvs bs

delBinderFV :: Var -> DVarSet -> DVarSet
-- This way round, so we can do it multiple times using foldr

-- (b `delBinderFV` s) removes the binder b from the free variable set s,
-- but *adds* to s
--
--      the free variables of b's type
--
-- This is really important for some lambdas:
--      In (\x::a -> x) the only mention of "a" is in the binder.
--
-- Also in
--      let x::a = b in ...
-- we should really note that "a" is free in this expression.
-- It'll be pinned inside the /\a by the binding for b, but
-- it seems cleaner to make sure that a is in the free-var set
-- when it is mentioned.
--
-- This also shows up in recursive bindings.  Consider:
--      /\a -> letrec x::a = x in E
-- Now, there are no explicit free type variables in the RHS of x,
-- but nevertheless "a" is free in its definition.  So we add in
-- the free tyvars of the types of the binders, and include these in the
-- free vars of the group, attached to the top level of each RHS.
--
-- This actually happened in the defn of errorIO in IOBase.hs:
--      errorIO (ST io) = case (errorIO# io) of
--                          _ -> bottom
--                        where
--                          bottom = bottom -- Never evaluated

delBinderFV b s = (s `delDVarSet` b) `unionFVs` dVarTypeTyVars b
        -- Include coercion variables too!

varTypeTyVars :: Var -> TyVarSet
-- Find the type/kind variables free in the type of the id/tyvar
varTypeTyVars var = runFVSet $ varTypeTyVarsAcc var

dVarTypeTyVars :: Var -> DTyVarSet
-- Find the type/kind variables free in the type of the id/tyvar
dVarTypeTyVars var = runFVDSet $ varTypeTyVarsAcc var

varTypeTyVarsAcc :: Var -> FV
varTypeTyVarsAcc var = tyVarsOfTypeAcc (varType var)

idFreeVars :: Id -> VarSet
idFreeVars id = ASSERT( isId id) runFVSet $ idFreeVarsAcc id

idFreeVarsAcc :: Id -> FV
-- Type variables, rule variables, and inline variables
idFreeVarsAcc id = ASSERT( isId id)
                   varTypeTyVarsAcc id `unionFV`
                   idRuleAndUnfoldingVarsAcc id

bndrRuleAndUnfoldingVarsAcc :: Var -> FV
bndrRuleAndUnfoldingVarsAcc v | isTyVar v = noVars
                              | otherwise = idRuleAndUnfoldingVarsAcc v

idRuleAndUnfoldingVars :: Id -> VarSet
idRuleAndUnfoldingVars id = runFVSet $ idRuleAndUnfoldingVarsAcc id

idRuleAndUnfoldingVarsAcc :: Id -> FV
idRuleAndUnfoldingVarsAcc id = ASSERT( isId id)
                               idRuleVarsAcc id `unionFV` idUnfoldingVarsAcc id


idRuleVars ::Id -> VarSet  -- Does *not* include CoreUnfolding vars
idRuleVars id = runFVSet $ idRuleVarsAcc id

idRuleVarsAcc :: Id -> FV
idRuleVarsAcc id = ASSERT( isId id)
  someVars (dVarSetElems $ ruleInfoFreeVars (idSpecialisation id))

idUnfoldingVars :: Id -> VarSet
-- Produce free vars for an unfolding, but NOT for an ordinary
-- (non-inline) unfolding, since it is a dup of the rhs
-- and we'll get exponential behaviour if we look at both unf and rhs!
-- But do look at the *real* unfolding, even for loop breakers, else
-- we might get out-of-scope variables
idUnfoldingVars id = runFVSet $ idUnfoldingVarsAcc id

idUnfoldingVarsAcc :: Id -> FV
idUnfoldingVarsAcc id = stableUnfoldingVarsAcc (realIdUnfolding id) `orElse` noVars

stableUnfoldingVars :: Unfolding -> Maybe VarSet
stableUnfoldingVars unf = runFVSet `fmap` stableUnfoldingVarsAcc unf

stableUnfoldingVarsAcc :: Unfolding -> Maybe FV
stableUnfoldingVarsAcc unf
  = case unf of
      CoreUnfolding { uf_tmpl = rhs, uf_src = src }
         | isStableSource src
         -> Just (filterFV isLocalVar $ expr_fvs rhs)
      DFunUnfolding { df_bndrs = bndrs, df_args = args }
         -> Just (filterFV isLocalVar $ FV.delFVs (mkVarSet bndrs) $ exprs_fvs args)
            -- DFuns are top level, so no fvs from types of bndrs
      _other -> Nothing


{-
************************************************************************
*                                                                      *
\subsection{Free variables (and types)}
*                                                                      *
************************************************************************
-}

freeVars :: CoreExpr -> CoreExprWithFVs
-- ^ Annotate a 'CoreExpr' with its (non-global) free type and value variables at every tree node
freeVars (Var v)
  = (fvs, AnnVar v)
  where
        -- ToDo: insert motivating example for why we *need*
        -- to include the idSpecVars in the FV list.
        --      Actually [June 98] I don't think it's necessary
        -- fvs = fvs_v `unionVarSet` idSpecVars v

    fvs | isLocalVar v = aFreeVar v
        | otherwise    = emptyDVarSet

freeVars (Lit lit) = (emptyDVarSet, AnnLit lit)
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
  = ((bndr `delBinderFV` alts_fvs) `unionFVs` freeVarsOf scrut2 `unionFVs` runFVDSet (tyVarsOfTypeAcc ty),
     AnnCase scrut2 bndr ty alts2)
  where
    scrut2 = freeVars scrut

    (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
    alts_fvs            = foldr unionFVs emptyDVarSet alts_fvs_s

    fv_alt (con,args,rhs) = (delBindersFV args (freeVarsOf rhs2),
                             (con, args, rhs2))
                          where
                             rhs2 = freeVars rhs

freeVars (Let (NonRec binder rhs) body)
  = (freeVarsOf rhs2
       `unionFVs` body_fvs
       `unionFVs` runFVDSet (bndrRuleAndUnfoldingVarsAcc binder),
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
    binders_fvs = runFVDSet $ foldr (unionFV . idRuleAndUnfoldingVarsAcc) noVars binders
    all_fvs      = rhs_body_fvs `unionFVs` binders_fvs
        -- The "delBinderFV" happens after adding the idSpecVars,
        -- since the latter may add some of the binders as fvs

    body2     = freeVars body
    body_fvs  = freeVarsOf body2

freeVars (Cast expr co)
  = (freeVarsOf expr2 `unionFVs` cfvs, AnnCast expr2 (cfvs, co))
  where
    expr2 = freeVars expr
    cfvs  = runFVDSet $ tyCoVarsOfCoAcc co

freeVars (Tick tickish expr)
  = (tickishFVs tickish `unionFVs` freeVarsOf expr2, AnnTick tickish expr2)
  where
    expr2 = freeVars expr
    tickishFVs (Breakpoint _ ids) = mkDVarSet ids
    tickishFVs _                  = emptyDVarSet

freeVars (Type ty) = (runFVDSet $ tyVarsOfTypeAcc ty, AnnType ty)

freeVars (Coercion co) = (runFVDSet $ tyCoVarsOfCoAcc co, AnnCoercion co)
