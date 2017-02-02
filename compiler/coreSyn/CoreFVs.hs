{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Taken quite directly from the Peyton Jones/Lester paper.
-}

{-# LANGUAGE CPP #-}

-- | A module concerned with finding the free variables of an expression.
module CoreFVs (
        -- * Free variables of expressions and binding groups
        exprFreeVars,
        exprFreeVarsDSet,
        exprFreeVarsList,
        exprFreeIds,
        exprFreeIdsDSet,
        exprFreeIdsList,
        exprsFreeIdsDSet,
        exprsFreeIdsList,
        exprsFreeVars,
        exprsFreeVarsList,
        bindFreeVars,

        -- * Selective free variables of expressions
        InterestingVarFun,
        exprSomeFreeVars, exprsSomeFreeVars,
        exprSomeFreeVarsList, exprsSomeFreeVarsList,

        -- * Free variables of Rules, Vars and Ids
        varTypeTyCoVars,
        varTypeTyCoFVs,
        idUnfoldingVars, idFreeVars, dIdFreeVars,
        idRuleAndUnfoldingVars, idRuleAndUnfoldingVarsDSet,
        idFVs,
        idRuleVars, idRuleRhsVars, stableUnfoldingVars,
        ruleRhsFreeVars, ruleFreeVars, rulesFreeVars,
        rulesFreeVarsDSet,
        ruleLhsFreeIds, ruleLhsFreeIdsList,
        vectsFreeVars,

        expr_fvs,

        -- * Orphan names
        orphNamesOfType, orphNamesOfCo, orphNamesOfAxiom,
        orphNamesOfTypes, orphNamesOfCoCon,
        exprsOrphNames, orphNamesOfFamInst,

        -- * Core syntax tree annotation with free variables
        FVAnn,                  -- annotation, abstract
        CoreExprWithFVs,        -- = AnnExpr Id FVAnn
        CoreExprWithFVs',       -- = AnnExpr' Id FVAnn
        CoreBindWithFVs,        -- = AnnBind Id FVAnn
        CoreAltWithFVs,         -- = AnnAlt Id FVAnn
        freeVars,               -- CoreExpr -> CoreExprWithFVs
        freeVarsOf,             -- CoreExprWithFVs -> DIdSet
        freeVarsOfType,         -- CoreExprWithFVs -> TyCoVarSet
        freeVarsOfAnn, freeVarsOfTypeAnn,
        exprTypeFV              -- CoreExprWithFVs -> Type
    ) where

#include "HsVersions.h"

import CoreSyn
import Id
import IdInfo
import NameSet
import UniqFM
import Literal ( literalType )
import Name
import VarSet
import Var
import Type
import TyCoRep
import TyCon
import CoAxiom
import FamInstEnv
import TysPrim( funTyConName )
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
-- returning a non-deterministic set.
exprFreeVars :: CoreExpr -> VarSet
exprFreeVars = fvVarSet . exprFVs

-- | Find all locally-defined free Ids or type variables in an expression
-- returning a composable FV computation. See Note [FV naming conventions] in FV
-- for why export it.
exprFVs :: CoreExpr -> FV
exprFVs = filterFV isLocalVar . expr_fvs

-- | Find all locally-defined free Ids or type variables in an expression
-- returning a deterministic set.
exprFreeVarsDSet :: CoreExpr -> DVarSet
exprFreeVarsDSet = fvDVarSet . exprFVs

-- | Find all locally-defined free Ids or type variables in an expression
-- returning a deterministically ordered list.
exprFreeVarsList :: CoreExpr -> [Var]
exprFreeVarsList = fvVarList . exprFVs

-- | Find all locally-defined free Ids in an expression
exprFreeIds :: CoreExpr -> IdSet        -- Find all locally-defined free Ids
exprFreeIds = exprSomeFreeVars isLocalId

-- | Find all locally-defined free Ids in an expression
-- returning a deterministic set.
exprFreeIdsDSet :: CoreExpr -> DIdSet -- Find all locally-defined free Ids
exprFreeIdsDSet = exprSomeFreeVarsDSet isLocalId

-- | Find all locally-defined free Ids in an expression
-- returning a deterministically ordered list.
exprFreeIdsList :: CoreExpr -> [Id] -- Find all locally-defined free Ids
exprFreeIdsList = exprSomeFreeVarsList isLocalId

-- | Find all locally-defined free Ids in several expressions
-- returning a deterministic set.
exprsFreeIdsDSet :: [CoreExpr] -> DIdSet -- Find all locally-defined free Ids
exprsFreeIdsDSet = exprsSomeFreeVarsDSet isLocalId

-- | Find all locally-defined free Ids in several expressions
-- returning a deterministically ordered list.
exprsFreeIdsList :: [CoreExpr] -> [Id]   -- Find all locally-defined free Ids
exprsFreeIdsList = exprsSomeFreeVarsList isLocalId

-- | Find all locally-defined free Ids or type variables in several expressions
-- returning a non-deterministic set.
exprsFreeVars :: [CoreExpr] -> VarSet
exprsFreeVars = fvVarSet . exprsFVs

-- | Find all locally-defined free Ids or type variables in several expressions
-- returning a composable FV computation. See Note [FV naming conventions] in FV
-- for why export it.
exprsFVs :: [CoreExpr] -> FV
exprsFVs exprs = mapUnionFV exprFVs exprs

-- | Find all locally-defined free Ids or type variables in several expressions
-- returning a deterministically ordered list.
exprsFreeVarsList :: [CoreExpr] -> [Var]
exprsFreeVarsList = fvVarList . exprsFVs

-- | Find all locally defined free Ids in a binding group
bindFreeVars :: CoreBind -> VarSet
bindFreeVars (NonRec b r) = fvVarSet $ filterFV isLocalVar $ rhs_fvs (b,r)
bindFreeVars (Rec prs)    = fvVarSet $ filterFV isLocalVar $
                                addBndrs (map fst prs)
                                     (mapUnionFV rhs_fvs prs)

-- | Finds free variables in an expression selected by a predicate
exprSomeFreeVars :: InterestingVarFun   -- ^ Says which 'Var's are interesting
                 -> CoreExpr
                 -> VarSet
exprSomeFreeVars fv_cand e = fvVarSet $ filterFV fv_cand $ expr_fvs e

-- | Finds free variables in an expression selected by a predicate
-- returning a deterministically ordered list.
exprSomeFreeVarsList :: InterestingVarFun -- ^ Says which 'Var's are interesting
                     -> CoreExpr
                     -> [Var]
exprSomeFreeVarsList fv_cand e = fvVarList $ filterFV fv_cand $ expr_fvs e

-- | Finds free variables in an expression selected by a predicate
-- returning a deterministic set.
exprSomeFreeVarsDSet :: InterestingVarFun -- ^ Says which 'Var's are interesting
                     -> CoreExpr
                     -> DVarSet
exprSomeFreeVarsDSet fv_cand e = fvDVarSet $ filterFV fv_cand $ expr_fvs e

-- | Finds free variables in several expressions selected by a predicate
exprsSomeFreeVars :: InterestingVarFun  -- Says which 'Var's are interesting
                  -> [CoreExpr]
                  -> VarSet
exprsSomeFreeVars fv_cand es =
  fvVarSet $ filterFV fv_cand $ mapUnionFV expr_fvs es

-- | Finds free variables in several expressions selected by a predicate
-- returning a deterministically ordered list.
exprsSomeFreeVarsList :: InterestingVarFun  -- Says which 'Var's are interesting
                      -> [CoreExpr]
                      -> [Var]
exprsSomeFreeVarsList fv_cand es =
  fvVarList $ filterFV fv_cand $ mapUnionFV expr_fvs es

-- | Finds free variables in several expressions selected by a predicate
-- returning a deterministic set.
exprsSomeFreeVarsDSet :: InterestingVarFun -- ^ Says which 'Var's are interesting
                      -> [CoreExpr]
                      -> DVarSet
exprsSomeFreeVarsDSet fv_cand e =
  fvDVarSet $ filterFV fv_cand $ mapUnionFV expr_fvs e

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

addBndr :: CoreBndr -> FV -> FV
addBndr bndr fv fv_cand in_scope acc
  = (varTypeTyCoFVs bndr `unionFV`
        -- Include type variables in the binder's type
        --      (not just Ids; coercion variables too!)
     FV.delFV bndr fv) fv_cand in_scope acc

addBndrs :: [CoreBndr] -> FV -> FV
addBndrs bndrs fv = foldr addBndr fv bndrs

expr_fvs :: CoreExpr -> FV
expr_fvs (Type ty) fv_cand in_scope acc =
  tyCoFVsOfType ty fv_cand in_scope acc
expr_fvs (Coercion co) fv_cand in_scope acc =
  tyCoFVsOfCo co fv_cand in_scope acc
expr_fvs (Var var) fv_cand in_scope acc = FV.unitFV var fv_cand in_scope acc
expr_fvs (Lit _) fv_cand in_scope acc = emptyFV fv_cand in_scope acc
expr_fvs (Tick t expr) fv_cand in_scope acc =
  (tickish_fvs t `unionFV` expr_fvs expr) fv_cand in_scope acc
expr_fvs (App fun arg) fv_cand in_scope acc =
  (expr_fvs fun `unionFV` expr_fvs arg) fv_cand in_scope acc
expr_fvs (Lam bndr body) fv_cand in_scope acc =
  addBndr bndr (expr_fvs body) fv_cand in_scope acc
expr_fvs (Cast expr co) fv_cand in_scope acc =
  (expr_fvs expr `unionFV` tyCoFVsOfCo co) fv_cand in_scope acc

expr_fvs (Case scrut bndr ty alts) fv_cand in_scope acc
  = (expr_fvs scrut `unionFV` tyCoFVsOfType ty `unionFV` addBndr bndr
      (mapUnionFV alt_fvs alts)) fv_cand in_scope acc
  where
    alt_fvs (_, bndrs, rhs) = addBndrs bndrs (expr_fvs rhs)

expr_fvs (Let (NonRec bndr rhs) body) fv_cand in_scope acc
  = (rhs_fvs (bndr, rhs) `unionFV` addBndr bndr (expr_fvs body))
      fv_cand in_scope acc

expr_fvs (Let (Rec pairs) body) fv_cand in_scope acc
  = addBndrs (map fst pairs)
             (mapUnionFV rhs_fvs pairs `unionFV` expr_fvs body)
               fv_cand in_scope acc

---------
rhs_fvs :: (Id, CoreExpr) -> FV
rhs_fvs (bndr, rhs) = expr_fvs rhs `unionFV`
                      bndrRuleAndUnfoldingFVs bndr
        -- Treat any RULES as extra RHSs of the binding

---------
exprs_fvs :: [CoreExpr] -> FV
exprs_fvs exprs = mapUnionFV expr_fvs exprs

tickish_fvs :: Tickish Id -> FV
tickish_fvs (Breakpoint _ ids) = FV.mkFVs ids
tickish_fvs _ = emptyFV

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


{- **********************************************************************
%*                                                                      *
                    orphNamesXXX

%*                                                                      *
%********************************************************************* -}

orphNamesOfTyCon :: TyCon -> NameSet
orphNamesOfTyCon tycon = unitNameSet (getName tycon) `unionNameSet` case tyConClass_maybe tycon of
    Nothing  -> emptyNameSet
    Just cls -> unitNameSet (getName cls)

orphNamesOfType :: Type -> NameSet
orphNamesOfType ty | Just ty' <- coreView ty = orphNamesOfType ty'
                -- Look through type synonyms (Trac #4912)
orphNamesOfType (TyVarTy _)          = emptyNameSet
orphNamesOfType (LitTy {})           = emptyNameSet
orphNamesOfType (TyConApp tycon tys) = orphNamesOfTyCon tycon
                                       `unionNameSet` orphNamesOfTypes tys
orphNamesOfType (ForAllTy bndr res)  = orphNamesOfType (binderKind bndr)
                                       `unionNameSet` orphNamesOfType res
orphNamesOfType (FunTy arg res)      = unitNameSet funTyConName    -- NB!  See Trac #8535
                                       `unionNameSet` orphNamesOfType arg
                                       `unionNameSet` orphNamesOfType res
orphNamesOfType (AppTy fun arg)      = orphNamesOfType fun `unionNameSet` orphNamesOfType arg
orphNamesOfType (CastTy ty co)       = orphNamesOfType ty `unionNameSet` orphNamesOfCo co
orphNamesOfType (CoercionTy co)      = orphNamesOfCo co

orphNamesOfThings :: (a -> NameSet) -> [a] -> NameSet
orphNamesOfThings f = foldr (unionNameSet . f) emptyNameSet

orphNamesOfTypes :: [Type] -> NameSet
orphNamesOfTypes = orphNamesOfThings orphNamesOfType

orphNamesOfCo :: Coercion -> NameSet
orphNamesOfCo (Refl _ ty)           = orphNamesOfType ty
orphNamesOfCo (TyConAppCo _ tc cos) = unitNameSet (getName tc) `unionNameSet` orphNamesOfCos cos
orphNamesOfCo (AppCo co1 co2)       = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (ForAllCo _ kind_co co)
  = orphNamesOfCo kind_co `unionNameSet` orphNamesOfCo co
orphNamesOfCo (FunCo _ co1 co2)     = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (CoVarCo _)           = emptyNameSet
orphNamesOfCo (AxiomInstCo con _ cos) = orphNamesOfCoCon con `unionNameSet` orphNamesOfCos cos
orphNamesOfCo (UnivCo p _ t1 t2)    = orphNamesOfProv p `unionNameSet` orphNamesOfType t1 `unionNameSet` orphNamesOfType t2
orphNamesOfCo (SymCo co)            = orphNamesOfCo co
orphNamesOfCo (TransCo co1 co2)     = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (NthCo _ co)          = orphNamesOfCo co
orphNamesOfCo (LRCo  _ co)          = orphNamesOfCo co
orphNamesOfCo (InstCo co arg)       = orphNamesOfCo co `unionNameSet` orphNamesOfCo arg
orphNamesOfCo (CoherenceCo co1 co2) = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (KindCo co)           = orphNamesOfCo co
orphNamesOfCo (SubCo co)            = orphNamesOfCo co
orphNamesOfCo (AxiomRuleCo _ cs)    = orphNamesOfCos cs

orphNamesOfProv :: UnivCoProvenance -> NameSet
orphNamesOfProv UnsafeCoerceProv    = emptyNameSet
orphNamesOfProv (PhantomProv co)    = orphNamesOfCo co
orphNamesOfProv (ProofIrrelProv co) = orphNamesOfCo co
orphNamesOfProv (PluginProv _)      = emptyNameSet
orphNamesOfProv (HoleProv _)        = emptyNameSet

orphNamesOfCos :: [Coercion] -> NameSet
orphNamesOfCos = orphNamesOfThings orphNamesOfCo

orphNamesOfCoCon :: CoAxiom br -> NameSet
orphNamesOfCoCon (CoAxiom { co_ax_tc = tc, co_ax_branches = branches })
  = orphNamesOfTyCon tc `unionNameSet` orphNamesOfCoAxBranches branches

orphNamesOfAxiom :: CoAxiom br -> NameSet
orphNamesOfAxiom axiom
  = orphNamesOfTypes (concatMap coAxBranchLHS $ fromBranches $ coAxiomBranches axiom)
    `extendNameSet` getName (coAxiomTyCon axiom)

orphNamesOfCoAxBranches :: Branches br -> NameSet
orphNamesOfCoAxBranches
  = foldr (unionNameSet . orphNamesOfCoAxBranch) emptyNameSet . fromBranches

orphNamesOfCoAxBranch :: CoAxBranch -> NameSet
orphNamesOfCoAxBranch (CoAxBranch { cab_lhs = lhs, cab_rhs = rhs })
  = orphNamesOfTypes lhs `unionNameSet` orphNamesOfType rhs

-- | orphNamesOfAxiom collects the names of the concrete types and
-- type constructors that make up the LHS of a type family instance,
-- including the family name itself.
--
-- For instance, given `type family Foo a b`:
-- `type instance Foo (F (G (H a))) b = ...` would yield [Foo,F,G,H]
--
-- Used in the implementation of ":info" in GHCi.
orphNamesOfFamInst :: FamInst -> NameSet
orphNamesOfFamInst fam_inst = orphNamesOfAxiom (famInstAxiom fam_inst)

{-
************************************************************************
*                                                                      *
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
*                                                                      *
************************************************************************
-}

-- | Those variables free in the right hand side of a rule returned as a
-- non-deterministic set
ruleRhsFreeVars :: CoreRule -> VarSet
ruleRhsFreeVars (BuiltinRule {}) = noFVs
ruleRhsFreeVars (Rule { ru_fn = _, ru_bndrs = bndrs, ru_rhs = rhs })
  = fvVarSet $ filterFV isLocalVar $ addBndrs bndrs (expr_fvs rhs)
      -- See Note [Rule free var hack]

-- | Those variables free in the both the left right hand sides of a rule
-- returned as a non-deterministic set
ruleFreeVars :: CoreRule -> VarSet
ruleFreeVars = fvVarSet . ruleFVs

-- | Those variables free in the both the left right hand sides of a rule
-- returned as FV computation
ruleFVs :: CoreRule -> FV
ruleFVs (BuiltinRule {}) = emptyFV
ruleFVs (Rule { ru_fn = _do_not_include
                  -- See Note [Rule free var hack]
              , ru_bndrs = bndrs
              , ru_rhs = rhs, ru_args = args })
  = filterFV isLocalVar $ addBndrs bndrs (exprs_fvs (rhs:args))

-- | Those variables free in the both the left right hand sides of rules
-- returned as FV computation
rulesFVs :: [CoreRule] -> FV
rulesFVs = mapUnionFV ruleFVs

-- | Those variables free in the both the left right hand sides of rules
-- returned as a deterministic set
rulesFreeVarsDSet :: [CoreRule] -> DVarSet
rulesFreeVarsDSet rules = fvDVarSet $ rulesFVs rules

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
        fvs = fvVarSet $ filterFV isLocalVar $ addBndrs bndrs (expr_fvs rhs)
    get_fvs _ = noFVs

-- | Those variables free in the right hand side of several rules
rulesFreeVars :: [CoreRule] -> VarSet
rulesFreeVars rules = mapUnionVarSet ruleFreeVars rules

ruleLhsFreeIds :: CoreRule -> VarSet
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns them as a non-deterministic set
ruleLhsFreeIds = fvVarSet . ruleLhsFVIds

ruleLhsFreeIdsList :: CoreRule -> [Var]
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns them as a determinisitcally ordered list
ruleLhsFreeIdsList = fvVarList . ruleLhsFVIds

ruleLhsFVIds :: CoreRule -> FV
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns an FV computation
ruleLhsFVIds (BuiltinRule {}) = emptyFV
ruleLhsFVIds (Rule { ru_bndrs = bndrs, ru_args = args })
  = filterFV isLocalId $ addBndrs bndrs (exprs_fvs args)

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
    vectFreeVars (Vect   _ rhs)   = fvVarSet $ filterFV isLocalId $ expr_fvs rhs
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

data FVAnn = FVAnn { fva_fvs    :: DVarSet   -- free in expression
                   , fva_ty_fvs :: DVarSet   -- free only in expression's type
                   , fva_ty     :: Type      -- expression's type
                   }

-- | Every node in a binding group annotated with its
-- (non-global) free variables, both Ids and TyVars, and type.
type CoreBindWithFVs = AnnBind Id FVAnn
-- | Every node in an expression annotated with its
-- (non-global) free variables, both Ids and TyVars, and type.
type CoreExprWithFVs  = AnnExpr Id FVAnn
type CoreExprWithFVs' = AnnExpr' Id FVAnn

-- | Every node in an expression annotated with its
-- (non-global) free variables, both Ids and TyVars, and type.
type CoreAltWithFVs = AnnAlt Id FVAnn

freeVarsOf :: CoreExprWithFVs -> DIdSet
-- ^ Inverse function to 'freeVars'
freeVarsOf (FVAnn { fva_fvs = fvs }, _) = fvs

-- | Extract the vars free in an annotated expression's type
freeVarsOfType :: CoreExprWithFVs -> DTyCoVarSet
freeVarsOfType (FVAnn { fva_ty_fvs = ty_fvs }, _) = ty_fvs

-- | Extract the type of an annotated expression. (This is cheap.)
exprTypeFV :: CoreExprWithFVs -> Type
exprTypeFV (FVAnn { fva_ty = ty }, _) = ty

-- | Extract the vars reported in a FVAnn
freeVarsOfAnn :: FVAnn -> DIdSet
freeVarsOfAnn = fva_fvs

-- | Extract the type-level vars reported in a FVAnn
freeVarsOfTypeAnn :: FVAnn -> DTyCoVarSet
freeVarsOfTypeAnn = fva_ty_fvs

noFVs :: VarSet
noFVs = emptyVarSet

aFreeVar :: Var -> DVarSet
aFreeVar = unitDVarSet

unionFVs :: DVarSet -> DVarSet -> DVarSet
unionFVs = unionDVarSet

unionFVss :: [DVarSet] -> DVarSet
unionFVss = unionDVarSets

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

delBinderFV b s = (s `delDVarSet` b) `unionFVs` dVarTypeTyCoVars b
        -- Include coercion variables too!

varTypeTyCoVars :: Var -> TyCoVarSet
-- Find the type/kind variables free in the type of the id/tyvar
varTypeTyCoVars var = fvVarSet $ varTypeTyCoFVs var

dVarTypeTyCoVars :: Var -> DTyCoVarSet
-- Find the type/kind/coercion variables free in the type of the id/tyvar
dVarTypeTyCoVars var = fvDVarSet $ varTypeTyCoFVs var

varTypeTyCoFVs :: Var -> FV
varTypeTyCoFVs var = tyCoFVsOfType (varType var)

idFreeVars :: Id -> VarSet
idFreeVars id = ASSERT( isId id) fvVarSet $ idFVs id

dIdFreeVars :: Id -> DVarSet
dIdFreeVars id = fvDVarSet $ idFVs id

idFVs :: Id -> FV
-- Type variables, rule variables, and inline variables
idFVs id = ASSERT( isId id)
           varTypeTyCoFVs id `unionFV`
           idRuleAndUnfoldingFVs id

bndrRuleAndUnfoldingFVs :: Var -> FV
bndrRuleAndUnfoldingFVs v | isTyVar v = emptyFV
                          | otherwise = idRuleAndUnfoldingFVs v

idRuleAndUnfoldingVars :: Id -> VarSet
idRuleAndUnfoldingVars id = fvVarSet $ idRuleAndUnfoldingFVs id

idRuleAndUnfoldingVarsDSet :: Id -> DVarSet
idRuleAndUnfoldingVarsDSet id = fvDVarSet $ idRuleAndUnfoldingFVs id

idRuleAndUnfoldingFVs :: Id -> FV
idRuleAndUnfoldingFVs id = ASSERT( isId id)
                           idRuleFVs id `unionFV` idUnfoldingFVs id


idRuleVars ::Id -> VarSet  -- Does *not* include CoreUnfolding vars
idRuleVars id = fvVarSet $ idRuleFVs id

idRuleFVs :: Id -> FV
idRuleFVs id = ASSERT( isId id)
  FV.mkFVs (dVarSetElems $ ruleInfoFreeVars (idSpecialisation id))

idUnfoldingVars :: Id -> VarSet
-- Produce free vars for an unfolding, but NOT for an ordinary
-- (non-inline) unfolding, since it is a dup of the rhs
-- and we'll get exponential behaviour if we look at both unf and rhs!
-- But do look at the *real* unfolding, even for loop breakers, else
-- we might get out-of-scope variables
idUnfoldingVars id = fvVarSet $ idUnfoldingFVs id

idUnfoldingFVs :: Id -> FV
idUnfoldingFVs id = stableUnfoldingFVs (realIdUnfolding id) `orElse` emptyFV

stableUnfoldingVars :: Unfolding -> Maybe VarSet
stableUnfoldingVars unf = fvVarSet `fmap` stableUnfoldingFVs unf

stableUnfoldingFVs :: Unfolding -> Maybe FV
stableUnfoldingFVs unf
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
freeVars = go
  where
    go :: CoreExpr -> CoreExprWithFVs
    go (Var v)
      = (FVAnn fvs ty_fvs (idType v), AnnVar v)
      where
            -- ToDo: insert motivating example for why we *need*
            -- to include the idSpecVars in the FV list.
            --      Actually [June 98] I don't think it's necessary
            -- fvs = fvs_v `unionVarSet` idSpecVars v

        (fvs, ty_fvs)
            | isLocalVar v = (aFreeVar v `unionFVs` ty_fvs, dVarTypeTyCoVars v)
            | otherwise    = (emptyDVarSet, emptyDVarSet)

    go (Lit lit) = (FVAnn emptyDVarSet emptyDVarSet (literalType lit), AnnLit lit)
    go (Lam b body)
      = ( FVAnn { fva_fvs    = b_fvs `unionFVs` (b `delBinderFV` body_fvs)
                , fva_ty_fvs = b_fvs `unionFVs` (b `delBinderFV` body_ty_fvs)
                , fva_ty     = mkFunTy b_ty body_ty }
        , AnnLam b body' )
      where
        body'@(FVAnn { fva_fvs = body_fvs, fva_ty_fvs = body_ty_fvs
                     , fva_ty = body_ty }, _) = go body
        b_ty  = idType b
        b_fvs = tyCoVarsOfTypeDSet b_ty

    go (App fun arg)
      = ( FVAnn { fva_fvs    = freeVarsOf fun' `unionFVs` freeVarsOf arg'
                , fva_ty_fvs = tyCoVarsOfTypeDSet res_ty
                , fva_ty     = res_ty }
        , AnnApp fun' arg' )
      where
        fun'   = go fun
        fun_ty = exprTypeFV fun'
        arg'   = go arg
        res_ty = applyTypeToArg fun_ty arg

    go (Case scrut bndr ty alts)
      = ( FVAnn { fva_fvs = (bndr `delBinderFV` alts_fvs)
                            `unionFVs` freeVarsOf scrut2
                            `unionFVs` tyCoVarsOfTypeDSet ty
                           -- don't need to look at (idType bndr)
                           -- b/c that's redundant with scrut
                , fva_ty_fvs = tyCoVarsOfTypeDSet ty
                , fva_ty     = ty }
        , AnnCase scrut2 bndr ty alts2 )
      where
        scrut2 = go scrut

        (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
        alts_fvs            = unionFVss alts_fvs_s

        fv_alt (con,args,rhs) = (delBindersFV args (freeVarsOf rhs2),
                                 (con, args, rhs2))
                              where
                                 rhs2 = go rhs

    go (Let (NonRec binder rhs) body)
      = ( FVAnn { fva_fvs    = freeVarsOf rhs2
                               `unionFVs` body_fvs
                               `unionFVs` fvDVarSet
                                            (bndrRuleAndUnfoldingFVs binder)
                               -- Remember any rules; cf rhs_fvs above
                , fva_ty_fvs = freeVarsOfType body2
                , fva_ty     = exprTypeFV body2 }
        , AnnLet (AnnNonRec binder rhs2) body2 )
      where
        rhs2     = go rhs
        body2    = go body
        body_fvs = binder `delBinderFV` freeVarsOf body2

    go (Let (Rec binds) body)
      = ( FVAnn { fva_fvs    = delBindersFV binders all_fvs
                , fva_ty_fvs = freeVarsOfType body2
                , fva_ty     = exprTypeFV body2 }
        , AnnLet (AnnRec (binders `zip` rhss2)) body2 )
      where
        (binders, rhss) = unzip binds

        rhss2        = map go rhss
        rhs_body_fvs = foldr (unionFVs . freeVarsOf) body_fvs rhss2
        binders_fvs  = fvDVarSet $ mapUnionFV idRuleAndUnfoldingFVs binders
        all_fvs      = rhs_body_fvs `unionFVs` binders_fvs
            -- The "delBinderFV" happens after adding the idSpecVars,
            -- since the latter may add some of the binders as fvs

        body2    = go body
        body_fvs = freeVarsOf body2

    go (Cast expr co)
      = ( FVAnn (freeVarsOf expr2 `unionFVs` cfvs) (tyCoVarsOfTypeDSet to_ty) to_ty
        , AnnCast expr2 (c_ann, co) )
      where
        expr2 = go expr
        cfvs  = tyCoVarsOfCoDSet co
        c_ann = FVAnn cfvs (tyCoVarsOfTypeDSet co_ki) co_ki
        co_ki = coercionType co
        Just (_, to_ty) = splitCoercionType_maybe co_ki


    go (Tick tickish expr)
      = ( FVAnn { fva_fvs    = tickishFVs tickish `unionFVs` freeVarsOf expr2
                , fva_ty_fvs = freeVarsOfType expr2
                , fva_ty     = exprTypeFV expr2 }
        , AnnTick tickish expr2 )
      where
        expr2 = go expr
        tickishFVs (Breakpoint _ ids) = mkDVarSet ids
        tickishFVs _                  = emptyDVarSet

    go (Type ty) = ( FVAnn (tyCoVarsOfTypeDSet ty)
                           (tyCoVarsOfTypeDSet ki)
                           ki
                   , AnnType ty)
      where
        ki = typeKind ty

    go (Coercion co) = ( FVAnn (tyCoVarsOfCoDSet co)
                               (tyCoVarsOfTypeDSet ki)
                               ki
                       , AnnCoercion co)
      where
        ki = coercionType co
