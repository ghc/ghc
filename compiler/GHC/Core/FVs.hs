{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Taken quite directly from the Peyton Jones/Lester paper.
-}

{-# LANGUAGE TypeFamilies #-}

-- | A module concerned with finding the free variables of an expression.
module GHC.Core.FVs (
        -- * Free variables of expressions and binding groups
        exprFreeVars,     exprsFreeVars,
        exprFreeVarsDSet,
        exprFreeVarsList, exprsFreeVarsList,
        exprFreeIds,      exprsFreeIds,
        exprFreeIdsDSet,  exprsFreeIdsDSet,
        exprFreeIdsList,  exprsFreeIdsList,
        bindFreeVars,

        -- * Selective free variables of expressions
        InterestingVarFun,
        exprSomeFreeVars, exprsSomeFreeVars,
        exprSomeFreeVarsList, exprsSomeFreeVarsList,

        -- * Free variables of Rules, Vars and Ids
        varTypeTyCoVars,
        varTypeTyCoFVs,
        idUnfoldingVars, idFreeVars, dIdFreeVars,
        bndrRuleAndUnfoldingVarsDSet,
        bndrRuleAndUnfoldingIdsList,
        idFVs,
        idRuleVars, stableUnfoldingVars,
        ruleFreeVars, rulesFreeVars,
        rulesFreeVarsDSet, mkRuleInfo,
        ruleLhsFreeIds, ruleLhsFreeIdsList,
        ruleRhsFreeVars, rulesRhsFreeIds,

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
        freeVarsBind,           -- CoreBind -> DVarSet -> (DVarSet, CoreBindWithFVs)
        freeVarsOf,             -- CoreExprWithFVs -> DIdSet
        freeVarsOfAnn
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Types.Tickish
import GHC.Types.Var.Set
import GHC.Types.Var
import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.FamInstEnv
import GHC.Builtin.Types( unrestrictedFunTyConName )
import GHC.Builtin.Types.Prim( funTyConName )
import GHC.Data.Maybe( orElse )

import GHC.Utils.FV as FV
import GHC.Utils.Misc
import GHC.Utils.Panic.Plain

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
-- returning a composable FV computation. See Note [FV naming conventions] in "GHC.Utils.FV"
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

exprsFreeIds :: [CoreExpr] -> IdSet        -- Find all locally-defined free Ids
exprsFreeIds = exprsSomeFreeVars isLocalId

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
-- returning a composable FV computation. See Note [FV naming conventions] in "GHC.Utils.FV"
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

--      Comment about obsolete code
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
    alt_fvs (Alt _ bndrs rhs) = addBndrs bndrs (expr_fvs rhs)

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

tickish_fvs :: CoreTickish -> FV
tickish_fvs (Breakpoint _ _ ids) = FV.mkFVs ids
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

    go_alt (Alt _ _ r)      = go r

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
                -- Look through type synonyms (#4912)
orphNamesOfType (TyVarTy _)          = emptyNameSet
orphNamesOfType (LitTy {})           = emptyNameSet
orphNamesOfType (TyConApp tycon tys) = func
                                       `unionNameSet` orphNamesOfTyCon tycon
                                       `unionNameSet` orphNamesOfTypes tys
        where func = case tys of
                       arg:_ | tycon == funTyCon -> orph_names_of_fun_ty_con arg
                       _ -> emptyNameSet
orphNamesOfType (ForAllTy bndr res)  = orphNamesOfType (binderType bndr)
                                       `unionNameSet` orphNamesOfType res
orphNamesOfType (FunTy _ w arg res)  =  orph_names_of_fun_ty_con w
                                       `unionNameSet` unitNameSet funTyConName
                                       `unionNameSet` orphNamesOfType w
                                       `unionNameSet` orphNamesOfType arg
                                       `unionNameSet` orphNamesOfType res
orphNamesOfType (AppTy fun arg)      = orphNamesOfType fun `unionNameSet` orphNamesOfType arg
orphNamesOfType (CastTy ty co)       = orphNamesOfType ty `unionNameSet` orphNamesOfCo co
orphNamesOfType (CoercionTy co)      = orphNamesOfCo co

orphNamesOfThings :: (a -> NameSet) -> [a] -> NameSet
orphNamesOfThings f = foldr (unionNameSet . f) emptyNameSet

orphNamesOfTypes :: [Type] -> NameSet
orphNamesOfTypes = orphNamesOfThings orphNamesOfType

orphNamesOfMCo :: MCoercion -> NameSet
orphNamesOfMCo MRefl    = emptyNameSet
orphNamesOfMCo (MCo co) = orphNamesOfCo co

orphNamesOfCo :: Coercion -> NameSet
orphNamesOfCo (Refl ty)             = orphNamesOfType ty
orphNamesOfCo (GRefl _ ty mco)      = orphNamesOfType ty `unionNameSet` orphNamesOfMCo mco
orphNamesOfCo (TyConAppCo _ tc cos) = unitNameSet (getName tc) `unionNameSet` orphNamesOfCos cos
orphNamesOfCo (AppCo co1 co2)       = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (ForAllCo _ kind_co co)
  = orphNamesOfCo kind_co `unionNameSet` orphNamesOfCo co
orphNamesOfCo (FunCo _ co_mult co1 co2) = orphNamesOfCo co_mult `unionNameSet` orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (CoVarCo _)           = emptyNameSet
orphNamesOfCo (AxiomInstCo con _ cos) = orphNamesOfCoCon con `unionNameSet` orphNamesOfCos cos
orphNamesOfCo (UnivCo p _ t1 t2)    = orphNamesOfProv p `unionNameSet` orphNamesOfType t1 `unionNameSet` orphNamesOfType t2
orphNamesOfCo (SymCo co)            = orphNamesOfCo co
orphNamesOfCo (TransCo co1 co2)     = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (NthCo _ _ co)        = orphNamesOfCo co
orphNamesOfCo (LRCo  _ co)          = orphNamesOfCo co
orphNamesOfCo (InstCo co arg)       = orphNamesOfCo co `unionNameSet` orphNamesOfCo arg
orphNamesOfCo (KindCo co)           = orphNamesOfCo co
orphNamesOfCo (SubCo co)            = orphNamesOfCo co
orphNamesOfCo (AxiomRuleCo _ cs)    = orphNamesOfCos cs
orphNamesOfCo (HoleCo _)            = emptyNameSet

orphNamesOfProv :: UnivCoProvenance -> NameSet
orphNamesOfProv (PhantomProv co)    = orphNamesOfCo co
orphNamesOfProv (ProofIrrelProv co) = orphNamesOfCo co
orphNamesOfProv (PluginProv _)      = emptyNameSet
orphNamesOfProv (CorePrepProv _)    = emptyNameSet

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

-- Detect FUN 'Many as an application of (->), so that :i (->) works as expected
-- (see #8535) Issue #16475 describes a more robust solution
orph_names_of_fun_ty_con :: Mult -> NameSet
orph_names_of_fun_ty_con Many = unitNameSet unrestrictedFunTyConName
orph_names_of_fun_ty_con _ = emptyNameSet

{-
************************************************************************
*                                                                      *
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
*                                                                      *
************************************************************************
-}

data RuleFVsFrom
  = LhsOnly
  | RhsOnly
  | BothSides

-- | Those locally-defined variables free in the left and/or right hand sides
-- of the rule, depending on the first argument. Returns an 'FV' computation.
ruleFVs :: RuleFVsFrom -> CoreRule -> FV
ruleFVs !_   (BuiltinRule {}) = emptyFV
ruleFVs from (Rule { ru_fn = _do_not_include
                     -- See Note [Rule free var hack]
                   , ru_bndrs = bndrs
                   , ru_rhs = rhs, ru_args = args })
  = filterFV isLocalVar $ addBndrs bndrs (exprs_fvs exprs)
  where
    exprs = case from of
      LhsOnly   -> args
      RhsOnly   -> [rhs]
      BothSides -> rhs:args

-- | Those locally-defined variables free in the left and/or right hand sides
-- from several rules, depending on the first argument.
-- Returns an 'FV' computation.
rulesFVs :: RuleFVsFrom -> [CoreRule] -> FV
rulesFVs from = mapUnionFV (ruleFVs from)

-- | Those variables free in the right hand side of a rule returned as a
-- non-deterministic set
ruleRhsFreeVars :: CoreRule -> VarSet
ruleRhsFreeVars = fvVarSet . ruleFVs RhsOnly

-- | Those locally-defined free 'Id's in the right hand side of several rules
-- returned as a non-deterministic set
rulesRhsFreeIds :: [CoreRule] -> VarSet
rulesRhsFreeIds = fvVarSet . filterFV isLocalId . rulesFVs RhsOnly

ruleLhsFreeIds :: CoreRule -> VarSet
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns them as a non-deterministic set
ruleLhsFreeIds = fvVarSet . filterFV isLocalId . ruleFVs LhsOnly

ruleLhsFreeIdsList :: CoreRule -> [Var]
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns them as a deterministically ordered list
ruleLhsFreeIdsList = fvVarList . filterFV isLocalId . ruleFVs LhsOnly

-- | Those variables free in the both the left right hand sides of a rule
-- returned as a non-deterministic set
ruleFreeVars :: CoreRule -> VarSet
ruleFreeVars = fvVarSet . ruleFVs BothSides

-- | Those variables free in the both the left right hand sides of rules
-- returned as a deterministic set
rulesFreeVarsDSet :: [CoreRule] -> DVarSet
rulesFreeVarsDSet rules = fvDVarSet $ rulesFVs BothSides rules

-- | Those variables free in both the left right hand sides of several rules
rulesFreeVars :: [CoreRule] -> VarSet
rulesFreeVars rules = fvVarSet $ rulesFVs BothSides rules

-- | Make a 'RuleInfo' containing a number of 'CoreRule's, suitable
-- for putting into an 'IdInfo'
mkRuleInfo :: [CoreRule] -> RuleInfo
mkRuleInfo rules = RuleInfo rules (rulesFreeVarsDSet rules)

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

{-
************************************************************************
*                                                                      *
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
*                                                                      *
************************************************************************

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.
-}

type FVAnn = DVarSet  -- See Note [The FVAnn invariant]

{- Note [The FVAnn invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant: a FVAnn, say S, is closed:
  That is: if v is in S,
           then freevars( v's type/kind ) is also in S
-}

-- | Every node in a binding group annotated with its
-- (non-global) free variables, both Ids and TyVars, and type.
type CoreBindWithFVs = AnnBind Id FVAnn

-- | Every node in an expression annotated with its
-- (non-global) free variables, both Ids and TyVars, and type.
-- NB: see Note [The FVAnn invariant]
type CoreExprWithFVs  = AnnExpr  Id FVAnn
type CoreExprWithFVs' = AnnExpr' Id FVAnn

-- | Every node in an expression annotated with its
-- (non-global) free variables, both Ids and TyVars, and type.
type CoreAltWithFVs = AnnAlt Id FVAnn

freeVarsOf :: CoreExprWithFVs -> DIdSet
-- ^ Inverse function to 'freeVars'
freeVarsOf (fvs, _) = fvs

-- | Extract the vars reported in a FVAnn
freeVarsOfAnn :: FVAnn -> DIdSet
freeVarsOfAnn fvs = fvs

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

-- (b `delBinderFV` s)
--   * removes the binder b from the free variable set s,
--   * AND *adds* to s the free variables of b's type
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
idFreeVars id = assert (isId id) $ fvVarSet $ idFVs id

dIdFreeVars :: Id -> DVarSet
dIdFreeVars id = fvDVarSet $ idFVs id

idFVs :: Id -> FV
-- Type variables, rule variables, and inline variables
idFVs id = assert (isId id) $
           varTypeTyCoFVs id `unionFV`
           bndrRuleAndUnfoldingFVs id

bndrRuleAndUnfoldingVarsDSet :: Id -> DVarSet
bndrRuleAndUnfoldingVarsDSet id = fvDVarSet $ bndrRuleAndUnfoldingFVs id

bndrRuleAndUnfoldingIdsList :: Id -> [Id]
bndrRuleAndUnfoldingIdsList id = fvVarList $ filterFV isId $ bndrRuleAndUnfoldingFVs id

bndrRuleAndUnfoldingFVs :: Id -> FV
bndrRuleAndUnfoldingFVs id
  | isId id   = idRuleFVs id `unionFV` idUnfoldingFVs id
  | otherwise = emptyFV

idRuleVars ::Id -> VarSet  -- Does *not* include CoreUnfolding vars
idRuleVars id = fvVarSet $ idRuleFVs id

idRuleFVs :: Id -> FV
idRuleFVs id = assert (isId id) $
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

freeVarsBind :: CoreBind
             -> DVarSet                     -- Free vars of scope of binding
             -> (CoreBindWithFVs, DVarSet)  -- Return free vars of binding + scope
freeVarsBind (NonRec binder rhs) body_fvs
  = ( AnnNonRec binder rhs2
    , freeVarsOf rhs2 `unionFVs` body_fvs2
                      `unionFVs` bndrRuleAndUnfoldingVarsDSet binder )
    where
      rhs2      = freeVars rhs
      body_fvs2 = binder `delBinderFV` body_fvs

freeVarsBind (Rec binds) body_fvs
  = ( AnnRec (binders `zip` rhss2)
    , delBindersFV binders all_fvs )
  where
    (binders, rhss) = unzip binds
    rhss2        = map freeVars rhss
    rhs_body_fvs = foldr (unionFVs . freeVarsOf) body_fvs rhss2
    binders_fvs  = fvDVarSet $ mapUnionFV bndrRuleAndUnfoldingFVs binders
                   -- See Note [The FVAnn invariant]
    all_fvs      = rhs_body_fvs `unionFVs` binders_fvs
            -- The "delBinderFV" happens after adding the idSpecVars,
            -- since the latter may add some of the binders as fvs

freeVars :: CoreExpr -> CoreExprWithFVs
-- ^ Annotate a 'CoreExpr' with its (non-global) free type
--   and value variables at every tree node.
freeVars = go
  where
    go :: CoreExpr -> CoreExprWithFVs
    go (Var v)
      | isLocalVar v = (aFreeVar v `unionFVs` ty_fvs `unionFVs` mult_vars, AnnVar v)
      | otherwise    = (emptyDVarSet,                 AnnVar v)
      where
        mult_vars = tyCoVarsOfTypeDSet (varMult v)
        ty_fvs = dVarTypeTyCoVars v
                 -- See Note [The FVAnn invariant]

    go (Lit lit) = (emptyDVarSet, AnnLit lit)
    go (Lam b body)
      = ( b_fvs `unionFVs` (b `delBinderFV` body_fvs)
        , AnnLam b body' )
      where
        body'@(body_fvs, _) = go body
        b_ty  = idType b
        b_fvs = tyCoVarsOfTypeDSet b_ty
                -- See Note [The FVAnn invariant]

    go (App fun arg)
      = ( freeVarsOf fun' `unionFVs` freeVarsOf arg'
        , AnnApp fun' arg' )
      where
        fun'   = go fun
        arg'   = go arg

    go (Case scrut bndr ty alts)
      = ( (bndr `delBinderFV` alts_fvs)
           `unionFVs` freeVarsOf scrut2
           `unionFVs` tyCoVarsOfTypeDSet ty
          -- Don't need to look at (idType bndr)
          -- because that's redundant with scrut
        , AnnCase scrut2 bndr ty alts2 )
      where
        scrut2 = go scrut

        (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
        alts_fvs            = unionFVss alts_fvs_s

        fv_alt (Alt con args rhs) = (delBindersFV args (freeVarsOf rhs2),
                                     (AnnAlt con args rhs2))
                              where
                                 rhs2 = go rhs

    go (Let bind body)
      = (bind_fvs, AnnLet bind2 body2)
      where
        (bind2, bind_fvs) = freeVarsBind bind (freeVarsOf body2)
        body2             = go body

    go (Cast expr co)
      = ( freeVarsOf expr2 `unionFVs` cfvs
        , AnnCast expr2 (cfvs, co) )
      where
        expr2 = go expr
        cfvs  = tyCoVarsOfCoDSet co

    go (Tick tickish expr)
      = ( tickishFVs tickish `unionFVs` freeVarsOf expr2
        , AnnTick tickish expr2 )
      where
        expr2 = go expr
        tickishFVs (Breakpoint _ _ ids) = mkDVarSet ids
        tickishFVs _                    = emptyDVarSet

    go (Type ty)     = (tyCoVarsOfTypeDSet ty, AnnType ty)
    go (Coercion co) = (tyCoVarsOfCoDSet co, AnnCoercion co)
