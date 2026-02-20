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
        bndrTypeTyCoFVs, bndrFVs, dBndrFreeVars,
        idUnfoldingVars, bndrFreeVars,
        bndrRuleAndUnfoldingVarsDSet,
        bndrRuleAndUnfoldingVars,
        idRuleVars, stableUnfoldingVars,
        ruleFreeVars, rulesFreeVars,
        rulesFreeVarsDSet, mkRuleInfo,
        ruleLhsFreeIds, ruleLhsFreeIdsList,
        ruleRhsFreeVars, rulesRhsFreeIds,

        exprFVs, addBndrFV, addBndrsFV, unitFV,

        -- * Orphan names
        orphNamesOfType, orphNamesOfTypes, orphNamesOfAxiomLHS,
        orphNamesOfExprs,

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
import GHC.Builtin.Types( unrestrictedFunTyConName )
import GHC.Builtin.Types.Prim( fUNTyCon )
import GHC.Data.Maybe( orElse )

import GHC.Utils.EndoOS
import GHC.Utils.Misc
import GHC.Utils.Panic.Plain

{-
************************************************************************
*                                                                      *
       Find the shallow free variables of term
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
exprFreeVars = dVarSetToVarSet . exprFreeVarsDSet

-- | Find all locally-defined free Ids or type variables in an expression
-- returning a deterministic set.
exprFreeVarsDSet :: CoreExpr -> DVarSet
exprFreeVarsDSet = runFVSelective isLocalVar . exprFVs

exprsFreeVarsDSet :: [CoreExpr] -> DVarSet
exprsFreeVarsDSet = runFVSelective isLocalVar . exprsFVs

-- | Find all locally-defined free Ids or type variables in an expression
-- returning a deterministically ordered list.
exprFreeVarsList :: CoreExpr -> [Var]
exprFreeVarsList = dVarSetElems . exprFreeVarsDSet

-- | Find all locally-defined free Ids in an expression
exprFreeIds :: CoreExpr -> IdSet        -- Find all locally-defined free Ids
exprFreeIds = dVarSetToVarSet . exprFreeIdsDSet

exprsFreeIds :: [CoreExpr] -> IdSet        -- Find all locally-defined free Ids
exprsFreeIds = dVarSetToVarSet . exprsFreeIdsDSet

-- | Find all locally-defined free Ids in an expression
-- returning a deterministic set.
exprFreeIdsDSet :: CoreExpr -> DIdSet -- Find all locally-defined free Ids
exprFreeIdsDSet = runFVSelective isLocalId . exprFVs

-- | Find all locally-defined free Ids in several expressions
-- returning a deterministic set.
exprsFreeIdsDSet :: [CoreExpr] -> DIdSet -- Find all locally-defined free Ids
exprsFreeIdsDSet = runFVSelective isLocalId . exprsFVs

-- | Find all locally-defined free Ids in an expression
-- returning a deterministically ordered list.
exprFreeIdsList :: CoreExpr -> [Id] -- Find all locally-defined free Ids
exprFreeIdsList = dVarSetElems . exprFreeIdsDSet

-- | Find all locally-defined free Ids in several expressions
-- returning a deterministically ordered list.
exprsFreeIdsList :: [CoreExpr] -> [Id]   -- Find all locally-defined free Ids
exprsFreeIdsList = dVarSetElems . exprsFreeIdsDSet

-- | Find all locally-defined free Ids or type variables in several expressions
-- returning a non-deterministic set.
exprsFreeVars :: [CoreExpr] -> VarSet
exprsFreeVars = dVarSetToVarSet . exprsFreeVarsDSet

-- | Find all locally-defined free Ids or type variables in several expressions
-- returning a deterministically ordered list.
exprsFreeVarsList :: [CoreExpr] -> [Var]
exprsFreeVarsList = runFVSelectiveList isLocalVar . exprsFVs

-- | Find all locally defined free Ids in a binding group
bindFreeVars :: CoreBind -> VarSet
bindFreeVars = runFVSelectiveSet isLocalVar . bind_fvs

bind_fvs :: CoreBind -> SelectiveFVRes
bind_fvs (NonRec b r) = rhs_fvs (b,r)
bind_fvs (Rec prs)    = addBndrsSelectiveFVRes (map fst prs) $
                        mapUnionFVRes rhs_fvs prs

-- | Finds free variables in an expression selected by a predicate
exprSomeFreeVars :: InterestingVarFun   -- ^ Says which 'Var's are interesting
                 -> CoreExpr
                 -> VarSet
exprSomeFreeVars fv_cand = dVarSetToVarSet . exprSomeFreeVarsDSet fv_cand

-- | Finds free variables in an expression selected by a predicate
-- returning a deterministically ordered list.
exprSomeFreeVarsList :: InterestingVarFun -- ^ Says which 'Var's are interesting
                     -> CoreExpr
                     -> [Var]
exprSomeFreeVarsList fv_cand = dVarSetElems . exprSomeFreeVarsDSet fv_cand

-- | Finds free variables in an expression selected by a predicate
-- returning a deterministic set.
exprSomeFreeVarsDSet :: InterestingVarFun -- ^ Says which 'Var's are interesting
                     -> CoreExpr
                     -> DVarSet
exprSomeFreeVarsDSet fv_cand = runFVSelective fv_cand . exprFVs

-- | Finds free variables in several expressions selected by a predicate
exprsSomeFreeVars :: InterestingVarFun  -- Says which 'Var's are interesting
                  -> [CoreExpr]
                  -> VarSet
exprsSomeFreeVars fv_cand = dVarSetToVarSet . exprsSomeFreeVarsDSet fv_cand

-- | Finds free variables in several expressions selected by a predicate
-- returning a deterministically ordered list.
exprsSomeFreeVarsList :: InterestingVarFun  -- Says which 'Var's are interesting
                      -> [CoreExpr]
                      -> [Var]
exprsSomeFreeVarsList fv_cand = dVarSetElems . exprsSomeFreeVarsDSet fv_cand

-- | Finds free variables in several expressions selected by a predicate
-- returning a deterministic set.
exprsSomeFreeVarsDSet :: InterestingVarFun -- ^ Says which 'Var's are interesting
                      -> [CoreExpr]
                      -> DVarSet
exprsSomeFreeVarsDSet fv_cand = runFVSelective fv_cand . exprsFVs

addBndrFV :: CoreBndr -> SelectiveFVRes -> SelectiveFVRes
addBndrFV bndr fvr
  = bndrTypeTyCoFVs bndr `mappend`
        -- Include type variables in the binder's type
        --      (not just Ids; coercion variables too!)
    addBndrSelectiveFVRes bndr fvr

addBndrsFV :: [CoreBndr] -> SelectiveFVRes -> SelectiveFVRes
addBndrsFV bndrs fv = foldr addBndrFV fv bndrs

unitFV :: Var -> SelectiveFVRes
-- Deals with an occurrence
-- Shallow: does not look at the kind
unitFV v = FVRes (\bvs -> EndoOS (do_it bvs))
  where
    do_it (is_interesting,bvs) acc
      | not (is_interesting v) = acc  -- The "selective" bit
      | v `elemVarSet` bvs     = acc
      | v `elemDVarSet` acc    = acc
      | otherwise              = acc `extendDVarSet` v

exprsFVs :: [CoreExpr] -> SelectiveFVRes
exprsFVs = mapUnionFVRes exprFVs

exprFVs :: CoreExpr -> SelectiveFVRes
exprFVs (Type ty)       = tyCoFVsOfType ty
exprFVs (Coercion co)   = tyCoFVsOfCo co
exprFVs (Var var)       = unitFV var
exprFVs (Lit _)         = mempty
exprFVs (Tick t expr)   = tickish_fvs t `mappend` exprFVs expr
exprFVs (App fun arg)   = exprFVs fun `mappend` exprFVs arg
exprFVs (Lam bndr body) = addBndrFV bndr (exprFVs body)
exprFVs (Cast expr co)  = exprFVs expr `mappend` tyCoFVsOfCo co
exprFVs (Case scrut bndr ty alts)
  = exprFVs scrut `mappend` tyCoFVsOfType ty `mappend`
    addBndrFV bndr (mapUnionFVRes alt_fvs alts)
  where
    alt_fvs (Alt _ bndrs rhs) = addBndrsFV bndrs (exprFVs rhs)
exprFVs (Let (NonRec bndr rhs) body)
  = rhs_fvs (bndr, rhs) `mappend` addBndrFV bndr (exprFVs body)
exprFVs (Let (Rec pairs) body)
  = addBndrsFV (map fst pairs) $
    mapUnionFVRes rhs_fvs pairs `mappend` exprFVs body

---------
rhs_fvs :: (Id, CoreExpr) -> SelectiveFVRes
rhs_fvs (bndr, rhs) = exprFVs rhs `mappend`
                      bndrRuleAndUnfoldingFVs bndr
        -- Treat any RULES as extra RHSs of the binding

---------
tickish_fvs :: CoreTickish -> SelectiveFVRes
tickish_fvs (Breakpoint _ _ ids) = mapUnionFVRes unitFV ids
tickish_fvs _ = mempty

---------
bndrTypeTyCoFVs :: Var -> SelectiveFVRes
-- Find the free variables of a binder.
-- In the case of ids, don't forget the multiplicity field!
bndrTypeTyCoFVs var
  = tyCoFVsOfType (varType var) `mappend` mult_fvs
  where
    mult_fvs = case varMultMaybe var of
                 Just mult -> tyCoFVsOfType mult
                 Nothing   -> mempty

dBndrTypeTyCoVars :: Var -> DTyCoVarSet
-- Find the type/kind/coercion variables free in the type of the id/tyvar
dBndrTypeTyCoVars = runFVSelective isLocalVar . bndrTypeTyCoFVs

bndrFreeVars :: Id -> VarSet
bndrFreeVars id = assert (isId id) $
                  dVarSetToVarSet  $
                  dBndrFreeVars id

dBndrFreeVars :: Id -> DVarSet
-- Shallow free vars
dBndrFreeVars id = runFVSelective isLocalVar $ bndrFVs id

bndrFVs :: Id -> SelectiveFVRes
-- Shallow free vars of types, rules, and inlining
bndrFVs id = assert (isId id) $
             bndrTypeTyCoFVs id `mappend`
             bndrRuleAndUnfoldingFVs id

bndrRuleAndUnfoldingVarsDSet :: Id -> DVarSet
bndrRuleAndUnfoldingVarsDSet = runFVSelective isLocalVar . bndrRuleAndUnfoldingFVs

bndrRuleAndUnfoldingVars :: Id -> VarSet
bndrRuleAndUnfoldingVars = dVarSetToVarSet . bndrRuleAndUnfoldingVarsDSet

bndrRuleAndUnfoldingFVs :: Id -> SelectiveFVRes
bndrRuleAndUnfoldingFVs id
  | isId id   = idRuleFVs id `mappend` idUnfoldingFVs id
  | otherwise = mempty

idRuleVars :: Id -> VarSet  -- Does *not* include CoreUnfolding vars
idRuleVars = dVarSetToVarSet . ruleInfoFreeVars . idSpecialisation

idRuleFVs :: Id -> SelectiveFVRes
idRuleFVs id = assert (isId id) $
               strictFoldDVarSet (mappend . unitFV) mempty $
               ruleInfoFreeVars (idSpecialisation id)

idUnfoldingVars :: Id -> VarSet
-- Produce free vars for an unfolding, but NOT for an ordinary
-- (non-inline) unfolding, since it is a dup of the rhs
-- and we'll get exponential behaviour if we look at both unf and rhs!
-- But do look at the *real* unfolding, even for loop breakers, else
-- we might get out-of-scope variables
idUnfoldingVars = runFVSelectiveSet isLocalVar . idUnfoldingFVs

idUnfoldingFVs :: Id -> SelectiveFVRes
idUnfoldingFVs id = stableUnfoldingFVs (realIdUnfolding id) `orElse` mempty

stableUnfoldingVars :: Unfolding -> Maybe VarSet
stableUnfoldingVars unf = fmap (runFVSelectiveSet isLocalVar) $
                          stableUnfoldingFVs unf

stableUnfoldingFVs :: Unfolding -> Maybe SelectiveFVRes
stableUnfoldingFVs unf
  = case unf of
      CoreUnfolding { uf_tmpl = rhs, uf_src = src }
         | isStableSource src
         -> Just (exprFVs rhs)
      DFunUnfolding { df_bndrs = bndrs, df_args = args }
         -> Just (addBndrsFV bndrs (exprsFVs args))
            -- DFuns are top level, so no fvs from types of bndrs
      _other -> Nothing


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


{- **********************************************************************
%*                                                                      *
                    Orphan names
%*                                                                      *
%********************************************************************* -}

{- Note [Finding orphan names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions here (orphNamesOfType, orphNamesOfExpr etc) traverse a template:
  * the head of an class instance decl
  * the LHS of a type-family instance
  * the arguments of a RULE
to find TyCons or (in the case of a RULE) Ids, that will be matched against when
matching the template. If none of these orphNames are locally defined, the instance
or RULE is an orphan: see Note [Orphans] in GHC.Core

Wrinkles:
 (ON1) We do not need to look inside coercions, because we never match against
       them.  Indeed, it'd be wrong to do so, because it could make an instance
       into a non-orphan, when it really is an orphan.

 (ON2) These orphNames functions are also (rather separately) used by GHCi, to
       implement :info.  When you say ":info Foo", we show all the instances that
       involve `Foo`; that is, all the instances whose oprhNames include `Foo`.

       To support `:info (->)` we need to ensure that (->) is treated as an orphName
       of FunTy, which is a bit messy since the "real" TyCon is `FUN`
-}

orphNamesOfTyCon :: TyCon -> NameSet
orphNamesOfTyCon tycon = unitNameSet (getName tycon) `unionNameSet` case tyConClass_maybe tycon of
    Nothing  -> emptyNameSet
    Just cls -> unitNameSet (getName cls)

orphNamesOfType :: Type -> NameSet
orphNamesOfType ty | Just ty' <- coreView ty = orphNamesOfType ty'
                -- Look through type synonyms (#4912)
orphNamesOfType (TyVarTy _)          = emptyNameSet
orphNamesOfType (LitTy {})           = emptyNameSet
orphNamesOfType (ForAllTy bndr res)  = orphNamesOfType (binderType bndr)
                                       `unionNameSet` orphNamesOfType res
orphNamesOfType (AppTy fun arg)      = orphNamesOfType fun `unionNameSet` orphNamesOfType arg

orphNamesOfType (TyConApp tycon tys) = func
                                       `unionNameSet` orphNamesOfTyCon tycon
                                       `unionNameSet` orphNamesOfTypes tys
        where func = case tys of
                       arg:_ | tycon == fUNTyCon -> orph_names_of_fun_ty_con arg
                       _ -> emptyNameSet

orphNamesOfType (FunTy af w arg res) =  func
                                       `unionNameSet` unitNameSet fun_tc
                                       `unionNameSet` orphNamesOfType w
                                       `unionNameSet` orphNamesOfType arg
                                       `unionNameSet` orphNamesOfType res
        where func | isVisibleFunArg af = orph_names_of_fun_ty_con w
                   | otherwise          = emptyNameSet

              fun_tc = tyConName (funTyFlagTyCon af)

-- Coercions: see wrinkle (ON1) of Note [Finding orphan names]
orphNamesOfType (CastTy ty _co)  = orphNamesOfType ty
orphNamesOfType (CoercionTy _co) = emptyNameSet

orphNamesOfThings :: (a -> NameSet) -> [a] -> NameSet
orphNamesOfThings f = foldr (unionNameSet . f) emptyNameSet

orphNamesOfTypes :: [Type] -> NameSet
orphNamesOfTypes = orphNamesOfThings orphNamesOfType

-- | `orphNamesOfAxiomLHS` collects the names of the concrete types and
-- type constructors that make up the LHS of a type family instance,
-- including the family name itself.
--
-- For instance, given `type family Foo a b`:
-- `type instance Foo (F (G (H a))) b = ...` would yield [Foo,F,G,H]
--
-- Used (via orphNamesOfFamInst) in the implementation of ":info" in GHCi.
-- and when determining orphan-hood for a FamInst or module
orphNamesOfAxiomLHS :: CoAxiom br -> NameSet
orphNamesOfAxiomLHS axiom
  = (orphNamesOfTypes $ concatMap coAxBranchLHS $ fromBranches $ coAxiomBranches axiom)
    `extendNameSet` getName (coAxiomTyCon axiom)

-- Detect (FUN 'Many) as an application of (->), so that :i (->) works as expected
-- (see #8535) Issue #16475 describes a more robust solution
-- See wrinkle (ON2) of Note [Finding orphan names]
orph_names_of_fun_ty_con :: Mult -> NameSet
orph_names_of_fun_ty_con ManyTy = unitNameSet unrestrictedFunTyConName
orph_names_of_fun_ty_con _      = emptyNameSet

-- | Finds the free /external/ names of an expression, notably
-- including the names of type constructors (which of course do not show
-- up in 'exprFreeVars').
orphNamesOfExpr :: CoreExpr -> NameSet
-- There's no need to delete local binders, because they will all
-- be /internal/ names.
orphNamesOfExpr e
  = go e
  where
    go (Var v)
      | isExternalName n    = unitNameSet n
      | otherwise           = emptyNameSet
      where n = idName v
    go (Lit _)              = emptyNameSet
    go (Type ty)            = orphNamesOfType ty        -- Don't need free tyvars
    go (Coercion _co)       = emptyNameSet -- See wrinkle (ON1) of Note [Finding orphan names]
    go (App e1 e2)          = go e1 `unionNameSet` go e2
    go (Lam v e)            = go e `delFromNameSet` idName v
    go (Tick _ e)           = go e
    go (Cast e _co)         = go e  -- See wrinkle (ON1) of Note [Finding orphan names]
    go (Let (NonRec _ r) e) = go e `unionNameSet` go r
    go (Let (Rec prs) e)    = orphNamesOfExprs (map snd prs) `unionNameSet` go e
    go (Case e _ ty as)     = go e `unionNameSet` orphNamesOfType ty
                              `unionNameSet` unionNameSets (map go_alt as)

    go_alt (Alt _ _ r)      = go r

-- | Finds the free /external/ names of several expressions: see 'exprOrphNames' for details
orphNamesOfExprs :: [CoreExpr] -> NameSet
orphNamesOfExprs es = foldr (unionNameSet . orphNamesOfExpr) emptyNameSet es


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
-- of the rule, depending on the first argument.
ruleFVs :: RuleFVsFrom -> CoreRule -> SelectiveFVRes
ruleFVs !_   (BuiltinRule {}) = mempty
ruleFVs from (Rule { ru_fn = _do_not_include
                     -- See Note [Rule free var hack]
                   , ru_bndrs = bndrs
                   , ru_rhs = rhs, ru_args = args })
  = addBndrsFV bndrs (exprsFVs exprs)
  where
    exprs = case from of
      LhsOnly   -> args
      RhsOnly   -> [rhs]
      BothSides -> rhs:args

-- | Those locally-defined variables free in the left and/or right hand sides
-- from several rules, depending on the first argument.
rulesFVs :: RuleFVsFrom -> [CoreRule] -> SelectiveFVRes
rulesFVs from = mapUnionFVRes (ruleFVs from)

-- | Those variables free in the right hand side of a rule returned as a
-- non-deterministic set
ruleRhsFreeVars :: CoreRule -> VarSet
ruleRhsFreeVars = runFVSelectiveSet isLocalId . ruleFVs RhsOnly

-- | Those locally-defined free 'Id's in the right hand side of several rules
-- returned as a non-deterministic set
rulesRhsFreeIds :: [CoreRule] -> VarSet
rulesRhsFreeIds = runFVSelectiveSet isLocalId . rulesFVs RhsOnly

ruleLhsFreeIds :: CoreRule -> VarSet
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns them as a non-deterministic set
ruleLhsFreeIds = runFVSelectiveSet isLocalId . ruleFVs LhsOnly

ruleLhsFreeIdsList :: CoreRule -> [Var]
-- ^ This finds all locally-defined free Ids on the left hand side of a rule
-- and returns them as a deterministically ordered list
ruleLhsFreeIdsList = runFVSelectiveList isLocalId . ruleFVs LhsOnly

-- | Those variables free in the both the left right hand sides of a rule
-- returned as a non-deterministic set
ruleFreeVars :: CoreRule -> VarSet
ruleFreeVars = runFVSelectiveSet isLocalVar . ruleFVs BothSides

-- | Those variables free in the both the left right hand sides of rules
-- returned as a deterministic set
rulesFreeVarsDSet :: [CoreRule] -> DVarSet
rulesFreeVarsDSet = runFVSelective isLocalVar . rulesFVs BothSides

-- | Those variables free in both the left right hand sides of several rules
rulesFreeVars :: [CoreRule] -> VarSet
rulesFreeVars = dVarSetToVarSet . rulesFreeVarsDSet

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
          Attaching free variables to every sub-expression

   The free variable pass annotates every node in the expression
   with its DEEP (non-global) free variables and type variables.
*                                                                      *
************************************************************************

Note [The FVAnn invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant: a FVAnn, say S, is closed:
  That is: if v is in S,
           then freevars( v's type/kind ) is also in S
So FVAnn computes /deep/ free variables
-}

type FVAnn = DVarSet  -- See Note [The FVAnn invariant]

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

delBinderFV b s = (s `delDVarSet` b) `unionDVarSet` dBndrTypeTyCoVars b
        -- Include coercion variables too!

freeVarsBind :: CoreBind
             -> DVarSet                     -- Free vars of scope of binding
             -> (CoreBindWithFVs, DVarSet)  -- Return free vars of binding + scope
freeVarsBind (NonRec binder rhs) body_fvs
  = ( AnnNonRec binder rhs2
    , freeVarsOf rhs2 `unionDVarSet` body_fvs2
                      `unionDVarSet` bndrRuleAndUnfoldingVarsDSet binder )
    where
      rhs2      = freeVars rhs
      body_fvs2 = binder `delBinderFV` body_fvs

freeVarsBind (Rec binds) body_fvs
  = ( AnnRec (binders `zip` rhss2)
    , delBindersFV binders all_fvs )
  where
    (binders, rhss) = unzip binds
    rhss2        = map freeVars rhss
    rhs_body_fvs = foldr (unionDVarSet . freeVarsOf) body_fvs rhss2
    binders_fvs  = runFVSelective isLocalVar $ mapUnionFVRes bndrRuleAndUnfoldingFVs binders
                   -- See Note [The FVAnn invariant]
    all_fvs      = rhs_body_fvs `unionDVarSet` binders_fvs
            -- The "delBinderFV" happens after adding the idSpecVars,
            -- since the latter may add some of the binders as fvs

freeVars :: CoreExpr -> CoreExprWithFVs
-- ^ Annotate a 'CoreExpr' with its (non-global) free type
--   and value variables at every tree node.
freeVars = go
  where
    go :: CoreExpr -> CoreExprWithFVs
    go (Var v)
      | isLocalVar v = (aFreeVar v `unionDVarSet` ty_fvs `unionDVarSet` mult_vars, AnnVar v)
      | otherwise    = (emptyDVarSet,                 AnnVar v)
      where
        mult_vars = tyCoVarsOfTypeDSet (idMult v)
        ty_fvs = dBndrTypeTyCoVars v
                 -- See Note [The FVAnn invariant]

    go (Lit lit) = (emptyDVarSet, AnnLit lit)
    go (Lam b body)
      = ( b_fvs `unionDVarSet` (b `delBinderFV` body_fvs)
        , AnnLam b body' )
      where
        body'@(body_fvs, _) = go body
        b_ty  = idType b
        b_fvs = tyCoVarsOfTypeDSet b_ty
                -- See Note [The FVAnn invariant]

    go (App fun arg)
      = ( freeVarsOf fun' `unionDVarSet` freeVarsOf arg'
        , AnnApp fun' arg' )
      where
        fun'   = go fun
        arg'   = go arg

    go (Case scrut bndr ty alts)
      = ( (bndr `delBinderFV` alts_fvs)
           `unionDVarSet` freeVarsOf scrut2
           `unionDVarSet` tyCoVarsOfTypeDSet ty
          -- Don't need to look at (idType bndr)
          -- because that's redundant with scrut
        , AnnCase scrut2 bndr ty alts2 )
      where
        scrut2 = go scrut

        (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
        alts_fvs            = unionDVarSets alts_fvs_s

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
      = ( freeVarsOf expr2 `unionDVarSet` cfvs
        , AnnCast expr2 (cfvs, co) )
      where
        expr2 = go expr
        cfvs  = tyCoVarsOfCoDSet co

    go (Tick tickish expr)
      = ( tickishFVs tickish `unionDVarSet` freeVarsOf expr2
        , AnnTick tickish expr2 )
      where
        expr2 = go expr
        tickishFVs (Breakpoint _ _ ids) = mkDVarSet ids
        tickishFVs _                    = emptyDVarSet

    go (Type ty)     = (tyCoVarsOfTypeDSet ty, AnnType ty)
    go (Coercion co) = (tyCoVarsOfCoDSet co, AnnCoercion co)
