{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


TcRules: Typechecking transformation rules
-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module TcRules ( tcRules ) where

import HsSyn
import TcRnMonad
import TcSimplify
import TcMType
import TcType
import TcHsType
import TcExpr
import TcEnv
import TcUnify( buildImplicationFor )
import TcEvidence( mkTcCoVarCo )
import Type
import Id
import Var( EvVar )
import BasicTypes       ( RuleName )
import SrcLoc
import Outputable
import FastString
import Bag
import Data.List( partition )

{-
Note [Typechecking rules]
~~~~~~~~~~~~~~~~~~~~~~~~~
We *infer* the typ of the LHS, and use that type to *check* the type of
the RHS.  That means that higher-rank rules work reasonably well. Here's
an example (test simplCore/should_compile/rule2.hs) produced by Roman:

   foo :: (forall m. m a -> m b) -> m a -> m b
   foo f = ...

   bar :: (forall m. m a -> m a) -> m a -> m a
   bar f = ...

   {-# RULES "foo/bar" foo = bar #-}

He wanted the rule to typecheck.
-}

tcRules :: [LRuleDecls GhcRn] -> TcM [LRuleDecls GhcTcId]
tcRules decls = mapM (wrapLocM tcRuleDecls) decls

tcRuleDecls :: RuleDecls GhcRn -> TcM (RuleDecls GhcTcId)
tcRuleDecls (HsRules src decls)
   = do { tc_decls <- mapM (wrapLocM tcRule) decls
        ; return (HsRules src tc_decls) }

tcRule :: RuleDecl GhcRn -> TcM (RuleDecl GhcTcId)
tcRule (HsRule name act hs_bndrs lhs fv_lhs rhs fv_rhs)
  = addErrCtxt (ruleCtxt $ snd $ unLoc name)  $
    do { traceTc "---- Rule ------" (pprFullRuleName name)

        -- Note [Typechecking rules]
       ; (vars, bndr_wanted) <- captureConstraints $
                                tcRuleBndrs hs_bndrs
              -- bndr_wanted constraints can include wildcard hole
              -- constraints, which we should not forget about.
              -- It may mention the skolem type variables bound by
              -- the RULE.  c.f. Trac #10072

       ; let (id_bndrs, tv_bndrs) = partition isId vars
       ; (lhs', lhs_wanted, rhs', rhs_wanted, rule_ty)
            <- tcExtendTyVarEnv tv_bndrs $
               tcExtendIdEnv    id_bndrs $
               do { -- See Note [Solve order for RULES]
                    ((lhs', rule_ty), lhs_wanted) <- captureConstraints (tcInferRho lhs)
                  ; (rhs', rhs_wanted) <- captureConstraints $
                                          tcMonoExpr rhs (mkCheckExpType rule_ty)
                  ; return (lhs', lhs_wanted, rhs', rhs_wanted, rule_ty) }

       ; traceTc "tcRule 1" (vcat [ pprFullRuleName name
                                  , ppr lhs_wanted
                                  , ppr rhs_wanted ])
       ; let all_lhs_wanted = bndr_wanted `andWC` lhs_wanted
       ; (lhs_evs, residual_lhs_wanted) <- simplifyRule (snd $ unLoc name)
                                              all_lhs_wanted
                                              rhs_wanted

       -- SimplfyRule Plan, step 4
       -- Now figure out what to quantify over
       -- c.f. TcSimplify.simplifyInfer
       -- We quantify over any tyvars free in *either* the rule
       --  *or* the bound variables.  The latter is important.  Consider
       --      ss (x,(y,z)) = (x,z)
       --      RULE:  forall v. fst (ss v) = fst v
       -- The type of the rhs of the rule is just a, but v::(a,(b,c))
       --
       -- We also need to get the completely-uconstrained tyvars of
       -- the LHS, lest they otherwise get defaulted to Any; but we do that
       -- during zonking (see TcHsSyn.zonkRule)

       ; let tpl_ids = lhs_evs ++ id_bndrs
       ; forall_tkvs <- zonkTcTypesAndSplitDepVars $
                        rule_ty : map idType tpl_ids
       ; gbls  <- tcGetGlobalTyCoVars -- Even though top level, there might be top-level
                                      -- monomorphic bindings from the MR; test tc111
       ; qtkvs <- quantifyTyVars gbls forall_tkvs
       ; traceTc "tcRule" (vcat [ pprFullRuleName name
                                , ppr forall_tkvs
                                , ppr qtkvs
                                , ppr rule_ty
                                , vcat [ ppr id <+> dcolon <+> ppr (idType id) | id <- tpl_ids ]
                  ])

       -- SimplfyRule Plan, step 5
       -- Simplify the LHS and RHS constraints:
       -- For the LHS constraints we must solve the remaining constraints
       -- (a) so that we report insoluble ones
       -- (b) so that we bind any soluble ones
       ; let skol_info = RuleSkol (snd (unLoc name))
       ; (lhs_implic, lhs_binds) <- buildImplicationFor topTcLevel skol_info qtkvs
                                         lhs_evs residual_lhs_wanted
       ; (rhs_implic, rhs_binds) <- buildImplicationFor topTcLevel skol_info qtkvs
                                         lhs_evs rhs_wanted

       ; emitImplications (lhs_implic `unionBags` rhs_implic)
       ; return (HsRule name act
                    (map (noLoc . RuleBndr . noLoc) (qtkvs ++ tpl_ids))
                    (mkHsDictLet lhs_binds lhs') fv_lhs
                    (mkHsDictLet rhs_binds rhs') fv_rhs) }

tcRuleBndrs :: [LRuleBndr GhcRn] -> TcM [Var]
tcRuleBndrs []
  = return []
tcRuleBndrs (L _ (RuleBndr (L _ name)) : rule_bndrs)
  = do  { ty <- newOpenFlexiTyVarTy
        ; vars <- tcRuleBndrs rule_bndrs
        ; return (mkLocalId name ty : vars) }
tcRuleBndrs (L _ (RuleBndrSig (L _ name) rn_ty) : rule_bndrs)
--  e.g         x :: a->a
--  The tyvar 'a' is brought into scope first, just as if you'd written
--              a::*, x :: a->a
  = do  { let ctxt = RuleSigCtxt name
        ; (_ , tvs, id_ty) <- tcHsPatSigType ctxt rn_ty
        ; let id  = mkLocalIdOrCoVar name id_ty
                    -- See Note [Pattern signature binders] in TcHsType

              -- The type variables scope over subsequent bindings; yuk
        ; vars <- tcExtendTyVarEnv tvs $
                  tcRuleBndrs rule_bndrs
        ; return (tvs ++ id : vars) }

ruleCtxt :: FastString -> SDoc
ruleCtxt name = text "When checking the transformation rule" <+>
                doubleQuotes (ftext name)


{-
*********************************************************************************
*                                                                                 *
              Constraint simplification for rules
*                                                                                 *
***********************************************************************************

Note [The SimplifyRule Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example.  Consider the following left-hand side of a rule
        f (x == y) (y > z) = ...
If we typecheck this expression we get constraints
        d1 :: Ord a, d2 :: Eq a
We do NOT want to "simplify" to the LHS
        forall x::a, y::a, z::a, d1::Ord a.
          f ((==) (eqFromOrd d1) x y) ((>) d1 y z) = ...
Instead we want
        forall x::a, y::a, z::a, d1::Ord a, d2::Eq a.
          f ((==) d2 x y) ((>) d1 y z) = ...

Here is another example:
        fromIntegral :: (Integral a, Num b) => a -> b
        {-# RULES "foo"  fromIntegral = id :: Int -> Int #-}
In the rule, a=b=Int, and Num Int is a superclass of Integral Int. But
we *dont* want to get
        forall dIntegralInt.
           fromIntegral Int Int dIntegralInt (scsel dIntegralInt) = id Int
because the scsel will mess up RULE matching.  Instead we want
        forall dIntegralInt, dNumInt.
          fromIntegral Int Int dIntegralInt dNumInt = id Int

Even if we have
        g (x == y) (y == z) = ..
where the two dictionaries are *identical*, we do NOT WANT
        forall x::a, y::a, z::a, d1::Eq a
          f ((==) d1 x y) ((>) d1 y z) = ...
because that will only match if the dict args are (visibly) equal.
Instead we want to quantify over the dictionaries separately.

In short, simplifyRuleLhs must *only* squash equalities, leaving
all dicts unchanged, with absolutely no sharing.

Also note that we can't solve the LHS constraints in isolation:
Example   foo :: Ord a => a -> a
          foo_spec :: Int -> Int
          {-# RULE "foo"  foo = foo_spec #-}
Here, it's the RHS that fixes the type variable

HOWEVER, under a nested implication things are different
Consider
  f :: (forall a. Eq a => a->a) -> Bool -> ...
  {-# RULES "foo" forall (v::forall b. Eq b => b->b).
       f b True = ...
    #-}
Here we *must* solve the wanted (Eq a) from the given (Eq a)
resulting from skolemising the argument type of g.  So we
revert to SimplCheck when going under an implication.


--------- So the SimplifyRule Plan is this -----------------------

* Step 0: typecheck the LHS and RHS to get constraints from each

* Step 1: Simplify the LHS and RHS constraints all together in one bag
          We do this to discover all unification equalities

* Step 2: Zonk the ORIGINAL (unsimplified) LHS constraints, to take
          advantage of those unifications

* Setp 3: Partition the LHS constraints into the ones we will
          quantify over, and the others.
          See Note [RULE quantification over equalities]

* Step 4: Decide on the type variables to quantify over

* Step 5: Simplify the LHS and RHS constraints separately, using the
          quantified constraints as givens

Note [Solve order for RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In step 1 above, we need to be a bit careful about solve order.
Consider
   f :: Int -> T Int
   type instance T Int = Bool

   RULE f 3 = True

From the RULE we get
   lhs-constraints:  T Int ~ alpha
   rhs-constraints:  Bool ~ alpha
where 'alpha' is the type that connects the two.  If we glom them
all together, and solve the RHS constraint first, we might solve
with alpha := Bool.  But then we'd end up with a RULE like

    RULE: f 3 |> (co :: T Int ~ Booo) = True

which is terrible.  We want

    RULE: f 3 = True |> (sym co :: Bool ~ T Int)

So we are careful to solve the LHS constraints first, and *then* the
RHS constraints.  Actually much of this is done by the on-the-fly
constraint solving, so the same order must be observed in
tcRule.


Note [RULE quantification over equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding which equalities to quantify over is tricky:
 * We do not want to quantify over insoluble equalities (Int ~ Bool)
    (a) because we prefer to report a LHS type error
    (b) because if such things end up in 'givens' we get a bogus
        "inaccessible code" error

 * But we do want to quantify over things like (a ~ F b), where
   F is a type function.

The difficulty is that it's hard to tell what is insoluble!
So we see whether the simplification step yielded any type errors,
and if so refrain from quantifying over *any* equalities.

Note [Quantifying over coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Equality constraints from the LHS will emit coercion hole Wanteds.
These don't have a name, so we can't quantify over them directly.
Instead, because we really do want to quantify here, invent a new
EvVar for the coercion, fill the hole with the invented EvVar, and
then quantify over the EvVar. Not too tricky -- just some
impedance matching, really.

Note [Simplify cloned constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At this stage, we're simplifying constraints only for insolubility
and for unification. Note that all the evidence is quickly discarded.
We use a clone of the real constraint. If we don't do this,
then RHS coercion-hole constraints get filled in, only to get filled
in *again* when solving the implications emitted from tcRule. That's
terrible, so we avoid the problem by cloning the constraints.

-}

simplifyRule :: RuleName
             -> WantedConstraints       -- Constraints from LHS
             -> WantedConstraints       -- Constraints from RHS
             -> TcM ( [EvVar]               -- Quantify over these LHS vars
                    , WantedConstraints)    -- Residual un-quantified LHS constraints
-- See Note [The SimplifyRule Plan]
-- NB: This consumes all simple constraints on the LHS, but not
-- any LHS implication constraints.
simplifyRule name lhs_wanted rhs_wanted
  = do {         -- We allow ourselves to unify environment
                 -- variables: runTcS runs with topTcLevel
       ; lhs_clone <- cloneWC lhs_wanted
       ; rhs_clone <- cloneWC rhs_wanted

       -- Note [The SimplifyRule Plan] step 1
       -- First solve the LHS and *then* solve the RHS
       -- Crucially, this performs unifications
       -- See Note [Solve order for RULES]
       -- See Note [Simplify cloned constraints]
       ; insoluble <- runTcSDeriveds $
             do { lhs_resid <- solveWanteds lhs_clone
                ; rhs_resid <- solveWanteds rhs_clone
                ; return ( insolubleWC lhs_resid ||
                           insolubleWC rhs_resid ) }

       -- Note [The SimplifyRule Plan] step 2
       ; zonked_lhs_simples <- zonkSimples (wc_simple lhs_wanted)

       -- Note [The SimplifyRule Plan] step 3
       ; let (quant_cts, no_quant_cts) = partitionBag (quantify_ct insoluble)
                                                      zonked_lhs_simples

       ; quant_evs <- mapM mk_quant_ev (bagToList quant_cts)

       ; traceTc "simplifyRule" $
         vcat [ text "LHS of rule" <+> doubleQuotes (ftext name)
              , text "lhs_wanted" <+> ppr lhs_wanted
              , text "rhs_wanted" <+> ppr rhs_wanted
              , text "zonked_lhs_simples" <+> ppr zonked_lhs_simples
              , text "quant_cts" <+> ppr quant_cts
              , text "no_quant_cts" <+> ppr no_quant_cts
              ]

       ; return (quant_evs, lhs_wanted { wc_simple = no_quant_cts }) }

  where
    quantify_ct :: Bool -> Ct -> Bool
    quantify_ct insol ct
      | EqPred _ t1 t2 <- classifyPredType (ctPred ct)
      = not (insol || t1 `tcEqType` t2)
        -- Note [RULE quantification over equalities]

      | otherwise
      = True

    mk_quant_ev :: Ct -> TcM EvVar
    mk_quant_ev ct
      | CtWanted { ctev_dest = dest, ctev_pred = pred } <- ctEvidence ct
      = case dest of
          EvVarDest ev_id -> return ev_id
          HoleDest hole   -> -- See Note [Quantifying over coercion holes]
                             do { ev_id <- newEvVar pred
                                ; fillCoercionHole hole (mkTcCoVarCo ev_id)
                                ; return ev_id }
    mk_quant_ev ct = pprPanic "mk_quant_ev" (ppr ct)
