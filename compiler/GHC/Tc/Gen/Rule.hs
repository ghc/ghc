{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}

-- | Typechecking rewrite rules
module GHC.Tc.Gen.Rule ( tcRules ) where

import GHC.Prelude

import GHC.Hs
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Solver
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Expr
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Unify( buildImplicationFor )
import GHC.Tc.Types.Evidence( mkTcCoVarCo )
import GHC.Core.Type
import GHC.Core.TyCon( isTypeFamilyTyCon )
import GHC.Types.Id
import GHC.Types.Var( EvVar )
import GHC.Types.Var.Set
import GHC.Types.Basic ( RuleName, NonStandardDefaultingStrategy(..) )
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Data.Bag

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

Note [TcLevel in type checking rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Bringing type variables into scope naturally bumps the TcLevel. Thus, we type
check the term-level binders in a bumped level, and we must accordingly bump
the level whenever these binders are in scope.

Note [Re-quantify type variables in rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example from #17710:

  foo :: forall k (a :: k) (b :: k). Proxy a -> Proxy b
  foo x = Proxy
  {-# RULES "foo" forall (x :: Proxy (a :: k)). foo x = Proxy #-}

Written out in more detail, the "foo" rewrite rule looks like this:

  forall k (a :: k). forall (x :: Proxy (a :: k)). foo @k @a @b0 x = Proxy @k @b0

Where b0 is a unification variable. Where should b0 be quantified? We have to
quantify it after k, since (b0 :: k). But generalization usually puts inferred
type variables (such as b0) at the /front/ of the telescope! This creates a
conflict.

One option is to simply throw an error, per the principles of
Note [Naughty quantification candidates] in GHC.Tc.Utils.TcMType. This is what would happen
if we were generalising over a normal type signature. On the other hand, the
types in a rewrite rule aren't quite "normal", since the notions of specified
and inferred type variables aren't applicable.

A more permissive design (and the design that GHC uses) is to simply requantify
all of the type variables. That is, we would end up with this:

  forall k (a :: k) (b :: k). forall (x :: Proxy (a :: k)). foo @k @a @b x = Proxy @k @b

It's a bit strange putting the generalized variable `b` after the user-written
variables `k` and `a`. But again, the notion of specificity is not relevant to
rewrite rules, since one cannot "visibly apply" a rewrite rule. This design not
only makes "foo" typecheck, but it also makes the implementation simpler.

See also Note [Generalising in tcTyFamInstEqnGuts] in GHC.Tc.TyCl, which
explains a very similar design when generalising over a type family instance
equation.
-}

tcRules :: [LRuleDecls GhcRn] -> TcM [LRuleDecls GhcTc]
tcRules decls = mapM (wrapLocMA tcRuleDecls) decls

tcRuleDecls :: RuleDecls GhcRn -> TcM (RuleDecls GhcTc)
tcRuleDecls (HsRules { rds_src = src
                     , rds_rules = decls })
   = do { tc_decls <- mapM (wrapLocMA tcRule) decls
        ; return $ HsRules { rds_ext   = noExtField
                           , rds_src   = src
                           , rds_rules = tc_decls } }

tcRule :: RuleDecl GhcRn -> TcM (RuleDecl GhcTc)
tcRule (HsRule { rd_ext  = ext
               , rd_name = rname@(L _ (_,name))
               , rd_act  = act
               , rd_tyvs = ty_bndrs
               , rd_tmvs = tm_bndrs
               , rd_lhs  = lhs
               , rd_rhs  = rhs })
  = addErrCtxt (ruleCtxt name)  $
    do { traceTc "---- Rule ------" (pprFullRuleName rname)
       ; skol_info <- mkSkolemInfo (RuleSkol name)
        -- Note [Typechecking rules]
       ; (tc_lvl, stuff) <- pushTcLevelM $
                            generateRuleConstraints name ty_bndrs tm_bndrs lhs rhs

       ; let (id_bndrs, lhs', lhs_wanted
                      , rhs', rhs_wanted, rule_ty) = stuff

       ; traceTc "tcRule 1" (vcat [ pprFullRuleName rname
                                  , ppr lhs_wanted
                                  , ppr rhs_wanted ])

       ; (lhs_evs, residual_lhs_wanted)
            <- simplifyRule name tc_lvl lhs_wanted rhs_wanted

       -- SimplfyRule Plan, step 4
       -- Now figure out what to quantify over
       -- c.f. GHC.Tc.Solver.simplifyInfer
       -- We quantify over any tyvars free in *either* the rule
       --  *or* the bound variables.  The latter is important.  Consider
       --      ss (x,(y,z)) = (x,z)
       --      RULE:  forall v. fst (ss v) = fst v
       -- The type of the rhs of the rule is just a, but v::(a,(b,c))
       --
       -- We also need to get the completely-unconstrained tyvars of
       -- the LHS, lest they otherwise get defaulted to Any; but we do that
       -- during zonking (see GHC.Tc.Utils.Zonk.zonkRule)

       ; let tpl_ids = lhs_evs ++ id_bndrs

       -- See Note [Re-quantify type variables in rules]
       ; forall_tkvs <- candidateQTyVarsOfTypes (rule_ty : map idType tpl_ids)
       ; qtkvs <- quantifyTyVars skol_info DefaultNonStandardTyVars forall_tkvs
       ; traceTc "tcRule" (vcat [ pprFullRuleName rname
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
       ; (lhs_implic, lhs_binds) <- buildImplicationFor tc_lvl (getSkolemInfo skol_info) qtkvs
                                         lhs_evs residual_lhs_wanted
       ; (rhs_implic, rhs_binds) <- buildImplicationFor tc_lvl (getSkolemInfo skol_info) qtkvs
                                         lhs_evs rhs_wanted

       ; emitImplications (lhs_implic `unionBags` rhs_implic)
       ; return $ HsRule { rd_ext = ext
                         , rd_name = rname
                         , rd_act = act
                         , rd_tyvs = ty_bndrs -- preserved for ppr-ing
                         , rd_tmvs = map (noLocA . RuleBndr noAnn . noLocA)
                                         (qtkvs ++ tpl_ids)
                         , rd_lhs  = mkHsDictLet lhs_binds lhs'
                         , rd_rhs  = mkHsDictLet rhs_binds rhs' } }

generateRuleConstraints :: FastString
                        -> Maybe [LHsTyVarBndr () GhcRn] -> [LRuleBndr GhcRn]
                        -> LHsExpr GhcRn -> LHsExpr GhcRn
                        -> TcM ( [TcId]
                               , LHsExpr GhcTc, WantedConstraints
                               , LHsExpr GhcTc, WantedConstraints
                               , TcType )
generateRuleConstraints rule_name ty_bndrs tm_bndrs lhs rhs
  = do { ((tv_bndrs, id_bndrs), bndr_wanted) <- captureConstraints $
                                                tcRuleBndrs rule_name ty_bndrs tm_bndrs
              -- bndr_wanted constraints can include wildcard hole
              -- constraints, which we should not forget about.
              -- It may mention the skolem type variables bound by
              -- the RULE.  c.f. #10072

       ; tcExtendTyVarEnv unkSkol tv_bndrs $
         tcExtendIdEnv    id_bndrs $
    do { -- See Note [Solve order for RULES]
         ((lhs', rule_ty), lhs_wanted) <- captureConstraints (tcInferRho lhs)
       ; (rhs',            rhs_wanted) <- captureConstraints $
                                          tcCheckMonoExpr rhs rule_ty
       ; let all_lhs_wanted = bndr_wanted `andWC` lhs_wanted
       ; return (id_bndrs, lhs', all_lhs_wanted, rhs', rhs_wanted, rule_ty) } }

-- See Note [TcLevel in type checking rules]
tcRuleBndrs :: FastString -> Maybe [LHsTyVarBndr () GhcRn] -> [LRuleBndr GhcRn]
            -> TcM ([TcTyVar], [Id])
tcRuleBndrs rule_name (Just bndrs) xs
  = do { skol_info <- mkSkolemInfo (RuleSkol rule_name)
       ; (tybndrs1,(tys2,tms)) <- bindExplicitTKBndrs_Skol skol_info bndrs $
                                  tcRuleTmBndrs rule_name xs
       ; let tys1 = binderVars tybndrs1
       ; return (tys1 ++ tys2, tms) }

tcRuleBndrs rule_name Nothing xs
  = tcRuleTmBndrs rule_name xs

-- See Note [TcLevel in type checking rules]
tcRuleTmBndrs :: FastString -> [LRuleBndr GhcRn] -> TcM ([TcTyVar],[Id])
tcRuleTmBndrs _ [] = return ([],[])
tcRuleTmBndrs rule_name (L _ (RuleBndr _ (L _ name)) : rule_bndrs)
  = do  { ty <- newOpenFlexiTyVarTy
        ; (tyvars, tmvars) <- tcRuleTmBndrs rule_name rule_bndrs
        ; return (tyvars, mkLocalId name Many ty : tmvars) }
tcRuleTmBndrs rule_name (L _ (RuleBndrSig _ (L _ name) rn_ty) : rule_bndrs)
--  e.g         x :: a->a
--  The tyvar 'a' is brought into scope first, just as if you'd written
--              a::*, x :: a->a
--  If there's an explicit forall, the renamer would have already reported an
--   error for each out-of-scope type variable used
  = do  { let ctxt = RuleSigCtxt rule_name name
        ; (_ , tvs, id_ty) <- tcHsPatSigType ctxt HM_Sig rn_ty OpenKind
        ; let id  = mkLocalId name Many id_ty
                    -- See Note [Typechecking pattern signature binders] in GHC.Tc.Gen.HsType

              -- The type variables scope over subsequent bindings; yuk
        ; (tyvars, tmvars) <- tcExtendNameTyVarEnv tvs $
                                   tcRuleTmBndrs rule_name rule_bndrs
        ; return (map snd tvs ++ tyvars, id : tmvars) }

ruleCtxt :: FastString -> SDoc
ruleCtxt name = text "When checking the rewrite rule" <+>
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

    RULE: f 3 |> (co :: T Int ~ Bool) = True

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
             -> TcLevel                 -- Level at which to solve the constraints
             -> WantedConstraints       -- Constraints from LHS
             -> WantedConstraints       -- Constraints from RHS
             -> TcM ( [EvVar]               -- Quantify over these LHS vars
                    , WantedConstraints)    -- Residual un-quantified LHS constraints
-- See Note [The SimplifyRule Plan]
-- NB: This consumes all simple constraints on the LHS, but not
-- any LHS implication constraints.
simplifyRule name tc_lvl lhs_wanted rhs_wanted
  = do {
       -- Note [The SimplifyRule Plan] step 1
       -- First solve the LHS and *then* solve the RHS
       -- Crucially, this performs unifications
       -- Why clone?  See Note [Simplify cloned constraints]
       ; lhs_clone <- cloneWC lhs_wanted
       ; rhs_clone <- cloneWC rhs_wanted
       ; setTcLevel tc_lvl $
         runTcSDeriveds    $
         do { _ <- solveWanteds lhs_clone
            ; _ <- solveWanteds rhs_clone
                  -- Why do them separately?
                  -- See Note [Solve order for RULES]
            ; return () }

       -- Note [The SimplifyRule Plan] step 2
       ; lhs_wanted <- zonkWC lhs_wanted
       ; let (quant_cts, residual_lhs_wanted) = getRuleQuantCts lhs_wanted

       -- Note [The SimplifyRule Plan] step 3
       ; quant_evs <- mapM mk_quant_ev (bagToList quant_cts)

       ; traceTc "simplifyRule" $
         vcat [ text "LHS of rule" <+> doubleQuotes (ftext name)
              , text "lhs_wanted" <+> ppr lhs_wanted
              , text "rhs_wanted" <+> ppr rhs_wanted
              , text "quant_cts" <+> ppr quant_cts
              , text "residual_lhs_wanted" <+> ppr residual_lhs_wanted
              ]

       ; return (quant_evs, residual_lhs_wanted) }

  where
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


getRuleQuantCts :: WantedConstraints -> (Cts, WantedConstraints)
-- Extract all the constraints we can quantify over,
--   also returning the depleted WantedConstraints
--
-- NB: we must look inside implications, because with
--     -fdefer-type-errors we generate implications rather eagerly;
--     see GHC.Tc.Utils.Unify.implicationNeeded. Not doing so caused #14732.
--
-- Unlike simplifyInfer, we don't leave the WantedConstraints unchanged,
--   and attempt to solve them from the quantified constraints.  That
--   nearly works, but fails for a constraint like (d :: Eq Int).
--   We /do/ want to quantify over it, but the short-cut solver
--   (see GHC.Tc.Solver.Interact Note [Shortcut solving]) ignores the quantified
--   and instead solves from the top level.
--
--   So we must partition the WantedConstraints ourselves
--   Not hard, but tiresome.

getRuleQuantCts wc
  = float_wc emptyVarSet wc
  where
    float_wc :: TcTyCoVarSet -> WantedConstraints -> (Cts, WantedConstraints)
    float_wc skol_tvs (WC { wc_simple = simples, wc_impl = implics, wc_holes = holes })
      = ( simple_yes `andCts` implic_yes
        , emptyWC { wc_simple = simple_no, wc_impl = implics_no, wc_holes = holes })
     where
        (simple_yes, simple_no) = partitionBag (rule_quant_ct skol_tvs) simples
        (implic_yes, implics_no) = mapAccumBagL (float_implic skol_tvs)
                                                emptyBag implics

    float_implic :: TcTyCoVarSet -> Cts -> Implication -> (Cts, Implication)
    float_implic skol_tvs yes1 imp
      = (yes1 `andCts` yes2, imp { ic_wanted = no })
      where
        (yes2, no) = float_wc new_skol_tvs (ic_wanted imp)
        new_skol_tvs = skol_tvs `extendVarSetList` ic_skols imp

    rule_quant_ct :: TcTyCoVarSet -> Ct -> Bool
    rule_quant_ct skol_tvs ct = case classifyPredType (ctPred ct) of
      EqPred _ t1 t2
        | not (ok_eq t1 t2)
        -> False        -- Note [RULE quantification over equalities]
      SpecialPred {}
        -- RULES must never quantify over special predicates, as that
        -- would leak internal GHC implementation details to the user.
        --
        -- Tests (for Concrete# predicates): RepPolyRule{1,2,3}.
        -> False
      _ -> tyCoVarsOfCt ct `disjointVarSet` skol_tvs

    ok_eq t1 t2
       | t1 `tcEqType` t2 = False
       | otherwise        = is_fun_app t1 || is_fun_app t2

    is_fun_app ty   -- ty is of form (F tys) where F is a type function
      = case tyConAppTyCon_maybe ty of
          Just tc -> isTypeFamilyTyCon tc
          Nothing -> False
