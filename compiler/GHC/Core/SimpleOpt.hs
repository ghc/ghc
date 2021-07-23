{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}


module GHC.Core.SimpleOpt (
        SimpleOpts (..), defaultSimpleOpts,

        -- ** Simple expression optimiser
        simpleOptPgm, simpleOptExpr, simpleOptExprWith,

        -- ** Join points
        joinPointBinding_maybe, joinPointBindings_maybe,

        -- ** Predicates on expressions
        exprIsConApp_maybe, exprIsLiteral_maybe, exprIsLambda_maybe,

    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Opt.Arity
import GHC.Core.Subst
import GHC.Core.Utils
import GHC.Core.FVs
import GHC.Core.Unfold
import GHC.Core.Unfold.Make
import GHC.Core.Make ( FloatBind(..) )
import GHC.Core.Opt.OccurAnal( occurAnalyseExpr, occurAnalysePgm )
import GHC.Types.Literal
import GHC.Types.Id
import GHC.Types.Id.Info  ( realUnfoldingInfo, setUnfoldingInfo, setRuleInfo, IdInfo (..) )
import GHC.Types.Var      ( isNonCoVarId )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Core.DataCon
import GHC.Types.Demand( etaConvertDmdSig )
import GHC.Types.Tickish
import GHC.Core.Coercion.Opt ( optCoercion, OptCoercionOpts (..) )
import GHC.Core.Type hiding ( substTy, extendTvSubst, extendCvSubst, extendTvSubstList
                            , isInScope, substTyVarBndr, cloneTyVarBndr )
import GHC.Core.Coercion hiding ( substCo, substCoVarBndr )
import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Types.Basic
import GHC.Unit.Module ( Module )
import GHC.Utils.Encoding
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Data.Maybe       ( orElse )
import Data.List (mapAccumL)
import qualified Data.ByteString as BS

{-
************************************************************************
*                                                                      *
        The Simple Optimiser
*                                                                      *
************************************************************************

Note [The simple optimiser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The simple optimiser is a lightweight, pure (non-monadic) function
that rapidly does a lot of simple optimisations, including

  - inlining things that occur just once,
      or whose RHS turns out to be trivial
  - beta reduction
  - case of known constructor
  - dead code elimination

It does NOT do any call-site inlining; it only inlines a function if
it can do so unconditionally, dropping the binding.  It thereby
guarantees to leave no un-reduced beta-redexes.

It is careful to follow the guidance of "Secrets of the GHC inliner",
and in particular the pre-inline-unconditionally and
post-inline-unconditionally story, to do effective beta reduction on
functions called precisely once, without repeatedly optimising the same
expression.  In fact, the simple optimiser is a good example of this
little dance in action; the full Simplifier is a lot more complicated.

-}

-- | Simple optimiser options
data SimpleOpts = SimpleOpts
   { so_uf_opts :: !UnfoldingOpts   -- ^ Unfolding options
   , so_co_opts :: !OptCoercionOpts -- ^ Coercion optimiser options
   , so_eta_red :: !Bool            -- ^ Eta reduction on?
   }

-- | Default options for the Simple optimiser.
defaultSimpleOpts :: SimpleOpts
defaultSimpleOpts = SimpleOpts
   { so_uf_opts = defaultUnfoldingOpts
   , so_co_opts = OptCoercionOpts { optCoercionEnabled = False }
   , so_eta_red = False
   }

simpleOptExpr :: HasDebugCallStack => SimpleOpts -> CoreExpr -> CoreExpr
-- See Note [The simple optimiser]
-- Do simple optimisation on an expression
-- The optimisation is very straightforward: just
-- inline non-recursive bindings that are used only once,
-- or where the RHS is trivial
--
-- We also inline bindings that bind a Eq# box: see
-- See Note [Getting the map/coerce RULE to work].
--
-- Also we convert functions to join points where possible (as
-- the occurrence analyser does most of the work anyway).
--
-- The result is NOT guaranteed occurrence-analysed, because
-- in  (let x = y in ....) we substitute for x; so y's occ-info
-- may change radically
--
-- Note that simpleOptExpr is a pure function that we want to be able to call
-- from lots of places, including ones that don't have DynFlags (e.g to optimise
-- unfoldings of statically defined Ids via mkCompulsoryUnfolding). It used to
-- fetch its options directly from the DynFlags, however, so some callers had to
-- resort to using unsafeGlobalDynFlags (a global mutable variable containing
-- the DynFlags). It has been modified to take its own SimpleOpts that may be
-- created from DynFlags, but not necessarily.

simpleOptExpr opts expr
  = -- pprTrace "simpleOptExpr" (ppr init_subst $$ ppr expr)
    simpleOptExprWith opts init_subst expr
  where
    init_subst = mkEmptySubst (mkInScopeSet (exprFreeVars expr))
        -- It's potentially important to make a proper in-scope set
        -- Consider  let x = ..y.. in \y. ...x...
        -- Then we should remember to clone y before substituting
        -- for x.  It's very unlikely to occur, because we probably
        -- won't *be* substituting for x if it occurs inside a
        -- lambda.
        --
        -- It's a bit painful to call exprFreeVars, because it makes
        -- three passes instead of two (occ-anal, and go)

simpleOptExprWith :: HasDebugCallStack => SimpleOpts -> Subst -> InExpr -> OutExpr
-- See Note [The simple optimiser]
simpleOptExprWith opts subst expr
  = simple_opt_expr init_env (occurAnalyseExpr expr)
  where
    init_env = (emptyEnv opts) { soe_subst = subst }

----------------------
simpleOptPgm :: SimpleOpts
             -> Module
             -> CoreProgram
             -> [CoreRule]
             -> (CoreProgram, [CoreRule], CoreProgram)
-- See Note [The simple optimiser]
simpleOptPgm opts this_mod binds rules =
    (reverse binds', rules', occ_anald_binds)
  where
    occ_anald_binds  = occurAnalysePgm this_mod
                          (\_ -> True)  {- All unfoldings active -}
                          (\_ -> False) {- No rules active -}
                          rules binds

    (final_env, binds') = foldl' do_one (emptyEnv opts, []) occ_anald_binds
    final_subst = soe_subst final_env

    rules' = substRulesForImportedIds final_subst rules
             -- We never unconditionally inline into rules,
             -- hence paying just a substitution

    do_one (env, binds') bind
      = case simple_opt_bind env bind TopLevel of
          (env', Nothing)    -> (env', binds')
          (env', Just bind') -> (env', bind':binds')

-- In these functions the substitution maps InVar -> OutExpr

----------------------
type SimpleClo = (SimpleOptEnv, InExpr)

data SimpleOptEnv
  = SOE { soe_opts :: {-# UNPACK #-} !SimpleOpts
             -- ^ Simplifier options

        , soe_inl :: IdEnv SimpleClo
             -- ^ Deals with preInlineUnconditionally; things
             -- that occur exactly once and are inlined
             -- without having first been simplified

        , soe_subst :: Subst
             -- ^ Deals with cloning; includes the InScopeSet
        }

instance Outputable SimpleOptEnv where
  ppr (SOE { soe_inl = inl, soe_subst = subst })
    = text "SOE {" <+> vcat [ text "soe_inl   =" <+> ppr inl
                            , text "soe_subst =" <+> ppr subst ]
                   <+> text "}"

emptyEnv :: SimpleOpts -> SimpleOptEnv
emptyEnv opts = SOE { soe_inl   = emptyVarEnv
                    , soe_subst = emptySubst
                    , soe_opts  = opts  }

soeZapSubst :: SimpleOptEnv -> SimpleOptEnv
soeZapSubst env@(SOE { soe_subst = subst })
  = env { soe_inl = emptyVarEnv, soe_subst = zapSubstEnv subst }

soeSetInScope :: SimpleOptEnv -> SimpleOptEnv -> SimpleOptEnv
-- Take in-scope set from env1, and the rest from env2
soeSetInScope (SOE { soe_subst = subst1 })
              env2@(SOE { soe_subst = subst2 })
  = env2 { soe_subst = setInScope subst2 (substInScope subst1) }

---------------
simple_opt_clo :: SimpleOptEnv -> SimpleClo -> OutExpr
simple_opt_clo env (e_env, e)
  = simple_opt_expr (soeSetInScope env e_env) e

simple_opt_expr :: HasCallStack => SimpleOptEnv -> InExpr -> OutExpr
simple_opt_expr env expr
  = go expr
  where
    subst        = soe_subst env
    in_scope     = substInScope subst
    in_scope_env = (in_scope, simpleUnfoldingFun)

    ---------------
    go (Var v)
       | Just clo <- lookupVarEnv (soe_inl env) v
       = simple_opt_clo env clo
       | otherwise
       = lookupIdSubst (soe_subst env) v

    go (App e1 e2)      = simple_app env e1 [(env,e2)]
    go (Type ty)        = Type     (substTy subst ty)
    go (Coercion co)    = Coercion (go_co co)
    go (Lit lit)        = Lit lit
    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
    go (Cast e co)      = mk_cast (go e) (go_co co)
    go (Let bind body)  = case simple_opt_bind env bind NotTopLevel of
                             (env', Nothing)   -> simple_opt_expr env' body
                             (env', Just bind) -> Let bind (simple_opt_expr env' body)

    go lam@(Lam {})     = go_lam env [] lam
    go (Case e b ty as)
       -- See Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , Just (_, [], con, _tys, es) <- exprIsConApp_maybe in_scope_env e'
        -- We don't need to be concerned about floats when looking for coerce.
      , Just (Alt altcon bs rhs) <- findAlt (DataAlt con) as
      = case altcon of
          DEFAULT -> go rhs
          _       -> foldr wrapLet (simple_opt_expr env' rhs) mb_prs
            where
              (env', mb_prs) = mapAccumL (simple_out_bind NotTopLevel) env $
                               zipEqual "simpleOptExpr" bs es

         -- Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , [Alt DEFAULT _ rhs] <- as
      , isCoVarType (varType b)
      , (Var fun, _args) <- collectArgs e
      , fun `hasKey` coercibleSCSelIdKey
         -- without this last check, we get #11230
      = go rhs

      | otherwise
      = Case e' b' (substTy subst ty)
                   (map (go_alt env') as)
      where
        e' = go e
        (env', b') = subst_opt_bndr env b

    ----------------------
    go_co co = optCoercion (so_co_opts (soe_opts env)) (getTCvSubst subst) co

    ----------------------
    go_alt env (Alt con bndrs rhs)
      = Alt con bndrs' (simple_opt_expr env' rhs)
      where
        (env', bndrs') = subst_opt_bndrs env bndrs

    ----------------------
    -- go_lam tries eta reduction
    go_lam env bs' (Lam b e)
       = go_lam env' (b':bs') e
       where
         (env', b') = subst_opt_bndr env b
    go_lam env bs' e
       | so_eta_red (soe_opts env)
       , Just etad_e <- tryEtaReduce bs e' = etad_e
       | otherwise                         = mkLams bs e'
       where
         bs = reverse bs'
         e' = simple_opt_expr env e

mk_cast :: CoreExpr -> CoercionR -> CoreExpr
-- Like GHC.Core.Utils.mkCast, but does a full reflexivity check.
-- mkCast doesn't do that because the Simplifier does (in simplCast)
-- But in SimpleOpt it's nice to kill those nested casts (#18112)
mk_cast (Cast e co1) co2        = mk_cast e (co1 `mkTransCo` co2)
mk_cast (Tick t e)   co         = Tick t (mk_cast e co)
mk_cast e co | isReflexiveCo co = e
             | otherwise        = Cast e co

----------------------
-- simple_app collects arguments for beta reduction
simple_app :: HasDebugCallStack => SimpleOptEnv -> InExpr -> [SimpleClo] -> CoreExpr

simple_app env (Var v) as
  | Just (env', e) <- lookupVarEnv (soe_inl env) v
  = simple_app (soeSetInScope env env') e as

  | let unf = idUnfolding v
  , isCompulsoryUnfolding (idUnfolding v)
  , isAlwaysActive (idInlineActivation v)
    -- See Note [Unfold compulsory unfoldings in LHSs]
  = simple_app (soeZapSubst env) (unfoldingTemplate unf) as

  | otherwise
  , let out_fn = lookupIdSubst (soe_subst env) v
  = finish_app env out_fn as

simple_app env (App e1 e2) as
  = simple_app env e1 ((env, e2) : as)

simple_app env e@(Lam {}) as@(_:_)
  | (bndrs, body) <- collectBinders e
  , let zapped_bndrs = zapLamBndrs (length as) bndrs
    -- Be careful to zap the lambda binders if necessary
    -- c.f. the Lam case of simplExprF1 in GHC.Core.Opt.Simplify
    -- Lacking this zap caused #19347, when we had a redex
    --   (\ a b. K a b) e1 e2
    -- where (as it happens) the eta-expanded K is produced by
    -- Note [Typechecking data constructors] in GHC.Tc.Gen.Head
  = do_beta env zapped_bndrs body as
  where
    do_beta env (b:bs) body (a:as)
      | (env', mb_pr) <- simple_bind_pair env b Nothing a NotTopLevel
      = wrapLet mb_pr $ do_beta env' bs body as
    do_beta env bs body as = simple_app env (mkLams bs body) as

simple_app env (Tick t e) as
  -- Okay to do "(Tick t e) x ==> Tick t (e x)"?
  | t `tickishScopesLike` SoftScope
  = mkTick t $ simple_app env e as

-- (let x = e in b) a1 .. an  =>  let x = e in (b a1 .. an)
-- The let might appear there as a result of inlining
-- e.g.   let f = let x = e in b
--        in f a1 a2
--   (#13208)
-- However, do /not/ do this transformation for join points
--    See Note [simple_app and join points]
simple_app env (Let bind body) args
  = case simple_opt_bind env bind NotTopLevel of
      (env', Nothing)   -> simple_app env' body args
      (env', Just bind')
        | isJoinBind bind' -> finish_app env expr' args
        | otherwise        -> Let bind' (simple_app env' body args)
        where
          expr' = Let bind' (simple_opt_expr env' body)

simple_app env e as
  = finish_app env (simple_opt_expr env e) as

finish_app :: SimpleOptEnv -> OutExpr -> [SimpleClo] -> OutExpr
finish_app _ fun []
  = fun
finish_app env fun (arg:args)
  = finish_app env (App fun (simple_opt_clo env arg)) args

----------------------
simple_opt_bind :: SimpleOptEnv -> InBind -> TopLevelFlag
                -> (SimpleOptEnv, Maybe OutBind)
simple_opt_bind env (NonRec b r) top_level
  = (env', case mb_pr of
            Nothing    -> Nothing
            Just (b,r) -> Just (NonRec b r))
  where
    (b', r') = joinPointBinding_maybe b r `orElse` (b, r)
    (env', mb_pr) = simple_bind_pair env b' Nothing (env,r') top_level

simple_opt_bind env (Rec prs) top_level
  = (env'', res_bind)
  where
    res_bind          = Just (Rec (reverse rev_prs'))
    prs'              = joinPointBindings_maybe prs `orElse` prs
    (env', bndrs')    = subst_opt_bndrs env (map fst prs')
    (env'', rev_prs') = foldl' do_pr (env', []) (prs' `zip` bndrs')
    do_pr (env, prs) ((b,r), b')
       = (env', case mb_pr of
                  Just pr -> pr : prs
                  Nothing -> prs)
       where
         (env', mb_pr) = simple_bind_pair env b (Just b') (env,r) top_level

----------------------
simple_bind_pair :: SimpleOptEnv
                 -> InVar -> Maybe OutVar
                 -> SimpleClo
                 -> TopLevelFlag
                 -> (SimpleOptEnv, Maybe (OutVar, OutExpr))
    -- (simple_bind_pair subst in_var out_rhs)
    --   either extends subst with (in_var -> out_rhs)
    --   or     returns Nothing
simple_bind_pair env@(SOE { soe_inl = inl_env, soe_subst = subst })
                 in_bndr mb_out_bndr clo@(rhs_env, in_rhs)
                 top_level
  | Type ty <- in_rhs        -- let a::* = TYPE ty in <body>
  , let out_ty = substTy (soe_subst rhs_env) ty
  = assertPpr (isTyVar in_bndr) (ppr in_bndr $$ ppr in_rhs) $
    (env { soe_subst = extendTvSubst subst in_bndr out_ty }, Nothing)

  | Coercion co <- in_rhs
  , let out_co = optCoercion (so_co_opts (soe_opts env)) (getTCvSubst (soe_subst rhs_env)) co
  = assert (isCoVar in_bndr)
    (env { soe_subst = extendCvSubst subst in_bndr out_co }, Nothing)

  | assertPpr (isNonCoVarId in_bndr) (ppr in_bndr)
    -- The previous two guards got rid of tyvars and coercions
    -- See Note [Core type and coercion invariant] in GHC.Core
    pre_inline_unconditionally
  = (env { soe_inl = extendVarEnv inl_env in_bndr clo }, Nothing)

  | otherwise
  = simple_out_bind_pair env in_bndr mb_out_bndr out_rhs
                         occ active stable_unf top_level
  where
    stable_unf = isStableUnfolding (idUnfolding in_bndr)
    active     = isAlwaysActive (idInlineActivation in_bndr)
    occ        = idOccInfo in_bndr

    out_rhs | Just join_arity <- isJoinId_maybe in_bndr
            = simple_join_rhs join_arity
            | otherwise
            = simple_opt_clo env clo

    simple_join_rhs join_arity -- See Note [Preserve join-binding arity]
      = mkLams join_bndrs' (simple_opt_expr env_body join_body)
      where
        env0 = soeSetInScope env rhs_env
        (join_bndrs, join_body) = collectNBinders join_arity in_rhs
        (env_body, join_bndrs') = subst_opt_bndrs env0 join_bndrs

    pre_inline_unconditionally :: Bool
    pre_inline_unconditionally
       | isExportedId in_bndr     = False
       | stable_unf               = False
       | not active               = False    -- Note [Inline prag in simplOpt]
       | not (safe_to_inline occ) = False
       | otherwise                = True

        -- Unconditionally safe to inline
    safe_to_inline :: OccInfo -> Bool
    safe_to_inline IAmALoopBreaker{}                  = False
    safe_to_inline IAmDead                            = True
    safe_to_inline OneOcc{ occ_in_lam = NotInsideLam
                         , occ_n_br = 1 }             = True
    safe_to_inline OneOcc{}                           = False
    safe_to_inline ManyOccs{}                         = False

-------------------
simple_out_bind :: TopLevelFlag
                -> SimpleOptEnv
                -> (InVar, OutExpr)
                -> (SimpleOptEnv, Maybe (OutVar, OutExpr))
simple_out_bind top_level env@(SOE { soe_subst = subst }) (in_bndr, out_rhs)
  | Type out_ty <- out_rhs
  = assertPpr (isTyVar in_bndr) (ppr in_bndr $$ ppr out_ty $$ ppr out_rhs)
    (env { soe_subst = extendTvSubst subst in_bndr out_ty }, Nothing)

  | Coercion out_co <- out_rhs
  = assert (isCoVar in_bndr)
    (env { soe_subst = extendCvSubst subst in_bndr out_co }, Nothing)

  | otherwise
  = simple_out_bind_pair env in_bndr Nothing out_rhs
                         (idOccInfo in_bndr) True False top_level

-------------------
simple_out_bind_pair :: SimpleOptEnv
                     -> InId -> Maybe OutId -> OutExpr
                     -> OccInfo -> Bool -> Bool -> TopLevelFlag
                     -> (SimpleOptEnv, Maybe (OutVar, OutExpr))
simple_out_bind_pair env in_bndr mb_out_bndr out_rhs
                     occ_info active stable_unf top_level
  | assertPpr (isNonCoVarId in_bndr) (ppr in_bndr)
    -- Type and coercion bindings are caught earlier
    -- See Note [Core type and coercion invariant]
    post_inline_unconditionally
  = ( env' { soe_subst = extendIdSubst (soe_subst env) in_bndr out_rhs }
    , Nothing)

  | otherwise
  = ( env', Just (out_bndr, out_rhs) )
  where
    (env', bndr1) = case mb_out_bndr of
                      Just out_bndr -> (env, out_bndr)
                      Nothing       -> subst_opt_bndr env in_bndr
    out_bndr = add_info env' in_bndr top_level out_rhs bndr1

    post_inline_unconditionally :: Bool
    post_inline_unconditionally
       | isExportedId in_bndr  = False -- Note [Exported Ids and trivial RHSs]
       | stable_unf            = False -- Note [Stable unfoldings and postInlineUnconditionally]
       | not active            = False --     in GHC.Core.Opt.Simplify.Utils
       | is_loop_breaker       = False -- If it's a loop-breaker of any kind, don't inline
                                       -- because it might be referred to "earlier"
       | exprIsTrivial out_rhs = True
       | coercible_hack        = True
       | otherwise             = False

    is_loop_breaker = isWeakLoopBreaker occ_info

    -- See Note [Getting the map/coerce RULE to work]
    coercible_hack | (Var fun, args) <- collectArgs out_rhs
                   , Just dc <- isDataConWorkId_maybe fun
                   , dc `hasKey` heqDataConKey || dc `hasKey` coercibleDataConKey
                   = all exprIsTrivial args
                   | otherwise
                   = False

{- Note [Exported Ids and trivial RHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We obviously do not want to unconditionally inline an Id that is exported.
In GHC.Core.Opt.Simplify.Utils, Note [Top level and postInlineUnconditionally], we
explain why we don't inline /any/ top-level things unconditionally, even
trivial ones.  But we do here!  Why?  In the simple optimiser

  * We do no rule rewrites
  * We do no call-site inlining

Those differences obviate the reasons for not inlining a trivial rhs,
and increase the benefit for doing so.  So we unconditionally inline trivial
rhss here.

Note [Preserve join-binding arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Be careful /not/ to eta-reduce the RHS of a join point, lest we lose
the join-point arity invariant.  #15108 was caused by simplifying
the RHS with simple_opt_expr, which does eta-reduction.  Solution:
simplify the RHS of a join point by simplifying under the lambdas
(which of course should be there).

Note [simple_app and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general for let-bindings we can do this:
   (let { x = e } in b) a  ==>  let { x = e } in b a

But not for join points!  For two reasons:

- We would need to push the continuation into the RHS:
   (join { j = e } in b) a  ==>  let { j' = e a } in b[j'/j] a
                                      NB ----^^
  and also change the type of j, hence j'.
  That's a bit sophisticated for the very simple optimiser.

- We might end up with something like
    join { j' = e a } in
    (case blah of        )
    (  True  -> j' void# ) a
    (  False -> blah     )
  and now the call to j' doesn't look like a tail call, and
  Lint may reject.  I say "may" because this is /explicitly/
  allowed in the "Compiling without Continuations" paper
  (Section 3, "Managing \Delta").  But GHC currently does not
  allow this slightly-more-flexible form.  See GHC.Core
  Note [Join points are less general than the paper].

The simple thing to do is to disable this transformation
for join points in the simple optimiser

Note [The Let-Unfoldings Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A program has the Let-Unfoldings property iff:

- For every let-bound variable f, whether top-level or nested, whether
  recursive or not:
  - Both the binding Id of f, and every occurrence Id of f, has an idUnfolding.
  - For non-INLINE things, that unfolding will be f's right hand sids
  - For INLINE things (which have a "stable" unfolding) that unfolding is
    semantically equivalent to f's RHS, but derived from the original RHS of f
    rather that its current RHS.

Informally, we can say that in a program that has the Let-Unfoldings property,
all let-bound Id's have an explicit unfolding attached to them.

Currently, the simplifier guarantees the Let-Unfoldings invariant for anything
it outputs.

-}

----------------------
subst_opt_bndrs :: SimpleOptEnv -> [InVar] -> (SimpleOptEnv, [OutVar])
subst_opt_bndrs env bndrs = mapAccumL subst_opt_bndr env bndrs

subst_opt_bndr :: SimpleOptEnv -> InVar -> (SimpleOptEnv, OutVar)
subst_opt_bndr env bndr
  | isTyVar bndr  = (env { soe_subst = subst_tv }, tv')
  | isCoVar bndr  = (env { soe_subst = subst_cv }, cv')
  | otherwise     = subst_opt_id_bndr env bndr
  where
    subst           = soe_subst env
    (subst_tv, tv') = substTyVarBndr subst bndr
    (subst_cv, cv') = substCoVarBndr subst bndr

subst_opt_id_bndr :: SimpleOptEnv -> InId -> (SimpleOptEnv, OutId)
-- Nuke all fragile IdInfo, unfolding, and RULES; it gets added back later by
-- add_info.
--
-- Rather like SimplEnv.substIdBndr
--
-- It's important to zap fragile OccInfo (which GHC.Core.Subst.substIdBndr
-- carefully does not do) because simplOptExpr invalidates it

subst_opt_id_bndr env@(SOE { soe_subst = subst, soe_inl = inl }) old_id
  = (env { soe_subst = new_subst, soe_inl = new_inl }, new_id)
  where
    Subst in_scope id_subst tv_subst cv_subst = subst

    id1    = uniqAway in_scope old_id
    id2    = updateIdTypeAndMult (substTy subst) id1
    new_id = zapFragileIdInfo id2
             -- Zaps rules, unfolding, and fragile OccInfo
             -- The unfolding and rules will get added back later, by add_info

    new_in_scope = in_scope `extendInScopeSet` new_id

    no_change = new_id == old_id

        -- Extend the substitution if the unique has changed,
        -- See the notes with substTyVarBndr for the delSubstEnv
    new_id_subst
      | no_change = delVarEnv id_subst old_id
      | otherwise = extendVarEnv id_subst old_id (Var new_id)

    new_subst = Subst new_in_scope new_id_subst tv_subst cv_subst
    new_inl   = delVarEnv inl old_id

----------------------
add_info :: SimpleOptEnv -> InVar -> TopLevelFlag -> OutExpr -> OutVar -> OutVar
add_info env old_bndr top_level new_rhs new_bndr
 | isTyVar old_bndr = new_bndr
 | otherwise        = lazySetIdInfo new_bndr new_info
 where
   subst    = soe_subst env
   uf_opts  = so_uf_opts (soe_opts env)
   old_info = idInfo old_bndr

   -- Add back in the rules and unfolding which were
   -- removed by zapFragileIdInfo in subst_opt_id_bndr.
   --
   -- See Note [The Let-Unfoldings Invariant]
   new_info = idInfo new_bndr `setRuleInfo`      new_rules
                              `setUnfoldingInfo` new_unfolding

   old_rules = ruleInfo old_info
   new_rules = substRuleInfo subst new_bndr old_rules

   old_unfolding = realUnfoldingInfo old_info
   new_unfolding | isStableUnfolding old_unfolding
                 = substUnfolding subst old_unfolding
                 | otherwise
                 = unfolding_from_rhs

   unfolding_from_rhs = mkUnfolding uf_opts InlineRhs
                                    (isTopLevel top_level)
                                    False -- may be bottom or not
                                    new_rhs

simpleUnfoldingFun :: IdUnfoldingFun
simpleUnfoldingFun id
  | isAlwaysActive (idInlineActivation id) = idUnfolding id
  | otherwise                              = noUnfolding

wrapLet :: Maybe (Id,CoreExpr) -> CoreExpr -> CoreExpr
wrapLet Nothing      body = body
wrapLet (Just (b,r)) body = Let (NonRec b r) body

{-
Note [Inline prag in simplOpt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If there's an INLINE/NOINLINE pragma that restricts the phase in
which the binder can be inlined, we don't inline here; after all,
we don't know what phase we're in.  Here's an example

  foo :: Int -> Int -> Int
  {-# INLINE foo #-}
  foo m n = inner m
     where
       {-# INLINE [1] inner #-}
       inner m = m+n

  bar :: Int -> Int
  bar n = foo n 1

When inlining 'foo' in 'bar' we want the let-binding for 'inner'
to remain visible until Phase 1

Note [Unfold compulsory unfoldings in LHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the user writes `RULES map coerce = coerce` as a rule, the rule
will only ever match if simpleOptExpr replaces coerce by its unfolding
on the LHS, because that is the core that the rule matching engine
will find. So do that for everything that has a compulsory
unfolding. Also see Note [Desugaring coerce as cast] in GHC.HsToCore.

However, we don't want to inline 'seq', which happens to also have a
compulsory unfolding, so we only do this unfolding only for things
that are always-active.  See Note [User-defined RULES for seq] in GHC.Types.Id.Make.

Note [Getting the map/coerce RULE to work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We wish to allow the "map/coerce" RULE to fire:

  {-# RULES "map/coerce" map coerce = coerce #-}

The naive core produced for this is

  forall a b (dict :: Coercible * a b).
    map @a @b (coerce @a @b @dict) = coerce @[a] @[b] @dict'

  where dict' :: Coercible [a] [b]
        dict' = ...

This matches literal uses of `map coerce` in code, but that's not what we
want. We want it to match, say, `map MkAge` (where newtype Age = MkAge Int)
too. Some of this is addressed by compulsorily unfolding coerce on the LHS,
yielding

  forall a b (dict :: Coercible * a b).
    map @a @b (\(x :: a) -> case dict of
      MkCoercible (co :: a ~R# b) -> x |> co) = ...

Getting better. But this isn't exactly what gets produced. This is because
Coercible essentially has ~R# as a superclass, and superclasses get eagerly
extracted during solving. So we get this:

  forall a b (dict :: Coercible * a b).
    case Coercible_SCSel @* @a @b dict of
      _ [Dead] -> map @a @b (\(x :: a) -> case dict of
                               MkCoercible (co :: a ~R# b) -> x |> co) = ...

Unfortunately, this still abstracts over a Coercible dictionary. We really
want it to abstract over the ~R# evidence. So, we have Desugar.unfold_coerce,
which transforms the above to (see also Note [Desugaring coerce as cast] in
Desugar)

  forall a b (co :: a ~R# b).
    let dict = MkCoercible @* @a @b co in
    case Coercible_SCSel @* @a @b dict of
      _ [Dead] -> map @a @b (\(x :: a) -> case dict of
         MkCoercible (co :: a ~R# b) -> x |> co) = let dict = ... in ...

Now, we need simpleOptExpr to fix this up. It does so by taking three
separate actions:
  1. Inline certain non-recursive bindings. The choice whether to inline
     is made in simple_bind_pair. Note the rather specific check for
     MkCoercible in there.

  2. Stripping case expressions like the Coercible_SCSel one.
     See the `Case` case of simple_opt_expr's `go` function.

  3. Look for case expressions that unpack something that was
     just packed and inline them. This is also done in simple_opt_expr's
     `go` function.

This is all a fair amount of special-purpose hackery, but it's for
a good cause. And it won't hurt other RULES and such that it comes across.


************************************************************************
*                                                                      *
                Join points
*                                                                      *
************************************************************************
-}

{- Note [Strictness and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   let f = \x.  if x>200 then e1 else e1

and we know that f is strict in x.  Then if we subsequently
discover that f is an arity-2 join point, we'll eta-expand it to

   let f = \x y.  if x>200 then e1 else e1

and now it's only strict if applied to two arguments.  So we should
adjust the strictness info.

A more common case is when

   f = \x. error ".."

and again its arity increases (#15517)
-}


-- | Returns Just (bndr,rhs) if the binding is a join point:
-- If it's a JoinId, just return it
-- If it's not yet a JoinId but is always tail-called,
--    make it into a JoinId and return it.
-- In the latter case, eta-expand the RHS if necessary, to make the
-- lambdas explicit, as is required for join points
--
-- Precondition: the InBndr has been occurrence-analysed,
--               so its OccInfo is valid
joinPointBinding_maybe :: InBndr -> InExpr -> Maybe (InBndr, InExpr)
joinPointBinding_maybe bndr rhs
  | not (isId bndr)
  = Nothing

  | isJoinId bndr
  = Just (bndr, rhs)

  | AlwaysTailCalled join_arity <- tailCallInfo (idOccInfo bndr)
  , (bndrs, body) <- etaExpandToJoinPoint join_arity rhs
  , let str_sig   = idDmdSig bndr
        str_arity = count isId bndrs  -- Strictness demands are for Ids only
        join_bndr = bndr `asJoinId`        join_arity
                         `setIdDmdSig` etaConvertDmdSig str_arity str_sig
  = Just (join_bndr, mkLams bndrs body)

  | otherwise
  = Nothing

joinPointBindings_maybe :: [(InBndr, InExpr)] -> Maybe [(InBndr, InExpr)]
joinPointBindings_maybe bndrs
  = mapM (uncurry joinPointBinding_maybe) bndrs


{- *********************************************************************
*                                                                      *
         exprIsConApp_maybe
*                                                                      *
************************************************************************

Note [exprIsConApp_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~
exprIsConApp_maybe is a very important function.  There are two principal
uses:
  * case e of { .... }
  * cls_op e, where cls_op is a class operation

In both cases you want to know if e is of form (C e1..en) where C is
a data constructor.

However e might not *look* as if


Note [exprIsConApp_maybe on literal strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #9400 and #13317.

Conceptually, a string literal "abc" is just ('a':'b':'c':[]), but in Core
they are represented as unpackCString# "abc"# by GHC.Core.Make.mkStringExprFS, or
unpackCStringUtf8# when the literal contains multi-byte UTF8 characters.

For optimizations we want to be able to treat it as a list, so they can be
decomposed when used in a case-statement. exprIsConApp_maybe detects those
calls to unpackCString# and returns:

Just (':', [Char], ['a', unpackCString# "bc"]).

We need to be careful about UTF8 strings here. ""# contains an encoded ByteString, so
we call utf8UnconsByteString to correctly deal with the encoding and splitting.

We must also be careful about
   lvl = "foo"#
   ...(unpackCString# lvl)...
to ensure that we see through the let-binding for 'lvl'.  Hence the
(exprIsLiteral_maybe .. arg) in the guard before the call to
dealWithStringLiteral.

The tests for this function are in T9400.

Note [Push coercions in exprIsConApp_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #13025 I found a case where we had
    op (df @t1 @t2)     -- op is a ClassOp
where
    df = (/\a b. K e1 e2) |> g

To get this to come out we need to simplify on the fly
   ((/\a b. K e1 e2) |> g) @t1 @t2

Hence the use of pushCoArgs.

Note [exprIsConApp_maybe on data constructors with wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem:
- some data constructors have wrappers
- these wrappers inline late (see MkId Note [Activation for data constructor wrappers])
- but we still want case-of-known-constructor to fire early.

Example:
   data T = MkT !Int
   $WMkT n = case n of n' -> MkT n'   -- Wrapper for MkT
   foo x = case $WMkT e of MkT y -> blah

Here we want the case-of-known-constructor transformation to fire, giving
   foo x = case e of x' -> let y = x' in blah

Here's how exprIsConApp_maybe achieves this:

0.  Start with scrutinee = $WMkT e

1.  Inline $WMkT on-the-fly.  That's why data-constructor wrappers are marked
    as expandable. (See GHC.Core.Utils.isExpandableApp.) Now we have
      scrutinee = (\n. case n of n' -> MkT n') e

2.  Beta-reduce the application, generating a floated 'let'.
    See Note [beta-reduction in exprIsConApp_maybe] below.  Now we have
      scrutinee = case n of n' -> MkT n'
      with floats {Let n = e}

3.  Float the "case x of x' ->" binding out.  Now we have
      scrutinee = MkT n'
      with floats {Let n = e; case n of n' ->}

And now we have a known-constructor MkT that we can return.

Notice that both (2) and (3) require exprIsConApp_maybe to gather and return
a bunch of floats, both let and case bindings.

Note that this strategy introduces some subtle scenarios where a data-con
wrapper can be replaced by a data-con worker earlier than we’d like, see
Note [exprIsConApp_maybe for data-con wrappers: tricky corner].

Note [beta-reduction in exprIsConApp_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The unfolding a definition (_e.g._ a let-bound variable or a datacon wrapper) is
typically a function. For instance, take the wrapper for MkT in Note
[exprIsConApp_maybe on data constructors with wrappers]:

    $WMkT n = case n of { n' -> T n' }

If `exprIsConApp_maybe` is trying to analyse `$MkT arg`, upon unfolding of $MkT,
it will see

   (\n -> case n of { n' -> T n' }) arg

In order to go progress, `exprIsConApp_maybe` must perform a beta-reduction.

We don't want to blindly substitute `arg` in the body of the function, because
it duplicates work. We can (and, in fact, used to) substitute `arg` in the body,
but only when `arg` is a variable (or something equally work-free).

But, because of Note [exprIsConApp_maybe on data constructors with wrappers],
'exprIsConApp_maybe' now returns floats. So, instead, we can beta-reduce
_always_:

    (\x -> body) arg

Is transformed into

   let x = arg in body

Which, effectively, means emitting a float `let x = arg` and recursively
analysing the body.

For newtypes, this strategy requires that their wrappers have compulsory unfoldings.
Suppose we have
   newtype T a b where
     MkT :: a -> T b a   -- Note args swapped

This defines a worker function MkT, a wrapper function $WMkT, and an axT:
   $WMkT :: forall a b. a -> T b a
   $WMkT = /\b a. \(x:a). MkT a b x    -- A real binding

   MkT :: forall a b. a -> T a b
   MkT = /\a b. \(x:a). x |> (ax a b)  -- A compulsory unfolding

   axiom axT :: a ~R# T a b

Now we are optimising
   case $WMkT (I# 3) |> sym axT of I# y -> ...
we clearly want to simplify this. If $WMkT did not have a compulsory
unfolding, we would end up with
   let a = I#3 in case a of I# y -> ...
because in general, we do this on-the-fly beta-reduction
   (\x. e) blah  -->  let x = blah in e
and then float the let.  (Substitution would risk duplicating 'blah'.)

But if the case-of-known-constructor doesn't actually fire (i.e.
exprIsConApp_maybe does not return Just) then nothing happens, and nothing
will happen the next time either.

See test T16254, which checks the behavior of newtypes.

Note [Don't float join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exprIsConApp_maybe should succeed on
   let v = e in Just v
returning [x=e] as one of the [FloatBind].  But it must
NOT succeed on
   join j x = rhs in Just v
because join-points can't be gaily floated.  Consider
   case (join j x = rhs in Just) of
     K p q -> blah
We absolutely must not "simplify" this to
   join j x = rhs
   in blah
because j's return type is (Maybe t), quite different to blah's.

You might think this could never happen, because j can't be
tail-called in the body if the body returns a constructor.  But
in !3113 we had a /dead/ join point (which is not illegal),
and its return type was wonky.

The simple thing is not to float a join point.  The next iteration
of the simplifier will sort everything out.  And it there is
a join point, the chances are that the body is not a constructor
application, so failing faster is good.

Note [exprIsConApp_maybe for data-con wrappers: tricky corner]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking

  * exprIsConApp_maybe honours the inline phase; that is, it does not look
    inside the unfolding for an Id unless its unfolding is active in this phase.
    That phase-sensitivity is expressed in the InScopeEnv (specifically, the
    IdUnfoldingFun component of the InScopeEnv) passed to exprIsConApp_maybe.

  * Data-constructor wrappers are active only in phase 0 (the last phase);
    see Note [Activation for data constructor wrappers] in GHC.Types.Id.Make.

On the face of it that means that exprIsConApp_maybe won't look inside data
constructor wrappers until phase 0. But that seems pretty Bad. So we cheat.
For data con wrappers we unconditionally look inside its unfolding, regardless
of phase, so that we get case-of-known-constructor to fire in every phase.

Perhaps unsurprisingly, this cheating can backfire. An example:

    data T = C !A B
    foo p q = let x = C e1 e2 in seq x $ f x
    {-# RULE "wurble" f (C a b) = b #-}

In Core, the RHS of foo is

    let x = $WC e1 e2 in case x of y { C _ _ -> f x }

and after doing a binder swap and inlining x, we have:

    case $WC e1 e2 of y { C _ _ -> f y }

Case-of-known-constructor fires, but now we have to reconstruct a binding for
`y` (which was dead before the binder swap) on the RHS of the case alternative.
Naturally, we’ll use the worker:

    case e1 of a { DEFAULT -> let y = C a e2 in f y }

and after inlining `y`, we have:

    case e1 of a { DEFAULT -> f (C a e2) }

Now we might hope the "wurble" rule would fire, but alas, it will not: we have
replaced $WC with C, but the (desugared) rule matches on $WC! We weren’t
supposed to inline $WC yet for precisely that reason (see Note [Activation for
data constructor wrappers]), but our cheating in exprIsConApp_maybe came back to
bite us.

This is rather unfortunate, especially since this can happen inside stable
unfoldings as well as ordinary code (which really happened, see !3041). But
there is no obvious solution except to delay case-of-known-constructor on
data-con wrappers, and that cure would be worse than the disease.

This Note exists solely to document the problem.
-}

data ConCont = CC [CoreExpr] Coercion
                  -- Substitution already applied

-- | Returns @Just ([b1..bp], dc, [t1..tk], [x1..xn])@ if the argument
-- expression is a *saturated* constructor application of the form @let b1 in
-- .. let bp in dc t1..tk x1 .. xn@, where t1..tk are the
-- *universally-quantified* type args of 'dc'. Floats can also be (and most
-- likely are) single-alternative case expressions. Why does
-- 'exprIsConApp_maybe' return floats? We may have to look through lets and
-- cases to detect that we are in the presence of a data constructor wrapper. In
-- this case, we need to return the lets and cases that we traversed. See Note
-- [exprIsConApp_maybe on data constructors with wrappers]. Data constructor wrappers
-- are unfolded late, but we really want to trigger case-of-known-constructor as
-- early as possible. See also Note [Activation for data constructor wrappers]
-- in "GHC.Types.Id.Make".
--
-- We also return the incoming InScopeSet, augmented with
-- the binders from any [FloatBind] that we return
exprIsConApp_maybe :: HasDebugCallStack
                   => InScopeEnv -> CoreExpr
                   -> Maybe (InScopeSet, [FloatBind], DataCon, [Type], [CoreExpr])
exprIsConApp_maybe (in_scope, id_unf) expr
  = go (Left in_scope) [] expr (CC [] (mkRepReflCo (exprType expr)))
  where
    go :: Either InScopeSet Subst
             -- Left in-scope  means "empty substitution"
             -- Right subst    means "apply this substitution to the CoreExpr"
             -- NB: in the call (go subst floats expr cont)
             --     the substitution applies to 'expr', but /not/ to 'floats' or 'cont'
       -> [FloatBind] -> CoreExpr -> ConCont
             -- Notice that the floats here are in reverse order
       -> Maybe (InScopeSet, [FloatBind], DataCon, [Type], [CoreExpr])
    go subst floats (Tick t expr) cont
       | not (tickishIsCode t) = go subst floats expr cont

    go subst floats (Cast expr co1) (CC args co2)
       | Just (args', m_co1') <- pushCoArgs (subst_co subst co1) args
            -- See Note [Push coercions in exprIsConApp_maybe]
       = case m_co1' of
           MCo co1' -> go subst floats expr (CC args' (co1' `mkTransCo` co2))
           MRefl    -> go subst floats expr (CC args' co2)

    go subst floats (App fun arg) (CC args co)
       = go subst floats fun (CC (subst_expr subst arg : args) co)

    go subst floats (Lam bndr body) (CC (arg:args) co)
       | exprIsTrivial arg          -- Don't duplicate stuff!
       = go (extend subst bndr arg) floats body (CC args co)
       | otherwise
       = let (subst', bndr') = subst_bndr subst bndr
             float           = FloatLet (NonRec bndr' arg)
         in go subst' (float:floats) body (CC args co)

    go subst floats (Let (NonRec bndr rhs) expr) cont
       | not (isJoinId bndr)
         -- Crucial guard! See Note [Don't float join points]
       = let rhs'            = subst_expr subst rhs
             (subst', bndr') = subst_bndr subst bndr
             float           = FloatLet (NonRec bndr' rhs')
         in go subst' (float:floats) expr cont

    go subst floats (Case scrut b _ [Alt con vars expr]) cont
       = let
          scrut'           = subst_expr subst scrut
          (subst', b')     = subst_bndr subst b
          (subst'', vars') = subst_bndrs subst' vars
          float            = FloatCase scrut' b' con vars'
         in
           go subst'' (float:floats) expr cont

    go (Right sub) floats (Var v) cont
       = go (Left (substInScope sub))
            floats
            (lookupIdSubst sub v)
            cont

    go (Left in_scope) floats (Var fun) cont@(CC args co)

        | Just con <- isDataConWorkId_maybe fun
        , count isValArg args == idArity fun
        = succeedWith in_scope floats $
          pushCoDataCon con args co

        -- Look through data constructor wrappers: they inline late (See Note
        -- [Activation for data constructor wrappers]) but we want to do
        -- case-of-known-constructor optimisation eagerly (see Note
        -- [exprIsConApp_maybe on data constructors with wrappers]).
        | isDataConWrapId fun
        , let rhs = uf_tmpl (realIdUnfolding fun)
        = go (Left in_scope) floats rhs cont

        -- Look through dictionary functions; see Note [Unfolding DFuns]
        | DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = dfun_args } <- unfolding
        , bndrs `equalLength` args    -- See Note [DFun arity check]
        , let in_scope' = extend_in_scope (exprsFreeVars dfun_args)
              subst = mkOpenSubst in_scope' (bndrs `zip` args)
              -- We extend the in-scope set here to silence warnings from
              -- substExpr when it finds not-in-scope Ids in dfun_args.
              -- simplOptExpr initialises the in-scope set with exprFreeVars,
              -- but that doesn't account for DFun unfoldings
        = succeedWith in_scope floats $
          pushCoDataCon con (map (substExpr subst) dfun_args) co

        -- Look through unfoldings, but only arity-zero one;
        -- if arity > 0 we are effectively inlining a function call,
        -- and that is the business of callSiteInline.
        -- In practice, without this test, most of the "hits" were
        -- CPR'd workers getting inlined back into their wrappers,
        | idArity fun == 0
        , Just rhs <- expandUnfolding_maybe unfolding
        , let in_scope' = extend_in_scope (exprFreeVars rhs)
        = go (Left in_scope') floats rhs cont

        -- See Note [exprIsConApp_maybe on literal strings]
        | (fun `hasKey` unpackCStringIdKey) ||
          (fun `hasKey` unpackCStringUtf8IdKey)
        , [arg]              <- args
        , Just (LitString str) <- exprIsLiteral_maybe (in_scope, id_unf) arg
        = succeedWith in_scope floats $
          dealWithStringLiteral fun str co
        where
          unfolding = id_unf fun
          extend_in_scope unf_fvs
            | isLocalId fun = in_scope `extendInScopeSetSet` unf_fvs
            | otherwise     = in_scope
            -- A GlobalId has no (LocalId) free variables; and the
            -- in-scope set tracks only LocalIds

    go _ _ _ _ = Nothing

    succeedWith :: InScopeSet -> [FloatBind]
                -> Maybe (DataCon, [Type], [CoreExpr])
                -> Maybe (InScopeSet, [FloatBind], DataCon, [Type], [CoreExpr])
    succeedWith in_scope rev_floats x
      = do { (con, tys, args) <- x
           ; let floats = reverse rev_floats
           ; return (in_scope, floats, con, tys, args) }

    ----------------------------
    -- Operations on the (Either InScopeSet GHC.Core.Subst)
    -- The Left case is wildly dominant
    subst_co (Left {}) co = co
    subst_co (Right s) co = GHC.Core.Subst.substCo s co

    subst_expr (Left {}) e = e
    subst_expr (Right s) e = substExpr s e

    subst_bndr msubst bndr
      = (Right subst', bndr')
      where
        (subst', bndr') = substBndr subst bndr
        subst = case msubst of
                  Left in_scope -> mkEmptySubst in_scope
                  Right subst   -> subst

    subst_bndrs subst bs = mapAccumL subst_bndr subst bs

    extend (Left in_scope) v e = Right (extendSubst (mkEmptySubst in_scope) v e)
    extend (Right s)       v e = Right (extendSubst s v e)


-- See Note [exprIsConApp_maybe on literal strings]
dealWithStringLiteral :: Var -> BS.ByteString -> Coercion
                      -> Maybe (DataCon, [Type], [CoreExpr])

-- This is not possible with user-supplied empty literals, GHC.Core.Make.mkStringExprFS
-- turns those into [] automatically, but just in case something else in GHC
-- generates a string literal directly.
dealWithStringLiteral fun str co =
  case utf8UnconsByteString str of
    Nothing -> pushCoDataCon nilDataCon [Type charTy] co
    Just (char, charTail) ->
      let char_expr = mkConApp charDataCon [mkCharLit char]
          -- In singleton strings, just add [] instead of unpackCstring# ""#.
          rest = if BS.null charTail
                   then mkConApp nilDataCon [Type charTy]
                   else App (Var fun)
                            (Lit (LitString charTail))

      in pushCoDataCon consDataCon [Type charTy, char_expr, rest] co

{-
Note [Unfolding DFuns]
~~~~~~~~~~~~~~~~~~~~~~
DFuns look like

  df :: forall a b. (Eq a, Eq b) -> Eq (a,b)
  df a b d_a d_b = MkEqD (a,b) ($c1 a b d_a d_b)
                               ($c2 a b d_a d_b)

So to split it up we just need to apply the ops $c1, $c2 etc
to the very same args as the dfun.  It takes a little more work
to compute the type arguments to the dictionary constructor.

Note [DFun arity check]
~~~~~~~~~~~~~~~~~~~~~~~
Here we check that the total number of supplied arguments (including
type args) matches what the dfun is expecting.  This may be *less*
than the ordinary arity of the dfun: see Note [DFun unfoldings] in GHC.Core
-}

exprIsLiteral_maybe :: InScopeEnv -> CoreExpr -> Maybe Literal
-- Same deal as exprIsConApp_maybe, but much simpler
-- Nevertheless we do need to look through unfoldings for
-- string literals, which are vigorously hoisted to top level
-- and not subsequently inlined
exprIsLiteral_maybe env@(_, id_unf) e
  = case e of
      Lit l     -> Just l
      Tick _ e' -> exprIsLiteral_maybe env e' -- dubious?
      Var v     -> expandUnfolding_maybe (id_unf v)
                    >>= exprIsLiteral_maybe env
      _         -> Nothing

{-
Note [exprIsLambda_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~
exprIsLambda_maybe will, given an expression `e`, try to turn it into the form
`Lam v e'` (returned as `Just (v,e')`). Besides using lambdas, it looks through
casts (using the Push rule), and it unfolds function calls if the unfolding
has a greater arity than arguments are present.

Currently, it is used in GHC.Core.Rules.match, and is required to make
"map coerce = coerce" match.
-}

exprIsLambda_maybe :: HasDebugCallStack
                   => InScopeEnv -> CoreExpr
                   -> Maybe (Var, CoreExpr,[CoreTickish])
    -- See Note [exprIsLambda_maybe]

-- The simple case: It is a lambda already
exprIsLambda_maybe _ (Lam x e)
    = Just (x, e, [])

-- Still straightforward: Ticks that we can float out of the way
exprIsLambda_maybe (in_scope_set, id_unf) (Tick t e)
    | tickishFloatable t
    , Just (x, e, ts) <- exprIsLambda_maybe (in_scope_set, id_unf) e
    = Just (x, e, t:ts)

-- Also possible: A casted lambda. Push the coercion inside
exprIsLambda_maybe (in_scope_set, id_unf) (Cast casted_e co)
    | Just (x, e,ts) <- exprIsLambda_maybe (in_scope_set, id_unf) casted_e
    -- Only do value lambdas.
    -- this implies that x is not in scope in gamma (makes this code simpler)
    , not (isTyVar x) && not (isCoVar x)
    , assert (not $ x `elemVarSet` tyCoVarsOfCo co) True
    , Just (x',e') <- pushCoercionIntoLambda in_scope_set x e co
    , let res = Just (x',e',ts)
    = --pprTrace "exprIsLambda_maybe:Cast" (vcat [ppr casted_e,ppr co,ppr res)])
      res

-- Another attempt: See if we find a partial unfolding
exprIsLambda_maybe (in_scope_set, id_unf) e
    | (Var f, as, ts) <- collectArgsTicks tickishFloatable e
    , idArity f > count isValArg as
    -- Make sure there is hope to get a lambda
    , Just rhs <- expandUnfolding_maybe (id_unf f)
    -- Optimize, for beta-reduction
    , let e' = simpleOptExprWith defaultSimpleOpts (mkEmptySubst in_scope_set) (rhs `mkApps` as)
    -- Recurse, because of possible casts
    , Just (x', e'', ts') <- exprIsLambda_maybe (in_scope_set, id_unf) e'
    , let res = Just (x', e'', ts++ts')
    = -- pprTrace "exprIsLambda_maybe:Unfold" (vcat [ppr e, ppr (x',e'')])
      res

exprIsLambda_maybe _ _e
    = -- pprTrace "exprIsLambda_maybe:Fail" (vcat [ppr _e])
      Nothing
