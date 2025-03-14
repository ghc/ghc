{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}


module GHC.Core.SimpleOpt (
        SimpleOpts (..), defaultSimpleOpts,

        -- ** Simple expression optimiser
        simpleOptPgm, simpleOptExpr, simpleOptExprNoInline, simpleOptExprWith,

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
import GHC.Core.Make ( FloatBind(..), mkWildValBinder )
import GHC.Core.Opt.OccurAnal( occurAnalyseExpr, occurAnalysePgm, zapLambdaBndrs )
import GHC.Core.DataCon
import GHC.Core.Coercion.Opt ( optCoercion, OptCoercionOpts (..) )
import GHC.Core.Type hiding ( substTy, extendTvSubst, extendCvSubst, extendTvSubstList
                            , isInScope, substTyVarBndr, cloneTyVarBndr )
import GHC.Core.Predicate( isCoVarType )
import GHC.Core.Coercion hiding ( substCo, substCoVarBndr )

import GHC.Types.Literal
import GHC.Types.Id
import GHC.Types.Id.Info  ( realUnfoldingInfo, setUnfoldingInfo, setRuleInfo, IdInfo (..) )
import GHC.Types.Var      ( isNonCoVarId )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Demand( etaConvertDmdSig, topSubDmd )
import GHC.Types.Tickish
import GHC.Types.Basic

import GHC.Builtin.Types
import GHC.Builtin.Names

import GHC.Unit.Module ( Module )
import GHC.Utils.Encoding
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Data.Maybe       ( orElse )
import GHC.Data.Graph.UnVar
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

Note [The InScopeSet for simpleOptExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Care must be taken to remove unfoldings from `Var`s collected by exprFreeVars
before using them to construct an in-scope set hence `zapIdUnfolding` in `init_subst`.
Consider calling `simpleOptExpr` on an expression like

```
 case x of (a,b) -> (x,a)
```

* One of those two occurrences of x has an unfolding (the one in (x,a), with
unfolding x = (a,b)) and the other does not. (Inside a case GHC adds
unfolding-info to the scrutinee's Id.)
* But exprFreeVars just builds a set, so it's a bit random which occurrence is collected.
* Then simpleOptExpr replaces each occurrence of x with the one in the in-scope set.
* Bad bad bad: then the x in  case x of ... may be replaced with a version that has an unfolding.

See ticket #25790
-}

-- | Simple optimiser options
data SimpleOpts = SimpleOpts
   { so_uf_opts :: !UnfoldingOpts   -- ^ Unfolding options
   , so_co_opts :: !OptCoercionOpts -- ^ Coercion optimiser options
   , so_eta_red :: !Bool            -- ^ Eta reduction on?
   , so_inline :: !Bool             -- ^ False <=> do no inlining whatsoever,
                                    --    even for trivial or used-once things
   }

-- | Default options for the Simple optimiser.
defaultSimpleOpts :: SimpleOpts
defaultSimpleOpts = SimpleOpts
   { so_uf_opts = defaultUnfoldingOpts
   , so_co_opts = OptCoercionOpts { optCoercionEnabled = False }
   , so_eta_red = False
   , so_inline  = True
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
    init_subst = mkEmptySubst (mkInScopeSet (mapVarSet zapIdUnfolding (exprFreeVars expr)))
        -- zapIdUnfolding: see Note [The InScopeSet for simpleOptExpr]

        -- It's a bit painful to call exprFreeVars, because it makes
        -- three passes instead of two (occ-anal, and go)

simpleOptExprNoInline :: HasDebugCallStack => SimpleOpts -> CoreExpr -> CoreExpr
-- A variant of simpleOptExpr, but without
-- occurrence analysis or inlining of any kind.
-- Result: we don't inline evidence bindings, which is useful for the specialiser
simpleOptExprNoInline opts expr
  = simple_opt_expr init_env expr
  where
    init_opts  = opts { so_inline = False }
    init_env   = (emptyEnv init_opts) { soe_subst = init_subst }
    init_subst = mkEmptySubst (mkInScopeSet (exprFreeVars expr))

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

        , soe_rec_ids :: !UnVarSet
             -- ^ Fast OutVarSet tracking which recursive RHSs we are analysing.
             -- See Note [Eta reduction in recursive RHSs]
        }

instance Outputable SimpleOptEnv where
  ppr (SOE { soe_inl = inl, soe_subst = subst })
    = text "SOE {" <+> vcat [ text "soe_inl   =" <+> ppr inl
                            , text "soe_subst =" <+> ppr subst ]
                   <+> text "}"

emptyEnv :: SimpleOpts -> SimpleOptEnv
emptyEnv opts = SOE { soe_inl     = emptyVarEnv
                    , soe_subst   = emptySubst
                    , soe_rec_ids = emptyUnVarSet
                    , soe_opts    = opts  }

soeZapSubst :: SimpleOptEnv -> SimpleOptEnv
soeZapSubst env@(SOE { soe_subst = subst })
  = env { soe_inl = emptyVarEnv, soe_subst = zapSubst subst }

soeInScope :: SimpleOptEnv -> InScopeSet
soeInScope (SOE { soe_subst = subst }) = substInScopeSet subst

soeSetInScope :: InScopeSet -> SimpleOptEnv -> SimpleOptEnv
soeSetInScope in_scope env2@(SOE { soe_subst = subst2 })
  = env2 { soe_subst = setInScope subst2 in_scope }

enterRecGroupRHSs :: SimpleOptEnv -> [OutBndr] -> (SimpleOptEnv -> (SimpleOptEnv, r))
                  -> (SimpleOptEnv, r)
enterRecGroupRHSs env bndrs k
  = (env'{soe_rec_ids = soe_rec_ids env}, r)
  where
    (env', r) = k env{soe_rec_ids = extendUnVarSetList bndrs (soe_rec_ids env)}

---------------
simple_opt_clo :: HasDebugCallStack
               => InScopeSet
               -> SimpleClo
               -> OutExpr
simple_opt_clo in_scope (e_env, e)
  = simple_opt_expr (soeSetInScope in_scope e_env) e

simple_opt_expr :: HasDebugCallStack => SimpleOptEnv -> InExpr -> OutExpr
simple_opt_expr env expr
  = go expr
  where
    rec_ids      = soe_rec_ids env
    subst        = soe_subst env
    in_scope     = substInScopeSet subst
    in_scope_env = ISE in_scope alwaysActiveUnfoldingFun

    ---------------
    go (Var v)
       | Just clo <- lookupVarEnv (soe_inl env) v
       = simple_opt_clo in_scope clo
       | otherwise
       = lookupIdSubst (soe_subst env) v

    go (App e1 e2)      = simple_app env e1 [(env,e2)]
    go (Type ty)        = Type     (substTyUnchecked subst ty)
    go (Coercion co)    = Coercion (go_co co)
    go (Lit lit)        = Lit lit
    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
    go (Cast e co)      = mk_cast (go e) (go_co co)
    go (Let bind body)  = case simple_opt_bind env bind NotTopLevel of
                             (env', Nothing)   -> simple_opt_expr env' body
                             (env', Just bind) -> Let bind (simple_opt_expr env' body)

    go lam@(Lam {})     = go_lam env [] lam
    go (Case e b ty as)
      | isDeadBinder b
      , Just (_, [], con, _tys, es) <- exprIsConApp_maybe in_scope_env e'
        -- We don't need to be concerned about floats when looking for coerce.
      , Just (Alt altcon bs rhs) <- findAlt (DataAlt con) as
      = case altcon of
          DEFAULT -> go rhs
          _       -> foldr wrapLet (simple_opt_expr env' rhs) mb_prs
            where
              (env', mb_prs) = mapAccumL (simple_out_bind NotTopLevel) env $
                               zipEqual bs es

         -- See Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , [Alt DEFAULT _ rhs] <- as
      , isCoVarType (varType b)
      , (Var fun, _args) <- collectArgs e
      , fun `hasKey` coercibleSCSelIdKey
         -- without this last check, we get #11230
      = go rhs

      | otherwise
      = Case e' b' (substTyUnchecked subst ty)
                   (map (go_alt env') as)
      where
        e' = go e
        (env', b') = subst_opt_bndr env b

    ----------------------
    go_co co = optCoercion (so_co_opts (soe_opts env)) subst co

    ----------------------
    go_alt env (Alt con bndrs rhs)
      = Alt con bndrs' (simple_opt_expr env' rhs)
      where
        (env', bndrs') = subst_opt_bndrs env bndrs

    ----------------------
    -- go_lam tries eta reduction
    -- It is quite important that it does so. I tried removing this code and
    -- got a lot of regressions, e.g., +11% ghc/alloc in T18223 and many
    -- run/alloc increases. Presumably RULEs are affected.
    go_lam env bs' (Lam b e)
       = go_lam env' (b':bs') e
       where
         (env', b') = subst_opt_bndr env b
    go_lam env bs' e
       | so_eta_red (soe_opts env)
       , Just etad_e <- tryEtaReduce rec_ids bs e' topSubDmd = etad_e
       | otherwise                                           = mkLams bs e'
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
  = simple_app (soeSetInScope (soeInScope env) env') e as

  | let unf = idUnfolding v
  , isCompulsoryUnfolding unf
  , isAlwaysActive (idInlineActivation v)
    -- See Note [Unfold compulsory unfoldings in RULE LHSs]
  , Just rhs <- maybeUnfoldingTemplate unf
    -- Always succeeds if isCompulsoryUnfolding does
  = simple_app (soeZapSubst env) rhs as

  | otherwise
  , let out_fn = lookupIdSubst (soe_subst env) v
  = finish_app env out_fn as

simple_app env (App e1 e2) as
  = simple_app env e1 ((env, e2) : as)

simple_app env e@(Lam {}) as@(_:_)
  = do_beta env (zapLambdaBndrs e n_args) as
    -- Be careful to zap the lambda binders if necessary
    -- c.f. the Lam case of simplExprF1 in GHC.Core.Opt.Simplify
    -- Lacking this zap caused #19347, when we had a redex
    --   (\ a b. K a b) e1 e2
    -- where (as it happens) the eta-expanded K is produced by
    -- Note [Typechecking data constructors] in GHC.Tc.Gen.Head
  where
    n_args = length as

    do_beta env (Lam b body) (a:as)
      | -- simpl binder before looking at its type
        -- See Note [Dark corner with representation polymorphism]
        needsCaseBinding (idType b') (snd a)
        -- This arg must not be inlined (side-effects) and cannot be let-bound,
        -- due to the let-can-float invariant. So simply case-bind it here.
      , let a' = simple_opt_clo (soeInScope env) a
      = mkDefaultCase a' b' $ do_beta env' body as

      | (env'', mb_pr) <- simple_bind_pair env' b (Just b') a NotTopLevel
      = wrapLet mb_pr $ do_beta env'' body as

      where (env', b') = subst_opt_bndr env b

    do_beta env body as
      = simple_app env body as

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

finish_app :: HasDebugCallStack
           => SimpleOptEnv -> OutExpr -> [SimpleClo] -> OutExpr
-- See Note [Eliminate casts in function position]
finish_app env (Cast (Lam x e) co) as@(_:_)
  | not (isTyVar x) && not (isCoVar x)
  , assert (not $ x `elemVarSet` tyCoVarsOfCo co) True
  , Just (x',e') <- pushCoercionIntoLambda (soeInScope env) x e co
  = simple_app (soeZapSubst env) (Lam x' e') as

finish_app env fun args
  = foldl mk_app fun args
  where
    in_scope = soeInScope env
    mk_app fun arg = App fun (simple_opt_clo in_scope arg)

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
  = (env2, res_bind)
  where
    res_bind          = Just (Rec (reverse rev_prs'))
    prs'              = joinPointBindings_maybe prs `orElse` prs
    (env1, bndrs')    = subst_opt_bndrs env (map fst prs')
    (env2, rev_prs')  = enterRecGroupRHSs env1 bndrs' $ \env ->
                          foldl' do_pr (env, []) (prs' `zip` bndrs')
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
simple_bind_pair env@(SOE { soe_inl = inl_env, soe_subst = subst, soe_opts = opts })
                 in_bndr mb_out_bndr clo@(rhs_env, in_rhs)
                 top_level
  | Type ty <- in_rhs        -- let a::* = TYPE ty in <body>
  , let out_ty = substTyUnchecked (soe_subst rhs_env) ty
  = assertPpr (isTyVar in_bndr) (ppr in_bndr $$ ppr in_rhs) $
    (env { soe_subst = extendTvSubst subst in_bndr out_ty }, Nothing)

  | Coercion co <- in_rhs
  , let out_co = optCoercion (so_co_opts (soe_opts env)) (soe_subst rhs_env) co
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
    in_scope   = substInScopeSet subst

    out_rhs | JoinPoint join_arity <- idJoinPointHood in_bndr
            = simple_join_rhs join_arity
            | otherwise
            = simple_opt_clo in_scope clo

    simple_join_rhs join_arity -- See Note [Preserve join-binding arity]
      = mkLams join_bndrs' (simple_opt_expr env_body join_body)
      where
        env0 = soeSetInScope in_scope rhs_env
        (join_bndrs, join_body) = collectNBinders join_arity in_rhs
        (env_body, join_bndrs') = subst_opt_bndrs env0 join_bndrs

    pre_inline_unconditionally :: Bool
    pre_inline_unconditionally
       | not (so_inline opts)     = False    -- Not if so_inline is False
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

do_beta_by_substitution :: Id -> CoreExpr -> Bool
-- True <=> you can inline (bndr = rhs) by substitution
-- See Note [Exploit occ-info in exprIsConApp_maybe]
do_beta_by_substitution bndr rhs
  = exprIsTrivial rhs                   -- Can duplicate
    || safe_to_inline (idOccInfo bndr)  -- Occurs at most once

do_case_elim :: CoreExpr -> Id -> [Id] -> Bool
do_case_elim scrut case_bndr alt_bndrs
  =  exprIsHNF scrut
  && safe_to_inline (idOccInfo case_bndr)
  && all isDeadBinder alt_bndrs

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
simple_out_bind_pair env@(SOE { soe_subst = subst, soe_opts = opts })
                     in_bndr mb_out_bndr out_rhs
                     occ_info active stable_unf top_level
  | assertPpr (isNonCoVarId in_bndr) (ppr in_bndr)
    -- Type and coercion bindings are caught earlier
    -- See Note [Core type and coercion invariant]
    post_inline_unconditionally
  = ( env' { soe_subst = extendIdSubst subst in_bndr out_rhs }
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
       | not (so_inline opts)  = False -- Not if so_inline is False
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

Note [Eliminate casts in function position]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following program:

  type R :: Type -> RuntimeRep
  type family R a where { R Float = FloatRep; R Double = DoubleRep }
  type F :: forall (a :: Type) -> TYPE (R a)
  type family F a where { F Float = Float#  ; F Double = Double# }

  type N :: forall (a :: Type) -> TYPE (R a)
  newtype N a = MkN (F a)

As MkN is a newtype, its unfolding is a lambda which wraps its argument
in a cast:

  MkN :: forall (a :: Type). F a -> N a
  MkN = /\a \(x::F a). x |> co_ax
    -- recall that F a :: TYPE (R a)

This is a representation-polymorphic lambda, in which the binder has an unknown
representation (R a). We can't compile such a lambda on its own, but we can
compile instantiations, such as `MkN @Float` or `MkN @Double`.

Our strategy to avoid running afoul of the representation-polymorphism
invariants of Note [Representation polymorphism invariants] in GHC.Core is thus:

  1. Give the newtype a compulsory unfolding (it has no binding, as we can't
     define lambdas with representation-polymorphic value binders in source Haskell).
  2. Rely on the optimiser to beta-reduce away any representation-polymorphic
     value binders.

For example, consider the application

    MkN @Float 34.0#

After inlining MkN we'll get

   ((/\a \(x:F a). x |> co_ax) @Float) |> co 34#

where co :: (F Float -> N Float) ~ (Float# ~ N Float)

But to actually beta-reduce that lambda, we need to push the 'co'
inside the `\x` with pushCoecionIntoLambda.  Hence the extra
equation for Cast-of-Lam in finish_app.

This is regrettably delicate.

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
    id2    = updateIdTypeAndMult (substTyUnchecked subst) id1
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

   unfolding_from_rhs = mkUnfolding uf_opts VanillaSrc
                                    (isTopLevel top_level)
                                    False -- may be bottom or not
                                    False -- Not a join point
                                    new_rhs Nothing

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

Note [Unfold compulsory unfoldings in RULE LHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
too.  Achieving all this is surprisingly tricky:

(MC1) We must compulsorily unfold MkAge to a cast.
      See Note [Compulsory newtype unfolding] in GHC.Types.Id.Make

(MC2) We must compulsorily unfold coerce on the rule LHS, yielding
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
  which transforms the above to

    forall a b (co :: a ~R# b).
      let dict = MkCoercible @* @a @b co in
      case Coercible_SCSel @* @a @b dict of
        _ [Dead] -> map @a @b (\(x :: a) -> case dict of
           MkCoercible (co :: a ~R# b) -> x |> co) = let dict = ... in ...

  See Note [Desugaring coerce as cast] in GHC.HsToCore

(MC3) Now, we need simpleOptExpr to fix this up. It does so by taking three
  separate actions:
  1. Inline certain non-recursive bindings. The choice whether to inline
     is made in simple_bind_pair. Note the rather specific check for
     MkCoercible in there.

  2. Stripping case expressions like the Coercible_SCSel one.
     See the `Case` case of simple_opt_expr's `go` function.

  3. Look for case expressions that unpack something that was
     just packed and inline them. This is also done in simple_opt_expr's
     `go` function.

(MC4) The map/coerce rule is the only compelling reason for having a RULE that
  quantifies over a coercion variable, something that is otherwise Very Deeply
  Suspicious.  See Note [Casts in the template] in GHC.Core.Rules. Ugh!

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
   let a = I# 3 in case a of I# y -> ...
because in general, we do this on-the-fly beta-reduction
   (\x. e) blah  -->  let x = blah in e
and then float the let.  (Substitution would risk duplicating 'blah'.)

But if the case-of-known-constructor doesn't actually fire (i.e.
exprIsConApp_maybe does not return Just) then nothing happens, and nothing
will happen the next time either.

See test T16254, which checks the behavior of newtypes.

Note [Exploit occ-info in exprIsConApp_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose (#23159) we have a simple data constructor wrapper like this (this one
might have come from a data family instance):
   $WK x y = K x y |> co
Now suppose the simplifier sees
   case ($WK e1 e2) |> co2 of
      K p q ->  case q of ...

`exprIsConApp_maybe` expands the wrapper on the fly
(see Note [beta-reduction in exprIsConApp_maybe]). It effectively expands
that ($WK e1 e2) to
   let x = e1; y = e2 in K x y |> co

So the Simplifier might end up producing this:
   let x = e1; y = e2
   in case x of ...

But suppose `q` was used just once in the body of the `K p q` alternative; we
don't want to wait a whole Simplifier iteration to inline that `x`.  (e1 might
be another constructor for example.)  This would happen if `exprIsConApp_maybe`
we created a let for every (non-trivial) argument.  So let's not do that when
the binder is used just once!

Instead, take advantage of the occurrence-info on `x` and `y` in the unfolding
of `$WK`.  Since in `$WK` both `x` and `y` occur once, we want to effectively
expand `($WK e1 e2)` to `(K e1 e2 |> co)`.  Hence in
`do_beta_by_substitution` we say "yes" if

  (a) the RHS is trivial (so we can duplicate it);
      see call to `exprIsTrivial`
or
  (b) the binder occurs at most once (so there is no worry about duplication);
      see call to `safe_to_inline`.

To see this in action, look at testsuite/tests/perf/compiler/T15703.  The
initial Simlifier run takes 5 iterations without (b), but only 3 when we add
(b).

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

data ConCont = CC [CoreExpr] MCoercion
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
exprIsConApp_maybe ise@(ISE in_scope id_unf) expr
  = go (Left in_scope) [] expr (CC [] MRefl)
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

    go subst floats (Cast expr co1) (CC args m_co2)
       | Just (args', m_co1') <- pushCoArgs (subst_co subst co1) args
            -- See Note [Push coercions in exprIsConApp_maybe]
       = go subst floats expr (CC args' (m_co1' `mkTransMCo` m_co2))

    go subst floats (App fun arg) (CC args mco)
       | let arg_type = exprType arg
       , not (isTypeArg arg) && needsCaseBinding arg_type arg
       -- An unlifted argument that’s not ok for speculation must not simply be
       -- put into the args, as these are going to be substituted into the case
       -- alternatives, and possibly lost on the way.
       --
       -- Instead, we need need to
       -- make sure they are evaluated right here (using a case float), and
       -- the case binder can then be substituted into the case alternaties.
       --
       -- Example:
       -- Simplifying  case Mk# exp of Mk# a → rhs
       -- will use     exprIsConApp_maybe (Mk# exp)
       --
       -- Bad:  returning (Mk#, [exp]) with no floats
       --       simplifier produces rhs[exp/a], changing semantics if exp is not ok-for-spec
       -- Good: returning (Mk#, [x]) with a float of  case exp of x { DEFAULT -> [] }
       --       simplifier produces case exp of a { DEFAULT -> exp[x/a] }
       , (subst', float, bndr) <- case_bind subst arg arg_type
       = go subst' (float:floats) fun (CC (Var bndr : args) mco)
       | otherwise
       = go subst floats fun (CC (subst_expr subst arg : args) mco)

    go subst floats (Lam bndr body) (CC (arg:args) mco)
       | do_beta_by_substitution bndr arg
       = go (extend subst bndr arg) floats body (CC args mco)
       | otherwise
       = let (subst', bndr') = subst_bndr subst bndr
             float           = FloatLet (NonRec bndr' arg)
         in go subst' (float:floats) body (CC args mco)

    go subst floats (Let (NonRec bndr rhs) expr) cont
       | not (isJoinId bndr)
         -- Crucial guard! See Note [Don't float join points]
       = let rhs'            = subst_expr subst rhs
             (subst', bndr') = subst_bndr subst bndr
             float           = FloatLet (NonRec bndr' rhs')
         in go subst' (float:floats) expr cont

    go subst floats (Case scrut b _ [Alt con vars expr]) cont
       | do_case_elim scrut' b vars  -- See Note [Case elim in exprIsConApp_maybe]
       = go (extend subst b scrut') floats expr cont
       | otherwise
       = let
          (subst', b')     = subst_bndr subst b
          (subst'', vars') = subst_bndrs subst' vars
          float            = FloatCase scrut' b' con vars'
         in
           go subst'' (float:floats) expr cont
       where
          scrut'           = subst_expr subst scrut

    go (Right sub) floats (Var v) cont
       = go (Left (substInScopeSet sub))
            floats
            (lookupIdSubst sub v)
            cont

    go (Left in_scope) floats (Var fun) cont@(CC args mco)

        | Just con <- isDataConWorkId_maybe fun
        , count isValArg args == idArity fun
        , (in_scope', seq_floats, args') <- mkFieldSeqFloats in_scope con args
          -- mkFieldSeqFloats: See (SFC2) in Note [Strict fields in Core]
        = succeedWith in_scope' (seq_floats ++ floats) $
          pushCoDataCon con args' mco

        -- Look through data constructor wrappers: they inline late (See Note
        -- [Activation for data constructor wrappers]) but we want to do
        -- case-of-known-constructor optimisation eagerly (see Note
        -- [exprIsConApp_maybe on data constructors with wrappers]).
        | Just rhs <- dataConWrapUnfolding_maybe fun
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
          pushCoDataCon con (map (substExpr subst) dfun_args) mco

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
        , Just (LitString str) <- exprIsLiteral_maybe ise arg
        = succeedWith in_scope floats $
          dealWithStringLiteral fun str mco
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

    subst_in_scope (Left in_scope) = in_scope
    subst_in_scope (Right s) = substInScopeSet s

    subst_extend_in_scope (Left in_scope) v = Left (in_scope `extendInScopeSet` v)
    subst_extend_in_scope (Right s) v = Right (s `extendSubstInScope` v)

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

    case_bind :: Either InScopeSet Subst -> CoreExpr -> Type -> (Either InScopeSet Subst, FloatBind, Id)
    case_bind subst expr expr_ty = (subst', float, bndr)
      where
        bndr   = setCaseBndrEvald MarkedStrict $
                 uniqAway (subst_in_scope subst) $
                 mkWildValBinder ManyTy expr_ty
        subst' = subst_extend_in_scope subst bndr
        expr'  = subst_expr subst expr
        float  = FloatCase expr' bndr DEFAULT []

    mkFieldSeqFloats :: InScopeSet -> DataCon -> [CoreExpr] -> (InScopeSet, [FloatBind], [CoreExpr])
    -- See Note [Strict fields in Core] for what a field seq is and (SFC2) for
    -- why we insert them
    mkFieldSeqFloats in_scope dc args
      | isLazyDataConRep dc
      = (in_scope, [], args)
      | otherwise
      = (in_scope', floats', ty_args ++ val_args')
      where
        (ty_args, val_args) = splitAtList (dataConUnivAndExTyCoVars dc) args
        (in_scope', floats', val_args') = foldr do_one (in_scope, [], []) $ zipEqual str_marks val_args
        str_marks = dataConRepStrictness dc
        do_one (str, arg) (in_scope,floats,args)
          | NotMarkedStrict <- str   = no_seq
          | exprIsHNF arg            = no_seq
          | otherwise                = (in_scope', float:floats, Var bndr:args)
          where
            no_seq = (in_scope, floats, arg:args)
            (in_scope', float, bndr) =
               case case_bind (Left in_scope) arg (exprType arg) of
                 (Left in_scope', float, bndr) -> (in_scope', float, bndr)
                 (right, _, _) -> pprPanic "case_bind did not preserve Left" (ppr in_scope $$ ppr arg $$ ppr right)

-- See Note [exprIsConApp_maybe on literal strings]
dealWithStringLiteral :: Var -> BS.ByteString -> MCoercion
                      -> Maybe (DataCon, [Type], [CoreExpr])

-- This is not possible with user-supplied empty literals, GHC.Core.Make.mkStringExprFS
-- turns those into [] automatically, but just in case something else in GHC
-- generates a string literal directly.
dealWithStringLiteral fun str mco =
  case utf8UnconsByteString str of
    Nothing -> pushCoDataCon nilDataCon [Type charTy] mco
    Just (char, charTail) ->
      let char_expr = mkConApp charDataCon [mkCharLit char]
          -- In singleton strings, just add [] instead of unpackCstring# ""#.
          rest = if BS.null charTail
                   then mkConApp nilDataCon [Type charTy]
                   else App (Var fun)
                            (Lit (LitString charTail))

      in pushCoDataCon consDataCon [Type charTy, char_expr, rest] mco

{-
Note [Case elim in exprIsConApp_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   data K a = MkK !a

   $WMkK x = case x of y -> K y   -- Wrapper for MkK

   ...case $WMkK v of K w -> <rhs>

We call `exprIsConApp_maybe` on ($WMkK v); we inline the wrapper
and beta-reduce, so we get to
   exprIsConApp_maybe (case v of y -> K y)

So we may float the case, and end up with
   case v of y -> <rhs>[y/w]

But if `v` is already evaluated, the next run of the Simplifier will
eliminate the case, and we may then make more progress with <rhs>.
Better to do it in one iteration.  Hence the `do_case_elim`
check in `exprIsConApp_maybe`.

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
exprIsLiteral_maybe env@(ISE _ id_unf) e
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
exprIsLambda_maybe ise (Tick t e)
    | tickishFloatable t
    , Just (x, e, ts) <- exprIsLambda_maybe ise e
    = Just (x, e, t:ts)

-- Also possible: A casted lambda. Push the coercion inside
exprIsLambda_maybe ise@(ISE in_scope_set _) (Cast casted_e co)
    | Just (x, e,ts) <- exprIsLambda_maybe ise casted_e
    -- Only do value lambdas.
    -- this implies that x is not in scope in gamma (makes this code simpler)
    , not (isTyVar x) && not (isCoVar x)
    , assert (not $ x `elemVarSet` tyCoVarsOfCo co) True
    , Just (x',e') <- pushCoercionIntoLambda in_scope_set x e co
    , let res = Just (x',e',ts)
    = --pprTrace "exprIsLambda_maybe:Cast" (vcat [ppr casted_e,ppr co,ppr res)])
      res

-- Another attempt: See if we find a partial unfolding
exprIsLambda_maybe ise@(ISE in_scope_set id_unf) e
    | (Var f, as, ts) <- collectArgsTicks tickishFloatable e
    , idArity f > count isValArg as
    -- Make sure there is hope to get a lambda
    , Just rhs <- expandUnfolding_maybe (id_unf f)
    -- Optimize, for beta-reduction
    , let e' = simpleOptExprWith defaultSimpleOpts (mkEmptySubst in_scope_set) (rhs `mkApps` as)
    -- Recurse, because of possible casts
    , Just (x', e'', ts') <- exprIsLambda_maybe ise e'
    , let res = Just (x', e'', ts++ts')
    = -- pprTrace "exprIsLambda_maybe:Unfold" (vcat [ppr e, ppr (x',e'')])
      res

exprIsLambda_maybe _ _e
    = -- pprTrace "exprIsLambda_maybe:Fail" (vcat [ppr _e])
      Nothing
