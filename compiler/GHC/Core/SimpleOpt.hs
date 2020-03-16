{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}
module GHC.Core.SimpleOpt (
        -- ** Simple expression optimiser
        simpleOptPgm, simpleOptExpr, simpleOptExprWith,

        -- ** Join points
        joinPointBinding_maybe, joinPointBindings_maybe,

        -- ** Predicates on expressions
        exprIsConApp_maybe, exprIsLiteral_maybe, exprIsLambda_maybe,

        -- ** Coercions and casts
        pushCoArg, pushCoValArg, pushCoTyArg, collectBindersPushingCo
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core.Arity( etaExpandToJoinPoint )

import GHC.Core
import GHC.Core.Subst
import GHC.Core.Utils
import GHC.Core.FVs
import {-# SOURCE #-} GHC.Core.Unfold( mkUnfolding )
import GHC.Core.Make ( FloatBind(..) )
import GHC.Core.Ppr  ( pprCoreBindings, pprRules )
import GHC.Core.Op.OccurAnal( occurAnalyseExpr, occurAnalysePgm )
import GHC.Types.Literal  ( Literal(LitString) )
import GHC.Types.Id
import GHC.Types.Id.Info  ( unfoldingInfo, setUnfoldingInfo, setRuleInfo, IdInfo (..) )
import GHC.Types.Var      ( isNonCoVarId )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Core.DataCon
import GHC.Types.Demand( etaConvertStrictSig )
import GHC.Core.Coercion.Opt ( optCoercion )
import GHC.Core.Type hiding ( substTy, extendTvSubst, extendCvSubst, extendTvSubstList
                            , isInScope, substTyVarBndr, cloneTyVarBndr )
import GHC.Core.Coercion hiding ( substCo, substCoVarBndr )
import GHC.Core.TyCon ( tyConArity )
import TysWiredIn
import PrelNames
import GHC.Types.Basic
import GHC.Types.Module ( Module )
import ErrUtils
import GHC.Driver.Session
import Outputable
import Pair
import Util
import Maybes       ( orElse )
import FastString
import Data.List
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

simpleOptExpr :: DynFlags -> CoreExpr -> CoreExpr
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

simpleOptExpr dflags expr
  = -- pprTrace "simpleOptExpr" (ppr init_subst $$ ppr expr)
    simpleOptExprWith dflags init_subst expr
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

simpleOptExprWith :: DynFlags -> Subst -> InExpr -> OutExpr
-- See Note [The simple optimiser]
simpleOptExprWith dflags subst expr
  = simple_opt_expr init_env (occurAnalyseExpr expr)
  where
    init_env = SOE { soe_dflags = dflags
                   , soe_inl = emptyVarEnv
                   , soe_subst = subst }

----------------------
simpleOptPgm :: DynFlags -> Module
             -> CoreProgram -> [CoreRule]
             -> IO (CoreProgram, [CoreRule])
-- See Note [The simple optimiser]
simpleOptPgm dflags this_mod binds rules
  = do { dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
            FormatCore (pprCoreBindings occ_anald_binds $$ pprRules rules );

       ; return (reverse binds', rules') }
  where
    occ_anald_binds  = occurAnalysePgm this_mod
                          (\_ -> True)  {- All unfoldings active -}
                          (\_ -> False) {- No rules active -}
                          rules binds

    (final_env, binds') = foldl' do_one (emptyEnv dflags, []) occ_anald_binds
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
  = SOE { soe_dflags :: DynFlags
        , soe_inl   :: IdEnv SimpleClo
             -- Deals with preInlineUnconditionally; things
             -- that occur exactly once and are inlined
             -- without having first been simplified

        , soe_subst :: Subst
             -- Deals with cloning; includes the InScopeSet
        }

instance Outputable SimpleOptEnv where
  ppr (SOE { soe_inl = inl, soe_subst = subst })
    = text "SOE {" <+> vcat [ text "soe_inl   =" <+> ppr inl
                            , text "soe_subst =" <+> ppr subst ]
                   <+> text "}"

emptyEnv :: DynFlags -> SimpleOptEnv
emptyEnv dflags
  = SOE { soe_dflags = dflags
        , soe_inl = emptyVarEnv
        , soe_subst = emptySubst }

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

    go (Var v)
       | Just clo <- lookupVarEnv (soe_inl env) v
       = simple_opt_clo env clo
       | otherwise
       = lookupIdSubst (text "simpleOptExpr") (soe_subst env) v

    go (App e1 e2)      = simple_app env e1 [(env,e2)]
    go (Type ty)        = Type     (substTy subst ty)
    go (Coercion co)    = Coercion (optCoercion (soe_dflags env) (getTCvSubst subst) co)
    go (Lit lit)        = Lit lit
    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
    go (Cast e co)      | isReflCo co' = go e
                        | otherwise    = Cast (go e) co'
                        where
                          co' = optCoercion (soe_dflags env) (getTCvSubst subst) co

    go (Let bind body)  = case simple_opt_bind env bind NotTopLevel of
                             (env', Nothing)   -> simple_opt_expr env' body
                             (env', Just bind) -> Let bind (simple_opt_expr env' body)

    go lam@(Lam {})     = go_lam env [] lam
    go (Case e b ty as)
       -- See Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , Just (_, [], con, _tys, es) <- exprIsConApp_maybe in_scope_env e'
        -- We don't need to be concerned about floats when looking for coerce.
      , Just (altcon, bs, rhs) <- findAlt (DataAlt con) as
      = case altcon of
          DEFAULT -> go rhs
          _       -> foldr wrapLet (simple_opt_expr env' rhs) mb_prs
            where
              (env', mb_prs) = mapAccumL (simple_out_bind NotTopLevel) env $
                               zipEqual "simpleOptExpr" bs es

         -- Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , [(DEFAULT, _, rhs)] <- as
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
    go_alt env (con, bndrs, rhs)
      = (con, bndrs', simple_opt_expr env' rhs)
      where
        (env', bndrs') = subst_opt_bndrs env bndrs

    ----------------------
    -- go_lam tries eta reduction
    go_lam env bs' (Lam b e)
       = go_lam env' (b':bs') e
       where
         (env', b') = subst_opt_bndr env b
    go_lam env bs' e
       | Just etad_e <- tryEtaReduce bs e' = etad_e
       | otherwise                         = mkLams bs e'
       where
         bs = reverse bs'
         e' = simple_opt_expr env e

----------------------
-- simple_app collects arguments for beta reduction
simple_app :: SimpleOptEnv -> InExpr -> [SimpleClo] -> CoreExpr

simple_app env (Var v) as
  | Just (env', e) <- lookupVarEnv (soe_inl env) v
  = simple_app (soeSetInScope env env') e as

  | let unf = idUnfolding v
  , isCompulsoryUnfolding (idUnfolding v)
  , isAlwaysActive (idInlineActivation v)
    -- See Note [Unfold compulsory unfoldings in LHSs]
  = simple_app (soeZapSubst env) (unfoldingTemplate unf) as

  | otherwise
  , let out_fn = lookupIdSubst (text "simple_app") (soe_subst env) v
  = finish_app env out_fn as

simple_app env (App e1 e2) as
  = simple_app env e1 ((env, e2) : as)

simple_app env (Lam b e) (a:as)
  = wrapLet mb_pr (simple_app env' e as)
  where
     (env', mb_pr) = simple_bind_pair env b Nothing a NotTopLevel

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
  = ASSERT( isTyVar in_bndr )
    (env { soe_subst = extendTvSubst subst in_bndr out_ty }, Nothing)

  | Coercion co <- in_rhs
  , let out_co = optCoercion (soe_dflags env) (getTCvSubst (soe_subst rhs_env)) co
  = ASSERT( isCoVar in_bndr )
    (env { soe_subst = extendCvSubst subst in_bndr out_co }, Nothing)

  | ASSERT2( isNonCoVarId in_bndr, ppr in_bndr )
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
                         , occ_one_br = InOneBranch } = True
    safe_to_inline OneOcc{}                           = False
    safe_to_inline ManyOccs{}                         = False

-------------------
simple_out_bind :: TopLevelFlag
                -> SimpleOptEnv
                -> (InVar, OutExpr)
                -> (SimpleOptEnv, Maybe (OutVar, OutExpr))
simple_out_bind top_level env@(SOE { soe_subst = subst }) (in_bndr, out_rhs)
  | Type out_ty <- out_rhs
  = ASSERT( isTyVar in_bndr )
    (env { soe_subst = extendTvSubst subst in_bndr out_ty }, Nothing)

  | Coercion out_co <- out_rhs
  = ASSERT( isCoVar in_bndr )
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
  | ASSERT2( isNonCoVarId in_bndr, ppr in_bndr )
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
       | not active            = False --     in GHC.Core.Op.Simplify.Utils
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
In GHC.Core.Op.Simplify.Utils, Note [Top level and postInlineUnconditionally], we
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
    id2    = setIdType id1 (substTy subst (idType old_id))
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
   dflags   = soe_dflags env
   old_info = idInfo old_bndr

   -- Add back in the rules and unfolding which were
   -- removed by zapFragileIdInfo in subst_opt_id_bndr.
   --
   -- See Note [The Let-Unfoldings Invariant]
   new_info = idInfo new_bndr `setRuleInfo`      new_rules
                              `setUnfoldingInfo` new_unfolding

   old_rules = ruleInfo old_info
   new_rules = substSpec subst new_bndr old_rules

   old_unfolding = unfoldingInfo old_info
   new_unfolding | isStableUnfolding old_unfolding
                 = substUnfolding subst old_unfolding
                 | otherwise
                 = unfolding_from_rhs

   unfolding_from_rhs = mkUnfolding dflags InlineRhs
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
  , let str_sig   = idStrictness bndr
        str_arity = count isId bndrs  -- Strictness demands are for Ids only
        join_bndr = bndr `asJoinId`        join_arity
                         `setIdStrictness` etaConvertStrictSig str_arity str_sig
  = Just (join_bndr, mkLams bndrs body)

  | otherwise
  = Nothing

joinPointBindings_maybe :: [(InBndr, InExpr)] -> Maybe [(InBndr, InExpr)]
joinPointBindings_maybe bndrs
  = mapM (uncurry joinPointBinding_maybe) bndrs


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

We need to be careful about UTF8 strings here. ""# contains a ByteString, so
we must parse it back into a FastString to split off the first character.
That way we can treat unpackCString# and unpackCStringUtf8# in the same way.

We must also be careful about
   lvl = "foo"#
   ...(unpackCString# lvl)...
to ensure that we see through the let-binding for 'lvl'.  Hence the
(exprIsLiteral_maybe .. arg) in the guard before the call to
dealWithStringLiteral.

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
and then float the the let.  (Substitution would risk duplicating 'blah'.)

But if the case-of-known-constructor doesn't actually fire (i.e.
exprIsConApp_maybe does not return Just) then nothing happens, and nothing
will happen the next time either.

See test T16254, which checks the behavior of newtypes.
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
-- in GHC.Types.Id.Make.
--
-- We also return the incoming InScopeSet, augmented with
-- the binders from any [FloatBind] that we return
exprIsConApp_maybe :: InScopeEnv -> CoreExpr
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
       = let rhs'            = subst_expr subst rhs
             (subst', bndr') = subst_bndr subst bndr
             float           = FloatLet (NonRec bndr' rhs')
         in go subst' (float:floats) expr cont

    go subst floats (Case scrut b _ [(con, vars, expr)]) cont
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
            (lookupIdSubst (text "exprIsConApp" <+> ppr expr) sub v)
            cont

    go (Left in_scope) floats (Var fun) cont@(CC args co)

        | Just con <- isDataConWorkId_maybe fun
        , count isValArg args == idArity fun
        = succeedWith in_scope floats $
          pushCoDataCon con args co

        -- Look through data constructor wrappers: they inline late (See Note
        -- [Activation for data constructor wrappers]) but we want to do
        -- case-of-known-constructor optimisation eagerly.
        | isDataConWrapId fun
        , let rhs = uf_tmpl (realIdUnfolding fun)
        = go (Left in_scope) floats rhs cont

        -- Look through dictionary functions; see Note [Unfolding DFuns]
        | DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = dfun_args } <- unfolding
        , bndrs `equalLength` args    -- See Note [DFun arity check]
        , let subst = mkOpenSubst in_scope (bndrs `zip` args)
        = succeedWith in_scope floats $
          pushCoDataCon con (map (substExpr (text "exprIsConApp1") subst) dfun_args) co

        -- Look through unfoldings, but only arity-zero one;
        -- if arity > 0 we are effectively inlining a function call,
        -- and that is the business of callSiteInline.
        -- In practice, without this test, most of the "hits" were
        -- CPR'd workers getting inlined back into their wrappers,
        | idArity fun == 0
        , Just rhs <- expandUnfolding_maybe unfolding
        , let in_scope' = extendInScopeSetSet in_scope (exprFreeVars rhs)
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
    subst_expr (Right s) e = substExpr (text "exprIsConApp2") s e

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
dealWithStringLiteral _   str co
  | BS.null str
  = pushCoDataCon nilDataCon [Type charTy] co

dealWithStringLiteral fun str co
  = let strFS = mkFastStringByteString str

        char = mkConApp charDataCon [mkCharLit (headFS strFS)]
        charTail = bytesFS (tailFS strFS)

        -- In singleton strings, just add [] instead of unpackCstring# ""#.
        rest = if BS.null charTail
                 then mkConApp nilDataCon [Type charTy]
                 else App (Var fun)
                          (Lit (LitString charTail))

    in pushCoDataCon consDataCon [Type charTy, char, rest] co

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
-- Integer and string literals, which are vigorously hoisted to top level
-- and not subsequently inlined
exprIsLiteral_maybe env@(_, id_unf) e
  = case e of
      Lit l     -> Just l
      Tick _ e' -> exprIsLiteral_maybe env e' -- dubious?
      Var v     | Just rhs <- expandUnfolding_maybe (id_unf v)
                -> exprIsLiteral_maybe env rhs
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

exprIsLambda_maybe :: InScopeEnv -> CoreExpr
                      -> Maybe (Var, CoreExpr,[Tickish Id])
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
    , ASSERT( not $ x `elemVarSet` tyCoVarsOfCo co) True
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
    , let e' = simpleOptExprWith unsafeGlobalDynFlags (mkEmptySubst in_scope_set) (rhs `mkApps` as)
    -- Recurse, because of possible casts
    , Just (x', e'', ts') <- exprIsLambda_maybe (in_scope_set, id_unf) e'
    , let res = Just (x', e'', ts++ts')
    = -- pprTrace "exprIsLambda_maybe:Unfold" (vcat [ppr e, ppr (x',e'')])
      res

exprIsLambda_maybe _ _e
    = -- pprTrace "exprIsLambda_maybe:Fail" (vcat [ppr _e])
      Nothing


{- *********************************************************************
*                                                                      *
              The "push rules"
*                                                                      *
************************************************************************

Here we implement the "push rules" from FC papers:

* The push-argument rules, where we can move a coercion past an argument.
  We have
      (fun |> co) arg
  and we want to transform it to
    (fun arg') |> co'
  for some suitable co' and transformed arg'.

* The PushK rule for data constructors.  We have
       (K e1 .. en) |> co
  and we want to transform to
       (K e1' .. en')
  by pushing the coercion into the arguments
-}

pushCoArgs :: CoercionR -> [CoreArg] -> Maybe ([CoreArg], MCoercion)
pushCoArgs co []         = return ([], MCo co)
pushCoArgs co (arg:args) = do { (arg',  m_co1) <- pushCoArg  co  arg
                              ; case m_co1 of
                                  MCo co1 -> do { (args', m_co2) <- pushCoArgs co1 args
                                                 ; return (arg':args', m_co2) }
                                  MRefl  -> return (arg':args, MRefl) }

pushCoArg :: CoercionR -> CoreArg -> Maybe (CoreArg, MCoercion)
-- We have (fun |> co) arg, and we want to transform it to
--         (fun arg) |> co
-- This may fail, e.g. if (fun :: N) where N is a newtype
-- C.f. simplCast in GHC.Core.Op.Simplify
-- 'co' is always Representational
-- If the returned coercion is Nothing, then it would have been reflexive
pushCoArg co (Type ty) = do { (ty', m_co') <- pushCoTyArg co ty
                            ; return (Type ty', m_co') }
pushCoArg co val_arg   = do { (arg_co, m_co') <- pushCoValArg co
                            ; return (val_arg `mkCast` arg_co, m_co') }

pushCoTyArg :: CoercionR -> Type -> Maybe (Type, MCoercionR)
-- We have (fun |> co) @ty
-- Push the coercion through to return
--         (fun @ty') |> co'
-- 'co' is always Representational
-- If the returned coercion is Nothing, then it would have been reflexive;
-- it's faster not to compute it, though.
pushCoTyArg co ty
  -- The following is inefficient - don't do `eqType` here, the coercion
  -- optimizer will take care of it. See #14737.
  -- -- | tyL `eqType` tyR
  -- -- = Just (ty, Nothing)

  | isReflCo co
  = Just (ty, MRefl)

  | isForAllTy_ty tyL
  = ASSERT2( isForAllTy_ty tyR, ppr co $$ ppr ty )
    Just (ty `mkCastTy` co1, MCo co2)

  | otherwise
  = Nothing
  where
    tyL = coercionLKind co
    tyR = coercionRKind co
       -- co :: tyL ~ tyR
       -- tyL = forall (a1 :: k1). ty1
       -- tyR = forall (a2 :: k2). ty2

    co1 = mkSymCo (mkNthCo Nominal 0 co)
       -- co1 :: k2 ~N k1
       -- Note that NthCo can extract a Nominal equality between the
       -- kinds of the types related by a coercion between forall-types.
       -- See the NthCo case in GHC.Core.Lint.

    co2 = mkInstCo co (mkGReflLeftCo Nominal ty co1)
        -- co2 :: ty1[ (ty|>co1)/a1 ] ~ ty2[ ty/a2 ]
        -- Arg of mkInstCo is always nominal, hence mkNomReflCo

pushCoValArg :: CoercionR -> Maybe (Coercion, MCoercion)
-- We have (fun |> co) arg
-- Push the coercion through to return
--         (fun (arg |> co_arg)) |> co_res
-- 'co' is always Representational
-- If the second returned Coercion is actually Nothing, then no cast is necessary;
-- the returned coercion would have been reflexive.
pushCoValArg co
  -- The following is inefficient - don't do `eqType` here, the coercion
  -- optimizer will take care of it. See #14737.
  -- -- | tyL `eqType` tyR
  -- -- = Just (mkRepReflCo arg, Nothing)

  | isReflCo co
  = Just (mkRepReflCo arg, MRefl)

  | isFunTy tyL
  , (co1, co2) <- decomposeFunCo Representational co
              -- If   co  :: (tyL1 -> tyL2) ~ (tyR1 -> tyR2)
              -- then co1 :: tyL1 ~ tyR1
              --      co2 :: tyL2 ~ tyR2
  = ASSERT2( isFunTy tyR, ppr co $$ ppr arg )
    Just (mkSymCo co1, MCo co2)

  | otherwise
  = Nothing
  where
    arg = funArgTy tyR
    Pair tyL tyR = coercionKind co

pushCoercionIntoLambda
    :: InScopeSet -> Var -> CoreExpr -> CoercionR -> Maybe (Var, CoreExpr)
-- This implements the Push rule from the paper on coercions
--    (\x. e) |> co
-- ===>
--    (\x'. e |> co')
pushCoercionIntoLambda in_scope x e co
    | ASSERT(not (isTyVar x) && not (isCoVar x)) True
    , Pair s1s2 t1t2 <- coercionKind co
    , Just (_s1,_s2) <- splitFunTy_maybe s1s2
    , Just (t1,_t2) <- splitFunTy_maybe t1t2
    = let (co1, co2) = decomposeFunCo Representational co
          -- Should we optimize the coercions here?
          -- Otherwise they might not match too well
          x' = x `setIdType` t1
          in_scope' = in_scope `extendInScopeSet` x'
          subst = extendIdSubst (mkEmptySubst in_scope')
                                x
                                (mkCast (Var x') co1)
      in Just (x', substExpr (text "pushCoercionIntoLambda") subst e `mkCast` co2)
    | otherwise
    = pprTrace "exprIsLambda_maybe: Unexpected lambda in case" (ppr (Lam x e))
      Nothing

pushCoDataCon :: DataCon -> [CoreExpr] -> Coercion
              -> Maybe (DataCon
                       , [Type]      -- Universal type args
                       , [CoreExpr]) -- All other args incl existentials
-- Implement the KPush reduction rule as described in "Down with kinds"
-- The transformation applies iff we have
--      (C e1 ... en) `cast` co
-- where co :: (T t1 .. tn) ~ to_ty
-- The left-hand one must be a T, because exprIsConApp returned True
-- but the right-hand one might not be.  (Though it usually will.)
pushCoDataCon dc dc_args co
  | isReflCo co || from_ty `eqType` to_ty  -- try cheap test first
  , let (univ_ty_args, rest_args) = splitAtList (dataConUnivTyVars dc) dc_args
  = Just (dc, map exprToType univ_ty_args, rest_args)

  | Just (to_tc, to_tc_arg_tys) <- splitTyConApp_maybe to_ty
  , to_tc == dataConTyCon dc
        -- These two tests can fail; we might see
        --      (C x y) `cast` (g :: T a ~ S [a]),
        -- where S is a type function.  In fact, exprIsConApp
        -- will probably not be called in such circumstances,
        -- but there's nothing wrong with it

  = let
        tc_arity       = tyConArity to_tc
        dc_univ_tyvars = dataConUnivTyVars dc
        dc_ex_tcvars   = dataConExTyCoVars dc
        arg_tys        = dataConRepArgTys dc

        non_univ_args  = dropList dc_univ_tyvars dc_args
        (ex_args, val_args) = splitAtList dc_ex_tcvars non_univ_args

        -- Make the "Psi" from the paper
        omegas = decomposeCo tc_arity co (tyConRolesRepresentational to_tc)
        (psi_subst, to_ex_arg_tys)
          = liftCoSubstWithEx Representational
                              dc_univ_tyvars
                              omegas
                              dc_ex_tcvars
                              (map exprToType ex_args)

          -- Cast the value arguments (which include dictionaries)
        new_val_args = zipWith cast_arg arg_tys val_args
        cast_arg arg_ty arg = mkCast arg (psi_subst arg_ty)

        to_ex_args = map Type to_ex_arg_tys

        dump_doc = vcat [ppr dc,      ppr dc_univ_tyvars, ppr dc_ex_tcvars,
                         ppr arg_tys, ppr dc_args,
                         ppr ex_args, ppr val_args, ppr co, ppr from_ty, ppr to_ty, ppr to_tc ]
    in
    ASSERT2( eqType from_ty (mkTyConApp to_tc (map exprToType $ takeList dc_univ_tyvars dc_args)), dump_doc )
    ASSERT2( equalLength val_args arg_tys, dump_doc )
    Just (dc, to_tc_arg_tys, to_ex_args ++ new_val_args)

  | otherwise
  = Nothing

  where
    Pair from_ty to_ty = coercionKind co

collectBindersPushingCo :: CoreExpr -> ([Var], CoreExpr)
-- Collect lambda binders, pushing coercions inside if possible
-- E.g.   (\x.e) |> g         g :: <Int> -> blah
--        = (\x. e |> Nth 1 g)
--
-- That is,
--
-- collectBindersPushingCo ((\x.e) |> g) === ([x], e |> Nth 1 g)
collectBindersPushingCo e
  = go [] e
  where
    -- Peel off lambdas until we hit a cast.
    go :: [Var] -> CoreExpr -> ([Var], CoreExpr)
    -- The accumulator is in reverse order
    go bs (Lam b e)   = go (b:bs) e
    go bs (Cast e co) = go_c bs e co
    go bs e           = (reverse bs, e)

    -- We are in a cast; peel off casts until we hit a lambda.
    go_c :: [Var] -> CoreExpr -> CoercionR -> ([Var], CoreExpr)
    -- (go_c bs e c) is same as (go bs e (e |> c))
    go_c bs (Cast e co1) co2 = go_c bs e (co1 `mkTransCo` co2)
    go_c bs (Lam b e)    co  = go_lam bs b e co
    go_c bs e            co  = (reverse bs, mkCast e co)

    -- We are in a lambda under a cast; peel off lambdas and build a
    -- new coercion for the body.
    go_lam :: [Var] -> Var -> CoreExpr -> CoercionR -> ([Var], CoreExpr)
    -- (go_lam bs b e c) is same as (go_c bs (\b.e) c)
    go_lam bs b e co
      | isTyVar b
      , let Pair tyL tyR = coercionKind co
      , ASSERT( isForAllTy_ty tyL )
        isForAllTy_ty tyR
      , isReflCo (mkNthCo Nominal 0 co)  -- See Note [collectBindersPushingCo]
      = go_c (b:bs) e (mkInstCo co (mkNomReflCo (mkTyVarTy b)))

      | isCoVar b
      , let Pair tyL tyR = coercionKind co
      , ASSERT( isForAllTy_co tyL )
        isForAllTy_co tyR
      , isReflCo (mkNthCo Nominal 0 co)  -- See Note [collectBindersPushingCo]
      , let cov = mkCoVarCo b
      = go_c (b:bs) e (mkInstCo co (mkNomReflCo (mkCoercionTy cov)))

      | isId b
      , let Pair tyL tyR = coercionKind co
      , ASSERT( isFunTy tyL) isFunTy tyR
      , (co_arg, co_res) <- decomposeFunCo Representational co
      , isReflCo co_arg  -- See Note [collectBindersPushingCo]
      = go_c (b:bs) e co_res

      | otherwise = (reverse bs, mkCast (Lam b e) co)

{-

Note [collectBindersPushingCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We just look for coercions of form
   <type> -> blah
(and similarly for foralls) to keep this function simple.  We could do
more elaborate stuff, but it'd involve substitution etc.

-}
