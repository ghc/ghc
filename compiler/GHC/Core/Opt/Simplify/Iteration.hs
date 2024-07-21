{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[Simplify]{The main module of the simplifier}
-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GHC.Core.Opt.Simplify.Iteration ( simplTopBinds, simplExpr, simplImpRules ) where

import GHC.Prelude

import GHC.Platform

import GHC.Driver.Flags

import GHC.Core
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.ConstantFold
import GHC.Core.Type hiding ( substTy, substCo, substTyVar, extendTvSubst, extendCvSubst )
import GHC.Core.TyCo.Compare( eqType )
import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Inline
import GHC.Core.Opt.Simplify.Utils
import GHC.Core.Opt.OccurAnal ( occurAnalyseExpr, zapLambdaBndrs, scrutBinderSwap_maybe )
import GHC.Core.Make       ( FloatBind, mkImpossibleExpr, castBottomExpr )
import qualified GHC.Core.Make
import GHC.Core.Coercion hiding ( substCo, substCoVar )
import GHC.Core.Reduction
import GHC.Core.Coercion.Opt    ( optCoercion )
import GHC.Core.FamInstEnv      ( FamInstEnv, topNormaliseType_maybe )
import GHC.Core.DataCon
   ( DataCon, dataConWorkId, dataConRepStrictness
   , dataConRepArgTys, isUnboxedTupleDataCon
   , StrictnessMark (..) )
import GHC.Core.Opt.Stats ( Tick(..) )
import GHC.Core.Ppr     ( pprCoreExpr )
import GHC.Core.Unfold
import GHC.Core.Unfold.Make
import GHC.Core.Utils
import GHC.Core.Opt.Arity ( ArityType, exprArity, arityTypeBotSigs_maybe
                          , pushCoTyArg, pushCoValArg, exprIsDeadEnd
                          , typeArity, arityTypeArity, etaExpandAT )
import GHC.Core.SimpleOpt ( exprIsConApp_maybe, joinPointBinding_maybe, joinPointBindings_maybe )
import GHC.Core.FVs     ( mkRuleInfo )
import GHC.Core.Rules   ( lookupRule, getRules )
import GHC.Core.Multiplicity

import GHC.Types.Literal   ( litIsLifted ) --, mkLitInt ) -- temporarily commented out. See #8326
import GHC.Types.SourceText
import GHC.Types.Id
import GHC.Types.Id.Make   ( seqId )
import GHC.Types.Id.Info
import GHC.Types.Name   ( mkSystemVarName, isExternalName, getOccFS )
import GHC.Types.Demand
import GHC.Types.Unique ( hasKey )
import GHC.Types.Basic
import GHC.Types.Tickish
import GHC.Types.Var    ( isTyCoVar )
import GHC.Builtin.PrimOps ( PrimOp (SeqOp) )
import GHC.Builtin.Types.Prim( realWorldStatePrimTy )
import GHC.Builtin.Names( runRWKey )

import GHC.Data.Maybe   ( isNothing, orElse, mapMaybe )
import GHC.Data.FastString
import GHC.Unit.Module ( moduleName )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Monad  ( mapAccumLM, liftIO )
import GHC.Utils.Logger
import GHC.Utils.Misc

import Control.Monad

{-
The guts of the simplifier is in this module, but the driver loop for
the simplifier is in GHC.Core.Opt.Pipeline

Note [The big picture]
~~~~~~~~~~~~~~~~~~~~~~
The general shape of the simplifier is this:

  simplExpr :: SimplEnv -> InExpr -> SimplCont -> SimplM (SimplFloats, OutExpr)
  simplBind :: SimplEnv -> InBind -> SimplM (SimplFloats, SimplEnv)

 * SimplEnv contains
     - Simplifier mode
     - Ambient substitution
     - InScopeSet

 * SimplFloats contains
     - Let-floats (which includes ok-for-spec case-floats)
     - Join floats
     - InScopeSet (including all the floats)

 * Expressions
      simplExpr :: SimplEnv -> InExpr -> SimplCont
                -> SimplM (SimplFloats, OutExpr)
   The result of simplifying an /expression/ is (floats, expr)
      - A bunch of floats (let bindings, join bindings)
      - A simplified expression.
   The overall result is effectively (let floats in expr)

 * Bindings
      simplBind :: SimplEnv -> InBind -> SimplM (SimplFloats, SimplEnv)
   The result of simplifying a binding is
     - A bunch of floats, the last of which is the simplified binding
       There may be auxiliary bindings too; see prepareRhs
     - An environment suitable for simplifying the scope of the binding

   The floats may also be empty, if the binding is inlined unconditionally;
   in that case the returned SimplEnv will have an augmented substitution.

   The returned floats and env both have an in-scope set, and they are
   guaranteed to be the same.

Eta expansion
~~~~~~~~~~~~~~
For eta expansion, we want to catch things like

        case e of (a,b) -> \x -> case a of (p,q) -> \y -> r

If the \x was on the RHS of a let, we'd eta expand to bring the two
lambdas together.  And in general that's a good thing to do.  Perhaps
we should eta expand wherever we find a (value) lambda?  Then the eta
expansion at a let RHS can concentrate solely on the PAP case.

Note [In-scope set as a substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As per Note [Lookups in in-scope set], an in-scope set can act as
a substitution. Specifically, it acts as a substitution from variable to
variables /with the same unique/.

Why do we need this? Well, during the course of the simplifier, we may want to
adjust inessential properties of a variable. For instance, when performing a
beta-reduction, we change

    (\x. e) u ==> let x = u in e

We typically want to add an unfolding to `x` so that it inlines to (the
simplification of) `u`.

We do that by adding the unfolding to the binder `x`, which is added to the
in-scope set. When simplifying occurrences of `x` (every occurrence!), they are
replaced by their “updated” version from the in-scope set, hence inherit the
unfolding. This happens in `SimplEnv.substId`.

Another example. Consider

   case x of y { Node a b -> ...y...
               ; Leaf v   -> ...y... }

In the Node branch want y's unfolding to be (Node a b); in the Leaf branch we
want y's unfolding to be (Leaf v). We achieve this by adding the appropriate
unfolding to y, and re-adding it to the in-scope set. See the calls to
`addBinderUnfolding` in `Simplify.addAltUnfoldings` and elsewhere.

It's quite convenient. This way we don't need to manipulate the substitution all
the time: every update to a binder is automatically reflected to its bound
occurrences.

Note [Bangs in the Simplifier]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both SimplFloats and SimplEnv do *not* generally benefit from making
their fields strict. I don't know if this is because of good use of
laziness or unintended side effects like closures capturing more variables
after WW has run.

But the end result is that we keep these lazy, but force them in some places
where we know it's beneficial to the compiler.

Similarly environments returned from functions aren't *always* beneficial to
force. In some places they would never be demanded so forcing them early
increases allocation. In other places they almost always get demanded so
it's worthwhile to force them early.

Would it be better to through every allocation of e.g. SimplEnv and decide
wether or not to make this one strict? Absolutely! Would be a good use of
someones time? Absolutely not! I made these strict that showed up during
a profiled build or which I noticed while looking at core for one reason
or another.

The result sadly is that we end up with "random" bangs in the simplifier
where we sometimes force e.g. the returned environment from a function and
sometimes we don't for the same function. Depending on the context around
the call. The treatment is also not very consistent. I only added bangs
where I saw it making a difference either in the core or benchmarks. Some
patterns where it would be beneficial aren't convered as a consequence as
I neither have the time to go through all of the core and some cases are
too small to show up in benchmarks.



************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************
-}

simplTopBinds :: SimplEnv -> [InBind] -> SimplM (SimplFloats, SimplEnv)
-- See Note [The big picture]
simplTopBinds env0 binds0
  = do  {       -- Put all the top-level binders into scope at the start
                -- so that if a rewrite rule has unexpectedly brought
                -- anything into scope, then we don't get a complaint about that.
                -- It's rather as if the top-level binders were imported.
                -- See Note [Glomming] in "GHC.Core.Opt.OccurAnal".
        -- See Note [Bangs in the Simplifier]
        ; !env1 <- {-#SCC "simplTopBinds-simplRecBndrs" #-} simplRecBndrs env0 (bindersOfBinds binds0)
        ; (floats, env2) <- {-#SCC "simplTopBinds-simpl_binds" #-} simpl_binds env1 binds0
        ; freeTick SimplifierDone
        ; return (floats, env2) }
  where
        -- We need to track the zapped top-level binders, because
        -- they should have their fragile IdInfo zapped (notably occurrence info)
        -- That's why we run down binds and bndrs' simultaneously.
        --
    simpl_binds :: SimplEnv -> [InBind] -> SimplM (SimplFloats, SimplEnv)
    simpl_binds env []           = return (emptyFloats env, env)
    simpl_binds env (bind:binds) = do { (float,  env1) <- simpl_bind env bind
                                      ; (floats, env2) <- simpl_binds env1 binds
                                      -- See Note [Bangs in the Simplifier]
                                      ; let !floats1 = float `addFloats` floats
                                      ; return (floats1, env2) }

    simpl_bind env (Rec pairs)
      = simplRecBind env (BC_Let TopLevel Recursive) pairs
    simpl_bind env (NonRec b r)
      = do { let bind_cxt = BC_Let TopLevel NonRecursive
           ; (env', b') <- addBndrRules env b (lookupRecBndr env b) bind_cxt
           ; simplRecOrTopPair env' bind_cxt b b' r }

{-
************************************************************************
*                                                                      *
        Lazy bindings
*                                                                      *
************************************************************************

simplRecBind is used for
        * recursive bindings only
-}

simplRecBind :: SimplEnv -> BindContext
             -> [(InId, InExpr)]
             -> SimplM (SimplFloats, SimplEnv)
simplRecBind env0 bind_cxt pairs0
  = do  { (env1, triples) <- mapAccumLM add_rules env0 pairs0
        ; let new_bndrs = map sndOf3 triples
        ; (rec_floats, env2) <- enterRecGroupRHSs env1 new_bndrs $ \env ->
                                go env triples
        ; return (mkRecFloats rec_floats, env2) }
  where
    add_rules :: SimplEnv -> (InBndr,InExpr) -> SimplM (SimplEnv, (InBndr, OutBndr, InExpr))
        -- Add the (substituted) rules to the binder
    add_rules env (bndr, rhs)
        = do { (env', bndr') <- addBndrRules env bndr (lookupRecBndr env bndr) bind_cxt
             ; return (env', (bndr, bndr', rhs)) }

    go env [] = return (emptyFloats env, env)

    go env ((old_bndr, new_bndr, rhs) : pairs)
        = do { (float, env1) <- simplRecOrTopPair env bind_cxt
                                                  old_bndr new_bndr rhs
             ; (floats, env2) <- go env1 pairs
             ; return (float `addFloats` floats, env2) }

{-
simplOrTopPair is used for
        * recursive bindings (whether top level or not)
        * top-level non-recursive bindings

It assumes the binder has already been simplified, but not its IdInfo.
-}

simplRecOrTopPair :: SimplEnv
                  -> BindContext
                  -> InId -> OutBndr -> InExpr  -- Binder and rhs
                  -> SimplM (SimplFloats, SimplEnv)

simplRecOrTopPair env bind_cxt old_bndr new_bndr rhs
  | Just env' <- preInlineUnconditionally env (bindContextLevel bind_cxt)
                                          old_bndr rhs env
  = {-#SCC "simplRecOrTopPair-pre-inline-uncond" #-}
    simplTrace "SimplBindr:inline-uncond" (ppr old_bndr) $
    do { tick (PreInlineUnconditionally old_bndr)
       ; return ( emptyFloats env, env' ) }

  | otherwise
  = case bind_cxt of
      BC_Join is_rec cont -> simplTrace "SimplBind:join" (ppr old_bndr) $
                             simplJoinBind is_rec cont
                                           (old_bndr,env) (new_bndr,env) (rhs,env)

      BC_Let top_lvl is_rec -> simplTrace "SimplBind:normal" (ppr old_bndr) $
                               simplLazyBind top_lvl is_rec
                                             (old_bndr,env) (new_bndr,env) (rhs,env)

simplTrace :: String -> SDoc -> SimplM a -> SimplM a
simplTrace herald doc thing_inside = do
  logger <- getLogger
  if logHasDumpFlag logger Opt_D_verbose_core2core
    then logTraceMsg logger herald doc thing_inside
    else thing_inside

--------------------------
simplLazyBind :: TopLevelFlag -> RecFlag
              -> (InId, SimplEnv)       -- InBinder, and static env for its unfolding (if any)
              -> (OutId, SimplEnv)      -- OutBinder, and SimplEnv after simplifying that binder
                                        -- The OutId has IdInfo (notably RULES),
                                        -- except arity, unfolding
              -> (InExpr, SimplEnv)     -- The RHS and its static environment
              -> SimplM (SimplFloats, SimplEnv)
-- Precondition: Ids only, no TyVars; not a JoinId
-- Precondition: rhs obeys the let-can-float invariant
simplLazyBind top_lvl is_rec (bndr,unf_se) (bndr1,env) (rhs,rhs_se)
  = assert (isId bndr )
    assertPpr (not (isJoinId bndr)) (ppr bndr) $
    -- pprTrace "simplLazyBind" ((ppr bndr <+> ppr bndr1) $$ ppr rhs $$ ppr (seIdSubst rhs_se)) $
    do  { let   !rhs_env     = rhs_se `setInScopeFromE` env -- See Note [Bangs in the Simplifier]
                (tvs, body) = case collectTyAndValBinders rhs of
                                (tvs, [], body)
                                  | surely_not_lam body -> (tvs, body)
                                _                       -> ([], rhs)

                surely_not_lam (Lam {})     = False
                surely_not_lam (Tick t e)
                  | not (tickishFloatable t) = surely_not_lam e
                   -- eta-reduction could float
                surely_not_lam _            = True
                        -- Do not do the "abstract tyvar" thing if there's
                        -- a lambda inside, because it defeats eta-reduction
                        --    f = /\a. \x. g a x
                        -- should eta-reduce.

        ; (body_env, tvs') <- {-#SCC "simplBinders" #-} simplBinders rhs_env tvs
                -- See Note [Floating and type abstraction] in GHC.Core.Opt.Simplify.Utils

        -- Simplify the RHS
        ; let rhs_cont = mkRhsStop (substTy body_env (exprType body))
                                   is_rec (idDemandInfo bndr)
        ; (body_floats0, body0) <- {-#SCC "simplExprF" #-} simplExprF body_env body rhs_cont

        -- ANF-ise a constructor or PAP rhs
        ; (body_floats2, body2) <- {-#SCC "prepareBinding" #-}
                                   prepareBinding env top_lvl is_rec
                                                  False  -- Not strict; this is simplLazyBind
                                                  bndr1 body_floats0 body0
          -- Subtle point: we do not need or want tvs' in the InScope set
          -- of body_floats2, so we pass in 'env' not 'body_env'.
          -- Don't want: if tvs' are in-scope in the scope of this let-binding, we may do
          -- more renaming than necessary => extra work (see !7777 and test T16577).
          -- Don't need: we wrap tvs' around the RHS anyway.

        ; (rhs_floats, body3)
            <-  if isEmptyFloats body_floats2 || null tvs then   -- Simple floating
                     {-#SCC "simplLazyBind-simple-floating" #-}
                     return (body_floats2, body2)

                else -- Non-empty floats, and non-empty tyvars: do type-abstraction first
                     {-#SCC "simplLazyBind-type-abstraction-first" #-}
                     do { (poly_binds, body3) <- abstractFloats (seUnfoldingOpts env) top_lvl
                                                                tvs' body_floats2 body2
                        ; let poly_floats = foldl' extendFloats (emptyFloats env) poly_binds
                        ; return (poly_floats, body3) }

        ; let env1 = env `setInScopeFromF` rhs_floats
        ; rhs' <- rebuildLam env1 tvs' body3 rhs_cont
        ; (bind_float, env2) <- completeBind (BC_Let top_lvl is_rec) (bndr,unf_se) (bndr1,rhs',env1)
        ; return (rhs_floats `addFloats` bind_float, env2) }

--------------------------
simplJoinBind :: RecFlag
              -> SimplCont
              -> (InId, SimplEnv)       -- InBinder, with static env for its unfolding
              -> (OutId, SimplEnv)      -- OutBinder; SimplEnv has the binder in scope
                                        -- The OutId has IdInfo, except arity, unfolding
              -> (InExpr, SimplEnv)     -- The right hand side and its env
              -> SimplM (SimplFloats, SimplEnv)
simplJoinBind is_rec cont (old_bndr, unf_se) (new_bndr, env) (rhs, rhs_se)
  = do  { let rhs_env = rhs_se `setInScopeFromE` env
        ; rhs' <- simplJoinRhs rhs_env old_bndr rhs cont
        ; completeBind (BC_Join is_rec cont) (old_bndr, unf_se) (new_bndr, rhs', env) }

--------------------------
simplAuxBind :: SimplEnv
             -> InId            -- Old binder; not a JoinId
             -> OutExpr         -- Simplified RHS
             -> SimplM (SimplFloats, SimplEnv)
-- A specialised variant of completeBindX used to construct non-recursive
-- auxiliary bindings, notably in knownCon.
--
-- The binder comes from a case expression (case binder or alternative)
-- and so does not have rules, unfolding, inline pragmas etc.
--
-- Precondition: rhs satisfies the let-can-float invariant

simplAuxBind env bndr new_rhs
  | assertPpr (isId bndr && not (isJoinId bndr)) (ppr bndr) $
    isDeadBinder bndr   -- Not uncommon; e.g. case (a,b) of c { (p,q) -> p }
  = return (emptyFloats env, env)    --  Here c is dead, and we avoid
                                     --  creating the binding c = (a,b)

  -- The cases would be inlined unconditionally by completeBind:
  -- but it seems not uncommon, and avoids faff to do it here
  -- This is safe because it's only used for auxiliary bindings, which
  -- have no NOLINE pragmas, nor RULEs
  | exprIsTrivial new_rhs  -- Short-cut for let x = y in ...
  = return ( emptyFloats env
           , case new_rhs of
                Coercion co -> extendCvSubst env bndr co
                _           -> extendIdSubst env bndr (DoneEx new_rhs NotJoinPoint) )

  | otherwise
  = do  { -- ANF-ise the RHS
          let !occ_fs = getOccFS bndr
        ; (anf_floats, rhs1) <- prepareRhs env NotTopLevel occ_fs new_rhs
        ; unless (isEmptyLetFloats anf_floats) (tick LetFloatFromLet)
        ; let rhs_floats = emptyFloats env `addLetFloats` anf_floats

          -- Simplify the binder and complete the binding
        ; (env1, new_bndr) <- simplBinder (env `setInScopeFromF` rhs_floats) bndr
        ; (bind_float, env2) <- completeBind (BC_Let NotTopLevel NonRecursive)
                                             (bndr,env) (new_bndr, rhs1, env1)

        ; return (rhs_floats `addFloats` bind_float, env2) }


{- *********************************************************************
*                                                                      *
           Cast worker/wrapper
*                                                                      *
************************************************************************

Note [Cast worker/wrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have a binding
   x = e |> co
we want to do something very similar to worker/wrapper:
   $wx = e
   x = $wx |> co

We call this making a cast worker/wrapper in tryCastWorkerWrapper.

The main motivaiton is that x can be inlined freely.  There's a chance
that e will be a constructor application or function, or something
like that, so moving the coercion to the usage site may well cancel
the coercions and lead to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          t = T m
          go 0 = 0
          go n = case t of { T m -> go (n-m) }
                -- This case should optimise

A second reason for doing cast worker/wrapper is that the worker/wrapper
pass after strictness analysis can't deal with RHSs like
     f = (\ a b c. blah) |> co
Instead, it relies on cast worker/wrapper to get rid of the cast,
leaving a simpler job for demand-analysis worker/wrapper.  See #19874.

Wrinkles

1. We must /not/ do cast w/w on
     f = g |> co
   otherwise it'll just keep repeating forever! You might think this
   is avoided because the call to tryCastWorkerWrapper is guarded by
   preInlineUnconditinally, but I'm worried that a loop-breaker or an
   exported Id might say False to preInlineUnonditionally.

2. We need to be careful with inline/noinline pragmas:
       rec { {-# NOINLINE f #-}
             f = (...g...) |> co
           ; g = ...f... }
   This is legitimate -- it tells GHC to use f as the loop breaker
   rather than g.  Now we do the cast thing, to get something like
       rec { $wf = ...g...
           ; f = $wf |> co
           ; g = ...f... }
   Where should the NOINLINE pragma go?  If we leave it on f we'll get
     rec { $wf = ...g...
         ; {-# NOINLINE f #-}
           f = $wf |> co
         ; g = ...f... }
   and that is bad: the whole point is that we want to inline that
   cast!  We want to transfer the pagma to $wf:
      rec { {-# NOINLINE $wf #-}
            $wf = ...g...
          ; f = $wf |> co
          ; g = ...f... }
   c.f. Note [Worker/wrapper for NOINLINE functions] in GHC.Core.Opt.WorkWrap.

3. We should still do cast w/w even if `f` is INLINEABLE.  E.g.
      {- f: Stable unfolding = <stable-big> -}
      f = (\xy. <big-body>) |> co
   Then we want to w/w to
      {- $wf: Stable unfolding = <stable-big> |> sym co -}
      $wf = \xy. <big-body>
      f = $wf |> co
   Notice that the stable unfolding moves to the worker!  Now demand analysis
   will work fine on $wf, whereas it has trouble with the original f.
   c.f. Note [Worker/wrapper for INLINABLE functions] in GHC.Core.Opt.WorkWrap.
   This point also applies to strong loopbreakers with INLINE pragmas, see
   wrinkle (4).

4. We should /not/ do cast w/w for non-loop-breaker INLINE functions (hence
   hasInlineUnfolding in tryCastWorkerWrapper, which responds False to
   loop-breakers) because they'll definitely be inlined anyway, cast and
   all. And if we do cast w/w for an INLINE function with arity zero, we get
   something really silly: we inline that "worker" right back into the wrapper!
   Worse than a no-op, because we have then lost the stable unfolding.

All these wrinkles are exactly like worker/wrapper for strictness analysis:
  f is the wrapper and must inline like crazy
  $wf is the worker and must carry f's original pragma
See Note [Worker/wrapper for INLINABLE functions]
and Note [Worker/wrapper for NOINLINE functions] in GHC.Core.Opt.WorkWrap.

See #17673, #18093, #18078, #19890.

Note [Preserve strictness in cast w/w]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the Note [Cast worker/wrapper] transformation, keep the strictness info.
Eg
        f = e `cast` co    -- f has strictness SSL
When we transform to
        f' = e             -- f' also has strictness SSL
        f = f' `cast` co   -- f still has strictness SSL

Its not wrong to drop it on the floor, but better to keep it.

Note [Preserve RuntimeRep info in cast w/w]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must not do cast w/w when the presence of the coercion is needed in order
to determine the runtime representation.

Example:

  Suppose we have a type family:

    type F :: RuntimeRep
    type family F where
      F = LiftedRep

  together with a type `ty :: TYPE F` and a top-level binding

    a :: ty |> TYPE F[0]

  The kind of `ty |> TYPE F[0]` is `LiftedRep`, so `a` is a top-level lazy binding.
  However, were we to apply cast w/w, we would get:

    b :: ty
    b = ...

    a :: ty |> TYPE F[0]
    a = b `cast` GRefl (TYPE F[0])

  Now we are in trouble because `ty :: TYPE F` does not have a known runtime
  representation, because we need to be able to reduce the nullary type family
  application `F` to find that out.

Conclusion: only do cast w/w when doing so would not lose the RuntimeRep
information. That is, when handling `Cast rhs co`, don't attempt cast w/w
unless the kind of the type of rhs is concrete, in the sense of
Note [Concrete types] in GHC.Tc.Utils.Concrete.
-}

tryCastWorkerWrapper :: SimplEnv -> BindContext
                     -> InId -> OccInfo
                     -> OutId -> OutExpr
                     -> SimplM (SimplFloats, SimplEnv)
-- See Note [Cast worker/wrapper]
tryCastWorkerWrapper env bind_cxt old_bndr occ_info bndr (Cast rhs co)
  | BC_Let top_lvl is_rec <- bind_cxt  -- Not join points
  , not (isDFunId bndr) -- nor DFuns; cast w/w is no help, and we can't transform
                        --            a DFunUnfolding in mk_worker_unfolding
  , not (exprIsTrivial rhs)        -- Not x = y |> co; Wrinkle 1
  , not (hasInlineUnfolding info)  -- Not INLINE things: Wrinkle 4
  , isConcreteType (typeKind work_ty) -- Don't peel off a cast if doing so would
                                      -- lose the underlying runtime representation.
                                      -- See Note [Preserve RuntimeRep info in cast w/w]
  , not (isOpaquePragma (idInlinePragma old_bndr)) -- Not for OPAQUE bindings
                                                   -- See Note [OPAQUE pragma]
  = do  { uniq <- getUniqueM
        ; let work_name = mkSystemVarName uniq occ_fs
              work_id   = mkLocalIdWithInfo work_name ManyTy work_ty work_info
              is_strict = isStrictId bndr

        ; (rhs_floats, work_rhs) <- prepareBinding env top_lvl is_rec is_strict
                                                   work_id (emptyFloats env) rhs

        ; work_unf <- mk_worker_unfolding top_lvl work_id work_rhs
        ; let  work_id_w_unf = work_id `setIdUnfolding` work_unf
               floats   = rhs_floats `addLetFloats`
                          unitLetFloat (NonRec work_id_w_unf work_rhs)

               triv_rhs = Cast (Var work_id_w_unf) co

        ; if postInlineUnconditionally env bind_cxt bndr occ_info triv_rhs
             -- Almost always True, because the RHS is trivial
             -- In that case we want to eliminate the binding fast
             -- We conservatively use postInlineUnconditionally so that we
             -- check all the right things
          then do { tick (PostInlineUnconditionally bndr)
                  ; return ( floats
                           , extendIdSubst (setInScopeFromF env floats) old_bndr $
                             DoneEx triv_rhs NotJoinPoint ) }

          else do { wrap_unf <- mkLetUnfolding uf_opts top_lvl VanillaSrc bndr triv_rhs
                  ; let bndr' = bndr `setInlinePragma` mkCastWrapperInlinePrag (idInlinePragma bndr)
                                `setIdUnfolding`  wrap_unf
                        floats' = floats `extendFloats` NonRec bndr' triv_rhs
                  ; return ( floats', setInScopeFromF env floats' ) } }
  where
    -- Force the occ_fs so that the old Id is not retained in the new Id.
    !occ_fs = getOccFS bndr
    uf_opts = seUnfoldingOpts env
    work_ty = coercionLKind co
    info   = idInfo bndr
    work_arity = arityInfo info `min` typeArity work_ty

    work_info = vanillaIdInfo `setDmdSigInfo`     dmdSigInfo info
                              `setCprSigInfo`     cprSigInfo info
                              `setDemandInfo`     demandInfo info
                              `setInlinePragInfo` inlinePragInfo info
                              `setArityInfo`      work_arity
           -- We do /not/ want to transfer OccInfo, Rules
           -- Note [Preserve strictness in cast w/w]
           -- and Wrinkle 2 of Note [Cast worker/wrapper]

    ----------- Worker unfolding -----------
    -- Stable case: if there is a stable unfolding we have to compose with (Sym co);
    --   the next round of simplification will do the job
    -- Non-stable case: use work_rhs
    -- Wrinkle 3 of Note [Cast worker/wrapper]
    mk_worker_unfolding top_lvl work_id work_rhs
      = case realUnfoldingInfo info of -- NB: the real one, even for loop-breakers
           unf@(CoreUnfolding { uf_tmpl = unf_rhs, uf_src = src })
             | isStableSource src -> return (unf { uf_tmpl = mkCast unf_rhs (mkSymCo co) })
           _ -> mkLetUnfolding uf_opts top_lvl VanillaSrc work_id work_rhs

tryCastWorkerWrapper env _ _ _ bndr rhs  -- All other bindings
  = do { traceSmpl "tcww:no" (vcat [ text "bndr:" <+> ppr bndr
                                   , text "rhs:" <+> ppr rhs ])
        ; return (mkFloatBind env (NonRec bndr rhs)) }

mkCastWrapperInlinePrag :: InlinePragma -> InlinePragma
-- See Note [Cast worker/wrapper]
mkCastWrapperInlinePrag (InlinePragma { inl_inline = fn_inl, inl_act = fn_act, inl_rule = rule_info })
  = InlinePragma { inl_src    = SourceText $ fsLit "{-# INLINE"
                 , inl_inline = fn_inl       -- See Note [Worker/wrapper for INLINABLE functions]
                 , inl_sat    = Nothing      --     in GHC.Core.Opt.WorkWrap
                 , inl_act    = wrap_act     -- See Note [Wrapper activation]
                 , inl_rule   = rule_info }  --     in GHC.Core.Opt.WorkWrap
                                -- RuleMatchInfo is (and must be) unaffected
  where
    -- See Note [Wrapper activation] in GHC.Core.Opt.WorkWrap
    -- But simpler, because we don't need to disable during InitialPhase
    wrap_act | isNeverActive fn_act = activateDuringFinal
             | otherwise            = fn_act


{- *********************************************************************
*                                                                      *
           prepareBinding, prepareRhs, makeTrivial
*                                                                      *
********************************************************************* -}

prepareBinding :: SimplEnv -> TopLevelFlag -> RecFlag -> Bool
               -> Id   -- Used only for its OccName; can be InId or OutId
               -> SimplFloats -> OutExpr
               -> SimplM (SimplFloats, OutExpr)
-- In (prepareBinding ... bndr floats rhs), the binding is really just
--    bndr = let floats in rhs
-- Maybe we can ANF-ise this binding and float out; e.g.
--    bndr = let a = f x in K a a (g x)
-- we could float out to give
--    a    = f x
--    tmp  = g x
--    bndr = K a a tmp
-- That's what prepareBinding does
-- Precondition: binder is not a JoinId
-- Postcondition: the returned SimplFloats contains only let-floats
prepareBinding env top_lvl is_rec strict_bind bndr rhs_floats rhs
  = do { -- Never float join-floats out of a non-join let-binding (which this is)
         -- So wrap the body in the join-floats right now
         -- Hence: rhs_floats1 consists only of let-floats
         let (rhs_floats1, rhs1) = wrapJoinFloatsX rhs_floats rhs

         -- rhs_env: add to in-scope set the binders from rhs_floats
         -- so that prepareRhs knows what is in scope in rhs
       ; let rhs_env = env `setInScopeFromF` rhs_floats1
             -- Force the occ_fs so that the old Id is not retained in the new Id.
             !occ_fs = getOccFS bndr

       -- Now ANF-ise the remaining rhs
       ; (anf_floats, rhs2) <- prepareRhs rhs_env top_lvl occ_fs rhs1

       -- Finally, decide whether or not to float
       ; let all_floats = rhs_floats1 `addLetFloats` anf_floats
       ; if doFloatFromRhs (seFloatEnable env) top_lvl is_rec strict_bind all_floats rhs2
         then -- Float!
              do { tick LetFloatFromLet
                 ; return (all_floats, rhs2) }

         else -- Abandon floating altogether; revert to original rhs
              -- Since we have already built rhs1, we just need to add
              -- rhs_floats1 to it
              return (emptyFloats env, wrapFloats rhs_floats1 rhs1) }

{- Note [prepareRhs]
~~~~~~~~~~~~~~~~~~~~
prepareRhs takes a putative RHS, checks whether it's a PAP or
constructor application and, if so, converts it to ANF, so that the
resulting thing can be inlined more easily.  Thus
        x = (f a, g b)
becomes
        t1 = f a
        t2 = g b
        x = (t1,t2)

We also want to deal well cases like this
        v = (f e1 `cast` co) e2
Here we want to make e1,e2 trivial and get
        x1 = e1; x2 = e2; v = (f x1 `cast` co) v2
That's what the 'go' loop in prepareRhs does
-}

prepareRhs :: HasDebugCallStack
           => SimplEnv -> TopLevelFlag
           -> FastString    -- Base for any new variables
           -> OutExpr
           -> SimplM (LetFloats, OutExpr)
-- Transforms a RHS into a better RHS by ANF'ing args
-- for expandable RHSs: constructors and PAPs
-- e.g        x = Just e
-- becomes    a = e               -- 'a' is fresh
--            x = Just a
-- See Note [prepareRhs]
prepareRhs env top_lvl occ rhs0
  | is_expandable = anfise rhs0
  | otherwise     = return (emptyLetFloats, rhs0)
  where
    -- We can' use exprIsExpandable because the WHOLE POINT is that
    -- we want to treat (K <big>) as expandable, because we are just
    -- about "anfise" the <big> expression.  exprIsExpandable would
    -- just say no!
    is_expandable = go rhs0 0
       where
         go (Var fun) n_val_args       = isExpandableApp fun n_val_args
         go (App fun arg) n_val_args
           | isTypeArg arg             = go fun n_val_args
           | otherwise                 = go fun (n_val_args + 1)
         go (Cast rhs _)  n_val_args   = go rhs n_val_args
         go (Tick _ rhs)  n_val_args   = go rhs n_val_args
         go _             _            = False

    anfise :: OutExpr -> SimplM (LetFloats, OutExpr)
    anfise (Cast rhs co)
        = do { (floats, rhs') <- anfise rhs
             ; return (floats, Cast rhs' co) }
    anfise (App fun (Type ty))
        = do { (floats, rhs') <- anfise fun
             ; return (floats, App rhs' (Type ty)) }
    anfise (App fun arg)
        = do { (floats1, fun') <- anfise fun
             ; (floats2, arg') <- makeTrivial env top_lvl topDmd occ arg
             ; return (floats1 `addLetFlts` floats2, App fun' arg') }
    anfise (Var fun)
        = return (emptyLetFloats, Var fun)

    anfise (Tick t rhs)
        -- We want to be able to float bindings past this
        -- tick. Non-scoping ticks don't care.
        | tickishScoped t == NoScope
        = do { (floats, rhs') <- anfise rhs
             ; return (floats, Tick t rhs') }

        -- On the other hand, for scoping ticks we need to be able to
        -- copy them on the floats, which in turn is only allowed if
        -- we can obtain non-counting ticks.
        | (not (tickishCounts t) || tickishCanSplit t)
        = do { (floats, rhs') <- anfise rhs
             ; let tickIt (id, expr) = (id, mkTick (mkNoCount t) expr)
                   floats' = mapLetFloats floats tickIt
             ; return (floats', Tick t rhs') }

    anfise other = return (emptyLetFloats, other)

makeTrivialArg :: HasDebugCallStack => SimplEnv -> ArgSpec -> SimplM (LetFloats, ArgSpec)
makeTrivialArg env arg@(ValArg { as_arg = e, as_dmd = dmd })
  = do { (floats, e') <- makeTrivial env NotTopLevel dmd (fsLit "arg") e
       ; return (floats, arg { as_arg = e' }) }
makeTrivialArg _ arg
  = return (emptyLetFloats, arg)  -- CastBy, TyArg

makeTrivial :: HasDebugCallStack
            => SimplEnv -> TopLevelFlag -> Demand
            -> FastString  -- ^ A "friendly name" to build the new binder from
            -> OutExpr
            -> SimplM (LetFloats, OutExpr)
-- Binds the expression to a variable, if it's not trivial, returning the variable
-- For the Demand argument, see Note [Keeping demand info in StrictArg Plan A]
makeTrivial env top_lvl dmd occ_fs expr
  | exprIsTrivial expr                          -- Already trivial
  || not (bindingOk top_lvl expr expr_ty)       -- Cannot trivialise
                                                --   See Note [Cannot trivialise]
  = return (emptyLetFloats, expr)

  | Cast expr' co <- expr
  = do { (floats, triv_expr) <- makeTrivial env top_lvl dmd occ_fs expr'
       ; return (floats, Cast triv_expr co) }

  | otherwise -- 'expr' is not of form (Cast e co)
  = do  { (floats, expr1) <- prepareRhs env top_lvl occ_fs expr
        ; uniq <- getUniqueM
        ; let name = mkSystemVarName uniq occ_fs
              var  = mkLocalIdWithInfo name ManyTy expr_ty id_info

        -- Now something very like completeBind,
        -- but without the postInlineUnconditionally part
        ; (arity_type, expr2) <- tryEtaExpandRhs env (BC_Let top_lvl NonRecursive) var expr1
          -- Technically we should extend the in-scope set in 'env' with
          -- the 'floats' from prepareRHS; but they are all fresh, so there is
          -- no danger of introducing name shadowing in eta expansion

        ; unf <- mkLetUnfolding uf_opts top_lvl VanillaSrc var expr2

        ; let final_id = addLetBndrInfo var arity_type unf
              bind     = NonRec final_id expr2

        ; traceSmpl "makeTrivial" (vcat [text "final_id" <+> ppr final_id, text "rhs" <+> ppr expr2 ])
        ; return ( floats `addLetFlts` unitLetFloat bind, Var final_id ) }
  where
    id_info = vanillaIdInfo `setDemandInfo` dmd
    expr_ty = exprType expr
    uf_opts = seUnfoldingOpts env

bindingOk :: TopLevelFlag -> CoreExpr -> Type -> Bool
-- True iff we can have a binding of this expression at this level
-- Precondition: the type is the type of the expression
bindingOk top_lvl expr expr_ty
  | isTopLevel top_lvl = exprIsTopLevelBindable expr expr_ty
  | otherwise          = True

{- Note [Cannot trivialise]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
   f :: Int -> Addr#

   foo :: Bar
   foo = Bar (f 3)

Then we can't ANF-ise foo, even though we'd like to, because
we can't make a top-level binding for the Addr# (f 3). And if
so we don't want to turn it into
   foo = let x = f 3 in Bar x
because we'll just end up inlining x back, and that makes the
simplifier loop.  Better not to ANF-ise it at all.

Literal strings are an exception.

   foo = Ptr "blob"#

We want to turn this into:

   foo1 = "blob"#
   foo = Ptr foo1

See Note [Core top-level string literals] in GHC.Core.

************************************************************************
*                                                                      *
          Completing a lazy binding
*                                                                      *
************************************************************************

completeBind
  * deals only with Ids, not TyVars
  * takes an already-simplified binder and RHS
  * is used for both recursive and non-recursive bindings
  * is used for both top-level and non-top-level bindings

It does the following:
  - tries discarding a dead binding
  - tries PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity
  - extend the InScopeSet of the SimplEnv

It does *not* attempt to do let-to-case.  Why?  Because it is used for
  - top-level bindings (when let-to-case is impossible)
  - many situations where the "rhs" is known to be a WHNF
                (so let-to-case is inappropriate).

Nor does it do the atomic-argument thing
-}

completeBind :: BindContext
             -> (InId, SimplEnv)           -- Old binder, and the static envt in which to simplify
                                           --   its stable unfolding (if any)
             -> (OutId, OutExpr, SimplEnv) -- New binder and rhs; can be a JoinId.
                                           -- And the SimplEnv with that OutId in scope.
             -> SimplM (SimplFloats, SimplEnv)
-- completeBind may choose to do its work
--      * by extending the substitution (e.g. let x = y in ...)
--      * or by adding to the floats in the envt
--
-- Binder /can/ be a JoinId
-- Precondition: rhs obeys the let-can-float invariant
completeBind bind_cxt (old_bndr, unf_se) (new_bndr, new_rhs, env)
 | isCoVar old_bndr
 = case new_rhs of
     Coercion co -> return (emptyFloats env, extendCvSubst env old_bndr co)
     _           -> return (mkFloatBind env (NonRec new_bndr new_rhs))

 | otherwise
 = assert (isId new_bndr) $
   do { let old_info = idInfo old_bndr
            old_unf  = realUnfoldingInfo old_info
            occ_info = occInfo old_info

         -- Do eta-expansion on the RHS of the binding
         -- See Note [Eta-expanding at let bindings] in GHC.Core.Opt.Simplify.Utils
      ; (new_arity, eta_rhs) <- tryEtaExpandRhs env bind_cxt new_bndr new_rhs

        -- Simplify the unfolding; see Note [Environment for simplLetUnfolding]
      ; new_unfolding <- simplLetUnfolding (unf_se `setInScopeFromE` env)
                            bind_cxt old_bndr
                            eta_rhs (idType new_bndr) new_arity old_unf

      ; let new_bndr_w_info = addLetBndrInfo new_bndr new_arity new_unfolding
        -- See Note [In-scope set as a substitution]

      ; if postInlineUnconditionally env bind_cxt new_bndr_w_info occ_info eta_rhs

        then -- Inline and discard the binding
             do  { tick (PostInlineUnconditionally old_bndr)
                 ; let unf_rhs = maybeUnfoldingTemplate new_unfolding `orElse` eta_rhs
                          -- See Note [Use occ-anald RHS in postInlineUnconditionally]
                 ; simplTrace "PostInlineUnconditionally" (ppr new_bndr <+> ppr unf_rhs) $
                   return ( emptyFloats env
                          , extendIdSubst env old_bndr $
                            DoneEx unf_rhs (idJoinPointHood new_bndr)) }
                -- Use the substitution to make quite, quite sure that the
                -- substitution will happen, since we are going to discard the binding

        else -- Keep the binding; do cast worker/wrapper
             -- pprTrace "Binding" (ppr new_bndr <+> ppr new_unfolding) $
             tryCastWorkerWrapper env bind_cxt old_bndr occ_info new_bndr_w_info eta_rhs }

addLetBndrInfo :: OutId -> ArityType -> Unfolding -> OutId
addLetBndrInfo new_bndr new_arity_type new_unf
  = new_bndr `setIdInfo` info5
  where
    new_arity = arityTypeArity new_arity_type
    info1 = idInfo new_bndr `setArityInfo` new_arity

    -- Unfolding info: Note [Setting the new unfolding]
    info2 = info1 `setUnfoldingInfo` new_unf

    -- Demand info: Note [Setting the demand info]
    info3 | isEvaldUnfolding new_unf
          = lazifyDemandInfo info2 `orElse` info2
          | otherwise
          = info2

    -- Bottoming bindings: see Note [Bottoming bindings]
    info4 = case arityTypeBotSigs_maybe new_arity_type of
        Nothing -> info3
        Just (ar, str_sig, cpr_sig) -> assert (ar == new_arity) $
                                       info3 `setDmdSigInfo` str_sig
                                             `setCprSigInfo` cpr_sig

     -- Zap call arity info. We have used it by now (via
     -- `tryEtaExpandRhs`), and the simplifier can invalidate this
     -- information, leading to broken code later (e.g. #13479)
    info5 = zapCallArityInfo info4


{- Note [Bottoming bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   let x = error "urk"
   in ...(case x of <alts>)...
or
   let f = \y. error (y ++ "urk")
   in ...(case f "foo" of <alts>)...

Then we'd like to drop the dead <alts> immediately.  So it's good to
propagate the info that x's (or f's) RHS is bottom to x's (or f's)
IdInfo as rapidly as possible.

We use tryEtaExpandRhs on every binding, and it turns out that the
arity computation it performs (via GHC.Core.Opt.Arity.findRhsArity) already
does a simple bottoming-expression analysis.  So all we need to do
is propagate that info to the binder's IdInfo.

This showed up in #12150; see comment:16.

There is a second reason for settting  the strictness signature. Consider
   let -- f :: <[S]b>
       f = \x. error "urk"
   in ...(f a b c)...
Then, in GHC.Core.Opt.Arity.findRhsArity we'll use the demand-info on `f`
to eta-expand to
   let f = \x y z. error "urk"
   in ...(f a b c)...

But now f's strictness signature has too short an arity; see
GHC.Core.Opt.DmdAnal Note [idArity varies independently of dmdTypeDepth].
Fortuitously, the same strictness-signature-fixup code
gives the function a new strictness signature with the right number of
arguments.  Example in stranal/should_compile/EtaExpansion.

Note [Setting the demand info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the unfolding is a value, the demand info may
go pear-shaped, so we nuke it.  Example:
     let x = (a,b) in
     case x of (p,q) -> h p q x
Here x is certainly demanded. But after we've nuked
the case, we'll get just
     let x = (a,b) in h a b x
and now x is not demanded (I'm assuming h is lazy)
This really happens.  Similarly
     let f = \x -> e in ...f..f...
After inlining f at some of its call sites the original binding may
(for example) be no longer strictly demanded.
The solution here is a bit ad hoc...

Note [Use occ-anald RHS in postInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we postInlineUnconditionally 'f in
  let f = \x -> x True in ...(f blah)...
then we'd like to inline the /occ-anald/ RHS for 'f'.  If we
use the non-occ-anald version, we'll end up with a
    ...(let x = blah in x True)...
and hence an extra Simplifier iteration.

We already /have/ the occ-anald version in the Unfolding for
the Id.  Well, maybe not /quite/ always.  If the binder is Dead,
postInlineUnconditionally will return True, but we may not have an
unfolding because it's too big. Hence the belt-and-braces `orElse`
in the defn of unf_rhs.  The Nothing case probably never happens.

Note [Environment for simplLetUnfolding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to be rather careful about the static environment in which
we simplify a stable unfolding.  Consider (#24242):

  f x = let y_Xb = ... in
        let step1_Xb {Stable unfolding = ....y_Xb...} = rhs in
         ...

Note that `y_Xb` and `step1_Xb` have the same unique (`Xb`). This can happen;
see Note [Shadowing in Core] in GHC.Core, and Note [Shadowing in the Simplifier].
This is perfectly fine. The `y_Xb` in the stable unfolding of the non-
recursive binding for `step1` refers, of course, to `let y_Xb = ....`.
When simplifying the binder `step1_Xb` we'll give it a new unique, and
extend the static environment with [Xb :-> step1_Xc], say.

But when simplifying step1's stable unfolding, we must use static environment
/before/ simplifying the binder `step1_Xb`; that is, a static envt that maps
[Xb :-> y_Xb], /not/ [Xb :-> step1_Xc].

That is why we pass around a pair `(InId, SimplEnv)` for the binder, keeping
track of the right environment for the unfolding of that InId.  See the type
of `simplLazyBind`, `simplJoinBind`, `completeBind`.

This only matters when we have
  - A non-recursive binding for f
  - has a stable unfolding
  - and that unfolding mentions a variable y
  - that has the same unique as f.
So triggering  a bug here is really hard!

************************************************************************
*                                                                      *
\subsection[Simplify-simplExpr]{The main function: simplExpr}
*                                                                      *
************************************************************************

The reason for this OutExprStuff stuff is that we want to float *after*
simplifying a RHS, not before.  If we do so naively we get quadratic
behaviour as things float out.

To see why it's important to do it after, consider this (real) example:

        let t = f x
        in fst t
==>
        let t = let a = e1
                    b = e2
                in (a,b)
        in fst t
==>
        let a = e1
            b = e2
            t = (a,b)
        in
        a       -- Can't inline a this round, cos it appears twice
==>
        e1

Each of the ==> steps is a round of simplification.  We'd save a
whole round if we float first.  This can cascade.  Consider

        let f = g d
        in \x -> ...f...
==>
        let f = let d1 = ..d.. in \y -> e
        in \x -> ...f...
==>
        let d1 = ..d..
        in \x -> ...(\y ->e)...

Only in this second round can the \y be applied, and it
might do the same again.
-}

simplExpr :: SimplEnv -> CoreExpr -> SimplM CoreExpr
simplExpr !env (Type ty) -- See Note [Bangs in the Simplifier]
  = do { ty' <- simplType env ty  -- See Note [Avoiding space leaks in OutType]
       ; return (Type ty') }

simplExpr env expr
  = simplExprC env expr (mkBoringStop expr_out_ty)
  where
    expr_out_ty :: OutType
    expr_out_ty = substTy env (exprType expr)
    -- NB: Since 'expr' is term-valued, not (Type ty), this call
    --     to exprType will succeed.  exprType fails on (Type ty).

simplExprC :: SimplEnv
           -> InExpr     -- A term-valued expression, never (Type ty)
           -> SimplCont
           -> SimplM OutExpr
        -- Simplify an expression, given a continuation
simplExprC env expr cont
  = -- pprTrace "simplExprC" (ppr expr $$ ppr cont) $
    do  { (floats, expr') <- simplExprF env expr cont
        ; -- pprTrace "simplExprC ret" (ppr expr $$ ppr expr') $
          -- pprTrace "simplExprC ret3" (ppr (seInScope env')) $
          -- pprTrace "simplExprC ret4" (ppr (seLetFloats env')) $
          return (wrapFloats floats expr') }

--------------------------------------------------
simplExprF :: SimplEnv
           -> InExpr     -- A term-valued expression, never (Type ty)
           -> SimplCont
           -> SimplM (SimplFloats, OutExpr)

simplExprF !env e !cont -- See Note [Bangs in the Simplifier]
  = {- pprTrace "simplExprF" (vcat
      [ ppr e
      , text "cont =" <+> ppr cont
      , text "inscope =" <+> ppr (seInScope env)
      , text "tvsubst =" <+> ppr (seTvSubst env)
      , text "idsubst =" <+> ppr (seIdSubst env)
      , text "cvsubst =" <+> ppr (seCvSubst env)
      ]) $ -}
    simplExprF1 env e cont

simplExprF1 :: HasDebugCallStack
            => SimplEnv -> InExpr -> SimplCont
            -> SimplM (SimplFloats, OutExpr)

simplExprF1 _ (Type ty) cont
  = pprPanic "simplExprF: type" (ppr ty <+> text"cont: " <+> ppr cont)
    -- simplExprF does only with term-valued expressions
    -- The (Type ty) case is handled separately by simplExpr
    -- and by the other callers of simplExprF

simplExprF1 env (Var v)        cont = {-#SCC "simplIdF" #-} simplIdF env v cont
simplExprF1 env (Lit lit)      cont = {-#SCC "rebuild" #-} rebuild env (Lit lit) cont
simplExprF1 env (Tick t expr)  cont = {-#SCC "simplTick" #-} simplTick env t expr cont
simplExprF1 env (Cast body co) cont = {-#SCC "simplCast" #-} simplCast env body co cont
simplExprF1 env (Coercion co)  cont = {-#SCC "simplCoercionF" #-} simplCoercionF env co cont

simplExprF1 env (App fun arg) cont
  = {-#SCC "simplExprF1-App" #-} case arg of
      Type ty -> do { -- The argument type will (almost) certainly be used
                      -- in the output program, so just force it now.
                      -- See Note [Avoiding space leaks in OutType]
                      arg' <- simplType env ty

                      -- But use substTy, not simplType, to avoid forcing
                      -- the hole type; it will likely not be needed.
                      -- See Note [The hole type in ApplyToTy]
                    ; let hole' = substTy env (exprType fun)

                    ; simplExprF env fun $
                      ApplyToTy { sc_arg_ty  = arg'
                                , sc_hole_ty = hole'
                                , sc_cont    = cont } }
      _       ->
          -- Crucially, sc_hole_ty is a /lazy/ binding.  It will
          -- be forced only if we need to run contHoleType.
          -- When these are forced, we might get quadratic behavior;
          -- this quadratic blowup could be avoided by drilling down
          -- to the function and getting its multiplicities all at once
          -- (instead of one-at-a-time). But in practice, we have not
          -- observed the quadratic behavior, so this extra entanglement
          -- seems not worthwhile.
        simplExprF env fun $
        ApplyToVal { sc_arg = arg, sc_env = env
                   , sc_hole_ty = substTy env (exprType fun)
                   , sc_dup = NoDup, sc_cont = cont }

simplExprF1 env expr@(Lam {}) cont
  = {-#SCC "simplExprF1-Lam" #-}
    simplLam env (zapLambdaBndrs expr n_args) cont
        -- zapLambdaBndrs: the issue here is under-saturated lambdas
        --   (\x1. \x2. e) arg1
        -- Here x1 might have "occurs-once" occ-info, because occ-info
        -- is computed assuming that a group of lambdas is applied
        -- all at once.  If there are too few args, we must zap the
        -- occ-info, UNLESS the remaining binders are one-shot
  where
    n_args = countArgs cont
        -- NB: countArgs counts all the args (incl type args)
        -- and likewise drop counts all binders (incl type lambdas)

simplExprF1 env (Case scrut bndr _ alts) cont
  = {-#SCC "simplExprF1-Case" #-}
    simplExprF env scrut (Select { sc_dup = NoDup, sc_bndr = bndr
                                 , sc_alts = alts
                                 , sc_env = env, sc_cont = cont })

simplExprF1 env (Let (Rec pairs) body) cont
  | Just pairs' <- joinPointBindings_maybe pairs
  = {-#SCC "simplRecJoinPoin" #-} simplRecJoinPoint env pairs' body cont

  | otherwise
  = {-#SCC "simplRecE" #-} simplRecE env pairs body cont

simplExprF1 env (Let (NonRec bndr rhs) body) cont
  | Type ty <- rhs    -- First deal with type lets (let a = Type ty in e)
  = {-#SCC "simplExprF1-NonRecLet-Type" #-}
    assert (isTyVar bndr) $
    do { ty' <- simplType env ty
       ; simplExprF (extendTvSubst env bndr ty') body cont }

  | Just env' <- preInlineUnconditionally env NotTopLevel bndr rhs env
    -- Because of the let-can-float invariant, it's ok to
    -- inline freely, or to drop the binding if it is dead.
  = do { tick (PreInlineUnconditionally bndr)
       ; simplExprF env' body cont }

  -- Now check for a join point.  It's better to do the preInlineUnconditionally
  -- test first, because joinPointBinding_maybe has to eta-expand, so a trivial
  -- binding like { j = j2 |> co } would first be eta-expanded and then inlined
  -- Better to test preInlineUnconditionally first.
  | Just (bndr', rhs') <- joinPointBinding_maybe bndr rhs
  = {-#SCC "simplNonRecJoinPoint" #-}
    simplNonRecJoinPoint env bndr' rhs' body cont

  | otherwise
  = {-#SCC "simplNonRecE" #-}
    simplNonRecE env FromLet bndr (rhs, env) body cont

{- Note [Avoiding space leaks in OutType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the simplifier is run for multiple iterations, we need to ensure
that any thunks in the output of one simplifier iteration are forced
by the evaluation of the next simplifier iteration. Otherwise we may
retain multiple copies of the Core program and leak a terrible amount
of memory (as in #13426).

The simplifier is naturally strict in the entire "Expr part" of the
input Core program, because any expression may contain binders, which
we must find in order to extend the SimplEnv accordingly. But types
do not contain binders and so it is tempting to write things like

    simplExpr env (Type ty) = return (Type (substTy env ty))   -- Bad!

This is Bad because the result includes a thunk (substTy env ty) which
retains a reference to the whole simplifier environment; and the next
simplifier iteration will not force this thunk either, because the
line above is not strict in ty.

So instead our strategy is for the simplifier to fully evaluate
OutTypes when it emits them into the output Core program, for example

    simplExpr env (Type ty) = do { ty' <- simplType env ty     -- Good
                                 ; return (Type ty') }

where the only difference from above is that simplType calls seqType
on the result of substTy.

However, SimplCont can also contain OutTypes and it's not necessarily
a good idea to force types on the way in to SimplCont, because they
may end up not being used and forcing them could be a lot of wasted
work. T5631 is a good example of this.

- For ApplyToTy's sc_arg_ty, we force the type on the way in because
  the type will almost certainly appear as a type argument in the
  output program.

- For the hole types in Stop and ApplyToTy, we force the type when we
  emit it into the output program, after obtaining it from
  contResultType. (The hole type in ApplyToTy is only directly used
  to form the result type in a new Stop continuation.)
-}

---------------------------------
-- Simplify a join point, adding the context.
-- Context goes *inside* the lambdas. IOW, if the join point has arity n, we do:
--   \x1 .. xn -> e => \x1 .. xn -> E[e]
-- Note that we need the arity of the join point, since e may be a lambda
-- (though this is unlikely). See Note [Join points and case-of-case].
simplJoinRhs :: SimplEnv -> InId -> InExpr -> SimplCont
             -> SimplM OutExpr
simplJoinRhs env bndr expr cont
  | JoinPoint arity <- idJoinPointHood bndr
  =  do { let (join_bndrs, join_body) = collectNBinders arity expr
              mult = contHoleScaling cont
        ; (env', join_bndrs') <- simplLamBndrs env (map (scaleVarBy mult) join_bndrs)
        ; join_body' <- simplExprC env' join_body cont
        ; return $ mkLams join_bndrs' join_body' }

  | otherwise
  = pprPanic "simplJoinRhs" (ppr bndr)

---------------------------------
simplType :: SimplEnv -> InType -> SimplM OutType
        -- Kept monadic just so we can do the seqType
        -- See Note [Avoiding space leaks in OutType]
simplType env ty
  = -- pprTrace "simplType" (ppr ty $$ ppr (seTvSubst env)) $
    seqType new_ty `seq` return new_ty
  where
    new_ty = substTy env ty

---------------------------------
simplCoercionF :: SimplEnv -> InCoercion -> SimplCont
               -> SimplM (SimplFloats, OutExpr)
simplCoercionF env co cont
  = do { co' <- simplCoercion env co
       ; rebuild env (Coercion co') cont }

simplCoercion :: SimplEnv -> InCoercion -> SimplM OutCoercion
simplCoercion env co
  = do { let opt_co = optCoercion opts (getSubst env) co
       ; seqCo opt_co `seq` return opt_co }
  where
    opts = seOptCoercionOpts env

-----------------------------------
-- | Push a TickIt context outwards past applications and cases, as
-- long as this is a non-scoping tick, to let case and application
-- optimisations apply.

simplTick :: SimplEnv -> CoreTickish -> InExpr -> SimplCont
          -> SimplM (SimplFloats, OutExpr)
simplTick env tickish expr cont
  -- A scoped tick turns into a continuation, so that we can spot
  -- (scc t (\x . e)) in simplLam and eliminate the scc.  If we didn't do
  -- it this way, then it would take two passes of the simplifier to
  -- reduce ((scc t (\x . e)) e').
  -- NB, don't do this with counting ticks, because if the expr is
  -- bottom, then rebuildCall will discard the continuation.

-- XXX: we cannot do this, because the simplifier assumes that
-- the context can be pushed into a case with a single branch. e.g.
--    scc<f>  case expensive of p -> e
-- becomes
--    case expensive of p -> scc<f> e
--
-- So I'm disabling this for now.  It just means we will do more
-- simplifier iterations that necessary in some cases.

--  | tickishScoped tickish && not (tickishCounts tickish)
--  = simplExprF env expr (TickIt tickish cont)

  -- For unscoped or soft-scoped ticks, we are allowed to float in new
  -- cost, so we simply push the continuation inside the tick.  This
  -- has the effect of moving the tick to the outside of a case or
  -- application context, allowing the normal case and application
  -- optimisations to fire.
  | tickish `tickishScopesLike` SoftScope
  = do { (floats, expr') <- simplExprF env expr cont
       ; return (floats, mkTick tickish expr')
       }

  -- Push tick inside if the context looks like this will allow us to
  -- do a case-of-case - see Note [case-of-scc-of-case]
  | Select {} <- cont, Just expr' <- push_tick_inside
  = simplExprF env expr' cont

  -- We don't want to move the tick, but we might still want to allow
  -- floats to pass through with appropriate wrapping (or not, see
  -- wrap_floats below)
  --- | not (tickishCounts tickish) || tickishCanSplit tickish
  -- = wrap_floats

  | otherwise
  = no_floating_past_tick

 where

  -- Try to push tick inside a case, see Note [case-of-scc-of-case].
  push_tick_inside =
    case expr0 of
      Case scrut bndr ty alts
             -> Just $ Case (tickScrut scrut) bndr ty (map tickAlt alts)
      _other -> Nothing
   where (ticks, expr0) = stripTicksTop movable (Tick tickish expr)
         movable t      = not (tickishCounts t) ||
                          t `tickishScopesLike` NoScope ||
                          tickishCanSplit t
         tickScrut e    = foldr mkTick e ticks
         -- Alternatives get annotated with all ticks that scope in some way,
         -- but we don't want to count entries.
         tickAlt (Alt c bs e) = Alt c bs (foldr mkTick e ts_scope)
         ts_scope         = map mkNoCount $
                            filter (not . (`tickishScopesLike` NoScope)) ticks

  no_floating_past_tick =
    do { let (inc,outc) = splitCont cont
       ; (floats, expr1) <- simplExprF env expr inc
       ; let expr2    = wrapFloats floats expr1
             tickish' = simplTickish env tickish
       ; rebuild env (mkTick tickish' expr2) outc
       }

-- Alternative version that wraps outgoing floats with the tick.  This
-- results in ticks being duplicated, as we don't make any attempt to
-- eliminate the tick if we re-inline the binding (because the tick
-- semantics allows unrestricted inlining of HNFs), so I'm not doing
-- this any more.  FloatOut will catch any real opportunities for
-- floating.
--
--  wrap_floats =
--    do { let (inc,outc) = splitCont cont
--       ; (env', expr') <- simplExprF (zapFloats env) expr inc
--       ; let tickish' = simplTickish env tickish
--       ; let wrap_float (b,rhs) = (zapIdDmdSig (setIdArity b 0),
--                                   mkTick (mkNoCount tickish') rhs)
--              -- when wrapping a float with mkTick, we better zap the Id's
--              -- strictness info and arity, because it might be wrong now.
--       ; let env'' = addFloats env (mapFloats env' wrap_float)
--       ; rebuild env'' expr' (TickIt tickish' outc)
--       }


  simplTickish env tickish
    | Breakpoint ext n ids modl <- tickish
          = Breakpoint ext n (mapMaybe (getDoneId . substId env) ids) modl
    | otherwise = tickish

  -- Push type application and coercion inside a tick
  splitCont :: SimplCont -> (SimplCont, SimplCont)
  splitCont cont@(ApplyToTy { sc_cont = tail }) = (cont { sc_cont = inc }, outc)
    where (inc,outc) = splitCont tail
  splitCont (CastIt co c) = (CastIt co inc, outc)
    where (inc,outc) = splitCont c
  splitCont other = (mkBoringStop (contHoleType other), other)

  getDoneId (DoneId id)  = Just id
  getDoneId (DoneEx (Var id) _) = Just id
  getDoneId (DoneEx e _) = getIdFromTrivialExpr_maybe e -- Note [substTickish] in GHC.Core.Subst
  getDoneId other = pprPanic "getDoneId" (ppr other)

-- Note [case-of-scc-of-case]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It's pretty important to be able to transform case-of-case when
-- there's an SCC in the way.  For example, the following comes up
-- in nofib/real/compress/Encode.hs:
--
--        case scctick<code_string.r1>
--             case $wcode_string_r13s wild_XC w1_s137 w2_s138 l_aje
--             of _ { (# ww1_s13f, ww2_s13g, ww3_s13h #) ->
--             (ww1_s13f, ww2_s13g, ww3_s13h)
--             }
--        of _ { (ww_s12Y, ww1_s12Z, ww2_s130) ->
--        tick<code_string.f1>
--        (ww_s12Y,
--         ww1_s12Z,
--         PTTrees.PT
--           @ GHC.Types.Char @ GHC.Types.Int wild2_Xj ww2_s130 r_ajf)
--        }
--
-- We really want this case-of-case to fire, because then the 3-tuple
-- will go away (indeed, the CPR optimisation is relying on this
-- happening).  But the scctick is in the way - we need to push it
-- inside to expose the case-of-case.  So we perform this
-- transformation on the inner case:
--
--   scctick c (case e of { p1 -> e1; ...; pn -> en })
--    ==>
--   case (scctick c e) of { p1 -> scc c e1; ...; pn -> scc c en }
--
-- So we've moved a constant amount of work out of the scc to expose
-- the case.  We only do this when the continuation is interesting: in
-- for now, it has to be another Case (maybe generalise this later).

{-
************************************************************************
*                                                                      *
\subsection{The main rebuilder}
*                                                                      *
************************************************************************
-}

rebuild :: SimplEnv -> OutExpr -> SimplCont -> SimplM (SimplFloats, OutExpr)
-- At this point the substitution in the SimplEnv should be irrelevant;
-- only the in-scope set matters
rebuild env expr cont
  = case cont of
      Stop {}          -> return (emptyFloats env, expr)
      TickIt t cont    -> rebuild env (mkTick t expr) cont
      CastIt co cont   -> rebuild env (mkCast expr co) cont
                       -- NB: mkCast implements the (Coercion co |> g) optimisation

      Select { sc_bndr = bndr, sc_alts = alts, sc_env = se, sc_cont = cont }
        -> rebuildCase (se `setInScopeFromE` env) expr bndr alts cont

      StrictArg { sc_fun = fun, sc_cont = cont, sc_fun_ty = fun_ty }
        -> rebuildCall env (addValArgTo fun expr fun_ty ) cont

      StrictBind { sc_bndr = b, sc_body = body, sc_env = se
                 , sc_cont = cont, sc_from = from_what }
        -> completeBindX (se `setInScopeFromE` env) from_what b expr body cont

      ApplyToTy  { sc_arg_ty = ty, sc_cont = cont}
        -> rebuild env (App expr (Type ty)) cont

      ApplyToVal { sc_arg = arg, sc_env = se, sc_dup = dup_flag
                 , sc_cont = cont, sc_hole_ty = fun_ty }
        -- See Note [Avoid redundant simplification]
        -> do { (_, _, arg') <- simplLazyArg env dup_flag fun_ty Nothing se arg
              ; rebuild env (App expr arg') cont }

completeBindX :: SimplEnv
              -> FromWhat
              -> InId -> OutExpr   -- Non-recursively bind this Id to this (simplified) expression
                                   -- (the let-can-float invariant may not be satisfied)
              -> InExpr            -- In this body
              -> SimplCont         -- Consumed by this continuation
              -> SimplM (SimplFloats, OutExpr)
completeBindX env from_what bndr rhs body cont
  | FromBeta arg_levity <- from_what
  , needsCaseBindingL arg_levity rhs -- Enforcing the let-can-float-invariant
  = do { (env1, bndr1)   <- simplNonRecBndr env bndr  -- Lambda binders don't have rules
       ; (floats, expr') <- simplNonRecBody env1 from_what body cont
       -- Do not float floats past the Case binder below
       ; let expr'' = wrapFloats floats expr'
             case_expr = Case rhs bndr1 (contResultType cont) [Alt DEFAULT [] expr'']
       ; return (emptyFloats env, case_expr) }

  | otherwise -- Make a let-binding
  = do  { (env1, bndr1) <- simplNonRecBndr env bndr
        ; (env2, bndr2) <- addBndrRules env1 bndr bndr1 (BC_Let NotTopLevel NonRecursive)

        ; let is_strict = isStrictId bndr2
              -- isStrictId: use simplified binder because the InId bndr might not have
              -- a fixed runtime representation, which isStrictId doesn't expect
              -- c.f. Note [Dark corner with representation polymorphism]

        ; (rhs_floats, rhs1) <- prepareBinding env NotTopLevel NonRecursive is_strict
                                               bndr2 (emptyFloats env) rhs
              -- NB: it makes a surprisingly big difference (5% in compiler allocation
              -- in T9630) to pass 'env' rather than 'env1'.  It's fine to pass 'env',
              -- because this is simplNonRecX, so bndr is not in scope in the RHS.

        ; let env3 = env2 `setInScopeFromF` rhs_floats
        ; (bind_float, env4) <- completeBind (BC_Let NotTopLevel NonRecursive)
                                             (bndr,env) (bndr2, rhs1, env3)
              -- Must pass env1 to completeBind in case simplBinder had to clone,
              -- and extended the substitution with [bndr :-> new_bndr]

        -- Simplify the body
        ; (body_floats, body') <- simplNonRecBody env4 from_what body cont

        ; let all_floats = rhs_floats `addFloats` bind_float `addFloats` body_floats
        ; return ( all_floats, body' ) }

{-
************************************************************************
*                                                                      *
\subsection{Lambdas}
*                                                                      *
************************************************************************
-}

{- Note [Optimising reflexivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important (for compiler performance) to get rid of reflexivity as soon
as it appears.  See #11735, #14737, and #15019.

In particular, we want to behave well on

 *  e |> co1 |> co2
    where the two happen to cancel out entirely. That is quite common;
    e.g. a newtype wrapping and unwrapping cancel.


 * (f |> co) @t1 @t2 ... @tn x1 .. xm
   Here we will use pushCoTyArg and pushCoValArg successively, which
   build up SelCo stacks.  Silly to do that if co is reflexive.

However, we don't want to call isReflexiveCo too much, because it uses
type equality which is expensive on big types (#14737 comment:7).

A good compromise (determined experimentally) seems to be to call
isReflexiveCo
 * when composing casts, and
 * at the end

In investigating this I saw missed opportunities for on-the-fly
coercion shrinkage. See #15090.
-}


simplCast :: SimplEnv -> InExpr -> Coercion -> SimplCont
          -> SimplM (SimplFloats, OutExpr)
simplCast env body co0 cont0
  = do  { co1   <- {-#SCC "simplCast-simplCoercion" #-} simplCoercion env co0
        ; cont1 <- {-#SCC "simplCast-addCoerce" #-}
                   if isReflCo co1
                   then return cont0  -- See Note [Optimising reflexivity]
                   else addCoerce co1 cont0
        ; {-#SCC "simplCast-simplExprF" #-} simplExprF env body cont1 }
  where
        -- If the first parameter is MRefl, then simplifying revealed a
        -- reflexive coercion. Omit.
        addCoerceM :: MOutCoercion -> SimplCont -> SimplM SimplCont
        addCoerceM MRefl   cont = return cont
        addCoerceM (MCo co) cont = addCoerce co cont

        addCoerce :: OutCoercion -> SimplCont -> SimplM SimplCont
        addCoerce co1 (CastIt co2 cont)  -- See Note [Optimising reflexivity]
          | isReflexiveCo co' = return cont
          | otherwise         = addCoerce co' cont
          where
            co' = mkTransCo co1 co2

        addCoerce co (ApplyToTy { sc_arg_ty = arg_ty, sc_cont = tail })
          | Just (arg_ty', m_co') <- pushCoTyArg co arg_ty
          = {-#SCC "addCoerce-pushCoTyArg" #-}
            do { tail' <- addCoerceM m_co' tail
               ; return (ApplyToTy { sc_arg_ty  = arg_ty'
                                   , sc_cont    = tail'
                                   , sc_hole_ty = coercionLKind co }) }
                                        -- NB!  As the cast goes past, the
                                        -- type of the hole changes (#16312)
        -- (f |> co) e   ===>   (f (e |> co1)) |> co2
        -- where   co :: (s1->s2) ~ (t1->t2)
        --         co1 :: t1 ~ s1
        --         co2 :: s2 ~ t2
        addCoerce co cont@(ApplyToVal { sc_arg = arg, sc_env = arg_se
                                      , sc_dup = dup, sc_cont = tail
                                      , sc_hole_ty = fun_ty })
          | Just (m_co1, m_co2) <- pushCoValArg co
          , fixed_rep m_co1
          = {-#SCC "addCoerce-pushCoValArg" #-}
            do { tail' <- addCoerceM m_co2 tail
               ; case m_co1 of {
                   MRefl -> return (cont { sc_cont = tail'
                                         , sc_hole_ty = coercionLKind co }) ;
                      -- Avoid simplifying if possible;
                      -- See Note [Avoiding exponential behaviour]

                   MCo co1 ->
            do { (dup', arg_se', arg') <- simplLazyArg env dup fun_ty Nothing arg_se arg
                    -- When we build the ApplyTo we can't mix the OutCoercion
                    -- 'co' with the InExpr 'arg', so we simplify
                    -- to make it all consistent.  It's a bit messy.
                    -- But it isn't a common case.
                    -- Example of use: #995
               ; return (ApplyToVal { sc_arg  = mkCast arg' co1
                                    , sc_env  = arg_se'
                                    , sc_dup  = dup'
                                    , sc_cont = tail'
                                    , sc_hole_ty = coercionLKind co }) } } }

        addCoerce co cont
          | isReflexiveCo co = return cont  -- Having this at the end makes a huge
                                            -- difference in T12227, for some reason
                                            -- See Note [Optimising reflexivity]
          | otherwise        = return (CastIt co cont)

        fixed_rep :: MCoercionR -> Bool
        fixed_rep MRefl    = True
        fixed_rep (MCo co) = typeHasFixedRuntimeRep $ coercionRKind co
          -- Without this check, we can get an argument which does not
          -- have a fixed runtime representation.
          -- See Note [Representation polymorphism invariants] in GHC.Core
          -- test: typecheck/should_run/EtaExpandLevPoly

simplLazyArg :: SimplEnv -> DupFlag
             -> OutType                 -- ^ Type of the function applied to this arg
             -> Maybe ArgInfo           -- ^ Just <=> This arg `ai` occurs in an app
                                        --   `f a1 ... an` where we have ArgInfo on
                                        --   how `f` uses `ai`, affecting the Stop
                                        --   continuation passed to 'simplExprC'
             -> StaticEnv -> CoreExpr   -- ^ Expression with its static envt
             -> SimplM (DupFlag, StaticEnv, OutExpr)
simplLazyArg env dup_flag fun_ty mb_arg_info arg_env arg
  | isSimplified dup_flag
  = return (dup_flag, arg_env, arg)
  | otherwise
  = do { let arg_env' = arg_env `setInScopeFromE` env
       ; let arg_ty = funArgTy fun_ty
       ; let stop = case mb_arg_info of
               Nothing -> mkBoringStop arg_ty
               Just ai -> mkLazyArgStop arg_ty ai
       ; arg' <- simplExprC arg_env' arg stop
       ; return (Simplified, zapSubstEnv arg_env', arg') }
         -- Return a StaticEnv that includes the in-scope set from 'env',
         -- because arg' may well mention those variables (#20639)

{-
************************************************************************
*                                                                      *
\subsection{Lambdas}
*                                                                      *
************************************************************************
-}

simplNonRecBody :: SimplEnv -> FromWhat
                -> InExpr -> SimplCont
                -> SimplM (SimplFloats, OutExpr)
simplNonRecBody env from_what body cont
  = case from_what of
      FromLet     -> simplExprF env body cont
      FromBeta {} -> simplLam   env body cont

simplLam :: SimplEnv -> InExpr -> SimplCont
         -> SimplM (SimplFloats, OutExpr)

simplLam env (Lam bndr body) cont = simpl_lam env bndr body cont
simplLam env expr            cont = simplExprF env expr cont

simpl_lam :: HasDebugCallStack
          => SimplEnv -> InBndr -> InExpr -> SimplCont
          -> SimplM (SimplFloats, OutExpr)

-- Type beta-reduction
simpl_lam env bndr body (ApplyToTy { sc_arg_ty = arg_ty, sc_cont = cont })
  = do { tick (BetaReduction bndr)
       ; simplLam (extendTvSubst env bndr arg_ty) body cont }

-- Coercion beta-reduction
simpl_lam env bndr body (ApplyToVal { sc_arg = Coercion arg_co, sc_env = arg_se
                                    , sc_cont = cont })
  = assertPpr (isCoVar bndr) (ppr bndr) $
    do { tick (BetaReduction bndr)
       ; let arg_co' = substCo (arg_se `setInScopeFromE` env) arg_co
       ; simplLam (extendCvSubst env bndr arg_co') body cont }

-- Value beta-reduction
-- This works for /coercion/ lambdas too
simpl_lam env bndr body (ApplyToVal { sc_arg = arg, sc_env = arg_se
                                    , sc_cont = cont, sc_dup = dup
                                    , sc_hole_ty = fun_ty})
  = do { tick (BetaReduction bndr)
       ; let from_what = FromBeta arg_levity
             arg_levity
               | isForAllTy fun_ty = assertPpr (isCoVar bndr) (ppr bndr) Unlifted
               | otherwise         = typeLevity (funArgTy fun_ty)
             -- Example:  (\(cv::a ~# b). blah) co
             -- The type of (\cv.blah) can be (forall cv. ty); see GHC.Core.Utils.mkLamType

             -- Using fun_ty: see Note [Dark corner with representation polymorphism]
             -- e.g  (\r \(a::TYPE r) \(x::a). blah) @LiftedRep @Int arg
             --      When we come to `x=arg` we must choose lazy/strict correctly
             --      It's wrong to err in either direction
             --      But fun_ty is an OutType, so is fully substituted

       ; if | isSimplified dup  -- Don't re-simplify if we've simplified it once
                                -- Including don't preInlineUnconditionally
                                -- See Note [Avoiding exponential behaviour]
            -> completeBindX env from_what bndr arg body cont

            | Just env' <- preInlineUnconditionally env NotTopLevel bndr arg arg_se
            , not (needsCaseBindingL arg_levity arg)
              -- Ok to test arg::InExpr in needsCaseBinding because
              -- exprOkForSpeculation is stable under simplification
            -> do { tick (PreInlineUnconditionally bndr)
                  ; simplLam env' body cont }

            | otherwise
            -> simplNonRecE env from_what bndr (arg, arg_se) body cont }

-- Discard a non-counting tick on a lambda.  This may change the
-- cost attribution slightly (moving the allocation of the
-- lambda elsewhere), but we don't care: optimisation changes
-- cost attribution all the time.
simpl_lam env bndr body (TickIt tickish cont)
  | not (tickishCounts tickish)
  = simpl_lam env bndr body cont

-- Not enough args, so there are real lambdas left to put in the result
simpl_lam env bndr body cont
  = do  { let (inner_bndrs, inner_body) = collectBinders body
        ; (env', bndrs') <- simplLamBndrs env (bndr:inner_bndrs)
        ; body'   <- simplExpr env' inner_body
        ; new_lam <- rebuildLam env' bndrs' body' cont
        ; rebuild env' new_lam cont }

-------------
simplLamBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- Historically this had a special case for when a lambda-binder
-- could have a stable unfolding;
-- see Historical Note [Case binders and join points]
-- But now it is much simpler! We now only remove unfoldings.
-- See Note [Never put `OtherCon` unfoldings on lambda binders]
simplLamBndr env bndr = simplBinder env (zapIdUnfolding bndr)

simplLamBndrs :: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplLamBndrs env bndrs = mapAccumLM simplLamBndr env bndrs

------------------
simplNonRecE :: HasDebugCallStack
             => SimplEnv
             -> FromWhat
             -> InId               -- The binder, always an Id
                                   -- Never a join point
                                   -- The static env for its unfolding (if any) is the first parameter
             -> (InExpr, SimplEnv) -- Rhs of binding (or arg of lambda)
             -> InExpr             -- Body of the let/lambda
             -> SimplCont
             -> SimplM (SimplFloats, OutExpr)

-- simplNonRecE is used for
--  * from=FromLet:  a non-top-level non-recursive non-join-point let-expression
--  * from=FromBeta: a binding arising from a beta reduction
--
-- simplNonRecE env b (rhs, rhs_se) body k
--   = let env in
--     cont< let b = rhs_se(rhs) in body >
--
-- It deals with strict bindings, via the StrictBind continuation,
-- which may abort the whole process.
--
-- from_what=FromLet => the RHS satisfies the let-can-float invariant
-- Otherwise it may or may not satisfy it.

simplNonRecE env from_what bndr (rhs, rhs_se) body cont
  | assert (isId bndr && not (isJoinId bndr) ) $
    is_strict_bind
  = -- Evaluate RHS strictly
    simplExprF (rhs_se `setInScopeFromE` env) rhs
               (StrictBind { sc_bndr = bndr, sc_body = body, sc_from = from_what
                           , sc_env = env, sc_cont = cont, sc_dup = NoDup })

  | otherwise  -- Evaluate RHS lazily
  = do { (env1, bndr1)    <- simplNonRecBndr env bndr
       ; (env2, bndr2)    <- addBndrRules env1 bndr bndr1 (BC_Let NotTopLevel NonRecursive)
       ; (floats1, env3)  <- simplLazyBind NotTopLevel NonRecursive
                                           (bndr,env) (bndr2,env2) (rhs,rhs_se)
       ; (floats2, expr') <- simplNonRecBody env3 from_what body cont
       ; return (floats1 `addFloats` floats2, expr') }

  where
    is_strict_bind = case from_what of
       FromBeta Unlifted -> True
       -- If we are coming from a beta-reduction (FromBeta) we must
       -- establish the let-can-float invariant, so go via StrictBind
       -- If not, the invariant holds already, and it's optional.

       -- (FromBeta Lifted) or FromLet: look at the demand info
       _ -> seCaseCase env && isStrUsedDmd (idDemandInfo bndr)


------------------
simplRecE :: SimplEnv
          -> [(InId, InExpr)]
          -> InExpr
          -> SimplCont
          -> SimplM (SimplFloats, OutExpr)

-- simplRecE is used for
--  * non-top-level recursive lets in expressions
-- Precondition: not a join-point binding
simplRecE env pairs body cont
  = do  { let bndrs = map fst pairs
        ; massert (all (not . isJoinId) bndrs)
        ; env1 <- simplRecBndrs env bndrs
                -- NB: bndrs' don't have unfoldings or rules
                -- We add them as we go down
        ; (floats1, env2)  <- simplRecBind env1 (BC_Let NotTopLevel Recursive) pairs
        ; (floats2, expr') <- simplExprF env2 body cont
        ; return (floats1 `addFloats` floats2, expr') }

{- Note [Dark corner with representation polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `simplNonRecE`, the call to `needsCaseBinding` or to `isStrictId` will fail
if the binder does not have a fixed runtime representation, e.g. if it is of kind (TYPE r).
So we are careful to call `isStrictId` on the OutId, not the InId, in case we have
     ((\(r::RuntimeRep) \(x::TYPE r). blah) Lifted arg)
That will lead to `simplNonRecE env (x::TYPE r) arg`, and we can't tell
if x is lifted or unlifted from that.

We only get such redexes from the compulsory inlining of a wired-in,
representation-polymorphic function like `rightSection` (see
GHC.Types.Id.Make).  Mind you, SimpleOpt should probably have inlined
such compulsory inlinings already, but belt and braces does no harm.

Plus, it turns out that GHC.Driver.Main.hscCompileCoreExpr calls the
Simplifier without first calling SimpleOpt, so anything involving
GHCi or TH and operator sections will fall over if we don't take
care here.

Note [Avoiding exponential behaviour]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One way in which we can get exponential behaviour is if we simplify a
big expression, and then re-simplify it -- and then this happens in a
deeply-nested way.  So we must be jolly careful about re-simplifying
an expression (#13379).  That is why simplNonRecX does not try
preInlineUnconditionally (unlike simplNonRecE).

Example:
  f BIG, where f has a RULE
Then
 * We simplify BIG before trying the rule; but the rule does not fire
 * We inline f = \x. x True
 * So if we did preInlineUnconditionally we'd re-simplify (BIG True)

However, if BIG has /not/ already been simplified, we'd /like/ to
simplify BIG True; maybe good things happen.  That is why

* simplLam has
    - a case for (isSimplified dup), which goes via simplNonRecX, and
    - a case for the un-simplified case, which goes via simplNonRecE

* We go to some efforts to avoid unnecessarily simplifying ApplyToVal,
  in at least two places
    - In simplCast/addCoerce, where we check for isReflCo
    - In rebuildCall we avoid simplifying arguments before we have to
      (see Note [Trying rewrite rules])


************************************************************************
*                                                                      *
                     Join points
*                                                                      *
********************************************************************* -}

{- Note [Rules and unfolding for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   simplExpr (join j x = rhs                         ) cont
             (      {- RULE j (p:ps) = blah -}       )
             (      {- StableUnfolding j = blah -}   )
             (in blah                                )

Then we will push 'cont' into the rhs of 'j'.  But we should *also* push
'cont' into the RHS of
  * Any RULEs for j, e.g. generated by SpecConstr
  * Any stable unfolding for j, e.g. the result of an INLINE pragma

Simplifying rules and stable-unfoldings happens a bit after
simplifying the right-hand side, so we remember whether or not it
is a join point, and what 'cont' is, in a value of type MaybeJoinCont

#13900 was caused by forgetting to push 'cont' into the RHS
of a SpecConstr-generated RULE for a join point.
-}

simplNonRecJoinPoint :: SimplEnv -> InId -> InExpr
                     -> InExpr -> SimplCont
                     -> SimplM (SimplFloats, OutExpr)
simplNonRecJoinPoint env bndr rhs body cont
   = assert (isJoinId bndr ) $
     wrapJoinCont env cont $ \ env cont ->
     do { -- We push join_cont into the join RHS and the body;
          -- and wrap wrap_cont around the whole thing
        ; let mult   = contHoleScaling cont
              res_ty = contResultType cont
        ; (env1, bndr1)    <- simplNonRecJoinBndr env bndr mult res_ty
        ; (env2, bndr2)    <- addBndrRules env1 bndr bndr1 (BC_Join NonRecursive cont)
        ; (floats1, env3)  <- simplJoinBind NonRecursive cont (bndr,env) (bndr2,env2) (rhs,env)
        ; (floats2, body') <- simplExprF env3 body cont
        ; return (floats1 `addFloats` floats2, body') }


------------------
simplRecJoinPoint :: SimplEnv -> [(InId, InExpr)]
                  -> InExpr -> SimplCont
                  -> SimplM (SimplFloats, OutExpr)
simplRecJoinPoint env pairs body cont
  = wrapJoinCont env cont $ \ env cont ->
    do { let bndrs  = map fst pairs
             mult   = contHoleScaling cont
             res_ty = contResultType cont
       ; env1 <- simplRecJoinBndrs env bndrs mult res_ty
               -- NB: bndrs' don't have unfoldings or rules
               -- We add them as we go down
       ; (floats1, env2)  <- simplRecBind env1 (BC_Join Recursive cont) pairs
       ; (floats2, body') <- simplExprF env2 body cont
       ; return (floats1 `addFloats` floats2, body') }

--------------------
wrapJoinCont :: SimplEnv -> SimplCont
             -> (SimplEnv -> SimplCont -> SimplM (SimplFloats, OutExpr))
             -> SimplM (SimplFloats, OutExpr)
-- Deal with making the continuation duplicable if necessary,
-- and with the no-case-of-case situation.
wrapJoinCont env cont thing_inside
  | contIsStop cont        -- Common case; no need for fancy footwork
  = thing_inside env cont

  | not (seCaseCase env)
    -- See Note [Join points with -fno-case-of-case]
  = do { (floats1, expr1) <- thing_inside env (mkBoringStop (contHoleType cont))
       ; let (floats2, expr2) = wrapJoinFloatsX floats1 expr1
       ; (floats3, expr3) <- rebuild (env `setInScopeFromF` floats2) expr2 cont
       ; return (floats2 `addFloats` floats3, expr3) }

  | otherwise
    -- Normal case; see Note [Join points and case-of-case]
  = do { (floats1, cont')  <- mkDupableCont env cont
       ; (floats2, result) <- thing_inside (env `setInScopeFromF` floats1) cont'
       ; return (floats1 `addFloats` floats2, result) }


--------------------
trimJoinCont :: Id         -- Used only in error message
             -> JoinPointHood
             -> SimplCont -> SimplCont
-- Drop outer context from join point invocation (jump)
-- See Note [Join points and case-of-case]

trimJoinCont _ NotJoinPoint cont
  = cont -- Not a jump
trimJoinCont var (JoinPoint arity) cont
  = trim arity cont
  where
    trim 0 cont@(Stop {})
      = cont
    trim 0 cont
      = mkBoringStop (contResultType cont)
    trim n cont@(ApplyToVal { sc_cont = k })
      = cont { sc_cont = trim (n-1) k }
    trim n cont@(ApplyToTy { sc_cont = k })
      = cont { sc_cont = trim (n-1) k } -- join arity counts types!
    trim _ cont
      = pprPanic "completeCall" $ ppr var $$ ppr cont


{- Note [Join points and case-of-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we perform the case-of-case transform (or otherwise push continuations
inward), we want to treat join points specially. Since they're always
tail-called and we want to maintain this invariant, we can do this (for any
evaluation context E):

  E[join j = e
    in case ... of
         A -> jump j 1
         B -> jump j 2
         C -> f 3]

    -->

  join j = E[e]
  in case ... of
       A -> jump j 1
       B -> jump j 2
       C -> E[f 3]

As is evident from the example, there are two components to this behavior:

  1. When entering the RHS of a join point, copy the context inside.
  2. When a join point is invoked, discard the outer context.

We need to be very careful here to remain consistent---neither part is
optional!

We need do make the continuation E duplicable (since we are duplicating it)
with mkDupableCont.


Note [Join points with -fno-case-of-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Supose case-of-case is switched off, and we are simplifying

    case (join j x = <j-rhs> in
          case y of
             A -> j 1
             B -> j 2
             C -> e) of <outer-alts>

Usually, we'd push the outer continuation (case . of <outer-alts>) into
both the RHS and the body of the join point j.  But since we aren't doing
case-of-case we may then end up with this totally bogus result

    join x = case <j-rhs> of <outer-alts> in
    case (case y of
             A -> j 1
             B -> j 2
             C -> e) of <outer-alts>

This would be OK in the language of the paper, but not in GHC: j is no longer
a join point.  We can only do the "push continuation into the RHS of the
join point j" if we also push the continuation right down to the /jumps/ to
j, so that it can evaporate there.  If we are doing case-of-case, we'll get to

    join x = case <j-rhs> of <outer-alts> in
    case y of
      A -> j 1
      B -> j 2
      C -> case e of <outer-alts>

which is great.

Bottom line: if case-of-case is off, we must stop pushing the continuation
inwards altogether at any join point.  Instead simplify the (join ... in ...)
with a Stop continuation, and wrap the original continuation around the
outside.  Surprisingly tricky!


************************************************************************
*                                                                      *
                     Variables
*                                                                      *
************************************************************************

Note [zapSubstEnv]
~~~~~~~~~~~~~~~~~~
When simplifying something that has already been simplified, be sure to
zap the SubstEnv.  This is VITAL.  Consider
     let x = e in
     let y = \z -> ...x... in
     \ x -> ...y...

We'll clone the inner \x, adding x->x' in the id_subst Then when we
inline y, we must *not* replace x by x' in the inlined copy!!

Note [Fast path for data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For applications of a data constructor worker, the full glory of
rebuildCall is a waste of effort;
* They never inline, obviously
* They have no rewrite rules
* They are not strict (see Note [Data-con worker strictness]
  in GHC.Core.DataCon)
So it's fine to zoom straight to `rebuild` which just rebuilds the
call in a very straightforward way.

Some programs have a /lot/ of data constructors in the source program
(compiler/perf/T9961 is an example), so this fast path can be very
valuable.
-}

simplVar :: SimplEnv -> InVar -> SimplM OutExpr
-- Look up an InVar in the environment
simplVar env var
  -- Why $! ? See Note [Bangs in the Simplifier]
  | isTyVar var = return $! Type $! (substTyVar env var)
  | isCoVar var = return $! Coercion $! (substCoVar env var)
  | otherwise
  = case substId env var of
        ContEx tvs cvs ids e -> let env' = setSubstEnv env tvs cvs ids
                                in simplExpr env' e
        DoneId var1          -> return (Var var1)
        DoneEx e _           -> return e

simplIdF :: SimplEnv -> InId -> SimplCont -> SimplM (SimplFloats, OutExpr)
simplIdF env var cont
  | isDataConWorkId var         -- See Note [Fast path for data constructors]
  = rebuild env (Var var) cont
  | otherwise
  = case substId env var of
      ContEx tvs cvs ids e -> simplExprF env' e cont
        -- Don't trimJoinCont; haven't already simplified e,
        -- so the cont is not embodied in e
        where
          env' = setSubstEnv env tvs cvs ids

      DoneId var1 ->
        do { rule_base <- getSimplRules
           ; let cont' = trimJoinCont var1 (idJoinPointHood var1) cont
                 info  = mkArgInfo env rule_base var1 cont'
           ; rebuildCall env info cont' }

      DoneEx e mb_join -> simplExprF env' e cont'
        where
          cont' = trimJoinCont var mb_join cont
          env'  = zapSubstEnv env  -- See Note [zapSubstEnv]

---------------------------------------------------------
--      Dealing with a call site

rebuildCall :: SimplEnv -> ArgInfo -> SimplCont
            -> SimplM (SimplFloats, OutExpr)

---------- Bottoming applications --------------
rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args, ai_dmds = [] }) cont
  -- When we run out of strictness args, it means
  -- that the call is definitely bottom; see GHC.Core.Opt.Simplify.Utils.mkArgInfo
  -- Then we want to discard the entire strict continuation.  E.g.
  --    * case (error "hello") of { ... }
  --    * (error "Hello") arg
  --    * f (error "Hello") where f is strict
  --    etc
  -- Then, especially in the first of these cases, we'd like to discard
  -- the continuation, leaving just the bottoming expression.  But the
  -- type might not be right, so we may have to add a coerce.
  | not (contIsTrivial cont)     -- Only do this if there is a non-trivial
                                 -- continuation to discard, else we do it
                                 -- again and again!
  = seqType cont_ty `seq`        -- See Note [Avoiding space leaks in OutType]
    return (emptyFloats env, castBottomExpr res cont_ty)
  where
    res     = argInfoExpr fun rev_args
    cont_ty = contResultType cont

---------- Try inlining, if ai_rewrite = TryInlining --------
-- In the TryInlining case we try inlining immediately, before simplifying
-- any (more) arguments. Why?  See Note [Rewrite rules and inlining].
--
-- If there are rewrite rules we'll skip this case until we have
-- simplified enough args to satisfy nr_wanted==0 in the TryRules case below
-- Then we'll try the rules, and if that fails, we'll do TryInlining
rebuildCall env info@(ArgInfo { ai_fun = fun, ai_args = rev_args
                              , ai_rewrite = TryInlining }) cont
  = do { logger <- getLogger
       ; let full_cont = pushSimplifiedRevArgs env rev_args cont
       ; mb_inline <- tryInlining env logger fun full_cont
       ; case mb_inline of
            Just expr -> do { checkedTick (UnfoldingDone fun)
                            ; let env1 = zapSubstEnv env
                            ; simplExprF env1 expr full_cont }
            Nothing -> rebuildCall env (info { ai_rewrite = TryNothing }) cont
       }

---------- Try rewrite RULES, if ai_rewrite = TryRules --------------
-- See Note [Rewrite rules and inlining]
-- See also Note [Trying rewrite rules]
rebuildCall env info@(ArgInfo { ai_fun = fun, ai_args = rev_args
                              , ai_rewrite = TryRules nr_wanted rules }) cont
  | nr_wanted == 0 || no_more_args
  = -- We've accumulated a simplified call in <fun,rev_args>
    -- so try rewrite rules; see Note [RULES apply to simplified arguments]
    -- See also Note [Rules for recursive functions]
    do { mb_match <- tryRules env rules fun (reverse rev_args) cont
       ; case mb_match of
             Just (env', rhs, cont') -> simplExprF env' rhs cont'
             Nothing -> rebuildCall env (info { ai_rewrite = TryInlining }) cont }
  where
    -- If we have run out of arguments, just try the rules; there might
    -- be some with lower arity.  Casts get in the way -- they aren't
    -- allowed on rule LHSs
    no_more_args = case cont of
                      ApplyToTy  {} -> False
                      ApplyToVal {} -> False
                      _             -> True

---------- Simplify type applications and casts --------------
rebuildCall env info (CastIt co cont)
  = rebuildCall env (addCastTo info co) cont

rebuildCall env info (ApplyToTy { sc_arg_ty = arg_ty, sc_hole_ty = hole_ty, sc_cont = cont })
  = rebuildCall env (addTyArgTo info arg_ty hole_ty) cont

---------- The runRW# rule. Do this after absorbing all arguments ------
-- See Note [Simplification of runRW#] in GHC.CoreToSTG.Prep.
--
-- runRW# :: forall (r :: RuntimeRep) (o :: TYPE r). (State# RealWorld -> o) -> o
-- K[ runRW# rr ty body ]   -->   runRW rr' ty' (\s. K[ body s ])
rebuildCall env (ArgInfo { ai_fun = fun_id, ai_args = rev_args })
            (ApplyToVal { sc_arg = arg, sc_env = arg_se
                        , sc_cont = cont, sc_hole_ty = fun_ty })
  | fun_id `hasKey` runRWKey
  , [ TyArg { as_arg_ty = hole_ty }, TyArg {} ] <- rev_args
  -- Do this even if (contIsStop cont), or if seCaseCase is off.
  -- See Note [No eta-expansion in runRW#]
  = do { let arg_env = arg_se `setInScopeFromE` env

             overall_res_ty  = contResultType cont
             -- hole_ty is the type of the current runRW# application
             (outer_cont, new_runrw_res_ty, inner_cont)
                | seCaseCase env = (mkBoringStop overall_res_ty, overall_res_ty, cont)
                | otherwise      = (cont, hole_ty, mkBoringStop hole_ty)
                -- Only when case-of-case is on. See GHC.Driver.Config.Core.Opt.Simplify
                --    Note [Case-of-case and full laziness]

       -- If the argument is a literal lambda already, take a short cut
       -- This isn't just efficiency:
       --    * If we don't do this we get a beta-redex every time, so the
       --      simplifier keeps doing more iterations.
       --    * Even more important: see Note [No eta-expansion in runRW#]
       ; arg' <- case arg of
           Lam s body -> do { (env', s') <- simplBinder arg_env s
                            ; body' <- simplExprC env' body inner_cont
                            ; return (Lam s' body') }
                            -- Important: do not try to eta-expand this lambda
                            -- See Note [No eta-expansion in runRW#]

           _ -> do { s' <- newId (fsLit "s") ManyTy realWorldStatePrimTy
                   ; let (m,_,_) = splitFunTy fun_ty
                         env'  = arg_env `addNewInScopeIds` [s']
                         cont' = ApplyToVal { sc_dup = Simplified, sc_arg = Var s'
                                            , sc_env = env', sc_cont = inner_cont
                                            , sc_hole_ty = mkVisFunTy m realWorldStatePrimTy new_runrw_res_ty }
                                -- cont' applies to s', then K
                   ; body' <- simplExprC env' arg cont'
                   ; return (Lam s' body') }

       ; let rr'   = getRuntimeRep new_runrw_res_ty
             call' = mkApps (Var fun_id) [mkTyArg rr', mkTyArg new_runrw_res_ty, arg']
       ; rebuild env call' outer_cont }

---------- Simplify value arguments --------------------
rebuildCall env fun_info
            (ApplyToVal { sc_arg = arg, sc_env = arg_se
                        , sc_dup = dup_flag, sc_hole_ty = fun_ty
                        , sc_cont = cont })
  -- Argument is already simplified
  | isSimplified dup_flag     -- See Note [Avoid redundant simplification]
  = rebuildCall env (addValArgTo fun_info arg fun_ty) cont

  -- Strict arguments
  | isStrictArgInfo fun_info
  , seCaseCase env    -- Only when case-of-case is on. See GHC.Driver.Config.Core.Opt.Simplify
                      --    Note [Case-of-case and full laziness]
  = -- pprTrace "Strict Arg" (ppr arg $$ ppr (seIdSubst env) $$ ppr (seInScope env)) $
    simplExprF (arg_se `setInScopeFromE` env) arg
               (StrictArg { sc_fun = fun_info, sc_fun_ty = fun_ty
                          , sc_dup = Simplified
                          , sc_cont = cont })
                -- Note [Shadowing in the Simplifier]

  -- Lazy arguments
  | otherwise
        -- DO NOT float anything outside, hence simplExprC
        -- There is no benefit (unlike in a let-binding), and we'd
        -- have to be very careful about bogus strictness through
        -- floating a demanded let.
  = do  { (_, _, arg') <- simplLazyArg env dup_flag fun_ty (Just fun_info) arg_se arg
        ; rebuildCall env (addValArgTo fun_info  arg' fun_ty) cont }

---------- No further useful info, revert to generic rebuild ------------
rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args }) cont
  = rebuild env (argInfoExpr fun rev_args) cont

-----------------------------------
tryInlining :: SimplEnv -> Logger -> OutId -> SimplCont -> SimplM (Maybe OutExpr)
tryInlining env logger var cont
  | Just expr <- callSiteInline logger uf_opts case_depth var active_unf
                                lone_variable arg_infos interesting_cont
  = do { dump_inline expr cont
       ; return (Just expr) }

  | otherwise
  = return Nothing

  where
    uf_opts    = seUnfoldingOpts env
    case_depth = seCaseDepth env
    (lone_variable, arg_infos, call_cont) = contArgs cont
    interesting_cont = interestingCallContext env call_cont
    active_unf       = activeUnfolding (seMode env) var

    log_inlining doc
      = liftIO $ logDumpFile logger (mkDumpStyle alwaysQualify)
           Opt_D_dump_inlinings
           "" FormatText doc

    dump_inline unfolding cont
      | not (logHasDumpFlag logger Opt_D_dump_inlinings) = return ()
      | not (logHasDumpFlag logger Opt_D_verbose_core2core)
      = when (isExternalName (idName var)) $
            log_inlining $
                sep [text "Inlining done:", nest 4 (ppr var)]
      | otherwise
      = log_inlining $
           sep [text "Inlining done: " <> ppr var,
                nest 4 (vcat [text "Inlined fn: " <+> nest 2 (ppr unfolding),
                              text "Cont:  " <+> ppr cont])]


{- Note [Trying rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider an application (f e1 e2 e3) where the e1,e2,e3 are not yet
simplified.  We want to simplify enough arguments to allow the rules
to apply, but it's more efficient to avoid simplifying e2,e3 if e1 alone
is sufficient.  Example: class ops
   (+) dNumInt e2 e3
If we rewrite ((+) dNumInt) to plusInt, we can take advantage of the
latter's strictness when simplifying e2, e3.  Moreover, suppose we have
  RULE  f Int = \x. x True

Then given (f Int e1) we rewrite to
   (\x. x True) e1
without simplifying e1.  Now we can inline x into its unique call site,
and absorb the True into it all in the same pass.  If we simplified
e1 first, we couldn't do that; see Note [Avoiding exponential behaviour].

So we try to apply rules if either
  (a) no_more_args: we've run out of argument that the rules can "see"
  (b) nr_wanted: none of the rules wants any more arguments


Note [RULES apply to simplified arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very desirable to try RULES once the arguments have been simplified, because
doing so ensures that rule cascades work in one pass.  Consider
   {-# RULES g (h x) = k x
             f (k x) = x #-}
   ...f (g (h x))...
Then we want to rewrite (g (h x)) to (k x) and only then try f's rules. If
we match f's rules against the un-simplified RHS, it won't match.  This
makes a particularly big difference when superclass selectors are involved:
        op ($p1 ($p2 (df d)))
We want all this to unravel in one sweep.

Note [Rewrite rules and inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we try to arrange that inlining is disabled (via a pragma) if
a rewrite rule should apply, so that the rule has a decent chance to fire
before we inline the function.

But it turns out that (especially when type-class specialisation or
SpecConstr is involved) it is very helpful for the the rewrite rule to
"win" over inlining when both are active at once: see #21851, #22097.

The simplifier arranges to do this, as follows. In effect, the ai_rewrite
field of the ArgInfo record is the state of a little state-machine:

* mkArgInfo sets the ai_rewrite field to TryRules if there are any rewrite
  rules avaialable for that function.

* rebuildCall simplifies arguments until enough are simplified to match the
  rule with greatest arity.  See Note [RULES apply to simplified arguments]
  and the first field of `TryRules`.

  But no more! As soon as we have simplified enough arguments to satisfy the
  maximum-arity rules, we try the rules; see Note [Trying rewrite rules].

* Once we have tried rules (or immediately if there are no rules) set
  ai_rewrite to TryInlining, and the Simplifier will try to inline the
  function.  We want to try this immediately (before simplifying any (more)
  arguments). Why? Consider
      f BIG      where   f = \x{OneOcc}. ...x...
  If we inline `f` before simplifying `BIG` well use preInlineUnconditionally,
  and we'll simplify BIG once, at x's occurrence, rather than twice.

* GHC.Core.Opt.Simplify.Utils. mkRewriteCall: if there are no rules, and no
  unfolding, we can skip both TryRules and TryInlining, which saves work.

Note [Avoid redundant simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because RULES apply to simplified arguments, there's a danger of repeatedly
simplifying already-simplified arguments.  An important example is that of
        (>>=) d e1 e2
Here e1, e2 are simplified before the rule is applied, but don't really
participate in the rule firing. So we mark them as Simplified to avoid
re-simplifying them.

Note [Shadowing in the Simplifier]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This part of the simplifier may return an expression that has shadowing.
(See Note [Shadowing in Core] in GHC.Core.hs.) Consider
        f (...(\a -> e)...) (case y of (a,b) -> e')
where f is strict in its second arg
If we simplify the innermost one first we get (...(\a -> e)...)
Simplifying the second arg makes us float the case out, so we end up with
        case y of (a,b) -> f (...(\a -> e)...) e'
So the output does not have the no-shadowing invariant.  However, there is
no danger of getting name-capture, because when the first arg was simplified
we used an in-scope set that at least mentioned all the variables free in its
static environment, and that is enough.

We can't just do innermost first, or we'd end up with a dual problem:
        case x of (a,b) -> f e (...(\a -> e')...)

I spent hours trying to recover the no-shadowing invariant, but I just could
not think of an elegant way to do it.  The simplifier is already knee-deep in
continuations.  We have to keep the right in-scope set around; AND we have
to get the effect that finding (error "foo") in a strict arg position will
discard the entire application and replace it with (error "foo").  Getting
all this at once is TOO HARD!

See also Note [Shadowing in prepareAlts] in GHC.Core.Opt.Simplify.Utils.

Note [No eta-expansion in runRW#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see `runRW# (\s. blah)` we must not attempt to eta-expand that
lambda.  Why not?  Because
* `blah` can mention join points bound outside the runRW#
* eta-expansion uses arityType, and
* `arityType` cannot cope with free join Ids:

So the simplifier spots the literal lambda, and simplifies inside it.
It's a very special lambda, because it is the one the OccAnal spots and
allows join points bound /outside/ to be called /inside/.

See Note [No free join points in arityType] in GHC.Core.Opt.Arity

************************************************************************
*                                                                      *
                Rewrite rules
*                                                                      *
************************************************************************
-}

tryRules :: SimplEnv -> [CoreRule]
         -> Id
         -> [ArgSpec]   -- In /normal, forward/ order
         -> SimplCont
         -> SimplM (Maybe (SimplEnv, CoreExpr, SimplCont))

tryRules env rules fn args call_cont
  | null rules
  = return Nothing

  | Just (rule, rule_rhs) <- lookupRule ropts (getUnfoldingInRuleMatch env)
                                        (activeRule (seMode env)) fn
                                        (argInfoAppArgs args) rules
  -- Fire a rule for the function
  = do { logger <- getLogger
       ; checkedTick (RuleFired (ruleName rule))
       ; let cont' = pushSimplifiedArgs zapped_env
                                        (drop (ruleArity rule) args)
                                        call_cont
                     -- (ruleArity rule) says how
                     -- many args the rule consumed

             occ_anald_rhs = occurAnalyseExpr rule_rhs
                 -- See Note [Occurrence-analyse after rule firing]
       ; dump logger rule rule_rhs
       ; return (Just (zapped_env, occ_anald_rhs, cont')) }
            -- The occ_anald_rhs and cont' are all Out things
            -- hence zapping the environment

  | otherwise  -- No rule fires
  = do { logger <- getLogger
       ; nodump logger  -- This ensures that an empty file is written
       ; return Nothing }

  where
    ropts      = seRuleOpts env
    zapped_env = zapSubstEnv env  -- See Note [zapSubstEnv]

    printRuleModule rule
      = parens (maybe (text "BUILTIN")
                      (pprModuleName . moduleName)
                      (ruleModule rule))

    dump logger rule rule_rhs
      | logHasDumpFlag logger Opt_D_dump_rule_rewrites
      = log_rule Opt_D_dump_rule_rewrites "Rule fired" $ vcat
          [ text "Rule:" <+> ftext (ruleName rule)
          , text "Module:" <+>  printRuleModule rule
          , text "Before:" <+> hang (ppr fn) 2 (sep (map ppr args))
          , text "After: " <+> hang (pprCoreExpr rule_rhs) 2
                               (sep $ map ppr $ drop (ruleArity rule) args)
          , text "Cont:  " <+> ppr call_cont ]

      | logHasDumpFlag logger Opt_D_dump_rule_firings
      = log_rule Opt_D_dump_rule_firings "Rule fired:" $
          ftext (ruleName rule)
            <+> printRuleModule rule

      | otherwise
      = return ()

    nodump logger
      | logHasDumpFlag logger Opt_D_dump_rule_rewrites
      = liftIO $
          touchDumpFile logger Opt_D_dump_rule_rewrites

      | logHasDumpFlag logger Opt_D_dump_rule_firings
      = liftIO $
          touchDumpFile logger Opt_D_dump_rule_firings

      | otherwise
      = return ()

    log_rule flag hdr details
      = do
      { logger <- getLogger
      ; liftIO $ logDumpFile logger (mkDumpStyle alwaysQualify) flag "" FormatText
               $ sep [text hdr, nest 4 details]
      }

trySeqRules :: SimplEnv
            -> OutExpr -> InExpr   -- Scrutinee and RHS
            -> SimplCont
            -> SimplM (Maybe (SimplEnv, CoreExpr, SimplCont))
-- See Note [User-defined RULES for seq]
trySeqRules in_env scrut rhs cont
  = do { rule_base <- getSimplRules
       ; tryRules in_env (getRules rule_base seqId) seqId out_args rule_cont }
  where
    no_cast_scrut = drop_casts scrut
    scrut_ty  = exprType no_cast_scrut
    seq_id_ty = idType seqId                    -- forall r a (b::TYPE r). a -> b -> b
    res1_ty   = piResultTy seq_id_ty rhs_rep    -- forall a (b::TYPE rhs_rep). a -> b -> b
    res2_ty   = piResultTy res1_ty   scrut_ty   -- forall (b::TYPE rhs_rep). scrut_ty -> b -> b
    res3_ty   = piResultTy res2_ty   rhs_ty     -- scrut_ty -> rhs_ty -> rhs_ty
    res4_ty   = funResultTy res3_ty             -- rhs_ty -> rhs_ty
    rhs_ty    = substTy in_env (exprType rhs)
    rhs_rep   = getRuntimeRep rhs_ty
    out_args  = [ TyArg { as_arg_ty  = rhs_rep
                        , as_hole_ty = seq_id_ty }
                , TyArg { as_arg_ty  = scrut_ty
                        , as_hole_ty = res1_ty }
                , TyArg { as_arg_ty  = rhs_ty
                        , as_hole_ty = res2_ty }
                , ValArg { as_arg = no_cast_scrut
                         , as_dmd = seqDmd
                         , as_hole_ty = res3_ty } ]
    rule_cont = ApplyToVal { sc_dup = NoDup, sc_arg = rhs
                           , sc_env = in_env, sc_cont = cont
                           , sc_hole_ty = res4_ty }

    -- Lazily evaluated, so we don't do most of this

    drop_casts (Cast e _) = drop_casts e
    drop_casts e          = e

{- Note [User-defined RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given
   case (scrut |> co) of _ -> rhs
look for rules that match the expression
   seq @t1 @t2 scrut
where scrut :: t1
      rhs   :: t2

If you find a match, rewrite it, and apply to 'rhs'.

Notice that we can simply drop casts on the fly here, which
makes it more likely that a rule will match.

See Note [User-defined RULES for seq] in GHC.Types.Id.Make.

Note [Occurrence-analyse after rule firing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After firing a rule, we occurrence-analyse the instantiated RHS before
simplifying it.  Usually this doesn't make much difference, but it can
be huge.  Here's an example (simplCore/should_compile/T7785)

  map f (map f (map f xs)

= -- Use build/fold form of map, twice
  map f (build (\cn. foldr (mapFB c f) n
                           (build (\cn. foldr (mapFB c f) n xs))))

= -- Apply fold/build rule
  map f (build (\cn. (\cn. foldr (mapFB c f) n xs) (mapFB c f) n))

= -- Beta-reduce
  -- Alas we have no occurrence-analysed, so we don't know
  -- that c is used exactly once
  map f (build (\cn. let c1 = mapFB c f in
                     foldr (mapFB c1 f) n xs))

= -- Use mapFB rule:   mapFB (mapFB c f) g = mapFB c (f.g)
  -- We can do this because (mapFB c n) is a PAP and hence expandable
  map f (build (\cn. let c1 = mapFB c n in
                     foldr (mapFB c (f.f)) n x))

This is not too bad.  But now do the same with the outer map, and
we get another use of mapFB, and t can interact with /both/ remaining
mapFB calls in the above expression.  This is stupid because actually
that 'c1' binding is dead.  The outer map introduces another c2. If
there is a deep stack of maps we get lots of dead bindings, and lots
of redundant work as we repeatedly simplify the result of firing rules.

The easy thing to do is simply to occurrence analyse the result of
the rule firing.  Note that this occ-anals not only the RHS of the
rule, but also the function arguments, which by now are OutExprs.
E.g.
      RULE f (g x) = x+1

Call   f (g BIG)  -->   (\x. x+1) BIG

The rule binders are lambda-bound and applied to the OutExpr arguments
(here BIG) which lack all internal occurrence info.

Is this inefficient?  Not really: we are about to walk over the result
of the rule firing to simplify it, so occurrence analysis is at most
a constant factor.

Note, however, that the rule RHS is /already/ occ-analysed; see
Note [OccInfo in unfoldings and rules] in GHC.Core.  There is something
unsatisfactory about doing it twice; but the rule RHS is usually very
small, and this is simple.

Note [Rules for recursive functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think that we shouldn't apply rules for a loop breaker:
doing so might give rise to an infinite loop, because a RULE is
rather like an extra equation for the function:
     RULE:           f (g x) y = x+y
     Eqn:            f a     y = a-y

But it's too drastic to disable rules for loop breakers.
Even the foldr/build rule would be disabled, because foldr
is recursive, and hence a loop breaker:
     foldr k z (build g) = g k z
So it's up to the programmer: rules can cause divergence


************************************************************************
*                                                                      *
                Rebuilding a case expression
*                                                                      *
************************************************************************

Note [Case elimination]
~~~~~~~~~~~~~~~~~~~~~~~
The case-elimination transformation discards redundant case expressions.
Start with a simple situation:

        case x# of      ===>   let y# = x# in e
          y# -> e

(when x#, y# are of primitive type, of course).  We can't (in general)
do this for algebraic cases, because we might turn bottom into
non-bottom!

The code in GHC.Core.Opt.Simplify.Utils.prepareAlts has the effect of generalise
this idea to look for a case where we're scrutinising a variable, and we know
that only the default case can match.  For example:

        case x of
          0#      -> ...
          DEFAULT -> ...(case x of
                         0#      -> ...
                         DEFAULT -> ...) ...

Here the inner case is first trimmed to have only one alternative, the
DEFAULT, after which it's an instance of the previous case.  This
really only shows up in eliminating error-checking code.

Note that GHC.Core.Opt.Simplify.Utils.mkCase combines identical RHSs.  So

        case e of       ===> case e of DEFAULT -> r
           True  -> r
           False -> r

Now again the case may be eliminated by the CaseElim transformation.
This includes things like (==# a# b#)::Bool so that we simplify
      case ==# a# b# of { True -> x; False -> x }
to just
      x
This particular example shows up in default methods for
comparison operations (e.g. in (>=) for Int.Int32)

Note [Case to let transformation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a case over a lifted type has a single alternative, and is being
used as a strict 'let' (all isDeadBinder bndrs), we may want to do
this transformation:

    case e of r       ===>   let r = e in ...r...
      _ -> ...r...

We treat the unlifted and lifted cases separately:

* Unlifted case: 'e' satisfies exprOkForSpeculation
  (ok-for-spec is needed to satisfy the let-can-float invariant).
  This turns     case a +# b of r -> ...r...
  into           let r = a +# b in ...r...
  and thence     .....(a +# b)....

  However, if we have
      case indexArray# a i of r -> ...r...
  we might like to do the same, and inline the (indexArray# a i).
  But indexArray# is not okForSpeculation, so we don't build a let
  in rebuildCase (lest it get floated *out*), so the inlining doesn't
  happen either.  Annoying.

* Lifted case: we need to be sure that the expression is already
  evaluated (exprIsHNF).  If it's not already evaluated
      - we risk losing exceptions, divergence or
        user-specified thunk-forcing
      - even if 'e' is guaranteed to converge, we don't want to
        create a thunk (call by need) instead of evaluating it
        right away (call by value)

  However, we can turn the case into a /strict/ let if the 'r' is
  used strictly in the body.  Then we won't lose divergence; and
  we won't build a thunk because the let is strict.
  See also Note [Case-to-let for strictly-used binders]

Note [Case-to-let for strictly-used binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have this:
   case <scrut> of r { _ -> ..r.. }
where 'r' is used strictly in (..r..), we /could/ safely transform to
   let r = <scrut> in ...r...
As a special case,  we have a plain `seq` like
   case r of r1 { _ -> ...r1... }
where `r` is used strictly, we /could/ simply drop the `case` to get
   ...r....

HOWEVER, there are some serious downsides to this transformation, so
GHC doesn't do it any longer (#24251):

* Suppose the Simplifier sees
     case x of y* { __DEFAULT ->
     let z = case y of { __DEFAULT -> expr } in
     z+1 }
  The "y*" means "y is used strictly in its scope.  Now we may:
   - Eliminate the inner case because `y` is evaluated.
  Now the demand-info on `y` is not right, because `y` is no longer used
  strictly in its scope.  But it is hard to spot that without doing a new
  demand analysis.  So there is a danger that we will subsequently:
   - Eliminate the outer case because `y` is used strictly
  Yikes!  We can't eliminate both!

* It introduces space leaks (#24251).  Consider
      go 0 where go x = x `seq` go (x + 1)
  It is an infinite loop, true, but it should not leak space. Yet if we drop
  the `seq`, it will.  Another great example is #21741.

* Dropping the outer `case can change the error behaviour.  For example,
  we might transform
       case x of { _ -> error "bad" }    -->     error "bad"
  which is might be puzzling if 'x' currently lambda-bound, but later gets
  let-bound to (error "good").  Tht is OK accoring to the paper "A semantics for
  imprecise exceptions", but see #8900 for an example where the loss of this
  transformation bit us in practice.

* If we have (case e of x -> f x), where `f` is strict, then it looks as if `x`
  is strictly used, and we could soundly transform to
     let x = e in f x
  But if f's strictness info got worse (which can happen in in obscure cases;
  see #21392) then we might have turned a non-thunk into a thunk!  Bad.

Lacking this "drop-strictly-used-seq" transformation means we can end up with
some redundant-looking evals.  For example, consider
    f x y = case x of DEFAULT ->    -- A redundant-looking eval
            case y of
              True  -> case x of { Nothing -> False; Just z  -> z }
              False -> case x of { Nothing -> True;  Just z  -> z }
That outer eval will be retained right through to code generation.  But,
perhaps surprisingly, that is probably a /good/ thing:

   Key point: those inner (case x) expressions will be compiled a simple 'if',
   because the code generator can see that `x` is, at those points, evaluated
   and properly tagged.

If we dropped the outer eval, both the inner (case x) expressions would need to
do a proper eval, pushing a return address, with an info table. See the example
in #15631 where, in the Description, the (case ys) will be a simple multi-way
jump.

In fact (#24251), when I stopped GHC implementing the drop-strictly-used-seqs
transformation, binary sizes fell by 1%, and a few programs actually allocated
less and ran faster.  A case in point is nofib/imaginary/digits-of-e2. (I'm not
sure exactly why it improves so much, though.)

Slightly related: Note [Empty case alternatives] in GHC.Core.

Historical notes:

There have been various earlier versions of this patch:

* By Sept 18 the code looked like this:
     || scrut_is_demanded_var scrut

    scrut_is_demanded_var :: CoreExpr -> Bool
    scrut_is_demanded_var (Cast s _) = scrut_is_demanded_var s
    scrut_is_demanded_var (Var _)    = isStrUsedDmd (idDemandInfo case_bndr)
    scrut_is_demanded_var _          = False

  This only fired if the scrutinee was a /variable/, which seems
  an unnecessary restriction. So in #15631 I relaxed it to allow
  arbitrary scrutinees.  Less code, less to explain -- but the change
  had 0.00% effect on nofib.

* Previously, in Jan 13 the code looked like this:
     || case_bndr_evald_next rhs

    case_bndr_evald_next :: CoreExpr -> Bool
      -- See Note [Case binder next]
    case_bndr_evald_next (Var v)         = v == case_bndr
    case_bndr_evald_next (Cast e _)      = case_bndr_evald_next e
    case_bndr_evald_next (App e _)       = case_bndr_evald_next e
    case_bndr_evald_next (Case e _ _ _)  = case_bndr_evald_next e
    case_bndr_evald_next _               = False

  This patch was part of fixing #7542. See also
  Note [Eta reduction soundness], criterion (E) in GHC.Core.Utils.)


Further notes about case elimination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:       test :: Integer -> IO ()
                test = print

Turns out that this compiles to:
    Print.test
      = \ eta :: Integer
          eta1 :: Void# ->
          case PrelNum.< eta PrelNum.zeroInteger of wild { __DEFAULT ->
          case hPutStr stdout
                 (PrelNum.jtos eta ($w[] @ Char))
                 eta1
          of wild1 { (# new_s, a4 #) -> PrelIO.lvl23 new_s  }}

Notice the strange '<' which has no effect at all. This is a funny one.
It started like this:

f x y = if x < 0 then jtos x
          else if y==0 then "" else jtos x

At a particular call site we have (f v 1).  So we inline to get

        if v < 0 then jtos x
        else if 1==0 then "" else jtos x

Now simplify the 1==0 conditional:

        if v<0 then jtos v else jtos v

Now common-up the two branches of the case:

        case (v<0) of DEFAULT -> jtos v

Why don't we drop the case?  Because it's strict in v.  It's technically
wrong to drop even unnecessary evaluations, and in practice they
may be a result of 'seq' so we *definitely* don't want to drop those.
I don't really know how to improve this situation.


Note [FloatBinds from constructor wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have FloatBinds coming from the constructor wrapper
(as in Note [exprIsConApp_maybe on data constructors with wrappers]),
we cannot float past them. We'd need to float the FloatBind
together with the simplify floats, unfortunately the
simplifier doesn't have case-floats. The simplest thing we can
do is to wrap all the floats here. The next iteration of the
simplifier will take care of all these cases and lets.

Given data T = MkT !Bool, this allows us to simplify
case $WMkT b of { MkT x -> f x }
to
case b of { b' -> f b' }.

We could try and be more clever (like maybe wfloats only contain
let binders, so we could float them). But the need for the
extra complication is not clear.

Note [Do not duplicate constructor applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#20125)
   let x = (a,b)
   in ...(case x of x' -> blah)...x...x...

We want that `case` to vanish (since `x` is bound to a data con) leaving
   let x = (a,b)
   in ...(let x'=x in blah)...x..x...

In rebuildCase, `exprIsConApp_maybe` will succeed on the scrutinee `x`,
since is bound to (a,b).  But in eliminating the case, if the scrutinee
is trivial, we want to bind the case-binder to the scrutinee, /not/ to
the constructor application.  Hence the case_bndr_rhs in rebuildCase.

This applies equally to a non-DEFAULT case alternative, say
   let x = (a,b) in ...(case x of x' { (p,q) -> blah })...
This variant is handled by bind_case_bndr in knownCon.

We want to bind x' to x, and not to a duplicated (a,b)).
-}

---------------------------------------------------------
--      Eliminate the case if possible

rebuildCase, reallyRebuildCase
   :: SimplEnv
   -> OutExpr          -- Scrutinee
   -> InId             -- Case binder
   -> [InAlt]          -- Alternatives (increasing order)
   -> SimplCont
   -> SimplM (SimplFloats, OutExpr)

--------------------------------------------------
--      1. Eliminate the case if there's a known constructor
--------------------------------------------------

rebuildCase env scrut case_bndr alts cont
  | Lit lit <- scrut    -- No need for same treatment as constructors
                        -- because literals are inlined more vigorously
  , not (litIsLifted lit)
  = do  { tick (KnownBranch case_bndr)
        ; case findAlt (LitAlt lit) alts of
            Nothing             -> missingAlt env case_bndr alts cont
            Just (Alt _ bs rhs) -> simple_rhs env [] scrut bs rhs }

  | Just (in_scope', wfloats, con, ty_args, other_args)
      <- exprIsConApp_maybe (getUnfoldingInRuleMatch env) scrut
        -- Works when the scrutinee is a variable with a known unfolding
        -- as well as when it's an explicit constructor application
  , let env0 = setInScopeSet env in_scope'
  = do  { tick (KnownBranch case_bndr)
        ; let scaled_wfloats = map scale_float wfloats
              -- case_bndr_unf: see Note [Do not duplicate constructor applications]
              case_bndr_rhs | exprIsTrivial scrut = scrut
                            | otherwise           = con_app
              con_app = Var (dataConWorkId con) `mkTyApps` ty_args
                                                `mkApps`   other_args
        ; case findAlt (DataAlt con) alts of
            Nothing                   -> missingAlt env0 case_bndr alts cont
            Just (Alt DEFAULT bs rhs) -> simple_rhs env0 scaled_wfloats case_bndr_rhs bs rhs
            Just (Alt _       bs rhs) -> knownCon env0 scrut scaled_wfloats con ty_args
                                                  other_args case_bndr bs rhs cont
        }
  where
    simple_rhs env wfloats case_bndr_rhs bs rhs =
      assert (null bs) $
      do { (floats1, env') <- simplAuxBind env case_bndr case_bndr_rhs
             -- scrut is a constructor application,
             -- hence satisfies let-can-float invariant
         ; (floats2, expr') <- simplExprF env' rhs cont
         ; case wfloats of
             [] -> return (floats1 `addFloats` floats2, expr')
             _ -> return
               -- See Note [FloatBinds from constructor wrappers]
                   ( emptyFloats env,
                     GHC.Core.Make.wrapFloats wfloats $
                     wrapFloats (floats1 `addFloats` floats2) expr' )}

    -- This scales case floats by the multiplicity of the continuation hole (see
    -- Note [Scaling in case-of-case]).  Let floats are _not_ scaled, because
    -- they are aliases anyway.
    scale_float (GHC.Core.Make.FloatCase scrut case_bndr con vars) =
      let
        scale_id id = scaleVarBy holeScaling id
      in
      GHC.Core.Make.FloatCase scrut (scale_id case_bndr) con (map scale_id vars)
    scale_float f = f

    holeScaling = contHoleScaling cont `mkMultMul` idMult case_bndr
     -- We are in the following situation
     --   case[p] case[q] u of { D x -> C v } of { C x -> w }
     -- And we are producing case[??] u of { D x -> w[x\v]}
     --
     -- What should the multiplicity `??` be? In order to preserve the usage of
     -- variables in `u`, it needs to be `pq`.
     --
     -- As an illustration, consider the following
     --   case[Many] case[1] of { C x -> C x } of { C x -> (x, x) }
     -- Where C :: A %1 -> T is linear
     -- If we were to produce a case[1], like the inner case, we would get
     --   case[1] of { C x -> (x, x) }
     -- Which is ill-typed with respect to linearity. So it needs to be a
     -- case[Many].

--------------------------------------------------
--      2. Eliminate the case if scrutinee is evaluated
--------------------------------------------------

rebuildCase env scrut case_bndr alts@[Alt _ bndrs rhs] cont
  -- See if we can get rid of the case altogether
  -- See Note [Case elimination]
  -- mkCase made sure that if all the alternatives are equal,
  -- then there is now only one (DEFAULT) rhs

  -- 2a.  Dropping the case altogether, if
  --      a) it binds nothing (so it's really just a 'seq')
  --      b) evaluating the scrutinee has no side effects
  | is_plain_seq
  , exprOkToDiscard scrut
          -- The entire case is dead, so we can drop it
          -- if the scrutinee converges without having imperative
          -- side effects or raising a Haskell exception
   = simplExprF env rhs cont

  -- 2b.  Turn the case into a let, if
  --      a) it binds only the case-binder
  --      b) unlifted case: the scrutinee is ok-for-speculation
  --           lifted case: the scrutinee is in HNF (or will later be demanded)
  -- See Note [Case to let transformation]
  | all_dead_bndrs
  , doCaseToLet scrut case_bndr
  = do { tick (CaseElim case_bndr)
       ; (floats1, env')  <- simplAuxBind env case_bndr scrut
       ; (floats2, expr') <- simplExprF env' rhs cont
       ; return (floats1 `addFloats` floats2, expr') }

  -- 2c. Try the seq rules if
  --     a) it binds only the case binder
  --     b) a rule for seq applies
  -- See Note [User-defined RULES for seq] in GHC.Types.Id.Make
  | is_plain_seq
  = do { mb_rule <- trySeqRules env scrut rhs cont
       ; case mb_rule of
           Just (env', rule_rhs, cont') -> simplExprF env' rule_rhs cont'
           Nothing                      -> reallyRebuildCase env scrut case_bndr alts cont }

--------------------------------------------------
--      3. Primop-related case-rules
--------------------------------------------------

  |Just (scrut', case_bndr', alts') <- caseRules2 scrut case_bndr alts
  = reallyRebuildCase env scrut' case_bndr' alts' cont

  where
    all_dead_bndrs = all isDeadBinder bndrs       -- bndrs are [InId]
    is_plain_seq   = all_dead_bndrs && isDeadBinder case_bndr -- Evaluation *only* for effect

rebuildCase env scrut case_bndr alts cont
  = reallyRebuildCase env scrut case_bndr alts cont


doCaseToLet :: OutExpr          -- Scrutinee
            -> InId             -- Case binder
            -> Bool
-- The situation is         case scrut of b { DEFAULT -> body }
-- Can we transform thus?   let { b = scrut } in body
doCaseToLet scrut case_bndr
  | isTyCoVar case_bndr    -- Respect GHC.Core
  = isTyCoArg scrut        -- Note [Core type and coercion invariant]

  | isUnliftedType (exprType scrut)
    -- We can call isUnliftedType here: scrutinees always have a fixed RuntimeRep (see FRRCase).
    -- Note however that we must check 'scrut' (which is an 'OutExpr') and not 'case_bndr'
    -- (which is an 'InId'): see Note [Dark corner with representation polymorphism].
    -- Using `exprType` is typically cheap because `scrut` is typically a variable.
    -- We could instead use mightBeUnliftedType (idType case_bndr), but that hurts
    -- the brain more.  Consider that if this test ever turns out to be a perf
    -- problem (which seems unlikely).
  = exprOkForSpeculation scrut

  | otherwise  -- Scrut has a lifted type
  = exprIsHNF scrut
       --    || isStrUsedDmd (idDemandInfo case_bndr)
       -- We no longer look at the demand on the case binder
       -- See Note [Case-to-let for strictly-used binders]

--------------------------------------------------
--      3. Catch-all case
--------------------------------------------------

reallyRebuildCase env scrut case_bndr alts cont
  | not (seCaseCase env)    -- Only when case-of-case is on.
                            -- See GHC.Driver.Config.Core.Opt.Simplify
                            --    Note [Case-of-case and full laziness]
  = do { case_expr <- simplAlts env scrut case_bndr alts
                                (mkBoringStop (contHoleType cont))
       ; rebuild env case_expr cont }

  | otherwise
  = do { (floats, env', cont') <- mkDupableCaseCont env alts cont
       ; case_expr <- simplAlts env' scrut
                                (scaleIdBy holeScaling case_bndr)
                                (scaleAltsBy holeScaling alts)
                                cont'
       ; return (floats, case_expr) }
  where
    holeScaling = contHoleScaling cont
    -- Note [Scaling in case-of-case]

{-
simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

Historical note: we use to do the "case binder swap" in the Simplifier
so there were additional complications if the scrutinee was a variable.
Now the binder-swap stuff is done in the occurrence analyser; see
"GHC.Core.Opt.OccurAnal" Note [Binder swap].

Note [knownCon occ info]
~~~~~~~~~~~~~~~~~~~~~~~~
If the case binder is not dead, then neither are the pattern bound
variables:
        case <any> of x { (a,b) ->
        case x of { (p,q) -> p } }
Here (a,b) both look dead, but come alive after the inner case is eliminated.
The point is that we bring into the envt a binding
        let x = (a,b)
after the outer case, and that makes (a,b) alive.  At least we do unless
the case binder is guaranteed dead.

Note [Case alternative occ info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are simply reconstructing a case (the common case), we always
zap the occurrence info on the binders in the alternatives.  Even
if the case binder is dead, the scrutinee is usually a variable, and *that*
can bring the case-alternative binders back to life.
See Note [Add unfolding for scrutinee]

Note [Improving seq]
~~~~~~~~~~~~~~~~~~~
Consider
        type family F :: * -> *
        type instance F Int = Int

We'd like to transform
        case e of (x :: F Int) { DEFAULT -> rhs }
===>
        case e `cast` co of (x'::Int)
           I# x# -> let x = x' `cast` sym co
                    in rhs

so that 'rhs' can take advantage of the form of x'.  Notice that Note
[Case of cast] (in OccurAnal) may then apply to the result.

We'd also like to eliminate empty types (#13468). So if

    data Void
    type instance F Bool = Void

then we'd like to transform
        case (x :: F Bool) of { _ -> error "urk" }
===>
        case (x |> co) of (x' :: Void) of {}

Nota Bene: we used to have a built-in rule for 'seq' that dropped
casts, so that
    case (x |> co) of { _ -> blah }
dropped the cast; in order to improve the chances of trySeqRules
firing.  But that works in the /opposite/ direction to Note [Improving
seq] so there's a danger of flip/flopping.  Better to make trySeqRules
insensitive to the cast, which is now is.

The need for [Improving seq] showed up in Roman's experiments.  Example:
  foo :: F Int -> Int -> Int
  foo t n = t `seq` bar n
     where
       bar 0 = 0
       bar n = bar (n - case t of TI i -> i)
Here we'd like to avoid repeated evaluating t inside the loop, by
taking advantage of the `seq`.

At one point I did transformation in LiberateCase, but it's more
robust here.  (Otherwise, there's a danger that we'll simply drop the
'seq' altogether, before LiberateCase gets to see it.)

Note [Scaling in case-of-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When two cases commute, if done naively, the multiplicities will be wrong:

  case (case u of w[1] { (x[1], y[1]) } -> f x y) of w'[Many]
  { (z[Many], t[Many]) -> z
  }

The multiplicities here, are correct, but if I perform a case of case:

  case u of w[1]
  { (x[1], y[1]) -> case f x y of w'[Many] of { (z[Many], t[Many]) -> z }
  }

This is wrong! Using `f x y` inside a `case … of w'[Many]` means that `x` and
`y` must have multiplicities `Many` not `1`! The correct solution is to make
all the `1`-s be `Many`-s instead:

  case u of w[Many]
  { (x[Many], y[Many]) -> case f x y of w'[Many] of { (z[Many], t[Many]) -> z }
  }

In general, when commuting two cases, the rule has to be:

  case (case … of x[p] {…}) of y[q] { … }
  ===> case … of x[p*q] { … case … of y[q] { … } }

This is materialised, in the simplifier, by the fact that every time we simplify
case alternatives with a continuation (the surrounded case (or more!)), we must
scale the entire case we are simplifying, by a scaling factor which can be
computed in the continuation (with function `contHoleScaling`).
-}

simplAlts :: SimplEnv
          -> OutExpr         -- Scrutinee
          -> InId            -- Case binder
          -> [InAlt]         -- Non-empty
          -> SimplCont
          -> SimplM OutExpr  -- Returns the complete simplified case expression

simplAlts env0 scrut case_bndr alts cont'
  = do  { traceSmpl "simplAlts" (vcat [ ppr case_bndr
                                      , text "cont':" <+> ppr cont'
                                      , text "in_scope" <+> ppr (seInScope env0) ])
        ; (env1, case_bndr1) <- simplBinder env0 case_bndr
        ; let case_bndr2 = case_bndr1 `setIdUnfolding` evaldUnfolding
              env2       = modifyInScope env1 case_bndr2
              -- See Note [Case binder evaluated-ness]
              fam_envs   = seFamEnvs env0

        ; (alt_env', scrut', case_bndr') <- improveSeq fam_envs env2 scrut
                                                       case_bndr case_bndr2 alts

        ; (imposs_deflt_cons, in_alts) <- prepareAlts scrut' case_bndr alts
          -- NB: it's possible that the returned in_alts is empty: this is handled
          --     by the caller (rebuildCase) in the missingAlt function
          -- NB: pass case_bndr::InId, not case_bndr' :: OutId, to prepareAlts
          --     See Note [Shadowing in prepareAlts] in GHC.Core.Opt.Simplify.Utils

        ; alts' <- mapM (simplAlt alt_env' (Just scrut') imposs_deflt_cons case_bndr' cont') in_alts
--      ; pprTrace "simplAlts" (ppr case_bndr $$ ppr alts $$ ppr cont') $ return ()

        ; let alts_ty' = contResultType cont'
        -- See Note [Avoiding space leaks in OutType]
        ; seqType alts_ty' `seq`
          mkCase (seMode env0) scrut' case_bndr' alts_ty' alts' }


------------------------------------
improveSeq :: (FamInstEnv, FamInstEnv) -> SimplEnv
           -> OutExpr -> InId -> OutId -> [InAlt]
           -> SimplM (SimplEnv, OutExpr, OutId)
-- Note [Improving seq]
improveSeq fam_envs env scrut case_bndr case_bndr1 [Alt DEFAULT _ _]
  | Just (Reduction co ty2) <- topNormaliseType_maybe fam_envs (idType case_bndr1)
  = do { case_bndr2 <- newId (fsLit "nt") ManyTy ty2
        ; let rhs  = DoneEx (Var case_bndr2 `Cast` mkSymCo co) NotJoinPoint
              env2 = extendIdSubst env case_bndr rhs
        ; return (env2, scrut `Cast` co, case_bndr2) }

improveSeq _ env scrut _ case_bndr1 _
  = return (env, scrut, case_bndr1)


------------------------------------
simplAlt :: SimplEnv
         -> Maybe OutExpr  -- The scrutinee
         -> [AltCon]       -- These constructors can't be present when
                           -- matching the DEFAULT alternative
         -> OutId          -- The case binder
         -> SimplCont
         -> InAlt
         -> SimplM OutAlt

simplAlt env _ imposs_deflt_cons case_bndr' cont' (Alt DEFAULT bndrs rhs)
  = assert (null bndrs) $
    do  { let env' = addBinderUnfolding env case_bndr'
                                        (mkOtherCon imposs_deflt_cons)
                -- Record the constructors that the case-binder *can't* be.
        ; rhs' <- simplExprC env' rhs cont'
        ; return (Alt DEFAULT [] rhs') }

simplAlt env scrut' _ case_bndr' cont' (Alt (LitAlt lit) bndrs rhs)
  = assert (null bndrs) $
    do  { env' <- addAltUnfoldings env scrut' case_bndr' (Lit lit)
        ; rhs' <- simplExprC env' rhs cont'
        ; return (Alt (LitAlt lit) [] rhs') }

simplAlt env scrut' _ case_bndr' cont' (Alt (DataAlt con) vs rhs)
  = do  { -- See Note [Adding evaluatedness info to pattern-bound variables]
          let vs_with_evals = addEvals scrut' con vs
        ; (env', vs') <- simplBinders env vs_with_evals

                -- Bind the case-binder to (con args)
        ; let inst_tys' = tyConAppArgs (idType case_bndr')
              con_app :: OutExpr
              con_app   = mkConApp2 con inst_tys' vs'

        ; env'' <- addAltUnfoldings env' scrut' case_bndr' con_app
        ; rhs' <- simplExprC env'' rhs cont'
        ; return (Alt (DataAlt con) vs' rhs') }

{- Note [Adding evaluatedness info to pattern-bound variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addEvals records the evaluated-ness of the bound variables of
a case pattern.  This is *important*.  Consider

     data T = T !Int !Int

     case x of { T a b -> T (a+1) b }

We really must record that b is already evaluated so that we don't
go and re-evaluate it when constructing the result.
See Note [Data-con worker strictness] in GHC.Core.DataCon

NB: simplLamBndrs preserves this eval info

In addition to handling data constructor fields with !s, addEvals
also records the fact that the result of seq# is always in WHNF.
See Note [seq# magic] in GHC.Core.Opt.ConstantFold.  Example (#15226):

  case seq# v s of
    (# s', v' #) -> E

we want the compiler to be aware that v' is in WHNF in E.

Open problem: we don't record that v itself is in WHNF (and we can't
do it here).  The right thing is to do some kind of binder-swap;
see #15226 for discussion.
-}

addEvals :: Maybe OutExpr -> DataCon -> [Id] -> [Id]
-- See Note [Adding evaluatedness info to pattern-bound variables]
addEvals scrut con vs
  -- Deal with seq# applications
  | Just scr <- scrut
  , isUnboxedTupleDataCon con
  , [s,x] <- vs
    -- Use stripNArgs rather than collectArgsTicks to avoid building
    -- a list of arguments only to throw it away immediately.
  , Just (Var f) <- stripNArgs 4 scr
  , Just SeqOp <- isPrimOpId_maybe f
  , let x' = zapIdOccInfoAndSetEvald MarkedStrict x
  = [s, x']

  -- Deal with banged datacon fields
addEvals _scrut con vs = go vs the_strs
    where
      the_strs = dataConRepStrictness con

      go [] [] = []
      go (v:vs') strs | isTyVar v = v : go vs' strs
      go (v:vs') (str:strs) = zapIdOccInfoAndSetEvald str v : go vs' strs
      go _ _ = pprPanic "Simplify.addEvals"
                (ppr con $$
                 ppr vs  $$
                 ppr_with_length (map strdisp the_strs) $$
                 ppr_with_length (dataConRepArgTys con) $$
                 ppr_with_length (dataConRepStrictness con))
        where
          ppr_with_length list
            = ppr list <+> parens (text "length =" <+> ppr (length list))
          strdisp :: StrictnessMark -> SDoc
          strdisp MarkedStrict = text "MarkedStrict"
          strdisp NotMarkedStrict = text "NotMarkedStrict"

zapIdOccInfoAndSetEvald :: StrictnessMark -> Id -> Id
zapIdOccInfoAndSetEvald str v =
  setCaseBndrEvald str $ -- Add eval'dness info
  zapIdOccInfo v         -- And kill occ info;
                         -- see Note [Case alternative occ info]

addAltUnfoldings :: SimplEnv -> Maybe OutExpr -> OutId -> OutExpr -> SimplM SimplEnv
addAltUnfoldings env mb_scrut case_bndr con_app
  = do { let con_app_unf = mk_simple_unf con_app
             env1 = addBinderUnfolding env case_bndr con_app_unf

             -- See Note [Add unfolding for scrutinee]
             env2 | Just scrut <- mb_scrut
                  , Just (v,mco) <- scrutBinderSwap_maybe scrut
                  = addBinderUnfolding env1 v $
                       if isReflMCo mco  -- isReflMCo: avoid calling mk_simple_unf
                       then con_app_unf  --            twice in the common case
                       else mk_simple_unf (mkCastMCo con_app mco)

                  | otherwise = env1

       ; traceSmpl "addAltUnf" (vcat [ppr case_bndr <+> ppr mb_scrut, ppr con_app])
       ; return env2 }
  where
    -- Force the opts, so that the whole SimplEnv isn't retained
    !opts = seUnfoldingOpts env
    mk_simple_unf = mkSimpleUnfolding opts

addBinderUnfolding :: SimplEnv -> Id -> Unfolding -> SimplEnv
addBinderUnfolding env bndr unf
  | debugIsOn, Just tmpl <- maybeUnfoldingTemplate unf
  = warnPprTrace (not (eqType (idType bndr) (exprType tmpl)))
          "unfolding type mismatch"
          (ppr bndr $$ ppr (idType bndr) $$ ppr tmpl $$ ppr (exprType tmpl)) $
          modifyInScope env (bndr `setIdUnfolding` unf)

  | otherwise
  = modifyInScope env (bndr `setIdUnfolding` unf)

zapBndrOccInfo :: Bool -> Id -> Id
-- Consider  case e of b { (a,b) -> ... }
-- Then if we bind b to (a,b) in "...", and b is not dead,
-- then we must zap the deadness info on a,b
zapBndrOccInfo keep_occ_info pat_id
  | keep_occ_info = pat_id
  | otherwise     = zapIdOccInfo pat_id

{- Note [Case binder evaluated-ness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pin on a (OtherCon []) unfolding to the case-binder of a Case,
even though it'll be over-ridden in every case alternative with a more
informative unfolding.  Why?  Because suppose a later, less clever, pass
simply replaces all occurrences of the case binder with the binder itself;
then Lint may complain about the let-can-float invariant.  Example
    case e of b { DEFAULT -> let v = reallyUnsafePtrEquality# b y in ....
                ; K       -> blah }

The let-can-float invariant requires that y is evaluated in the call to
reallyUnsafePtrEquality#, which it is.  But we still want that to be true if we
propagate binders to occurrences.

This showed up in #13027.

Note [Add unfolding for scrutinee]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general it's unlikely that a variable scrutinee will appear
in the case alternatives   case x of { ...x unlikely to appear... }
because the binder-swap in OccurAnal has got rid of all such occurrences
See Note [Binder swap] in "GHC.Core.Opt.OccurAnal".

BUT it is still VERY IMPORTANT to add a suitable unfolding for a
variable scrutinee, in simplAlt.  Here's why
   case x of y
     (a,b) -> case b of c
                I# v -> ...(f y)...
There is no occurrence of 'b' in the (...(f y)...).  But y gets
the unfolding (a,b), and *that* mentions b.  If f has a RULE
    RULE f (p, I# q) = ...
we want that rule to match, so we must extend the in-scope env with a
suitable unfolding for 'y'.  It's *essential* for rule matching; but
it's also good for case-elimination -- suppose that 'f' was inlined
and did multi-level case analysis, then we'd solve it in one
simplifier sweep instead of two.

HOWEVER, given
  case x of y { Just a -> r1; Nothing -> r2 }
we do not want to add the unfolding x -> y to 'x', which might seem cool,
since 'y' itself has different unfoldings in r1 and r2.  Reason: if we
did that, we'd have to zap y's deadness info and that is a very useful
piece of information.

So instead we add the unfolding x -> Just a, and x -> Nothing in the
respective RHSs.

Since this transformation is tantamount to a binder swap, we use
GHC.Core.Opt.OccurAnal.scrutBinderSwap_maybe to do the check.

Exactly the same issue arises in GHC.Core.Opt.SpecConstr;
see Note [Add scrutinee to ValueEnv too] in GHC.Core.Opt.SpecConstr


************************************************************************
*                                                                      *
\subsection{Known constructor}
*                                                                      *
************************************************************************

We are a bit careful with occurrence info.  Here's an example

        (\x* -> case x of (a*, b) -> f a) (h v, e)

where the * means "occurs once".  This effectively becomes
        case (h v, e) of (a*, b) -> f a)
and then
        let a* = h v; b = e in f a
and then
        f (h v)

All this should happen in one sweep.
-}

knownCon :: SimplEnv
         -> OutExpr                                           -- The scrutinee
         -> [FloatBind] -> DataCon -> [OutType] -> [OutExpr]  -- The scrutinee (in pieces)
         -> InId -> [InBndr] -> InExpr                        -- The alternative
         -> SimplCont
         -> SimplM (SimplFloats, OutExpr)

knownCon env scrut dc_floats dc dc_ty_args dc_args bndr bs rhs cont
  = do  { (floats1, env1)  <- bind_args env bs dc_args
        ; (floats2, env2)  <- bind_case_bndr env1
        ; (floats3, expr') <- simplExprF env2 rhs cont
        ; case dc_floats of
            [] ->
              return (floats1 `addFloats` floats2 `addFloats` floats3, expr')
            _ ->
              return ( emptyFloats env
               -- See Note [FloatBinds from constructor wrappers]
                     , GHC.Core.Make.wrapFloats dc_floats $
                       wrapFloats (floats1 `addFloats` floats2 `addFloats` floats3) expr') }
  where
    zap_occ = zapBndrOccInfo (isDeadBinder bndr)    -- bndr is an InId

                  -- Ugh!
    bind_args env' [] _  = return (emptyFloats env', env')

    bind_args env' (b:bs') (Type ty : args)
      = assert (isTyVar b )
        bind_args (extendTvSubst env' b ty) bs' args

    bind_args env' (b:bs') (Coercion co : args)
      = assert (isCoVar b )
        bind_args (extendCvSubst env' b co) bs' args

    bind_args env' (b:bs') (arg : args)
      = assert (isId b) $
        do { let b' = zap_occ b
             -- zap_occ: the binder might be "dead", because it doesn't
             -- occur in the RHS; and simplAuxBind may therefore discard it.
             -- Nevertheless we must keep it if the case-binder is alive,
             -- because it may be used in the con_app.  See Note [knownCon occ info]
           ; (floats1, env2) <- simplAuxBind env' b' arg  -- arg satisfies let-can-float invariant
           ; (floats2, env3)  <- bind_args env2 bs' args
           ; return (floats1 `addFloats` floats2, env3) }

    bind_args _ _ _ =
      pprPanic "bind_args" $ ppr dc $$ ppr bs $$ ppr dc_args $$
                             text "scrut:" <+> ppr scrut

       -- It's useful to bind bndr to scrut, rather than to a fresh
       -- binding      x = Con arg1 .. argn
       -- because very often the scrut is a variable, so we avoid
       -- creating, and then subsequently eliminating, a let-binding
       -- BUT, if scrut is a not a variable, we must be careful
       -- about duplicating the arg redexes; in that case, make
       -- a new con-app from the args
    bind_case_bndr env
      | isDeadBinder bndr   = return (emptyFloats env, env)
      | exprIsTrivial scrut = return (emptyFloats env
                                     , extendIdSubst env bndr (DoneEx scrut NotJoinPoint))
                              -- See Note [Do not duplicate constructor applications]
      | otherwise           = do { dc_args <- mapM (simplVar env) bs
                                         -- dc_ty_args are already OutTypes,
                                         -- but bs are InBndrs
                                 ; let con_app = Var (dataConWorkId dc)
                                                 `mkTyApps` dc_ty_args
                                                 `mkApps`   dc_args
                                 ; simplAuxBind env bndr con_app }

-------------------
missingAlt :: SimplEnv -> Id -> [InAlt] -> SimplCont
           -> SimplM (SimplFloats, OutExpr)
                -- This isn't strictly an error, although it is unusual.
                -- It's possible that the simplifier might "see" that
                -- an inner case has no accessible alternatives before
                -- it "sees" that the entire branch of an outer case is
                -- inaccessible.  So we simply put an error case here instead.
missingAlt env case_bndr _ cont
  = warnPprTrace True "missingAlt" (ppr case_bndr) $
    -- See Note [Avoiding space leaks in OutType]
    let cont_ty = contResultType cont
    in seqType cont_ty `seq`
       return (emptyFloats env, mkImpossibleExpr cont_ty "Simplify.Iteration.missingAlt")

{-
************************************************************************
*                                                                      *
\subsection{Duplicating continuations}
*                                                                      *
************************************************************************

Consider
  let x* = case e of { True -> e1; False -> e2 }
  in b
where x* is a strict binding.  Then mkDupableCont will be given
the continuation
   case [] of { True -> e1; False -> e2 } ; let x* = [] in b ; stop
and will split it into
   dupable:      case [] of { True -> $j1; False -> $j2 } ; stop
   join floats:  $j1 = e1, $j2 = e2
   non_dupable:  let x* = [] in b; stop

Putting this back together would give
   let x* = let { $j1 = e1; $j2 = e2 } in
            case e of { True -> $j1; False -> $j2 }
   in b
(Of course we only do this if 'e' wants to duplicate that continuation.)
Note how important it is that the new join points wrap around the
inner expression, and not around the whole thing.

In contrast, any let-bindings introduced by mkDupableCont can wrap
around the entire thing.

Note [Bottom alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have
     case (case x of { A -> error .. ; B -> e; C -> error ..)
       of alts
then we can just duplicate those alts because the A and C cases
will disappear immediately.  This is more direct than creating
join points and inlining them away.  See #4930.
-}

--------------------
mkDupableCaseCont :: SimplEnv -> [InAlt] -> SimplCont
                  -> SimplM ( SimplFloats  -- Join points (if any)
                            , SimplEnv     -- Use this for the alts
                            , SimplCont)
mkDupableCaseCont env alts cont
  | altsWouldDup alts = do { (floats, cont) <- mkDupableCont env cont
                           ; let env' = bumpCaseDepth $
                                        env `setInScopeFromF` floats
                           ; return (floats, env', cont) }
  | otherwise         = return (emptyFloats env, env, cont)

altsWouldDup :: [InAlt] -> Bool -- True iff strictly > 1 non-bottom alternative
altsWouldDup []  = False        -- See Note [Bottom alternatives]
altsWouldDup [_] = False
altsWouldDup (alt:alts)
  | is_bot_alt alt = altsWouldDup alts
  | otherwise      = not (all is_bot_alt alts)
    -- otherwise case: first alt is non-bot, so all the rest must be bot
  where
    is_bot_alt (Alt _ _ rhs) = exprIsDeadEnd rhs

-------------------------
mkDupableCont :: SimplEnv
              -> SimplCont
              -> SimplM ( SimplFloats  -- Incoming SimplEnv augmented with
                                       --   extra let/join-floats and in-scope variables
                        , SimplCont)   -- dup_cont: duplicable continuation
mkDupableCont env cont
  = mkDupableContWithDmds env (repeat topDmd) cont

mkDupableContWithDmds
   :: SimplEnv  -> [Demand]  -- Demands on arguments; always infinite
   -> SimplCont -> SimplM ( SimplFloats, SimplCont)

mkDupableContWithDmds env _ cont
  | contIsDupable cont
  = return (emptyFloats env, cont)

mkDupableContWithDmds _ _ (Stop {}) = panic "mkDupableCont"     -- Handled by previous eqn

mkDupableContWithDmds env dmds (CastIt ty cont)
  = do  { (floats, cont') <- mkDupableContWithDmds env dmds cont
        ; return (floats, CastIt ty cont') }

-- Duplicating ticks for now, not sure if this is good or not
mkDupableContWithDmds env dmds (TickIt t cont)
  = do  { (floats, cont') <- mkDupableContWithDmds env dmds cont
        ; return (floats, TickIt t cont') }

mkDupableContWithDmds env _
     (StrictBind { sc_bndr = bndr, sc_body = body, sc_from = from_what
                 , sc_env = se, sc_cont = cont})
-- See Note [Duplicating StrictBind]
-- K[ let x = <> in b ]  -->   join j x = K[ b ]
--                             j <>
  = do { let sb_env = se `setInScopeFromE` env
       ; (sb_env1, bndr')      <- simplBinder sb_env bndr
       ; (floats1, join_inner) <- simplNonRecBody sb_env1 from_what body cont
          -- No need to use mkDupableCont before simplNonRecBody; we
          -- use cont once here, and then share the result if necessary

       ; let join_body = wrapFloats floats1 join_inner
             res_ty    = contResultType cont

       ; mkDupableStrictBind env bndr' join_body res_ty }

mkDupableContWithDmds env _
    (StrictArg { sc_fun = fun, sc_cont = cont
               , sc_fun_ty = fun_ty })
  -- NB: sc_dup /= OkToDup; that is caught earlier by contIsDupable
  | isNothing (isDataConId_maybe (ai_fun fun))
  , thumbsUpPlanA cont  -- See point (3) of Note [Duplicating join points]
  = -- Use Plan A of Note [Duplicating StrictArg]
    do { let (_ : dmds) = ai_dmds fun
       ; (floats1, cont')  <- mkDupableContWithDmds env dmds cont
                              -- Use the demands from the function to add the right
                              -- demand info on any bindings we make for further args
       ; (floats_s, args') <- mapAndUnzipM (makeTrivialArg env)
                                           (ai_args fun)
       ; return ( foldl' addLetFloats floats1 floats_s
                , StrictArg { sc_fun = fun { ai_args = args' }
                            , sc_cont = cont'
                            , sc_fun_ty = fun_ty
                            , sc_dup = OkToDup} ) }

  | otherwise
  = -- Use Plan B of Note [Duplicating StrictArg]
    --   K[ f a b <> ]   -->   join j x = K[ f a b x ]
    --                         j <>
    do { let rhs_ty       = contResultType cont
             (m,arg_ty,_) = splitFunTy fun_ty
       ; arg_bndr <- newId (fsLit "arg") m arg_ty
       ; let env' = env `addNewInScopeIds` [arg_bndr]
       ; (floats, join_rhs) <- rebuildCall env' (addValArgTo fun (Var arg_bndr) fun_ty) cont
       ; mkDupableStrictBind env' arg_bndr (wrapFloats floats join_rhs) rhs_ty }
  where
    thumbsUpPlanA (StrictArg {})               = False
    thumbsUpPlanA (CastIt _ k)                 = thumbsUpPlanA k
    thumbsUpPlanA (TickIt _ k)                 = thumbsUpPlanA k
    thumbsUpPlanA (ApplyToVal { sc_cont = k }) = thumbsUpPlanA k
    thumbsUpPlanA (ApplyToTy  { sc_cont = k }) = thumbsUpPlanA k
    thumbsUpPlanA (Select {})                  = True
    thumbsUpPlanA (StrictBind {})              = True
    thumbsUpPlanA (Stop {})                    = True

mkDupableContWithDmds env dmds
    (ApplyToTy { sc_cont = cont, sc_arg_ty = arg_ty, sc_hole_ty = hole_ty })
  = do  { (floats, cont') <- mkDupableContWithDmds env dmds cont
        ; return (floats, ApplyToTy { sc_cont = cont'
                                    , sc_arg_ty = arg_ty, sc_hole_ty = hole_ty }) }

mkDupableContWithDmds env dmds
    (ApplyToVal { sc_arg = arg, sc_dup = dup, sc_env = se
                , sc_cont = cont, sc_hole_ty = hole_ty })
  =     -- e.g.         [...hole...] (...arg...)
        --      ==>
        --              let a = ...arg...
        --              in [...hole...] a
        -- NB: sc_dup /= OkToDup; that is caught earlier by contIsDupable
    do  { let (dmd:cont_dmds) = dmds   -- Never fails
        ; (floats1, cont') <- mkDupableContWithDmds env cont_dmds cont
        ; let env' = env `setInScopeFromF` floats1
        ; (_, se', arg') <- simplLazyArg env' dup hole_ty Nothing se arg
        ; (let_floats2, arg'') <- makeTrivial env NotTopLevel dmd (fsLit "karg") arg'
        ; let all_floats = floats1 `addLetFloats` let_floats2
        ; return ( all_floats
                 , ApplyToVal { sc_arg = arg''
                              , sc_env = se' `setInScopeFromF` all_floats
                                         -- Ensure that sc_env includes the free vars of
                                         -- arg'' in its in-scope set, even if makeTrivial
                                         -- has turned arg'' into a fresh variable
                                         -- See Note [StaticEnv invariant] in GHC.Core.Opt.Simplify.Utils
                              , sc_dup = OkToDup, sc_cont = cont'
                              , sc_hole_ty = hole_ty }) }

mkDupableContWithDmds env _
    (Select { sc_bndr = case_bndr, sc_alts = alts, sc_env = se, sc_cont = cont })
  =     -- e.g.         (case [...hole...] of { pi -> ei })
        --      ===>
        --              let ji = \xij -> ei
        --              in case [...hole...] of { pi -> ji xij }
        -- NB: sc_dup /= OkToDup; that is caught earlier by contIsDupable
    do  { tick (CaseOfCase case_bndr)
        ; (floats, alt_env, alt_cont) <- mkDupableCaseCont (se `setInScopeFromE` env) alts cont
                -- NB: We call mkDupableCaseCont here to make cont duplicable
                --     (if necessary, depending on the number of alts)
                -- And this is important: see Note [Fusing case continuations]

        ; let cont_scaling = contHoleScaling cont
          -- See Note [Scaling in case-of-case]
        ; (alt_env', case_bndr') <- simplBinder alt_env (scaleIdBy cont_scaling case_bndr)
        ; alts' <- mapM (simplAlt alt_env' Nothing [] case_bndr' alt_cont) (scaleAltsBy cont_scaling alts)
        -- Safe to say that there are no handled-cons for the DEFAULT case
                -- NB: simplBinder does not zap deadness occ-info, so
                -- a dead case_bndr' will still advertise its deadness
                -- This is really important because in
                --      case e of b { (# p,q #) -> ... }
                -- b is always dead, and indeed we are not allowed to bind b to (# p,q #),
                -- which might happen if e was an explicit unboxed pair and b wasn't marked dead.
                -- In the new alts we build, we have the new case binder, so it must retain
                -- its deadness.
        -- NB: we don't use alt_env further; it has the substEnv for
        --     the alternatives, and we don't want that

        ; let platform = sePlatform env
        ; (join_floats, alts'') <- mapAccumLM (mkDupableAlt platform case_bndr')
                                              emptyJoinFloats alts'

        ; let all_floats = floats `addJoinFloats` join_floats
                           -- Note [Duplicated env]
        ; return (all_floats
                 , Select { sc_dup  = OkToDup
                          , sc_bndr = case_bndr'
                          , sc_alts = alts''
                          , sc_env  = zapSubstEnv se `setInScopeFromF` all_floats
                                      -- See Note [StaticEnv invariant] in GHC.Core.Opt.Simplify.Utils
                          , sc_cont = mkBoringStop (contResultType cont) } ) }

mkDupableStrictBind :: SimplEnv -> OutId -> OutExpr -> OutType
                    -> SimplM (SimplFloats, SimplCont)
mkDupableStrictBind env arg_bndr join_rhs res_ty
  | exprIsTrivial join_rhs   -- See point (2) of Note [Duplicating join points]
  = return (emptyFloats env
           , StrictBind { sc_bndr = arg_bndr
                        , sc_body = join_rhs
                        , sc_env  = zapSubstEnv env
                        , sc_from = FromLet
                          -- See Note [StaticEnv invariant] in GHC.Core.Opt.Simplify.Utils
                        , sc_dup  = OkToDup
                        , sc_cont = mkBoringStop res_ty } )
  | otherwise
  = do { join_bndr <- newJoinId [arg_bndr] res_ty
       ; let arg_info = ArgInfo { ai_fun   = join_bndr
                                , ai_rewrite = TryNothing, ai_args  = []
                                , ai_encl  = False, ai_dmds  = repeat topDmd
                                , ai_discs = repeat 0 }
       ; return ( addJoinFloats (emptyFloats env) $
                  unitJoinFloat                   $
                  NonRec join_bndr                $
                  Lam (setOneShotLambda arg_bndr) join_rhs
                , StrictArg { sc_dup    = OkToDup
                            , sc_fun    = arg_info
                            , sc_fun_ty = idType join_bndr
                            , sc_cont   = mkBoringStop res_ty
                            } ) }

mkDupableAlt :: Platform -> OutId
             -> JoinFloats -> OutAlt
             -> SimplM (JoinFloats, OutAlt)
mkDupableAlt _platform case_bndr jfloats (Alt con alt_bndrs alt_rhs_in)
  | exprIsTrivial alt_rhs_in   -- See point (2) of Note [Duplicating join points]
  = return (jfloats, Alt con alt_bndrs alt_rhs_in)

  | otherwise
  = do  { let rhs_ty'  = exprType alt_rhs_in

              bangs
                | DataAlt c <- con
                = dataConRepStrictness c
                | otherwise = []

              abstracted_binders = abstract_binders alt_bndrs bangs

              abstract_binders :: [Var] -> [StrictnessMark] -> [(Id,StrictnessMark)]
              abstract_binders [] []
                -- Abstract over the case binder too if it's used.
                | isDeadBinder case_bndr  = []
                | otherwise               = [(case_bndr,MarkedStrict)]
              abstract_binders (alt_bndr:alt_bndrs) marks
                -- Abstract over all type variables just in case
                | isTyVar alt_bndr        = (alt_bndr,NotMarkedStrict) : abstract_binders alt_bndrs marks
              abstract_binders (alt_bndr:alt_bndrs) (mark:marks)
                -- The deadness info on the new Ids is preserved by simplBinders
                -- We don't abstract over dead ids here.
                | isDeadBinder alt_bndr   = abstract_binders alt_bndrs marks
                | otherwise               = (alt_bndr,mark) : abstract_binders alt_bndrs marks
              abstract_binders _ _ = pprPanic "abstrict_binders - failed to abstract" (ppr $ Alt con alt_bndrs alt_rhs_in)

              filtered_binders = map fst abstracted_binders
              -- We want to make any binder with an evaldUnfolding strict in the rhs.
              -- See Note [Call-by-value for worker args] (which also applies to join points)
              (rhs_with_seqs) = mkStrictFieldSeqs abstracted_binders alt_rhs_in

              final_args = varsToCoreExprs filtered_binders
                           -- Note [Join point abstraction]

                -- We make the lambdas into one-shot-lambdas.  The
                -- join point is sure to be applied at most once, and doing so
                -- prevents the body of the join point being floated out by
                -- the full laziness pass
              final_bndrs     = map one_shot filtered_binders
              one_shot v | isId v    = setOneShotLambda v
                         | otherwise = v

              -- No lambda binder has an unfolding, but (currently) case binders can,
              -- so we must zap them here.
              join_rhs   = mkLams (map zapIdUnfolding final_bndrs) rhs_with_seqs

        ; join_bndr <- newJoinId filtered_binders rhs_ty'

        ; let join_call = mkApps (Var join_bndr) final_args
              alt'      = Alt con alt_bndrs join_call

        ; return ( jfloats `addJoinFlts` unitJoinFloat (NonRec join_bndr join_rhs)
                 , alt') }
                -- See Note [Duplicated env]

{-
Note [Fusing case continuations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important to fuse two successive case continuations when the
first has one alternative.  That's why we call prepareCaseCont here.
Consider this, which arises from thunk splitting (see Note [Thunk
splitting] in GHC.Core.Opt.WorkWrap):

      let
        x* = case (case v of {pn -> rn}) of
               I# a -> I# a
      in body

The simplifier will find
    (Var v) with continuation
            Select (pn -> rn) (
            Select [I# a -> I# a] (
            StrictBind body Stop

So we'll call mkDupableCont on
   Select [I# a -> I# a] (StrictBind body Stop)
There is just one alternative in the first Select, so we want to
simplify the rhs (I# a) with continuation (StrictBind body Stop)
Supposing that body is big, we end up with
          let $j a = <let x = I# a in body>
          in case v of { pn -> case rn of
                                 I# a -> $j a }
This is just what we want because the rn produces a box that
the case rn cancels with.

See #4957 a fuller example.

Note [Duplicating join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IN #19996 we discovered that we want to be really careful about
inlining join points.   Consider
    case (join $j x = K f x )
         (in case v of      )
         (     p1 -> $j x1  ) of
         (     p2 -> $j x2  )
         (     p3 -> $j x3  )
      K g y -> blah[g,y]

Here the join-point RHS is very small, just a constructor
application (K x y).  So we might inline it to get
    case (case v of        )
         (     p1 -> K f x1  ) of
         (     p2 -> K f x2  )
         (     p3 -> K f x3  )
      K g y -> blah[g,y]

But now we have to make `blah` into a join point, /abstracted/
over `g` and `y`.   In contrast, if we /don't/ inline $j we
don't need a join point for `blah` and we'll get
    join $j x = let g=f, y=x in blah[g,y]
    in case v of
       p1 -> $j x1
       p2 -> $j x2
       p3 -> $j x3

This can make a /massive/ difference, because `blah` can see
what `f` is, instead of lambda-abstracting over it.

To achieve this:

1. Do not postInlineUnconditionally a join point, until the Final
   phase.  (The Final phase is still quite early, so we might consider
   delaying still more.)

2. In mkDupableAlt and mkDupableStrictBind, generate an alterative for
   all alternatives, except for exprIsTrival RHSs. Previously we used
   exprIsDupable.  This generates a lot more join points, but makes
   them much more case-of-case friendly.

   It is definitely worth checking for exprIsTrivial, otherwise we get
   an extra Simplifier iteration, because it is inlined in the next
   round.

3. By the same token we want to use Plan B in
   Note [Duplicating StrictArg] when the RHS of the new join point
   is a data constructor application.  That same Note explains why we
   want Plan A when the RHS of the new join point would be a
   non-data-constructor application

4. You might worry that $j will be inlined by the call-site inliner,
   but it won't because the call-site context for a join is usually
   extremely boring (the arguments come from the pattern match).
   And if not, then perhaps inlining it would be a good idea.

   You might also wonder if we get UnfWhen, because the RHS of the
   join point is no bigger than the call. But in the cases we care
   about it will be a little bigger, because of that free `f` in
       $j x = K f x
   So for now we don't do anything special in callSiteInline

There is a bit of tension between (2) and (3).  Do we want to retain
the join point only when the RHS is
* a constructor application? or
* just non-trivial?
Currently, a bit ad-hoc, but we definitely want to retain the join
point for data constructors in mkDupalbleALt (point 2); that is the
whole point of #19996 described above.

Historical Note [Case binders and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: this entire Note is now irrelevant.  In Jun 21 we stopped
adding unfoldings to lambda binders (#17530).  It was always a
hack and bit us in multiple small and not-so-small ways

Consider this
   case (case .. ) of c {
     I# c# -> ....c....

If we make a join point with c but not c# we get
  $j = \c -> ....c....

But if later inlining scrutinises the c, thus

  $j = \c -> ... case c of { I# y -> ... } ...

we won't see that 'c' has already been scrutinised.  This actually
happens in the 'tabulate' function in wave4main, and makes a significant
difference to allocation.

An alternative plan is this:

   $j = \c# -> let c = I# c# in ...c....

but that is bad if 'c' is *not* later scrutinised.

So instead we do both: we pass 'c' and 'c#' , and record in c's inlining
(a stable unfolding) that it's really I# c#, thus

   $j = \c# -> \c[=I# c#] -> ...c....

Absence analysis may later discard 'c'.

NB: take great care when doing strictness analysis;
    see Note [Lambda-bound unfoldings] in GHC.Core.Opt.DmdAnal.

Also note that we can still end up passing stuff that isn't used.  Before
strictness analysis we have
   let $j x y c{=(x,y)} = (h c, ...)
   in ...
After strictness analysis we see that h is strict, we end up with
   let $j x y c{=(x,y)} = ($wh x y, ...)
and c is unused.

Note [Duplicated env]
~~~~~~~~~~~~~~~~~~~~~
Some of the alternatives are simplified, but have not been turned into a join point
So they *must* have a zapped subst-env.  So we can't use completeNonRecX to
bind the join point, because it might to do PostInlineUnconditionally, and
we'd lose that when zapping the subst-env.  We could have a per-alt subst-env,
but zapping it (as we do in mkDupableCont, the Select case) is safe, and
at worst delays the join-point inlining.

Note [Funky mkLamTypes]
~~~~~~~~~~~~~~~~~~~~~~
Notice the funky mkLamTypes.  If the constructor has existentials
it's possible that the join point will be abstracted over
type variables as well as term variables.
 Example:  Suppose we have
        data T = forall t.  C [t]
 Then faced with
        case (case e of ...) of
            C t xs::[t] -> rhs
 We get the join point
        let j :: forall t. [t] -> ...
            j = /\t \xs::[t] -> rhs
        in
        case (case e of ...) of
            C t xs::[t] -> j t xs

Note [Duplicating StrictArg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dealing with making a StrictArg continuation duplicable has turned out
to be one of the trickiest corners of the simplifier, giving rise
to several cases in which the simplier expanded the program's size
*exponentially*.  They include
  #13253 exponential inlining
  #10421 ditto
  #18140 strict constructors
  #18282 another nested-function call case

Suppose we have a call
  f e1 (case x of { True -> r1; False -> r2 }) e3
and f is strict in its second argument.  Then we end up in
mkDupableCont with a StrictArg continuation for (f e1 <> e3).
There are two ways to make it duplicable.

* Plan A: move the entire call inwards, being careful not
  to duplicate e1 or e3, thus:
     let a1 = e1
         a3 = e3
     in case x of { True  -> f a1 r1 a3
                  ; False -> f a1 r2 a3 }

* Plan B: make a join point:
     join $j x = f e1 x e3
     in case x of { True  -> jump $j r1
                  ; False -> jump $j r2 }

  Notice that Plan B is very like the way we handle strict bindings;
  see Note [Duplicating StrictBind].  And Plan B is exactly what we'd
  get if we turned use a case expression to evaluate the strict arg:

       case (case x of { True -> r1; False -> r2 }) of
         r -> f e1 r e3

  So, looking at Note [Duplicating join points], we also want Plan B
  when `f` is a data constructor.

Plan A is often good. Here's an example from #3116
     go (n+1) (case l of
                 1  -> bs'
                 _  -> Chunk p fpc (o+1) (l-1) bs')

If we pushed the entire call for 'go' inside the case, we get
call-pattern specialisation for 'go', which is *crucial* for
this particular program.

Here is another example.
        && E (case x of { T -> F; F -> T })

Pushing the call inward (being careful not to duplicate E)
        let a = E
        in case x of { T -> && a F; F -> && a T }

and now the (&& a F) etc can optimise.  Moreover there might
be a RULE for the function that can fire when it "sees" the
particular case alternative.

But Plan A can have terrible, terrible behaviour. Here is a classic
case:
  f (f (f (f (f True))))

Suppose f is strict, and has a body that is small enough to inline.
The innermost call inlines (seeing the True) to give
  f (f (f (f (case v of { True -> e1; False -> e2 }))))

Now, suppose we naively push the entire continuation into both
case branches (it doesn't look large, just f.f.f.f). We get
  case v of
    True  -> f (f (f (f e1)))
    False -> f (f (f (f e2)))

And now the process repeats, so we end up with an exponentially large
number of copies of f. No good!

CONCLUSION: we want Plan A in general, but do Plan B is there a
danger of this nested call behaviour. The function that decides
this is called thumbsUpPlanA.

Note [Keeping demand info in StrictArg Plan A]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Following on from Note [Duplicating StrictArg], another common code
pattern that can go bad is this:
   f (case x1 of { T -> F; F -> T })
     (case x2 of { T -> F; F -> T })
     ...etc...
when f is strict in all its arguments.  (It might, for example, be a
strict data constructor whose wrapper has not yet been inlined.)

We use Plan A (because there is no nesting) giving
  let a2 = case x2 of ...
      a3 = case x3 of ...
  in case x1 of { T -> f F a2 a3 ... ; F -> f T a2 a3 ... }

Now we must be careful!  a2 and a3 are small, and the OneOcc code in
postInlineUnconditionally may inline them both at both sites; see Note
Note [Inline small things to avoid creating a thunk] in
Simplify.Utils. But if we do inline them, the entire process will
repeat -- back to exponential behaviour.

So we are careful to keep the demand-info on a2 and a3.  Then they'll
be /strict/ let-bindings, which will be dealt with by StrictBind.
That's why contIsDupableWithDmds is careful to propagage demand
info to the auxiliary bindings it creates.  See the Demand argument
to makeTrivial.

Note [Duplicating StrictBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We make a StrictBind duplicable in a very similar way to
that for case expressions.  After all,
   let x* = e in b   is similar to    case e of x -> b

So we potentially make a join-point for the body, thus:
   let x = <> in b   ==>   join j x = b
                           in j <>

Just like StrictArg in fact -- and indeed they share code.

Note [Join point abstraction]  Historical note
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: This note is now historical, describing how (in the past) we used
to add a void argument to nullary join points.  But now that "join
point" is not a fuzzy concept but a formal syntactic construct (as
distinguished by the JoinId constructor of IdDetails), each of these
concerns is handled separately, with no need for a vestigial extra
argument.

Join points always have at least one value argument,
for several reasons

* If we try to lift a primitive-typed something out
  for let-binding-purposes, we will *caseify* it (!),
  with potentially-disastrous strictness results.  So
  instead we turn it into a function: \v -> e
  where v::Void#.  The value passed to this function is void,
  which generates (almost) no code.

* CPR.  We used to say "&& isUnliftedType rhs_ty'" here, but now
  we make the join point into a function whenever used_bndrs'
  is empty.  This makes the join-point more CPR friendly.
  Consider:       let j = if .. then I# 3 else I# 4
                  in case .. of { A -> j; B -> j; C -> ... }

  Now CPR doesn't w/w j because it's a thunk, so
  that means that the enclosing function can't w/w either,
  which is a lose.  Here's the example that happened in practice:
          kgmod :: Int -> Int -> Int
          kgmod x y = if x > 0 && y < 0 || x < 0 && y > 0
                      then 78
                      else 5

* Let-no-escape.  We want a join point to turn into a let-no-escape
  so that it is implemented as a jump, and one of the conditions
  for LNE is that it's not updatable.  In CoreToStg, see
  Note [What is a non-escaping let]

* Floating.  Since a join point will be entered once, no sharing is
  gained by floating out, but something might be lost by doing
  so because it might be allocated.

I have seen a case alternative like this:
        True -> \v -> ...
It's a bit silly to add the realWorld dummy arg in this case, making
        $j = \s v -> ...
           True -> $j s
(the \v alone is enough to make CPR happy) but I think it's rare

There's a slight infelicity here: we pass the overall
case_bndr to all the join points if it's used in *any* RHS,
because we don't know its usage in each RHS separately



************************************************************************
*                                                                      *
                    Unfoldings
*                                                                      *
************************************************************************
-}

simplLetUnfolding :: SimplEnv
                  -> BindContext
                  -> InId
                  -> OutExpr -> OutType -> ArityType
                  -> Unfolding -> SimplM Unfolding
simplLetUnfolding env bind_cxt id new_rhs rhs_ty arity unf
  | isStableUnfolding unf
  = simplStableUnfolding env bind_cxt id rhs_ty arity unf
  | isExitJoinId id
  = return noUnfolding -- See Note [Do not inline exit join points] in GHC.Core.Opt.Exitify
  | otherwise
  = -- Otherwise, we end up retaining all the SimpleEnv
    let !opts = seUnfoldingOpts env
    in mkLetUnfolding opts (bindContextLevel bind_cxt) VanillaSrc id new_rhs

-------------------
mkLetUnfolding :: UnfoldingOpts -> TopLevelFlag -> UnfoldingSource
               -> InId -> OutExpr -> SimplM Unfolding
mkLetUnfolding !uf_opts top_lvl src id new_rhs
  = return (mkUnfolding uf_opts src is_top_lvl is_bottoming new_rhs Nothing)
            -- We make an  unfolding *even for loop-breakers*.
            -- Reason: (a) It might be useful to know that they are WHNF
            --         (b) In GHC.Iface.Tidy we currently assume that, if we want to
            --             expose the unfolding then indeed we *have* an unfolding
            --             to expose.  (We could instead use the RHS, but currently
            --             we don't.)  The simple thing is always to have one.
  where
    -- Might as well force this, profiles indicate up to 0.5MB of thunks
    -- just from this site.
    !is_top_lvl   = isTopLevel top_lvl
    -- See Note [Force bottoming field]
    !is_bottoming = isDeadEndId id

-------------------
simplStableUnfolding :: SimplEnv -> BindContext
                     -> InId
                     -> OutType
                     -> ArityType      -- Used to eta expand, but only for non-join-points
                     -> Unfolding
                     ->SimplM Unfolding
-- Note [Setting the new unfolding]
simplStableUnfolding env bind_cxt id rhs_ty id_arity unf
  = case unf of
      NoUnfolding   -> return unf
      BootUnfolding -> return unf
      OtherCon {}   -> return unf

      DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args }
        -> do { (env', bndrs') <- simplBinders unf_env bndrs
              ; args' <- mapM (simplExpr env') args
              ; return (mkDFunUnfolding bndrs' con args') }

      CoreUnfolding { uf_tmpl = expr, uf_src = src, uf_guidance = guide }
        | isStableSource src
        -> do { expr' <- case bind_cxt of
                  BC_Join _ cont    -> -- Binder is a join point
                                       -- See Note [Rules and unfolding for join points]
                                       simplJoinRhs unf_env id expr cont
                  BC_Let _ is_rec -> -- Binder is not a join point
                                     do { let cont = mkRhsStop rhs_ty is_rec topDmd
                                           -- mkRhsStop: switch off eta-expansion at the top level
                                        ; expr' <- simplExprC unf_env expr cont
                                        ; return (eta_expand expr') }
              ; case guide of
                  UnfWhen { ug_boring_ok = boring_ok }
                     -- Happens for INLINE things
                     -- Really important to force new_boring_ok since otherwise
                     --   `ug_boring_ok` is a thunk chain of
                     --   inlineBoringExprOk expr0 || inlineBoringExprOk expr1 || ...
                     -- See #20134
                     -> let !new_boring_ok = boring_ok || inlineBoringOk expr'
                            guide' = guide { ug_boring_ok = new_boring_ok }
                        -- Refresh the boring-ok flag, in case expr'
                        -- has got small. This happens, notably in the inlinings
                        -- for dfuns for single-method classes; see
                        -- Note [Single-method classes] in GHC.Tc.TyCl.Instance.
                        -- A test case is #4138
                        -- But retain a previous boring_ok of True; e.g. see
                        -- the way it is set in calcUnfoldingGuidanceWithArity
                        in return (mkCoreUnfolding src is_top_lvl expr' Nothing guide')
                            -- See Note [Top-level flag on inline rules] in GHC.Core.Unfold

                  _other              -- Happens for INLINABLE things
                     -> mkLetUnfolding uf_opts top_lvl src id expr' }
                -- If the guidance is UnfIfGoodArgs, this is an INLINABLE
                -- unfolding, and we need to make sure the guidance is kept up
                -- to date with respect to any changes in the unfolding.

        | otherwise -> return noUnfolding   -- Discard unstable unfoldings
  where
    uf_opts    = seUnfoldingOpts env
    -- Forcing this can save about 0.5MB of max residency and the result
    -- is small and easy to compute so might as well force it.
    top_lvl     = bindContextLevel bind_cxt
    !is_top_lvl = isTopLevel top_lvl
    act        = idInlineActivation id
    unf_env    = updMode (updModeForStableUnfoldings act) env
         -- See Note [Simplifying inside stable unfoldings] in GHC.Core.Opt.Simplify.Utils

    -- See Note [Eta-expand stable unfoldings]
    -- Use the arity from the main Id (in id_arity), rather than computing it from rhs
    -- Not used for join points
    eta_expand expr | seEtaExpand env
                    , exprArity expr < arityTypeArity id_arity
                    , wantEtaExpansion expr
                    = etaExpandAT (getInScope env) id_arity expr
                    | otherwise
                    = expr

{- Note [Eta-expand stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For INLINE/INLINABLE things (which get stable unfoldings) there's a danger
of getting
   f :: Int -> Int -> Int -> Blah
   [ Arity = 3                 -- Good arity
   , Unf=Stable (\xy. blah)    -- Less good arity, only 2
   f = \pqr. e

This can happen because f's RHS is optimised more vigorously than
its stable unfolding.  Now suppose we have a call
   g = f x
Because f has arity=3, g will have arity=2.  But if we inline f (using
its stable unfolding) g's arity will reduce to 1, because <blah>
hasn't been optimised yet.  This happened in the 'parsec' library,
for Text.Pasec.Char.string.

Generally, if we know that 'f' has arity N, it seems sensible to
eta-expand the stable unfolding to arity N too. Simple and consistent.

Wrinkles

* See Historical-note [Eta-expansion in stable unfoldings] in
  GHC.Core.Opt.Simplify.Utils

* Don't eta-expand a trivial expr, else each pass will eta-reduce it,
  and then eta-expand again. See Note [Which RHSs do we eta-expand?]
  in GHC.Core.Opt.Simplify.Utils.

* Don't eta-expand join points; see Note [Do not eta-expand join points]
  in GHC.Core.Opt.Simplify.Utils.  We uphold this because the join-point
  case (bind_cxt = BC_Join {}) doesn't use eta_expand.

Note [Force bottoming field]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to force bottoming, or the new unfolding holds
on to the old unfolding (which is part of the id).

Note [Setting the new unfolding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* If there's an INLINE pragma, we simplify the RHS gently.  Maybe we
  should do nothing at all, but simplifying gently might get rid of
  more crap.

* If not, we make an unfolding from the new RHS.  But *only* for
  non-loop-breakers. Making loop breakers not have an unfolding at all
  means that we can avoid tests in exprIsConApp, for example.  This is
  important: if exprIsConApp says 'yes' for a recursive thing, then we
  can get into an infinite loop

If there's a stable unfolding on a loop breaker (which happens for
INLINABLE), we hang on to the inlining.  It's pretty dodgy, but the
user did say 'INLINE'.  May need to revisit this choice.

************************************************************************
*                                                                      *
                    Rules
*                                                                      *
************************************************************************

Note [Rules in a letrec]
~~~~~~~~~~~~~~~~~~~~~~~~
After creating fresh binders for the binders of a letrec, we
substitute the RULES and add them back onto the binders; this is done
*before* processing any of the RHSs.  This is important.  Manuel found
cases where he really, really wanted a RULE for a recursive function
to apply in that function's own right-hand side.

See Note [Forming Rec groups] in "GHC.Core.Opt.OccurAnal"
-}

addBndrRules :: SimplEnv -> InBndr -> OutBndr
             -> BindContext
             -> SimplM (SimplEnv, OutBndr)
-- Rules are added back into the bin
addBndrRules env in_id out_id bind_cxt
  | null old_rules
  = return (env, out_id)
  | otherwise
  = do { new_rules <- simplRules env (Just out_id) old_rules bind_cxt
       ; let final_id  = out_id `setIdSpecialisation` mkRuleInfo new_rules
       ; return (modifyInScope env final_id, final_id) }
  where
    old_rules = ruleInfoRules (idSpecialisation in_id)

simplImpRules :: SimplEnv -> [CoreRule] -> SimplM [CoreRule]
-- Simplify local rules for imported Ids
simplImpRules env rules
  = simplRules env Nothing rules (BC_Let TopLevel NonRecursive)

simplRules :: SimplEnv -> Maybe OutId -> [CoreRule]
           -> BindContext -> SimplM [CoreRule]
simplRules env mb_new_id rules bind_cxt
  = mapM simpl_rule rules
  where
    simpl_rule rule@(BuiltinRule {})
      = return rule

    simpl_rule rule@(Rule { ru_bndrs = bndrs, ru_args = args
                          , ru_fn = fn_name, ru_rhs = rhs
                          , ru_act = act })
      = do { (env', bndrs') <- simplBinders env bndrs
           ; let rhs_ty = substTy env' (exprType rhs)
                 rhs_cont = case bind_cxt of  -- See Note [Rules and unfolding for join points]
                                BC_Let {}      -> mkBoringStop rhs_ty
                                BC_Join _ cont -> assertPpr join_ok bad_join_msg cont
                 lhs_env = updMode updModeForRules env'
                 rhs_env = updMode (updModeForStableUnfoldings act) env'
                           -- See Note [Simplifying the RHS of a RULE]
                 -- Force this to avoid retaining reference to old Id
                 !fn_name' = case mb_new_id of
                              Just id -> idName id
                              Nothing -> fn_name

                 -- join_ok is an assertion check that the join-arity of the
                 -- binder matches that of the rule, so that pushing the
                 -- continuation into the RHS makes sense
                 join_ok = case mb_new_id of
                             Just id | JoinPoint join_arity <- idJoinPointHood id
                                     -> length args == join_arity
                             _ -> False
                 bad_join_msg = vcat [ ppr mb_new_id, ppr rule
                                     , ppr (fmap idJoinPointHood mb_new_id) ]

           ; args' <- mapM (simplExpr lhs_env) args
           ; rhs'  <- simplExprC rhs_env rhs rhs_cont
           ; return (rule { ru_bndrs = bndrs'
                          , ru_fn    = fn_name'
                          , ru_args  = args'
                          , ru_rhs   = occurAnalyseExpr rhs' }) }
                            -- Remember to occ-analyse, to drop dead code.
                            -- See Note [OccInfo in unfoldings and rules] in GHC.Core

{- Note [Simplifying the RHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can simplify the RHS of a RULE much as we do the RHS of a stable
unfolding.  We used to use the much more conservative updModeForRules
for the RHS as well as the LHS, but that seems more conservative
than necesary.  Allowing some inlining might, for example, eliminate
a binding.
-}
