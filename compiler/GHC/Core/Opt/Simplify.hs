{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[Simplify]{The main module of the simplifier}
-}


{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates -Wno-incomplete-uni-patterns #-}
module GHC.Core.Opt.Simplify ( simplTopBinds, simplExpr, simplRules ) where

import GHC.Prelude

import GHC.Platform
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Type hiding ( substTy, substTyVar, extendTvSubst, extendCvSubst )
import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Utils
import GHC.Core.Opt.OccurAnal ( occurAnalyseExpr )
import GHC.Types.Literal   ( litIsLifted ) --, mkLitInt ) -- temporalily commented out. See #8326
import GHC.Types.SourceText
import GHC.Types.Id
import GHC.Types.Id.Make   ( seqId )
import GHC.Core.Make       ( FloatBind, mkImpossibleExpr, castBottomExpr )
import qualified GHC.Core.Make
import GHC.Types.Id.Info
import GHC.Types.Name           ( mkSystemVarName, isExternalName, getOccFS )
import GHC.Core.Coercion hiding ( substCo, substCoVar )
import GHC.Core.Coercion.Opt    ( optCoercion )
import GHC.Core.FamInstEnv      ( FamInstEnv, topNormaliseType_maybe )
import GHC.Core.DataCon
   ( DataCon, dataConWorkId, dataConRepStrictness
   , dataConRepArgTys, isUnboxedTupleDataCon
   , StrictnessMark (..) )
import GHC.Core.Opt.Monad ( Tick(..), SimplMode(..) )
import GHC.Core
import GHC.Builtin.Types.Prim( realWorldStatePrimTy )
import GHC.Builtin.Names( runRWKey )
import GHC.Types.Demand ( DmdSig(..), Demand, dmdTypeDepth, isStrUsedDmd
                        , mkClosedDmdSig, topDmd, seqDmd, isDeadEndDiv )
import GHC.Types.Cpr    ( mkCprSig, botCpr )
import GHC.Core.Ppr     ( pprCoreExpr )
import GHC.Types.Unique ( hasKey )
import GHC.Core.Unfold
import GHC.Core.Unfold.Make
import GHC.Core.Utils
import GHC.Core.Opt.Arity ( ArityType(..)
                          , pushCoTyArg, pushCoValArg
                          , etaExpandAT )
import GHC.Core.SimpleOpt ( exprIsConApp_maybe, joinPointBinding_maybe, joinPointBindings_maybe )
import GHC.Core.FVs     ( mkRuleInfo )
import GHC.Core.Rules   ( lookupRule, getRules, initRuleOpts )
import GHC.Types.Basic
import GHC.Utils.Monad  ( mapAccumLM, liftIO )
import GHC.Utils.Logger
import GHC.Types.Tickish
import GHC.Types.Var    ( isTyCoVar )
import GHC.Data.Maybe   ( orElse )
import Control.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Constants (debugIsOn)
import GHC.Data.FastString
import GHC.Unit.Module ( moduleName, pprModuleName )
import GHC.Core.Multiplicity
import GHC.Builtin.PrimOps ( PrimOp (SeqOp) )


{-
The guts of the simplifier is in this module, but the driver loop for
the simplifier is in GHC.Core.Opt.Pipeline

Note [The big picture]
~~~~~~~~~~~~~~~~~~~~~~
The general shape of the simplifier is this:

  simplExpr :: SimplEnv -> InExpr -> SimplCont -> SimplM (SimplFloats, OutExpr)
  simplBind :: SimplEnv -> InBind -> SimplM (SimplFloats, SimplEnv)

 * SimplEnv contains
     - Simplifier mode (which includes DynFlags for convenience)
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


Note [Shadowing]
~~~~~~~~~~~~~~~~
The simplifier used to guarantee that the output had no shadowing, but
it does not do so any more.   (Actually, it never did!)  The reason is
documented with simplifyArgs.


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
                -- See note [Glomming] in "GHC.Core.Opt.OccurAnal".
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
      = simplRecBind env TopLevel Nothing pairs
    simpl_bind env (NonRec b r)
      = do { (env', b') <- addBndrRules env b (lookupRecBndr env b) Nothing
           ; simplRecOrTopPair env' TopLevel NonRecursive Nothing b b' r }

{-
************************************************************************
*                                                                      *
        Lazy bindings
*                                                                      *
************************************************************************

simplRecBind is used for
        * recursive bindings only
-}

simplRecBind :: SimplEnv -> TopLevelFlag -> MaybeJoinCont
             -> [(InId, InExpr)]
             -> SimplM (SimplFloats, SimplEnv)
simplRecBind env0 top_lvl mb_cont pairs0
  = do  { (env_with_info, triples) <- mapAccumLM add_rules env0 pairs0
        ; (rec_floats, env1) <- go env_with_info triples
        ; return (mkRecFloats rec_floats, env1) }
  where
    add_rules :: SimplEnv -> (InBndr,InExpr) -> SimplM (SimplEnv, (InBndr, OutBndr, InExpr))
        -- Add the (substituted) rules to the binder
    add_rules env (bndr, rhs)
        = do { (env', bndr') <- addBndrRules env bndr (lookupRecBndr env bndr) mb_cont
             ; return (env', (bndr, bndr', rhs)) }

    go env [] = return (emptyFloats env, env)

    go env ((old_bndr, new_bndr, rhs) : pairs)
        = do { (float, env1) <- simplRecOrTopPair env top_lvl Recursive mb_cont
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
                  -> TopLevelFlag -> RecFlag -> MaybeJoinCont
                  -> InId -> OutBndr -> InExpr  -- Binder and rhs
                  -> SimplM (SimplFloats, SimplEnv)

simplRecOrTopPair env top_lvl is_rec mb_cont old_bndr new_bndr rhs
  | Just env' <- preInlineUnconditionally env top_lvl old_bndr rhs env
  = {-#SCC "simplRecOrTopPair-pre-inline-uncond" #-}
    trace_bind "pre-inline-uncond" $
    do { tick (PreInlineUnconditionally old_bndr)
       ; return ( emptyFloats env, env' ) }

  | Just cont <- mb_cont
  = {-#SCC "simplRecOrTopPair-join" #-}
    assert (isNotTopLevel top_lvl && isJoinId new_bndr )
    trace_bind "join" $
    simplJoinBind env cont old_bndr new_bndr rhs env

  | otherwise
  = {-#SCC "simplRecOrTopPair-normal" #-}
    trace_bind "normal" $
    simplLazyBind env top_lvl is_rec old_bndr new_bndr rhs env

  where
    logger = seLogger env

    -- trace_bind emits a trace for each top-level binding, which
    -- helps to locate the tracing for inlining and rule firing
    trace_bind what thing_inside
      | not (logHasDumpFlag logger Opt_D_verbose_core2core)
      = thing_inside
      | otherwise
      = logTraceMsg logger ("SimplBind " ++ what)
         (ppr old_bndr) thing_inside

--------------------------
simplLazyBind :: SimplEnv
              -> TopLevelFlag -> RecFlag
              -> InId -> OutId          -- Binder, both pre-and post simpl
                                        -- Not a JoinId
                                        -- The OutId has IdInfo, except arity, unfolding
                                        -- Ids only, no TyVars
              -> InExpr -> SimplEnv     -- The RHS and its environment
              -> SimplM (SimplFloats, SimplEnv)
-- Precondition: not a JoinId
-- Precondition: rhs obeys the let/app invariant
-- NOT used for JoinIds
simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se
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
        ; (body_floats0, body0) <- {-#SCC "simplExprF" #-} simplExprF body_env body rhs_cont

              -- Never float join-floats out of a non-join let-binding (which this is)
              -- So wrap the body in the join-floats right now
              -- Hence: body_floats1 consists only of let-floats
        ; let (body_floats1, body1) = wrapJoinFloatsX body_floats0 body0

        -- ANF-ise a constructor or PAP rhs
        -- We get at most one float per argument here
        ; (let_floats, bndr2, body2) <- {-#SCC "prepareBinding" #-}
                                        prepareBinding env top_lvl bndr bndr1 body1
        ; let body_floats2 = body_floats1 `addLetFloats` let_floats

        ; (rhs_floats, rhs')
            <-  if not (doFloatFromRhs top_lvl is_rec False body_floats2 body2)
                then                    -- No floating, revert to body1
                     {-#SCC "simplLazyBind-no-floating" #-}
                     do { rhs' <- mkLam env tvs' (wrapFloats body_floats2 body1) rhs_cont
                        ; return (emptyFloats env, rhs') }

                else if null tvs then   -- Simple floating
                     {-#SCC "simplLazyBind-simple-floating" #-}
                     do { tick LetFloatFromLet
                        ; return (body_floats2, body2) }

                else                    -- Do type-abstraction first
                     {-#SCC "simplLazyBind-type-abstraction-first" #-}
                     do { tick LetFloatFromLet
                        ; (poly_binds, body3) <- abstractFloats (seUnfoldingOpts env) top_lvl
                                                                tvs' body_floats2 body2
                        ; let floats = foldl' extendFloats (emptyFloats env) poly_binds
                        ; rhs' <- mkLam env tvs' body3 rhs_cont
                        ; return (floats, rhs') }

        ; (bind_float, env2) <- completeBind (env `setInScopeFromF` rhs_floats)
                                             top_lvl Nothing bndr bndr2 rhs'
        ; return (rhs_floats `addFloats` bind_float, env2) }

--------------------------
simplJoinBind :: SimplEnv
              -> SimplCont
              -> InId -> OutId          -- Binder, both pre-and post simpl
                                        -- The OutId has IdInfo, except arity,
                                        --   unfolding
              -> InExpr -> SimplEnv     -- The right hand side and its env
              -> SimplM (SimplFloats, SimplEnv)
simplJoinBind env cont old_bndr new_bndr rhs rhs_se
  = do  { let rhs_env = rhs_se `setInScopeFromE` env
        ; rhs' <- simplJoinRhs rhs_env old_bndr rhs cont
        ; completeBind env NotTopLevel (Just cont) old_bndr new_bndr rhs' }

--------------------------
simplNonRecX :: SimplEnv
             -> InId            -- Old binder; not a JoinId
             -> OutExpr         -- Simplified RHS
             -> SimplM (SimplFloats, SimplEnv)
-- A specialised variant of simplNonRec used when the RHS is already
-- simplified, notably in knownCon.  It uses case-binding where necessary.
--
-- Precondition: rhs satisfies the let/app invariant

simplNonRecX env bndr new_rhs
  | assertPpr (not (isJoinId bndr)) (ppr bndr) $
    isDeadBinder bndr   -- Not uncommon; e.g. case (a,b) of c { (p,q) -> p }
  = return (emptyFloats env, env)    --  Here c is dead, and we avoid
                                         --  creating the binding c = (a,b)

  | Coercion co <- new_rhs
  = return (emptyFloats env, extendCvSubst env bndr co)

  | otherwise
  = do  { (env', bndr') <- simplBinder env bndr
        ; completeNonRecX NotTopLevel env' (isStrictId bndr') bndr bndr' new_rhs }
          -- NotTopLevel: simplNonRecX is only used for NotTopLevel things
          --
          -- isStrictId: use bndr' because in a representation-polymorphic
          -- setting, the InId bndr might have a representation-polymorphic
          -- type, which isStrictId doesn't expect
          -- c.f. Note [Dark corner with representation polymorphism]

--------------------------
completeNonRecX :: TopLevelFlag -> SimplEnv
                -> Bool
                -> InId                 -- Old binder; not a JoinId
                -> OutId                -- New binder
                -> OutExpr              -- Simplified RHS
                -> SimplM (SimplFloats, SimplEnv)    -- The new binding is in the floats
-- Precondition: rhs satisfies the let/app invariant
--               See Note [Core let/app invariant] in GHC.Core

completeNonRecX top_lvl env is_strict old_bndr new_bndr new_rhs
  = assertPpr (not (isJoinId new_bndr)) (ppr new_bndr) $
    do  { (prepd_floats, new_bndr, new_rhs)
              <- prepareBinding env top_lvl old_bndr new_bndr new_rhs
        ; let floats = emptyFloats env `addLetFloats` prepd_floats
        ; (rhs_floats, rhs2) <-
                if doFloatFromRhs NotTopLevel NonRecursive is_strict floats new_rhs
                then    -- Add the floats to the main env
                     do { tick LetFloatFromLet
                        ; return (floats, new_rhs) }
                else    -- Do not float; wrap the floats around the RHS
                     return (emptyFloats env, wrapFloats floats new_rhs)

        ; (bind_float, env2) <- completeBind (env `setInScopeFromF` rhs_floats)
                                             NotTopLevel Nothing
                                             old_bndr new_bndr rhs2
        ; return (rhs_floats `addFloats` bind_float, env2) }


{- *********************************************************************
*                                                                      *
           prepareBinding, prepareRhs, makeTrivial
*                                                                      *
************************************************************************

Note [Cast worker/wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have a binding
   x = e |> co
we want to do something very similar to worker/wrapper:
   $wx = e
   x = $wx |> co

So now x can be inlined freely.  There's a chance that e will be a
constructor application or function, or something like that, so moving
the coercion to the usage site may well cancel the coercions and lead
to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          t = T m
          go 0 = 0
          go n = case t of { T m -> go (n-m) }
                -- This case should optimise

We call this making a cast worker/wrapper, and it's done by prepareBinding.

We need to be careful with inline/noinline pragmas:
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
It's exactly like worker/wrapper for strictness analysis:
  f is the wrapper and must inline like crazy
  $wf is the worker and must carry f's original pragma
See Note [Worker/wrapper for NOINLINE functions] in
GHC.Core.Opt.WorkWrap.

See #17673, #18093, #18078.

Note [Preserve strictness in cast w/w]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the Note [Cast worker/wrappers] transformation, keep the strictness info.
Eg
        f = e `cast` co    -- f has strictness SSL
When we transform to
        f' = e             -- f' also has strictness SSL
        f = f' `cast` co   -- f still has strictness SSL

Its not wrong to drop it on the floor, but better to keep it.

Note [Cast w/w: unlifted]
~~~~~~~~~~~~~~~~~~~~~~~~~
BUT don't do cast worker/wrapper if 'e' has an unlifted type.
This *can* happen:

     foo :: Int = (error (# Int,Int #) "urk")
                  `cast` CoUnsafe (# Int,Int #) Int

If do the makeTrivial thing to the error call, we'll get
    foo = case error (# Int,Int #) "urk" of v -> v `cast` ...
But 'v' isn't in scope!

These strange casts can happen as a result of case-of-case
        bar = case (case x of { T -> (# 2,3 #); F -> error "urk" }) of
                (# p,q #) -> p+q

NOTE: Nowadays we don't use casts for these error functions;
instead, we use (case erorr ... of {}). So I'm not sure
this Note makes much sense any more.
-}

prepareBinding :: SimplEnv -> TopLevelFlag
               -> InId -> OutId -> OutExpr
               -> SimplM (LetFloats, OutId, OutExpr)

prepareBinding env top_lvl old_bndr bndr rhs
  | Cast rhs1 co <- rhs
    -- Try for cast worker/wrapper
    -- See Note [Cast worker/wrappers]
  , not (isStableUnfolding (realIdUnfolding old_bndr))
        -- Don't make a cast w/w if the thing is going to be inlined anyway
  , not (exprIsTrivial rhs1)
        -- Nor if the RHS is trivial; then again it'll be inlined
  , let ty1 = coercionLKind co
  , not (isUnliftedType ty1)
        -- Not if rhs has an unlifted type; see Note [Cast w/w: unlifted]
  = do { (floats, new_id) <- makeTrivialBinding (getMode env) top_lvl
                                   (getOccFS bndr) worker_info rhs1 ty1
       ; let bndr' = bndr `setInlinePragma` mkCastWrapperInlinePrag (idInlinePragma bndr)
       ; return (floats, bndr', Cast (Var new_id) co) }

  | otherwise
  = do { (floats, rhs') <- prepareRhs (getMode env) top_lvl (getOccFS bndr) rhs
       ; return (floats, bndr, rhs') }
 where
   info = idInfo bndr
   worker_info = vanillaIdInfo `setDmdSigInfo` dmdSigInfo info
                               `setCprSigInfo`        cprSigInfo info
                               `setDemandInfo`     demandInfo info
                               `setInlinePragInfo` inlinePragInfo info
                               `setArityInfo`      arityInfo info
          -- We do /not/ want to transfer OccInfo, Rules, Unfolding
          -- Note [Preserve strictness in cast w/w]

mkCastWrapperInlinePrag :: InlinePragma -> InlinePragma
-- See Note [Cast wrappers]
mkCastWrapperInlinePrag (InlinePragma { inl_act = act, inl_rule = rule_info })
  = InlinePragma { inl_src    = SourceText "{-# INLINE"
                 , inl_inline = NoUserInlinePrag -- See Note [Wrapper NoUserInline]
                 , inl_sat    = Nothing      --     in GHC.Core.Opt.WorkWrap
                 , inl_act    = wrap_act     -- See Note [Wrapper activation]
                 , inl_rule   = rule_info }  --     in GHC.Core.Opt.WorkWrap
                                -- RuleMatchInfo is (and must be) unaffected
  where
    -- See Note [Wrapper activation] in GHC.Core.Opt.WorkWrap
    -- But simpler, because we don't need to disable during InitialPhase
    wrap_act | isNeverActive act = activateDuringFinal
             | otherwise         = act

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

prepareRhs :: SimplMode -> TopLevelFlag
           -> FastString    -- Base for any new variables
           -> OutExpr
           -> SimplM (LetFloats, OutExpr)
-- Transforms a RHS into a better RHS by ANF'ing args
-- for expandable RHSs: constructors and PAPs
-- e.g        x = Just e
-- becomes    a = e
--            x = Just a
-- See Note [prepareRhs]
prepareRhs mode top_lvl occ rhs0
  = do  { (_is_exp, floats, rhs1) <- go 0 rhs0
        ; return (floats, rhs1) }
  where
    go :: Int -> OutExpr -> SimplM (Bool, LetFloats, OutExpr)
    go n_val_args (Cast rhs co)
        = do { (is_exp, floats, rhs') <- go n_val_args rhs
             ; return (is_exp, floats, Cast rhs' co) }
    go n_val_args (App fun (Type ty))
        = do { (is_exp, floats, rhs') <- go n_val_args fun
             ; return (is_exp, floats, App rhs' (Type ty)) }
    go n_val_args (App fun arg)
        = do { (is_exp, floats1, fun') <- go (n_val_args+1) fun
             ; case is_exp of
                False -> return (False, emptyLetFloats, App fun arg)
                True  -> do { (floats2, arg') <- makeTrivial mode top_lvl topDmd occ arg
                            ; return (True, floats1 `addLetFlts` floats2, App fun' arg') } }
    go n_val_args (Var fun)
        = return (is_exp, emptyLetFloats, Var fun)
        where
          is_exp = isExpandableApp fun n_val_args   -- The fun a constructor or PAP
                        -- See Note [CONLIKE pragma] in GHC.Types.Basic
                        -- The definition of is_exp should match that in
                        -- 'GHC.Core.Opt.OccurAnal.occAnalApp'

    go n_val_args (Tick t rhs)
        -- We want to be able to float bindings past this
        -- tick. Non-scoping ticks don't care.
        | tickishScoped t == NoScope
        = do { (is_exp, floats, rhs') <- go n_val_args rhs
             ; return (is_exp, floats, Tick t rhs') }

        -- On the other hand, for scoping ticks we need to be able to
        -- copy them on the floats, which in turn is only allowed if
        -- we can obtain non-counting ticks.
        | (not (tickishCounts t) || tickishCanSplit t)
        = do { (is_exp, floats, rhs') <- go n_val_args rhs
             ; let tickIt (id, expr) = (id, mkTick (mkNoCount t) expr)
                   floats' = mapLetFloats floats tickIt
             ; return (is_exp, floats', Tick t rhs') }

    go _ other
        = return (False, emptyLetFloats, other)

makeTrivialArg :: SimplMode -> ArgSpec -> SimplM (LetFloats, ArgSpec)
makeTrivialArg mode arg@(ValArg { as_arg = e, as_dmd = dmd })
  = do { (floats, e') <- makeTrivial mode NotTopLevel dmd (fsLit "arg") e
       ; return (floats, arg { as_arg = e' }) }
makeTrivialArg _ arg
  = return (emptyLetFloats, arg)  -- CastBy, TyArg

makeTrivial :: SimplMode -> TopLevelFlag -> Demand
            -> FastString  -- ^ A "friendly name" to build the new binder from
            -> OutExpr     -- ^ This expression satisfies the let/app invariant
            -> SimplM (LetFloats, OutExpr)
-- Binds the expression to a variable, if it's not trivial, returning the variable
-- For the Demand argument, see Note [Keeping demand info in StrictArg Plan A]
makeTrivial mode top_lvl dmd occ_fs expr
  | exprIsTrivial expr                          -- Already trivial
  || not (bindingOk top_lvl expr expr_ty)       -- Cannot trivialise
                                                --   See Note [Cannot trivialise]
  = return (emptyLetFloats, expr)

  | Cast expr' co <- expr
  = do { (floats, triv_expr) <- makeTrivial mode top_lvl dmd occ_fs expr'
       ; return (floats, Cast triv_expr co) }

  | otherwise
  = do { (floats, new_id) <- makeTrivialBinding mode top_lvl occ_fs
                                                id_info expr expr_ty
       ; return (floats, Var new_id) }
  where
    id_info = vanillaIdInfo `setDemandInfo` dmd
    expr_ty = exprType expr

makeTrivialBinding :: SimplMode -> TopLevelFlag
                   -> FastString  -- ^ a "friendly name" to build the new binder from
                   -> IdInfo
                   -> OutExpr     -- ^ This expression satisfies the let/app invariant
                   -> OutType     -- Type of the expression
                   -> SimplM (LetFloats, OutId)
makeTrivialBinding mode top_lvl occ_fs info expr expr_ty
  = do  { (floats, expr1) <- prepareRhs mode top_lvl occ_fs expr
        ; uniq <- getUniqueM
        ; let name = mkSystemVarName uniq occ_fs
              var  = mkLocalIdWithInfo name Many expr_ty info

        -- Now something very like completeBind,
        -- but without the postInlineUnconditionally part
        ; (arity_type, expr2) <- tryEtaExpandRhs mode var expr1
        ; unf <- mkLetUnfolding (sm_uf_opts mode) top_lvl InlineRhs var expr2

        ; let final_id = addLetBndrInfo var arity_type unf
              bind     = NonRec final_id expr2

        ; return ( floats `addLetFlts` unitLetFloat bind, final_id ) }

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

It does *not* attempt to do let-to-case.  Why?  Because it is used for
  - top-level bindings (when let-to-case is impossible)
  - many situations where the "rhs" is known to be a WHNF
                (so let-to-case is inappropriate).

Nor does it do the atomic-argument thing
-}

completeBind :: SimplEnv
             -> TopLevelFlag            -- Flag stuck into unfolding
             -> MaybeJoinCont           -- Required only for join point
             -> InId                    -- Old binder
             -> OutId -> OutExpr        -- New binder and RHS
             -> SimplM (SimplFloats, SimplEnv)
-- completeBind may choose to do its work
--      * by extending the substitution (e.g. let x = y in ...)
--      * or by adding to the floats in the envt
--
-- Binder /can/ be a JoinId
-- Precondition: rhs obeys the let/app invariant
completeBind env top_lvl mb_cont old_bndr new_bndr new_rhs
 | isCoVar old_bndr
 = case new_rhs of
     Coercion co -> return (emptyFloats env, extendCvSubst env old_bndr co)
     _           -> return (mkFloatBind env (NonRec new_bndr new_rhs))

 | otherwise
 = assert (isId new_bndr) $
   do { let old_info = idInfo old_bndr
            old_unf  = unfoldingInfo old_info
            occ_info = occInfo old_info

         -- Do eta-expansion on the RHS of the binding
         -- See Note [Eta-expanding at let bindings] in GHC.Core.Opt.Simplify.Utils
      ; (new_arity, final_rhs) <- tryEtaExpandRhs (getMode env) new_bndr new_rhs

        -- Simplify the unfolding
      ; new_unfolding <- simplLetUnfolding env top_lvl mb_cont old_bndr
                          final_rhs (idType new_bndr) new_arity old_unf

      ; let final_bndr = addLetBndrInfo new_bndr new_arity new_unfolding
        -- See Note [In-scope set as a substitution]

      ; if postInlineUnconditionally env top_lvl final_bndr occ_info final_rhs

        then -- Inline and discard the binding
             do  { tick (PostInlineUnconditionally old_bndr)
                 ; return ( emptyFloats env
                          , extendIdSubst env old_bndr $
                            DoneEx final_rhs (isJoinId_maybe new_bndr)) }
                -- Use the substitution to make quite, quite sure that the
                -- substitution will happen, since we are going to discard the binding

        else -- Keep the binding
             -- pprTrace "Binding" (ppr final_bndr <+> ppr new_unfolding) $
             return (mkFloatBind env (NonRec final_bndr final_rhs)) }

addLetBndrInfo :: OutId -> ArityType -> Unfolding -> OutId
addLetBndrInfo new_bndr new_arity_type new_unf
  = new_bndr `setIdInfo` info5
  where
    AT oss div = new_arity_type
    new_arity  = length oss

    info1 = idInfo new_bndr `setArityInfo` new_arity

    -- Unfolding info: Note [Setting the new unfolding]
    info2 = info1 `setUnfoldingInfo` new_unf

    -- Demand info: Note [Setting the demand info]
    -- We also have to nuke demand info if for some reason
    -- eta-expansion *reduces* the arity of the binding to less
    -- than that of the strictness sig. This can happen: see Note [Arity decrease].
    info3 | isEvaldUnfolding new_unf
            || (case dmdSigInfo info2 of
                  DmdSig dmd_ty -> new_arity < dmdTypeDepth dmd_ty)
          = zapDemandInfo info2 `orElse` info2
          | otherwise
          = info2

    -- Bottoming bindings: see Note [Bottoming bindings]
    info4 | isDeadEndDiv div = info3 `setDmdSigInfo` bot_sig
                                     `setCprSigInfo`        bot_cpr
          | otherwise        = info3

    bot_sig = mkClosedDmdSig (replicate new_arity topDmd) div
    bot_cpr = mkCprSig new_arity botCpr

     -- Zap call arity info. We have used it by now (via
     -- `tryEtaExpandRhs`), and the simplifier can invalidate this
     -- information, leading to broken code later (e.g. #13479)
    info5 = zapCallArityInfo info4


{- Note [Arity decrease]
~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking the arity of a binding should not decrease.  But it *can*
legitimately happen because of RULES.  Eg
        f = g @Int
where g has arity 2, will have arity 2.  But if there's a rewrite rule
        g @Int --> h
where h has arity 1, then f's arity will decrease.  Here's a real-life example,
which is in the output of Specialise:

     Rec {
        $dm {Arity 2} = \d.\x. op d
        {-# RULES forall d. $dm Int d = $s$dm #-}

        dInt = MkD .... opInt ...
        opInt {Arity 1} = $dm dInt

        $s$dm {Arity 0} = \x. op dInt }

Here opInt has arity 1; but when we apply the rule its arity drops to 0.
That's why Specialise goes to a little trouble to pin the right arity
on specialised functions too.

Note [Bottoming bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   let x = error "urk"
   in ...(case x of <alts>)...
or
   let f = \x. error (x ++ "urk")
   in ...(case f "foo" of <alts>)...

Then we'd like to drop the dead <alts> immediately.  So it's good to
propagate the info that x's RHS is bottom to x's IdInfo as rapidly as
possible.

We use tryEtaExpandRhs on every binding, and it turns out that the
arity computation it performs (via GHC.Core.Opt.Arity.findRhsArity) already
does a simple bottoming-expression analysis.  So all we need to do
is propagate that info to the binder's IdInfo.

This showed up in #12150; see comment:16.

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
  = -- pprTrace "simplExprC" (ppr expr $$ ppr cont {- $$ ppr (seIdSubst env) -} $$ ppr (seLetFloats env) ) $
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

simplExprF1 :: SimplEnv -> InExpr -> SimplCont
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
    simplLam env zapped_bndrs body cont
        -- The main issue here is under-saturated lambdas
        --   (\x1. \x2. e) arg1
        -- Here x1 might have "occurs-once" occ-info, because occ-info
        -- is computed assuming that a group of lambdas is applied
        -- all at once.  If there are too few args, we must zap the
        -- occ-info, UNLESS the remaining binders are one-shot
  where
    (bndrs, body) = collectBinders expr
    zapped_bndrs = zapLamBndrs n_args bndrs
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

  | Just (bndr', rhs') <- joinPointBinding_maybe bndr rhs
  = {-#SCC "simplNonRecJoinPoint" #-} simplNonRecJoinPoint env bndr' rhs' body cont

  | otherwise
  = {-#SCC "simplNonRecE" #-} simplNonRecE env bndr (rhs, env) ([], body) cont

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
  | Just arity <- isJoinId_maybe bndr
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
  = do { opts <- getOptCoercionOpts
       ; let opt_co = optCoercion opts (getTCvSubst env) co
       ; seqCo opt_co `seq` return opt_co }

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
    | Breakpoint ext n ids <- tickish
          = Breakpoint ext n (map (getDoneId . substId env) ids)
    | otherwise = tickish

  -- Push type application and coercion inside a tick
  splitCont :: SimplCont -> (SimplCont, SimplCont)
  splitCont cont@(ApplyToTy { sc_cont = tail }) = (cont { sc_cont = inc }, outc)
    where (inc,outc) = splitCont tail
  splitCont (CastIt co c) = (CastIt co inc, outc)
    where (inc,outc) = splitCont c
  splitCont other = (mkBoringStop (contHoleType other), other)

  getDoneId (DoneId id)  = id
  getDoneId (DoneEx e _) = getIdFromTrivialExpr e -- Note [substTickish] in GHC.Core.Subst
  getDoneId other = pprPanic "getDoneId" (ppr other)

-- Note [case-of-scc-of-case]
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
      StrictBind { sc_bndr = b, sc_bndrs = bs, sc_body = body
                 , sc_env = se, sc_cont = cont }
        -> do { (floats1, env') <- simplNonRecX (se `setInScopeFromE` env) b expr
                                  -- expr satisfies let/app since it started life
                                  -- in a call to simplNonRecE
              ; (floats2, expr') <- simplLam env' bs body cont
              ; return (floats1 `addFloats` floats2, expr') }

      ApplyToTy  { sc_arg_ty = ty, sc_cont = cont}
        -> rebuild env (App expr (Type ty)) cont

      ApplyToVal { sc_arg = arg, sc_env = se, sc_dup = dup_flag, sc_cont = cont}
        -- See Note [Avoid redundant simplification]
        -> do { (_, _, arg') <- simplArg env dup_flag se arg
              ; rebuild env (App expr arg') cont }

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
   build up NthCo stacks.  Silly to do that if co is reflexive.

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
                                      , sc_dup = dup, sc_cont = tail })
          | Just (m_co1, m_co2) <- pushCoValArg co
          , levity_ok m_co1
          = {-#SCC "addCoerce-pushCoValArg" #-}
            do { tail' <- addCoerceM m_co2 tail
               ; case m_co1 of {
                   MRefl -> return (cont { sc_cont = tail'
                                         , sc_hole_ty = coercionLKind co }) ;
                      -- Avoid simplifying if possible;
                      -- See Note [Avoiding exponential behaviour]

                   MCo co1 ->
            do { (dup', arg_se', arg') <- simplArg env dup arg_se arg
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

        levity_ok :: MCoercionR -> Bool
        levity_ok MRefl = True
        levity_ok (MCo co) = not $ isTypeLevPoly $ coercionRKind co
          -- Without this check, we get a lev-poly arg
          -- See Note [Representation polymorphism invariants] in GHC.Core
          -- test: typecheck/should_run/EtaExpandLevPoly

simplArg :: SimplEnv -> DupFlag -> StaticEnv -> CoreExpr
         -> SimplM (DupFlag, StaticEnv, OutExpr)
simplArg env dup_flag arg_env arg
  | isSimplified dup_flag
  = return (dup_flag, arg_env, arg)
  | otherwise
  = do { arg' <- simplExpr (arg_env `setInScopeFromE` env) arg
       ; return (Simplified, zapSubstEnv arg_env, arg') }

{-
************************************************************************
*                                                                      *
\subsection{Lambdas}
*                                                                      *
************************************************************************
-}

simplLam :: SimplEnv -> [InId] -> InExpr -> SimplCont
         -> SimplM (SimplFloats, OutExpr)

simplLam env [] body cont
  = simplExprF env body cont

simplLam env (bndr:bndrs) body (ApplyToTy { sc_arg_ty = arg_ty, sc_cont = cont })
  = do { tick (BetaReduction bndr)
       ; simplLam (extendTvSubst env bndr arg_ty) bndrs body cont }

simplLam env (bndr:bndrs) body (ApplyToVal { sc_arg = arg, sc_env = arg_se
                                           , sc_cont = cont, sc_dup = dup })
  | isSimplified dup  -- Don't re-simplify if we've simplified it once
                      -- See Note [Avoiding exponential behaviour]
  = do  { tick (BetaReduction bndr)
        ; (floats1, env') <- simplNonRecX env bndr arg
        ; (floats2, expr') <- simplLam env' bndrs body cont
        ; return (floats1 `addFloats` floats2, expr') }

  | otherwise
  = do  { tick (BetaReduction bndr)
        ; simplNonRecE env bndr (arg, arg_se) (bndrs, body) cont }

      -- Discard a non-counting tick on a lambda.  This may change the
      -- cost attribution slightly (moving the allocation of the
      -- lambda elsewhere), but we don't care: optimisation changes
      -- cost attribution all the time.
simplLam env bndrs body (TickIt tickish cont)
  | not (tickishCounts tickish)
  = simplLam env bndrs body cont

        -- Not enough args, so there are real lambdas left to put in the result
simplLam env bndrs body cont
  = do  { (env', bndrs') <- simplLamBndrs env bndrs
        ; body' <- simplExpr env' body
        ; new_lam <- mkLam env bndrs' body' cont
        ; rebuild env' new_lam cont }

-------------
simplLamBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- Historically this had a special case for when a lambda-binder
-- could have a stable unfolding;
-- see Historical Note [Case binders and join points]
-- But now it is much simpler!
simplLamBndr env bndr = simplBinder env bndr

simplLamBndrs :: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplLamBndrs env bndrs = mapAccumLM simplLamBndr env bndrs

------------------
simplNonRecE :: SimplEnv
             -> InId                    -- The binder, always an Id
                                        -- Never a join point
             -> (InExpr, SimplEnv)      -- Rhs of binding (or arg of lambda)
             -> ([InBndr], InExpr)      -- Body of the let/lambda
                                        --      \xs.e
             -> SimplCont
             -> SimplM (SimplFloats, OutExpr)

-- simplNonRecE is used for
--  * non-top-level non-recursive non-join-point lets in expressions
--  * beta reduction
--
-- simplNonRec env b (rhs, rhs_se) (bs, body) k
--   = let env in
--     cont< let b = rhs_se(rhs) in \bs.body >
--
-- It deals with strict bindings, via the StrictBind continuation,
-- which may abort the whole process
--
-- Precondition: rhs satisfies the let/app invariant
--               Note [Core let/app invariant] in GHC.Core
--
-- The "body" of the binding comes as a pair of ([InId],InExpr)
-- representing a lambda; so we recurse back to simplLam
-- Why?  Because of the binder-occ-info-zapping done before
--       the call to simplLam in simplExprF (Lam ...)

simplNonRecE env bndr (rhs, rhs_se) (bndrs, body) cont
  | assert (isId bndr && not (isJoinId bndr) ) True
  , Just env' <- preInlineUnconditionally env NotTopLevel bndr rhs rhs_se
  = do { tick (PreInlineUnconditionally bndr)
       ; -- pprTrace "preInlineUncond" (ppr bndr <+> ppr rhs) $
         simplLam env' bndrs body cont }

  | otherwise
  = do { (env1, bndr1) <- simplNonRecBndr env bndr

       -- Deal with strict bindings
       -- See Note [Dark corner with representation polymorphism]
       ; if isStrictId bndr1 && sm_case_case (getMode env)
         then simplExprF (rhs_se `setInScopeFromE` env) rhs
                   (StrictBind { sc_bndr = bndr, sc_bndrs = bndrs, sc_body = body
                               , sc_env = env, sc_cont = cont, sc_dup = NoDup })

       -- Deal with lazy bindings
         else do
       { (env2, bndr2) <- addBndrRules env1 bndr bndr1 Nothing
       ; (floats1, env3) <- simplLazyBind env2 NotTopLevel NonRecursive bndr bndr2 rhs rhs_se
       ; (floats2, expr') <- simplLam env3 bndrs body cont
       ; return (floats1 `addFloats` floats2, expr') } }

------------------
simplRecE :: SimplEnv
          -> [(InId, InExpr)]
          -> InExpr
          -> SimplCont
          -> SimplM (SimplFloats, OutExpr)

-- simplRecE is used for
--  * non-top-level recursive lets in expressions
simplRecE env pairs body cont
  = do  { let bndrs = map fst pairs
        ; massert (all (not . isJoinId) bndrs)
        ; env1 <- simplRecBndrs env bndrs
                -- NB: bndrs' don't have unfoldings or rules
                -- We add them as we go down
        ; (floats1, env2) <- simplRecBind env1 NotTopLevel Nothing pairs
        ; (floats2, expr') <- simplExprF env2 body cont
        ; return (floats1 `addFloats` floats2, expr') }

{- Note [Dark corner with representation polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `simplNonRecE`, the call to `isStrictId` will fail if the binder
has a representation-polymorphic type, of kind (TYPE r).  So we are careful to
call `isStrictId` on the OutId, not the InId, in case we have
     ((\(r::RuntimeRep) \(x::Type r). blah) Lifted arg)
That will lead to `simplNonRecE env (x::Type r) arg`, and we can't tell
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One way in which we can get exponential behaviour is if we simplify a
big expression, and the re-simplify it -- and then this happens in a
deeply-nested way.  So we must be jolly careful about re-simplifying
an expression.  That is why completeNonRecX does not try
preInlineUnconditionally.

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

type MaybeJoinCont = Maybe SimplCont
  -- Nothing => Not a join point
  -- Just k  => This is a join binding with continuation k
  -- See Note [Rules and unfolding for join points]

simplNonRecJoinPoint :: SimplEnv -> InId -> InExpr
                     -> InExpr -> SimplCont
                     -> SimplM (SimplFloats, OutExpr)
simplNonRecJoinPoint env bndr rhs body cont
  | assert (isJoinId bndr ) True
  , Just env' <- preInlineUnconditionally env NotTopLevel bndr rhs env
  = do { tick (PreInlineUnconditionally bndr)
       ; simplExprF env' body cont }

   | otherwise
   = wrapJoinCont env cont $ \ env cont ->
     do { -- We push join_cont into the join RHS and the body;
          -- and wrap wrap_cont around the whole thing
        ; let mult   = contHoleScaling cont
              res_ty = contResultType cont
        ; (env1, bndr1)    <- simplNonRecJoinBndr env bndr mult res_ty
        ; (env2, bndr2)    <- addBndrRules env1 bndr bndr1 (Just cont)
        ; (floats1, env3)  <- simplJoinBind env2 cont bndr bndr2 rhs env
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
       ; (floats1, env2)  <- simplRecBind env1 NotTopLevel (Just cont) pairs
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

  | not (sm_case_case (getMode env))
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
trimJoinCont :: Id -> Maybe JoinArity -> SimplCont -> SimplCont
-- Drop outer context from join point invocation (jump)
-- See Note [Join points and case-of-case]

trimJoinCont _ Nothing cont
  = cont -- Not a jump
trimJoinCont var (Just arity) cont
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
  = case substId env var of
      ContEx tvs cvs ids e ->
          let env' = setSubstEnv env tvs cvs ids
          in simplExprF env' e cont
          -- Don't trim; haven't already simplified e,
          -- so the cont is not embodied in e

      DoneId var1 ->
          let cont' = trimJoinCont var (isJoinId_maybe var1) cont
          in completeCall env var1 cont'

      DoneEx e mb_join ->
          let env' = zapSubstEnv env
              cont' = trimJoinCont var mb_join cont
          in simplExprF env' e cont'
              -- Note [zapSubstEnv]
              -- The template is already simplified, so don't re-substitute.
              -- This is VITAL.  Consider
              --      let x = e in
              --      let y = \z -> ...x... in
              --      \ x -> ...y...
              -- We'll clone the inner \x, adding x->x' in the id_subst
              -- Then when we inline y, we must *not* replace x by x' in
              -- the inlined copy!!

---------------------------------------------------------
--      Dealing with a call site

completeCall :: SimplEnv -> OutId -> SimplCont -> SimplM (SimplFloats, OutExpr)
completeCall env var cont
  | Just expr <- callSiteInline logger uf_opts in_scope case_depth var active_unf
                                lone_variable arg_infos interesting_cont
  -- Inline the variable's RHS
  = do { checkedTick (UnfoldingDone var)
       ; dump_inline expr cont
       ; let env1 = zapSubstEnv env
       ; simplExprF env1 expr cont }

  | otherwise
  -- Don't inline; instead rebuild the call
  = do { rule_base <- getSimplRules
       ; let rules = getRules rule_base var
             info = mkArgInfo env var rules
                              n_val_args call_cont
       ; rebuildCall env info cont }

  where
    uf_opts    = seUnfoldingOpts env
    case_depth = seCaseDepth env
    in_scope   = seInScope env
    logger     = seLogger env
    (lone_variable, arg_infos, call_cont) = contArgs cont
    n_val_args       = length arg_infos
    interesting_cont = interestingCallContext env call_cont
    active_unf       = activeUnfolding (getMode env) var

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

rebuildCall :: SimplEnv
            -> ArgInfo
            -> SimplCont
            -> SimplM (SimplFloats, OutExpr)
-- We decided not to inline, so
--    - simplify the arguments
--    - try rewrite rules
--    - and rebuild

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

---------- Try rewrite RULES --------------
-- See Note [Trying rewrite rules]
rebuildCall env info@(ArgInfo { ai_fun = fun, ai_args = rev_args
                              , ai_rules = Just (nr_wanted, rules) }) cont
  | nr_wanted == 0 || no_more_args
  , let info' = info { ai_rules = Nothing }
  = -- We've accumulated a simplified call in <fun,rev_args>
    -- so try rewrite rules; see Note [RULEs apply to simplified arguments]
    -- See also Note [Rules for recursive functions]
    do { mb_match <- tryRules env rules fun (reverse rev_args) cont
       ; case mb_match of
             Just (env', rhs, cont') -> simplExprF env' rhs cont'
             Nothing                 -> rebuildCall env info' cont }
  where
    no_more_args = case cont of
                      ApplyToTy  {} -> False
                      ApplyToVal {} -> False
                      _             -> True


---------- Simplify applications and casts --------------
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
  , not (contIsStop cont)  -- Don't fiddle around if the continuation is boring
  , [ TyArg {}, TyArg {} ] <- rev_args
  = do { s <- newId (fsLit "s") Many realWorldStatePrimTy
       ; let (m,_,_) = splitFunTy fun_ty
             env'  = (arg_se `setInScopeFromE` env) `addNewInScopeIds` [s]
             ty'   = contResultType cont
             cont' = ApplyToVal { sc_dup = Simplified, sc_arg = Var s
                                , sc_env = env', sc_cont = cont
                                , sc_hole_ty = mkVisFunTy m realWorldStatePrimTy ty' }
                     -- cont' applies to s, then K
       ; body' <- simplExprC env' arg cont'
       ; let arg'  = Lam s body'
             rr'   = getRuntimeRep ty'
             call' = mkApps (Var fun_id) [mkTyArg rr', mkTyArg ty', arg']
       ; return (emptyFloats env, call') }

rebuildCall env fun_info
            (ApplyToVal { sc_arg = arg, sc_env = arg_se
                        , sc_dup = dup_flag, sc_hole_ty = fun_ty
                        , sc_cont = cont })
  -- Argument is already simplified
  | isSimplified dup_flag     -- See Note [Avoid redundant simplification]
  = rebuildCall env (addValArgTo fun_info arg fun_ty) cont

  -- Strict arguments
  | isStrictArgInfo fun_info
  , sm_case_case (getMode env)
  = -- pprTrace "Strict Arg" (ppr arg $$ ppr (seIdSubst env) $$ ppr (seInScope env)) $
    simplExprF (arg_se `setInScopeFromE` env) arg
               (StrictArg { sc_fun = fun_info, sc_fun_ty = fun_ty
                          , sc_dup = Simplified
                          , sc_cont = cont })
                -- Note [Shadowing]

  -- Lazy arguments
  | otherwise
        -- DO NOT float anything outside, hence simplExprC
        -- There is no benefit (unlike in a let-binding), and we'd
        -- have to be very careful about bogus strictness through
        -- floating a demanded let.
  = do  { arg' <- simplExprC (arg_se `setInScopeFromE` env) arg
                             (mkLazyArgStop arg_ty (lazyArgContext fun_info))
        ; rebuildCall env (addValArgTo fun_info  arg' fun_ty) cont }
  where
    arg_ty = funArgTy fun_ty


---------- No further useful info, revert to generic rebuild ------------
rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args }) cont
  = rebuild env (argInfoExpr fun rev_args) cont

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

Note [Avoid redundant simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because RULES apply to simplified arguments, there's a danger of repeatedly
simplifying already-simplified arguments.  An important example is that of
        (>>=) d e1 e2
Here e1, e2 are simplified before the rule is applied, but don't really
participate in the rule firing. So we mark them as Simplified to avoid
re-simplifying them.

Note [Shadowing]
~~~~~~~~~~~~~~~~
This part of the simplifier may break the no-shadowing invariant
Consider
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


************************************************************************
*                                                                      *
                Rewrite rules
*                                                                      *
************************************************************************
-}

tryRules :: SimplEnv -> [CoreRule]
         -> Id -> [ArgSpec]
         -> SimplCont
         -> SimplM (Maybe (SimplEnv, CoreExpr, SimplCont))

tryRules env rules fn args call_cont
  | null rules
  = return Nothing

{- Disabled until we fix #8326
  | fn `hasKey` tagToEnumKey   -- See Note [Optimising tagToEnum#]
  , [_type_arg, val_arg] <- args
  , Select dup bndr ((_,[],rhs1) : rest_alts) se cont <- call_cont
  , isDeadBinder bndr
  = do { let enum_to_tag :: CoreAlt -> CoreAlt
                -- Takes   K -> e  into   tagK# -> e
                -- where tagK# is the tag of constructor K
             enum_to_tag (DataAlt con, [], rhs)
               = assert (isEnumerationTyCon (dataConTyCon con) )
                (LitAlt tag, [], rhs)
              where
                tag = mkLitInt dflags (toInteger (dataConTag con - fIRST_TAG))
             enum_to_tag alt = pprPanic "tryRules: tagToEnum" (ppr alt)

             new_alts = (DEFAULT, [], rhs1) : map enum_to_tag rest_alts
             new_bndr = setIdType bndr intPrimTy
                 -- The binder is dead, but should have the right type
      ; return (Just (val_arg, Select dup new_bndr new_alts se cont)) }
-}

  | Just (rule, rule_rhs) <- lookupRule ropts (getUnfoldingInRuleMatch env)
                                        (activeRule (getMode env)) fn
                                        (argInfoAppArgs args) rules
  -- Fire a rule for the function
  = do { checkedTick (RuleFired (ruleName rule))
       ; let cont' = pushSimplifiedArgs zapped_env
                                        (drop (ruleArity rule) args)
                                        call_cont
                     -- (ruleArity rule) says how
                     -- many args the rule consumed

             occ_anald_rhs = occurAnalyseExpr rule_rhs
                 -- See Note [Occurrence-analyse after rule firing]
       ; dump rule rule_rhs
       ; return (Just (zapped_env, occ_anald_rhs, cont')) }
            -- The occ_anald_rhs and cont' are all Out things
            -- hence zapping the environment

  | otherwise  -- No rule fires
  = do { nodump  -- This ensures that an empty file is written
       ; return Nothing }

  where
    ropts      = initRuleOpts dflags
    dflags     = seDynFlags env
    logger     = seLogger env
    zapped_env = zapSubstEnv env  -- See Note [zapSubstEnv]

    printRuleModule rule
      = parens (maybe (text "BUILTIN")
                      (pprModuleName . moduleName)
                      (ruleModule rule))

    dump rule rule_rhs
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

    nodump
      | logHasDumpFlag logger Opt_D_dump_rule_rewrites
      = liftIO $
          touchDumpFile logger Opt_D_dump_rule_rewrites

      | logHasDumpFlag logger Opt_D_dump_rule_firings
      = liftIO $
          touchDumpFile logger Opt_D_dump_rule_firings

      | otherwise
      = return ()

    log_rule flag hdr details
      = liftIO $ logDumpFile logger (mkDumpStyle alwaysQualify) flag "" FormatText
               $ sep [text hdr, nest 4 details]

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

Possible improvement: occ-anal the rules when putting them in the
database; and in the simplifier just occ-anal the OutExpr arguments.
But that's more complicated and the rule RHS is usually tiny; so I'm
just doing the simple thing.

Historical note: previously we did occ-anal the rules in Rule.hs,
but failed to occ-anal the OutExpr arguments, which led to the
nasty performance problem described above.


Note [Optimising tagToEnum#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an enumeration data type:

  data Foo = A | B | C

Then we want to transform

   case tagToEnum# x of   ==>    case x of
     A -> e1                       DEFAULT -> e1
     B -> e2                       1#      -> e2
     C -> e3                       2#      -> e3

thereby getting rid of the tagToEnum# altogether.  If there was a DEFAULT
alternative we retain it (remember it comes first).  If not the case must
be exhaustive, and we reflect that in the transformed version by adding
a DEFAULT.  Otherwise Lint complains that the new case is not exhaustive.
See #8317.

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
  (ok-for-spec is needed to satisfy the let/app invariant).
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

where 'r' is used strictly in (..r..), we can safely transform to
   let r = <scrut> in ...r...

This is a Good Thing, because 'r' might be dead (if the body just
calls error), or might be used just once (in which case it can be
inlined); or we might be able to float the let-binding up or down.
E.g. #15631 has an example.

Note that this can change the error behaviour.  For example, we might
transform
    case x of { _ -> error "bad" }
    --> error "bad"
which is might be puzzling if 'x' currently lambda-bound, but later gets
let-bound to (error "good").

Nevertheless, the paper "A semantics for imprecise exceptions" allows
this transformation. If you want to fix the evaluation order, use
'pseq'.  See #8900 for an example where the loss of this
transformation bit us in practice.

See also Note [Empty case alternatives] in GHC.Core.

Historical notes

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
  Note [Eta reduction of an eval'd function] in GHC.Core.Utils.)


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
        ; case findAlt (DataAlt con) alts of
            Nothing  -> missingAlt env0 case_bndr alts cont
            Just (Alt DEFAULT bs rhs) -> let con_app = Var (dataConWorkId con)
                                                 `mkTyApps` ty_args
                                                 `mkApps`   other_args
                                         in simple_rhs env0 scaled_wfloats con_app bs rhs
            Just (Alt _ bs rhs)       -> knownCon env0 scrut scaled_wfloats con ty_args other_args
                                                  case_bndr bs rhs cont
        }
  where
    simple_rhs env wfloats scrut' bs rhs =
      assert (null bs) $
      do { (floats1, env') <- simplNonRecX env case_bndr scrut'
             -- scrut is a constructor application,
             -- hence satisfies let/app invariant
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
  , exprOkForSideEffects scrut
          -- The entire case is dead, so we can drop it
          -- if the scrutinee converges without having imperative
          -- side effects or raising a Haskell exception
          -- See Note [PrimOp can_fail and has_side_effects] in GHC.Builtin.PrimOps
   = simplExprF env rhs cont

  -- 2b.  Turn the case into a let, if
  --      a) it binds only the case-binder
  --      b) unlifted case: the scrutinee is ok-for-speculation
  --           lifted case: the scrutinee is in HNF (or will later be demanded)
  -- See Note [Case to let transformation]
  | all_dead_bndrs
  , doCaseToLet scrut case_bndr
  = do { tick (CaseElim case_bndr)
       ; (floats1, env') <- simplNonRecX env case_bndr scrut
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

  | isUnliftedType (idType case_bndr)
  = exprOkForSpeculation scrut

  | otherwise  -- Scrut has a lifted type
  = exprIsHNF scrut
    || isStrUsedDmd (idDemandInfo case_bndr)
    -- See Note [Case-to-let for strictly-used binders]

--------------------------------------------------
--      3. Catch-all case
--------------------------------------------------

reallyRebuildCase env scrut case_bndr alts cont
  | not (sm_case_case (getMode env))
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

        ; fam_envs <- getFamEnvs
        ; (alt_env', scrut', case_bndr') <- improveSeq fam_envs env2 scrut
                                                       case_bndr case_bndr2 alts

        ; (imposs_deflt_cons, in_alts) <- prepareAlts scrut' case_bndr' alts
          -- NB: it's possible that the returned in_alts is empty: this is handled
          -- by the caller (rebuildCase) in the missingAlt function

        ; alts' <- mapM (simplAlt alt_env' (Just scrut') imposs_deflt_cons case_bndr' cont') in_alts
        ; -- pprTrace "simplAlts" (ppr case_bndr $$ ppr alts_ty $$ ppr alts_ty' $$ ppr alts $$ ppr cont') $

        ; let alts_ty' = contResultType cont'
        -- See Note [Avoiding space leaks in OutType]
        ; seqType alts_ty' `seq`
          mkCase (seDynFlags env0) scrut' case_bndr' alts_ty' alts' }


------------------------------------
improveSeq :: (FamInstEnv, FamInstEnv) -> SimplEnv
           -> OutExpr -> InId -> OutId -> [InAlt]
           -> SimplM (SimplEnv, OutExpr, OutId)
-- Note [Improving seq]
improveSeq fam_envs env scrut case_bndr case_bndr1 [Alt DEFAULT _ _]
  | Just (co, ty2) <- topNormaliseType_maybe fam_envs (idType case_bndr1)
  = do { case_bndr2 <- newId (fsLit "nt") Many ty2
        ; let rhs  = DoneEx (Var case_bndr2 `Cast` mkSymCo co) Nothing
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
        ; (env', vs') <- simplLamBndrs env vs_with_evals

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
          strdisp MarkedStrict = text "MarkedStrict"
          strdisp NotMarkedStrict = text "NotMarkedStrict"

zapIdOccInfoAndSetEvald :: StrictnessMark -> Id -> Id
zapIdOccInfoAndSetEvald str v =
  setCaseBndrEvald str $ -- Add eval'dness info
  zapIdOccInfo v         -- And kill occ info;
                         -- see Note [Case alternative occ info]

addAltUnfoldings :: SimplEnv -> Maybe OutExpr -> OutId -> OutExpr -> SimplM SimplEnv
addAltUnfoldings env scrut case_bndr con_app
  = do { let con_app_unf = mk_simple_unf con_app
             env1 = addBinderUnfolding env case_bndr con_app_unf

             -- See Note [Add unfolding for scrutinee]
             env2 | Many <- idMult case_bndr = case scrut of
                      Just (Var v)           -> addBinderUnfolding env1 v con_app_unf
                      Just (Cast (Var v) co) -> addBinderUnfolding env1 v $
                                                mk_simple_unf (Cast con_app (mkSymCo co))
                      _                      -> env1
                  | otherwise = env1

       ; traceSmpl "addAltUnf" (vcat [ppr case_bndr <+> ppr scrut, ppr con_app])
       ; return env2 }
  where
    -- Force the opts, so that the whole SimplEnv isn't retained
    !opts = seUnfoldingOpts env
    mk_simple_unf = mkSimpleUnfolding opts

addBinderUnfolding :: SimplEnv -> Id -> Unfolding -> SimplEnv
addBinderUnfolding env bndr unf
  | debugIsOn, Just tmpl <- maybeUnfoldingTemplate unf
  = warnPprTrace (not (eqType (idType bndr) (exprType tmpl)))
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
then Lint may complain about the let/app invariant.  Example
    case e of b { DEFAULT -> let v = reallyUnsafePtrEq# b y in ....
                ; K       -> blah }

The let/app invariant requires that y is evaluated in the call to
reallyUnsafePtrEq#, which it is.  But we still want that to be true if we
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

Exactly the same issue arises in GHC.Core.Opt.SpecConstr;
see Note [Add scrutinee to ValueEnv too] in GHC.Core.Opt.SpecConstr

HOWEVER, given
  case x of y { Just a -> r1; Nothing -> r2 }
we do not want to add the unfolding x -> y to 'x', which might seem cool,
since 'y' itself has different unfoldings in r1 and r2.  Reason: if we
did that, we'd have to zap y's deadness info and that is a very useful
piece of information.

So instead we add the unfolding x -> Just a, and x -> Nothing in the
respective RHSs.

Since this transformation is tantamount to a binder swap, the same caveat as in
Note [Suppressing binder-swaps on linear case] in OccurAnal apply.


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
             -- Note that the binder might be "dead", because it doesn't
             -- occur in the RHS; and simplNonRecX may therefore discard
             -- it via postInlineUnconditionally.
             -- Nevertheless we must keep it if the case-binder is alive,
             -- because it may be used in the con_app.  See Note [knownCon occ info]
           ; (floats1, env2) <- simplNonRecX env' b' arg  -- arg satisfies let/app invariant
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
                                     , extendIdSubst env bndr (DoneEx scrut Nothing))
      | otherwise           = do { dc_args <- mapM (simplVar env) bs
                                         -- dc_ty_args are already OutTypes,
                                         -- but bs are InBndrs
                                 ; let con_app = Var (dataConWorkId dc)
                                                 `mkTyApps` dc_ty_args
                                                 `mkApps`   dc_args
                                 ; simplNonRecX env bndr con_app }

-------------------
missingAlt :: SimplEnv -> Id -> [InAlt] -> SimplCont
           -> SimplM (SimplFloats, OutExpr)
                -- This isn't strictly an error, although it is unusual.
                -- It's possible that the simplifier might "see" that
                -- an inner case has no accessible alternatives before
                -- it "sees" that the entire branch of an outer case is
                -- inaccessible.  So we simply put an error case here instead.
missingAlt env case_bndr _ cont
  = warnPprTrace True (text "missingAlt" <+> ppr case_bndr) $
    -- See Note [Avoiding space leaks in OutType]
    let cont_ty = contResultType cont
    in seqType cont_ty `seq`
       return (emptyFloats env, mkImpossibleExpr cont_ty)

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
     (StrictBind { sc_bndr = bndr, sc_bndrs = bndrs
                 , sc_body = body, sc_env = se, sc_cont = cont})
-- See Note [Duplicating StrictBind]
-- K[ let x = <> in b ]  -->   join j x = K[ b ]
--                             j <>
  = do { let sb_env = se `setInScopeFromE` env
       ; (sb_env1, bndr')      <- simplBinder sb_env bndr
       ; (floats1, join_inner) <- simplLam sb_env1 bndrs body cont
          -- No need to use mkDupableCont before simplLam; we
          -- use cont once here, and then share the result if necessary

       ; let join_body = wrapFloats floats1 join_inner
             res_ty    = contResultType cont

       ; mkDupableStrictBind env bndr' join_body res_ty }

mkDupableContWithDmds env _
    (StrictArg { sc_fun = fun, sc_cont = cont
               , sc_fun_ty = fun_ty })
  -- NB: sc_dup /= OkToDup; that is caught earlier by contIsDupable
  | thumbsUpPlanA cont
  = -- Use Plan A of Note [Duplicating StrictArg]
    do { let (_ : dmds) = ai_dmds fun
       ; (floats1, cont')  <- mkDupableContWithDmds env dmds cont
                              -- Use the demands from the function to add the right
                              -- demand info on any bindings we make for further args
       ; (floats_s, args') <- mapAndUnzipM (makeTrivialArg (getMode env))
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
    do  { let (dmd:_) = dmds   -- Never fails
        ; (floats1, cont') <- mkDupableContWithDmds env dmds cont
        ; let env' = env `setInScopeFromF` floats1
        ; (_, se', arg') <- simplArg env' dup se arg
        ; (let_floats2, arg'') <- makeTrivial (getMode env) NotTopLevel dmd (fsLit "karg") arg'
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

        ; (join_floats, alts'') <- mapAccumLM (mkDupableAlt (targetPlatform (seDynFlags env)) case_bndr')
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
  | exprIsDupable (targetPlatform (seDynFlags env)) join_rhs
  = return (emptyFloats env
           , StrictBind { sc_bndr = arg_bndr, sc_bndrs = []
                        , sc_body = join_rhs
                        , sc_env  = zapSubstEnv env
                          -- See Note [StaticEnv invariant] in GHC.Core.Opt.Simplify.Utils
                        , sc_dup  = OkToDup
                        , sc_cont = mkBoringStop res_ty } )
  | otherwise
  = do { join_bndr <- newJoinId [arg_bndr] res_ty
       ; let arg_info = ArgInfo { ai_fun   = join_bndr
                                , ai_rules = Nothing, ai_args  = []
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
mkDupableAlt platform case_bndr jfloats (Alt con bndrs' rhs')
  | exprIsDupable platform rhs'  -- Note [Small alternative rhs]
  = return (jfloats, Alt con bndrs' rhs')

  | otherwise
  = do  { let rhs_ty'  = exprType rhs'

              final_bndrs'
                | isDeadBinder case_bndr = filter abstract_over bndrs'
                | otherwise              = bndrs' ++ [case_bndr]

              abstract_over bndr
                  | isTyVar bndr = True -- Abstract over all type variables just in case
                  | otherwise    = not (isDeadBinder bndr)
                        -- The deadness info on the new Ids is preserved by simplBinders
              final_args = varsToCoreExprs final_bndrs'
                           -- Note [Join point abstraction]

                -- We make the lambdas into one-shot-lambdas.  The
                -- join point is sure to be applied at most once, and doing so
                -- prevents the body of the join point being floated out by
                -- the full laziness pass
              really_final_bndrs     = map one_shot final_bndrs'
              one_shot v | isId v    = setOneShotLambda v
                         | otherwise = v
              join_rhs   = mkLams really_final_bndrs rhs'

        ; join_bndr <- newJoinId final_bndrs' rhs_ty'

        ; let join_call = mkApps (Var join_bndr) final_args
              alt'      = Alt con bndrs' join_call

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

Note [Small alternative rhs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is worth checking for a small RHS because otherwise we
get extra let bindings that may cause an extra iteration of the simplifier to
inline back in place.  Quite often the rhs is just a variable or constructor.
The Ord instance of Maybe in PrelMaybe.hs, for example, took several extra
iterations because the version with the let bindings looked big, and so wasn't
inlined, but after the join points had been inlined it looked smaller, and so
was inlined.

NB: we have to check the size of rhs', not rhs.
Duplicating a small InAlt might invalidate occurrence information
However, if it *is* dupable, we return the *un* simplified alternative,
because otherwise we'd need to pair it up with an empty subst-env....
but we only have one env shared between all the alts.
(Remember we must zap the subst-env before re-simplifying something).
Rather than do this we simply agree to re-simplify the original (small) thing later.

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
  Notice that Plan B is very like the way we handle strict
  bindings; see Note [Duplicating StrictBind].

Plan A is good. Here's an example from #3116
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

simplLetUnfolding :: SimplEnv-> TopLevelFlag
                  -> MaybeJoinCont
                  -> InId
                  -> OutExpr -> OutType -> ArityType
                  -> Unfolding -> SimplM Unfolding
simplLetUnfolding env top_lvl cont_mb id new_rhs rhs_ty arity unf
  | isStableUnfolding unf
  = simplStableUnfolding env top_lvl cont_mb id rhs_ty arity unf
  | isExitJoinId id
  = return noUnfolding -- See Note [Do not inline exit join points] in GHC.Core.Opt.Exitify
  | otherwise
  = -- Otherwise, we end up retaining all the SimpleEnv
    let !opts = seUnfoldingOpts env
    in mkLetUnfolding opts top_lvl InlineRhs id new_rhs

-------------------
mkLetUnfolding :: UnfoldingOpts -> TopLevelFlag -> UnfoldingSource
               -> InId -> OutExpr -> SimplM Unfolding
mkLetUnfolding !uf_opts top_lvl src id new_rhs
  = is_bottoming `seq`  -- See Note [Force bottoming field]
    return (mkUnfolding uf_opts src is_top_lvl is_bottoming new_rhs)
            -- We make an  unfolding *even for loop-breakers*.
            -- Reason: (a) It might be useful to know that they are WHNF
            --         (b) In GHC.Iface.Tidy we currently assume that, if we want to
            --             expose the unfolding then indeed we *have* an unfolding
            --             to expose.  (We could instead use the RHS, but currently
            --             we don't.)  The simple thing is always to have one.
  where
    is_top_lvl   = isTopLevel top_lvl
    is_bottoming = isDeadEndId id

-------------------
simplStableUnfolding :: SimplEnv -> TopLevelFlag
                     -> MaybeJoinCont  -- Just k => a join point with continuation k
                     -> InId
                     -> OutType
                     -> ArityType      -- Used to eta expand, but only for non-join-points
                     -> Unfolding
                     ->SimplM Unfolding
-- Note [Setting the new unfolding]
simplStableUnfolding env top_lvl mb_cont id rhs_ty id_arity unf
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
        -> do { expr' <- case mb_cont of
                           Just cont -> -- Binder is a join point
                                        -- See Note [Rules and unfolding for join points]
                                        simplJoinRhs unf_env id expr cont
                           Nothing   -> -- Binder is not a join point
                                        do { expr' <- simplExprC unf_env expr (mkBoringStop rhs_ty)
                                           ; return (eta_expand expr') }
              ; case guide of
                  UnfWhen { ug_arity = arity
                          , ug_unsat_ok = sat_ok
                          , ug_boring_ok = boring_ok
                          }
                          -- Happens for INLINE things
                     -> let guide' =
                              UnfWhen { ug_arity = arity
                                      , ug_unsat_ok = sat_ok
                                      , ug_boring_ok =
                                          boring_ok || inlineBoringOk expr'
                                      }
                        -- Refresh the boring-ok flag, in case expr'
                        -- has got small. This happens, notably in the inlinings
                        -- for dfuns for single-method classes; see
                        -- Note [Single-method classes] in GHC.Tc.TyCl.Instance.
                        -- A test case is #4138
                        -- But retain a previous boring_ok of True; e.g. see
                        -- the way it is set in calcUnfoldingGuidanceWithArity
                        in return (mkCoreUnfolding src is_top_lvl expr' guide')
                            -- See Note [Top-level flag on inline rules] in GHC.Core.Unfold

                  _other              -- Happens for INLINABLE things
                     -> mkLetUnfolding uf_opts top_lvl src id expr' }
                -- If the guidance is UnfIfGoodArgs, this is an INLINABLE
                -- unfolding, and we need to make sure the guidance is kept up
                -- to date with respect to any changes in the unfolding.

        | otherwise -> return noUnfolding   -- Discard unstable unfoldings
  where
    uf_opts    = seUnfoldingOpts env
    is_top_lvl = isTopLevel top_lvl
    act        = idInlineActivation id
    unf_env    = updMode (updModeForStableUnfoldings act) env
         -- See Note [Simplifying inside stable unfoldings] in GHC.Core.Opt.Simplify.Utils

    -- See Note [Eta-expand stable unfoldings]
    eta_expand expr
      | not eta_on         = expr
      | exprIsTrivial expr = expr
      | otherwise          = etaExpandAT id_arity expr
    eta_on = sm_eta_expand (getMode env)

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
* Don't eta-expand a trivial expr, else each pass will eta-reduce it,
  and then eta-expand again. See Note [Do not eta-expand trivial expressions]
  in GHC.Core.Opt.Simplify.Utils.
* Don't eta-expand join points; see Note [Do not eta-expand join points]
  in GHC.Core.Opt.Simplify.Utils.  We uphold this because the join-point
  case (mb_cont = Just _) doesn't use eta_expand.

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
             -> MaybeJoinCont   -- Just k for a join point binder
                                -- Nothing otherwise
             -> SimplM (SimplEnv, OutBndr)
-- Rules are added back into the bin
addBndrRules env in_id out_id mb_cont
  | null old_rules
  = return (env, out_id)
  | otherwise
  = do { new_rules <- simplRules env (Just out_id) old_rules mb_cont
       ; let final_id  = out_id `setIdSpecialisation` mkRuleInfo new_rules
       ; return (modifyInScope env final_id, final_id) }
  where
    old_rules = ruleInfoRules (idSpecialisation in_id)

simplRules :: SimplEnv -> Maybe OutId -> [CoreRule]
           -> MaybeJoinCont -> SimplM [CoreRule]
simplRules env mb_new_id rules mb_cont
  = mapM simpl_rule rules
  where
    simpl_rule rule@(BuiltinRule {})
      = return rule

    simpl_rule rule@(Rule { ru_bndrs = bndrs, ru_args = args
                          , ru_fn = fn_name, ru_rhs = rhs
                          , ru_act = act })
      = do { (env', bndrs') <- simplBinders env bndrs
           ; let rhs_ty = substTy env' (exprType rhs)
                 rhs_cont = case mb_cont of  -- See Note [Rules and unfolding for join points]
                                Nothing   -> mkBoringStop rhs_ty
                                Just cont -> assertPpr join_ok bad_join_msg cont
                 lhs_env = updMode updModeForRules env'
                 rhs_env = updMode (updModeForStableUnfoldings act) env'
                           -- See Note [Simplifying the RHS of a RULE]
                 fn_name' = case mb_new_id of
                              Just id -> idName id
                              Nothing -> fn_name

                 -- join_ok is an assertion check that the join-arity of the
                 -- binder matches that of the rule, so that pushing the
                 -- continuation into the RHS makes sense
                 join_ok = case mb_new_id of
                             Just id | Just join_arity <- isJoinId_maybe id
                                     -> length args == join_arity
                             _ -> False
                 bad_join_msg = vcat [ ppr mb_new_id, ppr rule
                                     , ppr (fmap isJoinId_maybe mb_new_id) ]

           ; args' <- mapM (simplExpr lhs_env) args
           ; rhs'  <- simplExprC rhs_env rhs rhs_cont
           ; return (rule { ru_bndrs = bndrs'
                          , ru_fn    = fn_name'
                          , ru_args  = args'
                          , ru_rhs   = rhs' }) }

{- Note [Simplifying the RHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can simplify the RHS of a RULE much as we do the RHS of a stable
unfolding.  We used to use the much more conservative updModeForRules
for the RHS as well as the LHS, but that seems more conservative
than necesary.  Allowing some inlining might, for example, eliminate
a binding.
-}
