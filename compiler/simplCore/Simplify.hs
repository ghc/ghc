{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[Simplify]{The main module of the simplifier}
-}

{-# LANGUAGE CPP #-}

module Simplify ( simplTopBinds, simplExpr, simplRules ) where

#include "HsVersions.h"

import DynFlags
import SimplMonad
import Type hiding      ( substTy, substTyVar, extendTvSubst, extendCvSubst )
import SimplEnv
import SimplUtils
import OccurAnal        ( occurAnalyseExpr )
import FamInstEnv       ( FamInstEnv )
import Literal          ( litIsLifted ) --, mkMachInt ) -- temporalily commented out. See #8326
import Id
import MkId             ( seqId )
import MkCore           ( mkImpossibleExpr, castBottomExpr )
import IdInfo
import Name             ( Name, mkSystemVarName, isExternalName, getOccFS )
import Coercion hiding  ( substCo, substCoVar )
import OptCoercion      ( optCoercion )
import FamInstEnv       ( topNormaliseType_maybe )
import DataCon          ( DataCon, dataConWorkId, dataConRepStrictness, dataConRepArgTys )
--import TyCon            ( isEnumerationTyCon ) -- temporalily commented out. See #8326
import CoreMonad        ( Tick(..), SimplifierMode(..) )
import CoreSyn
import Demand           ( StrictSig(..), dmdTypeDepth, isStrictDmd )
import PprCore          ( pprCoreExpr )
import CoreUnfold
import CoreUtils
import CoreArity
import CoreOpt          ( pushCoTyArg, pushCoValArg
                        , joinPointBinding_maybe, joinPointBindings_maybe )
--import PrimOp           ( tagToEnumKey ) -- temporalily commented out. See #8326
import Rules            ( mkRuleInfo, lookupRule, getRules )
--import TysPrim          ( intPrimTy ) -- temporalily commented out. See #8326
import BasicTypes       ( TopLevelFlag(..), isNotTopLevel, isTopLevel,
                          RecFlag(..) )
import MonadUtils       ( foldlM, mapAccumLM, liftIO )
import Maybes           ( isJust, fromJust, orElse )
--import Unique           ( hasKey ) -- temporalily commented out. See #8326
import Control.Monad
import Outputable
import FastString
import Pair
import Util
import ErrUtils
import Module          ( moduleName, pprModuleName )

{-
The guts of the simplifier is in this module, but the driver loop for
the simplifier is in SimplCore.hs.


-----------------------------------------
        *** IMPORTANT NOTE ***
-----------------------------------------
The simplifier used to guarantee that the output had no shadowing, but
it does not do so any more.   (Actually, it never did!)  The reason is
documented with simplifyArgs.


-----------------------------------------
        *** IMPORTANT NOTE ***
-----------------------------------------
Many parts of the simplifier return a bunch of "floats" as well as an
expression. This is wrapped as a datatype SimplUtils.FloatsWith.

All "floats" are let-binds, not case-binds, but some non-rec lets may
be unlifted (with RHS ok-for-speculation).



-----------------------------------------
        ORGANISATION OF FUNCTIONS
-----------------------------------------
simplTopBinds
  - simplify all top-level binders
  - for NonRec, call simplRecOrTopPair
  - for Rec,    call simplRecBind


        ------------------------------
simplExpr (applied lambda)      ==> simplNonRecBind
simplExpr (Let (NonRec ...) ..) ==> simplNonRecBind
simplExpr (Let (Rec ...)    ..) ==> simplify binders; simplRecBind

        ------------------------------
simplRecBind    [binders already simplfied]
  - use simplRecOrTopPair on each pair in turn

simplRecOrTopPair [binder already simplified]
  Used for: recursive bindings (top level and nested)
            top-level non-recursive bindings
  Returns:
  - check for PreInlineUnconditionally
  - simplLazyBind

simplNonRecBind
  Used for: non-top-level non-recursive bindings
            beta reductions (which amount to the same thing)
  Because it can deal with strict arts, it takes a
        "thing-inside" and returns an expression

  - check for PreInlineUnconditionally
  - simplify binder, including its IdInfo
  - if strict binding
        simplStrictArg
        mkAtomicArgs
        completeNonRecX
    else
        simplLazyBind
        addFloats

simplNonRecX:   [given a *simplified* RHS, but an *unsimplified* binder]
  Used for: binding case-binder and constr args in a known-constructor case
  - check for PreInLineUnconditionally
  - simplify binder
  - completeNonRecX

        ------------------------------
simplLazyBind:  [binder already simplified, RHS not]
  Used for: recursive bindings (top level and nested)
            top-level non-recursive bindings
            non-top-level, but *lazy* non-recursive bindings
        [must not be strict or unboxed]
  Returns floats + an augmented environment, not an expression
  - substituteIdInfo and add result to in-scope
        [so that rules are available in rec rhs]
  - simplify rhs
  - mkAtomicArgs
  - float if exposes constructor or PAP
  - completeBind


completeNonRecX:        [binder and rhs both simplified]
  - if the the thing needs case binding (unlifted and not ok-for-spec)
        build a Case
   else
        completeBind
        addFloats

completeBind:   [given a simplified RHS]
        [used for both rec and non-rec bindings, top level and not]
  - try PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity



Right hand sides and arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In many ways we want to treat
        (a) the right hand side of a let(rec), and
        (b) a function argument
in the same way.  But not always!  In particular, we would
like to leave these arguments exactly as they are, so they
will match a RULE more easily.

        f (g x, h x)
        g (+ x)

It's harder to make the rule match if we ANF-ise the constructor,
or eta-expand the PAP:

        f (let { a = g x; b = h x } in (a,b))
        g (\y. + x y)

On the other hand if we see the let-defns

        p = (g x, h x)
        q = + x

then we *do* want to ANF-ise and eta-expand, so that p and q
can be safely inlined.

Even floating lets out is a bit dubious.  For let RHS's we float lets
out if that exposes a value, so that the value can be inlined more vigorously.
For example

        r = let x = e in (x,x)

Here, if we float the let out we'll expose a nice constructor. We did experiments
that showed this to be a generally good thing.  But it was a bad thing to float
lets out unconditionally, because that meant they got allocated more often.

For function arguments, there's less reason to expose a constructor (it won't
get inlined).  Just possibly it might make a rule match, but I'm pretty skeptical.
So for the moment we don't float lets out of function arguments either.


Eta expansion
~~~~~~~~~~~~~~
For eta expansion, we want to catch things like

        case e of (a,b) -> \x -> case a of (p,q) -> \y -> r

If the \x was on the RHS of a let, we'd eta expand to bring the two
lambdas together.  And in general that's a good thing to do.  Perhaps
we should eta expand wherever we find a (value) lambda?  Then the eta
expansion at a let RHS can concentrate solely on the PAP case.


Case-of-case and join points
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Clearly we need to be very careful here to remain consistent---neither part is
optional!

************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************
-}

simplTopBinds :: SimplEnv -> [InBind] -> SimplM SimplEnv

simplTopBinds env0 binds0
  = do  {       -- Put all the top-level binders into scope at the start
                -- so that if a transformation rule has unexpectedly brought
                -- anything into scope, then we don't get a complaint about that.
                -- It's rather as if the top-level binders were imported.
                -- See note [Glomming] in OccurAnal.
        ; env1 <- simplRecBndrs env0 (bindersOfBinds binds0)
        ; env2 <- simpl_binds env1 binds0
        ; freeTick SimplifierDone
        ; return env2 }
  where
        -- We need to track the zapped top-level binders, because
        -- they should have their fragile IdInfo zapped (notably occurrence info)
        -- That's why we run down binds and bndrs' simultaneously.
        --
    simpl_binds :: SimplEnv -> [InBind] -> SimplM SimplEnv
    simpl_binds env []           = return env
    simpl_binds env (bind:binds) = do { env' <- simpl_bind env bind
                                      ; simpl_binds env' binds }

    simpl_bind env (Rec pairs)  = simplRecBind env TopLevel Nothing pairs
    simpl_bind env (NonRec b r) = do { (env', b') <- addBndrRules env b (lookupRecBndr env b)
                                     ; simplRecOrTopPair env' TopLevel
                                                         NonRecursive Nothing
                                                         b b' r }

{-
************************************************************************
*                                                                      *
\subsection{Lazy bindings}
*                                                                      *
************************************************************************

simplRecBind is used for
        * recursive bindings only
-}

simplRecBind :: SimplEnv -> TopLevelFlag -> Maybe SimplCont
             -> [(InId, InExpr)]
             -> SimplM SimplEnv
simplRecBind env0 top_lvl mb_cont pairs0
  = do  { (env_with_info, triples) <- mapAccumLM add_rules env0 pairs0
        ; env1 <- go (zapFloats env_with_info) triples
        ; return (env0 `addRecFloats` env1) }
        -- addFloats adds the floats from env1,
        -- _and_ updates env0 with the in-scope set from env1
  where
    add_rules :: SimplEnv -> (InBndr,InExpr) -> SimplM (SimplEnv, (InBndr, OutBndr, InExpr))
        -- Add the (substituted) rules to the binder
    add_rules env (bndr, rhs)
        = do { (env', bndr') <- addBndrRules env bndr (lookupRecBndr env bndr)
             ; return (env', (bndr, bndr', rhs)) }

    go env [] = return env

    go env ((old_bndr, new_bndr, rhs) : pairs)
        = do { env' <- simplRecOrTopPair env top_lvl Recursive mb_cont
                                         old_bndr new_bndr rhs
             ; go env' pairs }

{-
simplOrTopPair is used for
        * recursive bindings (whether top level or not)
        * top-level non-recursive bindings

It assumes the binder has already been simplified, but not its IdInfo.
-}

simplRecOrTopPair :: SimplEnv
                  -> TopLevelFlag -> RecFlag -> Maybe SimplCont
                  -> InId -> OutBndr -> InExpr  -- Binder and rhs
                  -> SimplM SimplEnv    -- Returns an env that includes the binding

simplRecOrTopPair env top_lvl is_rec mb_cont old_bndr new_bndr rhs
  = do { dflags <- getDynFlags
       ; trace_bind dflags $
           if preInlineUnconditionally dflags env top_lvl old_bndr rhs
                    -- Check for unconditional inline
           then do tick (PreInlineUnconditionally old_bndr)
                   return (extendIdSubst env old_bndr (mkContEx env rhs))
           else simplBind env top_lvl is_rec mb_cont old_bndr new_bndr rhs env }
  where
    trace_bind dflags thing_inside
      | not (dopt Opt_D_verbose_core2core dflags)
      = thing_inside
      | otherwise
      = pprTrace "SimplBind" (ppr old_bndr) thing_inside
        -- trace_bind emits a trace for each top-level binding, which
        -- helps to locate the tracing for inlining and rule firing

{-
simplBind is used for
  * [simplRecOrTopPair] recursive bindings (whether top level or not)
  * [simplRecOrTopPair] top-level non-recursive bindings
  * [simplNonRecE]      non-top-level *lazy* non-recursive bindings

Nota bene:
    1. It assumes that the binder is *already* simplified,
       and is in scope, and its IdInfo too, except unfolding

    2. It assumes that the binder type is lifted.

    3. It does not check for pre-inline-unconditionally;
       that should have been done already.
-}

simplBind :: SimplEnv
          -> TopLevelFlag -> RecFlag -> Maybe SimplCont
          -> InId -> OutId      -- Binder, both pre-and post simpl
                                -- The OutId has IdInfo, except arity, unfolding
                                -- Ids only, no TyVars
          -> InExpr -> SimplEnv -- The RHS and its environment
          -> SimplM SimplEnv
simplBind env top_lvl is_rec mb_cont bndr bndr1 rhs rhs_se
  | ASSERT( isId bndr1 )
    isJoinId bndr1
  = ASSERT(isNotTopLevel top_lvl && isJust mb_cont)
    simplJoinBind env is_rec (fromJust mb_cont) bndr bndr1 rhs rhs_se
  | otherwise
  = simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se

simplLazyBind :: SimplEnv
              -> TopLevelFlag -> RecFlag
              -> InId -> OutId          -- Binder, both pre-and post simpl
                                        -- The OutId has IdInfo, except arity, unfolding
                                        -- Ids only, no TyVars
              -> InExpr -> SimplEnv     -- The RHS and its environment
              -> SimplM SimplEnv
-- Precondition: rhs obeys the let/app invariant
-- NOT used for JoinIds
simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se
  = ASSERT( isId bndr )
    ASSERT2( not (isJoinId bndr), ppr bndr )
    -- pprTrace "simplLazyBind" ((ppr bndr <+> ppr bndr1) $$ ppr rhs $$ ppr (seIdSubst rhs_se)) $
    do  { let   rhs_env     = rhs_se `setInScopeAndZapFloats` env
                (tvs, body) = case collectTyAndValBinders rhs of
                                (tvs, [], body)
                                  | surely_not_lam body -> (tvs, body)
                                _                       -> ([], rhs)

                surely_not_lam (Lam {})     = False
                surely_not_lam (Tick t e)
                  | not (tickishFloatable t) = surely_not_lam e
                   -- eta-reduction could float
                surely_not_lam _            = True
                        -- Do not do the "abstract tyyvar" thing if there's
                        -- a lambda inside, because it defeats eta-reduction
                        --    f = /\a. \x. g a x
                        -- should eta-reduce.


        ; (body_env, tvs') <- simplBinders rhs_env tvs
                -- See Note [Floating and type abstraction] in SimplUtils

        -- Simplify the RHS
        ; let   rhs_cont = mkRhsStop (substTy body_env (exprType body))
        ; (body_env0, body0) <- simplExprF body_env body rhs_cont
        ; let body1     = wrapJoinFloats body_env0 body0
              body_env1 = body_env0 `restoreJoinFloats` body_env

        -- ANF-ise a constructor or PAP rhs
        ; (body_env2, body2) <- prepareRhs top_lvl body_env1 bndr1 body1

        ; (env', rhs')
            <-  if not (doFloatFromRhs top_lvl is_rec False body2 body_env2)
                then                            -- No floating, revert to body1
                     do { rhs' <- mkLam env tvs' (wrapFloats body_env1 body1) rhs_cont
                        ; return (env, rhs') }

                else if null tvs then           -- Simple floating
                     do { tick LetFloatFromLet
                        ; return (addFloats env body_env2, body2) }

                else                            -- Do type-abstraction first
                     do { tick LetFloatFromLet
                        ; (poly_binds, body3) <- abstractFloats tvs' body_env2 body2
                        ; rhs' <- mkLam env tvs' body3 rhs_cont
                        ; env' <- foldlM (addPolyBind top_lvl) env poly_binds
                        ; return (env', rhs') }

        ; completeBind env' top_lvl is_rec Nothing bndr bndr1 rhs' }

simplJoinBind :: SimplEnv
              -> RecFlag
              -> SimplCont
              -> InId -> OutId          -- Binder, both pre-and post simpl
                                        -- The OutId has IdInfo, except arity,
                                        --   unfolding
              -> InExpr -> SimplEnv     -- The RHS and its environment
              -> SimplM SimplEnv
simplJoinBind env is_rec cont bndr bndr1 rhs rhs_se
  = -- pprTrace "simplLazyBind" ((ppr bndr <+> ppr bndr1) $$
    --                           ppr rhs $$ ppr (seIdSubst rhs_se)) $
    do  { let rhs_env = rhs_se `setInScopeAndZapFloats` env

        -- Simplify the RHS
        ; rhs' <- simplJoinRhs rhs_env bndr rhs cont
        ; completeBind env NotTopLevel is_rec (Just cont) bndr bndr1 rhs' }

{-
A specialised variant of simplNonRec used when the RHS is already simplified,
notably in knownCon.  It uses case-binding where necessary.
-}

simplNonRecX :: SimplEnv
             -> InId            -- Old binder
             -> OutExpr         -- Simplified RHS
             -> SimplM SimplEnv
-- Precondition: rhs satisfies the let/app invariant
simplNonRecX env bndr new_rhs
  | isDeadBinder bndr   -- Not uncommon; e.g. case (a,b) of c { (p,q) -> p }
  = return env    --  Here c is dead, and we avoid creating
                  --   the binding c = (a,b)

  | Coercion co <- new_rhs
  = return (extendCvSubst env bndr co)

  | otherwise
  = do  { (env', bndr') <- simplBinder env bndr
        ; completeNonRecX NotTopLevel env' (isStrictId bndr) bndr bndr' new_rhs }
                -- simplNonRecX is only used for NotTopLevel things

completeNonRecX :: TopLevelFlag -> SimplEnv
                -> Bool
                -> InId                 -- Old binder
                -> OutId                -- New binder
                -> OutExpr              -- Simplified RHS
                -> SimplM SimplEnv
-- Precondition: rhs satisfies the let/app invariant
--               See Note [CoreSyn let/app invariant] in CoreSyn

completeNonRecX top_lvl env is_strict old_bndr new_bndr new_rhs
  = ASSERT(not (isJoinId new_bndr))
    do  { (env1, rhs1) <- prepareRhs top_lvl (zapFloats env) new_bndr new_rhs
        ; (env2, rhs2) <-
                if doFloatFromRhs NotTopLevel NonRecursive is_strict rhs1 env1
                then do { tick LetFloatFromLet
                        ; return (addFloats env env1, rhs1) }   -- Add the floats to the main env
                else return (env, wrapFloats env1 rhs1)         -- Wrap the floats around the RHS
        ; completeBind env2 NotTopLevel NonRecursive Nothing
                       old_bndr new_bndr rhs2 }

{-
{- No, no, no!  Do not try preInlineUnconditionally in completeNonRecX
   Doing so risks exponential behaviour, because new_rhs has been simplified once already
   In the cases described by the following comment, postInlineUnconditionally will
   catch many of the relevant cases.
        -- This happens; for example, the case_bndr during case of
        -- known constructor:  case (a,b) of x { (p,q) -> ... }
        -- Here x isn't mentioned in the RHS, so we don't want to
        -- create the (dead) let-binding  let x = (a,b) in ...
        --
        -- Similarly, single occurrences can be inlined vigourously
        -- e.g.  case (f x, g y) of (a,b) -> ....
        -- If a,b occur once we can avoid constructing the let binding for them.

   Furthermore in the case-binding case preInlineUnconditionally risks extra thunks
        -- Consider     case I# (quotInt# x y) of
        --                I# v -> let w = J# v in ...
        -- If we gaily inline (quotInt# x y) for v, we end up building an
        -- extra thunk:
        --                let w = J# (quotInt# x y) in ...
        -- because quotInt# can fail.

  | preInlineUnconditionally env NotTopLevel bndr new_rhs
  = thing_inside (extendIdSubst env bndr (DoneEx new_rhs))
-}

----------------------------------
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

prepareRhs :: TopLevelFlag -> SimplEnv -> OutId -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS
prepareRhs top_lvl env id (Cast rhs co)    -- Note [Float coercions]
  | Pair ty1 _ty2 <- coercionKind co       -- Do *not* do this if rhs has an unlifted type
  , not (isUnliftedType ty1)            -- see Note [Float coercions (unlifted)]
  = do  { (env', rhs') <- makeTrivialWithInfo top_lvl env (getOccFS id) sanitised_info rhs
        ; return (env', Cast rhs' co) }
  where
    sanitised_info = vanillaIdInfo `setStrictnessInfo` strictnessInfo info
                                   `setDemandInfo` demandInfo info
    info = idInfo id

prepareRhs top_lvl env0 id rhs0
  = do  { (_is_exp, env1, rhs1) <- go 0 env0 rhs0
        ; return (env1, rhs1) }
  where
    go n_val_args env (Cast rhs co)
        = do { (is_exp, env', rhs') <- go n_val_args env rhs
             ; return (is_exp, env', Cast rhs' co) }
    go n_val_args env (App fun (Type ty))
        = do { (is_exp, env', rhs') <- go n_val_args env fun
             ; return (is_exp, env', App rhs' (Type ty)) }
    go n_val_args env (App fun arg)
        = do { (is_exp, env', fun') <- go (n_val_args+1) env fun
             ; case is_exp of
                True -> do { (env'', arg') <- makeTrivial top_lvl env' (getOccFS id) arg
                           ; return (True, env'', App fun' arg') }
                False -> return (False, env, App fun arg) }
    go n_val_args env (Var fun)
        = return (is_exp, env, Var fun)
        where
          is_exp = isExpandableApp fun n_val_args   -- The fun a constructor or PAP
                        -- See Note [CONLIKE pragma] in BasicTypes
                        -- The definition of is_exp should match that in
                        -- OccurAnal.occAnalApp

    go n_val_args env (Tick t rhs)
        -- We want to be able to float bindings past this
        -- tick. Non-scoping ticks don't care.
        | tickishScoped t == NoScope
        = do { (is_exp, env', rhs') <- go n_val_args env rhs
             ; return (is_exp, env', Tick t rhs') }
        -- On the other hand, for scoping ticks we need to be able to
        -- copy them on the floats, which in turn is only allowed if
        -- we can obtain non-counting ticks.
        | (not (tickishCounts t) || tickishCanSplit t)
        = do { (is_exp, env', rhs') <- go n_val_args (zapFloats env) rhs
             ; let tickIt (id, expr)
                       -- we have to take care not to tick top-level literal
                       -- strings. See Note [CoreSyn top-level string literals].
                     | isTopLevel top_lvl && exprIsLiteralString expr
                     = (id, expr)
                     | otherwise
                     = (id, mkTick (mkNoCount t) expr)
                   floats' = seFloats $ env `addFloats` mapFloats env' tickIt
             ; return (is_exp, env' { seFloats = floats' }, Tick t rhs') }

    go _ env other
        = return (False, env, other)

{-
Note [Float coercions]
~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coercion to the usage site may well cancel the coercions
and lead to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          x = T m
          go 0 = 0
          go n = case x of { T m -> go (n-m) }
                -- This case should optimise

Note [Preserve strictness when floating coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the Note [Float coercions] transformation, keep the strictness info.
Eg
        f = e `cast` co    -- f has strictness SSL
When we transform to
        f' = e             -- f' also has strictness SSL
        f = f' `cast` co   -- f still has strictness SSL

Its not wrong to drop it on the floor, but better to keep it.

Note [Float coercions (unlifted)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BUT don't do [Float coercions] if 'e' has an unlifted type.
This *can* happen:

     foo :: Int = (error (# Int,Int #) "urk")
                  `cast` CoUnsafe (# Int,Int #) Int

If do the makeTrivial thing to the error call, we'll get
    foo = case error (# Int,Int #) "urk" of v -> v `cast` ...
But 'v' isn't in scope!

These strange casts can happen as a result of case-of-case
        bar = case (case x of { T -> (# 2,3 #); F -> error "urk" }) of
                (# p,q #) -> p+q
-}

makeTrivialArg :: SimplEnv -> ArgSpec -> SimplM (SimplEnv, ArgSpec)
makeTrivialArg env (ValArg e) = do
    { (env', e') <- makeTrivial NotTopLevel env (fsLit "arg") e
    ; return (env', ValArg e') }
makeTrivialArg env arg        = return (env, arg)  -- CastBy, TyArg

makeTrivial :: TopLevelFlag -> SimplEnv
            -> FastString  -- ^ a "friendly name" to build the new binder from
            -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Binds the expression to a variable, if it's not trivial, returning the variable
makeTrivial top_lvl env context expr =
    makeTrivialWithInfo top_lvl env context vanillaIdInfo expr

makeTrivialWithInfo :: TopLevelFlag -> SimplEnv
                    -> FastString
                    -- ^ a "friendly name" to build the new binder from
                    -> IdInfo -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Propagate strictness and demand info to the new binder
-- Note [Preserve strictness when floating coercions]
-- Returned SimplEnv has same substitution as incoming one
makeTrivialWithInfo top_lvl env context info expr
  | exprIsTrivial expr                          -- Already trivial
  || not (bindingOk top_lvl expr expr_ty)       -- Cannot trivialise
                                                --   See Note [Cannot trivialise]
  = return (env, expr)
  | otherwise           -- See Note [Take care] below
  = do  { uniq <- getUniqueM
        ; let name = mkSystemVarName uniq context
              var = mkLocalIdOrCoVarWithInfo name expr_ty info
        ; env'  <- completeNonRecX top_lvl env False var var expr
        ; expr' <- simplVar env' var
        ; return (env', expr') }
        -- The simplVar is needed because we're constructing a new binding
        --     a = rhs
        -- And if rhs is of form (rhs1 |> co), then we might get
        --     a1 = rhs1
        --     a = a1 |> co
        -- and now a's RHS is trivial and can be substituted out, and that
        -- is what completeNonRecX will do
        -- To put it another way, it's as if we'd simplified
        --    let var = e in var
  where
    expr_ty = exprType expr

bindingOk :: TopLevelFlag -> CoreExpr -> Type -> Bool
-- True iff we can have a binding of this expression at this level
-- Precondition: the type is the type of the expression
bindingOk top_lvl expr expr_ty
  | isTopLevel top_lvl = exprIsTopLevelBindable expr expr_ty
  | otherwise          = True

{-
Note [Cannot trivialise]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider tih
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

See Note [CoreSyn top-level string literals] in CoreSyn.

************************************************************************
*                                                                      *
\subsection{Completing a lazy binding}
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
             -> RecFlag                 -- Recursive binding?
             -> Maybe SimplCont         -- Required only for join point
             -> InId                    -- Old binder
             -> OutId -> OutExpr        -- New binder and RHS
             -> SimplM SimplEnv
-- completeBind may choose to do its work
--      * by extending the substitution (e.g. let x = y in ...)
--      * or by adding to the floats in the envt
--
-- Precondition: rhs obeys the let/app invariant
completeBind env top_lvl is_rec mb_cont old_bndr new_bndr new_rhs
 | isCoVar old_bndr
 = case new_rhs of
     Coercion co -> return (extendCvSubst env old_bndr co)
     _           -> return (addNonRec env new_bndr new_rhs)

 | otherwise
 = ASSERT( isId new_bndr )
   do { let old_info = idInfo old_bndr
            old_unf  = unfoldingInfo old_info
            occ_info = occInfo old_info

        -- Do eta-expansion on the RHS of the binding
        -- See Note [Eta-expanding at let bindings] in SimplUtils
      ; (new_arity, final_rhs) <- if isJoinId new_bndr
                                    then return (manifestArity new_rhs, new_rhs)
                                         -- Note [Don't eta-expand join points]
                                    else tryEtaExpandRhs env is_rec
                                                         new_bndr new_rhs

        -- Simplify the unfolding
      ; new_unfolding <- simplLetUnfolding env top_lvl mb_cont old_bndr
                                           final_rhs old_unf

      ; dflags <- getDynFlags
      ; if postInlineUnconditionally dflags env top_lvl new_bndr occ_info
                                     final_rhs new_unfolding

                        -- Inline and discard the binding
        then do  { tick (PostInlineUnconditionally old_bndr)
                 ; return (extendIdSubst env old_bndr (DoneEx final_rhs)) }
                -- Use the substitution to make quite, quite sure that the
                -- substitution will happen, since we are going to discard the binding
        else
   do { let info1 = idInfo new_bndr `setArityInfo` new_arity

              -- Unfolding info: Note [Setting the new unfolding]
            info2 = info1 `setUnfoldingInfo` new_unfolding

              -- Demand info: Note [Setting the demand info]
              --
              -- We also have to nuke demand info if for some reason
              -- eta-expansion *reduces* the arity of the binding to less
              -- than that of the strictness sig. This can happen: see Note [Arity decrease].
            info3 | isEvaldUnfolding new_unfolding
                    || (case strictnessInfo info2 of
                          StrictSig dmd_ty -> new_arity < dmdTypeDepth dmd_ty)
                  = zapDemandInfo info2 `orElse` info2
                  | otherwise
                  = info2

              -- Zap call arity info. We have used it by now (via
              -- `tryEtaExpandRhs`), and the simplifier can invalidate this
              -- information, leading to broken code later (e.g. #13479)
            info4 = zapCallArityInfo info3

            final_id = new_bndr `setIdInfo` info4

      ; -- pprTrace "Binding" (ppr final_id <+> ppr new_unfolding) $
        return (addNonRec env final_id final_rhs) } }
                -- The addNonRec adds it to the in-scope set too

------------------------------
addPolyBind :: TopLevelFlag -> SimplEnv -> OutBind -> SimplM SimplEnv
-- Add a new binding to the environment, complete with its unfolding
-- but *do not* do postInlineUnconditionally, because we have already
-- processed some of the scope of the binding
-- We still want the unfolding though.  Consider
--      let
--            x = /\a. let y = ... in Just y
--      in body
-- Then we float the y-binding out (via abstractFloats and addPolyBind)
-- but 'x' may well then be inlined in 'body' in which case we'd like the
-- opportunity to inline 'y' too.
--
-- INVARIANT: the arity is correct on the incoming binders

addPolyBind top_lvl env (NonRec poly_id rhs)
  = do  { unfolding <- simplLetUnfolding env top_lvl Nothing poly_id rhs
                                         noUnfolding
                        -- Assumes that poly_id did not have an INLINE prag
                        -- which is perhaps wrong.  ToDo: think about this
        ; let final_id = setIdInfo poly_id $
                         idInfo poly_id `setUnfoldingInfo` unfolding

        ; return (addNonRec env final_id rhs) }

addPolyBind _ env bind@(Rec _)
  = return (extendFloats env bind)
        -- Hack: letrecs are more awkward, so we extend "by steam"
        -- without adding unfoldings etc.  At worst this leads to
        -- more simplifier iterations

{- Note [Arity decrease]
~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking the arity of a binding should not decrease.  But it *can*
legitimately happen because of RULES.  Eg
        f = g Int
where g has arity 2, will have arity 2.  But if there's a rewrite rule
        g Int --> h
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

Note [Don't eta-expand join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similarly to CPR (see Note [Don't CPR join points] in WorkWrap), a join point
stands well to gain from its outer binding's eta-expansion, and eta-expanding a
join point is fraught with issues like how to deal with a cast:

    let join $j1 :: IO ()
             $j1 = ...
             $j2 :: Int -> IO ()
             $j2 n = if n > 0 then $j1
                              else ...

    =>

    let join $j1 :: IO ()
             $j1 = (\eta -> ...)
                     `cast` N:IO :: State# RealWorld -> (# State# RealWorld, ())
                                 ~  IO ()
             $j2 :: Int -> IO ()
             $j2 n = (\eta -> if n > 0 then $j1
                                       else ...)
                     `cast` N:IO :: State# RealWorld -> (# State# RealWorld, ())
                                 ~  IO ()

The cast here can't be pushed inside the lambda (since it's not casting to a
function type), so the lambda has to stay, but it can't because it contains a
reference to a join point. In fact, $j2 can't be eta-expanded at all. Rather
than try and detect this situation (and whatever other situations crop up!), we
don't bother; again, any surrounding eta-expansion will improve these join
points anyway, since an outer cast can *always* be pushed inside. By the time
CorePrep comes around, the code is very likely to look more like this:

    let join $j1 :: State# RealWorld -> (# State# RealWorld, ())
             $j1 = (...) eta
             $j2 :: Int -> State# RealWorld -> (# State# RealWorld, ())
             $j2 = if n > 0 then $j1
                            else (...) eta

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
simplExpr env (Type ty)
  = do { ty' <- simplType env ty
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
  = -- pprTrace "simplExprC" (ppr expr $$ ppr cont {- $$ ppr (seIdSubst env) -} $$ ppr (seFloats env) ) $
    do  { (env', expr') <- simplExprF (zapFloats env) expr cont
        ; -- pprTrace "simplExprC ret" (ppr expr $$ ppr expr') $
          -- pprTrace "simplExprC ret3" (ppr (seInScope env')) $
          -- pprTrace "simplExprC ret4" (ppr (seFloats env')) $
          return (wrapFloats env' expr') }

--------------------------------------------------
simplExprF :: SimplEnv
           -> InExpr     -- A term-valued expression, never (Type ty)
           -> SimplCont
           -> SimplM (SimplEnv, OutExpr)

simplExprF env e cont
  = {- pprTrace "simplExprF" (vcat
      [ ppr e
      , text "cont =" <+> ppr cont
      , text "inscope =" <+> ppr (seInScope env)
      , text "tvsubst =" <+> ppr (seTvSubst env)
      , text "idsubst =" <+> ppr (seIdSubst env)
      , text "cvsubst =" <+> ppr (seCvSubst env)
      {- , ppr (seFloats env) -}
      ]) $ -}
    simplExprF1 env e cont

simplExprF1 :: SimplEnv -> InExpr -> SimplCont
            -> SimplM (SimplEnv, OutExpr)

simplExprF1 _ (Type ty) _
  = pprPanic "simplExprF: type" (ppr ty)
    -- simplExprF does only with term-valued expressions
    -- The (Type ty) case is handled separately by simplExpr
    -- and by the other callers of simplExprF

simplExprF1 env (Var v)        cont = simplIdF env v cont
simplExprF1 env (Lit lit)      cont = rebuild env (Lit lit) cont
simplExprF1 env (Tick t expr)  cont = simplTick env t expr cont
simplExprF1 env (Cast body co) cont = simplCast env body co cont
simplExprF1 env (Coercion co)  cont = simplCoercionF env co cont


simplExprF1 env (App fun arg) cont
  = simplExprF env fun $
    case arg of
      Type ty -> ApplyToTy  { sc_arg_ty  = substTy env ty
                            , sc_hole_ty = substTy env (exprType fun)
                            , sc_cont    = cont }
      _       -> ApplyToVal { sc_arg = arg, sc_env = env
                            , sc_dup = NoDup, sc_cont = cont }

simplExprF1 env expr@(Lam {}) cont
  = simplLam env zapped_bndrs body cont
        -- The main issue here is under-saturated lambdas
        --   (\x1. \x2. e) arg1
        -- Here x1 might have "occurs-once" occ-info, because occ-info
        -- is computed assuming that a group of lambdas is applied
        -- all at once.  If there are too few args, we must zap the
        -- occ-info, UNLESS the remaining binders are one-shot
  where
    (bndrs, body) = collectBinders expr
    zapped_bndrs | need_to_zap = map zap bndrs
                 | otherwise   = bndrs

    need_to_zap = any zappable_bndr (drop n_args bndrs)
    n_args = countArgs cont
        -- NB: countArgs counts all the args (incl type args)
        -- and likewise drop counts all binders (incl type lambdas)

    zappable_bndr b = isId b && not (isOneShotBndr b)
    zap b | isTyVar b = b
          | otherwise = zapLamIdInfo b

simplExprF1 env (Case scrut bndr _ alts) cont
  = simplExprF env scrut (Select { sc_dup = NoDup, sc_bndr = bndr
                                 , sc_alts = alts
                                 , sc_env = env, sc_cont = cont })

simplExprF1 env (Let (Rec pairs) body) cont
  = simplRecE env pairs body cont

simplExprF1 env (Let (NonRec bndr rhs) body) cont
  | Type ty <- rhs    -- First deal with type lets (let a = Type ty in e)
  = ASSERT( isTyVar bndr )
    do { ty' <- simplType env ty
       ; simplExprF (extendTvSubst env bndr ty') body cont }

  | otherwise
  = simplNonRecE env bndr (rhs, env) ([], body) cont

---------------------------------
-- Simplify a join point, adding the context.
-- Context goes *inside* the lambdas. IOW, if the join point has arity n, we do:
--   \x1 .. xn -> e => \x1 .. xn -> E[e]
-- Note that we need the arity of the join point, since e may be a lambda
-- (though this is unlikely). See Note [Case-of-case and join points].
simplJoinRhs :: SimplEnv -> InId -> InExpr -> SimplCont
             -> SimplM OutExpr
simplJoinRhs env bndr expr cont
  | Just arity <- isJoinId_maybe bndr
  =  do { let (join_bndrs, join_body) = collectNBinders arity expr
        ; (env', join_bndrs') <- simplLamBndrs env join_bndrs
        ; join_body' <- simplExprC env' join_body cont
        ; return $ mkLams join_bndrs' join_body' }

  | otherwise
  = pprPanic "simplJoinRhs" (ppr bndr)

---------------------------------
simplType :: SimplEnv -> InType -> SimplM OutType
        -- Kept monadic just so we can do the seqType
simplType env ty
  = -- pprTrace "simplType" (ppr ty $$ ppr (seTvSubst env)) $
    seqType new_ty `seq` return new_ty
  where
    new_ty = substTy env ty

---------------------------------
simplCoercionF :: SimplEnv -> InCoercion -> SimplCont
               -> SimplM (SimplEnv, OutExpr)
simplCoercionF env co cont
  = do { co' <- simplCoercion env co
       ; rebuild env (Coercion co') cont }

simplCoercion :: SimplEnv -> InCoercion -> SimplM OutCoercion
simplCoercion env co
  = let opt_co = optCoercion (getTCvSubst env) co
    in seqCo opt_co `seq` return opt_co

-----------------------------------
-- | Push a TickIt context outwards past applications and cases, as
-- long as this is a non-scoping tick, to let case and application
-- optimisations apply.

simplTick :: SimplEnv -> Tickish Id -> InExpr -> SimplCont
          -> SimplM (SimplEnv, OutExpr)
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
  = do { (env', expr') <- simplExprF env expr cont
       ; return (env', mkTick tickish expr')
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
         tickAlt (c,bs,e) = (c,bs, foldr mkTick e ts_scope)
         ts_scope         = map mkNoCount $
                            filter (not . (`tickishScopesLike` NoScope)) ticks

  no_floating_past_tick =
    do { let (inc,outc) = splitCont cont
       ; (env', expr') <- simplExprF (zapFloats env) expr inc
       ; let tickish' = simplTickish env tickish
       ; (env'', expr'') <- rebuild (zapFloats env')
                                    (wrapFloats env' expr')
                                    (TickIt tickish' outc)
       ; return (addFloats env env'', expr'')
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
--       ; let wrap_float (b,rhs) = (zapIdStrictness (setIdArity b 0),
--                                   mkTick (mkNoCount tickish') rhs)
--              -- when wrapping a float with mkTick, we better zap the Id's
--              -- strictness info and arity, because it might be wrong now.
--       ; let env'' = addFloats env (mapFloats env' wrap_float)
--       ; rebuild env'' expr' (TickIt tickish' outc)
--       }


  simplTickish env tickish
    | Breakpoint n ids <- tickish
          = Breakpoint n (map (getDoneId . substId env) ids)
    | otherwise = tickish

  -- Push type application and coercion inside a tick
  splitCont :: SimplCont -> (SimplCont, SimplCont)
  splitCont cont@(ApplyToTy { sc_cont = tail }) = (cont { sc_cont = inc }, outc)
    where (inc,outc) = splitCont tail
  splitCont (CastIt co c) = (CastIt co inc, outc)
    where (inc,outc) = splitCont c
  splitCont other = (mkBoringStop (contHoleType other), other)

  getDoneId (DoneId id) = id
  getDoneId (DoneEx e)  = getIdFromTrivialExpr e -- Note [substTickish] in CoreSubst
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

rebuild :: SimplEnv -> OutExpr -> SimplCont -> SimplM (SimplEnv, OutExpr)
-- At this point the substitution in the SimplEnv should be irrelevant
-- only the in-scope set and floats should matter
rebuild env expr cont
  = case cont of
      Stop {}          -> return (env, expr)
      TickIt t cont    -> rebuild env (mkTick t expr) cont
      CastIt co cont   -> rebuild env (mkCast expr co) cont
                       -- NB: mkCast implements the (Coercion co |> g) optimisation

      Select { sc_bndr = bndr, sc_alts = alts, sc_env = se, sc_cont = cont }
        -> rebuildCase (se `setFloats` env) expr bndr alts cont

      StrictArg info _ cont         -> rebuildCall env (info `addValArgTo` expr) cont
      StrictBind b bs body se cont  -> do { env' <- simplNonRecX (se `setFloats` env) b expr
                                               -- expr satisfies let/app since it started life
                                               -- in a call to simplNonRecE
                                          ; simplLam env' bs body cont }

      ApplyToTy  { sc_arg_ty = ty, sc_cont = cont}
        -> rebuild env (App expr (Type ty)) cont

      ApplyToVal { sc_arg = arg, sc_env = se, sc_dup = dup_flag, sc_cont = cont}
        -- See Note [Avoid redundant simplification]
        | isSimplified dup_flag
        -> rebuild env (App expr arg) cont

        | otherwise
        -> do { arg' <- simplExpr (se `setInScopeAndZapFloats` env) arg
              ; rebuild env (App expr arg') cont }


{-
************************************************************************
*                                                                      *
\subsection{Lambdas}
*                                                                      *
************************************************************************
-}

simplCast :: SimplEnv -> InExpr -> Coercion -> SimplCont
          -> SimplM (SimplEnv, OutExpr)
simplCast env body co0 cont0
  = do  { co1   <- simplCoercion env co0
        ; cont1 <- addCoerce co1 cont0
        ; simplExprF env body cont1 }
  where
       addCoerce :: OutCoercion -> SimplCont -> SimplM SimplCont
       addCoerce co1 (CastIt co2 cont)
         = addCoerce (mkTransCo co1 co2) cont

       addCoerce co cont@(ApplyToTy { sc_arg_ty = arg_ty, sc_cont = tail })
         | Just (arg_ty', co') <- pushCoTyArg co arg_ty
         = do { tail' <- addCoerce co' tail
              ; return (cont { sc_arg_ty = arg_ty', sc_cont = tail' }) }

       addCoerce co (ApplyToVal { sc_arg = arg, sc_env = arg_se
                                , sc_dup = dup, sc_cont = tail })
         | Just (co1, co2) <- pushCoValArg co
         , Pair _ new_ty <- coercionKind co1
         , not (isTypeLevPoly new_ty)  -- without this check, we get a lev-poly arg
                                       -- See Note [Levity polymorphism invariants] in CoreSyn
                                       -- test: typecheck/should_run/EtaExpandLevPoly
         = do { (dup', arg_se', arg') <- simplArg env dup arg_se arg
                   -- When we build the ApplyTo we can't mix the OutCoercion
                   -- 'co' with the InExpr 'arg', so we simplify
                   -- to make it all consistent.  It's a bit messy.
                   -- But it isn't a common case.
                   -- Example of use: Trac #995
              ; tail' <- addCoerce co2 tail
              ; return (ApplyToVal { sc_arg  = mkCast arg' co1
                                   , sc_env  = arg_se'
                                   , sc_dup  = dup'
                                   , sc_cont = tail' }) }

       addCoerce co cont
         | isReflexiveCo co = return cont
         | otherwise        = return (CastIt co cont)
                -- It's worth checking isReflexiveCo.
                -- For example, in the initial form of a worker
                -- we may find  (coerce T (coerce S (\x.e))) y
                -- and we'd like it to simplify to e[y/x] in one round
                -- of simplification

simplArg :: SimplEnv -> DupFlag -> StaticEnv -> CoreExpr
         -> SimplM (DupFlag, StaticEnv, OutExpr)
simplArg env dup_flag arg_env arg
  | isSimplified dup_flag
  = return (dup_flag, arg_env, arg)
  | otherwise
  = do { arg' <- simplExpr (arg_env `setInScopeAndZapFloats` env) arg
       ; return (Simplified, zapSubstEnv arg_env, arg') }

{-
************************************************************************
*                                                                      *
\subsection{Lambdas}
*                                                                      *
************************************************************************

Note [Zap unfolding when beta-reducing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lambda-bound variables can have stable unfoldings, such as
   $j = \x. \b{Unf=Just x}. e
See Note [Case binders and join points] below; the unfolding for lets
us optimise e better.  However when we beta-reduce it we want to
revert to using the actual value, otherwise we can end up in the
stupid situation of
          let x = blah in
          let b{Unf=Just x} = y
          in ...b...
Here it'd be far better to drop the unfolding and use the actual RHS.
-}

simplLam :: SimplEnv -> [InId] -> InExpr -> SimplCont
         -> SimplM (SimplEnv, OutExpr)

simplLam env [] body cont = simplExprF env body cont

        -- Beta reduction

simplLam env (bndr:bndrs) body (ApplyToTy { sc_arg_ty = arg_ty, sc_cont = cont })
  = do { tick (BetaReduction bndr)
       ; simplLam (extendTvSubst env bndr arg_ty) bndrs body cont }

simplLam env (bndr:bndrs) body (ApplyToVal { sc_arg = arg, sc_env = arg_se
                                           , sc_cont = cont })
  = do  { tick (BetaReduction bndr)
        ; simplNonRecE env (zap_unfolding bndr) (arg, arg_se) (bndrs, body) cont }
  where
    zap_unfolding bndr  -- See Note [Zap unfolding when beta-reducing]
      | isId bndr, isStableUnfolding (realIdUnfolding bndr)
      = setIdUnfolding bndr NoUnfolding
      | otherwise = bndr

      -- discard a non-counting tick on a lambda.  This may change the
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

simplLamBndrs :: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplLamBndrs env bndrs = mapAccumLM simplLamBndr env bndrs

-------------
simplLamBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- Used for lambda binders.  These sometimes have unfoldings added by
-- the worker/wrapper pass that must be preserved, because they can't
-- be reconstructed from context.  For example:
--      f x = case x of (a,b) -> fw a b x
--      fw a b x{=(a,b)} = ...
-- The "{=(a,b)}" is an unfolding we can't reconstruct otherwise.
simplLamBndr env bndr
  | isId bndr && isFragileUnfolding old_unf   -- Special case
  = do { (env1, bndr1) <- simplBinder env bndr
       ; unf'          <- simplUnfolding env1 NotTopLevel Nothing bndr old_unf
       ; let bndr2 = bndr1 `setIdUnfolding` unf'
       ; return (modifyInScope env1 bndr2, bndr2) }

  | otherwise
  = simplBinder env bndr                -- Normal case
  where
    old_unf = idUnfolding bndr

------------------
simplNonRecE :: SimplEnv
             -> InId                    -- The binder, always an Id for simplNonRecE
             -> (InExpr, SimplEnv)      -- Rhs of binding (or arg of lambda)
             -> ([InBndr], InExpr)      -- Body of the let/lambda
                                        --      \xs.e
             -> SimplCont
             -> SimplM (SimplEnv, OutExpr)

-- simplNonRecE is used for
--  * non-top-level non-recursive lets in expressions
--  * beta reduction
--
-- It deals with strict bindings, via the StrictBind continuation,
-- which may abort the whole process
--
-- Precondition: rhs satisfies the let/app invariant
--               Note [CoreSyn let/app invariant] in CoreSyn
--
-- The "body" of the binding comes as a pair of ([InId],InExpr)
-- representing a lambda; so we recurse back to simplLam
-- Why?  Because of the binder-occ-info-zapping done before
--       the call to simplLam in simplExprF (Lam ...)

simplNonRecE env bndr (rhs, rhs_se) (bndrs, body) cont
  = ASSERT( isId bndr )
    do dflags <- getDynFlags
       case () of
         _ | preInlineUnconditionally dflags env NotTopLevel bndr rhs
           -> do { tick (PreInlineUnconditionally bndr)
                 ; -- pprTrace "preInlineUncond" (ppr bndr <+> ppr rhs) $
                  simplLam (extendIdSubst env bndr (mkContEx rhs_se rhs)) bndrs body cont }

           | isStrictId bndr          -- Includes coercions
           -> simplExprF (rhs_se `setFloats` env) rhs
                         (StrictBind bndr bndrs body env cont)

           | Just (bndr', rhs') <- joinPointBinding_maybe bndr rhs
           -> do { let cont_dup_res_ty = resultTypeOfDupableCont (getMode env)
                                           [bndr'] cont
                 ; (env1, bndr1) <- simplNonRecJoinBndr env
                                                        cont_dup_res_ty bndr'
                 ; (env2, bndr2) <- addBndrRules env1 bndr' bndr1
                 ; (env3, cont_dup, cont_nodup)
                     <- prepareLetCont (zapJoinFloats env2) [bndr'] cont
                 ; MASSERT2(cont_dup_res_ty `eqType` contResultType cont_dup,
                     ppr cont_dup_res_ty $$ blankLine $$
                     ppr cont $$ blankLine $$
                     ppr cont_dup $$ blankLine $$
                     ppr cont_nodup)
                 ; env4 <- simplJoinBind env3 NonRecursive cont_dup bndr' bndr2
                                         rhs' rhs_se
                 ; (env5, expr) <- simplLam env4 bndrs body cont_dup
                 ; rebuild (env5 `restoreJoinFloats` env2)
                           (wrapJoinFloats env5 expr) cont_nodup }

           | otherwise
           -> ASSERT( not (isTyVar bndr) )
              do { (env1, bndr1) <- simplNonRecBndr env bndr
                 ; (env2, bndr2) <- addBndrRules env1 bndr bndr1
                 ; env3 <- simplLazyBind env2 NotTopLevel NonRecursive bndr bndr2 rhs rhs_se
                 ; simplLam env3 bndrs body cont }

------------------
simplRecE :: SimplEnv
          -> [(InId, InExpr)]
          -> InExpr
          -> SimplCont
          -> SimplM (SimplEnv, OutExpr)

-- simplRecE is used for
--  * non-top-level recursive lets in expressions
simplRecE env pairs body cont
  | Just pairs' <- joinPointBindings_maybe pairs
  = do  { let bndrs' = map fst pairs'
              cont_dup_res_ty = resultTypeOfDupableCont (getMode env)
                                                        bndrs' cont
        ; env1 <- simplRecJoinBndrs env cont_dup_res_ty bndrs'
                -- NB: bndrs' don't have unfoldings or rules
                -- We add them as we go down
        ; (env2, cont_dup, cont_nodup) <- prepareLetCont (zapJoinFloats env1)
                                                         bndrs' cont
        ; MASSERT2(cont_dup_res_ty `eqType` contResultType cont_dup,
            ppr cont_dup_res_ty $$ blankLine $$
            ppr cont $$ blankLine $$
            ppr cont_dup $$ blankLine $$
            ppr cont_nodup)
        ; env3 <- simplRecBind env2 NotTopLevel (Just cont_dup) pairs'
        ; (env4, expr) <- simplExprF env3 body cont_dup
        ; rebuild (env4 `restoreJoinFloats` env1)
                  (wrapJoinFloats env4 expr) cont_nodup }
  | otherwise
  = do  { let bndrs = map fst pairs
        ; MASSERT(all (not . isJoinId) bndrs)
        ; env1 <- simplRecBndrs env bndrs
                -- NB: bndrs' don't have unfoldings or rules
                -- We add them as we go down
        ; env2 <- simplRecBind env1 NotTopLevel (Just cont) pairs
        ; simplExprF env2 body cont }


{-
************************************************************************
*                                                                      *
                     Variables
*                                                                      *
************************************************************************
-}

simplVar :: SimplEnv -> InVar -> SimplM OutExpr
-- Look up an InVar in the environment
simplVar env var
  | isTyVar var = return (Type (substTyVar env var))
  | isCoVar var = return (Coercion (substCoVar env var))
  | otherwise
  = case substId env var of
        DoneId var1          -> return (Var var1)
        DoneEx e             -> return e
        ContEx tvs cvs ids e -> simplExpr (setSubstEnv env tvs cvs ids) e

simplIdF :: SimplEnv -> InId -> SimplCont -> SimplM (SimplEnv, OutExpr)
simplIdF env var cont
  = case substId env var of
        DoneEx e             -> simplExprF (zapSubstEnv env) e trimmed_cont
        ContEx tvs cvs ids e -> simplExprF (setSubstEnv env tvs cvs ids) e cont
                                  -- Don't trim; haven't already simplified
                                  -- the join, so the cont was never copied
        DoneId var1          -> completeCall env var1 trimmed_cont
                -- Note [zapSubstEnv]
                -- The template is already simplified, so don't re-substitute.
                -- This is VITAL.  Consider
                --      let x = e in
                --      let y = \z -> ...x... in
                --      \ x -> ...y...
                -- We'll clone the inner \x, adding x->x' in the id_subst
                -- Then when we inline y, we must *not* replace x by x' in
                -- the inlined copy!!
  where
    trimmed_cont | Just arity <- isJoinIdInEnv_maybe env var
                 = trim_cont arity cont
                 | otherwise
                 = cont

    -- Drop outer context from join point invocation
    -- Note [Case-of-case and join points]
    trim_cont 0 cont@(Stop {})
      = cont
    trim_cont 0 cont
      = mkBoringStop (contResultType cont)
    trim_cont n cont@(ApplyToVal { sc_cont = k })
      = cont { sc_cont = trim_cont (n-1) k }
    trim_cont n cont@(ApplyToTy { sc_cont = k })
      = cont { sc_cont = trim_cont (n-1) k } -- join arity counts types!
    trim_cont _ cont
      = pprPanic "completeCall" $ ppr var $$ ppr cont

---------------------------------------------------------
--      Dealing with a call site

completeCall :: SimplEnv -> OutId -> SimplCont -> SimplM (SimplEnv, OutExpr)
completeCall env var cont
  = do  {   ------------- Try inlining ----------------
          dflags <- getDynFlags
        ; let  (lone_variable, arg_infos, call_cont) = contArgs cont
               n_val_args = length arg_infos
               interesting_cont = interestingCallContext call_cont
               unfolding    = activeUnfolding env var
               maybe_inline = callSiteInline dflags var unfolding
                                             lone_variable arg_infos interesting_cont
        ; case maybe_inline of {
            Just expr      -- There is an inlining!
              ->  do { checkedTick (UnfoldingDone var)
                     ; dump_inline dflags expr cont
                     ; simplExprF (zapSubstEnv env) expr cont }

            ; Nothing -> do               -- No inlining!

        { rule_base <- getSimplRules
        ; let info = mkArgInfo var (getRules rule_base var) n_val_args call_cont
        ; rebuildCall env info cont
    }}}
  where
    dump_inline dflags unfolding cont
      | not (dopt Opt_D_dump_inlinings dflags) = return ()
      | not (dopt Opt_D_verbose_core2core dflags)
      = when (isExternalName (idName var)) $
            liftIO $ printOutputForUser dflags alwaysQualify $
                sep [text "Inlining done:", nest 4 (ppr var)]
      | otherwise
      = liftIO $ printOutputForUser dflags alwaysQualify $
           sep [text "Inlining done: " <> ppr var,
                nest 4 (vcat [text "Inlined fn: " <+> nest 2 (ppr unfolding),
                              text "Cont:  " <+> ppr cont])]

rebuildCall :: SimplEnv
            -> ArgInfo
            -> SimplCont
            -> SimplM (SimplEnv, OutExpr)
rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args, ai_strs = [] }) cont
  -- When we run out of strictness args, it means
  -- that the call is definitely bottom; see SimplUtils.mkArgInfo
  -- Then we want to discard the entire strict continuation.  E.g.
  --    * case (error "hello") of { ... }
  --    * (error "Hello") arg
  --    * f (error "Hello") where f is strict
  --    etc
  -- Then, especially in the first of these cases, we'd like to discard
  -- the continuation, leaving just the bottoming expression.  But the
  -- type might not be right, so we may have to add a coerce.
  | not (contIsTrivial cont)     -- Only do this if there is a non-trivial
  = return (env, castBottomExpr res cont_ty)  -- continuation to discard, else we do it
  where                                       -- again and again!
    res     = argInfoExpr fun rev_args
    cont_ty = contResultType cont

rebuildCall env info (CastIt co cont)
  = rebuildCall env (addCastTo info co) cont

rebuildCall env info (ApplyToTy { sc_arg_ty = arg_ty, sc_cont = cont })
  = rebuildCall env (info `addTyArgTo` arg_ty) cont

rebuildCall env info@(ArgInfo { ai_encl = encl_rules, ai_type = fun_ty
                              , ai_strs = str:strs, ai_discs = disc:discs })
            (ApplyToVal { sc_arg = arg, sc_env = arg_se
                        , sc_dup = dup_flag, sc_cont = cont })
  | isSimplified dup_flag     -- See Note [Avoid redundant simplification]
  = rebuildCall env (addValArgTo info' arg) cont

  | str                 -- Strict argument
  = -- pprTrace "Strict Arg" (ppr arg $$ ppr (seIdSubst env) $$ ppr (seInScope env)) $
    simplExprF (arg_se `setFloats` env) arg
               (StrictArg info' cci cont)
                -- Note [Shadowing]

  | otherwise                           -- Lazy argument
        -- DO NOT float anything outside, hence simplExprC
        -- There is no benefit (unlike in a let-binding), and we'd
        -- have to be very careful about bogus strictness through
        -- floating a demanded let.
  = do  { arg' <- simplExprC (arg_se `setInScopeAndZapFloats` env) arg
                             (mkLazyArgStop (funArgTy fun_ty) cci)
        ; rebuildCall env (addValArgTo info' arg') cont }
  where
    info' = info { ai_strs = strs, ai_discs = discs }
    cci | encl_rules = RuleArgCtxt
        | disc > 0   = DiscArgCtxt  -- Be keener here
        | otherwise  = BoringCtxt   -- Nothing interesting

rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args, ai_rules = rules }) cont
  | null rules
  = rebuild env (argInfoExpr fun rev_args) cont      -- No rules, common case

  | otherwise
  = do {  -- We've accumulated a simplified call in <fun,rev_args>
          -- so try rewrite rules; see Note [RULEs apply to simplified arguments]
          -- See also Note [Rules for recursive functions]
        ; let env' = zapSubstEnv env  -- See Note [zapSubstEnv];
                                      -- and NB that 'rev_args' are all fully simplified
        ; mb_rule <- tryRules env' rules fun (reverse rev_args) cont
        ; case mb_rule of {
             Just (rule_rhs, cont') -> simplExprF env' rule_rhs cont'

                 -- Rules don't match
           ; Nothing -> rebuild env (argInfoExpr fun rev_args) cont      -- No rules
    } }

{-
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
         -> Id -> [ArgSpec] -> SimplCont
         -> SimplM (Maybe (CoreExpr, SimplCont))
-- The SimplEnv already has zapSubstEnv applied to it

tryRules env rules fn args call_cont
  | null rules
  = return Nothing
{- Disabled until we fix #8326
  | fn `hasKey` tagToEnumKey   -- See Note [Optimising tagToEnum#]
  , [_type_arg, val_arg] <- args
  , Select dup bndr ((_,[],rhs1) : rest_alts) se cont <- call_cont
  , isDeadBinder bndr
  = do { dflags <- getDynFlags
       ; let enum_to_tag :: CoreAlt -> CoreAlt
                -- Takes   K -> e  into   tagK# -> e
                -- where tagK# is the tag of constructor K
             enum_to_tag (DataAlt con, [], rhs)
               = ASSERT( isEnumerationTyCon (dataConTyCon con) )
                (LitAlt tag, [], rhs)
              where
                tag = mkMachInt dflags (toInteger (dataConTag con - fIRST_TAG))
             enum_to_tag alt = pprPanic "tryRules: tagToEnum" (ppr alt)

             new_alts = (DEFAULT, [], rhs1) : map enum_to_tag rest_alts
             new_bndr = setIdType bndr intPrimTy
                 -- The binder is dead, but should have the right type
      ; return (Just (val_arg, Select dup new_bndr new_alts se cont)) }
-}
  | otherwise
  = do { dflags <- getDynFlags
       ; case lookupRule dflags (getUnfoldingInRuleMatch env) (activeRule env)
                         fn (argInfoAppArgs args) rules of {
           Nothing ->
             do { nodump dflags  -- This ensures that an empty file is written
                ; return Nothing } ;  -- No rule matches
           Just (rule, rule_rhs) ->
             do { checkedTick (RuleFired (ruleName rule))
                ; let cont' = pushSimplifiedArgs env
                                                 (drop (ruleArity rule) args)
                                                 call_cont
                              -- (ruleArity rule) says how
                              -- many args the rule consumed

                      occ_anald_rhs = occurAnalyseExpr rule_rhs
                          -- See Note [Occurrence-analyse after rule firing]
                ; dump dflags rule rule_rhs
                ; return (Just (occ_anald_rhs, cont')) }}}
  where
    printRuleModule rule =
      parens
        (maybe (text "BUILTIN") (pprModuleName . moduleName) (ruleModule rule))

    dump dflags rule rule_rhs
      | dopt Opt_D_dump_rule_rewrites dflags
      = log_rule dflags Opt_D_dump_rule_rewrites "Rule fired" $ vcat
          [ text "Rule:" <+> ftext (ruleName rule)
          , text "Module:" <+>  printRuleModule rule
          , text "Before:" <+> hang (ppr fn) 2 (sep (map ppr args))
          , text "After: " <+> pprCoreExpr rule_rhs
          , text "Cont:  " <+> ppr call_cont ]

      | dopt Opt_D_dump_rule_firings dflags
      = log_rule dflags Opt_D_dump_rule_firings "Rule fired:" $
          ftext (ruleName rule)
            <+> printRuleModule rule

      | otherwise
      = return ()

    nodump dflags
      | dopt Opt_D_dump_rule_rewrites dflags
      = liftIO $ dumpSDoc dflags alwaysQualify Opt_D_dump_rule_rewrites "" empty

      | dopt Opt_D_dump_rule_firings dflags
      = liftIO $ dumpSDoc dflags alwaysQualify Opt_D_dump_rule_firings "" empty

      | otherwise
      = return ()

    log_rule dflags flag hdr details
      = liftIO . dumpSDoc dflags alwaysQualify flag "" $
                   sep [text hdr, nest 4 details]

{- Note [Occurrence-analyse after rule firing]
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

The code in SimplUtils.prepareAlts has the effect of generalise this
idea to look for a case where we're scrutinising a variable, and we
know that only the default case can match.  For example:

        case x of
          0#      -> ...
          DEFAULT -> ...(case x of
                         0#      -> ...
                         DEFAULT -> ...) ...

Here the inner case is first trimmed to have only one alternative, the
DEFAULT, after which it's an instance of the previous case.  This
really only shows up in eliminating error-checking code.

Note that SimplUtils.mkCase combines identical RHSs.  So

        case e of       ===> case e of DEFAULT -> r
           True  -> r
           False -> r

Now again the case may be elminated by the CaseElim transformation.
This includes things like (==# a# b#)::Bool so that we simplify
      case ==# a# b# of { True -> x; False -> x }
to just
      x
This particular example shows up in default methods for
comparison operations (e.g. in (>=) for Int.Int32)

Note [Case elimination: lifted case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a case over a lifted type has a single alternative, and is being used
as a strict 'let' (all isDeadBinder bndrs), we may want to do this
transformation:

    case e of r       ===>   let r = e in ...r...
      _ -> ...r...

        (a) 'e' is already evaluated (it may so if e is a variable)
            Specifically we check (exprIsHNF e).  In this case
            we can just allocate the WHNF directly with a let.
or
        (b) 'x' is not used at all and e is ok-for-speculation
             The ok-for-spec bit checks that we don't lose any
             exceptions or divergence.

             NB: it'd be *sound* to switch from case to let if the
             scrutinee was not yet WHNF but was guaranteed to
             converge; but sticking with case means we won't build a
             thunk

or
        (c) 'x' is used strictly in the body, and 'e' is a variable
            Then we can just substitute 'e' for 'x' in the body.
            See Note [Eliminating redundant seqs]

For (b), the "not used at all" test is important.  Consider
   case (case a ># b of { True -> (p,q); False -> (q,p) }) of
     r -> blah
The scrutinee is ok-for-speculation (it looks inside cases), but we do
not want to transform to
   let r = case a ># b of { True -> (p,q); False -> (q,p) }
   in blah
because that builds an unnecessary thunk.

Note [Eliminating redundant seqs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have this:
   case x of r { _ -> ..r.. }
where 'r' is used strictly in (..r..), the case is effectively a 'seq'
on 'x', but since 'r' is used strictly anyway, we can safely transform to
   (...x...)

Note that this can change the error behaviour.  For example, we might
transform
    case x of { _ -> error "bad" }
    --> error "bad"
which is might be puzzling if 'x' currently lambda-bound, but later gets
let-bound to (error "good").

Nevertheless, the paper "A semantics for imprecise exceptions" allows
this transformation. If you want to fix the evaluation order, use
'pseq'.  See Trac #8900 for an example where the loss of this
transformation bit us in practice.

See also Note [Empty case alternatives] in CoreSyn.

Just for reference, the original code (added Jan 13) looked like this:
     || case_bndr_evald_next rhs

    case_bndr_evald_next :: CoreExpr -> Bool
      -- See Note [Case binder next]
    case_bndr_evald_next (Var v)         = v == case_bndr
    case_bndr_evald_next (Cast e _)      = case_bndr_evald_next e
    case_bndr_evald_next (App e _)       = case_bndr_evald_next e
    case_bndr_evald_next (Case e _ _ _)  = case_bndr_evald_next e
    case_bndr_evald_next _               = False

(This came up when fixing Trac #7542. See also Note [Eta reduction of
an eval'd function] in CoreUtils.)


Note [Case elimination: unlifted case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   case a +# b of r -> ...r...
Then we do case-elimination (to make a let) followed by inlining,
to get
        .....(a +# b)....
If we have
   case indexArray# a i of r -> ...r...
we might like to do the same, and inline the (indexArray# a i).
But indexArray# is not okForSpeculation, so we don't build a let
in rebuildCase (lest it get floated *out*), so the inlining doesn't
happen either.

This really isn't a big deal I think. The let can be


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
-}

---------------------------------------------------------
--      Eliminate the case if possible

rebuildCase, reallyRebuildCase
   :: SimplEnv
   -> OutExpr          -- Scrutinee
   -> InId             -- Case binder
   -> [InAlt]          -- Alternatives (inceasing order)
   -> SimplCont
   -> SimplM (SimplEnv, OutExpr)

--------------------------------------------------
--      1. Eliminate the case if there's a known constructor
--------------------------------------------------

rebuildCase env scrut case_bndr alts cont
  | Lit lit <- scrut    -- No need for same treatment as constructors
                        -- because literals are inlined more vigorously
  , not (litIsLifted lit)
  = do  { tick (KnownBranch case_bndr)
        ; case findAlt (LitAlt lit) alts of
            Nothing           -> missingAlt env case_bndr alts cont
            Just (_, bs, rhs) -> simple_rhs bs rhs }

  | Just (con, ty_args, other_args) <- exprIsConApp_maybe (getUnfoldingInRuleMatch env) scrut
        -- Works when the scrutinee is a variable with a known unfolding
        -- as well as when it's an explicit constructor application
  = do  { tick (KnownBranch case_bndr)
        ; case findAlt (DataAlt con) alts of
            Nothing  -> missingAlt env case_bndr alts cont
            Just (DEFAULT, bs, rhs) -> simple_rhs bs rhs
            Just (_, bs, rhs)       -> knownCon env scrut con ty_args other_args
                                                case_bndr bs rhs cont
        }
  where
    simple_rhs bs rhs = ASSERT( null bs )
                        do { env' <- simplNonRecX env case_bndr scrut
                               -- scrut is a constructor application,
                               -- hence satisfies let/app invariant
                           ; simplExprF env' rhs cont }


--------------------------------------------------
--      2. Eliminate the case if scrutinee is evaluated
--------------------------------------------------

rebuildCase env scrut case_bndr alts@[(_, bndrs, rhs)] cont
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
          -- See Note [PrimOp can_fail and has_side_effects] in PrimOp
   = simplExprF env rhs cont

  -- 2b.  Turn the case into a let, if
  --      a) it binds only the case-binder
  --      b) unlifted case: the scrutinee is ok-for-speculation
  --           lifted case: the scrutinee is in HNF (or will later be demanded)
  | all_dead_bndrs
  , if is_unlifted
    then exprOkForSpeculation scrut  -- See Note [Case elimination: unlifted case]
    else exprIsHNF scrut             -- See Note [Case elimination: lifted case]
      || scrut_is_demanded_var scrut
  = do { tick (CaseElim case_bndr)
       ; env' <- simplNonRecX env case_bndr scrut
       ; simplExprF env' rhs cont }

  -- 2c. Try the seq rules if
  --     a) it binds only the case binder
  --     b) a rule for seq applies
  -- See Note [User-defined RULES for seq] in MkId
  | is_plain_seq
  = do { let scrut_ty = exprType scrut
             rhs_ty   = substTy env (exprType rhs)
             out_args = [ TyArg { as_arg_ty  = scrut_ty
                                , as_hole_ty = seq_id_ty }
                        , TyArg { as_arg_ty  = rhs_ty
                                , as_hole_ty = piResultTy seq_id_ty scrut_ty }
                        , ValArg scrut]
             rule_cont = ApplyToVal { sc_dup = NoDup, sc_arg = rhs
                                    , sc_env = env, sc_cont = cont }
             env' = zapSubstEnv env
             -- Lazily evaluated, so we don't do most of this

       ; rule_base <- getSimplRules
       ; mb_rule <- tryRules env' (getRules rule_base seqId) seqId out_args rule_cont
       ; case mb_rule of
           Just (rule_rhs, cont') -> simplExprF env' rule_rhs cont'
           Nothing                -> reallyRebuildCase env scrut case_bndr alts cont }
  where
    is_unlifted        = isUnliftedType (idType case_bndr)
    all_dead_bndrs     = all isDeadBinder bndrs       -- bndrs are [InId]
    is_plain_seq       = all_dead_bndrs && isDeadBinder case_bndr -- Evaluation *only* for effect
    seq_id_ty          = idType seqId

    scrut_is_demanded_var :: CoreExpr -> Bool
            -- See Note [Eliminating redundant seqs]
    scrut_is_demanded_var (Cast s _) = scrut_is_demanded_var s
    scrut_is_demanded_var (Var _)    = isStrictDmd (idDemandInfo case_bndr)
    scrut_is_demanded_var _          = False


rebuildCase env scrut case_bndr alts cont
  = reallyRebuildCase env scrut case_bndr alts cont

--------------------------------------------------
--      3. Catch-all case
--------------------------------------------------

reallyRebuildCase env scrut case_bndr alts cont
  = do  {       -- Prepare the continuation;
                -- The new subst_env is in place
          (env', dup_cont, nodup_cont) <- prepareCaseCont (zapJoinFloats env)
                                                          alts cont

        -- Simplify the alternatives
        ; (scrut', case_bndr', alts') <- simplAlts env' scrut case_bndr alts dup_cont

        ; dflags <- getDynFlags
        ; let alts_ty' = contResultType dup_cont
        -- The seqType below is needed to avoid a space leak (#13426)
        -- but I don't know why.
        ; case_expr <- seqType alts_ty' `seq`
                       mkCase dflags scrut' case_bndr' alts_ty' alts'

        -- Notice that rebuild gets the in-scope set from env', not alt_env
        -- (which in any case is only build in simplAlts)
        -- The case binder *not* scope over the whole returned case-expression
        ; rebuild (env' `restoreJoinFloats` env)
                  (wrapJoinFloats env' case_expr) nodup_cont }

{-
simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

Historical note: we use to do the "case binder swap" in the Simplifier
so there were additional complications if the scrutinee was a variable.
Now the binder-swap stuff is done in the occurrence analyser; see
OccurAnal Note [Binder swap].

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

        ... case e of x { DEFAULT -> rhs } ...

where x::F Int.  Then we'd like to rewrite (F Int) to Int, getting

        case e `cast` co of x'::Int
           I# x# -> let x = x' `cast` sym co
                    in rhs

so that 'rhs' can take advantage of the form of x'.

Notice that Note [Case of cast] (in OccurAnal) may then apply to the result.

Nota Bene: We only do the [Improving seq] transformation if the
case binder 'x' is actually used in the rhs; that is, if the case
is *not* a *pure* seq.
  a) There is no point in adding the cast to a pure seq.
  b) There is a good reason not to: doing so would interfere
     with seq rules (Note [Built-in RULES for seq] in MkId).
     In particular, this [Improving seq] thing *adds* a cast
     while [Built-in RULES for seq] *removes* one, so they
     just flip-flop.

You might worry about
   case v of x { __DEFAULT ->
      ... case (v `cast` co) of y { I# -> ... }}
This is a pure seq (since x is unused), so [Improving seq] won't happen.
But it's ok: the simplifier will replace 'v' by 'x' in the rhs to get
   case v of x { __DEFAULT ->
      ... case (x `cast` co) of y { I# -> ... }}
Now the outer case is not a pure seq, so [Improving seq] will happen,
and then the inner case will disappear.

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
-}

simplAlts :: SimplEnv
          -> OutExpr
          -> InId                       -- Case binder
          -> [InAlt]                    -- Non-empty
          -> SimplCont
          -> SimplM (OutExpr, OutId, [OutAlt])  -- Includes the continuation
-- Like simplExpr, this just returns the simplified alternatives;
-- it does not return an environment
-- The returned alternatives can be empty, none are possible

simplAlts env scrut case_bndr alts cont'
  = do  { let env0 = zapFloats env

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
          return (scrut', case_bndr', alts') }


------------------------------------
improveSeq :: (FamInstEnv, FamInstEnv) -> SimplEnv
           -> OutExpr -> InId -> OutId -> [InAlt]
           -> SimplM (SimplEnv, OutExpr, OutId)
-- Note [Improving seq]
improveSeq fam_envs env scrut case_bndr case_bndr1 [(DEFAULT,_,_)]
  | not (isDeadBinder case_bndr) -- Not a pure seq!  See Note [Improving seq]
  , Just (co, ty2) <- topNormaliseType_maybe fam_envs (idType case_bndr1)
  = do { case_bndr2 <- newId (fsLit "nt") ty2
        ; let rhs  = DoneEx (Var case_bndr2 `Cast` mkSymCo co)
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

simplAlt env _ imposs_deflt_cons case_bndr' cont' (DEFAULT, bndrs, rhs)
  = ASSERT( null bndrs )
    do  { let env' = addBinderUnfolding env case_bndr'
                                        (mkOtherCon imposs_deflt_cons)
                -- Record the constructors that the case-binder *can't* be.
        ; rhs' <- simplExprC env' rhs cont'
        ; return (DEFAULT, [], rhs') }

simplAlt env scrut' _ case_bndr' cont' (LitAlt lit, bndrs, rhs)
  = ASSERT( null bndrs )
    do  { env' <- addAltUnfoldings env scrut' case_bndr' (Lit lit)
        ; rhs' <- simplExprC env' rhs cont'
        ; return (LitAlt lit, [], rhs') }

simplAlt env scrut' _ case_bndr' cont' (DataAlt con, vs, rhs)
  = do  {       -- Deal with the pattern-bound variables
                -- Mark the ones that are in ! positions in the
                -- data constructor as certainly-evaluated.
                -- NB: simplLamBinders preserves this eval info
        ; let vs_with_evals = add_evals (dataConRepStrictness con)
        ; (env', vs') <- simplLamBndrs env vs_with_evals

                -- Bind the case-binder to (con args)
        ; let inst_tys' = tyConAppArgs (idType case_bndr')
              con_app :: OutExpr
              con_app   = mkConApp2 con inst_tys' vs'

        ; env'' <- addAltUnfoldings env' scrut' case_bndr' con_app
        ; rhs' <- simplExprC env'' rhs cont'
        ; return (DataAlt con, vs', rhs') }
  where
        -- add_evals records the evaluated-ness of the bound variables of
        -- a case pattern.  This is *important*.  Consider
        --      data T = T !Int !Int
        --
        --      case x of { T a b -> T (a+1) b }
        --
        -- We really must record that b is already evaluated so that we don't
        -- go and re-evaluate it when constructing the result.
        -- See Note [Data-con worker strictness] in MkId.hs
    add_evals the_strs
        = go vs the_strs
        where
          go [] [] = []
          go (v:vs') strs | isTyVar v = v : go vs' strs
          go (v:vs') (str:strs) = zap str v : go vs' strs
          go _ _ = pprPanic "cat_evals"
                    (ppr con $$
                     ppr vs  $$
                     ppr_with_length the_strs $$
                     ppr_with_length (dataConRepArgTys con) $$
                     ppr_with_length (dataConRepStrictness con))
            where
              ppr_with_length list
                = ppr list <+> parens (text "length =" <+> ppr (length list))
                                    -- NB: If this panic triggers, note that
                                    -- NoStrictnessMark doesn't print!

          zap str v = setCaseBndrEvald str $ -- Add eval'dness info
                      zapIdOccInfo v         -- And kill occ info;
                                             -- see Note [Case alternative occ info]

addAltUnfoldings :: SimplEnv -> Maybe OutExpr -> OutId -> OutExpr -> SimplM SimplEnv
addAltUnfoldings env scrut case_bndr con_app
  = do { dflags <- getDynFlags
       ; let con_app_unf = mkSimpleUnfolding dflags con_app
             env1 = addBinderUnfolding env case_bndr con_app_unf

             -- See Note [Add unfolding for scrutinee]
             env2 = case scrut of
                      Just (Var v)           -> addBinderUnfolding env1 v con_app_unf
                      Just (Cast (Var v) co) -> addBinderUnfolding env1 v $
                                                mkSimpleUnfolding dflags (Cast con_app (mkSymCo co))
                      _                      -> env1

       ; traceSmpl "addAltUnf" (vcat [ppr case_bndr <+> ppr scrut, ppr con_app])
       ; return env2 }

addBinderUnfolding :: SimplEnv -> Id -> Unfolding -> SimplEnv
addBinderUnfolding env bndr unf
  | debugIsOn, Just tmpl <- maybeUnfoldingTemplate unf
  = WARN( not (eqType (idType bndr) (exprType tmpl)),
          ppr bndr $$ ppr (idType bndr) $$ ppr tmpl $$ ppr (exprType tmpl) )
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

This showed up in Trac #13027.

Note [Add unfolding for scrutinee]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general it's unlikely that a variable scrutinee will appear
in the case alternatives   case x of { ...x unlikely to appear... }
because the binder-swap in OccAnal has got rid of all such occcurrences
See Note [Binder swap] in OccAnal.

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
it's also good for case-elimintation -- suppose that 'f' was inlined
and did multi-level case analysis, then we'd solve it in one
simplifier sweep instead of two.

Exactly the same issue arises in SpecConstr;
see Note [Add scrutinee to ValueEnv too] in SpecConstr

HOWEVER, given
  case x of y { Just a -> r1; Nothing -> r2 }
we do not want to add the unfolding x -> y to 'x', which might seem cool,
since 'y' itself has different unfoldings in r1 and r2.  Reason: if we
did that, we'd have to zap y's deadness info and that is a very useful
piece of information.

So instead we add the unfolding x -> Just a, and x -> Nothing in the
respective RHSs.


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
         -> OutExpr                             -- The scrutinee
         -> DataCon -> [OutType] -> [OutExpr]   -- The scrutinee (in pieces)
         -> InId -> [InBndr] -> InExpr          -- The alternative
         -> SimplCont
         -> SimplM (SimplEnv, OutExpr)

knownCon env scrut dc dc_ty_args dc_args bndr bs rhs cont
  = do  { env'  <- bind_args env bs dc_args
        ; env'' <- bind_case_bndr env'
        ; simplExprF env'' rhs cont }
  where
    zap_occ = zapBndrOccInfo (isDeadBinder bndr)    -- bndr is an InId

                  -- Ugh!
    bind_args env' [] _  = return env'

    bind_args env' (b:bs') (Type ty : args)
      = ASSERT( isTyVar b )
        bind_args (extendTvSubst env' b ty) bs' args

    bind_args env' (b:bs') (Coercion co : args)
      = ASSERT( isCoVar b )
        bind_args (extendCvSubst env' b co) bs' args

    bind_args env' (b:bs') (arg : args)
      = ASSERT( isId b )
        do { let b' = zap_occ b
             -- Note that the binder might be "dead", because it doesn't
             -- occur in the RHS; and simplNonRecX may therefore discard
             -- it via postInlineUnconditionally.
             -- Nevertheless we must keep it if the case-binder is alive,
             -- because it may be used in the con_app.  See Note [knownCon occ info]
           ; env'' <- simplNonRecX env' b' arg  -- arg satisfies let/app invariant
           ; bind_args env'' bs' args }

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
      | isDeadBinder bndr   = return env
      | exprIsTrivial scrut = return (extendIdSubst env bndr (DoneEx scrut))
      | otherwise           = do { dc_args <- mapM (simplVar env) bs
                                         -- dc_ty_args are aready OutTypes,
                                         -- but bs are InBndrs
                                 ; let con_app = Var (dataConWorkId dc)
                                                 `mkTyApps` dc_ty_args
                                                 `mkApps`   dc_args
                                 ; simplNonRecX env bndr con_app }

-------------------
missingAlt :: SimplEnv -> Id -> [InAlt] -> SimplCont -> SimplM (SimplEnv, OutExpr)
                -- This isn't strictly an error, although it is unusual.
                -- It's possible that the simplifier might "see" that
                -- an inner case has no accessible alternatives before
                -- it "sees" that the entire branch of an outer case is
                -- inaccessible.  So we simply put an error case here instead.
missingAlt env case_bndr _ cont
  = WARN( True, text "missingAlt" <+> ppr case_bndr )
    return (env, mkImpossibleExpr (contResultType cont))

{-
************************************************************************
*                                                                      *
\subsection{Duplicating continuations}
*                                                                      *
************************************************************************
-}

prepareCaseCont :: SimplEnv
                -> [InAlt] -> SimplCont
                -> SimplM (SimplEnv,
                           SimplCont,   -- Dupable part
                           SimplCont)   -- Non-dupable part
-- We are considering
--     K[case _ of { p1 -> r1; ...; pn -> rn }]
-- where K is some enclosing continuation for the case
-- Goal: split K into two pieces Kdup,Knodup so that
--       a) Kdup can be duplicated
--       b) Knodup[Kdup[e]] = K[e]
-- The idea is that we'll transform thus:
--          Knodup[ (case _ of { p1 -> Kdup[r1]; ...; pn -> Kdup[rn] }
--
-- We may also return some extra value bindings in SimplEnv (that scope over
-- the entire continuation) as well as some join points (thus must *not* float
-- past the continuation!).
-- Hence, the full story is this:
--     K[case _ of { p1 -> r1; ...; pn -> rn }] ==>
--     F_v[Knodup[F_j[ (case _ of { p1 -> Kdup[r1]; ...; pn -> Kdup[rn] }) ]]]
-- Here F_v represents some values that got floated out and F_j represents some
-- join points that got floated out.
--
-- When case-of-case is off, just make the entire continuation non-dupable

prepareCaseCont env alts cont
  | not (sm_case_case (getMode env))
  = return (env, mkBoringStop (contHoleType cont), cont)
  | not (altsWouldDup alts)
  = return (env, cont, mkBoringStop (contResultType cont))
  | otherwise
  = mkDupableCont env cont

prepareLetCont :: SimplEnv
               -> [InBndr] -> SimplCont
               -> SimplM (SimplEnv,
                          SimplCont,   -- Dupable part
                          SimplCont)   -- Non-dupable part

-- Similar to prepareCaseCont, only for
--     K[let { j1 = r1; ...; jn -> rn } in _]
-- If the js are join points, this will turn into
--     Knodup[join { j1 = Kdup[r1]; ...; jn = Kdup[rn] } in Kdup[_]].
--
-- When case-of-case is off and it's a join binding, just make the entire
-- continuation non-dupable. This is necessary because otherwise
--     case (join j = ... in case e of { A -> jump j 1; ... }) of { B -> ... }
-- becomes
--     join j = case ... of { B -> ... } in
--     case (case e of { A -> jump j 1; ... }) of { B -> ... },
-- and the reference to j is invalid.

prepareLetCont env bndrs cont
  | not (isJoinId (head bndrs))
  = return (env, cont, mkBoringStop (contResultType cont))
  | not (sm_case_case (getMode env))
  = return (env, mkBoringStop (contHoleType cont), cont)
  | otherwise
  = mkDupableCont env cont

-- Predict the result type of the dupable cont returned by prepareLetCont (= the
-- hole type of the non-dupable part). Ugly, but sadly necessary so that we can
-- know what the new type of a recursive join point will be before we start
-- simplifying it.
resultTypeOfDupableCont :: SimplifierMode
                        -> [InBndr]
                        -> SimplCont
                        -> OutType   -- INVARIANT: Result type of dupable cont
                                     -- returned by prepareLetCont
-- IMPORTANT: This must be kept in sync with mkDupableCont!
resultTypeOfDupableCont mode bndrs cont
  | not (any isJoinId bndrs)   = contResultType cont
  | not (sm_case_case mode)    = contHoleType   cont
  | otherwise                  = go cont
  where
    go cont | contIsDupable cont = contResultType cont
    go (Stop {}) = panic "typeOfDupableCont" -- Handled by previous eqn
    go (CastIt _  cont)     = go cont
    go cont@(TickIt {})     = contHoleType cont
    go cont@(StrictBind {}) = contHoleType cont
    go (StrictArg _ _ cont) = go cont
    go cont@(ApplyToTy  {}) = go (sc_cont cont)
    go cont@(ApplyToVal {}) = go (sc_cont cont)
    go (Select { sc_alts = alts, sc_cont = cont })
      | not (sm_case_case mode) = contHoleType cont
      | not (altsWouldDup alts) = contResultType cont
      | otherwise               = go cont

altsWouldDup :: [InAlt] -> Bool -- True iff strictly > 1 non-bottom alternative
altsWouldDup []  = False        -- See Note [Bottom alternatives]
altsWouldDup [_] = False
altsWouldDup (alt:alts)
  | is_bot_alt alt = altsWouldDup alts
  | otherwise      = not (all is_bot_alt alts)
  where
    is_bot_alt (_,_,rhs) = exprIsBottom rhs

{-
Note [Bottom alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have
     case (case x of { A -> error .. ; B -> e; C -> error ..)
       of alts
then we can just duplicate those alts because the A and C cases
will disappear immediately.  This is more direct than creating
join points and inlining them away.  See Trac #4930.
-}

mkDupableCont :: SimplEnv -> SimplCont
              -> SimplM (SimplEnv, SimplCont, SimplCont)

mkDupableCont env cont
  | contIsDupable cont
  = return (env, cont, mkBoringStop (contResultType cont))

mkDupableCont _   (Stop {}) = panic "mkDupableCont"     -- Handled by previous eqn

mkDupableCont env (CastIt ty cont)
  = do  { (env', dup, nodup) <- mkDupableCont env cont
        ; return (env', CastIt ty dup, nodup) }

-- Duplicating ticks for now, not sure if this is good or not
mkDupableCont env cont@(TickIt{})
  = return (env, mkBoringStop (contHoleType cont), cont)

mkDupableCont env cont@(StrictBind {})
  =  return (env, mkBoringStop (contHoleType cont), cont)
        -- See Note [Duplicating StrictBind]

mkDupableCont env (StrictArg info cci cont)
        -- See Note [Duplicating StrictArg]
  = do { (env', dup, nodup) <- mkDupableCont env cont
       ; (env'', args')     <- mapAccumLM makeTrivialArg env' (ai_args info)
       ; return (env'', StrictArg (info { ai_args = args' }) cci dup, nodup) }

mkDupableCont env cont@(ApplyToTy { sc_cont = tail })
  = do  { (env', dup_cont, nodup_cont) <- mkDupableCont env tail
        ; return (env', cont { sc_cont = dup_cont }, nodup_cont ) }

mkDupableCont env (ApplyToVal { sc_arg = arg, sc_dup = dup, sc_env = se, sc_cont = cont })
  =     -- e.g.         [...hole...] (...arg...)
        --      ==>
        --              let a = ...arg...
        --              in [...hole...] a
    do  { (env', dup_cont, nodup_cont) <- mkDupableCont env cont
        ; (_, se', arg') <- simplArg env' dup se arg
        ; (env'', arg'') <- makeTrivial NotTopLevel env' (fsLit "karg") arg'
        ; let app_cont = ApplyToVal { sc_arg = arg'', sc_env = se'
                                    , sc_dup = OkToDup, sc_cont = dup_cont }
        ; return (env'', app_cont, nodup_cont) }

mkDupableCont env (Select { sc_bndr = case_bndr, sc_alts = alts
                          , sc_env = se, sc_cont = cont })
  =     -- e.g.         (case [...hole...] of { pi -> ei })
        --      ===>
        --              let ji = \xij -> ei
        --              in case [...hole...] of { pi -> ji xij }
    do  { tick (CaseOfCase case_bndr)
        ; (env', dup_cont, nodup_cont) <- prepareCaseCont env alts cont
                -- NB: We call prepareCaseCont here.  If there is only one
                -- alternative, then dup_cont may be big, but that's ok
                -- because we push it into the single alternative, and then
                -- use mkDupableAlt to turn that simplified alternative into
                -- a join point if it's too big to duplicate.
                -- And this is important: see Note [Fusing case continuations]

        ; let alt_env = se `setInScopeAndZapFloats` env'

        ; (alt_env', case_bndr') <- simplBinder alt_env case_bndr
        ; alts' <- mapM (simplAlt alt_env' Nothing [] case_bndr' dup_cont) alts
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

        ; (env'', alts'') <- mkDupableAlts env' case_bndr' alts'
        ; return (env'',  -- Note [Duplicated env]
                  Select { sc_dup = OkToDup
                         , sc_bndr = case_bndr', sc_alts = alts''
                         , sc_env = zapSubstEnv env''
                         , sc_cont = mkBoringStop (contHoleType nodup_cont) },
                  nodup_cont) }


mkDupableAlts :: SimplEnv -> OutId -> [InAlt]
              -> SimplM (SimplEnv, [InAlt])
-- Absorbs the continuation into the new alternatives

mkDupableAlts env case_bndr' the_alts
  = go env the_alts
  where
    go env0 [] = return (env0, [])
    go env0 (alt:alts)
        = do { (env1, alt') <- mkDupableAlt env0 case_bndr' alt
             ; (env2, alts') <- go env1 alts
             ; return (env2, alt' : alts' ) }

mkDupableAlt :: SimplEnv -> OutId -> (AltCon, [CoreBndr], CoreExpr)
              -> SimplM (SimplEnv, (AltCon, [CoreBndr], CoreExpr))
mkDupableAlt env case_bndr (con, bndrs', rhs') = do
  dflags <- getDynFlags
  if exprIsDupable dflags rhs'  -- Note [Small alternative rhs]
   then return (env, (con, bndrs', rhs'))
   else
    do  { let rhs_ty'  = exprType rhs'
              scrut_ty = idType case_bndr
              case_bndr_w_unf
                = case con of
                      DEFAULT    -> case_bndr
                      DataAlt dc -> setIdUnfolding case_bndr unf
                          where
                                 -- See Note [Case binders and join points]
                             unf = mkInlineUnfolding rhs
                             rhs = mkConApp2 dc (tyConAppArgs scrut_ty) bndrs'

                      LitAlt {} -> WARN( True, text "mkDupableAlt"
                                                <+> ppr case_bndr <+> ppr con )
                                   case_bndr
                           -- The case binder is alive but trivial, so why has
                           -- it not been substituted away?

              final_bndrs'
                | isDeadBinder case_bndr = filter abstract_over bndrs'
                | otherwise              = bndrs' ++ [case_bndr_w_unf]

              abstract_over bndr
                  | isTyVar bndr = True -- Abstract over all type variables just in case
                  | otherwise    = not (isDeadBinder bndr)
                        -- The deadness info on the new Ids is preserved by simplBinders
              final_args    -- Note [Join point abstraction]
                = varsToCoreExprs final_bndrs'

        ; join_bndr <- newId (fsLit "$j") (mkLamTypes final_bndrs' rhs_ty')
                -- Note [Funky mkLamTypes]

        ; let   -- We make the lambdas into one-shot-lambdas.  The
                -- join point is sure to be applied at most once, and doing so
                -- prevents the body of the join point being floated out by
                -- the full laziness pass
                really_final_bndrs     = map one_shot final_bndrs'
                one_shot v | isId v    = setOneShotLambda v
                           | otherwise = v
                join_rhs   = mkLams really_final_bndrs rhs'
                arity      = length (filter (not . isTyVar) final_bndrs')
                join_arity = length final_bndrs'
                final_join_bndr = (join_bndr `setIdArity` arity)
                                    `asJoinId` join_arity
                join_call  = mkApps (Var final_join_bndr) final_args
                final_join_bind = NonRec final_join_bndr join_rhs

        ; env' <- addPolyBind NotTopLevel env final_join_bind
        ; return (env', (con, bndrs', join_call)) }
                -- See Note [Duplicated env]

{-
Note [Fusing case continuations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important to fuse two successive case continuations when the
first has one alternative.  That's why we call prepareCaseCont here.
Consider this, which arises from thunk splitting (see Note [Thunk
splitting] in WorkWrap):

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
simplify the rhs (I# a) with continuation (StricgtBind body Stop)
Supposing that body is big, we end up with
          let $j a = <let x = I# a in body>
          in case v of { pn -> case rn of
                                 I# a -> $j a }
This is just what we want because the rn produces a box that
the case rn cancels with.

See Trac #4957 a fuller example.

Note [Case binders and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    see Note [Lambda-bound unfoldings] in DmdAnal.

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
So they *must* have an zapped subst-env.  So we can't use completeNonRecX to
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

Note [Join point abstraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NB: This note is now historical. Now that "join point" is not a fuzzy concept
but a formal syntactic construct (as distinguished by the JoinId constructor of
IdDetails), each of these concerns is handled separately, with no need for a
vestigial extra argument.

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


Note [Duplicating StrictArg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The original plan had (where E is a big argument)
e.g.    f E [..hole..]
        ==>     let $j = \a -> f E a
                in $j [..hole..]

But this is terrible! Here's an example:
        && E (case x of { T -> F; F -> T })
Now, && is strict so we end up simplifying the case with

an ArgOf continuation.  If we let-bind it, we get
        let $j = \v -> && E v
        in simplExpr (case x of { T -> F; F -> T })
                     (ArgOf (\r -> $j r)
And after simplifying more we get
        let $j = \v -> && E v
        in case x of { T -> $j F; F -> $j T }
Which is a Very Bad Thing

What we do now is this
        f E [..hole..]
        ==>     let a = E
                in f a [..hole..]
Now if the thing in the hole is a case expression (which is when
we'll call mkDupableCont), we'll push the function call into the
branches, which is what we want.  Now RULES for f may fire, and
call-pattern specialisation.  Here's an example from Trac #3116
     go (n+1) (case l of
                 1  -> bs'
                 _  -> Chunk p fpc (o+1) (l-1) bs')
If we can push the call for 'go' inside the case, we get
call-pattern specialisation for 'go', which is *crucial* for
this program.

Here is the (&&) example:
        && E (case x of { T -> F; F -> T })
  ==>   let a = E in
        case x of { T -> && a F; F -> && a T }
Much better!

Notice that
  * Arguments to f *after* the strict one are handled by
    the ApplyToVal case of mkDupableCont.  Eg
        f [..hole..] E

  * We can only do the let-binding of E because the function
    part of a StrictArg continuation is an explicit syntax
    tree.  In earlier versions we represented it as a function
    (CoreExpr -> CoreEpxr) which we couldn't take apart.

Do *not* duplicate StrictBind and StritArg continuations.  We gain
nothing by propagating them into the expressions, and we do lose a
lot.

The desire not to duplicate is the entire reason that
mkDupableCont returns a pair of continuations.

Note [Duplicating StrictBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unlike StrictArg, there doesn't seem anything to gain from
duplicating a StrictBind continuation, so we don't.


************************************************************************
*                                                                      *
                    Unfoldings
*                                                                      *
************************************************************************
-}

simplLetUnfolding :: SimplEnv-> TopLevelFlag
                  -> Maybe SimplCont
                  -> InId
                  -> OutExpr
                  -> Unfolding -> SimplM Unfolding
simplLetUnfolding env top_lvl cont_mb id new_rhs unf
  | isStableUnfolding unf
  = simplUnfolding env top_lvl cont_mb id unf
  | otherwise
  = is_bottoming `seq`  -- See Note [Force bottoming field]
    do { dflags <- getDynFlags
       ; return (mkUnfolding dflags InlineRhs is_top_lvl is_bottoming new_rhs) }
            -- We make an  unfolding *even for loop-breakers*.
            -- Reason: (a) It might be useful to know that they are WHNF
            --         (b) In TidyPgm we currently assume that, if we want to
            --             expose the unfolding then indeed we *have* an unfolding
            --             to expose.  (We could instead use the RHS, but currently
            --             we don't.)  The simple thing is always to have one.
  where
    is_top_lvl   = isTopLevel top_lvl
    is_bottoming = isBottomingId id

simplUnfolding :: SimplEnv -> TopLevelFlag -> Maybe SimplCont -> InId
               -> Unfolding -> SimplM Unfolding
-- Note [Setting the new unfolding]
simplUnfolding env top_lvl cont_mb id unf
  = case unf of
      NoUnfolding -> return unf
      BootUnfolding -> return unf
      OtherCon {} -> return unf

      DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args }
        -> do { (env', bndrs') <- simplBinders rule_env bndrs
              ; args' <- mapM (simplExpr env') args
              ; return (mkDFunUnfolding bndrs' con args') }

      CoreUnfolding { uf_tmpl = expr, uf_src = src, uf_guidance = guide }
        | isStableSource src
        -> do { expr' <- if isJoinId id
                            then let Just cont = cont_mb
                                 in simplJoinRhs rule_env id expr cont
                            else simplExpr rule_env expr
              ; case guide of
                  UnfWhen { ug_arity = arity, ug_unsat_ok = sat_ok }  -- Happens for INLINE things
                     -> let guide' = UnfWhen { ug_arity = arity, ug_unsat_ok = sat_ok
                                             , ug_boring_ok = inlineBoringOk expr' }
                        -- Refresh the boring-ok flag, in case expr'
                        -- has got small. This happens, notably in the inlinings
                        -- for dfuns for single-method classes; see
                        -- Note [Single-method classes] in TcInstDcls.
                        -- A test case is Trac #4138
                        in return (mkCoreUnfolding src is_top_lvl expr' guide')
                            -- See Note [Top-level flag on inline rules] in CoreUnfold

                  _other              -- Happens for INLINABLE things
                     -> is_bottoming `seq` -- See Note [Force bottoming field]
                        do { dflags <- getDynFlags
                           ; return (mkUnfolding dflags src is_top_lvl is_bottoming expr') } }
                -- If the guidance is UnfIfGoodArgs, this is an INLINABLE
                -- unfolding, and we need to make sure the guidance is kept up
                -- to date with respect to any changes in the unfolding.

        | otherwise -> return noUnfolding   -- Discard unstable unfoldings
  where
    is_top_lvl   = isTopLevel top_lvl
    is_bottoming = isBottomingId id
    act          = idInlineActivation id
    rule_env     = updMode (updModeForStableUnfoldings act) env
         -- See Note [Simplifying inside stable unfoldings] in SimplUtils

{-
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

If there's an stable unfolding on a loop breaker (which happens for
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

See Note [Forming Rec groups] in OccurAnal
-}

addBndrRules :: SimplEnv -> InBndr -> OutBndr -> SimplM (SimplEnv, OutBndr)
-- Rules are added back into the bin
addBndrRules env in_id out_id
  | null old_rules
  = return (env, out_id)
  | otherwise
  = do { new_rules <- simplRules env (Just (idName out_id)) old_rules
       ; let final_id  = out_id `setIdSpecialisation` mkRuleInfo new_rules
       ; return (modifyInScope env final_id, final_id) }
  where
    old_rules = ruleInfoRules (idSpecialisation in_id)

simplRules :: SimplEnv -> Maybe Name -> [CoreRule] -> SimplM [CoreRule]
simplRules env mb_new_nm rules
  = mapM simpl_rule rules
  where
    simpl_rule rule@(BuiltinRule {})
      = return rule

    simpl_rule rule@(Rule { ru_bndrs = bndrs, ru_args = args
                          , ru_fn = fn_name, ru_rhs = rhs })
      = do { (env', bndrs') <- simplBinders env bndrs
           ; let rhs_ty = substTy env' (exprType rhs)
                 rule_cont = mkBoringStop rhs_ty
                 rule_env  = updMode updModeForRules env'
           ; args' <- mapM (simplExpr rule_env) args
           ; rhs'  <- simplExprC rule_env rhs rule_cont
           ; return (rule { ru_bndrs = bndrs'
                          , ru_fn    = mb_new_nm `orElse` fn_name
                          , ru_args  = args'
                          , ru_rhs   = rhs' }) }
