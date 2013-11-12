%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import DynFlags
import SimplMonad
import Type hiding      ( substTy, extendTvSubst, substTyVar )
import SimplEnv
import SimplUtils
import FamInstEnv       ( FamInstEnv )
import Literal          ( litIsLifted ) --, mkMachInt ) -- temporalily commented out. See #8326
import Id
import MkId             ( seqId, realWorldPrimId )
import MkCore           ( mkImpossibleExpr, castBottomExpr )
import IdInfo
import Name             ( mkSystemVarName, isExternalName )
import Coercion hiding  ( substCo, substTy, substCoVar, extendTvSubst )
import OptCoercion      ( optCoercion )
import FamInstEnv       ( topNormaliseType_maybe )
import DataCon          ( DataCon, dataConWorkId, dataConRepStrictness
                        , isMarkedStrict ) --, dataConTyCon, dataConTag, fIRST_TAG )
--import TyCon            ( isEnumerationTyCon ) -- temporalily commented out. See #8326
import CoreMonad        ( Tick(..), SimplifierMode(..) )
import CoreSyn
import Demand           ( StrictSig(..), dmdTypeDepth )
import PprCore          ( pprParendExpr, pprCoreExpr )
import CoreUnfold
import CoreUtils
import CoreArity
--import PrimOp           ( tagToEnumKey ) -- temporalily commented out. See #8326
import Rules            ( lookupRule, getRules )
import TysPrim          ( realWorldStatePrimTy ) --, intPrimTy ) -- temporalily commented out. See #8326
import BasicTypes       ( TopLevelFlag(..), isTopLevel, RecFlag(..) )
import MonadUtils       ( foldlM, mapAccumLM, liftIO )
import Maybes           ( orElse )
--import Unique           ( hasKey ) -- temporalily commented out. See #8326
import Control.Monad
import Data.List        ( mapAccumL )
import Outputable
import FastString
import Pair
import Util
import ErrUtils
\end{code}


The guts of the simplifier is in this module, but the driver loop for
the simplifier is in SimplCore.lhs.


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


%************************************************************************
%*                                                                      *
\subsection{Bindings}
%*                                                                      *
%************************************************************************

\begin{code}
simplTopBinds :: SimplEnv -> [InBind] -> SimplM SimplEnv

simplTopBinds env0 binds0
  = do  {       -- Put all the top-level binders into scope at the start
                -- so that if a transformation rule has unexpectedly brought
                -- anything into scope, then we don't get a complaint about that.
                -- It's rather as if the top-level binders were imported.
                -- See note [Glomming] in OccurAnal.
        ; env1 <- simplRecBndrs env0 (bindersOfBinds binds0)
        ; dflags <- getDynFlags
        ; let dump_flag = dopt Opt_D_verbose_core2core dflags
        ; env2 <- simpl_binds dump_flag env1 binds0
        ; freeTick SimplifierDone
        ; return env2 }
  where
        -- We need to track the zapped top-level binders, because
        -- they should have their fragile IdInfo zapped (notably occurrence info)
        -- That's why we run down binds and bndrs' simultaneously.
        --
        -- The dump-flag emits a trace for each top-level binding, which
        -- helps to locate the tracing for inlining and rule firing
    simpl_binds :: Bool -> SimplEnv -> [InBind] -> SimplM SimplEnv
    simpl_binds _    env []           = return env
    simpl_binds dump env (bind:binds) = do { env' <- trace_bind dump bind $
                                                     simpl_bind env bind
                                           ; simpl_binds dump env' binds }

    trace_bind True  bind = pprTrace "SimplBind" (ppr (bindersOf bind))
    trace_bind False _    = \x -> x

    simpl_bind env (Rec pairs)  = simplRecBind      env  TopLevel pairs
    simpl_bind env (NonRec b r) = simplRecOrTopPair env' TopLevel NonRecursive b b' r
        where
          (env', b') = addBndrRules env b (lookupRecBndr env b)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Lazy bindings}
%*                                                                      *
%************************************************************************

simplRecBind is used for
        * recursive bindings only

\begin{code}
simplRecBind :: SimplEnv -> TopLevelFlag
             -> [(InId, InExpr)]
             -> SimplM SimplEnv
simplRecBind env0 top_lvl pairs0
  = do  { let (env_with_info, triples) = mapAccumL add_rules env0 pairs0
        ; env1 <- go (zapFloats env_with_info) triples
        ; return (env0 `addRecFloats` env1) }
        -- addFloats adds the floats from env1,
        -- _and_ updates env0 with the in-scope set from env1
  where
    add_rules :: SimplEnv -> (InBndr,InExpr) -> (SimplEnv, (InBndr, OutBndr, InExpr))
        -- Add the (substituted) rules to the binder
    add_rules env (bndr, rhs) = (env', (bndr, bndr', rhs))
        where
          (env', bndr') = addBndrRules env bndr (lookupRecBndr env bndr)

    go env [] = return env

    go env ((old_bndr, new_bndr, rhs) : pairs)
        = do { env' <- simplRecOrTopPair env top_lvl Recursive old_bndr new_bndr rhs
             ; go env' pairs }
\end{code}

simplOrTopPair is used for
        * recursive bindings (whether top level or not)
        * top-level non-recursive bindings

It assumes the binder has already been simplified, but not its IdInfo.

\begin{code}
simplRecOrTopPair :: SimplEnv
                  -> TopLevelFlag -> RecFlag
                  -> InId -> OutBndr -> InExpr  -- Binder and rhs
                  -> SimplM SimplEnv    -- Returns an env that includes the binding

simplRecOrTopPair env top_lvl is_rec old_bndr new_bndr rhs
  = do dflags <- getDynFlags
       -- Check for unconditional inline
       if preInlineUnconditionally dflags env top_lvl old_bndr rhs
           then do tick (PreInlineUnconditionally old_bndr)
                   return (extendIdSubst env old_bndr (mkContEx env rhs))
           else simplLazyBind env top_lvl is_rec old_bndr new_bndr rhs env
\end{code}


simplLazyBind is used for
  * [simplRecOrTopPair] recursive bindings (whether top level or not)
  * [simplRecOrTopPair] top-level non-recursive bindings
  * [simplNonRecE]      non-top-level *lazy* non-recursive bindings

Nota bene:
    1. It assumes that the binder is *already* simplified,
       and is in scope, and its IdInfo too, except unfolding

    2. It assumes that the binder type is lifted.

    3. It does not check for pre-inline-unconditionallly;
       that should have been done already.

\begin{code}
simplLazyBind :: SimplEnv
              -> TopLevelFlag -> RecFlag
              -> InId -> OutId          -- Binder, both pre-and post simpl
                                        -- The OutId has IdInfo, except arity, unfolding
              -> InExpr -> SimplEnv     -- The RHS and its environment
              -> SimplM SimplEnv

simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se
  = -- pprTrace "simplLazyBind" ((ppr bndr <+> ppr bndr1) $$ ppr rhs $$ ppr (seIdSubst rhs_se)) $
    do  { let   rhs_env     = rhs_se `setInScope` env
                (tvs, body) = case collectTyBinders rhs of
                                (tvs, body) | not_lam body -> (tvs,body)
                                            | otherwise    -> ([], rhs)
                not_lam (Lam _ _) = False
                not_lam _         = True
                        -- Do not do the "abstract tyyvar" thing if there's
                        -- a lambda inside, because it defeats eta-reduction
                        --    f = /\a. \x. g a x
                        -- should eta-reduce


        ; (body_env, tvs') <- simplBinders rhs_env tvs
                -- See Note [Floating and type abstraction] in SimplUtils

        -- Simplify the RHS
        ; let   rhs_cont = mkRhsStop (substTy body_env (exprType body))
        ; (body_env1, body1) <- simplExprF body_env body rhs_cont
        -- ANF-ise a constructor or PAP rhs
        ; (body_env2, body2) <- prepareRhs top_lvl body_env1 bndr1 body1

        ; (env', rhs')
            <-  if not (doFloatFromRhs top_lvl is_rec False body2 body_env2)
                then                            -- No floating, revert to body1
                     do { rhs' <- mkLam tvs' (wrapFloats body_env1 body1) rhs_cont
                        ; return (env, rhs') }

                else if null tvs then           -- Simple floating
                     do { tick LetFloatFromLet
                        ; return (addFloats env body_env2, body2) }

                else                            -- Do type-abstraction first
                     do { tick LetFloatFromLet
                        ; (poly_binds, body3) <- abstractFloats tvs' body_env2 body2
                        ; rhs' <- mkLam tvs' body3 rhs_cont
                        ; env' <- foldlM (addPolyBind top_lvl) env poly_binds
                        ; return (env', rhs') }

        ; completeBind env' top_lvl bndr bndr1 rhs' }
\end{code}

A specialised variant of simplNonRec used when the RHS is already simplified,
notably in knownCon.  It uses case-binding where necessary.

\begin{code}
simplNonRecX :: SimplEnv
             -> InId            -- Old binder
             -> OutExpr         -- Simplified RHS
             -> SimplM SimplEnv

simplNonRecX env bndr new_rhs
  | isDeadBinder bndr   -- Not uncommon; e.g. case (a,b) of c { (p,q) -> p }
  = return env          --               Here c is dead, and we avoid creating
                        --               the binding c = (a,b)
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

completeNonRecX top_lvl env is_strict old_bndr new_bndr new_rhs
  = do  { (env1, rhs1) <- prepareRhs top_lvl (zapFloats env) new_bndr new_rhs
        ; (env2, rhs2) <-
                if doFloatFromRhs NotTopLevel NonRecursive is_strict rhs1 env1
                then do { tick LetFloatFromLet
                        ; return (addFloats env env1, rhs1) }   -- Add the floats to the main env
                else return (env, wrapFloats env1 rhs1)         -- Wrap the floats around the RHS
        ; completeBind env2 NotTopLevel old_bndr new_bndr rhs2 }
\end{code}

{- No, no, no!  Do not try preInlineUnconditionally in completeNonRecX
   Doing so risks exponential behaviour, because new_rhs has been simplified once already
   In the cases described by the folowing commment, postInlineUnconditionally will
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

\begin{code}
prepareRhs :: TopLevelFlag -> SimplEnv -> OutId -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS
prepareRhs top_lvl env id (Cast rhs co)    -- Note [Float coercions]
  | Pair ty1 _ty2 <- coercionKind co       -- Do *not* do this if rhs has an unlifted type
  , not (isUnLiftedType ty1)            -- see Note [Float coercions (unlifted)]
  = do  { (env', rhs') <- makeTrivialWithInfo top_lvl env sanitised_info rhs
        ; return (env', Cast rhs' co) }
  where
    sanitised_info = vanillaIdInfo `setStrictnessInfo` strictnessInfo info
                                   `setDemandInfo` demandInfo info
    info = idInfo id

prepareRhs top_lvl env0 _ rhs0
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
                True -> do { (env'', arg') <- makeTrivial top_lvl env' arg
                           ; return (True, env'', App fun' arg') }
                False -> return (False, env, App fun arg) }
    go n_val_args env (Var fun)
        = return (is_exp, env, Var fun)
        where
          is_exp = isExpandableApp fun n_val_args   -- The fun a constructor or PAP
                        -- See Note [CONLIKE pragma] in BasicTypes
                        -- The definition of is_exp should match that in
                        -- OccurAnal.occAnalApp

    go _ env other
        = return (False, env, other)
\end{code}


Note [Float coercions]
~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
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


\begin{code}
makeTrivialArg :: SimplEnv -> ArgSpec -> SimplM (SimplEnv, ArgSpec)
makeTrivialArg env (ValArg e)  = do { (env', e') <- makeTrivial NotTopLevel env e
                                    ; return (env', ValArg e') }
makeTrivialArg env (CastBy co) = return (env, CastBy co)

makeTrivial :: TopLevelFlag -> SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Binds the expression to a variable, if it's not trivial, returning the variable
makeTrivial top_lvl env expr = makeTrivialWithInfo top_lvl env vanillaIdInfo expr

makeTrivialWithInfo :: TopLevelFlag -> SimplEnv -> IdInfo
                    -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Propagate strictness and demand info to the new binder
-- Note [Preserve strictness when floating coercions]
-- Returned SimplEnv has same substitution as incoming one
makeTrivialWithInfo top_lvl env info expr
  | exprIsTrivial expr                          -- Already trivial
  || not (bindingOk top_lvl expr expr_ty)       -- Cannot trivialise
                                                --   See Note [Cannot trivialise]
  = return (env, expr)
  | otherwise           -- See Note [Take care] below
  = do  { uniq <- getUniqueM
        ; let name = mkSystemVarName uniq (fsLit "a")
              var = mkLocalIdWithInfo name expr_ty info
        ; env'  <- completeNonRecX top_lvl env False var var expr
        ; expr' <- simplVar env' var
        ; return (env', expr') }
        -- The simplVar is needed becase we're constructing a new binding
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
bindingOk top_lvl _ expr_ty
  | isTopLevel top_lvl = not (isUnLiftedType expr_ty)
  | otherwise          = True
\end{code}

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

A case in point is literal strings (a MachStr is not regarded as
trivial):

   foo = Ptr "blob"#

We don't want to ANF-ise this.

%************************************************************************
%*                                                                      *
\subsection{Completing a lazy binding}
%*                                                                      *
%************************************************************************

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

\begin{code}
completeBind :: SimplEnv
             -> TopLevelFlag            -- Flag stuck into unfolding
             -> InId                    -- Old binder
             -> OutId -> OutExpr        -- New binder and RHS
             -> SimplM SimplEnv
-- completeBind may choose to do its work
--      * by extending the substitution (e.g. let x = y in ...)
--      * or by adding to the floats in the envt

completeBind env top_lvl old_bndr new_bndr new_rhs
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
      ; (new_arity, final_rhs) <- tryEtaExpandRhs env new_bndr new_rhs

        -- Simplify the unfolding
      ; new_unfolding <- simplUnfolding env top_lvl old_bndr final_rhs old_unf

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

            final_id = new_bndr `setIdInfo` info3

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
  = do  { unfolding <- simplUnfolding env top_lvl poly_id rhs noUnfolding
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

------------------------------
simplUnfolding :: SimplEnv-> TopLevelFlag
               -> InId
               -> OutExpr
               -> Unfolding -> SimplM Unfolding
-- Note [Setting the new unfolding]
simplUnfolding env _ _ _ df@(DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = do { (env', bndrs') <- simplBinders env bndrs
       ; args' <- mapM (simplExpr env') args
       ; return (df { df_bndrs = bndrs', df_args  = args' }) }

simplUnfolding env top_lvl id _
    (CoreUnfolding { uf_tmpl = expr, uf_arity = arity
                   , uf_src = src, uf_guidance = guide })
  | isStableSource src
  = do { expr' <- simplExpr rule_env expr
       ; let is_top_lvl = isTopLevel top_lvl
       ; case guide of
           UnfWhen sat_ok _    -- Happens for INLINE things
              -> let guide' = UnfWhen sat_ok (inlineBoringOk expr')
                     -- Refresh the boring-ok flag, in case expr'
                     -- has got small. This happens, notably in the inlinings
                     -- for dfuns for single-method classes; see
                     -- Note [Single-method classes] in TcInstDcls.
                     -- A test case is Trac #4138
                 in return (mkCoreUnfolding src is_top_lvl expr' arity guide')
                 -- See Note [Top-level flag on inline rules] in CoreUnfold

           _other              -- Happens for INLINABLE things
              -> let bottoming = isBottomingId id
                 in bottoming `seq` -- See Note [Force bottoming field]
                    do dflags <- getDynFlags
                       return (mkUnfolding dflags src is_top_lvl bottoming expr')
                -- If the guidance is UnfIfGoodArgs, this is an INLINABLE
                -- unfolding, and we need to make sure the guidance is kept up
                -- to date with respect to any changes in the unfolding.
       }
  where
    act      = idInlineActivation id
    rule_env = updMode (updModeForInlineRules act) env
               -- See Note [Simplifying inside InlineRules] in SimplUtils

simplUnfolding _ top_lvl id new_rhs _
  = let bottoming = isBottomingId id
    in bottoming `seq`  -- See Note [Force bottoming field]
       do dflags <- getDynFlags
          return (mkUnfolding dflags InlineRhs (isTopLevel top_lvl) bottoming new_rhs)
          -- We make an  unfolding *even for loop-breakers*.
          -- Reason: (a) It might be useful to know that they are WHNF
          --         (b) In TidyPgm we currently assume that, if we want to
          --             expose the unfolding then indeed we *have* an unfolding
          --             to expose.  (We could instead use the RHS, but currently
          --             we don't.)  The simple thing is always to have one.
\end{code}

Note [Force bottoming field]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to force bottoming, or the new unfolding holds
on to the old unfolding (which is part of the id).

Note [Arity decrease]
~~~~~~~~~~~~~~~~~~~~~
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

If there's an InlineRule on a loop breaker, we hang on to the inlining.
It's pretty dodgy, but the user did say 'INLINE'.  May need to revisit
this choice.

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


%************************************************************************
%*                                                                      *
\subsection[Simplify-simplExpr]{The main function: simplExpr}
%*                                                                      *
%************************************************************************

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


\begin{code}
simplExpr :: SimplEnv -> CoreExpr -> SimplM CoreExpr
simplExpr env expr = simplExprC env expr (mkBoringStop expr_out_ty)
  where
    expr_out_ty :: OutType
    expr_out_ty = substTy env (exprType expr)

simplExprC :: SimplEnv -> CoreExpr -> SimplCont -> SimplM CoreExpr
        -- Simplify an expression, given a continuation
simplExprC env expr cont
  = -- pprTrace "simplExprC" (ppr expr $$ ppr cont {- $$ ppr (seIdSubst env) -} $$ ppr (seFloats env) ) $
    do  { (env', expr') <- simplExprF (zapFloats env) expr cont
        ; -- pprTrace "simplExprC ret" (ppr expr $$ ppr expr') $
          -- pprTrace "simplExprC ret3" (ppr (seInScope env')) $
          -- pprTrace "simplExprC ret4" (ppr (seFloats env')) $
          return (wrapFloats env' expr') }

--------------------------------------------------
simplExprF :: SimplEnv -> InExpr -> SimplCont
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
simplExprF1 env (Var v)        cont = simplIdF env v cont
simplExprF1 env (Lit lit)      cont = rebuild env (Lit lit) cont
simplExprF1 env (Tick t expr)  cont = simplTick env t expr cont
simplExprF1 env (Cast body co) cont = simplCast env body co cont
simplExprF1 env (Coercion co)  cont = simplCoercionF env co cont
simplExprF1 env (Type ty)      cont = ASSERT( contIsRhsOrArg cont )
                                      rebuild env (Type (substTy env ty)) cont
simplExprF1 env (App fun arg)  cont = simplExprF env fun $
                                      ApplyTo NoDup arg env cont

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

simplExprF1 env (Case scrut bndr alts_ty alts) cont
  | sm_case_case (getMode env)
  =     -- Simplify the scrutinee with a Select continuation
    simplExprF env scrut (Select NoDup bndr alts env cont)

  | otherwise
  =     -- If case-of-case is off, simply simplify the case expression
        -- in a vanilla Stop context, and rebuild the result around it
    do  { case_expr' <- simplExprC env scrut
                             (Select NoDup bndr alts env (mkBoringStop alts_out_ty))
        ; rebuild env case_expr' cont }
  where
    alts_out_ty = substTy env alts_ty

simplExprF1 env (Let (Rec pairs) body) cont
  = do  { env' <- simplRecBndrs env (map fst pairs)
                -- NB: bndrs' don't have unfoldings or rules
                -- We add them as we go down

        ; env'' <- simplRecBind env' NotTopLevel pairs
        ; simplExprF env'' body cont }

simplExprF1 env (Let (NonRec bndr rhs) body) cont
  = simplNonRecE env bndr (rhs, env) ([], body) cont

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
  = let opt_co = optCoercion (getCvSubst env) co
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

  -- For non-scoped ticks, we push the continuation inside the
  -- tick.  This has the effect of moving the tick to the outside of a
  -- case or application context, allowing the normal case and
  -- application optimisations to fire.
  | not (tickishScoped tickish)
  = do { (env', expr') <- simplExprF env expr cont
       ; return (env', mkTick tickish expr')
       }

  -- For breakpoints, we cannot do any floating of bindings around the
  -- tick, because breakpoints cannot be split into tick/scope pairs.
  | not (tickishCanSplit tickish)
  = no_floating_past_tick

  | interesting_cont, Just expr' <- push_tick_inside tickish expr
    -- see Note [case-of-scc-of-case]
  = simplExprF env expr' cont

  | otherwise
  = no_floating_past_tick -- was: wrap_floats, see below

 where
  interesting_cont = case cont of
                        Select {} -> True
                        _ -> False

  push_tick_inside t expr0
     | not (tickishCanSplit t) = Nothing
     | otherwise
       = case expr0 of
           Tick t' expr
              -- scc t (tick t' E)
              --   Pull the tick to the outside
              -- This one is important for #5363
              | not (tickishScoped t')
                 -> Just (Tick t' (Tick t expr))

              -- scc t (scc t' E)
              --   Try to push t' into E first, and if that works,
              --   try to push t in again
              | Just expr' <- push_tick_inside t' expr
                 -> push_tick_inside t expr'

              | otherwise -> Nothing

           Case scrut bndr ty alts
              -> Just (Case (mkTick t scrut) bndr ty alts')
             where t_scope = mkNoTick t -- drop the tick on the dup'd ones
                   alts'   = [ (c,bs, mkTick t_scope e) | (c,bs,e) <- alts]
           _other -> Nothing
    where

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
--                                   mkTick (mkNoTick tickish') rhs)
--              -- when wrapping a float with mkTick, we better zap the Id's
--              -- strictness info and arity, because it might be wrong now.
--       ; let env'' = addFloats env (mapFloats env' wrap_float)
--       ; rebuild env'' expr' (TickIt tickish' outc)
--       }


  simplTickish env tickish
    | Breakpoint n ids <- tickish
          = Breakpoint n (map (getDoneId . substId env) ids)
    | otherwise = tickish

  -- push type application and coercion inside a tick
  splitCont :: SimplCont -> (SimplCont, SimplCont)
  splitCont (ApplyTo f (Type t) env c) = (ApplyTo f (Type t) env inc, outc)
    where (inc,outc) = splitCont c
  splitCont (CoerceIt co c) = (CoerceIt co inc, outc)
    where (inc,outc) = splitCont c
  splitCont other = (mkBoringStop (contInputType other), other)

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
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The main rebuilder}
%*                                                                      *
%************************************************************************

\begin{code}
rebuild :: SimplEnv -> OutExpr -> SimplCont -> SimplM (SimplEnv, OutExpr)
-- At this point the substitution in the SimplEnv should be irrelevant
-- only the in-scope set and floats should matter
rebuild env expr cont
  = case cont of
      Stop {}                       -> return (env, expr)
      CoerceIt co cont              -> rebuild env (mkCast expr co) cont
                                    -- NB: mkCast implements the (Coercion co |> g) optimisation
      Select _ bndr alts se cont    -> rebuildCase (se `setFloats` env) expr bndr alts cont
      StrictArg info _ cont         -> rebuildCall env (info `addArgTo` expr) cont
      StrictBind b bs body se cont  -> do { env' <- simplNonRecX (se `setFloats` env) b expr
                                          ; simplLam env' bs body cont }
      ApplyTo dup_flag arg se cont  -- See Note [Avoid redundant simplification]
        | isSimplified dup_flag     -> rebuild env (App expr arg) cont
        | otherwise                 -> do { arg' <- simplExpr (se `setInScope` env) arg
                                          ; rebuild env (App expr arg') cont }
      TickIt t cont                 -> rebuild env (mkTick t expr) cont
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Lambdas}
%*                                                                      *
%************************************************************************

\begin{code}
simplCast :: SimplEnv -> InExpr -> Coercion -> SimplCont
          -> SimplM (SimplEnv, OutExpr)
simplCast env body co0 cont0
  = do  { co1 <- simplCoercion env co0
        ; -- pprTrace "simplCast" (ppr co1) $
          simplExprF env body (addCoerce co1 cont0) }
  where
       addCoerce co cont = add_coerce co (coercionKind co) cont

       add_coerce _co (Pair s1 k1) cont     -- co :: ty~ty
         | s1 `eqType` k1 = cont    -- is a no-op

       add_coerce co1 (Pair s1 _k2) (CoerceIt co2 cont)
         | (Pair _l1 t1) <- coercionKind co2
                --      e |> (g1 :: S1~L) |> (g2 :: L~T1)
                -- ==>
                --      e,                       if S1=T1
                --      e |> (g1 . g2 :: S1~T1)  otherwise
                --
                -- For example, in the initial form of a worker
                -- we may find  (coerce T (coerce S (\x.e))) y
                -- and we'd like it to simplify to e[y/x] in one round
                -- of simplification
         , s1 `eqType` t1  = cont            -- The coerces cancel out
         | otherwise       = CoerceIt (mkTransCo co1 co2) cont

       add_coerce co (Pair s1s2 _t1t2) (ApplyTo dup (Type arg_ty) arg_se cont)
                -- (f |> g) ty  --->   (f ty) |> (g @ ty)
                -- This implements the PushT rule from the paper
         | Just (tyvar,_) <- splitForAllTy_maybe s1s2
         = ASSERT( isTyVar tyvar )
           ApplyTo Simplified (Type arg_ty') (zapSubstEnv arg_se) (addCoerce new_cast cont)
         where
           new_cast = mkInstCo co arg_ty'
           arg_ty' | isSimplified dup = arg_ty
                   | otherwise        = substTy (arg_se `setInScope` env) arg_ty

       add_coerce co (Pair s1s2 t1t2) (ApplyTo dup arg arg_se cont)
         | isFunTy s1s2   -- This implements the Push rule from the paper
         , isFunTy t1t2   -- Check t1t2 to ensure 'arg' is a value arg
                --      (e |> (g :: s1s2 ~ t1->t2)) f
                -- ===>
                --      (e (f |> (arg g :: t1~s1))
                --      |> (res g :: s2->t2)
                --
                -- t1t2 must be a function type, t1->t2, because it's applied
                -- to something but s1s2 might conceivably not be
                --
                -- When we build the ApplyTo we can't mix the out-types
                -- with the InExpr in the argument, so we simply substitute
                -- to make it all consistent.  It's a bit messy.
                -- But it isn't a common case.
                --
                -- Example of use: Trac #995
         = ApplyTo dup new_arg (zapSubstEnv arg_se) (addCoerce co2 cont)
         where
           -- we split coercion t1->t2 ~ s1->s2 into t1 ~ s1 and
           -- t2 ~ s2 with left and right on the curried form:
           --    (->) t1 t2 ~ (->) s1 s2
           [co1, co2] = decomposeCo 2 co
           new_arg    = mkCast arg' (mkSymCo co1)
           arg'       = substExpr (text "move-cast") arg_se' arg
           arg_se'    = arg_se `setInScope` env

       add_coerce co _ cont = CoerceIt co cont
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Lambdas}
%*                                                                      *
%************************************************************************

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

\begin{code}
simplLam :: SimplEnv -> [InId] -> InExpr -> SimplCont
         -> SimplM (SimplEnv, OutExpr)

simplLam env [] body cont = simplExprF env body cont

        -- Beta reduction
simplLam env (bndr:bndrs) body (ApplyTo _ arg arg_se cont)
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
        ; new_lam <- mkLam bndrs' body' cont
        ; rebuild env' new_lam cont }

------------------
simplNonRecE :: SimplEnv
             -> InBndr                  -- The binder
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
-- The "body" of the binding comes as a pair of ([InId],InExpr)
-- representing a lambda; so we recurse back to simplLam
-- Why?  Because of the binder-occ-info-zapping done before
--       the call to simplLam in simplExprF (Lam ...)

        -- First deal with type applications and type lets
        --   (/\a. e) (Type ty)   and   (let a = Type ty in e)
simplNonRecE env bndr (Type ty_arg, rhs_se) (bndrs, body) cont
  = ASSERT( isTyVar bndr )
    do  { ty_arg' <- simplType (rhs_se `setInScope` env) ty_arg
        ; simplLam (extendTvSubst env bndr ty_arg') bndrs body cont }

simplNonRecE env bndr (rhs, rhs_se) (bndrs, body) cont
  = do dflags <- getDynFlags
       case () of
         _
          | preInlineUnconditionally dflags env NotTopLevel bndr rhs ->
            do  { tick (PreInlineUnconditionally bndr)
                ; -- pprTrace "preInlineUncond" (ppr bndr <+> ppr rhs) $
                  simplLam (extendIdSubst env bndr (mkContEx rhs_se rhs)) bndrs body cont }

          | isStrictId bndr ->           -- Includes coercions
            do  { simplExprF (rhs_se `setFloats` env) rhs
                             (StrictBind bndr bndrs body env cont) }

          | otherwise ->
            ASSERT( not (isTyVar bndr) )
            do  { (env1, bndr1) <- simplNonRecBndr env bndr
                ; let (env2, bndr2) = addBndrRules env1 bndr bndr1
                ; env3 <- simplLazyBind env2 NotTopLevel NonRecursive bndr bndr2 rhs rhs_se
                ; simplLam env3 bndrs body cont }
\end{code}

%************************************************************************
%*                                                                      *
                     Variables
%*                                                                      *
%************************************************************************

\begin{code}
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
        DoneEx e             -> simplExprF (zapSubstEnv env) e cont
        ContEx tvs cvs ids e -> simplExprF (setSubstEnv env tvs cvs ids) e cont
        DoneId var1          -> completeCall env var1 cont
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
            liftIO $ printInfoForUser dflags alwaysQualify $
                sep [text "Inlining done:", nest 4 (ppr var)]
      | otherwise
      = liftIO $ printInfoForUser dflags alwaysQualify $
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
  = return (env, castBottomExpr res cont_ty)  -- contination to discard, else we do it
  where                                       -- again and again!
    res     = argInfoExpr fun rev_args
    cont_ty = contResultType cont

rebuildCall env info (CoerceIt co cont)
  = rebuildCall env (addCastTo info co) cont

rebuildCall env info (ApplyTo dup_flag (Type arg_ty) se cont)
  = do { arg_ty' <- if isSimplified dup_flag then return arg_ty
                    else simplType (se `setInScope` env) arg_ty
       ; rebuildCall env (info `addArgTo` Type arg_ty') cont }

rebuildCall env info@(ArgInfo { ai_encl = encl_rules, ai_type = fun_ty
                              , ai_strs = str:strs, ai_discs = disc:discs })
            (ApplyTo dup_flag arg arg_se cont)
  | isSimplified dup_flag     -- See Note [Avoid redundant simplification]
  = rebuildCall env (addArgTo info' arg) cont

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
  = do  { arg' <- simplExprC (arg_se `setInScope` env) arg
                             (mkLazyArgStop (funArgTy fun_ty) cci)
        ; rebuildCall env (addArgTo info' arg') cont }
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
        ; let env' = zapSubstEnv env
              (args, cont') = argInfoValArgs env' rev_args cont
        ; mb_rule <- tryRules env' rules fun args cont'
        ; case mb_rule of {
             Just (rule_rhs, cont'') -> simplExprF env' rule_rhs cont''

                 -- Rules don't match
           ; Nothing -> rebuild env (argInfoExpr fun rev_args) cont      -- No rules
    } }
\end{code}

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
We want all this to unravel in one sweeep.

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


%************************************************************************
%*                                                                      *
                Rewrite rules
%*                                                                      *
%************************************************************************

\begin{code}
tryRules :: SimplEnv -> [CoreRule]
         -> Id -> [OutExpr] -> SimplCont
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
                         fn args rules of {
           Nothing               -> return Nothing ;   -- No rule matches
           Just (rule, rule_rhs) ->
             do { checkedTick (RuleFired (ru_name rule))
                ; dump dflags rule rule_rhs
                ; let cont' = pushSimplifiedArgs env
                                                 (drop (ruleArity rule) args)
                                                 call_cont
                      -- (ruleArity rule) says how many args the rule consumed
                ; return (Just (rule_rhs, cont')) }}}
  where
    dump dflags rule rule_rhs
      | dopt Opt_D_dump_rule_rewrites dflags
      = log_rule dflags Opt_D_dump_rule_rewrites "Rule fired" $ vcat
          [ text "Rule:" <+> ftext (ru_name rule)
          , text "Before:" <+> hang (ppr fn) 2 (sep (map pprParendExpr args))
          , text "After: " <+> pprCoreExpr rule_rhs
          , text "Cont:  " <+> ppr call_cont ]

      | dopt Opt_D_dump_rule_firings dflags
      = log_rule dflags Opt_D_dump_rule_firings "Rule fired:" $
          ftext (ru_name rule)

      | otherwise
      = return ()

    log_rule dflags flag hdr details = liftIO . dumpSDoc dflags flag "" $
      sep [text hdr, nest 4 details]
\end{code}

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


%************************************************************************
%*                                                                      *
                Rebuilding a case expression
%*                                                                      *
%************************************************************************

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
comparision operations (e.g. in (>=) for Int.Int32)

Note [Case elimination: lifted case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We also make sure that we deal with this very common case,
where x has a lifted type:

        case e of
          x -> ...x...

Here we are using the case as a strict let; if x is used only once
then we want to inline it.  We have to be careful that this doesn't
make the program terminate when it would have diverged before, so we
check that
        (a) 'e' is already evaluated (it may so if e is a variable)
            Specifically we check (exprIsHNF e)
or
        (b) 'x' is not used at all and e is ok-for-speculation

For the (b), consider
   case (case a ># b of { True -> (p,q); False -> (q,p) }) of
     r -> blah
The scrutinee is ok-for-speculation (it looks inside cases), but we do
not want to transform to
   let r = case a ># b of { True -> (p,q); False -> (q,p) }
   in blah
because that builds an unnecessary thunk.

Note [Case binder next]
~~~~~~~~~~~~~~~~~~~~~~~
If we have
   case e of f { _ -> f e1 e2 }
then we can safely do CaseElim.   The main criterion is that the
case-binder is evaluated *next*.  Previously we just asked that
the case-binder is used strictly; but that can change
    case x of { _ -> error "bad" }
    --> error "bad"
which is very puzzling if 'x' is later bound to (error "good").
Where the order of evaluation is specified (via seq or case)
we should respect it.
See also Note [Empty case alternatives] in CoreSyn.

So instead we use case_bndr_evald_next to see when f is the *next*
thing to be eval'd.  This came up when fixing Trac #7542.
See also Note [Eta reduction of an eval'd function] in CoreUtils.

  For reference, the old code was an extra disjunct in elim_lifted
       || (strict_case_bndr && scrut_is_var scrut)
      strict_case_bndr = isStrictDmd (idDemandInfo case_bndr)
      scrut_is_var (Cast s _) = scrut_is_var s
      scrut_is_var (Var _)    = True
      scrut_is_var _          = False

      -- True if evaluation of the case_bndr is the next
      -- thing to be eval'd.  Then dropping the case

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
          eta1 :: State# RealWorld ->
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

\begin{code}
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
                           ; simplExprF env' rhs cont }


--------------------------------------------------
--      2. Eliminate the case if scrutinee is evaluated
--------------------------------------------------

rebuildCase env scrut case_bndr [(_, bndrs, rhs)] cont
  -- See if we can get rid of the case altogether
  -- See Note [Case elimination]
  -- mkCase made sure that if all the alternatives are equal,
  -- then there is now only one (DEFAULT) rhs
 | all isDeadBinder bndrs       -- bndrs are [InId]

 , if isUnLiftedType (idType case_bndr)
   then elim_unlifted        -- Satisfy the let-binding invariant
   else elim_lifted
  = do  { -- pprTrace "case elim" (vcat [ppr case_bndr, ppr (exprIsHNF scrut),
          --                            ppr ok_for_spec,
          --                            ppr scrut]) $
          tick (CaseElim case_bndr)
        ; env' <- simplNonRecX env case_bndr scrut
          -- If case_bndr is dead, simplNonRecX will discard
        ; simplExprF env' rhs cont }
  where
    elim_lifted   -- See Note [Case elimination: lifted case]
      = exprIsHNF scrut
     || (is_plain_seq && ok_for_spec)
              -- Note: not the same as exprIsHNF
     || case_bndr_evald_next rhs

    elim_unlifted
      | is_plain_seq = exprOkForSideEffects scrut
            -- The entire case is dead, so we can drop it,
            -- _unless_ the scrutinee has side effects
      | otherwise    = ok_for_spec
            -- The case-binder is alive, but we may be able
            -- turn the case into a let, if the expression is ok-for-spec
            -- See Note [Case elimination: unlifted case]

    ok_for_spec      = exprOkForSpeculation scrut
    is_plain_seq     = isDeadBinder case_bndr -- Evaluation *only* for effect

    case_bndr_evald_next :: CoreExpr -> Bool
      -- See Note [Case binder next]
    case_bndr_evald_next (Var v)         = v == case_bndr
    case_bndr_evald_next (Cast e _)      = case_bndr_evald_next e
    case_bndr_evald_next (App e _)       = case_bndr_evald_next e
    case_bndr_evald_next (Case e _ _ _)  = case_bndr_evald_next e
    case_bndr_evald_next _               = False
      -- Could add a case for Let,
      -- but I'm worried it could become expensive

--------------------------------------------------
--      3. Try seq rules; see Note [User-defined RULES for seq] in MkId
--------------------------------------------------

rebuildCase env scrut case_bndr alts@[(_, bndrs, rhs)] cont
  | all isDeadBinder (case_bndr : bndrs)  -- So this is just 'seq'
  = do { let rhs' = substExpr (text "rebuild-case") env rhs
             env' = zapSubstEnv env
             out_args = [Type (substTy env (idType case_bndr)),
                         Type (exprType rhs'), scrut, rhs']
                      -- Lazily evaluated, so we don't do most of this

       ; rule_base <- getSimplRules
       ; mb_rule <- tryRules env' (getRules rule_base seqId) seqId out_args cont
       ; case mb_rule of
           Just (rule_rhs, cont') -> simplExprF env' rule_rhs cont'
           Nothing                -> reallyRebuildCase env scrut case_bndr alts cont }

rebuildCase env scrut case_bndr alts cont
  = reallyRebuildCase env scrut case_bndr alts cont

--------------------------------------------------
--      3. Catch-all case
--------------------------------------------------

reallyRebuildCase env scrut case_bndr alts cont
  = do  {       -- Prepare the continuation;
                -- The new subst_env is in place
          (env', dup_cont, nodup_cont) <- prepareCaseCont env alts cont

        -- Simplify the alternatives
        ; (scrut', case_bndr', alts') <- simplAlts env' scrut case_bndr alts dup_cont

        ; dflags <- getDynFlags
        ; let alts_ty' = contResultType dup_cont
        ; case_expr <- mkCase dflags scrut' case_bndr' alts_ty' alts'

        -- Notice that rebuild gets the in-scope set from env', not alt_env
        -- (which in any case is only build in simplAlts)
        -- The case binder *not* scope over the whole returned case-expression
        ; rebuild env' case_expr nodup_cont }
\end{code}

simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

Historical note: we use to do the "case binder swap" in the Simplifier
so there were additional complications if the scrutinee was a variable.
Now the binder-swap stuff is done in the occurrence analyer; see
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

\begin{code}
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

        ; fam_envs <- getFamEnvs
        ; (alt_env', scrut', case_bndr') <- improveSeq fam_envs env1 scrut
                                                       case_bndr case_bndr1 alts

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
        -- See Note [Data-con worker strictness] in MkId.lhs
    add_evals the_strs
        = go vs the_strs
        where
          go [] [] = []
          go (v:vs') strs | isTyVar v = v : go vs' strs
          go (v:vs') (str:strs)
            | isMarkedStrict str = evald_v  : go vs' strs
            | otherwise          = zapped_v : go vs' strs
            where
              zapped_v = zapIdOccInfo v   -- See Note [Case alternative occ info]
              evald_v  = zapped_v `setIdUnfolding` evaldUnfolding
          go _ _ = pprPanic "cat_evals" (ppr con $$ ppr vs $$ ppr the_strs)


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
\end{code}

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


%************************************************************************
%*                                                                      *
\subsection{Known constructor}
%*                                                                      *
%************************************************************************

We are a bit careful with occurrence info.  Here's an example

        (\x* -> case x of (a*, b) -> f a) (h v, e)

where the * means "occurs once".  This effectively becomes
        case (h v, e) of (a*, b) -> f a)
and then
        let a* = h v; b = e in f a
and then
        f (h v)

All this should happen in one sweep.

\begin{code}
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

    bind_args env' (b:bs') (arg : args)
      = ASSERT( isId b )
        do { let b' = zap_occ b
             -- Note that the binder might be "dead", because it doesn't
             -- occur in the RHS; and simplNonRecX may therefore discard
             -- it via postInlineUnconditionally.
             -- Nevertheless we must keep it if the case-binder is alive,
             -- because it may be used in the con_app.  See Note [knownCon occ info]
           ; env'' <- simplNonRecX env' b' arg
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
                -- It's possible that the simplifer might "see" that
                -- an inner case has no accessible alternatives before
                -- it "sees" that the entire branch of an outer case is
                -- inaccessible.  So we simply put an error case here instead.
missingAlt env case_bndr _ cont
  = WARN( True, ptext (sLit "missingAlt") <+> ppr case_bndr )
    return (env, mkImpossibleExpr (contResultType cont))
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Duplicating continuations}
%*                                                                      *
%************************************************************************

\begin{code}
prepareCaseCont :: SimplEnv
                -> [InAlt] -> SimplCont
                -> SimplM (SimplEnv, SimplCont, SimplCont)
-- We are considering
--     K[case _ of { p1 -> r1; ...; pn -> rn }]
-- where K is some enclosing continuation for the case
-- Goal: split K into two pieces Kdup,Knodup so that
--       a) Kdup can be duplicated
--       b) Knodup[Kdup[e]] = K[e]
-- The idea is that we'll transform thus:
--          Knodup[ (case _ of { p1 -> Kdup[r1]; ...; pn -> Kdup[rn] }
--
-- We also return some extra bindings in SimplEnv (that scope over
-- the entire continuation)

prepareCaseCont env alts cont
  | many_alts alts = mkDupableCont env cont
  | otherwise      = return (env, cont, mkBoringStop (contResultType cont))
  where
    many_alts :: [InAlt] -> Bool  -- True iff strictly > 1 non-bottom alternative
    many_alts []  = False         -- See Note [Bottom alternatives]
    many_alts [_] = False
    many_alts (alt:alts)
      | is_bot_alt alt = many_alts alts
      | otherwise      = not (all is_bot_alt alts)

    is_bot_alt (_,_,rhs) = exprIsBottom rhs
\end{code}

Note [Bottom alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have
     case (case x of { A -> error .. ; B -> e; C -> error ..)
       of alts
then we can just duplicate those alts because the A and C cases
will disappear immediately.  This is more direct than creating
join points and inlining them away; and in some cases we would
not even create the join points (see Note [Single-alternative case])
and we would keep the case-of-case which is silly.  See Trac #4930.

\begin{code}
mkDupableCont :: SimplEnv -> SimplCont
              -> SimplM (SimplEnv, SimplCont, SimplCont)

mkDupableCont env cont
  | contIsDupable cont
  = return (env, cont, mkBoringStop (contResultType cont))

mkDupableCont _   (Stop {}) = panic "mkDupableCont"     -- Handled by previous eqn

mkDupableCont env (CoerceIt ty cont)
  = do  { (env', dup, nodup) <- mkDupableCont env cont
        ; return (env', CoerceIt ty dup, nodup) }

-- Duplicating ticks for now, not sure if this is good or not
mkDupableCont env cont@(TickIt{})
  = return (env, mkBoringStop (contInputType cont), cont)

mkDupableCont env cont@(StrictBind {})
  =  return (env, mkBoringStop (contInputType cont), cont)
        -- See Note [Duplicating StrictBind]

mkDupableCont env (StrictArg info cci cont)
        -- See Note [Duplicating StrictArg]
  = do { (env', dup, nodup) <- mkDupableCont env cont
       ; (env'', args')     <- mapAccumLM makeTrivialArg env' (ai_args info)
       ; return (env'', StrictArg (info { ai_args = args' }) cci dup, nodup) }

mkDupableCont env (ApplyTo _ arg se cont)
  =     -- e.g.         [...hole...] (...arg...)
        --      ==>
        --              let a = ...arg...
        --              in [...hole...] a
    do  { (env', dup_cont, nodup_cont) <- mkDupableCont env cont
        ; arg' <- simplExpr (se `setInScope` env') arg
        ; (env'', arg'') <- makeTrivial NotTopLevel env' arg'
        ; let app_cont = ApplyTo OkToDup arg'' (zapSubstEnv env'') dup_cont
        ; return (env'', app_cont, nodup_cont) }

mkDupableCont env cont@(Select _ case_bndr [(_, bs, _rhs)] _ _)
--  See Note [Single-alternative case]
--  | not (exprIsDupable rhs && contIsDupable case_cont)
--  | not (isDeadBinder case_bndr)
  | all isDeadBinder bs  -- InIds
    && not (isUnLiftedType (idType case_bndr))
    -- Note [Single-alternative-unlifted]
  = return (env, mkBoringStop (contInputType cont), cont)

mkDupableCont env (Select _ case_bndr alts se cont)
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

        ; let alt_env = se `setInScope` env'

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
                  Select OkToDup case_bndr' alts'' (zapSubstEnv env'')
                         (mkBoringStop (contInputType nodup_cont)),
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
                             unf = mkInlineUnfolding Nothing rhs
                             rhs = mkConApp2 dc (tyConAppArgs scrut_ty) bndrs'

                      LitAlt {} -> WARN( True, ptext (sLit "mkDupableAlt")
                                                <+> ppr case_bndr <+> ppr con )
                                   case_bndr
                           -- The case binder is alive but trivial, so why has
                           -- it not been substituted away?

              used_bndrs' | isDeadBinder case_bndr = filter abstract_over bndrs'
                          | otherwise              = bndrs' ++ [case_bndr_w_unf]

              abstract_over bndr
                  | isTyVar bndr = True -- Abstract over all type variables just in case
                  | otherwise    = not (isDeadBinder bndr)
                        -- The deadness info on the new Ids is preserved by simplBinders

        ; (final_bndrs', final_args)    -- Note [Join point abstraction]
                <- if (any isId used_bndrs')
                   then return (used_bndrs', varsToCoreExprs used_bndrs')
                    else do { rw_id <- newId (fsLit "w") realWorldStatePrimTy
                            ; return ([rw_id], [Var realWorldPrimId]) }

        ; join_bndr <- newId (fsLit "$j") (mkPiTypes final_bndrs' rhs_ty')
                -- Note [Funky mkPiTypes]

        ; let   -- We make the lambdas into one-shot-lambdas.  The
                -- join point is sure to be applied at most once, and doing so
                -- prevents the body of the join point being floated out by
                -- the full laziness pass
                really_final_bndrs     = map one_shot final_bndrs'
                one_shot v | isId v    = setOneShotLambda v
                           | otherwise = v
                join_rhs   = mkLams really_final_bndrs rhs'
                join_arity = exprArity join_rhs
                join_call  = mkApps (Var join_bndr) final_args

        ; env' <- addPolyBind NotTopLevel env (NonRec (join_bndr `setIdArity` join_arity) join_rhs)
        ; return (env', (con, bndrs', join_call)) }
                -- See Note [Duplicated env]
\end{code}

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

But if later inlining scrutines the c, thus

  $j = \c -> ... case c of { I# y -> ... } ...

we won't see that 'c' has already been scrutinised.  This actually
happens in the 'tabulate' function in wave4main, and makes a significant
difference to allocation.

An alternative plan is this:

   $j = \c# -> let c = I# c# in ...c....

but that is bad if 'c' is *not* later scrutinised.

So instead we do both: we pass 'c' and 'c#' , and record in c's inlining
(an InlineRule) that it's really I# c#, thus

   $j = \c# -> \c[=I# c#] -> ...c....

Absence analysis may later discard 'c'.

NB: take great care when doing strictness analysis;
    see Note [Lamba-bound unfoldings] in DmdAnal.

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
The Ord instance of Maybe in PrelMaybe.lhs, for example, took several extra
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

Note [Funky mkPiTypes]
~~~~~~~~~~~~~~~~~~~~~~
Notice the funky mkPiTypes.  If the contructor has existentials
it's possible that the join point will be abstracted over
type varaibles as well as term variables.
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
Join points always have at least one value argument,
for several reasons

* If we try to lift a primitive-typed something out
  for let-binding-purposes, we will *caseify* it (!),
  with potentially-disastrous strictness results.  So
  instead we turn it into a function: \v -> e
  where v::State# RealWorld#.  The value passed to this function
  is realworld#, which generates (almost) no code.

* CPR.  We used to say "&& isUnLiftedType rhs_ty'" here, but now
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
    the ApplyTo case of mkDupableCont.  Eg
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


Note [Single-alternative cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This case is just like the ArgOf case.  Here's an example:
        data T a = MkT !a
        ...(MkT (abs x))...
Then we get
        case (case x of I# x' ->
              case x' <# 0# of
                True  -> I# (negate# x')
                False -> I# x') of y {
          DEFAULT -> MkT y
Because the (case x) has only one alternative, we'll transform to
        case x of I# x' ->
        case (case x' <# 0# of
                True  -> I# (negate# x')
                False -> I# x') of y {
          DEFAULT -> MkT y
But now we do *NOT* want to make a join point etc, giving
        case x of I# x' ->
        let $j = \y -> MkT y
        in case x' <# 0# of
                True  -> $j (I# (negate# x'))
                False -> $j (I# x')
In this case the $j will inline again, but suppose there was a big
strict computation enclosing the orginal call to MkT.  Then, it won't
"see" the MkT any more, because it's big and won't get duplicated.
And, what is worse, nothing was gained by the case-of-case transform.

So, in circumstances like these, we don't want to build join points
and push the outer case into the branches of the inner one. Instead,
don't duplicate the continuation.

When should we use this strategy?  We should not use it on *every*
single-alternative case:
  e.g.  case (case ....) of (a,b) -> (# a,b #)
Here we must push the outer case into the inner one!
Other choices:

   * Match [(DEFAULT,_,_)], but in the common case of Int,
     the alternative-filling-in code turned the outer case into
                case (...) of y { I# _ -> MkT y }

   * Match on single alternative plus (not (isDeadBinder case_bndr))
     Rationale: pushing the case inwards won't eliminate the construction.
     But there's a risk of
                case (...) of y { (a,b) -> let z=(a,b) in ... }
     Now y looks dead, but it'll come alive again.  Still, this
     seems like the best option at the moment.

   * Match on single alternative plus (all (isDeadBinder bndrs))
     Rationale: this is essentially  seq.

   * Match when the rhs is *not* duplicable, and hence would lead to a
     join point.  This catches the disaster-case above.  We can test
     the *un-simplified* rhs, which is fine.  It might get bigger or
     smaller after simplification; if it gets smaller, this case might
     fire next time round.  NB also that we must test contIsDupable
     case_cont *too, because case_cont might be big!

     HOWEVER: I found that this version doesn't work well, because
     we can get         let x = case (...) of { small } in ...case x...
     When x is inlined into its full context, we find that it was a bad
     idea to have pushed the outer case inside the (...) case.

Note [Single-alternative-unlifted]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here's another single-alternative where we really want to do case-of-case:

data Mk1 = Mk1 Int# | Mk2 Int#

M1.f =
    \r [x_s74 y_s6X]
        case
            case y_s6X of tpl_s7m {
              M1.Mk1 ipv_s70 -> ipv_s70;
              M1.Mk2 ipv_s72 -> ipv_s72;
            }
        of
        wild_s7c
        { __DEFAULT ->
              case
                  case x_s74 of tpl_s7n {
                    M1.Mk1 ipv_s77 -> ipv_s77;
                    M1.Mk2 ipv_s79 -> ipv_s79;
                  }
              of
              wild1_s7b
              { __DEFAULT -> ==# [wild1_s7b wild_s7c];
              };
        };

So the outer case is doing *nothing at all*, other than serving as a
join-point.  In this case we really want to do case-of-case and decide
whether to use a real join point or just duplicate the continuation:

    let $j s7c = case x of
                   Mk1 ipv77 -> (==) s7c ipv77
                   Mk1 ipv79 -> (==) s7c ipv79
    in
    case y of
      Mk1 ipv70 -> $j ipv70
      Mk2 ipv72 -> $j ipv72

Hence: check whether the case binder's type is unlifted, because then
the outer case is *not* a seq.
