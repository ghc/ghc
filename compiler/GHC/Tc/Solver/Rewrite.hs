{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Tc.Solver.Rewrite(
   rewrite, rewrite_shallow, rewriteArgsNom,
   rewriteType
 ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Core.TyCo.Ppr ( pprTyVar )
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Core.FamInstEnv (FamInstEnvs, FamInst(..), lookupFamInstEnvByTyCon, apartnessCheck)
import GHC.Tc.Types.Evidence
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep   -- performs delicate algorithm on types
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import qualified GHC.Core.Unify as Unify
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Tc.Solver.Monad as TcS
import GHC.Tc.Errors

import GHC.Types.Basic
import GHC.Utils.Misc
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Data.Pair
import GHC.Exts (oneShot)
import Control.Monad
import GHC.Utils.Monad ( zipWith3M )
import Data.List.NonEmpty ( NonEmpty(..) )

import Control.Arrow ( first )
import Data.Array
import Data.Either

import GHC.Driver.Ppr

{-
************************************************************************
*                                                                      *
*                RewriteEnv & RewriteM
*             The rewriting environment & monad
*                                                                      *
************************************************************************
-}

data RewriteEnv
  = FE { fe_loc     :: !CtLoc             -- See Note [Rewriter CtLoc]
       , fe_flavour :: !CtFlavour
       , fe_eq_rel  :: !EqRel             -- See Note [Rewriter EqRels]
       }

-- | The 'RewriteM' monad is a wrapper around 'TcS' with a 'RewriteEnv'
newtype RewriteM a
  = RewriteM { runRewriteM :: RewriteEnv -> TcS a }
  deriving (Functor)

-- | Smart constructor for 'RewriteM', as describe in Note [The one-shot state
-- monad trick] in "GHC.Utils.Monad".
mkRewriteM :: (RewriteEnv -> TcS a) -> RewriteM a
mkRewriteM f = RewriteM (oneShot f)
{-# INLINE mkRewriteM #-}

instance Monad RewriteM where
  m >>= k  = mkRewriteM $ \env ->
             do { a  <- runRewriteM m env
                ; runRewriteM (k a) env }

instance Applicative RewriteM where
  pure x = mkRewriteM $ \_ -> pure x
  (<*>) = ap

instance HasDynFlags RewriteM where
  getDynFlags = liftTcS getDynFlags

liftTcS :: TcS a -> RewriteM a
liftTcS thing_inside
  = mkRewriteM $ \_ -> thing_inside

-- convenient wrapper when you have a CtEvidence describing
-- the rewriting operation
runRewriteCtEv :: CtEvidence -> RewriteM a -> TcS a
runRewriteCtEv ev
  = runRewrite (ctEvLoc ev) (ctEvFlavour ev) (ctEvEqRel ev)

-- Run thing_inside (which does the rewriting)
runRewrite :: CtLoc -> CtFlavour -> EqRel -> RewriteM a -> TcS a
runRewrite loc flav eq_rel thing_inside
  = runRewriteM thing_inside fmode
  where
    fmode = FE { fe_loc  = loc
               , fe_flavour = flav
               , fe_eq_rel = eq_rel }

traceRewriteM :: String -> SDoc -> RewriteM ()
traceRewriteM herald doc = liftTcS $ traceTcS herald doc
{-# INLINE traceRewriteM #-}  -- see Note [INLINE conditional tracing utilities]

getRewriteEnvField :: (RewriteEnv -> a) -> RewriteM a
getRewriteEnvField accessor
  = mkRewriteM $ \env -> return (accessor env)

getEqRel :: RewriteM EqRel
getEqRel = getRewriteEnvField fe_eq_rel

getRole :: RewriteM Role
getRole = eqRelRole <$> getEqRel

getFlavour :: RewriteM CtFlavour
getFlavour = getRewriteEnvField fe_flavour

getFlavourRole :: RewriteM CtFlavourRole
getFlavourRole
  = do { flavour <- getFlavour
       ; eq_rel <- getEqRel
       ; return (flavour, eq_rel) }

getLoc :: RewriteM CtLoc
getLoc = getRewriteEnvField fe_loc

checkStackDepth :: Type -> RewriteM ()
checkStackDepth ty
  = do { loc <- getLoc
       ; liftTcS $ checkReductionDepth loc ty }

-- | Change the 'EqRel' in a 'RewriteM'.
setEqRel :: EqRel -> RewriteM a -> RewriteM a
setEqRel new_eq_rel thing_inside
  = mkRewriteM $ \env ->
    if new_eq_rel == fe_eq_rel env
    then runRewriteM thing_inside env
    else runRewriteM thing_inside (env { fe_eq_rel = new_eq_rel })
{-# INLINE setEqRel #-}

-- | Make sure that rewriting actually produces a coercion (in other
-- words, make sure our flavour is not Derived)
-- Note [No derived kind equalities]
noBogusCoercions :: RewriteM a -> RewriteM a
noBogusCoercions thing_inside
  = mkRewriteM $ \env ->
    -- No new thunk is made if the flavour hasn't changed (note the bang).
    let !env' = case fe_flavour env of
          Derived -> env { fe_flavour = Wanted WDeriv }
          _       -> env
    in
    runRewriteM thing_inside env'

bumpDepth :: RewriteM a -> RewriteM a
bumpDepth (RewriteM thing_inside)
  = mkRewriteM $ \env -> do
      -- bumpDepth can be called a lot during rewriting so we force the
      -- new env to avoid accumulating thunks.
      { let !env' = env { fe_loc = bumpCtLocDepth (fe_loc env) }
      ; thing_inside env' }

{-
Note [Rewriter EqRels]
~~~~~~~~~~~~~~~~~~~~~~~
When rewriting, we need to know which equality relation -- nominal
or representation -- we should be respecting. The only difference is
that we rewrite variables by representational equalities when fe_eq_rel
is ReprEq, and that we unwrap newtypes when rewriting w.r.t.
representational equality.

Note [Rewriter CtLoc]
~~~~~~~~~~~~~~~~~~~~~~
The rewriter does eager type-family reduction.
Type families might loop, and we
don't want GHC to do so. A natural solution is to have a bounded depth
to these processes. A central difficulty is that such a solution isn't
quite compositional. For example, say it takes F Int 10 steps to get to Bool.
How many steps does it take to get from F Int -> F Int to Bool -> Bool?
10? 20? What about getting from Const Char (F Int) to Char? 11? 1? Hard to
know and hard to track. So, we punt, essentially. We store a CtLoc in
the RewriteEnv and just update the environment when recurring. In the
TyConApp case, where there may be multiple type families to rewrite,
we just copy the current CtLoc into each branch. If any branch hits the
stack limit, then the whole thing fails.

A consequence of this is that setting the stack limits appropriately
will be essentially impossible. So, the official recommendation if a
stack limit is hit is to disable the check entirely. Otherwise, there
will be baffling, unpredictable errors.

Note [Phantoms in the rewriter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

data Proxy p = Proxy

and we're rewriting (Proxy ty) w.r.t. ReprEq. Then, we know that `ty`
is really irrelevant -- it will be ignored when solving for representational
equality later on. So, we omit rewriting `ty` entirely. This may
violate the expectation of "xi"s for a bit, but the canonicaliser will
soon throw out the phantoms when decomposing a TyConApp. (Or, the
canonicaliser will emit an insoluble, in which case we get
a better error message anyway.)

Note [No derived kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A kind-level coercion can appear in types, via mkCastTy. So, whenever
we are generating a coercion in a dependent context (in other words,
in a kind) we need to make sure that our flavour is never Derived
(as Derived constraints have no evidence). The noBogusCoercions function
changes the flavour from Derived just for this purpose.

-}

{- *********************************************************************
*                                                                      *
*      Externally callable rewriting functions                         *
*                                                                      *
************************************************************************
-}

-- | See Note [Rewriting].
-- If (xi, co) <- rewrite mode ev ty, then co :: xi ~r ty
-- where r is the role in @ev@.
rewrite :: CtEvidence -> TcType
        -> TcS (Xi, TcCoercion)
rewrite ev ty
  = do { traceTcS "rewrite {" (ppr ty)
       ; (ty', co) <- runRewriteCtEv ev (rewrite_one ty)
       ; traceTcS "rewrite }" (ppr ty')
       ; return (ty', co) }

rewrite_shallow :: CtEvidence -> TcType
        -> TcS (Xi, TcCoercion)
rewrite_shallow ev ty
  = do { traceTcS "rewrite_shallow {" (ppr ty)
       ; (ty', mco) <- runRewriteCtEv ev (rewrite_one_shallow ty MRefl)
       ; zty' <- zonkTcType ty' -- TODO: we currently need to zonk for correctness,
       ; zco <- case mco of     -- but this seems pretty terrible for performance!
                  MRefl -> pure $ mkReflCo (ctEvRole ev) zty'
                  MCo co -> zonkCo co
       ; traceTcS "rewrite_shallow }" (ppr zty')
       ; return (zty', zco) }


{-
-- TODO: this isn't actually used anywhere?

-- specialized to rewriting kinds: never Derived, always Nominal
-- See Note [No derived kind equalities]
-- See Note [Rewriting]
rewriteKind :: CtLoc -> CtFlavour -> TcType -> TcS (Xi, TcCoercionN)
rewriteKind loc flav ty
  = do { traceTcS "rewriteKind {" (ppr flav <+> ppr ty)
       ; let flav' = case flav of
                       Derived -> Wanted WDeriv  -- the WDeriv/WOnly choice matters not
                       _       -> flav
       ; (ty', co) <- runRewrite loc flav' NomEq (rewrite_one ty)
       ; traceTcS "rewriteKind }" (ppr ty' $$ ppr co) -- co is never a panic
       ; return (ty', co) }
-}

-- See Note [Rewriting]
rewriteArgsNom :: CtEvidence -> TyCon -> [TcType] -> TcS ([Xi], [TcCoercion])
-- Externally-callable, hence runRewrite
-- Rewrite a vector of types all at once; in fact they are
-- always the arguments of type family or class, so
--      ctEvFlavour ev = Nominal
-- and we want to rewrite all at nominal role
-- The kind passed in is the kind of the type family or class, call it T
-- The kind of T args must be constant (i.e. not depend on the args)
--
-- For Derived constraints the returned coercion may be undefined
-- because rewriting may use a Derived equality ([D] a ~ ty)
rewriteArgsNom ev tc tys
  = do { traceTcS "rewrite_args {" (vcat (map ppr tys))
       ; (tys', cos, kind_co)
           <- runRewriteCtEv ev (rewrite_args_tc tc Nothing tys)
       ; MASSERT( isReflMCo kind_co )
       ; traceTcS "rewrite }" (vcat (map ppr tys'))
       ; return (tys', cos) }

-- | Rewrite a type w.r.t. nominal equality. This is useful to rewrite
-- a type w.r.t. any givens. It does not do type-family reduction. This
-- will never emit new constraints. Call this when the inert set contains
-- only givens.
--
-- TODO: comments here claim it does not do any TF reduction, but that seems
-- wrong? Do we need a shallow variant?
rewriteType :: CtLoc -> TcType -> TcS TcType
rewriteType loc ty
  = do { (xi, _) <- runRewrite loc Given NomEq $
                    rewrite_one ty
                     -- use Given flavor so that it is rewritten
                     -- only w.r.t. Givens, never Wanteds/Deriveds
                     -- (Shouldn't matter, if only Givens are present
                     -- anyway)
       ; return xi }

{- *********************************************************************
*                                                                      *
*           The main rewriting functions
*                                                                      *
********************************************************************* -}

{- Note [Rewriting]
~~~~~~~~~~~~~~~~~~~~
  rewrite ty  ==>   (xi, co)
    where
      xi has no reducible type functions
         has no skolems that are mapped in the inert set
         has no filled-in metavariables
      co :: xi ~ ty

Key invariants:
  (F0) co :: xi ~ zonk(ty')    where zonk(ty') ~ zonk(ty)
  (F1) tcTypeKind(xi) succeeds and returns a fully zonked kind
  (F2) tcTypeKind(xi) `eqType` zonk(tcTypeKind(ty))

Note that it is rewrite's job to try to reduce *every type function it sees*.

Rewriting also:
  * zonks, removing any metavariables, and
  * applies the substitution embodied in the inert set

Because rewriting zonks and the returned coercion ("co" above) is also
zonked, it's possible that (co :: xi ~ ty) isn't quite true. So, instead,
we can rely on this fact:

  (F0) co :: xi ~ zonk(ty'), where zonk(ty') ~ zonk(ty)

Note that the left-hand type of co is *always* precisely xi. The right-hand
type may or may not be ty, however: if ty has unzonked filled-in metavariables,
then the right-hand type of co will be the zonk-equal to ty.
It is for this reason that we
occasionally have to explicitly zonk, when (co :: xi ~ ty) is important
even before we zonk the whole program. For example, see the RTRNotFollowed
case in rewriteTyVar.

Why have these invariants on rewriting? Because we sometimes use tcTypeKind
during canonicalisation, and we want this kind to be zonked (e.g., see
GHC.Tc.Solver.Canonical.canEqCanLHS).

Rewriting is always homogeneous. That is, the kind of the result of rewriting is
always the same as the kind of the input, modulo zonking. More formally:

  (F2) tcTypeKind(xi) `eqType` zonk(tcTypeKind(ty))

This invariant means that the kind of a rewritten type might not itself be rewritten.

Note that we prefer to leave type synonyms unexpanded when possible,
so when the rewriter encounters one, it first asks whether its
transitive expansion contains any type function applications or is
forgetful -- that is, omits one or more type variables in its RHS.  If so,
it expands the synonym and proceeds; if not, it simply returns the
unexpanded synonym. See also Note [Rewriting synonyms].

Where do we actually perform rewriting within a type? See Note [Rewritable] in
GHC.Tc.Solver.Monad.

Note [rewrite_args performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In programs with lots of type-level evaluation, rewrite_args becomes
part of a tight loop. For example, see test perf/compiler/T9872a, which
calls rewrite_args a whopping 7,106,808 times. It is thus important
that rewrite_args be efficient.

Performance testing showed that the current implementation is indeed
efficient. It's critically important that zipWithAndUnzipM be
specialized to TcS, and it's also quite helpful to actually `inline`
it. On test T9872a, here are the allocation stats (Dec 16, 2014):

 * Unspecialized, uninlined:     8,472,613,440 bytes allocated in the heap
 * Specialized, uninlined:       6,639,253,488 bytes allocated in the heap
 * Specialized, inlined:         6,281,539,792 bytes allocated in the heap

To improve performance even further, rewrite_args_nom is split off
from rewrite_args, as nominal equality is the common case. This would
be natural to write using mapAndUnzipM, but even inlined, that function
is not as performant as a hand-written loop.

 * mapAndUnzipM, inlined:        7,463,047,432 bytes allocated in the heap
 * hand-written recursion:       5,848,602,848 bytes allocated in the heap

If you make any change here, pay close attention to the T9872{a,b,c} tests
and T5321Fun.

If we need to make this yet more performant, a possible way forward is to
duplicate the rewriter code for the nominal case, and make that case
faster. This doesn't seem quite worth it, yet.

Note [rewrite_exact_fam_app performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Once we've got a rewritten rhs, we extend the famapp-cache to record
the result. Doing so can save lots of work when the same redex shows up more
than once. Note that we record the link from the redex all the way to its
*final* value, not just the single step reduction.

If we can reduce the family application right away (the first call
to try_to_reduce), we do *not* add to the cache. There are two possibilities
here: 1) we just read the result from the cache, or 2) we used one type
family instance. In either case, recording the result in the cache doesn't
save much effort the next time around. And adding to the cache here is
actually disastrous: it more than doubles the allocations for T9872a. So
we skip adding to the cache here.
-}

{-# INLINE rewrite_args_tc #-}
rewrite_args_tc
  :: TyCon         -- T
  -> Maybe [Role]  -- Nothing: ambient role is Nominal; all args are Nominal
                   -- Otherwise: no assumptions; use roles provided
  -> [Type]        -- Arg types [t1,..,tn]
  -> RewriteM ( [Xi]  -- List of rewritten args [x1,..,xn]
                   -- 1-1 corresp with [t1,..,tn]
           , [Coercion]  -- List of arg coercions [co1,..,con]
                         -- 1-1 corresp with [t1,..,tn]
                         --    coi :: xi ~r ti
           , MCoercionN) -- Result coercion, rco
                         --    rco : (T t1..tn) ~N (T (x1 |> co1) .. (xn |> con))
rewrite_args_tc tc = rewrite_args all_bndrs any_named_bndrs inner_ki emptyVarSet
  -- NB: TyCon kinds are always closed
  where
    (bndrs, named)
      = ty_con_binders_ty_binders' (tyConBinders tc)
    -- it's possible that the result kind has arrows (for, e.g., a type family)
    -- so we must split it
    (inner_bndrs, inner_ki, inner_named) = split_pi_tys' (tyConResKind tc)
    !all_bndrs                           = bndrs `chkAppend` inner_bndrs
    !any_named_bndrs                     = named || inner_named
    -- NB: Those bangs there drop allocations in T9872{a,c,d} by 8%.

{-# INLINE rewrite_args #-}
rewrite_args :: [TyCoBinder] -> Bool -- Binders, and True iff any of them are
                                     -- named.
             -> Kind -> TcTyCoVarSet -- function kind; kind's free vars
             -> Maybe [Role] -> [Type]    -- these are in 1-to-1 correspondence
                                          -- Nothing: use all Nominal
             -> RewriteM ([Xi], [Coercion], MCoercionN)
-- Coercions :: Xi ~ Type, at roles given
-- Third coercion :: tcTypeKind(fun xis) ~N tcTypeKind(fun tys)
-- That is, the third coercion relates the kind of some function (whose kind is
-- passed as the first parameter) instantiated at xis to the kind of that
-- function instantiated at the tys. This is useful in keeping rewriting
-- homoegeneous. The list of roles must be at least as long as the list of
-- types.
rewrite_args orig_binders
             any_named_bndrs
             orig_inner_ki
             orig_fvs
             orig_m_roles
             orig_tys
  = case (orig_m_roles, any_named_bndrs) of
      (Nothing, False) -> rewrite_args_fast orig_tys
      _ -> rewrite_args_slow orig_binders orig_inner_ki orig_fvs orig_roles orig_tys
        where orig_roles = fromMaybe (repeat Nominal) orig_m_roles

{-# INLINE rewrite_args_fast #-}
-- | fast path rewrite_args, in which none of the binders are named and
-- therefore we can avoid tracking a lifting context.
-- There are many bang patterns in here. It's been observed that they
-- greatly improve performance of an optimized build.
-- The T9872 test cases are good witnesses of this fact.
rewrite_args_fast :: [Type]
                  -> RewriteM ([Xi], [Coercion], MCoercionN)
rewrite_args_fast orig_tys
  = fmap finish (iterate orig_tys)
  where

    iterate :: [Type]
            -> RewriteM ([Xi], [Coercion])
    iterate (ty:tys) = do
      (xi, co)   <- rewrite_one ty
      (xis, cos) <- iterate tys
      pure (xi : xis, co : cos)
    iterate [] = pure ([], [])

    {-# INLINE finish #-}
    finish :: ([Xi], [Coercion]) -> ([Xi], [Coercion], MCoercionN)
    finish (xis, cos) = (xis, cos, MRefl)

{-# INLINE rewrite_args_fast_shallow #-}
-- | fast path rewrite_args, in which none of the binders are named and
-- therefore we can avoid tracking a lifting context.
-- There are many bang patterns in here. It's been observed that they
-- greatly improve performance of an optimized build.
-- The T9872 test cases are good witnesses of this fact.
rewrite_args_fast_shallow :: [Type]
                  -> RewriteM ([Xi], [MCoercion], MCoercionN)
rewrite_args_fast_shallow orig_tys
  = fmap finish (iterate orig_tys)
  where

    iterate :: [Type]
            -> RewriteM ([Xi], [MCoercion])
    iterate (ty:tys) = do
      (xi, co)   <- rewrite_one_shallow ty MRefl
      (xis, cos) <- iterate tys
      pure (xi : xis, co : cos)
    iterate [] = pure ([], [])

    {-# INLINE finish #-}
    finish :: ([Xi], [MCoercion]) -> ([Xi], [MCoercion], MCoercionN)
    finish (xis, cos) = (xis, cos, MRefl)

{-# INLINE rewrite_args_slow #-}
-- | Slow path, compared to rewrite_args_fast, because this one must track
-- a lifting context.
rewrite_args_slow :: [TyCoBinder] -> Kind -> TcTyCoVarSet
                  -> [Role] -> [Type]
                  -> RewriteM ([Xi], [Coercion], MCoercionN)
rewrite_args_slow binders inner_ki fvs roles tys
-- Arguments used dependently must be rewritten with proper coercions, but
-- we're not guaranteed to get a proper coercion when rewriting with the
-- "Derived" flavour. So we must call noBogusCoercions when rewriting arguments
-- corresponding to binders that are dependent. However, we might legitimately
-- have *more* arguments than binders, in the case that the inner_ki is a variable
-- that gets instantiated with a Π-type. We conservatively choose not to produce
-- bogus coercions for these, too. Note that this might miss an opportunity for
-- a Derived rewriting a Derived. The solution would be to generate evidence for
-- Deriveds, thus avoiding this whole noBogusCoercions idea. See also
-- Note [No derived kind equalities]
  = do { rewritten_args <- zipWith3M fl (map isNamedBinder binders ++ repeat True)
                                        roles tys
       ; return (simplifyArgsWorker binders inner_ki fvs roles rewritten_args) }
  where
    {-# INLINE fl #-}
    fl :: Bool   -- must we ensure to produce a real coercion here?
                  -- see comment at top of function
       -> Role -> Type -> RewriteM (Xi, Coercion)
    fl True  r ty = noBogusCoercions $ fl1 r ty
    fl False r ty =                    fl1 r ty

    {-# INLINE fl1 #-}
    fl1 :: Role -> Type -> RewriteM (Xi, Coercion)
    fl1 Nominal ty
      = setEqRel NomEq $
        rewrite_one ty

    fl1 Representational ty
      = setEqRel ReprEq $
        rewrite_one ty

    fl1 Phantom ty
    -- See Note [Phantoms in the rewriter]
      = do { ty <- liftTcS $ zonkTcType ty
           ; return (ty, mkReflCo Phantom ty) }

------------------
rewrite_one :: TcType -> RewriteM (Xi, Coercion)
-- Rewrite a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Rewriting] for more detail.
--
-- Postcondition: Coercion :: Xi ~ TcType
-- The role on the result coercion matches the EqRel in the RewriteEnv

rewrite_one ty
--  | pprTrace "rewrite_one" (ppr ty) False = undefined

  | Just ty' <- rewriterView ty  -- See Note [Rewriting synonyms]
  = rewrite_one ty'

rewrite_one xi@(LitTy {})
  = do { role <- getRole
       ; return (xi, mkReflCo role xi) }

rewrite_one (TyVarTy tv)
  = rewriteTyVar tv

rewrite_one (AppTy ty1 ty2)
  = rewrite_app_tys ty1 [ty2]

rewrite_one (TyConApp tc tys)
  -- If it's a type family application, try to reduce it
  | isTypeFamilyTyCon tc
  = rewrite_fam_app tc tys

  -- For * a normal data type application
  --     * data family application
  -- we just recursively rewrite the arguments.
  | otherwise
  = rewrite_ty_con_app tc tys

rewrite_one ty@(FunTy { ft_mult = mult, ft_arg = ty1, ft_res = ty2 })
  = do { (xi1,co1) <- rewrite_one ty1
       ; (xi2,co2) <- rewrite_one ty2
       ; (xi3,co3) <- setEqRel NomEq $ rewrite_one mult
       ; role <- getRole
       ; return (ty { ft_mult = xi3, ft_arg = xi1, ft_res = xi2 }
                , mkFunCo role co3 co1 co2) }

rewrite_one ty@(ForAllTy {})
-- TODO (RAE): This is inadequate, as it doesn't rewrite the kind of
-- the bound tyvar. Doing so will require carrying around a substitution
-- and the usual substTyVarBndr-like silliness. Argh.

-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (bndrs, rho) = tcSplitForAllTyVarBinders ty
             tvs           = binderVars bndrs
       ; (rho', co) <- rewrite_one rho
       ; return (mkForAllTys bndrs rho', mkHomoForAllCos tvs co) }

rewrite_one (CastTy ty g)
  = do { (xi, co) <- rewrite_one ty
       ; (g', _)  <- rewrite_co g
       ; role <- getRole
       ; return (mkCastTy xi g', castCoercionKind1 co role xi ty g') }
         -- It makes a /big/ difference to call castCoercionKind1 not
         -- the more general castCoercionKind2.
         -- See Note [castCoercionKind1] in GHC.Core.Coercion

rewrite_one (CoercionTy co) = first mkCoercionTy <$> rewrite_co co


-- | Given a (ty, co :: ty ~r ty0) pair, produce (ty1, co1 :: ty1 ~r ty0)
-- where r is the ambient role in the monad.
rewrite_one_shallow :: Type -> MCoercion -> RewriteM (Type, MCoercion)
rewrite_one_shallow ty !co
--  | pprTrace "rewrite_one_shallow" (ppr ty $$ ppr co) False = undefined

  | Just ty' <- rewriterView ty  -- See Note [Rewriting synonyms]
  = rewrite_one_shallow ty' co

rewrite_one_shallow (TyVarTy tv) co = rewriteTyVar_shallow tv co
rewrite_one_shallow ty@(TyConApp tc tys) co
  -- If it's a type family application, try to reduce it
  | isTypeFamilyTyCon tc
  = rewrite_fam_app_shallow ty co tc tys

  | otherwise
  = pure (ty, co)

rewrite_one_shallow (CastTy ty g) co
 = do { role <- getRole
      ; (ty', mco') <- rewrite_one_shallow ty MRefl
      ; let mco'' = case mco' of
                      MRefl -> MRefl
                      MCo co' -> MCo (castCoercionKind1 co' role ty' ty g)
      ; return (mkCastTy ty' g, mco'' `transMCo` co)
        -- TODO: is this right? Should we  rewrite_co g?
      }

-- One of AppTy, LitTy, FunTy or ForAllTy, all of which are already in WHNF
rewrite_one_shallow ty co = ASSERT(isWHNF ty) pure (ty, co)



-- | "Rewrite" a coercion. Really, just zonk it so we can uphold
-- (F1) of Note [Rewriting]
rewrite_co :: Coercion -> RewriteM (Coercion, Coercion)
rewrite_co co
  = do { co <- liftTcS $ zonkCo co
       ; env_role <- getRole
       ; let co' = mkTcReflCo env_role (mkCoercionTy co)
       ; return (co, co') }

-- rewrite (nested) AppTys
rewrite_app_tys :: Type -> [Type] -> RewriteM (Xi, Coercion)
-- commoning up nested applications allows us to look up the function's kind
-- only once. Without commoning up like this, we would spend a quadratic amount
-- of time looking up functions' types
rewrite_app_tys (AppTy ty1 ty2) tys = rewrite_app_tys ty1 (ty2:tys)
rewrite_app_tys fun_ty arg_tys
  = do { (fun_xi, fun_co) <- rewrite_one fun_ty
       ; rewrite_app_ty_args fun_xi fun_co arg_tys }

-- Given a rewritten function (with the coercion produced by rewriting) and
-- a bunch of unrewritten arguments, rewrite the arguments and apply.
-- The coercion argument's role matches the role stored in the RewriteM monad.
--
-- The bang patterns used here were observed to improve performance. If you
-- wish to remove them, be sure to check for regeressions in allocations.
rewrite_app_ty_args :: Xi -> Coercion -> [Type] -> RewriteM (Xi, Coercion)
rewrite_app_ty_args fun_xi fun_co []
  -- this will be a common case when called from rewrite_fam_app, so shortcut
  = return (fun_xi, fun_co)
rewrite_app_ty_args fun_xi fun_co arg_tys
  = do { (xi, co, kind_co) <- case tcSplitTyConApp_maybe fun_xi of
           Just (tc, xis) ->
             do { let tc_roles  = tyConRolesRepresentational tc
                      arg_roles = dropList xis tc_roles
                ; (arg_xis, arg_cos, kind_co)
                    <- rewrite_vector (tcTypeKind fun_xi) arg_roles arg_tys

                  -- Here, we have fun_co :: T xi1 xi2 ~ ty
                  -- and we need to apply fun_co to the arg_cos. The problem is
                  -- that using mkAppCo is wrong because that function expects
                  -- its second coercion to be Nominal, and the arg_cos might
                  -- not be. The solution is to use transitivity:
                  -- T <xi1> <xi2> arg_cos ;; fun_co <arg_tys>
                ; eq_rel <- getEqRel
                ; let app_xi = mkTyConApp tc (xis ++ arg_xis)
                      app_co = case eq_rel of
                        NomEq  -> mkAppCos fun_co arg_cos
                        ReprEq -> mkTcTyConAppCo Representational tc
                                    (zipWith mkReflCo tc_roles xis ++ arg_cos)
                                  `mkTcTransCo`
                                  mkAppCos fun_co (map mkNomReflCo arg_tys)
                ; return (app_xi, app_co, kind_co) }
           Nothing ->
             do { (arg_xis, arg_cos, kind_co)
                    <- rewrite_vector (tcTypeKind fun_xi) (repeat Nominal) arg_tys
                ; let arg_xi = mkAppTys fun_xi arg_xis
                      arg_co = mkAppCos fun_co arg_cos
                ; return (arg_xi, arg_co, kind_co) }

       ; role <- getRole
       ; return (homogenise_result xi co role kind_co) }

rewrite_ty_con_app :: TyCon -> [TcType] -> RewriteM (Xi, Coercion)
rewrite_ty_con_app tc tys
  = do { role <- getRole
       ; let m_roles | Nominal <- role = Nothing
                     | otherwise       = Just $ tyConRolesX role tc
       ; (xis, cos, kind_co) <- rewrite_args_tc tc m_roles tys
       ; let tyconapp_xi = mkTyConApp tc xis
             tyconapp_co = mkTyConAppCo role tc cos
       ; return (homogenise_result tyconapp_xi tyconapp_co role kind_co) }

-- Make the result of rewriting homogeneous (Note [Rewriting] (F2))
homogenise_result :: Xi              -- a rewritten type
                  -> Coercion        -- :: xi ~r original ty
                  -> Role            -- r
                  -> MCoercionN      -- kind_co :: tcTypeKind(xi) ~N tcTypeKind(ty)
                  -> (Xi, Coercion)  -- (xi |> kind_co, (xi |> kind_co)
                                     --   ~r original ty)
homogenise_result xi co _ MRefl = (xi, co)
homogenise_result xi co r mco@(MCo kind_co)
  = (xi `mkCastTy` kind_co, (mkSymCo $ GRefl r xi mco) `mkTransCo` co)
{-# INLINE homogenise_result #-}

-- Rewrite a vector (list of arguments).
rewrite_vector :: Kind   -- of the function being applied to these arguments
               -> [Role] -- If we're rewrite w.r.t. ReprEq, what roles do the
                         -- args have?
               -> [Type] -- the args to rewrite
               -> RewriteM ([Xi], [Coercion], MCoercionN)
rewrite_vector ki roles tys
  = do { eq_rel <- getEqRel
       ; case eq_rel of
           NomEq  -> rewrite_args bndrs
                                  any_named_bndrs
                                  inner_ki
                                  fvs
                                  Nothing
                                  tys
           ReprEq -> rewrite_args bndrs
                                  any_named_bndrs
                                  inner_ki
                                  fvs
                                  (Just roles)
                                  tys
       }
  where
    (bndrs, inner_ki, any_named_bndrs) = split_pi_tys' ki
    fvs                                = tyCoVarsOfType ki
{-# INLINE rewrite_vector #-}

{-
Note [Rewriting synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Not expanding synonyms aggressively improves error messages, and
keeps types smaller. But we need to take care.

Suppose
   type Syn a = Int
   type instance F Bool = Syn (F Bool)
   [G] F Bool ~ Syn (F Bool)

If we don't expand the synonym, we'll get a spurious occurs-check
failure. This is normally what occCheckExpand takes care of, but
the LHS is a type family application, and occCheckExpand (already
complex enough as it is) does not know how to expand to avoid
a type family application.

In addition, expanding the forgetful synonym like this
will generally yield a *smaller* type. To wit, if we spot
S ( ... F tys ... ), where S is forgetful, we don't want to bother
doing hard work simplifying (F tys). We thus expand forgetful
synonyms, but not others.

isForgetfulSynTyCon returns True more often than it needs to, so
we err on the side of more expansion.

We also, of course, must expand type synonyms that mention type families,
so those families can get reduced.

************************************************************************
*                                                                      *
             Rewriting a type-family application
*                                                                      *
************************************************************************

Note [How to normalise a family application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given an exactly saturated family application, how should we normalise it?
This Note spells out the algorithm and its reasoning.

STEP 1. Try the famapp-cache. If we get a cache hit, jump to FINISH.

STEP 2. Try top-level instances. Note that we haven't simplified the arguments
  yet. Example:
    type instance F (Maybe a) = Int
    target: F (Maybe (G Bool))
  Instead of first trying to simplify (G Bool), we use the instance first. This
  avoids the work of simplifying G Bool.

  If an instance is found, jump to FINISH.

STEP 3. Rewrite all arguments. This might expose more information so that we
  can use a top-level instance.

  Continue to the next step.

STEP 4. Try the inerts. Note that we try the inerts *after* rewriting the
  arguments, because the inerts will have rewritten LHSs.

  If an inert is found, jump to FINISH.

STEP 5. Try the famapp-cache again. Now that we've revealed more information
  in the arguments, the cache might be helpful.

  If we get a cache hit, jump to FINISH.

STEP 6. Try top-level instances, which might trigger now that we know more
  about the argumnents.

  If an instance is found, jump to FINISH.

STEP 7. No progress to be made. Return what we have. (Do not do FINISH.)

FINISH 1. We've made a reduction, but the new type may still have more
  work to do. So rewrite the new type.

FINISH 2. Add the result to the famapp-cache, connecting the type we started
  with to the one we ended with.

Because STEP 1/2 and STEP 5/6 happen the same way, they are abstracted into
try_to_reduce.

FINISH is naturally implemented in `finish`. But, Note [rewrite_exact_fam_app performance]
tells us that we should not add to the famapp-cache after STEP 1/2. So `finish`
is inlined in that case, and only FINISH 1 is performed.

-}


rewrite_fam_app :: TyCon -> [TcType] -> RewriteM (Xi, Coercion)
  --   rewrite_fam_app            can be over-saturated
  --   rewrite_exact_fam_app      lifts out the application to top level
  -- Postcondition: Coercion :: Xi ~ F tys
rewrite_fam_app tc tys  -- Can be over-saturated
    = ASSERT2( tys `lengthAtLeast` tyConArity tc
             , ppr tc $$ ppr (tyConArity tc) $$ ppr tys)

                 -- Type functions are saturated
                 -- The type function might be *over* saturated
                 -- in which case the remaining arguments should
                 -- be dealt with by AppTys
      do { let (tys1, tys_rest) = splitAt (tyConArity tc) tys
         ; (xi1, co1) <- rewrite_exact_fam_app tc tys1
               -- co1 :: xi1 ~ F tys1

         ; rewrite_app_ty_args xi1 co1 tys_rest }

-- the [TcType] exactly saturate the TyCon
-- See Note [How to normalise a family application]
rewrite_exact_fam_app_old :: TyCon -> [TcType] -> RewriteM (Xi, Coercion)
rewrite_exact_fam_app_old tc tys
  = do { checkStackDepth (mkTyConApp tc tys)

       -- STEP 1/2. Try to reduce without reducing arguments first.
       ; result1 <- try_to_reduce tc tys
       ; case result1 of
             -- Don't use the cache;
             -- See Note [rewrite_exact_fam_app performance]
         { Just (co, xi) -> finish False (xi, co)
         ; Nothing ->

        -- That didn't work. So reduce the arguments, in STEP 3.
    do { eq_rel <- getEqRel
           -- checking eq_rel == NomEq saves ~0.5% in T9872a
       ; (xis, cos, kind_co) <- if eq_rel == NomEq
                                then rewrite_args_tc tc Nothing tys
                                else setEqRel NomEq $
                                     rewrite_args_tc tc Nothing tys
           -- kind_co :: tcTypeKind(F xis) ~N tcTypeKind(F tys)

       ; let role    = eqRelRole eq_rel
             args_co = mkTyConAppCo role tc cos
           -- args_co :: F xis ~r F tys

             homogenise :: TcType -> TcCoercion -> (TcType, TcCoercion)
               -- in (xi', co') = homogenise xi co
               --   assume co :: xi ~r F xis, co is homogeneous
               --   then xi' :: tcTypeKind(F tys)
               --   and co' :: xi' ~r F tys, which is homogeneous
             homogenise xi co = homogenise_result xi (co `mkTcTransCo` args_co) role kind_co

         -- STEP 4: try the inerts
       ; result2 <- liftTcS $ lookupFamAppInert_hacked tc xis
       ; flavour <- getFlavour
       ; case result2 of
         { Just (co, xi, fr@(_, inert_eq_rel))
             -- co :: F xis ~ir xi

             | fr `eqCanRewriteFR` (flavour, eq_rel) ->
                 do { traceRewriteM "rewrite family application with inert"
                                (ppr tc <+> ppr xis $$ ppr xi)
                    ; finish True (homogenise xi downgraded_co) }
               -- this will sometimes duplicate an inert in the cache,
               -- but avoiding doing so had no impact on performance, and
               -- it seems easier not to weed out that special case
             where
               inert_role    = eqRelRole inert_eq_rel
               role          = eqRelRole eq_rel
               downgraded_co = tcDowngradeRole role inert_role (mkTcSymCo co)
                 -- downgraded_co :: xi ~r F xis

         ; _ ->

         -- inert didn't work. Try to reduce again, in STEP 5/6.
    do { result3 <- try_to_reduce tc xis
       ; case result3 of
           Just (co, xi) -> finish True (homogenise xi co)
           Nothing       -> -- we have made no progress at all: STEP 7.
                            return (homogenise reduced (mkTcReflCo role reduced))
             where
               reduced = mkTyConApp tc xis }}}}}
  where
      -- call this if the above attempts made progress.
      -- This recursively rewrites the result and then adds to the cache
    finish :: Bool  -- add to the cache?
           -> (Xi, Coercion) -> RewriteM (Xi, Coercion)
    finish use_cache (xi, co)
      = do { -- rewrite the result: FINISH 1
             (fully, fully_co) <- bumpDepth $ rewrite_one xi
           ; let final_co = fully_co `mkTcTransCo` co
           ; eq_rel <- getEqRel
           ; flavour <- getFlavour

             -- extend the cache: FINISH 2
           ; when (use_cache && eq_rel == NomEq && flavour /= Derived) $
             -- the cache only wants Nominal eqs
             -- and Wanteds can rewrite Deriveds; the cache
             -- has only Givens
             liftTcS $ extendFamAppCache tc tys (final_co, fully)
           ; return (fully, final_co) }
    {-# INLINE finish #-}

-- Returned coercion is output ~r input, where r is the role in the RewriteM monad
-- See Note [How to normalise a family application]
try_to_reduce :: TyCon -> [TcType] -> RewriteM (Maybe (TcCoercion, TcType))
try_to_reduce tc tys
  = do { result <- liftTcS $ firstJustsM [ lookupFamAppCache tc tys  -- STEP 5
                                         , matchFam tc tys ]         -- STEP 6
       ; downgrade result }
  where
    -- The result above is always Nominal. We might want a Representational
    -- coercion; this downgrades (and prints, out of convenience).
    downgrade :: Maybe (TcCoercionN, TcType) -> RewriteM (Maybe (TcCoercion, TcType))
    downgrade Nothing = return Nothing
    downgrade result@(Just (co, xi))
      = do { traceRewriteM "Eager T.F. reduction success" $
             vcat [ ppr tc, ppr tys, ppr xi
                  , ppr co <+> dcolon <+> ppr (coercionKind co)
                  ]
           ; eq_rel <- getEqRel
              -- manually doing it this way avoids allocation in the vastly
              -- common NomEq case
           ; case eq_rel of
               NomEq  -> return result
               ReprEq -> return (Just (mkSubCo co, xi)) }


-- the [TcType] exactly saturate the TyCon
-- See Note [How to normalise a family application]
--
-- Careful here: we call rewrite_exact_fam_app_shallow, which guarantees to
-- expose a head constructor if possible, but doesn't recursively rewrite the
-- arguments.  If we want a xi we have to rewrite again, but we mustn't call
-- rewrite_one/rewrite_fam_app if we had a stuck type family.
rewrite_exact_fam_app :: TyCon -> [TcType] -> RewriteM (Xi, Coercion)
rewrite_exact_fam_app tc tys
  | can't_cope_with tc = rewrite_exact_fam_app_old tc tys
  | otherwise
  = do { let ty = mkTyConApp tc tys
       ; (ty, mco) <- rewrite_exact_fam_app_shallow ty MRefl tc tys
       ; let co = to_co (ty, mco)
       ; case tcSplitTyConApp_maybe ty of
           Just (new_tc, new_tys)
             | isTypeFamilyTyCon new_tc
                         -> do { (xi, co') <- rewrite_ty_con_app new_tc new_tys
                               ; pure (xi, co' `transCo` co) }
           _             -> do { (xi, co') <- rewrite_one ty
                               ; pure (xi, co' `transCo` co) }
       }

downgrade :: MCoercion -> RewriteM MCoercion
downgrade MRefl = pure MRefl
downgrade (MCo co) = do {
           ; eq_rel <- getEqRel
           ; pure $ case eq_rel of
               NomEq  -> MCo co
               ReprEq -> MCo (mkSubCo co) }

-- TODO: we don't yet support dependently-kinded TyCons so for now just fall
-- back on the old code path
can't_cope_with :: TyCon -> Bool
can't_cope_with tc = snd (ty_con_binders_ty_binders' (tyConBinders tc))

rewrite_fam_app_shallow :: Type -> MCoercion -> TyCon -> [TcType] -> RewriteM (Xi, MCoercion)
  --   rewrite_fam_app_shallow    can be over-saturated
  --   rewrite_exact_fam_app_shallow lifts out the application to top level
  -- Postcondition: Coercion :: result_ty ~ F tys
rewrite_fam_app_shallow ty0 co tc tys  -- Can be over-saturated
    | can't_cope_with tc = do (xi, co') <- rewrite_fam_app tc tys
                              pure (xi, if isReflCo co' && (xi `eqType` ty0) then co else MCo co' `transMCo` co)

    | ASSERT2( tys `lengthAtLeast` arity
             , ppr tc $$ ppr arity $$ ppr tys)

                 -- Type functions are saturated
                 -- The type function might be *over* saturated
                 -- in which case the remaining arguments should
                 -- be dealt with by AppTys

      -- TODO: is this shortcut case worthwhile?
      tys `lengthIs` arity
       = rewrite_exact_fam_app_shallow ty0 co tc tys

    | otherwise =
      do { let (tys1, tys_rest) = splitAt arity tys
         ; let exact_ty = mkTyConApp tc tys1
         ; (xi1, mco1) <- rewrite_exact_fam_app_shallow exact_ty MRefl tc tys1
               -- co1 :: xi1 ~ F tys1

         ; let mco = case mco1 of
                       MRefl -> mco1
                       MCo co1 -> MCo (co1 `mkAppCos` map mkNomReflCo tys_rest)

           -- TODO: can we avoid the call stack here and make this tail-recursive?
           -- TODO: perhaps just allow rewrite_exact_fam_app_shallow to be called with extra arguments?
         ; pure (xi1 `mkAppTys` tys_rest, mco) }
  where
    arity = tyConArity tc


{-
************************************************************************
*                                                                      *
             Rewriting a type variable
*                                                                      *
********************************************************************* -}

-- | The result of rewriting a tyvar "one step".
data RewriteTvResult
  = RTRNotFollowed
      -- ^ The inert set doesn't make the tyvar equal to anything else

  | RTRFollowed TcType Coercion
      -- ^ The tyvar rewrites to a not-necessarily rewritten other type.
      -- co :: new type ~r old type, where the role is determined by
      -- the RewriteEnv
      --
      -- With Quick Look, the returned TcType can be a polytype;
      -- that is, in the constraint solver, a unification variable
      -- can contain a polytype.  See GHC.Tc.Gen.App
      -- Note [Instantiation variables are short lived]

rewriteTyVar_shallow :: TyVar -> MCoercion -> RewriteM (Type, MCoercion)
rewriteTyVar_shallow tv mco
 = do { mb_yes <- rewrite_tyvar1 tv
      ; case mb_yes of
          RTRFollowed ty1 co1 -> do flavour <- getFlavour
                                    let co = MCo co1 `transMCo` mco
                                    rewrite_one_shallow ty1 co
          RTRNotFollowed      -> pure (mkTyVarTy tv, mco) -- TODO: do we need the zonking stuff?
      }

rewriteTyVar :: TyVar -> RewriteM (Xi, Coercion)
rewriteTyVar tv
  = do { mb_yes <- rewrite_tyvar1 tv
       ; case mb_yes of
           RTRFollowed ty1 co1  -- Recur
             -> do { (ty2, co2) <- rewrite_one ty1
                   -- ; traceRewriteM "rewriteTyVar2" (ppr tv $$ ppr ty2)
                   ; return (ty2, co2 `mkTransCo` co1) }

           RTRNotFollowed   -- Done, but make sure the kind is zonked
                            -- Note [Rewriting] invariant (F0) and (F1)
             -> do { tv' <- liftTcS $ updateTyVarKindM zonkTcType tv
                   ; role <- getRole
                   ; let ty' = mkTyVarTy tv'
                   ; return (ty', mkTcReflCo role ty') } }

rewrite_tyvar1 :: TcTyVar -> RewriteM RewriteTvResult
-- "Rewriting" a type variable means to apply the substitution to it
-- Specifically, look up the tyvar in
--   * the internal MetaTyVar box
--   * the inerts
-- See also the documentation for RewriteTvResult

rewrite_tyvar1 tv
  = do { mb_ty <- liftTcS $ isFilledMetaTyVar_maybe tv
       ; case mb_ty of
           Just ty -> do { traceRewriteM "Following filled tyvar"
                             (ppr tv <+> equals <+> ppr ty)
                         ; role <- getRole
                         ; return (RTRFollowed ty (mkReflCo role ty)) } ;
           Nothing -> do { traceRewriteM "Unfilled tyvar" (pprTyVar tv)
                         ; fr <- getFlavourRole
                         ; rewrite_tyvar2 tv fr } }

rewrite_tyvar2 :: TcTyVar -> CtFlavourRole -> RewriteM RewriteTvResult
-- The tyvar is not a filled-in meta-tyvar
-- Try in the inert equalities
-- See Definition [Applying a generalised substitution] in GHC.Tc.Solver.Monad
-- See Note [Stability of rewriting] in GHC.Tc.Solver.Monad

rewrite_tyvar2 tv fr@(_, eq_rel)
  = do { ieqs <- liftTcS $ getInertEqs
       ; case lookupDVarEnv ieqs tv of
           Just (EqualCtList (ct :| _))   -- If the first doesn't work,
                                          -- the subsequent ones won't either
             | CEqCan { cc_ev = ctev, cc_lhs = TyVarLHS tv
                      , cc_rhs = rhs_ty, cc_eq_rel = ct_eq_rel } <- ct
             , let ct_fr = (ctEvFlavour ctev, ct_eq_rel)
             , ct_fr `eqCanRewriteFR` fr  -- This is THE key call of eqCanRewriteFR
             -> do { traceRewriteM "Following inert tyvar"
                        (ppr tv <+>
                         equals <+>
                         ppr rhs_ty $$ ppr ctev)
                      -- TODO: hack for the ctEvCoercion issue
                    ; let rewrite_co1 | isDerived ctev  = Refl (LitTy (NumTyLit 42))
                                      | otherwise       = mkSymCo (ctEvCoercion ctev)
                          rewrite_co  = case (ct_eq_rel, eq_rel) of
                            (ReprEq, _rel)  -> ASSERT( _rel == ReprEq )
                                    -- if this ASSERT fails, then
                                    -- eqCanRewriteFR answered incorrectly
                                               rewrite_co1
                            (NomEq, NomEq)  -> rewrite_co1
                            (NomEq, ReprEq) -> mkSubCo rewrite_co1

                    ; return (RTRFollowed rhs_ty rewrite_co) }
                    -- NB: ct is Derived then fmode must be also, hence
                    -- we are not going to touch the returned coercion
                    -- so ctEvCoercion is fine.

           _other -> return RTRNotFollowed }

{-
Note [An alternative story for the inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(This entire note is just background, left here in case we ever want
 to return the previous state of affairs)

We used (GHC 7.8) to have this story for the inert substitution inert_eqs

 * 'a' is not in fvs(ty)
 * They are *inert* in the weaker sense that there is no infinite chain of
   (i1 `eqCanRewrite` i2), (i2 `eqCanRewrite` i3), etc

This means that rewriting must be recursive, but it does allow
  [G] a ~ [b]
  [G] b ~ Maybe c

This avoids "saturating" the Givens, which can save a modest amount of work.
It is easy to implement, in GHC.Tc.Solver.Interact.kick_out, by only kicking out an inert
only if (a) the work item can rewrite the inert AND
        (b) the inert cannot rewrite the work item

This is significantly harder to think about. It can save a LOT of work
in occurs-check cases, but we don't care about them much.  #5837
is an example, but it causes trouble only with the old (pre-Fall 2020)
rewriting story. It is unclear if there is any gain w.r.t. to
the new story.

-}

--------------------------------------
-- Utilities

-- | Like 'splitPiTys'' but comes with a 'Bool' which is 'True' iff there is at
-- least one named binder.
split_pi_tys' :: Type -> ([TyCoBinder], Type, Bool)
split_pi_tys' ty = split ty ty
  where
     -- put common cases first
  split _       (ForAllTy b res) = let -- This bang is necessary lest we see rather
                                       -- terrible reboxing, as noted in #19102.
                                       !(bs, ty, _) = split res res
                                   in  (Named b : bs, ty, True)
  split _       (FunTy { ft_af = af, ft_mult = w, ft_arg = arg, ft_res = res })
                                 = let -- See #19102
                                       !(bs, ty, named) = split res res
                                   in  (Anon af (mkScaled w arg) : bs, ty, named)

  split orig_ty ty | Just ty' <- coreView ty = split orig_ty ty'
  split orig_ty _                = ([], orig_ty, False)
{-# INLINE split_pi_tys' #-}

-- | Like 'tyConBindersTyCoBinders' but you also get a 'Bool' which is true iff
-- there is at least one named binder.
ty_con_binders_ty_binders' :: [TyConBinder] -> ([TyCoBinder], Bool)
ty_con_binders_ty_binders' = foldr go ([], False)
  where
    go (Bndr tv (NamedTCB vis)) (bndrs, _)
      = (Named (Bndr tv vis) : bndrs, True)
    go (Bndr tv (AnonTCB af))   (bndrs, n)
      = (Anon af (tymult (tyVarKind tv)) : bndrs, n)
    {-# INLINE go #-}
{-# INLINE ty_con_binders_ty_binders' #-}








{-

The plan:
 - rewrite_exact_fam_app_shallow tries to rewrite an (exactly saturated) type family application `F args` to WHNF (if possible)
 - if it makes some progress (even if not all the way to WHNF) it returns `(new_ty, co :: new_ty ~ F args)`
 - if the result ty is a family application, it must be stuck (i.e. we can't make more progress by rewriting it)

We take a (ty, co :: ty ~ ty0) as input and produce (ty1, co2 :: ty1 ~ ty0) as output
so that we can be called tail-recursively when we get a long sequence of top-level reduction steps.

We use an MCoercion so it is possible to tell whether we made progress e.g. by
following a filled metavariable, which results in a MCo Refl.  TODO: maybe we
should return a (Coercion, Bool) pair instead?

-}

rewrite_exact_fam_app_shallow :: Type -> MCoercion -> TyCon -> [Type] -> RewriteM (Type, MCoercion)
rewrite_exact_fam_app_shallow ty0 co tc tys
  = do { checkStackDepth ty0 -- TODO: need to think about stack depth checks in the new world
       -- TODO: setEqRel here is wrong: we might have [G] F waffle ~R Int, [W] F waffle ~R Int.
       -- Instead we need to make the called functions handle roles properly.
       ; (ty, co') <- setEqRel NomEq $ bumpDepth $ do_it
       ; co'' <- downgrade co'
       ; return (ty, co'')
       }
  where
    do_it = case famTyConFlav_maybe tc of
              Just OpenSynFamilyTyCon                  -> rewrite_exact_fam_app_open    ty0 co tc tys
              Just (ClosedSynFamilyTyCon (Just axiom)) -> rewrite_exact_fam_app_closed  ty0 co tc tys axiom
              Just (BuiltInSynFamTyCon builtin_family) -> rewrite_exact_fam_app_builtin ty0 co tc tys builtin_family
              Just (ClosedSynFamilyTyCon Nothing)      -> rewrite_exact_fam_app_inerts  ty0 co tc tys
              Just AbstractClosedSynFamilyTyCon        -> rewrite_exact_fam_app_inerts  ty0 co tc tys
              Just DataFamilyTyCon{}                   -> pprPanic "rewrite_exact_fam_app_shallow: data family" (ppr tc)
              Nothing                                  -> pprPanic "rewrite_exact_fam_app_shallow: not  family" (ppr tc)


-- If we can make no progress using the type family definition, we fall back on
-- trying the inert set in case we have a Given like `F blah ~ t`.
-- See Note [Shallow rewriting: using the inerts]
rewrite_exact_fam_app_inerts :: Type -> MCoercion -> TyCon -> [Type] -> RewriteM (Type, MCoercion)
rewrite_exact_fam_app_inerts ty0 co0 tc tys =
    do { traceRewriteM "rewrite_exact_fam_app_inerts" (ppr (mkTyConApp tc tys))
       ; (xis, cos, mco) <- rewrite_args_fast tys -- TODO: do we really want to be deep here?
       ; result2 <- liftTcS $ lookupFamAppInert_hacked tc xis
       ; flavour <- getFlavour
       ; eq_rel <- getEqRel

       ; case result2 of
         { Just (co, xi, fr@(_, inert_eq_rel))
             -- co :: F xis ~ir xi

             | fr `eqCanRewriteFR` (flavour, eq_rel) ->
                 do { traceRewriteM "rewrite family application with inert"
                                (ppr tc <+> ppr xis $$ ppr xi)
                    ; rewrite_one_shallow xi (MCo (mkTcSymCo co `transCo` mkTyConAppCo Nominal tc cos) `transMCo` co0) }
             | otherwise ->  do { traceRewriteM "cannot rewrite family application with inert"
                                        (ppr (mkTyConApp tc xis) $$ ppr xi $$ ppr fr $$ ppr (flavour, eq_rel))
                                ; pure (ty0, co0) }
             where
               -- TODO: need to pay attention to roles here!
               inert_role    = eqRelRole inert_eq_rel
               role          = eqRelRole eq_rel
               downgraded_co = tcDowngradeRole role inert_role (mkTcSymCo co)
                 -- downgraded_co :: xi ~r F xis
         ;  Nothing -> do { traceRewriteM "no matching inerts" (ppr (mkTyConApp tc xis))
                          ; pure (ty0, co0) }
                               -- TODO: throwing away information from rewriting tys to get xis
         }

        }


-- TODO: not clear exactly what to do here. This currently tries to reduce the
-- built-in family once, but if that fails, it reduces all the arguments to WHNF
-- before trying again.  Is that enough? Do we have any lazy built-in families,
-- or any that require their arguments to be evaluated beyond WHNF?
rewrite_exact_fam_app_builtin :: Type -> MCoercion -> TyCon -> [Type] -> BuiltInSynFamily
                              -> RewriteM (Type, MCoercion)
rewrite_exact_fam_app_builtin ty0 co0 tc tys builtin_family = case sfMatchFam builtin_family tys of
    Just r -> hit r ty0 co0
    Nothing -> do (xis, mcos, kind_co) <- rewrite_args_fast_shallow tys -- TODO: dependency not handled properly here
                  ; let cos = map to_co (zip xis mcos)
                  -- TODO: determine whether we actually made progress...
                  let tyconapp_xi = mkTyConApp tc xis
                      tyconapp_co = MCo (mkTyConAppCo role tc cos) `transMCo` co0
                      role = Nominal -- TODO
                      --(ty,co) = homogenise_result tyconapp_xi tyconapp_co role kind_co
                      ty = tyconapp_xi
                      co = tyconapp_co
                  case sfMatchFam builtin_family xis of
                    Just r  -> hit r tyconapp_xi tyconapp_co
                    Nothing -> rewrite_exact_fam_app_inerts ty co tc xis

  where
    hit (coax,ts,new_ty) old_ty tyconapp_co = do
        -- TODO: settle whether to use StepsProv
        let !co = mkTcSymCo (mkAxiomRuleCo coax (zipWith mkReflCo (coaxrAsmpRoles coax) ts))
        -- let !co = UnivCo (StepsProv 0 1) Nominal new_ty $! old_ty
        let !co' = MCo co `transMCo` tyconapp_co
        rewrite_one_shallow new_ty co'


rewrite_exact_fam_app_open :: Type -> MCoercion -> TyCon -> [Type] -> RewriteM (Type, MCoercion)
rewrite_exact_fam_app_open ty0 co0 tc tys0
  = do { fam_insts <- liftTcS getFamInstEnvs
       ; let fis = lookupFamInstEnvByTyCon fam_insts tc
       ; go fis (strictMap to_both tys0) False
       }
  where
    -- TODO: would be nice to use a trie here rather than a list? #19703
    go :: [FamInst] -> [(Type, MCoercion)] -> Bool -> RewriteM (Type, MCoercion)
    go []       tycos progress
      | progress  = let tys = strictMap to_type tycos
                        cos = strictMap to_co tycos
                    in rewrite_exact_fam_app_inerts (mkTyConApp tc tys) (MCo (mkTyConAppCo Nominal tc cos) `transMCo` co0) tc tys
      | otherwise = rewrite_exact_fam_app_inerts ty0 co0 tc tys0
    go (fi:fis) tycos progress = do
      let tys = strictMap to_type tycos
      -- TODO: avoid re-calculating this InScopeSet
      let in_scope = mkInScopeSet (unionVarSets [mkVarSet (fi_tvs fi), mkVarSet (fi_cvs fi), tyCoVarsOfTypes tys])
      let subst0 = mkEmptyTCvSubst in_scope
      (mb, res) <- match_list subst0 (fi_tys fi) tys
      let (tycos', progress') = maybe (tycos, progress) (\more -> (combine tycos more, True)) mb
      case res of
        MatchingSuccess subst -> hit fi subst tycos' progress'
        MatchingApart         -> go fis tycos' progress'
        MatchingStuck _       -> go fis tycos' progress'

    hit fi subst tycos' progress' = do
          let tys = strictMap to_type tycos'
          let cos = strictMap to_co tycos'
          let new_ty = substTy subst (fi_rhs fi)
          -- TODO: looks like we can end up retaining lots of thunks here...
          -- but may not want to force all the work if we're just going to throw away the coercion?
          let co = mkSymCo $ mkUnbranchedAxInstCo Nominal (fi_axiom fi) (substTyVars subst (fi_tvs fi)) (substCoVars subst (fi_cvs fi))
          -- TODO: using a StepsProv avoids the thunk leak, but (a) isn't linted properly
          -- and (b) seemingly leads to more Core Lint errors?
          -- let co = UnivCo (StepsProv 0 1) Nominal new_ty $! mkTyConApp tc tys
          let !co' | progress' = MCo (co `transCo` mkTyConAppCo Nominal tc cos) `transMCo` co0
                   | otherwise = MCo co `transMCo` co0
          rewrite_one_shallow new_ty co'


rewrite_exact_fam_app_closed :: Type -> MCoercion -> TyCon -> [Type] -> CoAxiom Branched
                             -> RewriteM (Type, MCoercion)
rewrite_exact_fam_app_closed ty0 co0 tc tys axiom
  = go (assocs (unMkBranches (coAxiomBranches axiom))) (strictMap to_both tys) [] False
  where
    go :: [(BranchIndex, CoAxBranch)] -> [(Type, MCoercion)] -> [CoAxBranch] -> Bool -> RewriteM (Type, MCoercion)
    go [] tycos stuck_branches progress
        | progress  = let tys = strictMap to_type tycos
                          cos = strictMap to_co tycos
                      in rewrite_exact_fam_app_inerts (mkTyConApp tc tys) (MCo (mkTyConAppCo Nominal tc cos) `transMCo` co0) tc tys
        | otherwise = rewrite_exact_fam_app_inerts  ty0 co0 tc tys
    go ((index, branch) : branches) tycos stuck_branches progress = do
        let tys = strictMap to_type tycos
        let in_scope = mkInScopeSet (unionVarSets [mkVarSet (cab_tvs branch), mkVarSet (cab_cvs branch), tyCoVarsOfTypes tys])
        let subst0 = mkEmptyTCvSubst in_scope
        (mb, res) <- match_list subst0 (cab_lhs branch) tys
        let (tycos',progress') = maybe (tycos,progress) (\more -> (combine tycos more, True)) mb
        let in_scope = mkInScopeSet (unionVarSets $ map (tyCoVarsOfTypes . coAxBranchLHS) (cab_incomps branch))
        let flattened_target = Unify.flattenTys in_scope (map fst tycos')
        case res of
            MatchingSuccess subst
              -- TODO: this is a really ugly way to test there is no preceding incompatible branch that is stuck!
              -- TODO: it is also not good enough, because of Note [Shallow rewriting: getting unstuck]
              -- TODO: for now we are doing the core-flattened apartness check, which is horrible but sound.
              | {-all (`notElem` map cab_loc (cab_incomps branch)) (map cab_loc stuck_branches)-}
                apartnessCheck flattened_target branch -> hit index branch subst tycos' progress'
              | otherwise   -> go branches tycos' (branch:stuck_branches) progress'
            MatchingApart   -> go branches tycos' stuck_branches progress'
            MatchingStuck _ -> go branches tycos' (branch:stuck_branches) progress'

    hit index branch subst tycos' progress' = do
        let tys = strictMap to_type tycos'
        let cos = strictMap to_co tycos'
        let new_ty = substTy subst (cab_rhs branch)
        -- TODO: again there is a risk of a thunk leak in the AxiomInstCo
        -- TODO: not clear if these args are correct, or if we can use a smart constructor of AxiomInstCo
        let args = map mkNomReflCo $ substTyVars subst (cab_tvs branch) ++ substTyVars subst (cab_cvs branch)
        let co = mkTcSymCo (AxiomInstCo axiom index args)
        -- let co = UnivCo (StepsProv 0 1) Nominal new_ty $! mkTyConApp tc tys
        let !co' | progress' = MCo (co `transCo` mkTyConAppCo Nominal tc cos) `transMCo` co0
                 | otherwise = MCo co `transMCo` co0
        rewrite_one_shallow new_ty co'

combine :: [(Type, MCoercion)] -> [(Type, Coercion)] -> [(Type, MCoercion)]
combine tycos tycos_more = strictZipWith
                                 (\ tyco1@(_,_) tyco2@(_,_) -> (fst tyco1, MCo (snd tyco1) `transMCo` snd tyco2))
                                 tycos_more tycos

transMCo :: MCoercion -> MCoercion -> MCoercion
transMCo MRefl co = co
transMCo co MRefl = co
transMCo (MCo co1) (MCo co2) = MCo (co1 `transCo` co2)

-- | Create a new 'Coercion' by composing the two given 'Coercion's transitively.
--   (co1 ; co2)
transCo :: Coercion -> Coercion -> Coercion
transCo = mkTcTransCo

{-
-- Like mkTcTransCo, but tries to be clever about squashing UnivCo.
-- This means we can avoid accumulating big structures when we get
-- lots of top-level type family reductions.
-- Currently disabled because StepsProv isn't properly linted.
transCo co1 co2 | isReflCo co1 = co2
                | isReflCo co2 = co1
transCo co1 co2
  | Just (Pair m1 n1) <- isSteps_maybe co1
  , Just (Pair m2 n2) <- isSteps_maybe co2
  , let !prov | n1 > m2   = StepsProv m1 (n2 + (n1 - m2))  -- TODO: check arithmetic
              | otherwise = StepsProv (m1 + (m2 - n1)) n2
        !lty = coercionLKind co1
        !rty = coercionRKind co2
  = UnivCo prov Nominal lty rty
transCo (GRefl r t1 (MCo co1)) (GRefl _ _ (MCo co2))
  = GRefl r t1 (MCo $ transCo co1 co2)
transCo co1 co2  = TransCo co1 co2
-}

isSteps_maybe :: Coercion -> Maybe (Pair Int)
isSteps_maybe (SymCo co) = swap <$> isSteps_maybe co
isSteps_maybe (AxiomInstCo{}) = Just (Pair 1 0)
isSteps_maybe (AxiomRuleCo{}) = Just (Pair 1 0)
isSteps_maybe (UnivCo (StepsProv m n) _ _ _) = Just (Pair m n)
isSteps_maybe _ = Nothing



-- TODO: lookupFamAppInert can yield a bottom constraint in the derived case
-- (because of calling ctEvCoercion), so with our additional strictness we have
-- to hack around it.
lookupFamAppInert_hacked :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcType, CtFlavourRole))
lookupFamAppInert_hacked tc tys = do
  mb <- lookupFamAppInert tc tys
  pure $ case mb of
           Just (co, ty, fr@(Derived, _)) -> Just (Refl (LitTy (NumTyLit 42)), ty, fr)
           _ -> mb



{-

if match_type subst0 pat_ty target_ty = (Just (target_ty', co), res)
then co :: target_ty' ~ target_ty

if match_type subst0 pat_ty target_ty = (Nothing, res)
then take target_ty' = target_ty, co = Refl in the following.


if res is MatchingSuccess subst1, then subst1 is a most general substitution
extending subst0 such that subst1(pat_ty) `eqType` target_ty'.

if res is MatchingStuck subst1, then subst1 is a substitution extending subst0
such that: if unif_subst is a unifying substitution extending subst0
(i.e. unif_subst pat_ty ~ unif_subst target_ty') then unif_subst must factor
through subst1.

if res is MatchingApart, then there is no unifying substitution unif_subst such
that unif_subst pat_ty ~ unif_subst target_ty'.


match_type tries to match (or unify) a pattern against a target.  If the target
is not in WHNF and the pattern forces it, it will try to rewrite the target.
Alongside the result of matching, returns a (rewritten target, co :: rewritten
target ~ original target) pair if it made any progress with rewriting.

If this gets stuck, we know that rewriting the target further can't help.

When matching a list of arguments, if we get stuck in one argument, we check the
other arguments just in case one of them is obviously apart.  For example given

type family F x y where
  F True False = True
  F _    True  = False

If we see `F alpha True` we want to observe that the first equation is apart, so
that we go on to try the second equation.

-}


data MatchingResult = MatchingSuccess !TCvSubst
                    | MatchingStuck !TCvSubst
                    | MatchingApart


-- TODO: consider using (Type, MCoercion) instead of Maybe (Type, Coercion)
-- here... but what would that look like for match_list?
match_type :: TCvSubst -> Type -> Type -> RewriteM (Maybe (Type, Coercion), MatchingResult)
match_type subst pat_ty target_ty
--  | pprTrace "match_type" (ppr pat_ty $$ ppr target_ty $$ ppr subst) False = undefined

  | Just pat_ty' <- tcView pat_ty
  = match_type subst pat_ty' target_ty

  | Just target_ty' <- tcView target_ty
  = match_type subst pat_ty target_ty'

  -- binding a variable in a pattern, no need to reduce the target
  | TyVarTy v <- pat_ty
  , Nothing <- lookupTyVar subst v
  = pure (Nothing, MatchingSuccess (extendTvSubst subst v target_ty))

  -- binding a non-linear occurrence of a variable in a pattern, need to
  -- equalise both sides
  | TyVarTy v <- pat_ty
  , Just x_ty <- lookupTyVar subst v
  = do (p1, p2, res) <- equalise_type (x_ty, MRefl) (target_ty, MRefl)
       -- See Note [Shallow rewriting: non-linear patterns: forgetting work]
       -- for why this is a bit unsatisfactory
       pure $ case res of
         EqualiseSuccess -> (Just (x_ty, mkSymCo (to_co p1) `transCo` to_co p2), MatchingSuccess subst)
         EqualiseApart   -> (to_mb p2, MatchingApart)
         EqualiseStuck   -> (to_mb p2, MatchingStuck subst)

  | LitTy x <- pat_ty
  , LitTy y <- target_ty
  = pure (Nothing, if x == y then MatchingSuccess subst else MatchingApart)

  -- TODO: TyConApp vs AppTy handle better?
  -- Do we need that at least one is an AppTy?
  | AppTy p_head p_arg <- pat_ty
  , Just (a_head, a_arg) <- tcRepSplitAppTy_maybe target_ty
  = first (fmap (\[(ty1,co1),(ty2,co2)] -> (mkAppTy ty1 ty2, mkAppCo co1 co2)))
        <$> match_list subst [p_head,p_arg] [a_head,a_arg]

  | Just (p_head, p_arg) <- tcRepSplitAppTy_maybe pat_ty
  , AppTy a_head a_arg <- target_ty
  = first (fmap (\[(ty1,co1),(ty2,co2)] -> (mkAppTy ty1 ty2, mkAppCo co1 co2)))
        <$> match_list subst [p_head,p_arg] [a_head,a_arg]

  -- pattern and target have the same head constructor, match the sub-patterns
  | Just (p_tc, p_args) <- tcSplitTyConApp_maybe pat_ty
  , Just (a_tc, a_args) <- tcSplitTyConApp_maybe target_ty
  , p_tc == a_tc
  = ASSERT( not (isTypeFamilyTyCon p_tc) )
    do (mb, res) <-  match_list subst p_args a_args
       let f xs = (mkTyConApp p_tc (map fst xs), mkTyConAppCo Nominal p_tc (map snd xs))
       pure (fmap f mb, res)

  -- pattern and target have disjoint head constructors, we are surely apart!
  | Just (p_tc, p_args) <- tcSplitTyConApp_maybe pat_ty
  , Just (a_tc, a_args) <- tcSplitTyConApp_maybe target_ty
  , p_tc /= a_tc
  , not (isTypeFamilyTyCon a_tc)
  = ASSERT( not (isTypeFamilyTyCon p_tc) )
    pure (Nothing, MatchingApart)

   -- pattern forces the argument
  | isWHNF pat_ty
  = ASSERT (not (isWHNF target_ty))
    do (target_ty', mco) <- rewrite_one_shallow target_ty MRefl
       -- This is where we need rewrite_one_shallow to return an MCoercion:
       -- rewriting might have followed a filled metatyvar, which will have
       -- MCo Refl as evidence rather than MRefl,
       -- so we know to keep looking here
       case mco of
         MRefl -> pure (Nothing, MatchingStuck subst)
         MCo co -> do (mb', res) <- match_type subst pat_ty target_ty'
                      pure (Just (maybe (target_ty', co) (\(ty',co') -> (ty', co' `transCo` co)) mb'), res)


  | CastTy pat_ty' g <- pat_ty
  = do traceRewriteM "dodgy matching CastTy" (ppr pat_ty' $$ ppr target_ty)
       (mb, res) <- match_type subst pat_ty' target_ty -- TODO: ignoring co can't be right! Do we need to pass it around?
       pure $ case mb of
         Nothing -> (Nothing, res)
         Just (ty',co') -> (Just (mkCastTy ty' g, castCoercionKind1 co' Nominal ty' pat_ty' g), res) -- TODO: needs thinking

  | otherwise = pprPanic "match_type" (ppr pat_ty $$ ppr target_ty $$ ppr subst)


match_list :: TCvSubst -> [Type] -> [Type]
       -> RewriteM (Maybe [(Type, Coercion)], MatchingResult)
match_list = go False []
  where
    go :: Bool -> [(Type, MCoercion)] -> TCvSubst -> [Type] -> [Type]
       -> RewriteM (Maybe [(Type, Coercion)], MatchingResult)
    go stuck acc subst [] [] = pure (anyProgress acc, if stuck then MatchingStuck subst else MatchingSuccess subst)
    go stuck acc subst (pat_ty:pat_tys) (target_ty:target_tys)
      = do (mb, res) <- match_type subst pat_ty target_ty
           let !target_ty' = maybe (target_ty, MRefl) (\(ty,co)->(ty,MCo co)) mb
               acc' = target_ty' : acc
           case res of
              MatchingSuccess subst' -> go stuck acc' subst' pat_tys target_tys
              MatchingApart          -> pure (anyProgress2 acc' target_tys, res)
              MatchingStuck subst'   -> go True acc' subst' pat_tys target_tys
                -- See Note [Shallow rewriting: getting stuck] for why we continue here

    anyProgress :: [(Type, MCoercion)] -> Maybe [(Type, Coercion)]
    anyProgress acc
      | any (isMCo . snd) acc = Just (map to_tyco (reverse acc))
      | otherwise       = Nothing

    anyProgress2 :: [(Type, MCoercion)] -> [Type] -> Maybe [(Type, Coercion)]
    anyProgress2 acc' target_tys
      | any (isMCo . snd) acc' = Just $ map to_tyco acc' `revApp` map (\ty -> (ty, mkNomReflCo ty)) target_tys
      | otherwise              = Nothing



data EqualiseResult = EqualiseSuccess  -- Rewritten types must be eqType
                    | EqualiseStuck
                    | EqualiseApart


-- See Note [equalise_type]
-- TODO: not clear there is any point taking MCoercions as input.
equalise_type :: (Type, MCoercion) -> (Type, MCoercion)
              -> RewriteM ( (Type, MCoercion)
                          , (Type, MCoercion)
                          , EqualiseResult
                          )
equalise_type p1@(ty1, co1) p2@(ty2, co2)
--  | pprTrace "equalise_type" (ppr ty1 $$ ppr co1 $$ ppr ty2 $$ ppr co2) False = undefined

  | Just ty1' <- tcView ty1
  = equalise_type (ty1', co1) p2

  | Just ty2' <- tcView ty2
  = equalise_type p1 (ty2', co2)

  -- equalising two literals, they are either identical or apart
  | LitTy x <- ty1
  , LitTy y <- ty2
  = pure (p1, p2, if x == y then EqualiseSuccess else EqualiseApart)

  -- TODO: TyConApp vs AppTy handle better?
  -- Do we need that at least one is an AppTy?
  | AppTy h1 a1 <- ty1
  , Just (h2, a2) <- tcRepSplitAppTy_maybe ty2
  = do (mb1, mb2, res) <- equalise_list [h1,a1] [h2,a2]
       let f [(h',hco),(a',aco)] = (mkAppTy h' a', mkAppCo hco aco)
       pure (rebuild f p1 mb1, rebuild f p2 mb2, res)

  | AppTy h2 a2 <- ty2
  , Just (h1, a1) <- tcRepSplitAppTy_maybe ty1
  = do (mb1, mb2, res) <- equalise_list [h1,a1] [h2,a2]
       let f [(h',hco),(a',aco)] = (mkAppTy h' a', mkAppCo hco aco)
       pure (rebuild f p1 mb1, rebuild f p2 mb2, res)

  -- both sides have the same head constructor, match the sub-patterns
  | Just (tc1, args1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, args2) <- tcSplitTyConApp_maybe ty2
  , tc1 == tc2
  , not (isTypeFamilyTyCon tc1)
  = do (mb1, mb2, res) <- equalise_list args1 args2
       let f xs = (mkTyConApp tc1 (map fst xs), mkTyConAppCo Nominal tc1 (map snd xs))
       pure (rebuild f p1 mb1, rebuild f p2 mb2, res)

  -- both sides have disjoint head constructors, we are surely apart!
  | Just (tc1, args1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, args2) <- tcSplitTyConApp_maybe ty2
  , tc1 /= tc2
  , not (isTypeFamilyTyCon tc1)
  , not (isTypeFamilyTyCon tc2)
  = pure (p1, p2, EqualiseApart)

  -- both sides are headed by the same type family:
  --   * if the arguments match on the nose we are done
  --   * otherwise, try rewriting both sides and see if we get WHNFs
  --   * if that gets stuck, and they are still headed by the same type families,
  --     try to equalise the arguments
  | Just (tc1, args1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, args2) <- tcSplitTyConApp_maybe ty2
  , tc1 == tc2
  , isTypeFamilyTyCon tc1
  = if eqTypes args1 args2
    then pure (p1, p2, EqualiseSuccess)
    else do (ty1', co1') <- rewrite_one_shallow ty1 MRefl
            (ty2', co2') <- rewrite_one_shallow ty2 MRefl
            let p1' = (ty1', co1' `transMCo` co1)
            let p2' = (ty2', co2' `transMCo` co2)
            case () of
              -- TODO: if only one is WHNF, the recursive call will try to
              -- rewrite the other again, which is a waste of time!
             _| isWHNF ty1' || isWHNF ty2' -> equalise_type p1' p2'
              | Just (tc1', args1') <- tcSplitTyConApp_maybe ty1'
              , Just (tc2', args2') <- tcSplitTyConApp_maybe ty2'
              , tc1' == tc2' -> do (mb1, mb2, res) <- equalise_list args1' args2'
              -- TODO: in this case can we rely on the fact that if rewriting
              -- produces a stuck type family application, it will be deeply
              -- rewritten?  If so we could just check eqTypes rather than
              -- calling equalise_list.
                                   let f xs = (mkTyConApp tc1 (map fst xs), mkTyConAppCo Nominal tc1 (map snd xs))
                                   -- If the arguments are equal we are done, but if they are apart
                                   -- the type families may still reduce to equal results...
                                   -- TODO: could we take advantage of injectivity here?
                                   let res' = case res of
                                                 EqualiseSuccess -> EqualiseSuccess
                                                 EqualiseApart   -> EqualiseStuck
                                                 EqualiseStuck   -> EqualiseStuck
                                   pure (rebuild f p1 mb1, rebuild f p2 mb2, res')
              | otherwise -> pure (p1', p2', EqualiseStuck)

   -- pattern forces the argument
  | isWHNF ty1
  = ASSERT (not (isWHNF ty2))
    do p2'@(ty2', mco) <- rewrite_one_shallow ty2 MRefl
       -- This is where we need rewrite_one_shallow to return an MCoercion:
       -- rewriting might have followed a filled metatyvar, which will have
       -- MCo Refl as evidence rather than MRefl,
       -- so we know to keep looking here.
       -- TODO: annoyingly we can't pass in co2 when calling rewrite_one_shallow,
       -- as otherwise this test doesn't work. Is it even worth taking a coercion
       -- as input to equalise_type? Or should rewrite_one_shallow return a
       -- progress boolean as well as a (M)Coercion?
       case mco of
         MRefl -> pure (p1, p2, EqualiseStuck)
         MCo{} -> equalise_type p1 (ty2', mco `transMCo` co2)

  | isWHNF ty2
  = ASSERT (not (isWHNF ty1))
    do p1'@(ty1', mco) <- rewrite_one_shallow ty1 MRefl
       case mco of
         MRefl -> pure (p1, p2, EqualiseStuck)
         MCo{} -> equalise_type (ty1', mco `transMCo` co1) p2

  -- equalising two equal variables, nothing to do
  | TyVarTy x <- ty1
  , TyVarTy y <- ty2
  , x == y
  = pure (p1, p2, EqualiseSuccess)

  -- equalising a variable with something that is not the same variable, try to
  -- rewrite the variable or the other type
  | TyVarTy v1 <- ty1
  = do rtr <- rewrite_tyvar1 v1
       case rtr of
           RTRFollowed ty co -> equalise_type (ty, MCo co `transMCo` co1) p2
           RTRNotFollowed    -> do (ty2', mco) <- rewrite_one_shallow ty2 MRefl
                                   case mco of -- TODO: think further here...
                                       MRefl  -> pure (p1, p2, EqualiseStuck)
                                       MCo co -> equalise_type p1 (ty2', mco `transMCo` co2)

  -- equalising a non-variable with a variable, try to rewrite the variable or
  -- the other type
  | TyVarTy v2 <- ty2
  = do rtr <- rewrite_tyvar1 v2
       case rtr of
           RTRFollowed ty co -> equalise_type p1 (ty, MCo co `transMCo` co2)
           RTRNotFollowed    -> do (ty1', mco) <- rewrite_one_shallow ty1 MRefl
                                   case mco of
                                       MRefl  -> pure (p1, p2, EqualiseStuck)
                                       MCo co -> equalise_type (ty1', mco `transMCo` co1) p2
                                                  -- TODO: deduplicate the above?

  | otherwise = pprPanic "equalise_type" (ppr p1 $$ ppr p2)

equalise_list :: [Type] -> [Type]
             -> RewriteM ( Maybe [(Type, Coercion)]
                         , Maybe [(Type, Coercion)]
                         , EqualiseResult
                         )
equalise_list = go False [] []
  where
    -- TODO: handle mismatched list lengths?
    go :: Bool -> [(Type, MCoercion)] -> [(Type, MCoercion)] -> [Type] -> [Type]
       -> RewriteM (Maybe [(Type, Coercion)], Maybe [(Type, Coercion)], EqualiseResult)
    go stuck acc1 acc2 [] [] = pure (anyProgress acc1, anyProgress acc2, if stuck then EqualiseStuck else EqualiseSuccess)
    go stuck acc1 acc2 (ty1:tys1) (ty2:tys2)
      = do (p1, p2, res) <- equalise_type (ty1, MRefl) (ty2, MRefl)
           let acc1' = p1 : acc1
               acc2' = p2 : acc2
           case res of
              EqualiseSuccess -> go stuck acc1' acc2' tys1 tys2
              EqualiseApart   -> pure (anyProgress2 acc1' tys1, anyProgress2 acc2' tys2, EqualiseApart)
              EqualiseStuck   -> go True acc1' acc2' tys1 tys2
              -- See Note [Shallow rewriting: getting stuck] for why we continue here

    anyProgress :: [(Type, MCoercion)] -> Maybe [(Type, Coercion)]
    anyProgress acc
      | any (isMCo . snd) acc = Just (map to_tyco (reverse acc))
      | otherwise       = Nothing

    anyProgress2 :: [(Type, MCoercion)] -> [Type] -> Maybe [(Type, Coercion)]
    anyProgress2 acc' target_tys
      | any (isMCo . snd) acc' = Just $ map to_tyco acc' `revApp` map (\ty -> (ty, mkNomReflCo ty)) target_tys
      | otherwise              = Nothing


rebuild :: ([(Type, Coercion)] -> (Type, Coercion)) -> (Type, MCoercion) -> Maybe [(Type, Coercion)]
        -> (Type, MCoercion)
rebuild f p        Nothing = p
rebuild f (_, mco) (Just xs) = let (ty, co) = f xs
                               in (ty, MCo co `transMCo` mco)


to_type :: (Type, MCoercion) -> Type
to_type = fst

to_co :: (Type, MCoercion) -> Coercion
to_co (ty, MRefl) = mkNomReflCo ty
to_co (_, MCo co) = co

to_both :: Type -> (Type, MCoercion)
to_both ty = (ty, MRefl)

to_tyco :: (Type, MCoercion) -> (Type, Coercion)
to_tyco (ty, MCo co) = (ty, co)
to_tyco (ty, MRefl)  = (ty, mkNomReflCo ty)

to_mb  :: (Type, MCoercion) -> Maybe (Type, Coercion)
to_mb (_, MRefl) = Nothing
to_mb (ty, MCo co) = Just (ty, co)

isMCo :: MCoercion -> Bool
isMCo MCo{} = True
isMCo MRefl = False

revApp :: [a] -> [a] -> [a]
revApp []     ys = ys
revApp (x:xs) ys = revApp xs (x:ys)


isWHNF :: TcType -> Bool
isWHNF t | Just t' <- tcView t = isWHNF t'
isWHNF TyVarTy{}       = False
isWHNF AppTy{}         = True
isWHNF (TyConApp tc _) = not (isTypeFamilyTyCon tc)
isWHNF ForAllTy{}      = True
isWHNF FunTy{}         = True
isWHNF LitTy{}         = True
isWHNF (CastTy ty _)   = isWHNF ty -- TODO ??
isWHNF (CoercionTy co) = True -- TODO ??


{-

Note [Shallow rewriting]
~~~~~~~~~~~~~~~~~~~~~~~~

Previously the rewriter satisfied the following specification:

  rewrite ty  ==>   (xi, co)
    where
      xi has no reducible type functions
         has no skolems that are mapped in the inert set
         has no filled-in metavariables
      co :: xi ~ ty

Crucially, xi has been rewritten as much as possible, i.e. it contains no type
family applications that can be reduced. We call this "deep rewriting".

However, in most contexts where we use rewriting it is enough to do less work,
merely making sure that the head of the type is fully rewritten.  Thus we aim to
define:

  rewrite_shallow ty ==> (ty', co)
    where
      ty' is not headed by a reductible type function
                        or a skolem that is mapped in the inert set
                        or a filled-in metavariable
      co :: ty' ~ ty

Shallow rewriting produces a WHNF if possible, but it may also result in a type
family application that cannot be further reduced, or in a skolem or
metavariable about which we have no information.

For example, suppose we have the following definitions:

  type family F a where
    F Int = Char
    F a   = a

  type family G b

Then we have the following examples:

     input type         rewrite         rewrite_shallow
   ------------------|---------------|--------------------
     F Int           |  Char         |  Char
     G (F Int)       |  G Char       |  G (F Int)
     F (Int, F Int)  |  (Int, Char)  |  (Int, F Int)

Why do this? Because deep rewriting can end up doing (perhaps infinitely) more
work than shallow rewriting.  For example:

  type family If b then_ else_ where
    If True then_ else_ = then_
    If False then_ else_ = else_

  type family x == y where
    x == x = True
    _ == _ = False

  type family Loop where
    Loop = Loop

  foo :: proxy x -> If x () Loop
  foo _ = undefined

  bar :: proxy x -> If x () Loop
  bar _ = () -- type error

If we try to deeply rewrite the type `If x () Loop`, we will end up rewriting
`Loop` and hence failing to terminate (or, in practice, running into the
`-freduction-depth` bound).  In contrast, shallow rewriting gives `If x () Loop`
so the definition of `foo` is accepted, while the definition of `bar` is
correctly rejected because `If x () Loop` does not match `()`.

(TODO: currently we get a loop in this case because we try deeply rewriting
stuck applications in case there is a useful Given. Probably we need to check
for matching Givens more lazily.)

From an implementation of rewrite_shallow, it is easy to implement (deep)
rewrite on top of it: call rewrite_shallow on the type, then recursively call it
on the subterms of the result.  (Although currently rewrite_one and
rewrite_one_shallow are independent.)


Note [Shallow rewriting for type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The interesting case of shallow rewriting is for type family applications,
implemented by rewrite_exact_fam_app_shallow.  This performs the stack depth
check (see Note [Shallow rewriting: stack depth check]) then calls a suitable
implementation depending on whether the family is closed, open or built-in.  We
will discuss closed families here.

For example:

  type family F a b where
    F Int Bool = Char
    F a   b    = b

How do we rewrite `F (F Int Int) Bool`?  For the outermost type family
application, rewrite_exact_fam_app_closed needs to decide which of the branches
of the type family matches.  Thus match_list considers each branch in turn and
tries to match the list of patterns (here [Int,Bool]) against the list of
targets (here [F Int Int,Bool]).  Crucially, matching can directly call back
into (shallow) rewriting, so it will call rewrite_one_shallow on `F Int Int` and
get back Int, at which point the first branch matches and the outermost type
family application reduces to Char.


Note [Shallow rewriting: using the inerts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:

  type family F a

  foo :: (F Int ~ Char) => F Int
  foo = 'x'

Type-checking the body of `foo` invokes the rewriter on `F Int`, which needs to
make use of the given constraint `F Int ~ Char` in the inert set.

In general, once we have tried rewriting a type family application using the
top-level instances/branches, and established that it is stuck, we must check to
see if any constraints in the inert set allow further rewrites.  This is
implemented in rewrite_exact_fam_app_inerts.

TODO: Currently, the inert_funeqs are stored deeply rewritten, and we deeply
rewrite before carrying out this check.  We should do this more lazily.  (We
could also establish an invariant that if shallow-rewriting a type family
application gets stuck, the arguments will be deeply rewritten, which might be
useful when handling non-linear patterns.  But it means we can't handle the
example in Note [Shallow rewriting].)


Note [Shallow rewriting: stack depth check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to avoid infinite loops, we call checkStackDepth on each call to
rewrite_exact_fam_app_shallow, and immediately call bumpDepth.  This means that
any rewriting done during matching, or once we have successfully reduced the
type family application, will have a higher depth.

In general rewrite_shallow should do fewer type family reductions than rewrite,
and hence trigger the stack depth limit less often.  But because we may end up
reducing in a different order, we may report slightly different errors when the
depth limit is reached (e.g. T7788 changes error message).

TODO: think further about this.  Are there any cases where the old code would
succeed but the new code will hit the limit?


Note [Shallow rewriting: getting stuck]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type family application may be stuck either because it involves a
(meta)variable for which no further information is available, or because it
doesn't match any of the (visible) branches.  For example:

  type family F a b where
    F Int Bool = Char
    F a   b    = b

  type family G b -- no equations

If we see `F alpha beta` or `F (G Int) (G Bool)` we can't make any progress.

However, if we see `F alpha Int` we know that the first branch cannot possibly
match regardless of alpha, so it is safe to choose the second branch.  Hence
match_list and equalise_list continue looking past stuck matches, but set a flag
so that the final result will be stuck rather than a definite match.


Note [Shallow rewriting: getting unstuck]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The scheme described in the previous section doesn't quite deal with `F alpha
alpha` or `F (G Int) (G Int)` which cannot possibly match the first equation,
because we need to be rather clever to see that. See T8020 for an example.

The old implementation of type family matching succeeds in this case by trying
the second branch, discovering that it matches, then checking apartness from the
first branch using core flattening. But that's a big hammer.

At the moment, rewrite_exact_fam_app_closed uses the core flattened apartness
check, but we need a better approach.

TODO: try out the following?

Idea: what if matching accumulates a "generalised substitution" or (another way
of describing the same thing) some "stuck constraints"?  (It will still reduce
type families as it goes, but if it gets stuck, it will return the set of stuck
constraints that are preventing progress.)

  match_list () () [alpha, alpha] [Int, Bool]
     match_type () () alpha Int  -- returns alpha ~?~ Int in the stuck constraints
     match_type () (alpha ~?~ Int) alpha Bool -- tries to add alpha ~?~ Bool to stuck constraints,
                                              -- but that is impossible so we get MatchingApart

  match_list () () [alpha, alpha] [G Int, G Int]
    ditto

The matching substitution maps tyvars in the pattern to types from the target.

The stuck constraints map stuck types (tyvars or tyfam apps) in the target to
types which may bind variables from the pattern or the target (the latter
arising from non-linear patterns).  I write `ty1 ~?~ ty2` for a stuck constraint
that says `ty1` must be equal to `ty2` for the match to succeed.

What about non-linearity?

  type family F a b where
    F [a] a = Int

  F (G w) w

  match_type () () [a] (G w)  -- returns (G w ~?~ [a]) stuck; doesn't bind a in subst?
  match_type () (G w ~?~ [a]) a w -- returns a := w in subst
  -- apply subst to stucks to get G w ~?~ [w]

I don't think we can ever learn more about the stuck tyfam to make it reduce,
because the subst never tells us about variables in the target, only in the
pattern.  The stuck constraints say "these things need to hold for the matching
to succeed" but we can't make them hold directly; they just might let us learn
that the match cannot possibly work.

What about equalising?

  type family F a b c where
    F [a] a a = Int

  F (G w) Int w

  match_type () () [a] (G w)  -- returns (G w ~?~ [a]) stuck; doesn't bind a in subst?
  match_type () (G w ~?~ [a]) a Int -- returns a := Int in subst
  -- apply subst to stuck constraints to get G w ~?~ [Int]
  match_type (a := Int) (G w ~?~ [Int]) a w
    -- calls equalise_type Int w
    -- gets stuck with w ~?~ Int
    -- conclusion: (a := Int) would be a matching substitution if we had (w ~?~ Int, G w ~?~ [Int])
       but we can't fill in w here, because we haven't committed to this branch.
    -- If we had some reason to commit to the branch, we could perhaps get more inference?
    -- Or perhaps we might be able to see that G Int is apart from [Int] and hence
    -- it can't possibly match?  GHC doesn't currently do this, but could it?

So equalising also has to return stuck constraints, but that's okay.

Here's an awkward example:

  type family F a where
    F Int = Char

  type family K a b where
    K Int Char = Bool
    K x y = x

  -- rewriting: K (F (F Int)) (F Char)

Here we need to rewrite the nested `F Int` to see that both arguments are the
same type family application, and hence the first branch cannot match.

Here's a yet more twisted variant of the previous example:

  {-# LANGUAGE TypeFamilyDependencies #-}

  type family Inj a = r | r -> a

  type family L a b where
    L Bool Bool = Bool
    L x y = x

  -- rewriting: L (Inj Char) (Inj Bool)

This will get stuck on `Inj Char ~?~ Bool, Inj Bool ~?~ Bool`.  But from the
injectivity annotation, we know that these constraints can never be mutually
satisfied.  So in principle it would be safe to report the first branch as apart
and choose the second branch.  This isn't currently supported by GHC, however.


Note [Shallow rewriting: problem with reducing type families during matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a problem with the idea of having matching reduce type families
directly, potentially returning a generalised substitution (or set of stuck
constraints), as described above. Consider:

  type family F a b where
    F Int Char = Double
    F x y = Bool

  type family Expensive a

Suppose we are rewriting `F (Expensive ()) (Expensive ())`.  With existing GHC,
we can see that the first branch doesn't match without doing *any* type family
reductions. Thus reduceTyFamApp_maybe succeeds immediately, without ever needing
to reduce `Expensive ()`.

This case is bad with the new approach, however. We first try to match
`Expensive ()` against Int and hence try to reduce `Expensive ()` to
WHNF. Unless it reduces to something apart from Int, we then reduce the second
occurrence of `Expensive ()` in order to match it against Char. Finally we
discover (either directly or via the stuck constraints) that `Expensive ()`
cannot be both Int and Char, so we fall through to the second branch.

In the worst case, where `Expensive ()` hits the stack depth limit, this could
make a program that type-checked under the old approach fail to type-check under
the new approach.

Perhaps matching should not immediately rewrite subterms, but should produce a
generalised substitution after all? Then if the final result is stuck on a type
family redex, we should try to reduce it?


Note [Shallow rewriting: non-linear patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following type family definition with a non-linear pattern match:

  type family H x y where
    H x x = True
    H _ _ = False

Now suppose we are rewriting `H (F Int) (G Bool)`.  Trying the first branch
calls

  match_list () [x,x] [F Int, G Bool]

which will trivially bind x := F Int (without reducing F) and end up calling

  match_type (x := F Int) x (G Bool)

But now match_type cannot bind x again! Instead, it must "equalise" F Int with G
Bool by calling

  equalise_type (F Int) (G Bool)

This will (shallowly) rewrite both sides in an attempt to uncover common
structure.

(This is named "equalise" rather than "unify" because we don't produce a
unifying substitution. But in fact I now think we will need to yield a
(generalised) substitution in order to handle non-linear patterns correctly, per
Note [Shallow rewriting: getting unstuck]. So it should perhaps be renamed.)

What about the cases?

  equalise_type (F Int) (F Int)
  equalise_type (F alpha) (F b)
  equalise_type (F (G Int)) (F Char)

In the first case, we can simply check eqType and we are done.  In the second,
zonking the arguments might reveal they are equal if we happen to have already
solved alpha with b. In the third case, it is possible that rewriting the
arguments to F will allow us to prove that they are equal, without reducing F
itself.

The equalise_type case for two type family applications headed by the same
family currently works as follows:

1. Call eqTypes to see if the arguments are already identical. If so, return
   EqualiseSuccess.  (This is an optimization to avoid unnecessary reductions.
   We could imagine other approaches, e.g. zonking the arguments before
   comparing them.)

2. Otherwise, try to rewrite both type families.  If either is now in WHNF,
   recursively call equalise_type. (TODO: we could do better here; if only one
   is WHNF this will pointlessly try to rewrite the other again.)

3. Otherwise, if we still have two type family applications with the same head,
   try to equalise their arguments.  If this succeeds, we know the applications
   are equal.  But if the arguments are apart, we are stuck, because the
   applications might still end up being equal (unless the type family is
   injective).

TODO: could we take advantage of injectivity here? Presumably we'd need some
kind of evidence of injectivity, though.


Note [equalise_type]
~~~~~~~~~~~~~~~~~~~~

equalise_type (ty1, co1) (ty2, co2)
requires
  co1 :: ty1 ~ ty1_orig
  co2 :: ty2 ~ ty2_orig

returns ((ty1', co1'), (ty2', co2'), eres)
  where
    co1' :: ty1' ~ ty1_orig
    co2' :: ty2' ~ ty2_orig

    eres == EqualiseSuccess  =>  ty1' `eqType` ty2'
    eres == EqualiseStuck    =>  we can't determine if ty1 and ty2 are equal
    eres == EqualiseApart    =>  there is no unifying substitution making ty1 equal to ty2


Note [Shallow rewriting: non-linear patterns: forgetting work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  type family F a where
    F Int = Char
    F a   = a

  type family H x y where
    H x x = x
    H _ _ = Int

If we are matching `H Char (F Int)` against the first branch of H, we first bind
[x := Char] then call `equalise_type Char (F Int)`, which rewrites F Int to Char
and succeeds, reducing the whole application of H to just Char.

However, suppose we are matching `H (F Int) Char` against the first branch.
This time we bind [x := F Int], call `equalise_type (F Int) Char` and succeed
again, but this time the resulting substitution has [x := F Int] and hence we
reduce the whole application to F Int, which we then have to rewrite again to
reach WHNF.

There is nothing unsound here, but we can end up doing more work than should in
principle be necessary.  The difficulty is seeing how to represent the fact that
the types in the substitution might be further rewritten before the match is
complete.

I think it might be possible to write a circular program for this (passing the
lazily-computed result substitution as an argument to matching, so we can use it
in the case for matching pattern variables).  I haven't yet tried, however.


Note [Shallow rewriting: use of MCoercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  MRefl    ==> no rewriting has been done,
               output type is eqType to input

  MCo Refl ==> input type has been rewritten solely by following meta-tyvars

  MCo co   ==> input type has been rewritten using type family reductions or inerts

rewrite_one_shallow and related functions use MCoercion to represent the
resulting coercions, where MRefl means that no progress has been made (i.e. the
returned type is eqType to the input type) while MCo means that some rewriting
has been done.

In particular, MCo Refl means that rewriting made progress (by zonking). This
matters in match_type. Suppose we are matching the pattern () against some
meta-tyvar alpha. If alpha is unfilled, then rewriting it produces MRefl so
match_type will get stuck.  But if alpha is filled with () already, then
rewriting it produces MCo Refl and match_type will succeed.

TODO: perhaps we should use a (Coercion, Bool) pair instead of
MCoercion. Sometimes we may have accumulated progress (and hence could pass in a
MCoercion) but need to know if the call being made makes more progress (and
hence have to pass in MRefl instead).


Note [Shallow rewriting: coercion accumulator]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rewrite_one_shallow takes as input a type ty and a coercion co :: ty ~r ty0; it
returns a rewritten type ty1 and a coercion co1 :: ty1 ~r ty0.

This means that when a type family application reduces, the subsequent call to
rewrite_one_shallow is a tail call, and we can avoid growing the stack for each
type family reduction step.  In particular, when the rewriter takes many
top-level reduction steps, the coercion merely consists of 'StepsProv'
containing a count of the number of steps.


Note [Shallow rewriting: dependent quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: this is not yet supported at all, we just fall back on the old behaviour.


Note [Shallow rewriting: call-by-name rather than call-by-need]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:

  type family Dup x where
    Dup x = (x, x)

  type family Expensive y where
    ...

Rewriting `Dup (Expensive y)` yields `(Expensive y, Expensive y)` thereby
duplicating the computation.  It would be preferable to use call-by-need
evaluation instead, i.e. evaluate `Expensive y` once and share the result.

(Perhaps we could arrange to rewrite `Dup (Expensive y)` to `(alpha, alpha)`
where `alpha` is a metavariable already filled with `Expensive y`.  Then when we
come to rewrite `alpha`, we would rewrite `Expensive y` and store the result so
that subsequently rewriting `alpha` again yields the result directly.  However
this needs care because the result of rewriting may depend on the inert set
which is not necessarily the same across multiple occurrences.)


Note [Shallow rewriting: avoiding building evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are some cases where we won't need the coercion:

 * Derived constraints

 * If we are just normalising a type using :kind!  (currently this doesn't use
   the rewriter, but if we were to make topNormalisetype use the same code path,
   it would)

 * TODO: are there any others?

In such cases it would be nice to avoid ever constructing coercions.  In
particular, evidence for Derived coercions is sometimes bottom (see
lookupFamAppInert_hacked) and if we want more strictness to avoid thunk leaks
this is problematic.

Idea: we could make rewriting polymorphic in an `Applicative box`, and construct
`box Coercion` rather than plain `Coercion`s. Then we can specialise `box ~
Identity` for the normal case and `box ~ Const ()` for the no-evidence case.  A
downside is we'll have to identify Deriveds early and send them to a different
function.



Note [Shallow rewriting: alternative approaches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The approach taken here involves making matching/unification directly call
rewriting, so that their results refer to potentially-rewritten versions of the
inputs.  This means rewriting and matching/unification are somewhat intertwined.

An alternative possibility would be to define them without reference to
rewriting, and have them return some kind of partial result including a subterm
that needs to be rewritten in order to make progress (possibly even with a
continuation to call once progress has been made).  However, this partial result
is in itself quite complex to represent, so the gain in simplicity is limited.
Moreover, it seems likely this will incur a performance penatly compared to just
doing the rewriting immediately we discover it is necessary.

(A plausible intermediate position here would be to have matching/unification
defined with reference to an abstract rewriting function, then specialised to
the actual rewriter.  This might also allow us to use the same code in
topNormaliseType, with a "rewriter" that merely does type family reduction.  I'd
like to get everything working with the current approach first though.)


Note [Shallow rewriting: unresolved issues]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * As discussed above in Note [Shallow rewriting: getting unstuck] and Note
   [Shallow rewriting: problem with reducing type families during matching],
   rewrite_exact_fam_app_closed currently relies on the core-flattened apartness
   check and can in some cases do too much reduction. We may need some kind of
   generalised substitution approach to solve this.

 * Dependently-kinded TyCons or coercions in types are not currently handled
   correctly.

 * Roles are not handled correctly. Currently rewrite_exact_fam_app_shallow sets
   the ambient role to Nominal, and downgrades the result if necessary.  But
   this fails to account for Given Representational equalities on type families.
   This manifests as a failure of (at least) T3423.

 * rewrite_shallow is currently not called anywhere, instead we hit the new code
   paths only when reducing a type family application.  This needs further work
   to figure out when it is safe to use rewrite_shallow in place of rewrite.  In
   particular:

    - rewrite_shallow currently zonks its results, because some call sites
      seemingly depended on this.  We shouldn't need to do so, however, and this
      will be a major performance cost.

    - If can_eq_nc' calls rewrite_shallow, it still needs to deeply rewrite
      before calling canEqCanLHS otherwise T10931 fails (amongst others).

 * Building Coercions leaks lots of thunks. We can avoid this to some extent
   using StepsProv, but this is currently disabled because StepsProv isn't
   correctly linted. It's not entirely clear how best to address this, though,
   especially if we want to continue using AxiomInstCo for single steps and
   StepsProv for two or more steps.

-}
