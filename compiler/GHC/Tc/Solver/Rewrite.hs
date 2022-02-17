{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Tc.Solver.Rewrite(
   rewrite, rewriteKind, rewriteArgsNom,
   rewriteType
 ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Core.TyCo.Ppr ( pprTyVar )
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep   -- performs delicate algorithm on types
import GHC.Core.Coercion
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Tc.Solver.Monad as TcS

import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Exts (oneShot)
import Data.Bifunctor
import Control.Monad
import GHC.Utils.Monad ( zipWith3M )
import Data.List.NonEmpty ( NonEmpty(..) )
import Control.Applicative (liftA3)
import GHC.Builtin.Types.Prim (tYPETyCon)

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
  = do { rewritten_args <- zipWith3M rw (map isNamedBinder binders ++ repeat True)
                                        roles tys
       ; return (simplifyArgsWorker binders inner_ki fvs roles rewritten_args) }
  where
    {-# INLINE rw #-}
    rw :: Bool   -- must we ensure to produce a real coercion here?
                 -- see comment at top of function
       -> Role -> Type -> RewriteM (Xi, Coercion)
    rw True  r ty = noBogusCoercions $ rw1 r ty
    rw False r ty =                    rw1 r ty

    {-# INLINE rw1 #-}
    rw1 :: Role -> Type -> RewriteM (Xi, Coercion)
    rw1 Nominal ty
      = setEqRel NomEq $
        rewrite_one ty

    rw1 Representational ty
      = setEqRel ReprEq $
        rewrite_one ty

    rw1 Phantom ty
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

rewrite_one (FunTy { ft_af = vis, ft_mult = mult, ft_arg = ty1, ft_res = ty2 })
  = do { (arg_xi,arg_co) <- rewrite_one ty1
       ; (res_xi,res_co) <- rewrite_one ty2

        -- Important: look at the *reduced* type, so that any unzonked variables
        -- in kinds are gone and the getRuntimeRep succeeds.
        -- cf. Note [Decomposing FunTy] in GHC.Tc.Solver.Canonical.
       ; let arg_rep = getRuntimeRep arg_xi
             res_rep = getRuntimeRep res_xi

       ; (w_redn, arg_rep_redn, res_rep_redn) <- setEqRel NomEq $
           liftA3 (,,) (rewrite_one mult)
                       (rewrite_one arg_rep)
                       (rewrite_one res_rep)
       ; role <- getRole

       ; let arg_rep_co = mkSymCo (snd arg_rep_redn)
                -- :: arg_rep ~ arg_rep_xi
             arg_ki_co  = mkTyConAppCo Nominal tYPETyCon [arg_rep_co]
                -- :: TYPE arg_rep ~ TYPE arg_rep_xi
             casted_arg_redn =
                 ( mkCastTy arg_xi arg_ki_co
                 , mkCoherenceLeftCo role arg_xi arg_ki_co arg_co
                 )
                -- :: ty1 ~> arg_xi |> arg_ki_co

             res_ki_co  = mkTyConAppCo Nominal tYPETyCon [mkSymCo $ snd res_rep_redn]
             casted_res_redn =
                ( mkCastTy res_xi res_ki_co
                , mkCoherenceLeftCo role res_xi res_ki_co res_co
                )

          -- We must rewrite the representations, because that's what would
          -- be done if we used TyConApp instead of FunTy. These rewritten
          -- representations are seen only in casts of the arg and res, below.
          -- Forgetting this caused #19677.
       ; return
            ( mkFunTy vis (fst w_redn) (fst casted_arg_redn) (fst casted_res_redn)
            , mkFunCo role (snd w_redn) (snd casted_arg_redn) (snd casted_res_redn)
            )
       }

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
rewrite_exact_fam_app :: TyCon -> [TcType] -> RewriteM (Xi, Coercion)
rewrite_exact_fam_app tc tys
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
       ; result2 <- liftTcS $ lookupFamAppInert tc xis
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
                    ; let rewrite_co1 = mkSymCo (ctEvCoercion ctev)
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
