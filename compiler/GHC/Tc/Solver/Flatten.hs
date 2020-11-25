{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Tc.Solver.Flatten(
   flatten, flattenKind, flattenArgsNom,
   flattenType
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
import Control.Monad
import GHC.Utils.Monad ( zipWith3M )
import Data.List.NonEmpty ( NonEmpty(..) )

import Control.Arrow ( first )

{-
************************************************************************
*                                                                      *
*                FlattenEnv & FlatM
*             The flattening environment & monad
*                                                                      *
************************************************************************
-}

data FlattenEnv
  = FE { fe_loc     :: !CtLoc             -- See Note [Flattener CtLoc]
       , fe_flavour :: !CtFlavour
       , fe_eq_rel  :: !EqRel             -- See Note [Flattener EqRels]
       }

-- | The 'FlatM' monad is a wrapper around 'TcS' with a 'FlattenEnv'
newtype FlatM a
  = FlatM { runFlatM :: FlattenEnv -> TcS a }
  deriving (Functor)

instance Monad FlatM where
  m >>= k  = FlatM $ \env ->
             do { a  <- runFlatM m env
                ; runFlatM (k a) env }

instance Applicative FlatM where
  pure x = FlatM $ const (pure x)
  (<*>) = ap

instance HasDynFlags FlatM where
  getDynFlags = liftTcS getDynFlags

liftTcS :: TcS a -> FlatM a
liftTcS thing_inside
  = FlatM $ const thing_inside

-- convenient wrapper when you have a CtEvidence describing
-- the flattening operation
runFlattenCtEv :: CtEvidence -> FlatM a -> TcS a
runFlattenCtEv ev
  = runFlatten (ctEvLoc ev) (ctEvFlavour ev) (ctEvEqRel ev)

-- Run thing_inside (which does the flattening)
runFlatten :: CtLoc -> CtFlavour -> EqRel -> FlatM a -> TcS a
runFlatten loc flav eq_rel thing_inside
  = runFlatM thing_inside fmode
  where
    fmode = FE { fe_loc  = loc
               , fe_flavour = flav
               , fe_eq_rel = eq_rel }

traceFlat :: String -> SDoc -> FlatM ()
traceFlat herald doc = liftTcS $ traceTcS herald doc
{-# INLINE traceFlat #-}  -- see Note [INLINE conditional tracing utilities]

getFlatEnvField :: (FlattenEnv -> a) -> FlatM a
getFlatEnvField accessor
  = FlatM $ \env -> return (accessor env)

getEqRel :: FlatM EqRel
getEqRel = getFlatEnvField fe_eq_rel

getRole :: FlatM Role
getRole = eqRelRole <$> getEqRel

getFlavour :: FlatM CtFlavour
getFlavour = getFlatEnvField fe_flavour

getFlavourRole :: FlatM CtFlavourRole
getFlavourRole
  = do { flavour <- getFlavour
       ; eq_rel <- getEqRel
       ; return (flavour, eq_rel) }

getLoc :: FlatM CtLoc
getLoc = getFlatEnvField fe_loc

checkStackDepth :: Type -> FlatM ()
checkStackDepth ty
  = do { loc <- getLoc
       ; liftTcS $ checkReductionDepth loc ty }

-- | Change the 'EqRel' in a 'FlatM'.
setEqRel :: EqRel -> FlatM a -> FlatM a
setEqRel new_eq_rel thing_inside
  = FlatM $ \env ->
    if new_eq_rel == fe_eq_rel env
    then runFlatM thing_inside env
    else runFlatM thing_inside (env { fe_eq_rel = new_eq_rel })
{-# INLINE setEqRel #-}

-- | Make sure that flattening actually produces a coercion (in other
-- words, make sure our flavour is not Derived)
-- Note [No derived kind equalities]
noBogusCoercions :: FlatM a -> FlatM a
noBogusCoercions thing_inside
  = FlatM $ \env ->
    -- No new thunk is made if the flavour hasn't changed (note the bang).
    let !env' = case fe_flavour env of
          Derived -> env { fe_flavour = Wanted WDeriv }
          _       -> env
    in
    runFlatM thing_inside env'

bumpDepth :: FlatM a -> FlatM a
bumpDepth (FlatM thing_inside)
  = FlatM $ \env -> do
      -- bumpDepth can be called a lot during flattening so we force the
      -- new env to avoid accumulating thunks.
      { let !env' = env { fe_loc = bumpCtLocDepth (fe_loc env) }
      ; thing_inside env' }

{-
Note [Flattener EqRels]
~~~~~~~~~~~~~~~~~~~~~~~
When flattening, we need to know which equality relation -- nominal
or representation -- we should be respecting. The only difference is
that we rewrite variables by representational equalities when fe_eq_rel
is ReprEq, and that we unwrap newtypes when flattening w.r.t.
representational equality.

Note [Flattener CtLoc]
~~~~~~~~~~~~~~~~~~~~~~
The flattener does eager type-family reduction.
Type families might loop, and we
don't want GHC to do so. A natural solution is to have a bounded depth
to these processes. A central difficulty is that such a solution isn't
quite compositional. For example, say it takes F Int 10 steps to get to Bool.
How many steps does it take to get from F Int -> F Int to Bool -> Bool?
10? 20? What about getting from Const Char (F Int) to Char? 11? 1? Hard to
know and hard to track. So, we punt, essentially. We store a CtLoc in
the FlattenEnv and just update the environment when recurring. In the
TyConApp case, where there may be multiple type families to flatten,
we just copy the current CtLoc into each branch. If any branch hits the
stack limit, then the whole thing fails.

A consequence of this is that setting the stack limits appropriately
will be essentially impossible. So, the official recommendation if a
stack limit is hit is to disable the check entirely. Otherwise, there
will be baffling, unpredictable errors.

Note [Phantoms in the flattener]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

data Proxy p = Proxy

and we're flattening (Proxy ty) w.r.t. ReprEq. Then, we know that `ty`
is really irrelevant -- it will be ignored when solving for representational
equality later on. So, we omit flattening `ty` entirely. This may
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
*      Externally callable flattening functions                        *
*                                                                      *
*  They are all wrapped in runFlatten, so their                        *
*  flattening work gets put into the work list                         *
*                                                                      *
*********************************************************************
-}

-- | See Note [Flattening].
-- If (xi, co) <- flatten mode ev ty, then co :: xi ~r ty
-- where r is the role in @ev@.
flatten :: CtEvidence -> TcType
        -> TcS (Xi, TcCoercion)
flatten ev ty
  = do { traceTcS "flatten {" (ppr ty)
       ; (ty', co) <- runFlattenCtEv ev (flatten_one ty)
       ; traceTcS "flatten }" (ppr ty')
       ; return (ty', co) }

-- specialized to flattening kinds: never Derived, always Nominal
-- See Note [No derived kind equalities]
-- See Note [Flattening]
flattenKind :: CtLoc -> CtFlavour -> TcType -> TcS (Xi, TcCoercionN)
flattenKind loc flav ty
  = do { traceTcS "flattenKind {" (ppr flav <+> ppr ty)
       ; let flav' = case flav of
                       Derived -> Wanted WDeriv  -- the WDeriv/WOnly choice matters not
                       _       -> flav
       ; (ty', co) <- runFlatten loc flav' NomEq (flatten_one ty)
       ; traceTcS "flattenKind }" (ppr ty' $$ ppr co) -- co is never a panic
       ; return (ty', co) }

-- See Note [Flattening]
flattenArgsNom :: CtEvidence -> TyCon -> [TcType] -> TcS ([Xi], [TcCoercion])
-- Externally-callable, hence runFlatten
-- Flatten a vector of types all at once; in fact they are
-- always the arguments of type family or class, so
--      ctEvFlavour ev = Nominal
-- and we want to flatten all at nominal role
-- The kind passed in is the kind of the type family or class, call it T
-- The kind of T args must be constant (i.e. not depend on the args)
--
-- For Derived constraints the returned coercion may be undefined
-- because flattening may use a Derived equality ([D] a ~ ty)
flattenArgsNom ev tc tys
  = do { traceTcS "flatten_args {" (vcat (map ppr tys))
       ; (tys', cos, kind_co)
           <- runFlattenCtEv ev (flatten_args_tc tc Nothing tys)
       ; MASSERT( isReflMCo kind_co )
       ; traceTcS "flatten }" (vcat (map ppr tys'))
       ; return (tys', cos) }

-- | Flatten a type w.r.t. nominal equality. This is useful to rewrite
-- a type w.r.t. any givens. It does not do type-family reduction. This
-- will never emit new constraints. Call this when the inert set contains
-- only givens.
flattenType :: CtLoc -> TcType -> TcS TcType
flattenType loc ty
  = do { (xi, _) <- runFlatten loc Given NomEq $
                    flatten_one ty
                     -- use Given flavor so that it is rewritten
                     -- only w.r.t. Givens, never Wanteds/Deriveds
                     -- (Shouldn't matter, if only Givens are present
                     -- anyway)
       ; return xi }

{- *********************************************************************
*                                                                      *
*           The main flattening functions
*                                                                      *
********************************************************************* -}

{- Note [Flattening]
~~~~~~~~~~~~~~~~~~~~
  flatten ty  ==>   (xi, co)
    where
      xi has no reducible type functions
         has no skolems that are mapped in the inert set
         has no filled-in metavariables
      co :: xi ~ ty

Key invariants:
  (F0) co :: xi ~ zonk(ty')    where zonk(ty') ~ zonk(ty)
  (F1) tcTypeKind(xi) succeeds and returns a fully zonked kind
  (F2) tcTypeKind(xi) `eqType` zonk(tcTypeKind(ty))

Note that it is flatten's job to try to reduce *every type function it sees*.

Flattening also:
  * zonks, removing any metavariables, and
  * applies the substitution embodied in the inert set

Because flattening zonks and the returned coercion ("co" above) is also
zonked, it's possible that (co :: xi ~ ty) isn't quite true. So, instead,
we can rely on this fact:

  (F0) co :: xi ~ zonk(ty'), where zonk(ty') ~ zonk(ty)

Note that the left-hand type of co is *always* precisely xi. The right-hand
type may or may not be ty, however: if ty has unzonked filled-in metavariables,
then the right-hand type of co will be the zonk-equal to ty.
It is for this reason that we
occasionally have to explicitly zonk, when (co :: xi ~ ty) is important
even before we zonk the whole program. For example, see the FTRNotFollowed
case in flattenTyVar.

Why have these invariants on flattening? Because we sometimes use tcTypeKind
during canonicalisation, and we want this kind to be zonked (e.g., see
GHC.Tc.Solver.Canonical.canEqCanLHS).

Flattening is always homogeneous. That is, the kind of the result of flattening is
always the same as the kind of the input, modulo zonking. More formally:

  (F2) tcTypeKind(xi) `eqType` zonk(tcTypeKind(ty))

This invariant means that the kind of a flattened type might not itself be flat.

Recall that in comments we use alpha[flat = ty] to represent a
flattening skolem variable alpha which has been generated to stand in
for ty.

Note that we prefer to leave type synonyms unexpanded when possible,
so when the flattener encounters one, it first asks whether its
transitive expansion contains any type function applications or is
forgetful -- that is, omits one or more type variables in its RHS.  If so,
it expands the synonym and proceeds; if not, it simply returns the
unexpanded synonym. See also Note [Flattening synonyms].

Note [flatten_args performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In programs with lots of type-level evaluation, flatten_args becomes
part of a tight loop. For example, see test perf/compiler/T9872a, which
calls flatten_args a whopping 7,106,808 times. It is thus important
that flatten_args be efficient.

Performance testing showed that the current implementation is indeed
efficient. It's critically important that zipWithAndUnzipM be
specialized to TcS, and it's also quite helpful to actually `inline`
it. On test T9872a, here are the allocation stats (Dec 16, 2014):

 * Unspecialized, uninlined:     8,472,613,440 bytes allocated in the heap
 * Specialized, uninlined:       6,639,253,488 bytes allocated in the heap
 * Specialized, inlined:         6,281,539,792 bytes allocated in the heap

To improve performance even further, flatten_args_nom is split off
from flatten_args, as nominal equality is the common case. This would
be natural to write using mapAndUnzipM, but even inlined, that function
is not as performant as a hand-written loop.

 * mapAndUnzipM, inlined:        7,463,047,432 bytes allocated in the heap
 * hand-written recursion:       5,848,602,848 bytes allocated in the heap

If you make any change here, pay close attention to the T9872{a,b,c} tests
and T5321Fun.

If we need to make this yet more performant, a possible way forward is to
duplicate the flattener code for the nominal case, and make that case
faster. This doesn't seem quite worth it, yet.

Note [flatten_exact_fam_app performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Once we've got a flat rhs, we extend the famapp-cache to record
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

{-# INLINE flatten_args_tc #-}
flatten_args_tc
  :: TyCon         -- T
  -> Maybe [Role]  -- Nothing: ambient role is Nominal; all args are Nominal
                   -- Otherwise: no assumptions; use roles provided
  -> [Type]        -- Arg types [t1,..,tn]
  -> FlatM ( [Xi]  -- List of flattened args [x1,..,xn]
                   -- 1-1 corresp with [t1,..,tn]
           , [Coercion]  -- List of arg coercions [co1,..,con]
                         -- 1-1 corresp with [t1,..,tn]
                         --    coi :: xi ~r ti
           , MCoercionN) -- Result coercion, rco
                         --    rco : (T t1..tn) ~N (T (x1 |> co1) .. (xn |> con))
flatten_args_tc tc = flatten_args all_bndrs any_named_bndrs inner_ki emptyVarSet
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

{-# INLINE flatten_args #-}
flatten_args :: [TyCoBinder] -> Bool -- Binders, and True iff any of them are
                                     -- named.
             -> Kind -> TcTyCoVarSet -- function kind; kind's free vars
             -> Maybe [Role] -> [Type]    -- these are in 1-to-1 correspondence
                                          -- Nothing: use all Nominal
             -> FlatM ([Xi], [Coercion], MCoercionN)
-- Coercions :: Xi ~ Type, at roles given
-- Third coercion :: tcTypeKind(fun xis) ~N tcTypeKind(fun tys)
-- That is, the third coercion relates the kind of some function (whose kind is
-- passed as the first parameter) instantiated at xis to the kind of that
-- function instantiated at the tys. This is useful in keeping flattening
-- homoegeneous. The list of roles must be at least as long as the list of
-- types.
flatten_args orig_binders
             any_named_bndrs
             orig_inner_ki
             orig_fvs
             orig_m_roles
             orig_tys
  = case (orig_m_roles, any_named_bndrs) of
      (Nothing, False) -> flatten_args_fast orig_tys
      _ -> flatten_args_slow orig_binders orig_inner_ki orig_fvs orig_roles orig_tys
        where orig_roles = fromMaybe (repeat Nominal) orig_m_roles

{-# INLINE flatten_args_fast #-}
-- | fast path flatten_args, in which none of the binders are named and
-- therefore we can avoid tracking a lifting context.
-- There are many bang patterns in here. It's been observed that they
-- greatly improve performance of an optimized build.
-- The T9872 test cases are good witnesses of this fact.
flatten_args_fast :: [Type]
                  -> FlatM ([Xi], [Coercion], MCoercionN)
flatten_args_fast orig_tys
  = fmap finish (iterate orig_tys)
  where

    iterate :: [Type]
            -> FlatM ([Xi], [Coercion])
    iterate (ty:tys) = do
      (xi, co)   <- flatten_one ty
      (xis, cos) <- iterate tys
      pure (xi : xis, co : cos)
    iterate [] = pure ([], [])

    {-# INLINE finish #-}
    finish :: ([Xi], [Coercion]) -> ([Xi], [Coercion], MCoercionN)
    finish (xis, cos) = (xis, cos, MRefl)

{-# INLINE flatten_args_slow #-}
-- | Slow path, compared to flatten_args_fast, because this one must track
-- a lifting context.
flatten_args_slow :: [TyCoBinder] -> Kind -> TcTyCoVarSet
                  -> [Role] -> [Type]
                  -> FlatM ([Xi], [Coercion], MCoercionN)
flatten_args_slow binders inner_ki fvs roles tys
-- Arguments used dependently must be flattened with proper coercions, but
-- we're not guaranteed to get a proper coercion when flattening with the
-- "Derived" flavour. So we must call noBogusCoercions when flattening arguments
-- corresponding to binders that are dependent. However, we might legitimately
-- have *more* arguments than binders, in the case that the inner_ki is a variable
-- that gets instantiated with a Π-type. We conservatively choose not to produce
-- bogus coercions for these, too. Note that this might miss an opportunity for
-- a Derived rewriting a Derived. The solution would be to generate evidence for
-- Deriveds, thus avoiding this whole noBogusCoercions idea. See also
-- Note [No derived kind equalities]
  = do { flattened_args <- zipWith3M fl (map isNamedBinder binders ++ repeat True)
                                        roles tys
       ; return (simplifyArgsWorker binders inner_ki fvs roles flattened_args) }
  where
    {-# INLINE fl #-}
    fl :: Bool   -- must we ensure to produce a real coercion here?
                  -- see comment at top of function
       -> Role -> Type -> FlatM (Xi, Coercion)
    fl True  r ty = noBogusCoercions $ fl1 r ty
    fl False r ty =                    fl1 r ty

    {-# INLINE fl1 #-}
    fl1 :: Role -> Type -> FlatM (Xi, Coercion)
    fl1 Nominal ty
      = setEqRel NomEq $
        flatten_one ty

    fl1 Representational ty
      = setEqRel ReprEq $
        flatten_one ty

    fl1 Phantom ty
    -- See Note [Phantoms in the flattener]
      = do { ty <- liftTcS $ zonkTcType ty
           ; return (ty, mkReflCo Phantom ty) }

------------------
flatten_one :: TcType -> FlatM (Xi, Coercion)
-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.
--
-- Postcondition: Coercion :: Xi ~ TcType
-- The role on the result coercion matches the EqRel in the FlattenEnv

flatten_one ty
  | Just ty' <- flattenView ty  -- See Note [Flattening synonyms]
  = flatten_one ty'

flatten_one xi@(LitTy {})
  = do { role <- getRole
       ; return (xi, mkReflCo role xi) }

flatten_one (TyVarTy tv)
  = flattenTyVar tv

flatten_one (AppTy ty1 ty2)
  = flatten_app_tys ty1 [ty2]

flatten_one (TyConApp tc tys)
  -- If it's a type family application, try to reduce it
  | isTypeFamilyTyCon tc
  = flatten_fam_app tc tys

  -- For * a normal data type application
  --     * data family application
  -- we just recursively flatten the arguments.
  | otherwise
  = flatten_ty_con_app tc tys

flatten_one ty@(FunTy { ft_mult = mult, ft_arg = ty1, ft_res = ty2 })
  = do { (xi1,co1) <- flatten_one ty1
       ; (xi2,co2) <- flatten_one ty2
       ; (xi3,co3) <- setEqRel NomEq $ flatten_one mult
       ; role <- getRole
       ; return (ty { ft_mult = xi3, ft_arg = xi1, ft_res = xi2 }
                , mkFunCo role co3 co1 co2) }

flatten_one ty@(ForAllTy {})
-- TODO (RAE): This is inadequate, as it doesn't flatten the kind of
-- the bound tyvar. Doing so will require carrying around a substitution
-- and the usual substTyVarBndr-like silliness. Argh.

-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (bndrs, rho) = tcSplitForAllTyVarBinders ty
             tvs           = binderVars bndrs
       ; (rho', co) <- flatten_one rho
       ; return (mkForAllTys bndrs rho', mkHomoForAllCos tvs co) }

flatten_one (CastTy ty g)
  = do { (xi, co) <- flatten_one ty
       ; (g', _)  <- flatten_co g
       ; role <- getRole
       ; return (mkCastTy xi g', castCoercionKind1 co role xi ty g') }
         -- It makes a /big/ difference to call castCoercionKind1 not
         -- the more general castCoercionKind2.
         -- See Note [castCoercionKind1] in GHC.Core.Coercion

flatten_one (CoercionTy co) = first mkCoercionTy <$> flatten_co co

-- | "Flatten" a coercion. Really, just zonk it so we can uphold
-- (F1) of Note [Flattening]
flatten_co :: Coercion -> FlatM (Coercion, Coercion)
flatten_co co
  = do { co <- liftTcS $ zonkCo co
       ; env_role <- getRole
       ; let co' = mkTcReflCo env_role (mkCoercionTy co)
       ; return (co, co') }

-- flatten (nested) AppTys
flatten_app_tys :: Type -> [Type] -> FlatM (Xi, Coercion)
-- commoning up nested applications allows us to look up the function's kind
-- only once. Without commoning up like this, we would spend a quadratic amount
-- of time looking up functions' types
flatten_app_tys (AppTy ty1 ty2) tys = flatten_app_tys ty1 (ty2:tys)
flatten_app_tys fun_ty arg_tys
  = do { (fun_xi, fun_co) <- flatten_one fun_ty
       ; flatten_app_ty_args fun_xi fun_co arg_tys }

-- Given a flattened function (with the coercion produced by flattening) and
-- a bunch of unflattened arguments, flatten the arguments and apply.
-- The coercion argument's role matches the role stored in the FlatM monad.
--
-- The bang patterns used here were observed to improve performance. If you
-- wish to remove them, be sure to check for regeressions in allocations.
flatten_app_ty_args :: Xi -> Coercion -> [Type] -> FlatM (Xi, Coercion)
flatten_app_ty_args fun_xi fun_co []
  -- this will be a common case when called from flatten_fam_app, so shortcut
  = return (fun_xi, fun_co)
flatten_app_ty_args fun_xi fun_co arg_tys
  = do { (xi, co, kind_co) <- case tcSplitTyConApp_maybe fun_xi of
           Just (tc, xis) ->
             do { let tc_roles  = tyConRolesRepresentational tc
                      arg_roles = dropList xis tc_roles
                ; (arg_xis, arg_cos, kind_co)
                    <- flatten_vector (tcTypeKind fun_xi) arg_roles arg_tys

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
                    <- flatten_vector (tcTypeKind fun_xi) (repeat Nominal) arg_tys
                ; let arg_xi = mkAppTys fun_xi arg_xis
                      arg_co = mkAppCos fun_co arg_cos
                ; return (arg_xi, arg_co, kind_co) }

       ; role <- getRole
       ; return (homogenise_result xi co role kind_co) }

flatten_ty_con_app :: TyCon -> [TcType] -> FlatM (Xi, Coercion)
flatten_ty_con_app tc tys
  = do { role <- getRole
       ; let m_roles | Nominal <- role = Nothing
                     | otherwise       = Just $ tyConRolesX role tc
       ; (xis, cos, kind_co) <- flatten_args_tc tc m_roles tys
       ; let tyconapp_xi = mkTyConApp tc xis
             tyconapp_co = mkTyConAppCo role tc cos
       ; return (homogenise_result tyconapp_xi tyconapp_co role kind_co) }

-- Make the result of flattening homogeneous (Note [Flattening] (F2))
homogenise_result :: Xi              -- a flattened type
                  -> Coercion        -- :: xi ~r original ty
                  -> Role            -- r
                  -> MCoercionN      -- kind_co :: tcTypeKind(xi) ~N tcTypeKind(ty)
                  -> (Xi, Coercion)  -- (xi |> kind_co, (xi |> kind_co)
                                     --   ~r original ty)
homogenise_result xi co _ MRefl = (xi, co)
homogenise_result xi co r mco@(MCo kind_co)
  = (xi `mkCastTy` kind_co, (mkSymCo $ GRefl r xi mco) `mkTransCo` co)
{-# INLINE homogenise_result #-}

-- Flatten a vector (list of arguments).
flatten_vector :: Kind   -- of the function being applied to these arguments
               -> [Role] -- If we're flatten w.r.t. ReprEq, what roles do the
                         -- args have?
               -> [Type] -- the args to flatten
               -> FlatM ([Xi], [Coercion], MCoercionN)
flatten_vector ki roles tys
  = do { eq_rel <- getEqRel
       ; case eq_rel of
           NomEq  -> flatten_args bndrs
                                  any_named_bndrs
                                  inner_ki
                                  fvs
                                  Nothing
                                  tys
           ReprEq -> flatten_args bndrs
                                  any_named_bndrs
                                  inner_ki
                                  fvs
                                  (Just roles)
                                  tys
       }
  where
    (bndrs, inner_ki, any_named_bndrs) = split_pi_tys' ki  -- "RAE" fix
    fvs                                = tyCoVarsOfType ki
{-# INLINE flatten_vector #-}

{-
Note [Flattening synonyms]
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
             Flattening a type-family application
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

STEP 3. Flatten all arguments. This might expose more information so that we
  can use a top-level instance.

  Continue to the next step.

STEP 4. Try the inerts. Note that we try the inerts *after* flattening the
  arguments, because the inerts will have flattened LHSs.

  If an inert is found, jump to FINISH.

STEP 5. Try the famapp-cache again. Now that we've revealed more information
  in the arguments, the cache might be helpful.

  If we get a cache hit, jump to FINISH.

STEP 6. Try top-level instances, which might trigger now that we know more
  about the argumnents.

  If an instance is found, jump to FINISH.

STEP 7. No progress to be made. Return what we have. (Do not do FINISH.)

FINISH 1. We've made a reduction, but the new type may still have more
  work to do. So flatten the new type.

FINISH 2. Add the result to the famapp-cache, connecting the type we started
  with to the one we ended with.

Because STEP 1/2 and STEP 5/6 happen the same way, they are abstracted into
try_to_reduce.

FINISH is naturally implemented in `finish`. But, Note [flatten_exact_fam_app performance]
tells us that we should not add to the famapp-cache after STEP 1/2. So `finish`
is inlined in that case, and only FINISH 1 is performed.

-}

flatten_fam_app :: TyCon -> [TcType] -> FlatM (Xi, Coercion)
  --   flatten_fam_app            can be over-saturated
  --   flatten_exact_fam_app      lifts out the application to top level
  -- Postcondition: Coercion :: Xi ~ F tys
flatten_fam_app tc tys  -- Can be over-saturated
    = ASSERT2( tys `lengthAtLeast` tyConArity tc
             , ppr tc $$ ppr (tyConArity tc) $$ ppr tys)

                 -- Type functions are saturated
                 -- The type function might be *over* saturated
                 -- in which case the remaining arguments should
                 -- be dealt with by AppTys
      do { let (tys1, tys_rest) = splitAt (tyConArity tc) tys
         ; (xi1, co1) <- flatten_exact_fam_app tc tys1
               -- co1 :: xi1 ~ F tys1

         ; flatten_app_ty_args xi1 co1 tys_rest }

-- the [TcType] exactly saturate the TyCon
-- See Note [How to normalise a family application]
flatten_exact_fam_app :: TyCon -> [TcType] -> FlatM (Xi, Coercion)
flatten_exact_fam_app tc tys
  = do { checkStackDepth (mkTyConApp tc tys)

       -- STEP 1/2. Try to reduce without reducing arguments first.
       ; result1 <- try_to_reduce tc tys
       ; case result1 of
             -- Don't use the cache;
             -- See Note [flatten_exact_fam_app performance]
         { Just (co, xi) -> finish False (xi, co)
         ; Nothing ->

        -- That didn't work. So reduce the arguments, in STEP 3.
    do { eq_rel <- getEqRel
           -- checking eq_rel == NomEq saves ~0.5% in T9872a
       ; (xis, cos, kind_co) <- if eq_rel == NomEq
                                then flatten_args_tc tc Nothing tys
                                else setEqRel NomEq $
                                     flatten_args_tc tc Nothing tys
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
                 do { traceFlat "rewrite family application with inert"
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
      -- This recursively flattens the result and then adds to the cache
    finish :: Bool  -- add to the cache?
           -> (Xi, Coercion) -> FlatM (Xi, Coercion)
    finish use_cache (xi, co)
      = do { -- flatten the result: FINISH 1
             (fully, fully_co) <- bumpDepth $ flatten_one xi
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

-- Returned coercion is output ~r input, where r is the role in the FlatM monad
-- See Note [How to normalise a family application]
try_to_reduce :: TyCon -> [TcType] -> FlatM (Maybe (TcCoercion, TcType))
try_to_reduce tc tys
  = do { result <- liftTcS $ firstJustsM [ lookupFamAppCache tc tys  -- STEP 5
                                         , matchFam tc tys ]         -- STEP 6
       ; downgrade result }
  where
    -- The result above is always Nominal. We might want a Representational
    -- coercion; this downgrades (and prints, out of convenience).
    downgrade :: Maybe (TcCoercionN, TcType) -> FlatM (Maybe (TcCoercion, TcType))
    downgrade Nothing = return Nothing
    downgrade result@(Just (co, xi))
      = do { traceFlat "Eager T.F. reduction success" $
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
             Flattening a type variable
*                                                                      *
********************************************************************* -}

-- | The result of flattening a tyvar "one step".
data FlattenTvResult
  = FTRNotFollowed
      -- ^ The inert set doesn't make the tyvar equal to anything else

  | FTRFollowed TcType Coercion
      -- ^ The tyvar flattens to a not-necessarily flat other type.
      -- co :: new type ~r old type, where the role is determined by
      -- the FlattenEnv

flattenTyVar :: TyVar -> FlatM (Xi, Coercion)
flattenTyVar tv
  = do { mb_yes <- flatten_tyvar1 tv
       ; case mb_yes of
           FTRFollowed ty1 co1  -- Recur
             -> do { (ty2, co2) <- flatten_one ty1
                   -- ; traceFlat "flattenTyVar2" (ppr tv $$ ppr ty2)
                   ; return (ty2, co2 `mkTransCo` co1) }

           FTRNotFollowed   -- Done, but make sure the kind is zonked
                            -- Note [Flattening] invariant (F0) and (F1)
             -> do { tv' <- liftTcS $ updateTyVarKindM zonkTcType tv
                   ; role <- getRole
                   ; let ty' = mkTyVarTy tv'
                   ; return (ty', mkTcReflCo role ty') } }

flatten_tyvar1 :: TcTyVar -> FlatM FlattenTvResult
-- "Flattening" a type variable means to apply the substitution to it
-- Specifically, look up the tyvar in
--   * the internal MetaTyVar box
--   * the inerts
-- See also the documentation for FlattenTvResult

flatten_tyvar1 tv
  = do { mb_ty <- liftTcS $ isFilledMetaTyVar_maybe tv
       ; case mb_ty of
           Just ty -> do { traceFlat "Following filled tyvar"
                             (ppr tv <+> equals <+> ppr ty)
                         ; role <- getRole
                         ; return (FTRFollowed ty (mkReflCo role ty)) } ;
           Nothing -> do { traceFlat "Unfilled tyvar" (pprTyVar tv)
                         ; fr <- getFlavourRole
                         ; flatten_tyvar2 tv fr } }

flatten_tyvar2 :: TcTyVar -> CtFlavourRole -> FlatM FlattenTvResult
-- The tyvar is not a filled-in meta-tyvar
-- Try in the inert equalities
-- See Definition [Applying a generalised substitution] in GHC.Tc.Solver.Monad
-- See Note [Stability of flattening] in GHC.Tc.Solver.Monad

flatten_tyvar2 tv fr@(_, eq_rel)
  = do { ieqs <- liftTcS $ getInertEqs
       ; case lookupDVarEnv ieqs tv of
           Just (EqualCtList (ct :| _))   -- If the first doesn't work,
                                          -- the subsequent ones won't either
             | CEqCan { cc_ev = ctev, cc_lhs = TyVarLHS tv
                      , cc_rhs = rhs_ty, cc_eq_rel = ct_eq_rel } <- ct
             , let ct_fr = (ctEvFlavour ctev, ct_eq_rel)
             , ct_fr `eqCanRewriteFR` fr  -- This is THE key call of eqCanRewriteFR
             -> do { traceFlat "Following inert tyvar"
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

                    ; return (FTRFollowed rhs_ty rewrite_co) }
                    -- NB: ct is Derived then fmode must be also, hence
                    -- we are not going to touch the returned coercion
                    -- so ctEvCoercion is fine.

           _other -> return FTRNotFollowed }

{-
Note [An alternative story for the inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(This entire note is just background, left here in case we ever want
 to return the previous state of affairs)

We used (GHC 7.8) to have this story for the inert substitution inert_eqs

 * 'a' is not in fvs(ty)
 * They are *inert* in the weaker sense that there is no infinite chain of
   (i1 `eqCanRewrite` i2), (i2 `eqCanRewrite` i3), etc

This means that flattening must be recursive, but it does allow
  [G] a ~ [b]
  [G] b ~ Maybe c

This avoids "saturating" the Givens, which can save a modest amount of work.
It is easy to implement, in GHC.Tc.Solver.Interact.kick_out, by only kicking out an inert
only if (a) the work item can rewrite the inert AND
        (b) the inert cannot rewrite the work item

This is significantly harder to think about. It can save a LOT of work
in occurs-check cases, but we don't care about them much.  #5837
is an example, but it causes trouble only with the old (pre-Fall 2020)
flattening story. It is unclear if there is any gain w.r.t. to
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
  split _       (ForAllTy b res) = let (bs, ty, _) = split res res
                                   in  (Named b : bs, ty, True)
  split _       (FunTy { ft_af = af, ft_mult = w, ft_arg = arg, ft_res = res })
                                 = let (bs, ty, named) = split res res
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
