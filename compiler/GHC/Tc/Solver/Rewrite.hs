{-# OPTIONS_GHC -fspec-constr-threshold=1200 #-}
  -- This threshold (smaller than the default 2000) avoids SpecConstr from firing
  -- on the inner loop of GHC.Core.Reduction.simplifyArgsWorker (inlined into
  -- this module) based on a 'LiftingContext' argument, as this causes
  -- significant reboxing (regressing compile-time allocations in T9872d by ~4%),
  -- as per #27628.
  --
  -- The 1200 threshold was chosen to avoid this issue while still allowing
  -- beneficial SpecConstr to fire in the rest of the module.

module GHC.Tc.Solver.Rewrite(
   rewrite, rewriteForErrors, rewriteArgsNom,
   rewriteType
 ) where

import GHC.Prelude

import GHC.Core.TyCo.Ppr ( pprTyVar )
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( CtLoc, resetCtLocDepth )
import GHC.Core.Predicate
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep   -- performs delicate algorithm on types
import GHC.Core.TyCo.FVs( univCoDepsDeepCoVars, univCoDepsHasHoles
                        , emptyUnivCoDeps, isEmptyUnivCoDeps
                        , addUnivCoDep, addUnivCoDepCoVar, addUnivCoDepHole
                        , unionUnivCoDeps
                        )
import GHC.Core.Coercion
import GHC.Core.Lint( lintTcCoercion )
import GHC.Core.Reduction
import GHC.Types.Unique.FM
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Driver.Config.Core.Lint( initLintConfig )
import GHC.Driver.DynFlags
import GHC.Tc.Utils.Monad (rewriterTcMPlugins)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Tc.Solver.Monad as TcS

import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Exts (oneShot)
import Control.Monad
import Control.Applicative (liftA3)
import GHC.Builtin.WiredIn.Types (tYPETyCon)
import Data.List ( find )
import GHC.Data.List.Infinite (Infinite)
import GHC.Data.Bag( listToBag, bagToList, isEmptyBag )
import GHC.Data.Pair (Pair(..))
import qualified GHC.Data.List.Infinite as Inf

{-
************************************************************************
*                                                                      *
*                RewriteEnv & RewriteM
*             The rewriting environment & monad
*                                                                      *
************************************************************************
-}

-- | The 'RewriteM' monad is a wrapper around 'TcS' with a 'RewriteEnv'
newtype RewriteM a
  = RewriteM { runRewriteM :: RewriteEnv -> TcS a }

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

instance Functor RewriteM where
  fmap f (RewriteM x) = mkRewriteM $ \env -> fmap f (x env)

instance HasDynFlags RewriteM where
  getDynFlags = liftTcS getDynFlags

liftTcS :: TcS a -> RewriteM a
liftTcS thing_inside
  = mkRewriteM $ \_ -> thing_inside

-- convenient wrapper when you have a CtEvidence describing
-- the rewriting operation
runRewriteCtEv :: CtEvidence -> RewriteM a -> TcS (a, CoHoleSet)
runRewriteCtEv ev
  = runRewrite (ctEvLoc ev) (ctEvFlavour ev) (ctEvRewriteEqRel ev)

-- Run thing_inside (which does the rewriting)
-- Also returns the set of Wanteds which rewrote a Wanted;
-- See Note [Wanteds rewrite Wanteds: rewriter-sets] in GHC.Tc.Types.Constraint
runRewrite :: CtLoc -> CtFlavour -> EqRel -> RewriteM a -> TcS (a, CoHoleSet)
runRewrite loc flav eq_rel thing_inside
  = do { dflags <- getDynFlags
       ; used_cos_ref  <- newTcRef emptyUnivCoDeps
       ; rewriters_ref <- newTcRef emptyCoHoleSet
       ; let fmode = RE { re_loc       = resetCtLocDepth loc
                            -- Start reducing from zero
                        , re_flavour   = flav
                        , re_eq_rel    = eq_rel
                        , re_rewriters = rewriters_ref
                        , re_used_cos  = used_cos_ref
                        , re_zap_coercions = gopt Opt_ZapCoercions dflags }
       ; res <- runRewriteM thing_inside fmode
       ; rewriters <- readTcRef rewriters_ref
       ; return (res, rewriters) }

traceRewriteM :: String -> SDoc -> RewriteM ()
traceRewriteM herald doc = liftTcS $ traceTcS herald doc
{-# INLINE traceRewriteM #-}  -- see Note [INLINE conditional tracing utilities]

getRewriteEnv :: RewriteM RewriteEnv
getRewriteEnv
  = mkRewriteM $ \env -> return env

getRewriteEnvField :: (RewriteEnv -> a) -> RewriteM a
getRewriteEnvField accessor
  = mkRewriteM $ \env -> return (accessor env)

getEqRel :: RewriteM EqRel
getEqRel = getRewriteEnvField re_eq_rel

getRole :: RewriteM Role
getRole = eqRelRole <$> getEqRel

getFlavour :: RewriteM CtFlavour
getFlavour = getRewriteEnvField re_flavour

getFlavourRole :: RewriteM CtFlavourRole
getFlavourRole
  = do { flavour <- getFlavour
       ; eq_rel <- getEqRel
       ; return (flavour, eq_rel) }

-- | Change the 'EqRel' in a 'RewriteM'.
setEqRel :: EqRel -> RewriteM a -> RewriteM a
setEqRel new_eq_rel thing_inside
  = mkRewriteM $ \env ->
    if new_eq_rel == re_eq_rel env
    then runRewriteM thing_inside env
    else runRewriteM thing_inside (env { re_eq_rel = new_eq_rel })
{-# INLINE setEqRel #-}

bumpReductionDepthRM :: Type -> RewriteM a -> RewriteM a
bumpReductionDepthRM ty (RewriteM thing_inside)
  = mkRewriteM $ \env -> do
      { loc' <- TcS.bumpReductionDepth (re_loc env) ty
      ; let !env' = env { re_loc = loc' }
            -- !env: bumpReductionDepth can be called a lot during rewriting
            -- so we force the new env to avoid accumulating thunks
      ; thing_inside env' }

recordRewriter :: CtEvidence -> RewriteM ()
-- ^ Record that we have rewritten the target with this (equality) evidence
--
-- See Note [Wanteds rewrite Wanteds: rewriter-sets] in GHC.Tc.Types.Constraint
--
-- Precondition: the CtEvidence is for an equality constraint
--
-- Also records the evidence coercion in 're_used_cos',
-- as per Note [Computing coercion dependencies].
recordRewriter (CtGiven (GivenCt { ctev_evar = evar })) = mkRewriteM $ \env ->
  updTcRef (re_used_cos env) (addUnivCoDepCoVar evar)
recordRewriter (CtWanted (WantedCt { ctev_dest = dest })) =
  case dest of
    HoleDest hole -> mkRewriteM $ \env ->
      do { updTcRef (re_rewriters env) (`addRewriter` hole)
         ; updTcRef (re_used_cos env) (addUnivCoDepHole hole) }
    other -> pprPanic "recordRewriter: non-equality constraint" (ppr other)

{- Note [Rewriter EqRels]
~~~~~~~~~~~~~~~~~~~~~~~~~
When rewriting, we need to know which equality relation -- nominal
or representational -- we should be respecting.  This is controlled
by the `re_eq_rel` field of RewriteEnv.

* When rewriting primitive /representational/ equalities, (t1 ~# t2),
  we set re_eq_rel=ReprEq.
* For all other constraints, we set re_eq_rel=NomEq

See Note [The rewrite-role of a constraint] in GHC.Tc.Types.Constraint.

The only difference is that when re_eq_rel=ReprEq
* we rewrite variables by representational equalities
* we unwrap newtypes

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

-}

{- *********************************************************************
*                                                                      *
*      Externally callable rewriting functions                         *
*                                                                      *
************************************************************************
-}

-- | See Note [Rewriting].
--
-- If @(Reduction co xi, rws) <- rewrite ev ty@,
-- then @co :: ty ~r xi@, where @r@ is the role in @ev@ and @rws@ is the set of
-- coercion holes that have been used to rewrite (see Note [Wanteds rewrite Wanteds: rewriter-sets]
-- in GHC.Tc.Types.Constraint).
rewrite :: CtEvidence -> TcType
        -> TcS (Reduction, CoHoleSet)
rewrite ev ty
  = do { traceTcS "rewrite {" (ppr ty)
       ; result@(redn, _) <- runRewriteCtEv ev (rewrite_one ty)
       ; traceTcS "rewrite }" (ppr $ reductionReducedType redn)
       ; return result }

-- | See Note [Rewriting]
-- `rewriteForErrors` is a variant of 'rewrite' that rewrites
-- w.r.t. nominal equality only, as this is better than full rewriting
-- for error messages. (This was important when we flirted with rewriting
-- newtypes but perhaps less so now.)
rewriteForErrors :: CtEvidence -> TcType
                 -> TcS (Reduction, CoHoleSet)
rewriteForErrors ev ty
  = do { traceTcS "rewriteForErrors {" (ppr ty)
       ; result@(redn, rewriters) <-
           runRewrite (ctEvLoc ev) (ctEvFlavour ev) NomEq (rewrite_one ty)
       ; traceTcS "rewriteForErrors }" (ppr $ reductionReducedType redn)
       ; return $ case ctEvRewriteEqRel ev of
           NomEq -> result
           ReprEq -> (mkSubRedn redn, rewriters) }

-- See Note [Rewriting]
rewriteArgsNom :: CtEvidence -> TyCon -> [TcType]
               -> TcS (Reductions, CoHoleSet)
-- Externally-callable, hence runRewrite
-- Rewrite a vector of types all at once; in fact they are
-- always the arguments of type family or class, so
--      ctEvFlavour ev = Nominal
-- and we want to rewrite all at nominal role
-- The kind passed in is the kind of the type family or class, call it T
-- The kind of T args must be constant (i.e. not depend on the args)
--
-- Final return value returned which Wanteds rewrote another Wanted
-- See Note [Wanteds rewrite Wanteds: rewriter-sets] in GHC.Tc.Types.Constraint
rewriteArgsNom ev tc tys
  = do { traceTcS "rewrite_args {" (vcat (map ppr tys))
       ; (ArgsReductions redns@(Reductions _ tys') kind_co, rewriters)
           <- runRewriteCtEv ev (rewrite_args_tc tc Nothing tys)
       ; massert (isReflMCo kind_co)
       ; traceTcS "rewrite }" (vcat (map ppr tys'))
       ; return (redns, rewriters) }

-- | Rewrite a type w.r.t. nominal equality. This is useful to rewrite
-- a type w.r.t. any givens. It does not do type-family reduction. This
-- will never emit new constraints. Call this when the inert set contains
-- only givens.
rewriteType :: CtLoc -> TcType -> TcS TcType
rewriteType loc ty
  = do { (redn, _) <- runRewrite loc Given NomEq $
                       rewrite_one ty
                     -- use Given flavor so that it is rewritten
                     -- only w.r.t. Givens, never Wanteds
                     -- (Shouldn't matter, if only Givens are present
                     -- anyway)
       ; return $ reductionReducedType redn }

{- *********************************************************************
*                                                                      *
*           The main rewriting functions
*                                                                      *
********************************************************************* -}

{- Note [Rewriting]
~~~~~~~~~~~~~~~~~~~~
  rewrite ty  ==>  Reduction co xi
    where
      xi has no reducible type functions
         has no skolems that are mapped in the inert set
         has no filled-in metavariables
      co :: ty ~ xi (coercions in reductions are always left-to-right)

Key invariants:
  (F0) co :: zonk(ty') ~ xi   where zonk(ty') ~ zonk(ty)
  (F1) typeKind(xi) succeeds and returns a fully zonked kind
  (F2) typeKind(xi) `eqType` zonk(typeKind(ty))

Note that it is rewrite's job to try to reduce *every type function it sees*.

Rewriting also:
  * zonks, removing any metavariables, and
  * applies the substitution embodied in the inert set

Because rewriting zonks and the returned coercion ("co" above) is also
zonked, it's possible that (co :: ty ~ xi) isn't quite true. So, instead,
we can rely on this fact:

  (F0) co :: zonk(ty') ~ xi, where zonk(ty') ~ zonk(ty)

Note that the right-hand type of co is *always* precisely xi. The left-hand
type may or may not be ty, however: if ty has unzonked filled-in metavariables,
then the left-hand type of co will be the zonk-equal to ty.
It is for this reason that we occasionally have to explicitly zonk,
when (co :: ty ~ xi) is important even before we zonk the whole program.
For example, see the RTRNotFollowed case in rewriteTyVar.

Why have these invariants on rewriting? Because we sometimes use typeKind
during canonicalisation, and we want this kind to be zonked (e.g., see
GHC.Tc.Solver.Equality.canEqCanLHS).

Rewriting is always homogeneous. That is, the kind of the result of rewriting is
always the same as the kind of the input, modulo zonking. More formally:

  (F2) zonk(typeKind(ty)) `eqType` typeKind(xi)

This invariant means that the kind of a rewritten type might not itself be rewritten.

Note that we prefer to leave type synonyms unexpanded when possible,
so when the rewriter encounters one, it first asks whether its
transitive expansion contains any type function applications or is
forgetful -- that is, omits one or more type variables in its RHS.  If so,
it expands the synonym and proceeds; if not, it simply returns the
unexpanded synonym. See also Note [Rewriting synonyms].

Where do we actually perform rewriting within a type? See Note [Rewritable] in
GHC.Tc.Solver.InertSet.

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

Note [The family application cache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To speed up the compilation of type-family heavy programs, when reducing a type
family application, we /sometimes/ store the resulting reduction in a cache
(the family application cache) so that further computations that rewrite this
same type family application directly look up the result in the cache instead of
re-deriving the reduction from scratch.

Specifically:

  - when we rewrite a type family application, and then
    recursively rewrite the RHS of this reduction, obtaining

      F tys ~> Reduction co xi

  - and [Conditions for adding to the cache] are satisfied

then 'rewriteExactFamApp' stores the reduction 'redn = Reduction co xi'
into the cache ('inert_famapp_cache'), under the cache key 'F tys'.

For example, rewriting 'Length [5,11]' is a reduction chain of two steps:

  Length [5,11]
      --> 1 + Length [11]
      --> 2

in which the nested 'Length [11]' is rewritten by a separate (recursive) call to
'rewriteExactFamApp'. We then cache:
  - the whole chain 'Length [5,11] --> 2',
  - the nested chain 'Length [11] --> 1',
  - but nothing for the intermediate step '1 + Length [11] --> 2'.

Caching the entire chain of reductions, rather than each single reduction step,
was measured to reduce the number of type family reductions in T9872{a,b,c} by ~12%.

[Conditions for adding to the cache]

  We only store a reduction in the family application cache when /all/ of the
  following conditions are satisfied:

  (FAC_Flag)    The -ffamily-application-cache flag is on (which it is by default)

    The cache is just a performance optimisation. It is controlled by a flag to
    allow A/B testing its effect, and to let users turn it off if it causes issues.

  (FAC_Role)    The ambient role is Nominal

    Type family axioms have Nominal role, and Nominal cache entries can be
    downgraded to Representational when the ambient role is Representational.

    Storing Representational reductions would require storing the role in the
    cache key, for an unclear payoff.

  (FAC_Holes)   The reduction's coercion does not contain any coercion holes

    All coercion holes used when rewriting come from Wanteds in the inert set,
    but a Wanted may be removed from the inert set (e.g. by kick out), so we
    need to be careful when caching reductions that make use of inert Wanteds.

    For example, suppose we have an inert [W] cohole :: F alpha ~ Int, and
    that we are normalising [W] G (F alpha) ~ Bool. We rewrite 'F alpha --> Int'
    using the inert and add 'F alpha ~cohole~> Int' to the fam-app cache.
    Suppose that we then unify 'alpha', kicking out the previously-inert
      [W] cohole :: F alpha ~ Int
    When we come to solving it, we see 'F alpha --> Int' in the fam-app cache
    and thus solve the Wanted with itself. Bad!

    To avoid this problem, we simply insist that the fam-app cache never
    contains any coercion holes. The alternative would be to keep the fam-app
    cache in sync, e.g. when kicking out a Wanted, drop all entries in the cache
    that mention that Wanted (and similarly for other operations that remove a
    Wanted from the inert set).

  (FAC_WorthCaching) The reduction is "worth caching".

    See Note [When is a reduction worth caching?]

Note [When is a reduction worth caching?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whether a chain of type family reductions is "worth caching" is a heuristic
tuned for compiler performance. Currently, we consider a chain of type family
reductions "worth caching" precisely when at least one step in the chain has
one of the following shapes:

  (WorthCaching1)
    We succeeded in rewriting a type family application,
    but only after rewriting one of its arguments.
  (WorthCaching2)
    We rewrote a type family application using an exactly-matching inert.

In particular, if every step in the chain reduces immediately, using a type
family equation (or a previously cached reduction) without having to rewrite any
arguments first, then we don't add to the cache. This is the most important part
here; adding to the cache in such cases can have disastrous consequences, more
than doubling the allocations for T9872a.

Examples:

  Id Int ~> Int
    not worth caching: immediately reduces using an axiom; cheap

  (1 + 2) + 4 ~> 3 + 4 ~> 7
    worth caching: we had to rewrite (1+2) ~> 3 inside the first argument
    of the outer (+) in order to make progress

  Id (2 + 3) ~> 2 + 3 ~> 5
    not worth caching: we did not need to rewrite the argument to 'Id'
    before reducing it

This heuristic isn't particularly great, it's just what we have today.
Case in point:

  data Peano = Z | S Peano

  type Length :: [k] -> Peano
  type family Length xs where
    Length '[]       = 'Z
    Length (_ ': xs) = 'S (Length xs)

A reduction chain for this type family looks like:

  Length [x1, ..., x100]
    ~> 'S (Length [x2, ..., x100])
    ~> 'S ('S (Length [x3, ..., x100]))
    ~> ...
    ~> 100 (as a Peano natural number)

At no point do we rewrite an argument to a type family (given that S is a
promoted data constructor), so we don't cache any reduction at all in this entire
chain, and we will re-run the entire computation at every occurrence of
'Length [x1, ..., x100]'.

On the other hand, consider a reduction chain for factorial:

  type family Factorial n where
    Factorial 0 = 1
    Factorial n = n * Factorial (n - 1)

  Factorial 9
    ~>
      9 * Factorial (9 - 1)
        ~> (REWRITE-RESULT)
          362880

The (*) type family cannot reduce until its second argument is rewritten,
so this chain of reductions /will/ be cached.
The result reduction proceeds by rewriting the second argument to (*),
namely 'Factorial (9 - 1) ~> 40320', which will also be cached.
-}

{-# INLINE rewrite_args_tc #-}
rewrite_args_tc
  :: TyCon         -- T
  -> Maybe (Infinite Role)  -- Nothing: ambient role is Nominal; all args are Nominal
                   -- Otherwise: no assumptions; use roles provided
  -> [Type]
  -> RewriteM ArgsReductions -- See the commentary on rewrite_args
rewrite_args_tc tc = rewrite_args all_bndrs any_named_bndrs inner_ki emptyVarSet
  -- NB: TyCon kinds are always closed
  where
  -- There are many bang patterns in here. It's been observed that they
  -- greatly improve performance of an optimized build.
  -- The T9872 test cases are good witnesses of this fact.

    (bndrs, named)
      = ty_con_binders_ty_binders' (tyConBinders tc)
    -- it's possible that the result kind has arrows (for, e.g., a type family)
    -- so we must split it
    (inner_bndrs, inner_ki, inner_named) = split_pi_tys' (tyConResKind tc)
    !all_bndrs                           = bndrs `chkAppend` inner_bndrs
    !any_named_bndrs                     = named || inner_named
    -- NB: Those bangs there drop allocations in T9872{a,c,d} by 8%.

{-# INLINE rewrite_args #-}
rewrite_args :: [PiTyBinder] -> Bool -- Binders, and True iff any of them are
                                     -- named.
             -> Kind -> TcTyCoVarSet -- function kind; kind's free vars
             -> Maybe (Infinite Role) -> [Type]    -- these are in 1-to-1 correspondence
                                          -- Nothing: use all Nominal
             -> RewriteM ArgsReductions
-- This function returns ArgsReductions (Reductions cos xis) res_co
--   coercions: co_i :: ty_i ~ xi_i, at roles given
--   types:     xi_i
--   coercion:  res_co :: typeKind(fun tys) ~N typeKind(fun xis)
-- That is, the result coercion relates the kind of some function (whose kind is
-- passed as the first parameter) instantiated at tys to the kind of that
-- function instantiated at the xis. This is useful in keeping rewriting
-- homogeneous. The list of roles must be at least as long as the list of
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
        where orig_roles = fromMaybe (Inf.repeat Nominal) orig_m_roles

{-# INLINE rewrite_args_fast #-}
-- | fast path rewrite_args, in which none of the binders are named and
-- therefore we can avoid tracking a lifting context.
rewrite_args_fast :: [Type] -> RewriteM ArgsReductions
rewrite_args_fast orig_tys
  = fmap finish (iterate orig_tys)
  where

    iterate :: [Type] -> RewriteM Reductions
    iterate (ty : tys) = do
      Reduction  co  xi  <- rewrite_one ty
      Reductions cos xis <- iterate tys
      pure $ Reductions (co : cos) (xi : xis)
    iterate [] = pure $ Reductions [] []

    {-# INLINE finish #-}
    finish :: Reductions -> ArgsReductions
    finish redns = ArgsReductions redns MRefl

{-# INLINE rewrite_args_slow #-}
-- | Slow path, compared to rewrite_args_fast, because this one must track
-- a lifting context.
rewrite_args_slow :: [PiTyBinder] -> Kind -> TcTyCoVarSet
                  -> Infinite Role -> [Type]
                  -> RewriteM ArgsReductions
rewrite_args_slow binders inner_ki fvs roles tys
  = do { rewritten_args <- zipWithM rw (Inf.toList roles) tys
       ; return $
           simplifyArgsWorker binders inner_ki fvs roles tys rewritten_args
       }
  where
    {-# INLINE rw #-}
    rw :: Role -> Type -> RewriteM Reduction
    rw Nominal ty
      = setEqRel NomEq $
        rewrite_one ty

    rw Representational ty
      = setEqRel ReprEq $
        rewrite_one ty

    rw Phantom ty
    -- See Note [Phantoms in the rewriter]
      = do { ty <- liftTcS $ zonkTcType ty
           ; return $! mkReflRedn Phantom ty }

------------------
rewrite_one :: TcType -> RewriteM Reduction
-- Rewrite a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Rewriting] for more detail.
--
-- Postcondition:
-- the role on the result coercion matches the EqRel in the RewriteEnv

rewrite_one ty
  | Just ty' <- rewriterView ty  -- See Note [Rewriting synonyms]
  = rewrite_one ty'

rewrite_one xi@(LitTy {})
  = do { role <- getRole
       ; return $! mkReflRedn role xi }

rewrite_one (TyVarTy tv)
  = rewriteTyVar tv

rewrite_one (AppTy ty1 ty2)
  = rewrite_app_tys ty1 [ty2]

rewrite_one (TyConApp tc tys)
  -- If it's a type family application, try to reduce it
  | isTypeFamilyTyCon tc
  = rewrite_fam_app tc tys

  | otherwise -- We just recursively rewrite the arguments.
              -- See Note [Do not rewrite newtypes]
  = rewrite_ty_con_app tc tys

rewrite_one (FunTy { ft_af = vis, ft_mult = mult, ft_arg = ty1, ft_res = ty2 })
  = do { arg_redn <- rewrite_one ty1
       ; res_redn <- rewrite_one ty2

        -- Important: look at the *reduced* type, so that any unzonked variables
        -- in kinds are gone and the getRuntimeRep succeeds.
        -- cf. Note [Decomposing FunTy] in GHC.Tc.Solver.Equality.
       ; let arg_rep = getRuntimeRep (reductionReducedType arg_redn)
             res_rep = getRuntimeRep (reductionReducedType res_redn)

       ; (w_redn, arg_rep_redn, res_rep_redn) <- setEqRel NomEq $
           liftA3 (,,) (rewrite_one mult)
                       (rewrite_one arg_rep)
                       (rewrite_one res_rep)
       ; role <- getRole

       ; let arg_rep_co = reductionCoercion arg_rep_redn
                -- :: arg_rep ~ arg_rep_xi
             arg_ki_co  = mkTyConAppCo Nominal tYPETyCon [arg_rep_co]
                -- :: TYPE arg_rep ~ TYPE arg_rep_xi
             casted_arg_redn = mkCoherenceRightRedn role arg_redn arg_ki_co
                -- :: ty1 ~> arg_xi |> arg_ki_co

             res_rep_co = reductionCoercion res_rep_redn
             res_ki_co  = mkTyConAppCo Nominal tYPETyCon [res_rep_co]
             casted_res_redn = mkCoherenceRightRedn role res_redn res_ki_co

          -- We must rewrite the representations, because that's what would
          -- be done if we used TyConApp instead of FunTy. These rewritten
          -- representations are seen only in casts of the arg and res, below.
          -- Forgetting this caused #19677.
       ; return $! mkFunRedn role vis w_redn casted_arg_redn casted_res_redn }

rewrite_one ty@(ForAllTy {})
-- TODO (RAE): This is inadequate, as it doesn't rewrite the kind of
-- the bound tyvar. Doing so will require carrying around a substitution
-- and the usual substTyVarBndr-like silliness. Argh.

-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (bndrs, rho) = tcSplitForAllTyVarBinders ty
       ; redn <- rewrite_one rho
       ; return $! mkHomoForAllRedn bndrs redn }

rewrite_one (CastTy ty g)
  = do { redn <- rewrite_one ty
       ; g'   <- rewrite_co g
       ; role <- getRole
       ; return $! mkCastRedn1 role ty g' redn }
      -- This calls castCoercionKind1.
      -- It makes a /big/ difference to call castCoercionKind1 not
      -- the more general castCoercionKind2.
      -- See Note [castCoercionKind1] in GHC.Core.Coercion

rewrite_one (CoercionTy co)
  = do { co' <- rewrite_co co
       ; role <- getRole
       ; return $! mkReflCoRedn role co' }

-- | "Rewrite" a coercion. Really, just zonk it so we can uphold
-- (F1) of Note [Rewriting]
rewrite_co :: Coercion -> RewriteM Coercion
rewrite_co co = liftTcS $ zonkCo co

-- | Rewrite a reduction, composing the resulting coercions.
rewrite_reduction :: Reduction -> RewriteM Reduction
rewrite_reduction (Reduction co ty)
  = do { redn <- rewrite_one ty
       ; return $! co `mkTransRedn` redn }

-- rewrite (nested) AppTys
rewrite_app_tys :: Type -> [Type] -> RewriteM Reduction
-- commoning up nested applications allows us to look up the function's kind
-- only once. Without commoning up like this, we would spend a quadratic amount
-- of time looking up functions' types
rewrite_app_tys (AppTy ty1 ty2) tys = rewrite_app_tys ty1 (ty2:tys)
rewrite_app_tys fun_ty arg_tys
  = do { redn <- rewrite_one fun_ty
       ; rewrite_app_ty_args redn arg_tys }

-- Given a rewritten function (with the coercion produced by rewriting) and
-- a bunch of unrewritten arguments, rewrite the arguments and apply.
-- The coercion argument's role matches the role stored in the RewriteM monad.
--
-- The bang patterns used here were observed to improve performance. If you
-- wish to remove them, be sure to check for regressions in allocations.
rewrite_app_ty_args :: Reduction -> [Type] -> RewriteM Reduction
rewrite_app_ty_args redn []
  -- this will be a common case when called from rewrite_fam_app, so shortcut
  = return redn
rewrite_app_ty_args fun_redn@(Reduction fun_co fun_xi) arg_tys
  = do { het_redn <- case tcSplitTyConApp_maybe fun_xi of
           Just (tc, xis) ->
             do { let tc_roles  = tyConRolesRepresentational tc
                      arg_roles = Inf.dropList xis tc_roles
                ; ArgsReductions (Reductions arg_cos arg_xis) kind_co
                    <- rewrite_vector (typeKind fun_xi) arg_roles arg_tys

                  -- We start with a reduction of the form
                  --   fun_co :: ty ~ T xi_1 ... xi_n
                  -- and further arguments a_1, ..., a_m.
                  -- We rewrite these arguments, and obtain coercions:
                  --   arg_co_i :: a_i ~ zeta_i
                  -- Now, we need to apply fun_co to the arg_cos. The problem is
                  -- that using mkAppCo is wrong because that function expects
                  -- its second coercion to be Nominal, and the arg_cos might
                  -- not be. The solution is to use transitivity:
                  -- fun_co <a_1> ... <a_m> ;; T <xi_1> .. <xi_n> arg_co_1 ... arg_co_m
                ; eq_rel <- getEqRel
                ; let app_xi = mkTyConApp tc (xis ++ arg_xis)
                      app_co = case eq_rel of
                        NomEq  -> mkAppCos fun_co arg_cos
                        ReprEq -> mkAppCos fun_co (map mkNomReflCo arg_tys)
                                  `mkTransCo`
                                  mkTyConAppCo Representational tc
                                    (zipWith mkReflCo (Inf.toList tc_roles) xis ++ arg_cos)

                ; return $
                    mkHetReduction
                      (mkReduction app_co app_xi )
                      kind_co }
           Nothing ->
             do { ArgsReductions redns kind_co
                    <- rewrite_vector (typeKind fun_xi) (Inf.repeat Nominal) arg_tys
                ; return $ mkHetReduction (mkAppRedns fun_redn redns) kind_co }

       ; role <- getRole
       ; return (homogeniseHetRedn role het_redn) }

rewrite_ty_con_app :: TyCon -> [TcType] -> RewriteM Reduction
rewrite_ty_con_app tc tys
  = do { role <- getRole
       ; let m_roles | Nominal <- role = Nothing
                     | otherwise       = Just $ tyConRolesX role tc
       ; ArgsReductions redns kind_co <- rewrite_args_tc tc m_roles tys
       ; let tyconapp_redn
                = mkHetReduction
                    (mkTyConAppRedn role tc redns)
                    kind_co
       ; return $ homogeniseHetRedn role tyconapp_redn }

-- Rewrite a vector (list of arguments).
rewrite_vector :: Kind   -- of the function being applied to these arguments
               -> Infinite Role -- If we're rewriting w.r.t. ReprEq, what roles do the
                         -- args have?
               -> [Type] -- the args to rewrite
               -> RewriteM ArgsReductions
rewrite_vector ki roles tys
  = do { eq_rel <- getEqRel
       ; let mb_roles = case eq_rel of { NomEq -> Nothing; ReprEq -> Just roles }
       ; rewrite_args bndrs any_named_bndrs inner_ki fvs mb_roles tys
       }
  where
    (bndrs, inner_ki, any_named_bndrs) = split_pi_tys' ki
    fvs                                = tyCoVarsOfType ki
{-# INLINE rewrite_vector #-}


{- Note [Do not rewrite newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We flirted with unwrapping newtypes in the rewriter -- see GHC.Tc.Solver.Equality
Note [Unwrap newtypes first]. But that turned out to be a bad idea because
of recursive newtypes, as that Note says.  So be careful if you re-add it!

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
To normalise an exactly saturated type family application 'F tys', we proceed
as follows:

  - 'rewrite_exact_fam_app' tries to reduce 'F tys'.
    If it succeeds, say reducing to 'rhs', we then recursively rewrite 'rhs'
    as it may contain further type family applications (see REWRITE-RESULT below).

  - 'rewriteExactFamApp' wraps 'rewrite_exact_fam_app' and decides what to do
    with the chain of reductions it produces: it may zap it, and it may add it
    to the family application cache.

    NB: we deliberately cache the chain of reductions as a whole instead of
    caching individual steps; see Note [The family application cache].

To reduce a single type family application, we proceed in two phases (in
'rewrite_exact_fam_app'): first try reducing the type family application without
rewriting its arguments (to avoid doing unnecessary work), and then try again
after having rewritten the arguments.

STEP 1a. Call the rewriting plugins. If any plugin rewrites the type family
application, jump to REWRITE-RESULT.

STEP 1b. Try the famapp-cache. If we get a cache hit, jump to REWRITE-RESULT.

STEP 1c. Try top-level instances. Remember: we haven't simplified the arguments
  yet. Example:
    type instance F (Maybe a) = Int
    target: F (Maybe (G Bool))
  Instead of first trying to simplify (G Bool), we use the instance first. This
  avoids the work of simplifying G Bool.

  If an instance is found, jump to REWRITE-RESULT.

STEP 2: At this point we rewrite all arguments. This might expose more
  information, which might allow plugins to make progress, or allow us to
  pick up a top-level instance.

STEP 3. Try the inerts. Note that we try the inerts *after* rewriting the
  arguments, because the inerts will have rewritten LHSs. That is, trying the
  inerts is not part of Step 1.

  If an inert is found, jump to REWRITE-RESULT.

Next, we try STEP 1 again, as we might be able to make further progress after
having rewritten the arguments:

STEP 4a (like STEP 1a). Query the rewriting plugins again.

  If any plugin supplies a rewriting, jump to REWRITE-RESULT.

STEP 4b (like STEP 1b). Try the famapp-cache again.

  If we get a cache hit, jump to REWRITE-RESULT.

STEP 4c (like STEP 1c). Try top-level instances again.

  If an instance is found, jump to REWRITE-RESULT.

STEP 5: GIVEUP. No progress to be made. Return what we have.
  (Do not REWRITE-RESULT.)

REWRITE-RESULT. The type family application reduced to 'rhs'.
  Recursively rewrite 'rhs': if it is itself a family application, go
  round the loop again from STEP 1; otherwise hand it to the general rewriter.
  Stop when there is no more work to do (or we hit the rewriter's depth limit).
  We then return the composite reduction.

Because STEP 1{a,b,c} and STEP 4{a,b,c} happen the same way, they are abstracted into
try_to_reduce.
-}

-- | Split the arguments of a (possibly oversaturated) type family application
-- into the arguments that exactly saturate the 'TyCon', and the remaining
-- arguments.
splitFamAppArgs :: TyCon -> [TcType] -> ([TcType], [TcType])
splitFamAppArgs tc tys
    -- Type functions are saturated, but might be *over* saturated,
    -- in which case the remaining arguments are dealt with by AppTys.
  = assertPpr (tys `lengthAtLeast` arity) (ppr tc $$ ppr arity $$ ppr tys) $
    splitAt arity tys
  where
    arity = tyConArity tc

-- | Rewrite a (possibly oversaturated) type family application.
rewrite_fam_app :: TyCon -> [TcType] -> RewriteM Reduction
rewrite_fam_app tc tys
  = do { let (tys1, tys_rest) = splitFamAppArgs tc tys
       ; redn <- rewriteExactFamApp tc tys1
       ; rewrite_app_ty_args redn tys_rest }

-- | Rewrite an exactly-saturated family application.
--
-- This dispatches to 'rewrite_exact_fam_app' to rewrite the family application
-- and recursively rewrite its result, handling:
--
--  - dependency tracking (Note [Computing coercion dependencies]),
--  - zapping (Note [Coercion zapping]),
--  - extending the family application cache (Note [The family application cache])
rewriteExactFamApp
  :: TyCon      -- ^ @F@
  -> [TcType]   -- ^ @tys@, exactly saturating @F@
  -> RewriteM Reduction
rewriteExactFamApp tc tys =
  do { let !fam_ty = mkTyConApp tc tys
     ; (deps, res) <- trackingDependencies $
                      rewrite_exact_fam_app fam_ty tc tys
     ; case res of
         FamAppNotReduced redn ->
            -- We only rewrote the arguments: nothing to zap nor cache.
           return redn
         FamAppReduced redn worth_caching ->
           do { -- Zap the reduction (when -fzap-coercions is on).
              ; zapped_redn <- zapRedn deps fam_ty redn

                -- See Note [The family application cache]
              ; eq_rel <- getEqRel
              ; when (  worth_caching                 -- (FAC_WorthCaching)
                     && eq_rel == NomEq               -- (FAC_Role)
                     && not (univCoDepsHasHoles deps) -- (FAC_Holes)
                     ) $
                  -- NB: (FAC_Flag) is checked in 'extendFamAppCache'.
                  liftTcS $ extendFamAppCache tc tys zapped_redn

              ; return zapped_redn
              }}

-- | Reduce an exactly-saturated type family application.
--
-- See Note [How to normalise a family application].
rewrite_exact_fam_app :: TcType -> TyCon -> [TcType] -> RewriteM FamAppRewrite
rewrite_exact_fam_app fam_ty tc tys
  = assertPpr (fam_ty `eqType` mkTyConApp tc tys)
              (ppr fam_ty $$ ppr tc $$ ppr tys) $
    do { step <- rewrite_exact_fam_app_step tc tys
       ; case step of
           -- STEP 5 (GIVEUP): the type family application did not reduce.
           FamAppNotReduced {} -> return step
           -- REWRITE-RESULT: 'F tys' rewrote to 'rhs'; now rewrite 'rhs'.
           FamAppReduced (Reduction co rhs) worth_caching
             -> do { (rest_redn, rest_worth_caching)
                       <- bumpReductionDepthRM fam_ty $ rewrite_rhs rhs
                   ; return $! FamAppReduced (co `mkTransRedn` rest_redn)
                                             (worth_caching || rest_worth_caching) } }
  where
    -- Rewrite the right-hand side of a type family reduction.
    -- Also returns whether the reduction is worth adding to the famapp cache.
    rewrite_rhs :: TcType -> RewriteM (Reduction, Bool)
    rewrite_rhs rhs
      | Just (fam_ty', tc', tys', tys_rest) <- splitTyFamApp_maybe rhs
      -- Another family application: go round the loop again.
      = do { res <- rewrite_exact_fam_app fam_ty' tc' tys'
           ; let (redn, worth_caching) = famAppRewriteRedn res
           ; redn' <- rewrite_app_ty_args redn tys_rest
           ; return (redn', worth_caching) }
      | otherwise
      = do { redn <- rewrite_one rhs
           ; return (redn, False) }

-- | Try to reduce the an exactly-saturated type family application once:
-- STEPs 1-5 of Note [How to normalise a family application].
rewrite_exact_fam_app_step :: TyCon -> [TcType] -> RewriteM FamAppRewrite
rewrite_exact_fam_app_step tc tys
  = do { -- Query the typechecking plugins for all their rewriting functions
         -- which apply to a type family application headed by the TyCon 'tc'.
       ; tc_rewriters <- getTcPluginRewritersForTyCon tc

       -- STEP 1. Try to reduce without reducing arguments first.
       ; result1 <- try_to_reduce tc tys tc_rewriters
       ; case result1 of
             -- We reduced without rewriting any arguments: not worth caching.
             -- See Note [When is a reduction worth caching?]
         { Just redn -> return $! FamAppReduced redn False
         ; Nothing ->

        -- That didn't work. So reduce the arguments, in STEP 2.
    do { eq_rel <- getEqRel
          -- checking eq_rel == NomEq saves ~0.5% in T9872a
       ; ArgsReductions (Reductions cos xis) kind_co <-
            if eq_rel == NomEq
            then rewrite_args_tc tc Nothing tys
            else setEqRel NomEq $
                 rewrite_args_tc tc Nothing tys

         -- If we manage to rewrite the type family application after
         -- rewriting the arguments, we will need to compose these
         -- reductions.
         --
         -- We have:
         --
         --   arg_co_i :: ty_i ~ xi_i
         --   fam_co :: F xi_1 ... xi_n ~ zeta
         --
         -- The full reduction is obtained as a composite:
         --
         --   full_co :: F ty_1 ... ty_n ~ zeta
         --   full_co = F co_1 ... co_n ;; fam_co
       ; let
           role    = eqRelRole eq_rel
           args_co = mkTyConAppCo role tc cos
       ;  let
            -- Compose the argument reductions with the given reduction, then homogenise
            redn_args_and_then :: Reduction -> Reduction
            redn_args_and_then redn
              = homogeniseHetRedn role
              $ mkHetReduction
                  (args_co `mkTransRedn` redn)
                  kind_co

            give_up :: Reduction
            give_up = redn_args_and_then $ mkReflRedn role reduced
              where reduced = mkTyConApp tc xis

         -- STEP 3: try the inerts
       ; flavour <- getFlavour
       ; mb_eq <- liftTcS $ lookupFamAppInert (`eqCanRewriteFR` (flavour, eq_rel)) tc xis
       ; case mb_eq of
         { Just (EqCt { eq_ev = inert_ev, eq_rhs = inert_rhs, eq_eq_rel = inert_eq_rel })
             -> do { traceRewriteM "rewrite family application with inert"
                                (ppr tc <+> ppr xis $$ ppr redn)
                   ; recordRewriter inert_ev
                     -- (WorthCaching2) in Note [When is a reduction worth caching?]
                   ; return $! FamAppReduced (redn_args_and_then downgraded_redn) True }
               -- This will sometimes duplicate an inert in the cache,
               -- but avoiding doing so had no impact on performance, and
               -- it seems easier not to weed out that special case
             where
               redn            = mkReduction (ctEvCoercion inert_ev) inert_rhs
               inert_role      = eqRelRole inert_eq_rel
               role            = eqRelRole eq_rel
               downgraded_redn = downgradeRedn role inert_role redn

         ; _ ->

         -- Inerts didn't work. Try to reduce again, in STEP 4.
    do { result3 <- try_to_reduce tc xis tc_rewriters
       ; case result3 of
           -- (WorthCaching1) in Note [When is a reduction worth caching?]
           Just redn -> return $! FamAppReduced (redn_args_and_then redn) True
           -- We have made no progress at all: STEP 5 (GIVEUP).
           _         -> return $! FamAppNotReduced give_up }}}}}

-- | The result of rewriting an exactly-saturated type family application.
--
-- See Note [How to normalise a family application].
data FamAppRewrite
  -- | The type family application was reduced.
  = FamAppReduced
      !Reduction -- ^ the type family reduction @F tys ~ rhs@
      !Bool -- ^ is this reduction worth caching?
            -- See (FAC_WorthCaching) in Note [The family application cache].

  -- | The type family application was not reduced, but we have rewritten
  -- its arguments.
  | FamAppNotReduced !Reduction

-- | The 'Reduction' obtained by rewriting a type family application, together
-- with whether it is worth recording in the family application cache.
famAppRewriteRedn :: FamAppRewrite -> (Reduction, Bool)
famAppRewriteRedn (FamAppReduced redn worth_caching) = (redn, worth_caching)
famAppRewriteRedn (FamAppNotReduced redn)            = (redn, False)

-- | Is this type a (possibly oversaturated) type family application?
--
-- Returns the exactly-saturated application, its 'TyCon', the arguments that
-- saturate it, and any remaining arguments (in case of over-saturation).
splitTyFamApp_maybe :: TcType -> Maybe (TcType, TyCon, [TcType], [TcType])
splitTyFamApp_maybe ty
  | Just ty' <- rewriterView ty  -- See Note [Rewriting synonyms]
  = splitTyFamApp_maybe ty'
  | TyConApp tc tys <- ty
  , isTypeFamilyTyCon tc
  = Just $! case splitFamAppArgs tc tys of
      (_   , [])       -> (ty                , tc, tys , [])
      (tys1, tys_rest) -> (mkTyConApp tc tys1, tc, tys1, tys_rest)
  | otherwise
  = Nothing

-- | Collect all dependent coercions used within a rewriting action.
--
-- Continues to record all inner dependencies in the outer action.
--
-- See Note [Computing coercion dependencies].
trackingDependencies :: RewriteM a -> RewriteM (UnivCoDeps, a)
trackingDependencies thing_inside = mkRewriteM $ \ env ->
  do { let ref = re_used_cos env
     ; outer_deps <- readTcRef ref
     ; unless (isEmptyUnivCoDeps outer_deps) $
         writeTcRef ref emptyUnivCoDeps
     ; res  <- runRewriteM thing_inside env
     ; deps <- readTcRef ref
       -- Restore the outer dependencies, adding on the inner ones.
     ; unless (isEmptyUnivCoDeps outer_deps) $
         writeTcRef ref $! outer_deps `unionUnivCoDeps` deps
     ; return (deps, res) }
{-# INLINE trackingDependencies #-} -- avoid allocating the return pair

{- Note [Coercion zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking programs that involve long chains of type family reductions,
the rewriter naturally produces large proof terms witnessing the reductions.

Two examples illustrate the problem:

  Example A.

    data Nat = Z | S Nat

    type F :: Nat -> Nat
    type family F n where
      F Z = Z
      F (S n) = F n

    co :: F ( S ( S ( S Z ) ) ) ~ Z
    co = AxiomCo F[1] <S(S Z)>   -- F (S(S(S Z))) ~ F (S(S Z))
       ; AxiomCo F[1] <S Z>      -- F (S(S Z))    ~ F (S Z)
       ; AxiomCo F[1] <Z>        -- F (S Z)       ~ F Z
       ; AxiomCo F[0]            -- F Z           ~ Z

    The proof is a composition of a linear number of type family reduction steps.
    However, the stored sizes also grow linearly, giving rise to a typical
    triangular shape, with a total coercion size ≈ O(n²).

  Example B.

    type (++) :: [k] -> [k] -> [k]
    type family xs ++ ys where
      '[] ++ ys = ys
      (x ': xs) ++ ys = x ': (xs ++ ys)

    co :: ([x1, x2, x3] ++ ys) ~ (x1 ': x2 ': x3 ': ys)
    co = AxiomCo (++)[1] <x1> <[x2, x3]> <ys>  -- Reduce at the top
       ; TyConAppCo (:) <k> <x1>
         ( AxiomCo (++)[1] <x2> <[x3]> <ys>    -- reduce under (x1:)
         ; TyConAppCo (:) <k> <x2>
           ( AxiomCo (++)[1] <x3> <[]> <ys>    -- reduce under (x1:x2:)
           ; TyConAppCo (:) <k> <x3>
             ( AxiomCo (++)[0] <ys> )          -- reduce under (x1:x2:x3:)
           )
         )

    This time, the proof interleaves AxiomCo and TyConAppCo. It otherwise has
    a similar shape as above, with the stored types giving rise to a total
    coercion size ≈ O(n²).

These large coercions cause significant slowdown (e.g. the simplifier needs to
traverse them when substituting). To solve this, we need a smaller
representation of these proof terms. Today, GHC does this by /coercion zapping/.

To /zap/ a coercion is to replace it with a 'UnivCo' with 'ZappedProv' provenance.
That is, we discard the proof, keeping only the LHS and RHS types it relates,
together with its dependencies (as required by
Note [The importance of tracking UnivCo dependencies] in GHC.Core.TyCo.Rep).

Specifically, zapped coercions are created in exactly one place in the compiler:

  (ZapRewrite)

    When -fzap-coercions is on (which it is by default), each time the rewriter
    produces a chain of type family reductions (see Note [How to normalise a family application]),
    we zap the coercion stored in the reduction returned by the rewriter
    (see 'rewriteExactFamApp').

    The zapped coercion thus relates the original type family application with
    its final reduct, not storing any intermediate types.

    In both Example A and Example B, we end up with a single 'UnivCo' relating
    the two endpoints, of size O(n).

    The rewriter is the right place to create zapped coercions, because of
    Wrinkle [Don't look at what you are about to zap].

    A mild bonus of storing both the LHS and RHS types is that 'coercionKind' of
    a zapped coercion is O(1), which is a boon to 'isReflexiveCo_maybe'.

We make sure to lint the coercion we are about to zap to check we have a valid
proof before we throw it away forevermore. See Note [Core Lint and zapped coercions].

Wrinkle [We also propagate zappedness]

  The rewriter is the unique birth place of zapped coercions, but the coercion
  optimiser also /propagates/ zappedness in transitive chains of coercions,
  regardless of -fzap-coercions.
  This is a small optimisation that is not at all central to the implementation
  of coercion zapping. See Note [Propagating zappedness] in GHC.Core.Coercion.Opt.

Wrinkle [Don't look at what you are about to zap]

  The 'Coercion' field of 'Reduction' is deliberately kept lazy so that we don't
  pay to build a coercion we don't need. It is /critical/ that we never so much
  as peek at the 'Coercion' we are about to zap. This is because, in practice,
  the coercions built up by the rewriter are often large trees of smart
  constructors (particularly 'mkTyConAppCo' and 'mkTransCo').
  Empirically, compiling T9872a generates about 170k calls to 'mkTyConAppCo'
  even though we end up zapping only ~1300 coercions in total. Moreover,
  forcing such trees of smart constructors can be surprisingly expensive
  (e.g. reflexivity checks).

  This is why zapping in the rewriter is a good fit: we already have in our
  hands everything we need to produce the zapped coercion, without looking at
  the coercion we are about to zap:

    - The LHS and RHS types are the type family application we started with
      and its final reduct (computed by the rewriter).

    - The dependencies are accumulated in the rewriter monad during rewriting,
      as explained in Note [Computing coercion dependencies].

  In contrast, zapping coercions after the fact (e.g. when zonking, as was
  attempted in MR !3792) requires traversing the big coercion in order to
  compute its kind and its dependencies.

  The only exception to this rule is that we do look at the coercion we are
  about to zap when running Core Lint.

Wrinkle [Zapping a coercion may increase its size]

  Perhaps surprisingly, zapping a coercion may **increase** its size, because
  a 'UnivCo' must store both its LHS and RHS types.

  For example, if we have:
    type family F a where F Int = (Int, Bool, Char, [Word], ...) -- some huge type
  then 'AxiomCo F[0] <Int>' is a much smaller coercion than
    UnivCo { uco_lty = F Int, uco_rty = (Int, Bool, Char, [Word], ...) }

  You might naively think this can be trivially solved by ad-hoc logic such as
  "if the zapped coercion is larger, don't zap", but this /does not work/:
  computing 'coercionSize' traverses the big coercion, which contravenes
  Wrinkle [Don't look at what you are about to zap]. The same goes for
  a budgeted traversal (bail out and zap once the coercion looks big).

  We don't have a convincing answer to this today. What limits the damage is
  that we are careful about which coercions we zap, only zapping entire chains
  of type family reductions in the rewriter.

  This ensures that, in the following example (taken from T3064), we zap the
  type family reduction but not the 'TyConAppCo' around it:

    -- all types stored are small
    ResourceT <[Ch]> <(s,c)> (UnivCo { uco_lty = CtxLabel (CAT s c m), uco_rty = c })

  If we propagated zappedness in 'mkTyConAppCo', we would instead end up with:

    -- very big stored types
    UnivCo { uco_lty = ResourceT [Ch] (..., CtxLabel (ResourceT ...))
           , uco_rty = ResourceT [Ch] (..., ...)
           }

  which would unacceptably regress coercion sizes.

  A more comprehensive potential solution to the problem described here is
  provided in Note [Directed coercions].

Note [Core Lint and zapped coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When zapping a coercion, we are turning a proof (checkable by Core Lint) into
a "trust me" atom.
As a belt-and-braces check, when -dcore-lint is on, we run Core Lint on the
coercion we are about to zap to ensure that the coercion we are discarding
really is a valid proof.

The checks are performed in 'checkZappedRedn' called by 'zapRedn', when
Core Lint is enabled. We check the following:

  (ZapCheck1)
    The coercion we are about to zap proves what it claims to: it has the right
    role, and its LHS (RHS) type equals the redex we are rewriting (respectively,
    the rewritten type), up to zonking.

  (ZapCheck2)
    The dependencies we are about to declare (the 'UnivCoDeps'), together with
    the LHS and RHS types of the 'UnivCo' we are about to build, cover every
    CoVar mentioned in the coercion we are about to zap.

  (ZapCheck3)
    The coercion we are about to zap is a valid coercion: it passes Core Lint.
    This makes sure e.g. that all types line up in 'TransCo' chains, etc.

    Traditionally, Core Lint only runs after typechecking is complete. To allow
    us to run Core Lint in the rewriter, on a coercion that may contain unfilled
    coercion holes and metavariables, we run a pared-down version of Core Lint
    which tolerates unfilled coercion holes, using the 'lf_tc_coercion' flag.

This checks every 'ZappedProv' 'UnivCo' at birth, in the rewriter.
Note that, even with -fno-zap-coercions, Core Lint may still encounter zapped
coercions (e.g. loaded from interface files that were produced with
-fzap-coercions). That's fine: Core Lint treats them like any other 'UnivCo'.

The coercion optimiser also discards proofs when it collapses (zapped_co ; co)
into a single zapped coercion, and performs similar checks when Core Lint is
enabled. See Wrinkle [Core Lint and propagated zappedness] in
Note [Propagating zappedness] in GHC.Core.Coercion.Opt.

Note [Computing coercion dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Two situations require us to compute the dependencies of a coercion produced by
the rewriter:

  1. When we add a reduction to the type family application cache, we must
     ensure it does not contain any coercion holes.
     See (FAC_Holes) in Note [The family application cache].

  2. When we zap a coercion, we need to populate the 'uco_deps' field of
     the 'ZappedProv' 'UnivCo' we are building.

(1) only requires us to know whether there are coercion holes or not, while (2)
requires a precise accounting of the exact set of dependencies. That is,
if we zap 'co :: lhs ~ xi' to 'UnivCo ZappedProv deps lhs xi', then 'deps' must
be exactly the dependencies of 'co', no less and no more.
See the explanation in Wrinkle [Coercion dependencies must be exact].

As explained in Wrinkle [Don't look at what you are about to zap]
in Note [Coercion zapping], we must compute 'deps' without inspecting the
coercion 'co' we are about to zap (e.g. we /must not/ call 'coVarsOfCo co').
We do so by keeping track, in the rewriter monad, of the inert equalities used
during rewriting, in the field 're_used_cos' of GHC.Tc.Types.RewriteEnv. Such
inert equalities can be used in the following ways, each recording the
dependency in 're_used_cos':

  - Directly, when the rewriter rewrites using an inert equality (either
    Given or Wanted); see 'recordRewriter'.
  - Indirectly via a reduction that was stashed in the family application cache
    and that used an inert equality; see 'lookupFamAppCache'.
  - Indirectly via a typechecker plugin, which has the obligation to declare
    dependencies on used inert equalities; see 'runTcPluginRewriters'.

The dependency tracking mechanism is scoped to individual chains of type family
reductions, in the sense of Note [How to normalise a family application].
That is, each call to 'rewriteExactFamApp' has its own dependency tracking
scope (see 'trackingDependencies'), which also means there is exactly one scope
per zapping site.

These scopes can nest: for example, we may rewrite 'F (G a)' by first rewriting
the argument 'G a', and the latter is its own chain of type family reductions,
with its own dependency tracking scope. As the reduction for 'G a' is part of
the overall reduction of 'F (G a)', everything recorded in the inner scope is
also added on to the outer scope.

The dependencies recorded in a scope tell us whether the reduction mentions any
coercion holes, as needed for (1). This is why we track dependencies even with
-fno-zap-coercions. (This solves #27518.)

When we come to zapping a coercion 'co :: lhs ~ xi' (in 'zapRedn'), we set

  uco_deps = re_used_cos ∪ shallowCoVarsOfTypes [lhs, xi]

NB: we union-in the shallow CoVars (and coercion holes) of the LHS and RHS
types to satisfy (UnivCoDepsInv) from Note [UnivCoDepsInv] in GHC.Core.TyCo.Rep.

Wrinkle [Coercion dependencies must be exact]

  When we zap 'co :: lhs ~ xi' to 'UnivCo ZappedProv deps lhs xi', we must
  certainly not under-estimate the dependencies, as explained in
  Note [The importance of tracking UnivCo dependencies] in GHC.Core.TyCo.Rep.
  So we must have:

    (NoMissingDeps)   coVarsOfCo co  ⊆  deps

  This is checked by Core Lint: see (ZapCheck2) in
  Note [Core Lint and zapped coercions].
  Perhaps more surprisingly, we in fact need the converse containment:

    (NoSpuriousDeps)       deps      ⊆ coVarsOfCo co

  This is because over-estimating the set of dependencies, while sound,
  can cause us to reject valid programs. See
  Wrinkle [Spurious dependencies can cause a program to be rejected].

  The dependency tracking scheme outlined above upholds (NoMissingDeps) because
  every coercion variable or coercion hole appearing in a coercion produced by
  the rewriter was either:

    - introduced by using an inert equality, which was recorded in one of the
      three ways listed above, or
    - already present in the type being rewritten, in which case it appears
      in 'lhs' (up to zonking), and is thus added to 'deps' by (UnivCoDepsInv).

  It is vitally important that the recording sites stay exhaustive, as missing a
  dependency risks re-introducing #23923.

  (NoSpuriousDeps) is also upheld. This is clear for the dependencies that come
  from 'lhs' and 'xi', which are the LHS and RHS types of 'co'. As for the
  recorded dependencies, every dependency recorded in a scope really is
  a dependency of the coercion zapped at the end of it:

    a. Every dependency recorded inside a scope ends up stored into a
       Reduction that is then incorporated (via some Reduction smart constructor)
       into the final Reduction returned at the end of the scope.
    b. None of the reduction combinators (mkTransRedn, mkAppRedns...) ever
       discard any part of a coercion.

  (NB: (b) is not entirely true: for example, 'mkUnivCo' may optimise a
  reflexive coercion to 'Refl', discarding any inner 'CoVar's or 'CoercionHole's.
  This infelicity is not specific to zapping, so we accept it.)

Wrinkle [Spurious dependencies can cause a program to be rejected]

  Over-estimating the dependencies may cause a spurious occurs-check failure.
  This is quite tricky to trigger, but an example is the ZapDeps test. The
  constraint solving derivation is incredibly painful to write out in full, but
  it goes roughly like this:

    [W] {hole} :: F (F x) ~ x
    [G] g1 :: FX xs ~# '[x]

    [G] g2 :: FX (FX xs) ~# xs

      ==> rewrite using g1

      [G] g3 :: FX '[x] ~# xs

      ==> type family reductions (no Givens used)

      [G] g4 :: (F x ': (FX @[Type] '[] |> γ)) ~# xs

        -- here 'γ' is made up entirely of TyConAppCo/CoAxiom/TransCo
        -- it does not contain a single CoVar (no Givens used)

      ==> re-orient to get a TyVarLHS

      [G] g5 :: xs ~# (F x ': (FX @[Type] '[] |> γ))

  Now, if, in the process of producing the kind coercion 'γ', the rewriter
  makes the mistake of recording 'g1' as a dependency of 'γ' (e.g. because
  all dependencies were recorded in a single global scope), this causes an
  occurs-check failure, because the LHS 'xs' of 'g5' occurs in 'g1'. In this
  case 'g5' becomes an IrredCt, so we can't use it to now rewrite 'g1'. This
  stops us from solving the Wanted; that would otherwise proceed as follows
  (omitting all casts for simplicity):

    [G] g1 :: FX xs ~# '[x]

      ==> rewrite using g5

      [G] g6 :: FX (F x : FX '[]) ~# '[x]

      ==> type family reductions

      [G] g7 :: F ( F x ) ': [] ~# '[x]

      ==> decomposing

      [G] g8 :: F ( F x ) ~ x

  which allows us to solve the Wanted {hole} := g8.

Graveyard: rejected ways to compute coercion dependencies.

  Alternative 1: compute dependencies using 'coVarsOfCo'.

    When zapping, we have the coercion 'co' produced by the rewriter in hand.
    We could just call 'coVarsOfCo co' to get its dependencies and store that
    in the zapped coercion.

    The problem with this approach is that it evaluates the coercion, which
    is exactly what Wrinkle [Don't look at what you are about to zap]
    in Note [Coercion zapping] warns against.

    Empirically, this approach added ~250MB allocations (~+15%) when compiling
    T9872a, and ~+35% runtime.

  Alternative 2: store the dependencies in the 'Reduction'.

    Instead of keeping track of dependencies in the rewriter monad, we could
    store them in the 'Reduction' datatype, updating the combinators to union
    these dependencies when combining coercions.

    There are two ways we could proceed here:

      2A: Store the dependencies alongside the coercion.

        This is workable, but it is a much bigger amount of code to change for
        no gain, given that this computes the exact same set of dependencies as
        the monadic approach. In particular, making sure that the extra field of
        'Reduction' does not result in extra allocations, while possible,
        requires a fair amount of delicately crafted code.

      2B: Store the dependencies, don't store the coercion at all.

        It seems wasteful for the rewriter to accumulate a big thunk for the
        coercion if we are not even going to look at it. This suggests removing
        the 'Coercion' field of 'Reduction' entirely, replacing it with a set of
        dependencies.

        However, we only want to zap chains of type family reductions, not
        arbitrary coercions. So if we tried something like:

          data Reduction = Reduction  Coercion   !Type
                         | ZappedRedn UnivCoDeps !Type

        the combinators (such as 'mkTyConAppRedn') would have to be implemented
        either by materialising intermediate UnivCos with possibly large types,
        or by infectiously propagating zappedness (which would end up zapping
        way more coercions than we bargained for and bloat coercion sizes
        as explained in Wrinkle [Zapping a coercion may increase its size]
        in Note [Coercion zapping]).

Note [Directed coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~
This note describes a **non-implemented** alternative design to solve the issues
described in Note [Coercion zapping], in particular addressing
Wrinkle [Zapping a coercion may increase its size].

As Example A and Example B from that Note show, the coercions only grow
quadratically in size because of the types stored therein.

The idea of directed coercions is to use a different coercion representation in
which we no longer store the LHS type. It becomes implicit from the context
(e.g. the input type in a reduction). Instead of

  coercionRKind :: Coercion -> Type

one gets:

  dcoercionRKind :: Role -> Type -> DCoercion -> Type
  dcoercionRKind r lhs_ty dco =
    case dco of
      DRefl -> lhs_ty
      (dco1 ; dco2) ->
        let mid_ty = dcoercionRKind r lhs_ty dco1
        in dcoercionRKind r mid_ty dco2
      ...

The benefit of this approach compared to coercion zapping is that the full proofs
are retained in Core Lint (no "trust me" 'UnivCo's), and we avoid the problem
where the zapped coercion may be larger than the original coercion.

This approach was attempted in MR !6476, but that work stalled due to
difficulties in implementing coercion optimisation (and also due to the
significant implementation cost).
-}

-- | When @-fzap-coercions@ is enabled: zap the 'Coercion' stored in the
-- 'Reduction', replacing it with a 'UnivCo' with 'ZappedProv' provenance.
--
-- See (ZapRewrite) in Note [Coercion zapping].
--
-- No-op under @-fno-zap-coercions@.
zapRedn :: UnivCoDeps -- ^ dependencies of the coercion being zapped
        -> TcType     -- ^ LHS type of the reduction
        -> Reduction
        -> RewriteM Reduction
zapRedn deps lhs redn@(Reduction _co xi) = mkRewriteM $ \env ->
  if not (re_zap_coercions env)
  then return redn
  else
  do { let
         -- See Note [Bangs in zapRedn]
         !r = eqRelRole $ re_eq_rel env

       -- When Core Lint is enabled: check the zapping is valid while we still
       -- have the proof in hand. See Note [Core Lint and zapped coercions].
     ; dflags <- getDynFlags
     ; when (gopt Opt_DoCoreLinting dflags) $
         checkZappedRedn dflags deps r lhs redn

       -- Crucially, nothing here (save for the check above) inspects
       -- the reduction's coercion at all.
       -- See Wrinkle [Don't look at what you are about to zap]
       -- in Note [Coercion zapping].
     ; return $! -- See Note [Bangs in zapRedn]
         mkReduction
           (mkUnivCoAddTypeDeps ZappedProv deps r lhs xi) xi
     }

{- Note [Bangs in zapRedn]
~~~~~~~~~~~~~~~~~~~~~~~~~~
zapRedn is on a hot path, being hammered by the rewriter when reducing type
family applications.

It's important to make it allocate as little as possible. The bangs in 'zapRedn'
were measured to shave off about 1% of allocations in T9872{a,b,c}, as they
avoid allocating intermediate thunks that would end up being forced anyway.

These banging opportunities were found with the usual strategy:
use -ddump-stg-final and hunt for naughty allocating 'let' bindings.

checkZappedRedn still ends up costing a ~0.3% increase in allocations in those
tests when Core Lint is off (so the check is skipped), but so be it.
-}

{-# NOINLINE checkZappedRedn #-} -- cold code (only for Core Lint)

-- | Check that the coercion we are about to zap is valid.
--
-- See Note [Core Lint and zapped coercions].
checkZappedRedn :: DynFlags -> UnivCoDeps -> Role -> TcType -> Reduction -> TcS ()
checkZappedRedn dflags deps r lhs_ty (Reduction co xi)
  = do { -- Zonk to handle metavariables that the rewriter looked through.
         -- See (F0) in Note [Rewriting].
         lhs_ty <- zonkTcType lhs_ty
       ; co     <- zonkCo co

       ; let (Pair co_lhs co_rhs, co_role) = coercionKindRole co

             checkEq :: Outputable a => (a -> a -> Bool) -> SDoc -> a -> a -> Maybe SDoc
             checkEq eq what expected actual
               | expected `eq` actual
               = Nothing
               | otherwise
               = Just $
                   hang (text "coercion has incorrect" <+> what)
                      2 $ vcat [ text "expected:" <+> ppr expected
                               , text "  actual:" <+> ppr actual ]

             -- The CoVars of the LHS and RHS types count as declared: they are
             -- stored in the 'UnivCo' we are about to build.
             deep_deps =
                univCoDepsDeepCoVars deps
                  `unionVarSet` coVarsOfTypes [lhs_ty, xi]
             co_cvs = coVarsOfCo co

             (_warns, lint_errs) = lintTcCoercion (initLintConfig dflags []) co

             errs =
              catMaybes
                -- (ZapCheck1): the coercion proves what we want it to prove.
                [ checkEq eqType (text "LHS")  lhs_ty co_lhs
                , checkEq eqType (text "RHS")  xi     co_rhs
                , checkEq (==)   (text "role") r      co_role

                -- (ZapCheck2): the declared dependencies cover the actual dependencies
                , if co_cvs `subVarSet` deep_deps
                  then Nothing
                  else
                    Just $
                      hang (text "unrecorded coercion dependency") 2 $
                        vcat [ text "coercion CoVars:" <+> ppr co_cvs
                             , text "declared deps:" <+> ppr deps
                             , text " missing deps:" <+> ppr (co_cvs `minusVarSet` deep_deps)
                             ]
                ]
              ++
                -- (ZapCheck3): the coercion we are zapping is valid (passes Core Lint).
                [ hang (text "ill-formed coercion")
                     2 (vcat $ bagToList lint_errs)
                | not $ isEmptyBag lint_errs ]

       ; case errs of
           []  -> return ()
           _:_ -> pprPanic "checkZappedRedn" $
                    vcat [ text "  co:" <+> ppr co
                         , text "deps:" <+> ppr deps
                         , vcat errs
                         ]
       }

-- | Try to reduce a type family application.
-- Returns a 'Reduction' whose role is the role in the 'RewriteM' monad.
--
-- See Note [How to normalise a family application]
try_to_reduce :: TyCon -> [TcType] -> [TcPluginRewriter]
              -> RewriteM (Maybe Reduction)
try_to_reduce tc tys tc_rewriters
  = do { rewrite_env <- getRewriteEnv
       ; let -- Record the dependencies of this reduction, to handle any
             -- reduction introduced by plugins or looked up in the famapp cache.
             record_deps :: Maybe Reduction -> TcS (Maybe Reduction)
             record_deps mb_redn
               | Just redn <- mb_redn
               = do { updTcRef (re_used_cos rewrite_env)
                        ( `addUnivCoDep` reductionCoercion redn )
                    ; return mb_redn }
               | otherwise
               = return mb_redn

       ; result <-
            liftTcS $ firstJustsM
              [ -- STEP 1a & STEP 4a
                record_deps
                  =<< runTcPluginRewriters rewrite_env tc_rewriters tys
              , -- STEP 1b & STEP 4b
                record_deps
                  =<< lookupFamAppCache tc tys
              , -- STEP 1c & STEP 4c
                matchFam tc tys
              ]
       ; traverse downgrade result }
  where
    -- The result above is always Nominal. We might want a Representational
    -- coercion; this downgrades (and prints, out of convenience).
    downgrade :: Reduction -> RewriteM Reduction
    downgrade redn
      = do { traceRewriteM "Eager T.F. reduction success" $
             vcat [ ppr tc
                  , ppr tys
                  , ppr redn
                  ]
           ; eq_rel <- getEqRel
              -- manually doing it this way avoids allocation in the vastly
              -- common NomEq case
           ; case eq_rel of
               NomEq  -> return redn
               ReprEq -> return $! mkSubRedn redn }

-- Retrieve all type-checking plugins that can rewrite a (saturated) type-family application
-- headed by the given 'TyCon`.
getTcPluginRewritersForTyCon :: TyCon -> RewriteM [TcPluginRewriter]
getTcPluginRewritersForTyCon tc
  = liftTcS $ do { tcg_env <- getGblEnv
                 ; plugins <- readTcRef (tcg_plugins tcg_env)
                 ; let rewriters = rewriterTcMPlugins plugins
                 ; return (lookupWithDefaultUFM rewriters [] tc) }

-- Run a collection of rewriting functions obtained from type-checking plugins,
-- querying in sequence if any plugin wants to rewrite the type family
-- applied to the given arguments.
--
-- Note that the 'TcPluginRewriter's provided all pertain to the same type family
-- (the 'TyCon' of which has been obtained ahead of calling this function).
runTcPluginRewriters :: RewriteEnv
                     -> [TcPluginRewriter]
                     -> [TcType]
                     -> TcS (Maybe Reduction)
runTcPluginRewriters rewriteEnv rewriterFunctions tys
  | null rewriterFunctions
  = return Nothing -- short-circuit for common case
  | otherwise
  = do { givens <- getInertGivens
       ; runRewriters givens rewriterFunctions }
  where
  runRewriters :: [Ct] -> [TcPluginRewriter] -> TcS (Maybe Reduction)
  runRewriters _ []
    = return Nothing
  runRewriters givens (rewriter:rewriters)
    = do
        rewriteResult <- wrapTcS . runTcPluginM $ rewriter rewriteEnv givens tys
        case rewriteResult of
           TcPluginRewriteTo
             { tcPluginReduction    = redn
             , tcRewriterNewWanteds = wanteds
             } -> do { emitWork (listToBag wanteds); return $ Just redn }
           TcPluginNoRewrite {} -> runRewriters givens rewriters

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

  | RTRFollowed !Reduction
      -- ^ The tyvar rewrites to a not-necessarily rewritten other type.
      -- The role is determined by the RewriteEnv.
      --
      -- With Quick Look, the returned TcType can be a polytype;
      -- that is, in the constraint solver, a unification variable
      -- can contain a polytype.  See GHC.Tc.Gen.App
      -- Note [Instantiation variables are short lived]

rewriteTyVar :: TyVar -> RewriteM Reduction
rewriteTyVar tv
  = do { mb_yes <- rewrite_tyvar1 tv
       ; case mb_yes of
           RTRFollowed redn -> rewrite_reduction redn

           RTRNotFollowed   -- Done, but make sure the kind is zonked
                            -- Note [Rewriting] invariant (F0) and (F1)
             -> do { tv' <- liftTcS $ updateTyVarKindM zonkTcType tv
                   ; role <- getRole
                   ; let ty' = mkTyVarTy tv'
                   ; return $! mkReflRedn role ty' } }

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
                         ; return $! RTRFollowed $!
                             mkReflRedn role ty }
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
           Just equal_ct_list
             | Just ct <- find can_rewrite equal_ct_list
             , EqCt { eq_ev = ctev, eq_lhs = TyVarLHS tv
                    , eq_rhs = rhs_ty, eq_eq_rel = ct_eq_rel } <- ct
             -> do { traceRewriteM "Following inert tyvar" $
                        vcat [ ppr tv <+> equals <+> ppr rhs_ty
                             , ppr ctev ]
                   ; recordRewriter ctev
                         -- See Note [Wanteds rewrite Wanteds: rewriter-sets]
                         -- in GHC.Tc.Types.Constraint

                   ; let rewriting_co1 = ctEvCoercion ctev
                         rewriting_co  = case (ct_eq_rel, eq_rel) of
                            (ReprEq, _rel)  -> assert (_rel == ReprEq)
                                    -- if this assert fails, then
                                    -- eqCanRewriteFR answered incorrectly
                                               rewriting_co1
                            (NomEq, NomEq)  -> rewriting_co1
                            (NomEq, ReprEq) -> mkSubCo rewriting_co1

                   ; return $! RTRFollowed $! mkReduction rewriting_co rhs_ty }

           _other -> return RTRNotFollowed }

  where
    can_rewrite :: EqCt -> Bool
    can_rewrite ct = eqCtFlavourRole ct `eqCanRewriteFR` fr
      -- This is THE key call of eqCanRewriteFR

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
It is easy to implement, in GHC.Tc.Solver.InertSet.kickOutRewritableLHS, by
only kicking out an inert only if
        (a) the work item can rewrite the inert AND
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
split_pi_tys' :: Type -> ([PiTyBinder], Type, Bool)
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
                                   in  (Anon (mkScaled w arg) af : bs, ty, named)

  split orig_ty ty | Just ty' <- coreView ty = split orig_ty ty'
  split orig_ty _                = ([], orig_ty, False)
{-# INLINE split_pi_tys' #-}

-- | Like 'tyConBindersPiTyBinders' but you also get a 'Bool' which is true iff
-- there is at least one named binder.
ty_con_binders_ty_binders' :: [TyConBinder] -> ([PiTyBinder], Bool)
ty_con_binders_ty_binders' = foldr go ([], False)
  where
    go (Bndr tv (NamedTCB vis)) (bndrs, _)
      = (Named (Bndr tv vis) : bndrs, True)
    go (Bndr tv AnonTCB)   (bndrs, n)
      = (Anon (tymult (tyVarKind tv)) FTF_T_T : bndrs, n)
    {-# INLINE go #-}
{-# INLINE ty_con_binders_ty_binders' #-}
