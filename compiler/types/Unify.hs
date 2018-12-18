-- (c) The University of Glasgow 2006

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

module Unify (
        tcMatchTy, tcMatchTyKi,
        tcMatchTys, tcMatchTyKis,
        tcMatchTyX, tcMatchTysX, tcMatchTyKisX,
        tcMatchTyX_BM, ruleMatchTyKiX,

        -- * Rough matching
        roughMatchTcs, instanceCantMatch,
        typesCantMatch,

        -- Side-effect free unification
        tcUnifyTy, tcUnifyTyKi, tcUnifyTys, tcUnifyTyKis,
        tcUnifyTysFG, tcUnifyTyWithTFs,
        BindFlag(..),
        UnifyResult, UnifyResultM(..),

        -- Matching a type against a lifted type (coercion)
        liftCoMatch
   ) where

#include "HsVersions.h"

import GhcPrelude

import Var
import VarEnv
import VarSet
import Name( Name )
import Type hiding ( getTvSubstEnv )
import Coercion hiding ( getCvSubstEnv )
import TyCon
import TyCoRep hiding ( getTvSubstEnv, getCvSubstEnv )
import FV( FV, fvVarSet, fvVarList )
import Util
import Pair
import Outputable
import UniqFM
import UniqSet

import Control.Monad
import qualified Control.Monad.Fail as MonadFail
import Control.Applicative hiding ( empty )
import qualified Control.Applicative

{-

Unification is much tricker than you might think.

1. The substitution we generate binds the *template type variables*
   which are given to us explicitly.

2. We want to match in the presence of foralls;
        e.g     (forall a. t1) ~ (forall b. t2)

   That is what the RnEnv2 is for; it does the alpha-renaming
   that makes it as if a and b were the same variable.
   Initialising the RnEnv2, so that it can generate a fresh
   binder when necessary, entails knowing the free variables of
   both types.

3. We must be careful not to bind a template type variable to a
   locally bound variable.  E.g.
        (forall a. x) ~ (forall b. b)
   where x is the template type variable.  Then we do not want to
   bind x to a/b!  This is a kind of occurs check.
   The necessary locals accumulate in the RnEnv2.

Note [tcMatchTy vs tcMatchTyKi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module offers two variants of matching: with kinds and without.
The TyKi variant takes two types, of potentially different kinds,
and matches them. Along the way, it necessarily also matches their
kinds. The Ty variant instead assumes that the kinds are already
eqType and so skips matching up the kinds.

How do you choose between them?

1. If you know that the kinds of the two types are eqType, use
   the Ty variant. It is more efficient, as it does less work.

2. If the kinds of variables in the template type might mention type families,
   use the Ty variant (and do other work to make sure the kinds
   work out). These pure unification functions do a straightforward
   syntactic unification and do no complex reasoning about type
   families. Note that the types of the variables in instances can indeed
   mention type families, so instance lookup must use the Ty variant.

   (Nothing goes terribly wrong -- no panics -- if there might be type
   families in kinds in the TyKi variant. You just might get match
   failure even though a reducing a type family would lead to success.)

3. Otherwise, if you're sure that the variable kinds do not mention
   type families and you're not already sure that the kind of the template
   equals the kind of the target, then use the TyKi version.
-}

-- | @tcMatchTy t1 t2@ produces a substitution (over fvs(t1))
-- @s@ such that @s(t1)@ equals @t2@.
-- The returned substitution might bind coercion variables,
-- if the variable is an argument to a GADT constructor.
--
-- Precondition: typeKind ty1 `eqType` typeKind ty2
--
-- We don't pass in a set of "template variables" to be bound
-- by the match, because tcMatchTy (and similar functions) are
-- always used on top-level types, so we can bind any of the
-- free variables of the LHS.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTy :: Type -> Type -> Maybe TCvSubst
tcMatchTy ty1 ty2 = tcMatchTys [ty1] [ty2]

tcMatchTyX_BM :: (TyVar -> BindFlag) -> TCvSubst
              -> Type -> Type -> Maybe TCvSubst
tcMatchTyX_BM bind_me subst ty1 ty2
  = tc_match_tys_x bind_me False subst [ty1] [ty2]

-- | Like 'tcMatchTy', but allows the kinds of the types to differ,
-- and thus matches them as well.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyKi :: Type -> Type -> Maybe TCvSubst
tcMatchTyKi ty1 ty2
  = tc_match_tys (const BindMe) True [ty1] [ty2]

-- | This is similar to 'tcMatchTy', but extends a substitution
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyX :: TCvSubst            -- ^ Substitution to extend
           -> Type                -- ^ Template
           -> Type                -- ^ Target
           -> Maybe TCvSubst
tcMatchTyX subst ty1 ty2
  = tc_match_tys_x (const BindMe) False subst [ty1] [ty2]

-- | Like 'tcMatchTy' but over a list of types.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTys :: [Type]         -- ^ Template
           -> [Type]         -- ^ Target
           -> Maybe TCvSubst -- ^ One-shot; in principle the template
                             -- variables could be free in the target
tcMatchTys tys1 tys2
  = tc_match_tys (const BindMe) False tys1 tys2

-- | Like 'tcMatchTyKi' but over a list of types.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyKis :: [Type]         -- ^ Template
             -> [Type]         -- ^ Target
             -> Maybe TCvSubst -- ^ One-shot substitution
tcMatchTyKis tys1 tys2
  = tc_match_tys (const BindMe) True tys1 tys2

-- | Like 'tcMatchTys', but extending a substitution
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTysX :: TCvSubst       -- ^ Substitution to extend
            -> [Type]         -- ^ Template
            -> [Type]         -- ^ Target
            -> Maybe TCvSubst -- ^ One-shot substitution
tcMatchTysX subst tys1 tys2
  = tc_match_tys_x (const BindMe) False subst tys1 tys2

-- | Like 'tcMatchTyKis', but extending a substitution
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyKisX :: TCvSubst        -- ^ Substitution to extend
              -> [Type]          -- ^ Template
              -> [Type]          -- ^ Target
              -> Maybe TCvSubst  -- ^ One-shot substitution
tcMatchTyKisX subst tys1 tys2
  = tc_match_tys_x (const BindMe) True subst tys1 tys2

-- | Same as tc_match_tys_x, but starts with an empty substitution
tc_match_tys :: (TyVar -> BindFlag)
               -> Bool          -- ^ match kinds?
               -> [Type]
               -> [Type]
               -> Maybe TCvSubst
tc_match_tys bind_me match_kis tys1 tys2
  = tc_match_tys_x bind_me match_kis (mkEmptyTCvSubst in_scope) tys1 tys2
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes tys1 `unionVarSet` tyCoVarsOfTypes tys2)

-- | Worker for 'tcMatchTysX' and 'tcMatchTyKisX'
tc_match_tys_x :: (TyVar -> BindFlag)
               -> Bool          -- ^ match kinds?
               -> TCvSubst
               -> [Type]
               -> [Type]
               -> Maybe TCvSubst
tc_match_tys_x bind_me match_kis (TCvSubst in_scope tv_env cv_env) tys1 tys2
  = case tc_unify_tys bind_me
                      False  -- Matching, not unifying
                      False  -- Not an injectivity check
                      match_kis
                      (mkRnEnv2 in_scope) tv_env cv_env tys1 tys2 of
      Unifiable (tv_env', cv_env')
        -> Just $ TCvSubst in_scope tv_env' cv_env'
      _ -> Nothing

-- | This one is called from the expression matcher,
-- which already has a MatchEnv in hand
ruleMatchTyKiX
  :: TyCoVarSet          -- ^ template variables
  -> RnEnv2
  -> TvSubstEnv          -- ^ type substitution to extend
  -> Type                -- ^ Template
  -> Type                -- ^ Target
  -> Maybe TvSubstEnv
ruleMatchTyKiX tmpl_tvs rn_env tenv tmpl target
-- See Note [Kind coercions in Unify]
  = case tc_unify_tys (matchBindFun tmpl_tvs) False False
                      True -- <-- this means to match the kinds
                      rn_env tenv emptyCvSubstEnv [tmpl] [target] of
      Unifiable (tenv', _) -> Just tenv'
      _                    -> Nothing

matchBindFun :: TyCoVarSet -> TyVar -> BindFlag
matchBindFun tvs tv = if tv `elemVarSet` tvs then BindMe else Skolem


{- *********************************************************************
*                                                                      *
                Rough matching
*                                                                      *
********************************************************************* -}

-- See Note [Rough match] field in InstEnv

roughMatchTcs :: [Type] -> [Maybe Name]
roughMatchTcs tys = map rough tys
  where
    rough ty
      | Just (ty', _) <- splitCastTy_maybe ty   = rough ty'
      | Just (tc,_)   <- splitTyConApp_maybe ty = Just (tyConName tc)
      | otherwise                               = Nothing

instanceCantMatch :: [Maybe Name] -> [Maybe Name] -> Bool
-- (instanceCantMatch tcs1 tcs2) returns True if tcs1 cannot
-- possibly be instantiated to actual, nor vice versa;
-- False is non-committal
instanceCantMatch (mt : ts) (ma : as) = itemCantMatch mt ma || instanceCantMatch ts as
instanceCantMatch _         _         =  False  -- Safe

itemCantMatch :: Maybe Name -> Maybe Name -> Bool
itemCantMatch (Just t) (Just a) = t /= a
itemCantMatch _        _        = False


{-
************************************************************************
*                                                                      *
                GADTs
*                                                                      *
************************************************************************

Note [Pruning dead case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider        data T a where
                   T1 :: T Int
                   T2 :: T a

                newtype X = MkX Int
                newtype Y = MkY Char

                type family F a
                type instance F Bool = Int

Now consider    case x of { T1 -> e1; T2 -> e2 }

The question before the house is this: if I know something about the type
of x, can I prune away the T1 alternative?

Suppose x::T Char.  It's impossible to construct a (T Char) using T1,
        Answer = YES we can prune the T1 branch (clearly)

Suppose x::T (F a), where 'a' is in scope.  Then 'a' might be instantiated
to 'Bool', in which case x::T Int, so
        ANSWER = NO (clearly)

We see here that we want precisely the apartness check implemented within
tcUnifyTysFG. So that's what we do! Two types cannot match if they are surely
apart. Note that since we are simply dropping dead code, a conservative test
suffices.
-}

-- | Given a list of pairs of types, are any two members of a pair surely
-- apart, even after arbitrary type function evaluation and substitution?
typesCantMatch :: [(Type,Type)] -> Bool
-- See Note [Pruning dead case alternatives]
typesCantMatch prs = any (uncurry cant_match) prs
  where
    cant_match :: Type -> Type -> Bool
    cant_match t1 t2 = case tcUnifyTysFG (const BindMe) [t1] [t2] of
      SurelyApart -> True
      _           -> False

{-
************************************************************************
*                                                                      *
             Unification
*                                                                      *
************************************************************************

Note [Fine-grained unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do the types (x, x) and ([y], y) unify? The answer is seemingly "no" --
no substitution to finite types makes these match. But, a substitution to
*infinite* types can unify these two types: [x |-> [[[...]]], y |-> [[[...]]] ].
Why do we care? Consider these two type family instances:

type instance F x x   = Int
type instance F [y] y = Bool

If we also have

type instance Looper = [Looper]

then the instances potentially overlap. The solution is to use unification
over infinite terms. This is possible (see [1] for lots of gory details), but
a full algorithm is a little more power than we need. Instead, we make a
conservative approximation and just omit the occurs check.

[1]: http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf

tcUnifyTys considers an occurs-check problem as the same as general unification
failure.

tcUnifyTysFG ("fine-grained") returns one of three results: success, occurs-check
failure ("MaybeApart"), or general failure ("SurelyApart").

See also Trac #8162.

It's worth noting that unification in the presence of infinite types is not
complete. This means that, sometimes, a closed type family does not reduce
when it should. See test case indexed-types/should_fail/Overlap15 for an
example.

Note [The substitution in MaybeApart]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The constructor MaybeApart carries data with it, typically a TvSubstEnv. Why?
Because consider unifying these:

(a, a, Int) ~ (b, [b], Bool)

If we go left-to-right, we start with [a |-> b]. Then, on the middle terms, we
apply the subst we have so far and discover that we need [b |-> [b]]. Because
this fails the occurs check, we say that the types are MaybeApart (see above
Note [Fine-grained unification]). But, we can't stop there! Because if we
continue, we discover that Int is SurelyApart from Bool, and therefore the
types are apart. This has practical consequences for the ability for closed
type family applications to reduce. See test case
indexed-types/should_compile/Overlap14.

Note [Unifying with skolems]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we discover that two types unify if and only if a skolem variable is
substituted, we can't properly unify the types. But, that skolem variable
may later be instantiated with a unifyable type. So, we return maybeApart
in these cases.
-}

-- | Simple unification of two types; all type variables are bindable
-- Precondition: the kinds are already equal
tcUnifyTy :: Type -> Type       -- All tyvars are bindable
          -> Maybe TCvSubst
                       -- A regular one-shot (idempotent) substitution
tcUnifyTy t1 t2 = tcUnifyTys (const BindMe) [t1] [t2]

-- | Like 'tcUnifyTy', but also unifies the kinds
tcUnifyTyKi :: Type -> Type -> Maybe TCvSubst
tcUnifyTyKi t1 t2 = tcUnifyTyKis (const BindMe) [t1] [t2]

-- | Unify two types, treating type family applications as possibly unifying
-- with anything and looking through injective type family applications.
-- Precondition: kinds are the same
tcUnifyTyWithTFs :: Bool  -- ^ True <=> do two-way unification;
                          --   False <=> do one-way matching.
                          --   See end of sec 5.2 from the paper
                 -> Type -> Type -> Maybe TCvSubst
-- This algorithm is an implementation of the "Algorithm U" presented in
-- the paper "Injective type families for Haskell", Figures 2 and 3.
-- The code is incorporated with the standard unifier for convenience, but
-- its operation should match the specification in the paper.
tcUnifyTyWithTFs twoWay t1 t2
  = case tc_unify_tys (const BindMe) twoWay True False
                       rn_env emptyTvSubstEnv emptyCvSubstEnv
                       [t1] [t2] of
      Unifiable  (subst, _) -> Just $ niFixTCvSubst subst
      MaybeApart (subst, _) -> Just $ niFixTCvSubst subst
      -- we want to *succeed* in questionable cases. This is a
      -- pre-unification algorithm.
      SurelyApart      -> Nothing
  where
    rn_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [t1, t2]

-----------------
tcUnifyTys :: (TyCoVar -> BindFlag)
           -> [Type] -> [Type]
           -> Maybe TCvSubst
                                -- ^ A regular one-shot (idempotent) substitution
                                -- that unifies the erased types. See comments
                                -- for 'tcUnifyTysFG'

-- The two types may have common type variables, and indeed do so in the
-- second call to tcUnifyTys in FunDeps.checkClsFD
tcUnifyTys bind_fn tys1 tys2
  = case tcUnifyTysFG bind_fn tys1 tys2 of
      Unifiable result -> Just result
      _                -> Nothing

-- | Like 'tcUnifyTys' but also unifies the kinds
tcUnifyTyKis :: (TyCoVar -> BindFlag)
             -> [Type] -> [Type]
             -> Maybe TCvSubst
tcUnifyTyKis bind_fn tys1 tys2
  = case tcUnifyTyKisFG bind_fn tys1 tys2 of
      Unifiable result -> Just result
      _                -> Nothing

-- This type does double-duty. It is used in the UM (unifier monad) and to
-- return the final result. See Note [Fine-grained unification]
type UnifyResult = UnifyResultM TCvSubst
data UnifyResultM a = Unifiable a        -- the subst that unifies the types
                    | MaybeApart a       -- the subst has as much as we know
                                         -- it must be part of a most general unifier
                                         -- See Note [The substitution in MaybeApart]
                    | SurelyApart
                    deriving Functor

instance Applicative UnifyResultM where
  pure  = Unifiable
  (<*>) = ap

instance Monad UnifyResultM where

  SurelyApart  >>= _ = SurelyApart
  MaybeApart x >>= f = case f x of
                         Unifiable y -> MaybeApart y
                         other       -> other
  Unifiable x  >>= f = f x

instance Alternative UnifyResultM where
  empty = SurelyApart

  a@(Unifiable {})  <|> _                 = a
  _                 <|> b@(Unifiable {})  = b
  a@(MaybeApart {}) <|> _                 = a
  _                 <|> b@(MaybeApart {}) = b
  SurelyApart       <|> SurelyApart       = SurelyApart

instance MonadPlus UnifyResultM

-- | @tcUnifyTysFG bind_tv tys1 tys2@ attepts to find a substitution @s@ (whose
-- domain elements all respond 'BindMe' to @bind_tv@) such that
-- @s(tys1)@ and that of @s(tys2)@ are equal, as witnessed by the returned
-- Coercions. This version requires that the kinds of the types are the same,
-- if you unify left-to-right.
tcUnifyTysFG :: (TyVar -> BindFlag)
             -> [Type] -> [Type]
             -> UnifyResult
tcUnifyTysFG bind_fn tys1 tys2
  = tc_unify_tys_fg False bind_fn tys1 tys2

tcUnifyTyKisFG :: (TyVar -> BindFlag)
               -> [Type] -> [Type]
               -> UnifyResult
tcUnifyTyKisFG bind_fn tys1 tys2
  = tc_unify_tys_fg True bind_fn tys1 tys2

tc_unify_tys_fg :: Bool
                -> (TyVar -> BindFlag)
                -> [Type] -> [Type]
                -> UnifyResult
tc_unify_tys_fg match_kis bind_fn tys1 tys2
  = do { (env, _) <- tc_unify_tys bind_fn True False match_kis env
                                  emptyTvSubstEnv emptyCvSubstEnv
                                  tys1 tys2
       ; return $ niFixTCvSubst env }
  where
    vars = tyCoVarsOfTypes tys1 `unionVarSet` tyCoVarsOfTypes tys2
    env  = mkRnEnv2 $ mkInScopeSet vars

-- | This function is actually the one to call the unifier -- a little
-- too general for outside clients, though.
tc_unify_tys :: (TyVar -> BindFlag)
             -> AmIUnifying -- ^ True <=> unify; False <=> match
             -> Bool        -- ^ True <=> doing an injectivity check
             -> Bool        -- ^ True <=> treat the kinds as well
             -> RnEnv2
             -> TvSubstEnv  -- ^ substitution to extend
             -> CvSubstEnv
             -> [Type] -> [Type]
             -> UnifyResultM (TvSubstEnv, CvSubstEnv)
-- NB: It's tempting to ASSERT here that, if we're not matching kinds, then
-- the kinds of the types should be the same. However, this doesn't work,
-- as the types may be a dependent telescope, where later types have kinds
-- that mention variables occurring earlier in the list of types. Here's an
-- example (from typecheck/should_fail/T12709):
--   template: [rep :: RuntimeRep,       a :: TYPE rep]
--   target:   [LiftedRep :: RuntimeRep, Int :: TYPE LiftedRep]
-- We can see that matching the first pair will make the kinds of the second
-- pair equal. Yet, we still don't need a separate pass to unify the kinds
-- of these types, so it's appropriate to use the Ty variant of unification.
-- See also Note [tcMatchTy vs tcMatchTyKi].
tc_unify_tys bind_fn unif inj_check match_kis rn_env tv_env cv_env tys1 tys2
  = initUM tv_env cv_env $
    do { when match_kis $
         unify_tys env kis1 kis2
       ; unify_tys env tys1 tys2
       ; (,) <$> getTvSubstEnv <*> getCvSubstEnv }
  where
    env = UMEnv { um_bind_fun = bind_fn
                , um_skols    = emptyVarSet
                , um_unif     = unif
                , um_inj_tf   = inj_check
                , um_rn_env   = rn_env }

    kis1 = map typeKind tys1
    kis2 = map typeKind tys2

instance Outputable a => Outputable (UnifyResultM a) where
  ppr SurelyApart    = text "SurelyApart"
  ppr (Unifiable x)  = text "Unifiable" <+> ppr x
  ppr (MaybeApart x) = text "MaybeApart" <+> ppr x

{-
************************************************************************
*                                                                      *
                Non-idempotent substitution
*                                                                      *
************************************************************************

Note [Non-idempotent substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During unification we use a TvSubstEnv/CvSubstEnv pair that is
  (a) non-idempotent
  (b) loop-free; ie repeatedly applying it yields a fixed point

Note [Finding the substitution fixpoint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finding the fixpoint of a non-idempotent substitution arising from a
unification is much trickier than it looks, because of kinds.  Consider
   T k (H k (f:k)) ~ T * (g:*)
If we unify, we get the substitution
   [ k -> *
   , g -> H k (f:k) ]
To make it idempotent we don't want to get just
   [ k -> *
   , g -> H * (f:k) ]
We also want to substitute inside f's kind, to get
   [ k -> *
   , g -> H k (f:*) ]
If we don't do this, we may apply the substitution to something,
and get an ill-formed type, i.e. one where typeKind will fail.
This happened, for example, in Trac #9106.

It gets worse.  In Trac #14164 we wanted to take the fixpoint of
this substitution
   [ xs_asV :-> F a_aY6 (z_aY7 :: a_aY6)
                        (rest_aWF :: G a_aY6 (z_aY7 :: a_aY6))
   , a_aY6  :-> a_aXQ ]

We have to apply the substitution for a_aY6 two levels deep inside
the invocation of F!  We don't have a function that recursively
applies substitutions inside the kinds of variable occurrences (and
probably rightly so).

So, we work as follows:

 1. Start with the current substitution (which we are
    trying to fixpoint
       [ xs :-> F a (z :: a) (rest :: G a (z :: a))
       , a  :-> b ]

 2. Take all the free vars of the range of the substitution:
       {a, z, rest, b}
    NB: the free variable finder closes over
    the kinds of variable occurrences

 3. If none are in the domain of the substitution, stop.
    We have found a fixpoint.

 4. Remove the variables that are bound by the substitution, leaving
       {z, rest, b}

 5. Do a topo-sort to put them in dependency order:
       [ b :: *, z :: a, rest :: G a z ]

 6. Apply the substitution left-to-right to the kinds of these
    tyvars, extending it each time with a new binding, so we
    finish up with
       [ xs   :-> ..as before..
       , a    :-> b
       , b    :-> b    :: *
       , z    :-> z    :: b
       , rest :-> rest :: G b (z :: b) ]
    Note that rest now has the right kind

 7. Apply this extended substitution (once) to the range of
    the /original/ substitution.  (Note that we do the
    extended substitution would go on forever if you tried
    to find its fixpoint, because it maps z to z.)

 8. And go back to step 1

In Step 6 we use the free vars from Step 2 as the initial
in-scope set, because all of those variables appear in the
range of the substitution, so they must all be in the in-scope
set.  But NB that the type substitution engine does not look up
variables in the in-scope set; it is used only to ensure no
shadowing.
-}

niFixTCvSubst :: TvSubstEnv -> TCvSubst
-- Find the idempotent fixed point of the non-idempotent substitution
-- This is surprisingly tricky:
--   see Note [Finding the substitution fixpoint]
-- ToDo: use laziness instead of iteration?
niFixTCvSubst tenv
  | not_fixpoint = niFixTCvSubst (mapVarEnv (substTy subst) tenv)
  | otherwise    = subst
  where
    range_fvs :: FV
    range_fvs = tyCoFVsOfTypes (nonDetEltsUFM tenv)
          -- It's OK to use nonDetEltsUFM here because the
          -- order of range_fvs, range_tvs is immaterial

    range_tvs :: [TyVar]
    range_tvs = fvVarList range_fvs

    not_fixpoint  = any in_domain range_tvs
    in_domain tv  = tv `elemVarEnv` tenv

    free_tvs = scopedSort (filterOut in_domain range_tvs)

    -- See Note [Finding the substitution fixpoint], Step 6
    init_in_scope = mkInScopeSet (fvVarSet range_fvs)
    subst = foldl' add_free_tv
                  (mkTvSubst init_in_scope tenv)
                  free_tvs

    add_free_tv :: TCvSubst -> TyVar -> TCvSubst
    add_free_tv subst tv
      = extendTvSubst subst tv (mkTyVarTy tv')
     where
        tv' = updateTyVarKind (substTy subst) tv

niSubstTvSet :: TvSubstEnv -> TyCoVarSet -> TyCoVarSet
-- Apply the non-idempotent substitution to a set of type variables,
-- remembering that the substitution isn't necessarily idempotent
-- This is used in the occurs check, before extending the substitution
niSubstTvSet tsubst tvs
  = nonDetFoldUniqSet (unionVarSet . get) emptyVarSet tvs
  -- It's OK to nonDetFoldUFM here because we immediately forget the
  -- ordering by creating a set.
  where
    get tv
      | Just ty <- lookupVarEnv tsubst tv
      = niSubstTvSet tsubst (tyCoVarsOfType ty)

      | otherwise
      = unitVarSet tv

{-
************************************************************************
*                                                                      *
                unify_ty: the main workhorse
*                                                                      *
************************************************************************

Note [Specification of unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The pure unifier, unify_ty, defined in this module, tries to work out
a substitution to make two types say True to eqType. NB: eqType is
itself not purely syntactic; it accounts for CastTys;
see Note [Non-trivial definitional equality] in TyCoRep

Unlike the "impure unifiers" in the typechecker (the eager unifier in
TcUnify, and the constraint solver itself in TcCanonical), the pure
unifier It does /not/ work up to ~.

The algorithm implemented here is rather delicate, and we depend on it
to uphold certain properties. This is a summary of these required
properties. Any reference to "flattening" refers to the flattening
algorithm in FamInstEnv (See Note [Flattening] in FamInstEnv), not
the flattening algorithm in the solver.

Notation:
 θ,φ    substitutions
 ξ    type-function-free types
 τ,σ  other types
 τ♭   type τ, flattened

 ≡    eqType

(U1) Soundness.
     If (unify τ₁ τ₂) = Unifiable θ, then θ(τ₁) ≡ θ(τ₂).
     θ is a most general unifier for τ₁ and τ₂.

(U2) Completeness.
     If (unify ξ₁ ξ₂) = SurelyApart,
     then there exists no substitution θ such that θ(ξ₁) ≡ θ(ξ₂).

These two properties are stated as Property 11 in the "Closed Type Families"
paper (POPL'14). Below, this paper is called [CTF].

(U3) Apartness under substitution.
     If (unify ξ τ♭) = SurelyApart, then (unify ξ θ(τ)♭) = SurelyApart,
     for any θ. (Property 12 from [CTF])

(U4) Apart types do not unify.
     If (unify ξ τ♭) = SurelyApart, then there exists no θ
     such that θ(ξ) = θ(τ). (Property 13 from [CTF])

THEOREM. Completeness w.r.t ~
    If (unify τ₁♭ τ₂♭) = SurelyApart,
    then there exists no proof that (τ₁ ~ τ₂).

PROOF. See appendix of [CTF].


The unification algorithm is used for type family injectivity, as described
in the "Injective Type Families" paper (Haskell'15), called [ITF]. When run
in this mode, it has the following properties.

(I1) If (unify σ τ) = SurelyApart, then σ and τ are not unifiable, even
     after arbitrary type family reductions. Note that σ and τ are
     not flattened here.

(I2) If (unify σ τ) = MaybeApart θ, and if some
     φ exists such that φ(σ) ~ φ(τ), then φ extends θ.


Furthermore, the RULES matching algorithm requires this property,
but only when using this algorithm for matching:

(M1) If (match σ τ) succeeds with θ, then all matchable tyvars
     in σ are bound in θ.

     Property M1 means that we must extend the substitution with,
     say (a ↦ a) when appropriate during matching.
     See also Note [Self-substitution when matching].

(M2) Completeness of matching.
     If θ(σ) = τ, then (match σ τ) = Unifiable φ,
     where θ is an extension of φ.

Sadly, property M2 and I2 conflict. Consider

type family F1 a b where
  F1 Int    Bool   = Char
  F1 Double String = Char

Consider now two matching problems:

P1. match (F1 a Bool) (F1 Int Bool)
P2. match (F1 a Bool) (F1 Double String)

In case P1, we must find (a ↦ Int) to satisfy M2.
In case P2, we must /not/ find (a ↦ Double), in order to satisfy I2. (Note
that the correct mapping for I2 is (a ↦ Int). There is no way to discover
this, but we musn't map a to anything else!)

We thus must parameterize the algorithm over whether it's being used
for an injectivity check (refrain from looking at non-injective arguments
to type families) or not (do indeed look at those arguments).  This is
implemented  by the uf_inj_tf field of UmEnv.

(It's all a question of whether or not to include equation (7) from Fig. 2
of [ITF].)

This extra parameter is a bit fiddly, perhaps, but seemingly less so than
having two separate, almost-identical algorithms.

Note [Self-substitution when matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should happen when we're *matching* (not unifying) a1 with a1? We
should get a substitution [a1 |-> a1]. A successful match should map all
the template variables (except ones that disappear when expanding synonyms).
But when unifying, we don't want to do this, because we'll then fall into
a loop.

This arrangement affects the code in three places:
 - If we're matching a refined template variable, don't recur. Instead, just
   check for equality. That is, if we know [a |-> Maybe a] and are matching
   (a ~? Maybe Int), we want to just fail.

 - Skip the occurs check when matching. This comes up in two places, because
   matching against variables is handled separately from matching against
   full-on types.

Note that this arrangement was provoked by a real failure, where the same
unique ended up in the template as in the target. (It was a rule firing when
compiling Data.List.NonEmpty.)

Note [Matching coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

   type family F a

   data G a where
     MkG :: F a ~ Bool => G a

   type family Foo (x :: G a) :: F a
   type instance Foo MkG = False

We would like that to be accepted. For that to work, we need to introduce
a coercion variable on the left and then use it on the right. Accordingly,
at use sites of Foo, we need to be able to use matching to figure out the
value for the coercion. (See the desugared version:

   axFoo :: [a :: *, c :: F a ~ Bool]. Foo (MkG c) = False |> (sym c)

) We never want this action to happen during *unification* though, when
all bets are off.

Note [Kind coercions in Unify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We wish to match/unify while ignoring casts. But, we can't just ignore
them completely, or we'll end up with ill-kinded substitutions. For example,
say we're matching `a` with `ty |> co`. If we just drop the cast, we'll
return [a |-> ty], but `a` and `ty` might have different kinds. We can't
just match/unify their kinds, either, because this might gratuitously
fail. After all, `co` is the witness that the kinds are the same -- they
may look nothing alike.

So, we pass a kind coercion to the match/unify worker. This coercion witnesses
the equality between the substed kind of the left-hand type and the substed
kind of the right-hand type. Note that we do not unify kinds at the leaves
(as we did previously). We thus have

INVARIANT: In the call
    unify_ty ty1 ty2 kco
it must be that subst(kco) :: subst(kind(ty1)) ~N subst(kind(ty2)), where
`subst` is the ambient substitution in the UM monad.

To get this coercion, we first have to match/unify
the kinds before looking at the types. Happily, we need look only one level
up, as all kinds are guaranteed to have kind *.

When we're working with type applications (either TyConApp or AppTy) we
need to worry about establishing INVARIANT, as the kinds of the function
& arguments aren't (necessarily) included in the kind of the result.
When unifying two TyConApps, this is easy, because the two TyCons are
the same. Their kinds are thus the same. As long as we unify left-to-right,
we'll be sure to unify types' kinds before the types themselves. (For example,
think about Proxy :: forall k. k -> *. Unifying the first args matches up
the kinds of the second args.)

For AppTy, we must unify the kinds of the functions, but once these are
unified, we can continue unifying arguments without worrying further about
kinds.

The interface to this module includes both "...Ty" functions and
"...TyKi" functions. The former assume that INVARIANT is already
established, either because the kinds are the same or because the
list of types being passed in are the well-typed arguments to some
type constructor (see two paragraphs above). The latter take a separate
pre-pass over the kinds to establish INVARIANT. Sometimes, it's important
not to take the second pass, as it caused #12442.

We thought, at one point, that this was all unnecessary: why should
casts be in types in the first place? But they are sometimes. In
dependent/should_compile/KindEqualities2, we see, for example the
constraint Num (Int |> (blah ; sym blah)).  We naturally want to find
a dictionary for that constraint, which requires dealing with
coercions in this manner.

Note [Matching in the presence of casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When matching, it is crucial that no variables from the template
end up in the range of the matching substitution (obviously!).
When unifying, that's not a constraint; instead we take the fixpoint
of the substitution at the end.

So what should we do with this, when matching?
   unify_ty (tmpl |> co) tgt kco

Previously, wrongly, we pushed 'co' in the (horrid) accumulating
'kco' argument like this:
   unify_ty (tmpl |> co) tgt kco
     = unify_ty tmpl tgt (kco ; co)

But that is obviously wrong because 'co' (from the template) ends
up in 'kco', which in turn ends up in the range of the substitution.

This all came up in Trac #13910.  Because we match tycon arguments
left-to-right, the ambient substitution will already have a matching
substitution for any kinds; so there is an easy fix: just apply
the substitution-so-far to the coercion from the LHS.

Note that

* When matching, the first arg of unify_ty is always the template;
  we never swap round.

* The above argument is distressingly indirect. We seek a
  better way.

* One better way is to ensure that type patterns (the template
  in the matching process) have no casts.  See Trac #14119.

Note [Polykinded tycon applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose  T :: forall k. Type -> K
and we are unifying
  ty1:  T @Type         Int       :: Type
  ty2:  T @(Type->Type) Int Int   :: Type

These two TyConApps have the same TyCon at the front but they
(legitimately) have different numbers of arguments.  They
are surelyApart, so we can report that without looking any
further (see Trac #15704).
-}

-------------- unify_ty: the main workhorse -----------

type AmIUnifying = Bool   -- True  <=> Unifying
                          -- False <=> Matching

unify_ty :: UMEnv
         -> Type -> Type  -- Types to be unified and a co
         -> CoercionN     -- A coercion between their kinds
                          -- See Note [Kind coercions in Unify]
         -> UM ()
-- See Note [Specification of unification]
-- Respects newtypes, PredTypes

unify_ty env ty1 ty2 kco
    -- TODO: More commentary needed here
  | Just ty1' <- tcView ty1   = unify_ty env ty1' ty2 kco
  | Just ty2' <- tcView ty2   = unify_ty env ty1 ty2' kco
  | CastTy ty1' co <- ty1     = if um_unif env
                                then unify_ty env ty1' ty2 (co `mkTransCo` kco)
                                else -- See Note [Matching in the presence of casts]
                                     do { subst <- getSubst env
                                        ; let co' = substCo subst co
                                        ; unify_ty env ty1' ty2 (co' `mkTransCo` kco) }
  | CastTy ty2' co <- ty2     = unify_ty env ty1 ty2' (kco `mkTransCo` mkSymCo co)

unify_ty env (TyVarTy tv1) ty2 kco
  = uVar env tv1 ty2 kco
unify_ty env ty1 (TyVarTy tv2) kco
  | um_unif env  -- If unifying, can swap args
  = uVar (umSwapRn env) tv2 ty1 (mkSymCo kco)

unify_ty env ty1 ty2 _kco
  | Just (tc1, tys1) <- mb_tc_app1
  , Just (tc2, tys2) <- mb_tc_app2
  , tc1 == tc2 || (tcIsLiftedTypeKind ty1 && tcIsLiftedTypeKind ty2)
  = if isInjectiveTyCon tc1 Nominal
    then unify_tys env tys1 tys2
    else do { let inj | isTypeFamilyTyCon tc1
                      = case tyConInjectivityInfo tc1 of
                               NotInjective -> repeat False
                               Injective bs -> bs
                      | otherwise
                      = repeat False

                  (inj_tys1, noninj_tys1) = partitionByList inj tys1
                  (inj_tys2, noninj_tys2) = partitionByList inj tys2

            ; unify_tys env inj_tys1 inj_tys2
            ; unless (um_inj_tf env) $ -- See (end of) Note [Specification of unification]
              don'tBeSoSure $ unify_tys env noninj_tys1 noninj_tys2 }

  | Just (tc1, _) <- mb_tc_app1
  , not (isGenerativeTyCon tc1 Nominal)
    -- E.g.   unify_ty (F ty1) b  =  MaybeApart
    --        because the (F ty1) behaves like a variable
    --        NB: if unifying, we have already dealt
    --            with the 'ty2 = variable' case
  = maybeApart

  | Just (tc2, _) <- mb_tc_app2
  , not (isGenerativeTyCon tc2 Nominal)
  , um_unif env
    -- E.g.   unify_ty [a] (F ty2) =  MaybeApart, when unifying (only)
    --        because the (F ty2) behaves like a variable
    --        NB: we have already dealt with the 'ty1 = variable' case
  = maybeApart

  where
    mb_tc_app1 = tcSplitTyConApp_maybe ty1
    mb_tc_app2 = tcSplitTyConApp_maybe ty2

        -- Applications need a bit of care!
        -- They can match FunTy and TyConApp, so use splitAppTy_maybe
        -- NB: we've already dealt with type variables,
        -- so if one type is an App the other one jolly well better be too
unify_ty env (AppTy ty1a ty1b) ty2 _kco
  | Just (ty2a, ty2b) <- tcRepSplitAppTy_maybe ty2
  = unify_ty_app env ty1a [ty1b] ty2a [ty2b]

unify_ty env ty1 (AppTy ty2a ty2b) _kco
  | Just (ty1a, ty1b) <- tcRepSplitAppTy_maybe ty1
  = unify_ty_app env ty1a [ty1b] ty2a [ty2b]

unify_ty _ (LitTy x) (LitTy y) _kco | x == y = return ()

unify_ty env (ForAllTy (Bndr tv1 _) ty1) (ForAllTy (Bndr tv2 _) ty2) kco
  = do { unify_ty env (varType tv1) (varType tv2) (mkNomReflCo liftedTypeKind)
       ; let env' = umRnBndr2 env tv1 tv2
       ; unify_ty env' ty1 ty2 kco }

-- See Note [Matching coercion variables]
unify_ty env (CoercionTy co1) (CoercionTy co2) kco
  = do { c_subst <- getCvSubstEnv
       ; case co1 of
           CoVarCo cv
             | not (um_unif env)
             , not (cv `elemVarEnv` c_subst)
             , BindMe <- tvBindFlag env cv
             -> do { checkRnEnv env (tyCoVarsOfCo co2)
                   ; let (co_l, co_r) = decomposeFunCo Nominal kco
                      -- cv :: t1 ~ t2
                      -- co2 :: s1 ~ s2
                      -- co_l :: t1 ~ s1
                      -- co_r :: t2 ~ s2
                   ; extendCvEnv cv (co_l `mkTransCo`
                                     co2 `mkTransCo`
                                     mkSymCo co_r) }
           _ -> return () }

unify_ty _ _ _ _ = surelyApart

unify_ty_app :: UMEnv -> Type -> [Type] -> Type -> [Type] -> UM ()
unify_ty_app env ty1 ty1args ty2 ty2args
  | Just (ty1', ty1a) <- repSplitAppTy_maybe ty1
  , Just (ty2', ty2a) <- repSplitAppTy_maybe ty2
  = unify_ty_app env ty1' (ty1a : ty1args) ty2' (ty2a : ty2args)

  | otherwise
  = do { let ki1 = typeKind ty1
             ki2 = typeKind ty2
           -- See Note [Kind coercions in Unify]
       ; unify_ty  env ki1 ki2 (mkNomReflCo liftedTypeKind)
       ; unify_ty  env ty1 ty2 (mkNomReflCo ki1)
       ; unify_tys env ty1args ty2args }

unify_tys :: UMEnv -> [Type] -> [Type] -> UM ()
unify_tys env orig_xs orig_ys
  = go orig_xs orig_ys
  where
    go []     []     = return ()
    go (x:xs) (y:ys)
      -- See Note [Kind coercions in Unify]
      = do { unify_ty env x y (mkNomReflCo $ typeKind x)
           ; go xs ys }
    go _ _ = surelyApart
      -- Possibly different saturations of a polykinded tycon
      -- See Note [Polykinded tycon applications]

---------------------------------
uVar :: UMEnv
     -> InTyVar         -- Variable to be unified
     -> Type            -- with this Type
     -> Coercion        -- :: kind tv ~N kind ty
     -> UM ()

uVar env tv1 ty kco
 = do { -- Apply the ambient renaming
        let tv1' = umRnOccL env tv1

        -- Check to see whether tv1 is refined by the substitution
      ; subst <- getTvSubstEnv
      ; case (lookupVarEnv subst tv1') of
          Just ty' | um_unif env                -- Unifying, so call
                   -> unify_ty env ty' ty kco   -- back into unify
                   | otherwise
                   -> -- Matching, we don't want to just recur here.
                      -- this is because the range of the subst is the target
                      -- type, not the template type. So, just check for
                      -- normal type equality.
                      guard ((ty' `mkCastTy` kco) `eqType` ty)
          Nothing  -> uUnrefined env tv1' ty ty kco } -- No, continue

uUnrefined :: UMEnv
           -> OutTyVar          -- variable to be unified
           -> Type              -- with this Type
           -> Type              -- (version w/ expanded synonyms)
           -> Coercion          -- :: kind tv ~N kind ty
           -> UM ()

-- We know that tv1 isn't refined

uUnrefined env tv1' ty2 ty2' kco
  | Just ty2'' <- coreView ty2'
  = uUnrefined env tv1' ty2 ty2'' kco    -- Unwrap synonyms
                -- This is essential, in case we have
                --      type Foo a = a
                -- and then unify a ~ Foo a

  | TyVarTy tv2 <- ty2'
  = do { let tv2' = umRnOccR env tv2
       ; unless (tv1' == tv2' && um_unif env) $ do
           -- If we are unifying a ~ a, just return immediately
           -- Do not extend the substitution
           -- See Note [Self-substitution when matching]

          -- Check to see whether tv2 is refined
       { subst <- getTvSubstEnv
       ; case lookupVarEnv subst tv2 of
         {  Just ty' | um_unif env -> uUnrefined env tv1' ty' ty' kco
         ;  _ ->

    do {   -- So both are unrefined
           -- Bind one or the other, depending on which is bindable
       ; let b1  = tvBindFlag env tv1'
             b2  = tvBindFlag env tv2'
             ty1 = mkTyVarTy tv1'
       ; case (b1, b2) of
           (BindMe, _) -> bindTv env tv1' (ty2 `mkCastTy` mkSymCo kco)
           (_, BindMe) | um_unif env
                       -> bindTv (umSwapRn env) tv2 (ty1 `mkCastTy` kco)

           _ | tv1' == tv2' -> return ()
             -- How could this happen? If we're only matching and if
             -- we're comparing forall-bound variables.

           _ -> maybeApart -- See Note [Unification with skolems]
  }}}}

uUnrefined env tv1' ty2 _ kco -- ty2 is not a type variable
  = case tvBindFlag env tv1' of
      Skolem -> maybeApart  -- See Note [Unification with skolems]
      BindMe -> bindTv env tv1' (ty2 `mkCastTy` mkSymCo kco)

bindTv :: UMEnv -> OutTyVar -> Type -> UM ()
-- OK, so we want to extend the substitution with tv := ty
-- But first, we must do a couple of checks
bindTv env tv1 ty2
  = do  { let free_tvs2 = tyCoVarsOfType ty2

        -- Make sure tys mentions no local variables
        -- E.g.  (forall a. b) ~ (forall a. [a])
        -- We should not unify b := [a]!
        ; checkRnEnv env free_tvs2

        -- Occurs check, see Note [Fine-grained unification]
        -- Make sure you include 'kco' (which ty2 does) Trac #14846
        ; occurs <- occursCheck env tv1 free_tvs2

        ; if occurs then maybeApart
                    else extendTvEnv tv1 ty2 }

occursCheck :: UMEnv -> TyVar -> VarSet -> UM Bool
occursCheck env tv free_tvs
  | um_unif env
  = do { tsubst <- getTvSubstEnv
       ; return (tv `elemVarSet` niSubstTvSet tsubst free_tvs) }

  | otherwise      -- Matching; no occurs check
  = return False   -- See Note [Self-substitution when matching]

{-
%************************************************************************
%*                                                                      *
                Binding decisions
*                                                                      *
************************************************************************
-}

data BindFlag
  = BindMe      -- A regular type variable

  | Skolem      -- This type variable is a skolem constant
                -- Don't bind it; it only matches itself
  deriving Eq

{-
************************************************************************
*                                                                      *
                Unification monad
*                                                                      *
************************************************************************
-}

data UMEnv
  = UMEnv { um_unif :: AmIUnifying

          , um_inj_tf :: Bool
            -- Checking for injectivity?
            -- See (end of) Note [Specification of unification]

          , um_rn_env :: RnEnv2
            -- Renaming InTyVars to OutTyVars; this eliminates
            -- shadowing, and lines up matching foralls on the left
            -- and right

          , um_skols :: TyVarSet
            -- OutTyVars bound by a forall in this unification;
            -- Do not bind these in the substitution!
            -- See the function tvBindFlag

          , um_bind_fun :: TyVar -> BindFlag
            -- User-supplied BindFlag function,
            -- for variables not in um_skols
          }

data UMState = UMState
                   { um_tv_env   :: TvSubstEnv
                   , um_cv_env   :: CvSubstEnv }

newtype UM a = UM { unUM :: UMState -> UnifyResultM (UMState, a) }

instance Functor UM where
      fmap = liftM

instance Applicative UM where
      pure a = UM (\s -> pure (s, a))
      (<*>)  = ap

instance Monad UM where
#if !MIN_VERSION_base(4,13,0)
  fail     = MonadFail.fail
#endif
  m >>= k  = UM (\state ->
                  do { (state', v) <- unUM m state
                     ; unUM (k v) state' })

-- need this instance because of a use of 'guard' above
instance Alternative UM where
  empty     = UM (\_ -> Control.Applicative.empty)
  m1 <|> m2 = UM (\state ->
                  unUM m1 state <|>
                  unUM m2 state)

instance MonadPlus UM

instance MonadFail.MonadFail UM where
    fail _   = UM (\_ -> SurelyApart) -- failed pattern match

initUM :: TvSubstEnv  -- subst to extend
       -> CvSubstEnv
       -> UM a -> UnifyResultM a
initUM subst_env cv_subst_env um
  = case unUM um state of
      Unifiable (_, subst)  -> Unifiable subst
      MaybeApart (_, subst) -> MaybeApart subst
      SurelyApart           -> SurelyApart
  where
    state = UMState { um_tv_env = subst_env
                    , um_cv_env = cv_subst_env }

tvBindFlag :: UMEnv -> OutTyVar -> BindFlag
tvBindFlag env tv
  | tv `elemVarSet` um_skols env = Skolem
  | otherwise                    = um_bind_fun env tv

getTvSubstEnv :: UM TvSubstEnv
getTvSubstEnv = UM $ \state -> Unifiable (state, um_tv_env state)

getCvSubstEnv :: UM CvSubstEnv
getCvSubstEnv = UM $ \state -> Unifiable (state, um_cv_env state)

getSubst :: UMEnv -> UM TCvSubst
getSubst env = do { tv_env <- getTvSubstEnv
                  ; cv_env <- getCvSubstEnv
                  ; let in_scope = rnInScopeSet (um_rn_env env)
                  ; return (mkTCvSubst in_scope (tv_env, cv_env)) }

extendTvEnv :: TyVar -> Type -> UM ()
extendTvEnv tv ty = UM $ \state ->
  Unifiable (state { um_tv_env = extendVarEnv (um_tv_env state) tv ty }, ())

extendCvEnv :: CoVar -> Coercion -> UM ()
extendCvEnv cv co = UM $ \state ->
  Unifiable (state { um_cv_env = extendVarEnv (um_cv_env state) cv co }, ())

umRnBndr2 :: UMEnv -> TyCoVar -> TyCoVar -> UMEnv
umRnBndr2 env v1 v2
  = env { um_rn_env = rn_env', um_skols = um_skols env `extendVarSet` v' }
  where
    (rn_env', v') = rnBndr2_var (um_rn_env env) v1 v2

checkRnEnv :: UMEnv -> VarSet -> UM ()
checkRnEnv env varset
  | isEmptyVarSet skol_vars           = return ()
  | varset `disjointVarSet` skol_vars = return ()
  | otherwise                         = maybeApart
               -- ToDo: why MaybeApart?
               -- I think SurelyApart would be right
  where
    skol_vars = um_skols env
    -- NB: That isEmptyVarSet guard is a critical optimization;
    -- it means we don't have to calculate the free vars of
    -- the type, often saving quite a bit of allocation.

-- | Converts any SurelyApart to a MaybeApart
don'tBeSoSure :: UM () -> UM ()
don'tBeSoSure um = UM $ \ state ->
  case unUM um state of
    SurelyApart -> MaybeApart (state, ())
    other       -> other

umRnOccL :: UMEnv -> TyVar -> TyVar
umRnOccL env v = rnOccL (um_rn_env env) v

umRnOccR :: UMEnv -> TyVar -> TyVar
umRnOccR env v = rnOccR (um_rn_env env) v

umSwapRn :: UMEnv -> UMEnv
umSwapRn env = env { um_rn_env = rnSwap (um_rn_env env) }

maybeApart :: UM ()
maybeApart = UM (\state -> MaybeApart (state, ()))

surelyApart :: UM a
surelyApart = UM (\_ -> SurelyApart)

{-
%************************************************************************
%*                                                                      *
            Matching a (lifted) type against a coercion
%*                                                                      *
%************************************************************************

This section defines essentially an inverse to liftCoSubst. It is defined
here to avoid a dependency from Coercion on this module.

-}

data MatchEnv = ME { me_tmpls :: TyVarSet
                   , me_env   :: RnEnv2 }

-- | 'liftCoMatch' is sort of inverse to 'liftCoSubst'.  In particular, if
--   @liftCoMatch vars ty co == Just s@, then @liftCoSubst s ty == co@,
--   where @==@ there means that the result of 'liftCoSubst' has the same
--   type as the original co; but may be different under the hood.
--   That is, it matches a type against a coercion of the same
--   "shape", and returns a lifting substitution which could have been
--   used to produce the given coercion from the given type.
--   Note that this function is incomplete -- it might return Nothing
--   when there does indeed exist a possible lifting context.
--
-- This function is incomplete in that it doesn't respect the equality
-- in `eqType`. That is, it's possible that this will succeed for t1 and
-- fail for t2, even when t1 `eqType` t2. That's because it depends on
-- there being a very similar structure between the type and the coercion.
-- This incompleteness shouldn't be all that surprising, especially because
-- it depends on the structure of the coercion, which is a silly thing to do.
--
-- The lifting context produced doesn't have to be exacting in the roles
-- of the mappings. This is because any use of the lifting context will
-- also require a desired role. Thus, this algorithm prefers mapping to
-- nominal coercions where it can do so.
liftCoMatch :: TyCoVarSet -> Type -> Coercion -> Maybe LiftingContext
liftCoMatch tmpls ty co
  = do { cenv1 <- ty_co_match menv emptyVarEnv ki ki_co ki_ki_co ki_ki_co
       ; cenv2 <- ty_co_match menv cenv1       ty co
                              (mkNomReflCo co_lkind) (mkNomReflCo co_rkind)
       ; return (LC (mkEmptyTCvSubst in_scope) cenv2) }
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyCoVarsOfCo co)
    -- Like tcMatchTy, assume all the interesting variables
    -- in ty are in tmpls

    ki       = typeKind ty
    ki_co    = promoteCoercion co
    ki_ki_co = mkNomReflCo liftedTypeKind

    Pair co_lkind co_rkind = coercionKind ki_co

-- | 'ty_co_match' does all the actual work for 'liftCoMatch'.
ty_co_match :: MatchEnv   -- ^ ambient helpful info
            -> LiftCoEnv  -- ^ incoming subst
            -> Type       -- ^ ty, type to match
            -> Coercion   -- ^ co, coercion to match against
            -> Coercion   -- ^ :: kind of L type of substed ty ~N L kind of co
            -> Coercion   -- ^ :: kind of R type of substed ty ~N R kind of co
            -> Maybe LiftCoEnv
ty_co_match menv subst ty co lkco rkco
  | Just ty' <- coreView ty = ty_co_match menv subst ty' co lkco rkco

  -- handle Refl case:
  | tyCoVarsOfType ty `isNotInDomainOf` subst
  , Just (ty', _) <- isReflCo_maybe co
  , ty `eqType` ty'
  = Just subst

  where
    isNotInDomainOf :: VarSet -> VarEnv a -> Bool
    isNotInDomainOf set env
      = noneSet (\v -> elemVarEnv v env) set

    noneSet :: (Var -> Bool) -> VarSet -> Bool
    noneSet f = allVarSet (not . f)

ty_co_match menv subst ty co lkco rkco
  | CastTy ty' co' <- ty
     -- See Note [Matching in the presence of casts]
  = let empty_subst  = mkEmptyTCvSubst (rnInScopeSet (me_env menv))
        substed_co_l = substCo (liftEnvSubstLeft empty_subst subst)  co'
        substed_co_r = substCo (liftEnvSubstRight empty_subst subst) co'
    in
    ty_co_match menv subst ty' co (substed_co_l `mkTransCo` lkco)
                                  (substed_co_r `mkTransCo` rkco)

  | SymCo co' <- co
  = swapLiftCoEnv <$> ty_co_match menv (swapLiftCoEnv subst) ty co' rkco lkco

  -- Match a type variable against a non-refl coercion
ty_co_match menv subst (TyVarTy tv1) co lkco rkco
  | Just co1' <- lookupVarEnv subst tv1' -- tv1' is already bound to co1
  = if eqCoercionX (nukeRnEnvL rn_env) co1' co
    then Just subst
    else Nothing       -- no match since tv1 matches two different coercions

  | tv1' `elemVarSet` me_tmpls menv           -- tv1' is a template var
  = if any (inRnEnvR rn_env) (tyCoVarsOfCoList co)
    then Nothing      -- occurs check failed
    else Just $ extendVarEnv subst tv1' $
                castCoercionKindI co (mkSymCo lkco) (mkSymCo rkco)

  | otherwise
  = Nothing

  where
    rn_env = me_env menv
    tv1' = rnOccL rn_env tv1

  -- just look through SubCo's. We don't really care about roles here.
ty_co_match menv subst ty (SubCo co) lkco rkco
  = ty_co_match menv subst ty co lkco rkco

ty_co_match menv subst (AppTy ty1a ty1b) co _lkco _rkco
  | Just (co2, arg2) <- splitAppCo_maybe co     -- c.f. Unify.match on AppTy
  = ty_co_match_app menv subst ty1a [ty1b] co2 [arg2]
ty_co_match menv subst ty1 (AppCo co2 arg2) _lkco _rkco
  | Just (ty1a, ty1b) <- repSplitAppTy_maybe ty1
       -- yes, the one from Type, not TcType; this is for coercion optimization
  = ty_co_match_app menv subst ty1a [ty1b] co2 [arg2]

ty_co_match menv subst (TyConApp tc1 tys) (TyConAppCo _ tc2 cos) _lkco _rkco
  = ty_co_match_tc menv subst tc1 tys tc2 cos
ty_co_match menv subst (FunTy ty1 ty2) co _lkco _rkco
    -- Despite the fact that (->) is polymorphic in four type variables (two
    -- runtime rep and two types), we shouldn't need to explicitly unify the
    -- runtime reps here; unifying the types themselves should be sufficient.
    -- See Note [Representation of function types].
  | Just (tc, [_,_,co1,co2]) <- splitTyConAppCo_maybe co
  , tc == funTyCon
  = let Pair lkcos rkcos = traverse (fmap mkNomReflCo . coercionKind) [co1,co2]
    in ty_co_match_args menv subst [ty1, ty2] [co1, co2] lkcos rkcos

ty_co_match menv subst (ForAllTy (Bndr tv1 _) ty1)
                       (ForAllCo tv2 kind_co2 co2)
                       lkco rkco
  | isTyVar tv1 && isTyVar tv2
  = do { subst1 <- ty_co_match menv subst (tyVarKind tv1) kind_co2
                               ki_ki_co ki_ki_co
       ; let rn_env0 = me_env menv
             rn_env1 = rnBndr2 rn_env0 tv1 tv2
             menv'   = menv { me_env = rn_env1 }
       ; ty_co_match menv' subst1 ty1 co2 lkco rkco }
  where
    ki_ki_co = mkNomReflCo liftedTypeKind

-- ty_co_match menv subst (ForAllTy (Bndr cv1 _) ty1)
--                        (ForAllCo cv2 kind_co2 co2)
--                        lkco rkco
--   | isCoVar cv1 && isCoVar cv2
--   We seems not to have enough information for this case
--   1. Given:
--        cv1      :: (s1 :: k1) ~r (s2 :: k2)
--        kind_co2 :: (s1' ~ s2') ~N (t1 ~ t2)
--        eta1      = mkNthCo role 2 (downgradeRole r Nominal kind_co2)
--                 :: s1' ~ t1
--        eta2      = mkNthCo role 3 (downgradeRole r Nominal kind_co2)
--                 :: s2' ~ t2
--      Wanted:
--        subst1 <- ty_co_match menv subst  s1 eta1 kco1 kco2
--        subst2 <- ty_co_match menv subst1 s2 eta2 kco3 kco4
--      Question: How do we get kcoi?
--   2. Given:
--        lkco :: <*>    -- See Note [Weird typing rule for ForAllTy] in Type
--        rkco :: <*>
--      Wanted:
--        ty_co_match menv' subst2 ty1 co2 lkco' rkco'
--      Question: How do we get lkco' and rkco'?

ty_co_match _ subst (CoercionTy {}) _ _ _
  = Just subst -- don't inspect coercions

ty_co_match menv subst ty (GRefl r t (MCo co)) lkco rkco
  =  ty_co_match menv subst ty (GRefl r t MRefl) lkco (rkco `mkTransCo` mkSymCo co)

ty_co_match menv subst ty co1 lkco rkco
  | Just (CastTy t co, r) <- isReflCo_maybe co1
  -- In @pushRefl@, pushing reflexive coercion inside CastTy will give us
  -- t |> co ~ t ; <t> ; t ~ t |> co
  -- But transitive coercions are not helpful. Therefore we deal
  -- with it here: we do recursion on the smaller reflexive coercion,
  -- while propagating the correct kind coercions.
  = let kco' = mkSymCo co
    in ty_co_match menv subst ty (mkReflCo r t) (lkco `mkTransCo` kco')
                                                (rkco `mkTransCo` kco')


ty_co_match menv subst ty co lkco rkco
  | Just co' <- pushRefl co = ty_co_match menv subst ty co' lkco rkco
  | otherwise               = Nothing

ty_co_match_tc :: MatchEnv -> LiftCoEnv
               -> TyCon -> [Type]
               -> TyCon -> [Coercion]
               -> Maybe LiftCoEnv
ty_co_match_tc menv subst tc1 tys1 tc2 cos2
  = do { guard (tc1 == tc2)
       ; ty_co_match_args menv subst tys1 cos2 lkcos rkcos }
  where
    Pair lkcos rkcos
      = traverse (fmap mkNomReflCo . coercionKind) cos2

ty_co_match_app :: MatchEnv -> LiftCoEnv
                -> Type -> [Type] -> Coercion -> [Coercion]
                -> Maybe LiftCoEnv
ty_co_match_app menv subst ty1 ty1args co2 co2args
  | Just (ty1', ty1a) <- repSplitAppTy_maybe ty1
  , Just (co2', co2a) <- splitAppCo_maybe co2
  = ty_co_match_app menv subst ty1' (ty1a : ty1args) co2' (co2a : co2args)

  | otherwise
  = do { subst1 <- ty_co_match menv subst ki1 ki2 ki_ki_co ki_ki_co
       ; let Pair lkco rkco = mkNomReflCo <$> coercionKind ki2
       ; subst2 <- ty_co_match menv subst1 ty1 co2 lkco rkco
       ; let Pair lkcos rkcos = traverse (fmap mkNomReflCo . coercionKind) co2args
       ; ty_co_match_args menv subst2 ty1args co2args lkcos rkcos }
  where
    ki1 = typeKind ty1
    ki2 = promoteCoercion co2
    ki_ki_co = mkNomReflCo liftedTypeKind

ty_co_match_args :: MatchEnv -> LiftCoEnv -> [Type]
                 -> [Coercion] -> [Coercion] -> [Coercion]
                 -> Maybe LiftCoEnv
ty_co_match_args _    subst []       []         _ _ = Just subst
ty_co_match_args menv subst (ty:tys) (arg:args) (lkco:lkcos) (rkco:rkcos)
  = do { subst' <- ty_co_match menv subst ty arg lkco rkco
       ; ty_co_match_args menv subst' tys args lkcos rkcos }
ty_co_match_args _    _     _        _          _ _ = Nothing

pushRefl :: Coercion -> Maybe Coercion
pushRefl co =
  case (isReflCo_maybe co) of
    Just (AppTy ty1 ty2, Nominal)
      -> Just (AppCo (mkReflCo Nominal ty1) (mkNomReflCo ty2))
    Just (FunTy ty1 ty2, r)
      | Just rep1 <- getRuntimeRep_maybe ty1
      , Just rep2 <- getRuntimeRep_maybe ty2
      ->  Just (TyConAppCo r funTyCon [ mkReflCo r rep1, mkReflCo r rep2
                                       , mkReflCo r ty1,  mkReflCo r ty2 ])
    Just (TyConApp tc tys, r)
      -> Just (TyConAppCo r tc (zipWith mkReflCo (tyConRolesX r tc) tys))
    Just (ForAllTy (Bndr tv _) ty, r)
      -> Just (ForAllCo tv (mkNomReflCo (varType tv)) (mkReflCo r ty))
    -- NB: NoRefl variant. Otherwise, we get a loop!
    _ -> Nothing
