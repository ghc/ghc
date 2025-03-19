-- (c) The University of Glasgow 2006

{-# LANGUAGE ScopedTypeVariables, PatternSynonyms, MultiWayIf #-}

{-# LANGUAGE DeriveFunctor #-}

module GHC.Core.Unify (
        tcMatchTy, tcMatchTyKi,
        tcMatchTys, tcMatchTyKis,
        tcMatchTyX, tcMatchTysX, tcMatchTyKisX,
        tcMatchTyX_BM, ruleMatchTyKiX,

        -- Side-effect free unification
        tcUnifyTy, tcUnifyTys, tcUnifyFunDeps, tcUnifyDebugger,
        tcUnifyTysFG, tcUnifyTyForInjectivity,
        BindTvFun, BindFamFun, BindFlag(..),
        matchBindTv, alwaysBindTv, alwaysBindFam, dontCareBindFam,
        UnifyResult, UnifyResultM(..), MaybeApartReason(..),
        typesCantMatch, typesAreApart,

        -- Matching a type against a lifted type (coercion)
        liftCoMatch
   ) where

import GHC.Prelude

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Builtin.Names( tYPETyConKey, cONSTRAINTTyConKey )
import GHC.Core.Type     hiding ( getTvSubstEnv )
import GHC.Core.Coercion hiding ( getCvSubstEnv )
import GHC.Core.Predicate( scopedSort )
import GHC.Core.TyCon
import GHC.Core.Predicate( CanEqLHS(..), canEqLHS_maybe )
import GHC.Core.TyCon.Env
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Compare ( eqType, tcEqType, tcEqTyConAppArgs )
import GHC.Core.TyCo.FVs     ( tyCoVarsOfCoList, tyCoFVsOfTypes )
import GHC.Core.TyCo.Subst   ( mkTvSubst )
import GHC.Core.Map.Type
import GHC.Core.Multiplicity

import GHC.Utils.FV( FV, fvVarList )
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Types.Basic( SwapFlag(..) )
import GHC.Types.Unique.FM
import GHC.Exts( oneShot )
import GHC.Utils.Panic

import GHC.Data.Pair
import GHC.Data.TrieMap
import GHC.Data.Maybe( orElse )

import Control.Monad
import qualified Data.Semigroup as S
import GHC.Builtin.Types.Prim (fUNTyCon)

{- Note [The Core unifier]
~~~~~~~~~~~~~~~~~~~~~~~~~~
This module contains the (pure) unifier two types.  It is subtle in a number
of ways.  Here we summarise, but see Note [Specification of unification].

(CU1) It creates a substition only for "bindable" or "template" type variables.
  These are identified by a `um_bind_tv_fun` function passed down in the `UMEnv`
  environment.

(CU2) We want to match in the presence of foralls;
        e.g     (forall a. t1) ~ (forall b. t2)
   That is what the `um_rn_env :: RnEnv2` field of `UMEnv` is for; it does the
   alpha-renaming that makes it as if `a` and `b` were the same variable.
   Initialising the `RnEnv2`, so that it can generate a fresh binder when
   necessary, entails knowing the free variables of both types.

   Of course, we must be careful not to bind a template type variable to a
   locally bound variable.  E.g.
        (forall a. x) ~ (forall b. b)
   where `x` is the template type variable.  Then we do not want to
   bind `x` to a/b!  See `mentionsForAllBoundTyVarsL/R`.

(CU3) We want to take special care for type families.
  See the big Note [Apartness and type families]

(CU4) Rather than returning just "unifiable" or "not-unifiable" we do "fine-grained"
  unification (hence "fg" or "FG" in this module) returning three possiblities,
  captured in `UnifyResult`:
    - Unifiable subst : certainly unifiable with this type substitution
    - SurelyApart     : cannot be unifiable, regardless of how type familes reduce
    - MaybeApart      : neither of the above
  See Note [Unification result].

  Four reasons for MaybeApart (see `MaybeApartReason`).  The first two are the
  big ones!
    * MARTypeFamily:
         Family reduction might make the two types equal
             Maybe (F Int) ~ Maybe Bool
         See Note [Apartness and type families]
    * MARInfinite (occurs check):
         See Note [Infinitary substitutions]
    * MARTypeVsConstraint:
         See Note [Type and Constraint are not apart] in GHC.Builtin.Types.Prim
    * MARCast (obscure):
         See (KCU2) in Note [Kind coercions in Unify]

(CU5) We need to take care with kinds.  See Note [tcMatchTy vs tcMatchTyKi]

(CU6) The "unifier" can also do /matching/, governed by `um_unif :: AmIUnifying`.
   When matching, the LHS and RHS namespaces are unrelated. In particular, the
   bindable type variable can occur (unrelatedly) in the RHS.  E.g.
        match  (a,Maybe a) ~  ([a], Maybe [a])
   We get the substitution [a :-> [a]], without confusing the
   LHS `a` with the RHS `a`.  The substitition is "one-shot", and should not be
   iterated.

Note [Infinitary substitutions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do the types (x, x) and ([y], y) unify? The answer is seemingly "no" --
no substitution to finite types makes these match. This is the famous
"occurs check".

But, a substitution to *infinite* types can unify these two types:
  [x |-> [[...]]], y |-> [[[...]]] ].

Why do we care? Consider these two type family instances:

  type instance F x x   = Int
  type instance F [y] y = Bool

If we also have

  type instance Looper = [Looper]

then the instances potentially overlap -- they are not "apart". So we must
distinguish failure-to-unify from definitely-apart. The solution is to use
unification over infinite terms. This is possible (see [1] for lots of gory
details), but a full algorithm is a little more powerful than we need. Instead,
we make a conservative approximation and just omit the occurs check.

  [1]: http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf

tcUnifyTys considers an occurs-check problem as the same as general unification
failure.

See also #8162.

It's worth noting that unification in the presence of infinite types is not
complete. This means that, sometimes, a closed type family does not reduce
when it should. See test case indexed-types/should_fail/Overlap15 for an
example.

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

Note [Unification result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See `UnifyResult` and `UnifyResultM`.  When unifying t1 ~ t2, we return
* Unifiable s, if s is a substitution such that s(t1) is syntactically the
  same as s(t2), modulo type-synonym expansion.
* SurelyApart, if there is no substitution s such that s(t1) = s(t2),
  where "=" includes type-family reductions.
* MaybeApart mar s, when we aren't sure. `mar` is a MaybeApartReason.

Examples
* [a] ~ Maybe b: SurelyApart, because [] and Maybe can't unify

* [(a,Int)] ~ [(Bool,b)]:  Unifiable

* [F Int] ~ [Bool]: MaybeApart MARTypeFamily, because F Int might reduce to Bool
                    (the unifier does not try this)

* a ~ Maybe a: MaybeApart MARInfinite. Not Unifiable clearly, but not SurelyApart
    either; consider
       a := Loop
       where  type family Loop where Loop = Maybe Loop

Wrinkle (UR1): see `combineMAR`
   There is the possibility that two types are MaybeApart for *both* reasons:

   * (a, F Int) ~ (Maybe a, Bool)

   What reason should we use? The *only* consumer of the reason is described
   in Note [Infinitary substitution in lookup] in GHC.Core.InstEnv. The goal
   there is identify which instances might match a target later (but don't
   match now) -- except that we want to ignore the possibility of infinitary
   substitutions. So let's examine a concrete scenario:

     class C a b c
     instance C a (Maybe a) Bool
     -- other instances, including one that will actually match
     [W] C b b (F Int)

   Do we want the instance as a future possibility? No. The only way that
   instance can match is in the presence of an infinite type (infinitely nested
   Maybes). We thus say that `MARInfinite` takes precedence, so that InstEnv treats
   this case as an infinitary substitution case; the fact that a type family is
   involved is only incidental. We thus define `combineMAR` to prefer
   `MARInfinite`.

Note [Apartness and type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

  type family F a b where
    F Int Bool = Char
    F a   b    = Double
  type family G a         -- open, no instances

How do we reduce (F (G Float) (G Float))? The first equation clearly doesn't
match immediately while the second equation does. But, before reducing, we must
make sure that the target can never become (F Int Bool). Well, no matter what G
Float becomes, it certainly won't become *both* Int and Bool, so indeed we're
safe reducing (F (G Float) (G Float)) to Double.

So we must say that the argument list
     (G Float) (G Float)   is SurelyApart from   Int Bool

This is necessary not only to get more reductions (which we might be willing to
give up on), but for /substitutivity/. If we have (F x x), we can see that (F x x)
can reduce to Double. So, it had better be the case that (F blah blah) can
reduce to Double, no matter what (blah) is!

To achieve this, `go_fam` in `uVarOrFam` does this;

* When we attempt to unify (G Float) ~ Int, we return MaybeApart..
  but we /also/ extend a "family substitution" [G Float :-> Int],
  in `um_fam_env`, alongside the regular [tyvar :-> type] substitution in
  `um_tv_env`.  See the `BindMe` case of `go_fam` in `uVarOrFam`.

* When we later encounter (G Float) ~ Bool, we apply the family substitution,
  very much as we apply the conventional [tyvar :-> type] substitution
  when we encounter a type variable.  See the `lookupFamEnv` in `go_fam` in
  `uVarOrFam`.

  So (G Float ~ Bool) becomes (Int ~ Bool) which is SurelyApart.  Bingo.


Wrinkles

(ATF0) Once we encounter a type-family application, we only ever return
             MaybeApart   or   SurelyApart
  but never `Unifiable`.  Accordingly, we only return a TyCoVar substitution
  from `tcUnifyTys` and friends; we don't return a type-family substitution as
  well.  (We could imagine doing so, though.)

(ATF1) Exactly the same mechanism is used in class-instance checking.
    If we have
        instance C (Maybe b)
        instance {-# OVERLAPPING #-} C (Maybe Bool)
        [W] C (Maybe (F a))
    we want to know that the second instance might match later, when we know more about `a`.
    The function `GHC.Core.InstEnv.instEnvMatchesAndUnifiers` uses `tcUnifyTysFG` to
    account for type families in the type being matched.

(ATF2) A very similar check is made in `GHC.Tc.Utils.Unify.mightEqualLater`, which
  again uses `tcUnifyTysFG` to account for the possibility of type families.  See
  Note [What might equal later?] in GHC.Tc.Utils.Unify, esp example (10).

(ATF3) What about foralls?   For example, supppose we are unifying
           (forall a. F a) -> (forall a. F a)
   Those two (F a) types are unrelated, bound by different foralls.

   So to keep things simple, the entire family-substitution machinery is used
   only if there are no enclosing foralls (see the (um_foralls env)) check in
   `uSatFamApp`).  That's fine, because the apartness business is used only for
   reducing type-family applications, and class instances, and their arguments
   can't have foralls anyway.

   The bottom line is that we won't discover that
       (forall a. (a, F Int, F Int))
   is surely apart from
       (forall a. (a, Int, Bool))
   but that doesn't matter.  Fixing this would be possible, but would require
   quite a bit of head-scratching.

(ATF4) The family substitution only has /saturated/ family applications in
   its domain. Consider the following concrete example from #16995:

     type family Param :: Type -> Type   -- arity 0

     type family LookupParam (a :: Type) :: Type where
       LookupParam (f Char) = Bool
       LookupParam x        = Int

     foo :: LookupParam (Param ())
     foo = 42

   In order for `foo` to typecheck, `LookupParam (Param ())` must reduce to
   `Int`.  So    (f Char) ~ (Param ())   must be SurelyApart.  Remember, since
   `Param` is a nullary type family, it is over-saturated in (Param ()).
   This unification will only be SurelyApart if we decompose the outer AppTy
   separately, to then give (() ~ Char).

   Not only does this allow more programs to be accepted, it's also important
   for correctness. Not doing this was the root cause of the Core Lint error
   in #16995.

(ATF5) Consider
          instance (Generic1 f, Ord (Rep1 f a))
                => Ord (Generically1 f a) where ...
              -- The "..." gives rise to [W] Ord (Generically1 f a)
   We must use the instance decl (recursively) to simplify the [W] constraint;
   we do /not/ want to worry that the `[G] Ord (Rep1 f a)` might be an
   alternative path.  So `noMatchableGivenDicts` must return False;
   so `mightMatchLater` must return False; so when um_bind_fam_fun returns
   `DontBindMe`, the unifier must return `SurelyApart`, not `MaybeApart`.  See
   `go_fam` in `uVarOrFam`

(ATF6) You might think that when /matching/ the um_fam_env will always be empty,
   because type-class-instance and type-family-instance heads can't include type
   families.  E.g.   instance C (F a) where ...   -- Illegal

   But you'd be wrong: when "improving" type family constraint we may have a
   type family on the LHS of a match. Consider
      type family G6 a = r | r -> a
      type instance G6 [a]  = [G a]
      type instance G6 Bool = Int
   and the Wanted constraint [W] G6 alpha ~ [Int].  We /match/ each type instance
   RHS against [Int]!  So we try
        [G a] ~ [Int]
   and we want to succeed with MaybeApart, so that we can generate the improvement
   constraint  [W] alpha ~ [beta]  where beta is fresh.
   See Section 5.2 of "Injective type families for Haskell".

   A second place that we match with type-fams on the LHS is in `checkValidClass`.
   In `check_dm` we check that the default method has the right type, using matching,
   both ways.  And that type may have type-family applications in it. Example in
   test CoOpt_Singletons.

(ATF7) You might think that (ATF6) is a very special case, and in /other/ uses of
  matching, where we enter via `tc_match_tys_x` we will never see a type-family
  in the template. But actually we do see that case in the specialiser: see
  the call to `tcMatchTy` in `GHC.Core.Opt.Specialise.beats_or_same`

  Also: a user-written RULE could conceivably have a type-family application
  in the template.  It might not be a good rule, but I don't think we currently
  check for this.

(ATF8) The treatment of type families is governed by
         um_bind_fam_fun :: BindFamFun
  in UMEnv, where
         type BindFamFun = TyCon -> [Type] -> Type -> BindFlag
  There are some simple BindFamFun functions provided:
     alwaysBindFam    do the clever stuff above
     neverBindFam     treat type families as SurelyApart
     dontCareBindFam  type families shouldn't exist at all
  This function only affects the difference between the results MaybeApart and
  SurelyApart; it never does not affect whether or not we return Unifiable.

(ATF9) Decomposition.  Consider unifying
          F a  ~  F Int
  There is a unifying substitition [a :-> Int], and we want to find it, returning
  Unifiable. (Remember, this is the Core unifier -- we are not doing type inference.)
  So we should decompose to get (a ~ Int)

  But consider unifying
          F Int ~ F Bool
  Although Int and Bool are SurelyApart, we must return MaybeApart for the outer
  unification.  Hence the use of `don'tBeSoSure` in `go_fam_fam`; it leaves Unifiable
  alone, but weakens `SurelyApart` to `MaybeApart`.

(ATF10) Injectivity.  Consider (AFT9) where F is known to be injective.  Then if we
  are unifying
          F Int ~ F Bool
  we /can/ say SurelyApart.  See the inj/noninj stuff in `go_fam_fam`.

(ATF11) Consider unifying
          [F Int, F Int, F Bool]  ~  [F Bool, Char, Double]
  We find (F Int ~ F Bool), so we can decompose.  But we /also/ want to remember
  the substitution [F Int :-> F Bool].  Then from (F Int ~ Char) we get the
  substitution [F Bool :-> Char].  And that flat-out contradicts (F Bool ~ Double)
  so we should get SurelyApart.

  Key point: when decomposing (F tys1 ~ F tys2), we should /also/ extend the
  type-family substitution.

(ATF12) There is a horrid exception for the injectivity check. See (UR1) in
  in Note [Specification of unification].

SIDE NOTE.  The paper "Closed type families with overlapping equations"
http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf
tries to achieve the same effect with a standard yes/no unifier, by "flattening"
the types (replacing each type-family application with a fresh type variable)
and then unifying.  But that does not work well. Consider (#25657)

    type MyEq :: k -> k -> Bool
    type family MyEq a b where
       MyEq a a = 'True
       MyEq _ _ = 'False

    type Var :: forall {k}. Tag -> k
    type family Var tag = a | a -> tag

Then, because Var is injective, we want
     MyEq (Var A) (Var B) --> False
     MyEq (Var A) (Var A) --> True

But if we flattten the types (Var A) and (Var B) we'll just get fresh type variables,
and all is lost.  But with the current algorithm we have that
    a a   ~    (Var A) (Var B)
is SurelyApart, so the first equation definitely doesn't match and we can try the
second, which does.  END OF SIDE NOTE.
-}

{- *********************************************************************
*                                                                      *
                Binding decisions
*                                                                      *
********************************************************************* -}

data BindFlag
  = BindMe      -- ^ A bindable type variable

  | DontBindMe  -- ^ Do not bind this type variable is /apart/
                -- See also Note [Super skolems: binding when looking up instances]
                -- in GHC.Core.InstEnv.
  deriving Eq

-- | Some unification functions are parameterised by a 'BindTvFun', which
-- says whether or not to allow a certain unification to take place.
-- A 'BindTvFun' takes the 'TyVar' involved along with the 'Type' it will
-- potentially be bound to.
--
-- It is possible for the variable to actually be a coercion variable
-- (Note [Matching coercion variables]), but only when one-way matching.
-- In this case, the 'Type' will be a 'CoercionTy'.
type BindTvFun = TyCoVar -> Type -> BindFlag

-- | BindFamFun is similiar to BindTvFun, but deals with a saturated
-- type-family application.  See Note [Apartness and type families].
type BindFamFun = TyCon -> [Type] -> Type -> BindFlag

-- | Allow binding only for any variable in the set. Variables may
-- be bound to any type.
-- Used when doing simple matching; e.g. can we find a substitution
--
-- @
-- S = [a :-> t1, b :-> t2] such that
--     S( Maybe (a, b->Int )  =   Maybe (Bool, Char -> Int)
-- @
matchBindTv :: TyCoVarSet -> BindTvFun
matchBindTv tvs tv _ty
  | tv `elemVarSet` tvs = BindMe
  | otherwise           = DontBindMe

-- | Allow the binding of any variable to any type
alwaysBindTv :: BindTvFun
alwaysBindTv _tv _ty = BindMe

-- | Allow the binding of a type-family application to any type
alwaysBindFam :: BindFamFun
-- See (ATF8) in Note [Apartness and type families]
alwaysBindFam _tc _args _rhs = BindMe

dontCareBindFam :: HasCallStack => BindFamFun
-- See (ATF8) in Note [Apartness and type families]
dontCareBindFam tc args rhs
  = pprPanic "dontCareBindFam" $
    vcat [ ppr tc <+> ppr args, text "rhs" <+> ppr rhs ]

-- | Don't allow the binding of a type-family application at all
neverBindFam :: BindFamFun
-- See (ATF8) in Note [Apartness and type families]
neverBindFam _tc _args _rhs = DontBindMe


{- *********************************************************************
*                                                                      *
                Various wrappers for matching
*                                                                      *
********************************************************************* -}

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
tcMatchTy :: HasDebugCallStack => Type -> Type -> Maybe Subst
tcMatchTy ty1 ty2 = tcMatchTys [ty1] [ty2]

tcMatchTyX_BM :: HasDebugCallStack
              => BindTvFun -> Subst
              -> Type -> Type -> Maybe Subst
tcMatchTyX_BM bind_tv subst ty1 ty2
  = tc_match_tys_x bind_tv False subst [ty1] [ty2]

-- | Like 'tcMatchTy', but allows the kinds of the types to differ,
-- and thus matches them as well.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyKi :: HasDebugCallStack => Type -> Type -> Maybe Subst
tcMatchTyKi ty1 ty2
  = tc_match_tys alwaysBindTv True [ty1] [ty2]

-- | This is similar to 'tcMatchTy', but extends a substitution
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyX :: HasDebugCallStack
           => Subst               -- ^ Substitution to extend
           -> Type                -- ^ Template
           -> Type                -- ^ Target
           -> Maybe Subst
tcMatchTyX subst ty1 ty2
  = tc_match_tys_x alwaysBindTv False subst [ty1] [ty2]

-- | Like 'tcMatchTy' but over a list of types.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTys :: HasDebugCallStack
           => [Type]         -- ^ Template
           -> [Type]         -- ^ Target
           -> Maybe Subst    -- ^ One-shot; in principle the template
                             -- variables could be free in the target
                             -- See (CU6) in Note [The Core unifier]
tcMatchTys tys1 tys2
  = tc_match_tys alwaysBindTv False tys1 tys2

-- | Like 'tcMatchTyKi' but over a list of types.
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyKis :: HasDebugCallStack
             => [Type]         -- ^ Template
             -> [Type]         -- ^ Target
             -> Maybe Subst    -- ^ One-shot substitution
                               -- See (CU6) in Note [The Core unifier]
tcMatchTyKis tys1 tys2
  = tc_match_tys alwaysBindTv True tys1 tys2

-- | Like 'tcMatchTys', but extending a substitution
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTysX :: HasDebugCallStack
            => Subst          -- ^ Substitution to extend
            -> [Type]         -- ^ Template
            -> [Type]         -- ^ Target
            -> Maybe Subst    -- ^ One-shot substitution
tcMatchTysX subst tys1 tys2
  = tc_match_tys_x alwaysBindTv False subst tys1 tys2

-- | Like 'tcMatchTyKis', but extending a substitution
-- See also Note [tcMatchTy vs tcMatchTyKi]
tcMatchTyKisX :: HasDebugCallStack
              => Subst        -- ^ Substitution to extend
              -> [Type]       -- ^ Template
              -> [Type]       -- ^ Target
              -> Maybe Subst  -- ^ One-shot substitution
tcMatchTyKisX subst tys1 tys2
  = tc_match_tys_x alwaysBindTv True subst tys1 tys2

-- | Same as tc_match_tys_x, but starts with an empty substitution
tc_match_tys :: HasDebugCallStack
             => BindTvFun
             -> Bool          -- ^ match kinds?
             -> [Type]
             -> [Type]
             -> Maybe Subst
tc_match_tys bind_me match_kis tys1 tys2
  = tc_match_tys_x bind_me match_kis (mkEmptySubst in_scope) tys1 tys2
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes tys1 `unionVarSet` tyCoVarsOfTypes tys2)

-- | Worker for 'tcMatchTysX' and 'tcMatchTyKisX'
tc_match_tys_x :: HasDebugCallStack
               => BindTvFun
               -> Bool          -- ^ match kinds?
               -> Subst
               -> [Type]
               -> [Type]
               -> Maybe Subst
tc_match_tys_x bind_tv match_kis (Subst in_scope id_env tv_env cv_env) tys1 tys2
  = case tc_unify_tys alwaysBindFam  -- (ATF7) in Note [Apartness and type families]
                      bind_tv
                      False  -- Matching, not unifying
                      False  -- Not an injectivity check
                      match_kis
                      RespectMultiplicities
                      (mkRnEnv2 in_scope) tv_env cv_env tys1 tys2 of
      Unifiable (tv_env', cv_env')
        -> Just $ Subst in_scope id_env tv_env' cv_env'
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
  = case tc_unify_tys neverBindFam (matchBindTv tmpl_tvs)
      -- neverBindFam: a type family probably shouldn't appear
      -- on the LHS of a RULE, although we don't currently prevent it.
      -- But even if it did, (ATF8) in Note [Apartness and type families]
      -- says it doesn't matter becuase here we only care about Unifiable.
      -- So neverBindFam is efficient, and sufficient.
                      False    -- Matching, not unifying
                      False    -- No doing an injectivity check
                      True     -- Match the kinds
                      IgnoreMultiplicities
                        -- See Note [Rewrite rules ignore multiplicities in FunTy]
                      rn_env tenv emptyCvSubstEnv [tmpl] [target] of
      Unifiable (tenv', _) -> Just tenv'
      _                    -> Nothing

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
typesCantMatch prs = any (uncurry typesAreApart) prs

typesAreApart :: Type -> Type -> Bool
typesAreApart t1 t2 = case tcUnifyTysFG alwaysBindFam alwaysBindTv [t1] [t2] of
                        SurelyApart -> True
                        _           -> False
{-
************************************************************************
*                                                                      *
             Various wrappers for unification
*                                                                      *
********************************************************************* -}

-- | Simple unification of two types; all type variables are bindable
-- Precondition: the kinds are already equal
tcUnifyTy :: Type -> Type       -- All tyvars are bindable
          -> Maybe Subst
                       -- A regular one-shot (idempotent) substitution
tcUnifyTy t1 t2 = tcUnifyTys alwaysBindTv [t1] [t2]

tcUnifyDebugger :: Type -> Type -> Maybe Subst
tcUnifyDebugger t1 t2
  = case tc_unify_tys_fg
             True            -- Unify kinds
             neverBindFam    -- Does not affect Unifiable, so pick max efficient
                             -- See (ATF8) in Note [Apartness and type families]
             alwaysBindTv
             [t1] [t2] of
      Unifiable subst -> Just subst
      _               -> Nothing

-- | Like 'tcUnifyTys' but also unifies the kinds
tcUnifyFunDeps :: TyCoVarSet
               -> [Type] -> [Type]
               -> Maybe Subst
tcUnifyFunDeps qtvs tys1 tys2
  = case tc_unify_tys_fg
             True               -- Unify kinds
             dontCareBindFam    -- Class-instance heads never mention type families
             (matchBindTv qtvs)
             tys1 tys2 of
      Unifiable subst -> Just subst
      _               -> Nothing

-- | Unify or match a type-family RHS with a type (possibly another type-family RHS)
-- Precondition: kinds are the same
tcUnifyTyForInjectivity
    :: AmIUnifying  -- ^ True <=> do two-way unification;
                    --   False <=> do one-way matching.
                    --   See end of sec 5.2 from the paper
    -> InScopeSet     -- Should include the free tyvars of both Type args
    -> Type -> Type   -- Types to unify
    -> Maybe Subst
-- This algorithm is an implementation of the "Algorithm U" presented in
-- the paper "Injective type families for Haskell", Figures 2 and 3.
-- The code is incorporated with the standard unifier for convenience, but
-- its operation should match the specification in the paper.
tcUnifyTyForInjectivity unif in_scope t1 t2
  = case tc_unify_tys alwaysBindFam alwaysBindTv
                       unif   -- Am I unifying?
                       True   -- Do injectivity checks
                       False  -- Don't check outermost kinds
                       RespectMultiplicities
                       rn_env emptyTvSubstEnv emptyCvSubstEnv
                       [t1] [t2] of
      Unifiable          (tv_subst, _cv_subst) -> Just $ maybe_fix tv_subst
      MaybeApart _reason (tv_subst, _cv_subst) -> Just $ maybe_fix tv_subst
                 -- We want to *succeed* in questionable cases.
                 -- This is a pre-unification algorithm.
      SurelyApart      -> Nothing
  where
    rn_env   = mkRnEnv2 in_scope

    maybe_fix | unif      = niFixSubst in_scope
              | otherwise = mkTvSubst in_scope -- when matching, don't confuse
                                               -- domain with range

-----------------
tcUnifyTys :: BindTvFun
           -> [Type] -> [Type]
           -> Maybe Subst
                                -- ^ A regular one-shot (idempotent) substitution
                                -- that unifies the erased types. See comments
                                -- for 'tcUnifyTysFG'

-- The two types may have common type variables, and indeed do so in the
-- second call to tcUnifyTys in GHC.Tc.Instance.FunDeps.checkClsFD
tcUnifyTys bind_fn tys1 tys2
  = case tcUnifyTysFG neverBindFam bind_fn tys1 tys2 of
      Unifiable result -> Just result
      _                -> Nothing

-- | (tcUnifyTysFG bind_fam bind_tv tys1 tys2) does "fine-grain" unification
-- of tys1 and tys2, under the control of `bind_fam` and `bind_tv`.
-- This version requires that the kinds of the types are the same,
-- if you unify left-to-right.
-- See Note [The Core unifier]
tcUnifyTysFG :: BindFamFun -> BindTvFun
             -> [Type] -> [Type]
             -> UnifyResult
tcUnifyTysFG bind_fam bind_tv tys1 tys2
  = tc_unify_tys_fg False bind_fam bind_tv tys1 tys2

tc_unify_tys_fg :: Bool
                -> BindFamFun -> BindTvFun
                -> [Type] -> [Type]
                -> UnifyResult
tc_unify_tys_fg match_kis bind_fam bind_tv tys1 tys2
  = do { (tv_env, _) <- tc_unify_tys bind_fam bind_tv
                                  True       -- Unifying
                                  False      -- Not doing an injectivity check
                                  match_kis  -- Match outer kinds
                                  RespectMultiplicities rn_env
                                  emptyTvSubstEnv emptyCvSubstEnv
                                  tys1 tys2
       ; return $ niFixSubst in_scope tv_env }
  where
    in_scope = mkInScopeSet $ tyCoVarsOfTypes tys1 `unionVarSet` tyCoVarsOfTypes tys2
    rn_env   = mkRnEnv2 in_scope

-- | This function is actually the one to call the unifier -- a little
-- too general for outside clients, though.
tc_unify_tys :: BindFamFun -> BindTvFun
             -> AmIUnifying -- ^ True <=> unify; False <=> match
             -> Bool        -- ^ True <=> doing an injectivity check
             -> Bool        -- ^ True <=> treat the kinds as well
             -> MultiplicityFlag -- ^ see Note [Rewrite rules ignore multiplicities in FunTy] in GHC.Core.Unify
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
tc_unify_tys bind_fam bind_tv unif inj_check match_kis match_mults rn_env tv_env cv_env tys1 tys2
  = initUM tv_env cv_env $
    do { when match_kis $
         unify_tys env kis1 kis2
       ; unify_tys env tys1 tys2 }
  where
    env = UMEnv { um_bind_tv_fun  = bind_tv
                , um_bind_fam_fun = bind_fam
                , um_foralls      = emptyVarSet
                , um_unif         = unif
                , um_inj_tf       = inj_check
                , um_arr_mult     = match_mults
                , um_rn_env       = rn_env }

    kis1 = map typeKind tys1
    kis2 = map typeKind tys2


{- *********************************************************************
*                                                                      *
                UnifyResult, MaybeApart etc
*                                                                      *
********************************************************************* -}

{- Note [Rewrite rules ignore multiplicities in FunTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following (higher-order) rule:

m :: Bool -> Bool -> Bool
{-# RULES "m" forall f. m (f True) = f #-}

let x = m ((,) @Bool @Bool True True)

The rewrite rule expects an `f :: Bool -> Bool`, but `(,) @Bool @Bool True ::
Bool %1 -> Bool` is linear (see Note [Data constructors are linear by default]
in GHC.Core.Multiplicity) Should the rule match? Yes! According to the
principles laid out in Note [Linting linearity] in GHC.Core.Lint, optimisation
shouldn't be constrained by linearity.

However, when matching the template variable `f` to `(,) True`, we do check that
their types unify (see Note [Matching variable types] in GHC.Core.Rules). So
when unifying types for the sake of rule-matching, the unification algorithm
must be able to ignore multiplicities altogether.

How is this done?
  (1) The `um_arr_mult` field of `UMEnv` recordsw when we are doing rule-matching,
      and hence want to ignore multiplicities.
  (2) The field is set to True in by `ruleMatchTyKiX`.
  (3) It is consulted when matching `FunTy` in `unify_ty`.

Wrinkle in (3). In `unify_tc_app`, in `unify_ty`, `FunTy` is handled as if it
was a regular type constructor. In this case, and when the types being unified
are *function* arrows, but not constraint arrows, then the first argument is a
multiplicity.

We select this situation by comparing the type constructor with fUNTyCon. In
this case, and this case only, we can safely drop the first argument (using the
tail function) and unify the rest.

Note [The substitution in MaybeApart]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The constructor MaybeApart carries data with it, typically a TvSubstEnv. Why?
Because consider unifying these:

(a, a, Int) ~ (b, [b], Bool)

If we go left-to-right, we start with [a |-> b]. Then, on the middle terms, we
apply the subst we have so far and discover that we need [b |-> [b]]. Because
this fails the occurs check, we say that the types are MaybeApart (see above
Note [Infinitary substitutions]). But, we can't stop there! Because if we
continue, we discover that Int is SurelyApart from Bool, and therefore the
types are apart. This has practical consequences for the ability for closed
type family applications to reduce. See test case
indexed-types/should_compile/Overlap14.
-}

-- This type does double-duty. It is used in the UM (unifier monad) and to
-- return the final result. See Note [Unification result]
type UnifyResult = UnifyResultM Subst

-- | See Note [Unification result]
data UnifyResultM a = Unifiable a        -- the subst that unifies the types
                    | MaybeApart MaybeApartReason
                                 a       -- the subst has as much as we know
                                         -- it must be part of a most general unifier
                                         -- See Note [The substitution in MaybeApart]
                    | SurelyApart
                    deriving Functor

-- | Why are two types 'MaybeApart'? 'MARInfinite' takes precedence:
-- This is used (only) in Note [Infinitary substitution in lookup] in GHC.Core.InstEnv
-- As of Feb 2022, we never differentiate between MARTypeFamily and MARTypeVsConstraint;
-- it's really only MARInfinite that's interesting here.
data MaybeApartReason
  = MARTypeFamily   -- ^ matching e.g. F Int ~? Bool

  | MARInfinite     -- ^ matching e.g. a ~? Maybe a

  | MARTypeVsConstraint  -- ^ matching Type ~? Constraint or the arrow types
    -- See Note [Type and Constraint are not apart] in GHC.Builtin.Types.Prim

  | MARCast         -- ^ Very obscure.
    -- See (KCU2) in Note [Kind coercions in Unify]


combineMAR :: MaybeApartReason -> MaybeApartReason -> MaybeApartReason
-- See (UR1) in Note [Unification result] for why MARInfinite wins
combineMAR MARInfinite         _ = MARInfinite   -- MARInfinite wins
combineMAR MARTypeFamily       r = r             -- Otherwise it doesn't really matter
combineMAR MARTypeVsConstraint r = r
combineMAR MARCast             r = r

instance Outputable MaybeApartReason where
  ppr MARTypeFamily       = text "MARTypeFamily"
  ppr MARInfinite         = text "MARInfinite"
  ppr MARTypeVsConstraint = text "MARTypeVsConstraint"
  ppr MARCast             = text "MARCast"

instance Semigroup MaybeApartReason where
  (<>) = combineMAR

instance Applicative UnifyResultM where
  pure  = Unifiable
  (<*>) = ap

instance Monad UnifyResultM where
  SurelyApart  >>= _ = SurelyApart
  MaybeApart r1 x >>= f = case f x of
                            Unifiable y     -> MaybeApart r1 y
                            MaybeApart r2 y -> MaybeApart (r1 S.<> r2) y
                            SurelyApart     -> SurelyApart
  Unifiable x  >>= f = f x

instance Outputable a => Outputable (UnifyResultM a) where
  ppr SurelyApart      = text "SurelyApart"
  ppr (Unifiable x)    = text "Unifiable" <+> ppr x
  ppr (MaybeApart r x) = text "MaybeApart" <+> ppr r <+> ppr x

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
This happened, for example, in #9106.

It gets worse.  In #14164 we wanted to take the fixpoint of
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

niFixSubst :: InScopeSet -> TvSubstEnv -> Subst
-- Find the idempotent fixed point of the non-idempotent substitution
-- This is surprisingly tricky:
--   see Note [Finding the substitution fixpoint]
-- ToDo: use laziness instead of iteration?
niFixSubst in_scope tenv
  | not_fixpoint = niFixSubst in_scope (mapVarEnv (substTy subst) tenv)
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
    subst = foldl' add_free_tv
                  (mkTvSubst in_scope tenv)
                  free_tvs

    add_free_tv :: Subst -> TyVar -> Subst
    add_free_tv subst tv
      = extendTvSubst subst tv (mkTyVarTy tv')
     where
        tv' = updateTyVarKind (substTy subst) tv

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
see Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep

Unlike the "impure unifiers" in the typechecker (the eager unifier in
GHC.Tc.Utils.Unify, and the constraint solver itself in GHC.Tc.Solver.Equality),
the pure unifier does /not/ work up to ~.

The algorithm implemented here is rather delicate, and we depend on it
to uphold certain properties. This is a summary of these required
properties.

Notation:
 θ,φ  substitutions
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
     after arbitrary type family reductions.

(I2) If (unify σ τ) = MaybeApart θ, and if some
     φ exists such that φ(σ) ~ φ(τ), then φ extends θ.


Furthermore, the RULES matching algorithm requires this property,
but only when using this algorithm for matching:

(M1) If (match σ τ) succeeds with θ, then all matchable tyvars
     in σ are bound in θ.

     Property M1 means that we must extend the substitution with,
     say (a ↦ a) when appropriate during matching.
     See also Note [Self-substitution when unifying or matching].

(M2) Completeness of matching.
     If θ(σ) = τ, then (match σ τ) = Unifiable φ,
     where θ is an extension of φ.

Wrinkle (SI1): um_inj_tf:
    Sadly, property M2 and I2 conflict. Consider

    type family F1 a b where
      F1 Int    Bool   = Char
      F1 Double String = Char

    Consider now two matching problems:

    P1. match (F1 a Bool) (F1 Int Bool)
    P2. match (F1 a Bool) (F1 Double String)

    In case P1, we must find (a ↦ Int) to satisfy M2.  In case P2, we must /not/
    find (a ↦ Double), in order to satisfy I2. (Note that the correct mapping for
    I2 is (a ↦ Int). There is no way to discover this, but we mustn't map a to
    anything else!)

    We thus must parameterize the algorithm over whether it's being used
    for an injectivity check (refrain from looking at non-injective arguments
    to type families) or not (do indeed look at those arguments).  This is
    implemented  by the um_inj_tf field of UMEnv.

    (It's all a question of whether or not to include equation (7) from Fig. 2
    of [ITF].)

    This extra parameter is a bit fiddly, perhaps, but seemingly less so than
    having two separate, almost-identical algorithms.

Note [Self-substitution when unifying or matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What happens when we are unifying or matching two identical type variables?
     a ~ a

* When /unifying/, just succeed, without binding [a :-> a] in the substitution,
  else we'd get an infinite substitution.  We need to make this check before
  we do the occurs check, of course.

* When /matching/, and `a` is a bindable variable from the template, we /do/
  want to extend the substitution.  Remember, a successful match should map all
  the template variables (except ones that disappear when expanding synonyms),

  But when `a` is /not/ a bindable variable (perhaps it is a globally-in-scope
  skolem) we want to treat it like a constant `Int ~ Int` and succeed.

  Notice: no occurs check!  It's fine to match (a ~ Maybe a), because the
  template vars of the template come from a different name space to the free
  vars of the target.

  Note that this arrangement was provoked by a real failure, where the same
  unique ended up in the template as in the target. (It was a rule firing when
  compiling Data.List.NonEmpty.)

* What about matching a /non-bindable/ variable?  For example:
      template-vars   : {a}
      matching problem: (forall b. b -> a) ~ (forall c. c -> Int)
  We want to emerge with the substitution [a :-> Int]
  But on the way we will encounter (b ~ b), when we match the bits before the
  arrow under the forall, having renamed `c` to `b`.  This match should just
  succeed, just like (Int ~ Int), without extending the substitution.

  It's important to do this for /non-bindable/ variables, not just for
  forall-bound ones.  In an associated type
         instance C (Maybe a) where {  type F (Maybe a) = Int }
  `checkConsistentFamInst` matches (Maybe a) from the header against (Maybe a)
  from the type-family instance, with `a` marked as non-bindable.


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

So, we pass a kind coercion `kco` to the main `unify_ty`. This coercion witnesses
the equality between the substed kind of the left-hand type and the substed
kind of the right-hand type. Note that we do not unify kinds at the leaves
(as we did previously).

Hence: (UKINV) Unification Kind Invariant
* In the call
     unify_ty ty1 ty2 kco
  it must be that
     subst(kco) :: subst(kind(ty1)) ~N subst(kind(ty2))
  where `subst` is the ambient substitution in the UM monad
* In the call
     unify_tys tys1 tys2
  (which has no kco), after we unify any prefix of tys1,tys2, the kinds of the
  head of the remaining tys1,tys2 are identical after substitution.  This
  implies, for example, that the kinds of the head of tys1,tys2 are identical
  after substitution.

Preserving (UKINV) takes a bit of work, governed by the `match_kis` flag in
`tc_unify_tys`:

* When we're working with type applications (either TyConApp or AppTy) we
  need to worry about establishing (UKINV), as the kinds of the function
  & arguments aren't (necessarily) included in the kind of the result.
  When unifying two TyConApps, this is easy, because the two TyCons are
  the same. Their kinds are thus the same. As long as we unify left-to-right,
  we'll be sure to unify types' kinds before the types themselves. (For example,
  think about Proxy :: forall k. k -> *. Unifying the first args matches up
  the kinds of the second args.)

* For AppTy, we must unify the kinds of the functions, but once these are
  unified, we can continue unifying arguments without worrying further about
  kinds.

* The interface to this module includes both "...Ty" functions and
  "...TyKi" functions. The former assume that (UKINV) is already
  established, either because the kinds are the same or because the
  list of types being passed in are the well-typed arguments to some
  type constructor (see two paragraphs above). The latter take a separate
  pre-pass over the kinds to establish (UKINV). Sometimes, it's important
  not to take the second pass, as it caused #12442.

Wrinkles

(KCU1) We ensure that the `kco` argument never mentions variables in the
  domain of either RnEnvL or RnEnvR.  Why?

  * `kco` is used only to build the final well-kinded substitution
         a :-> ty |> kco
    The range of the substitution never mentions forall-bound variables,
    so `kco` cannot either.

  * `kco` mixes up types from both left and right arguments of
    `unify_ty`, which have different renamings in the RnEnv2.

  The easiest thing is to insist that `kco` does not need renaming with
  the RnEnv2; it mentions no forall-bound variables.

  To achieve this we do a `mentionsForAllBoundTyVars` test in the
  `CastTy` cases of `unify_ty`.

(KCU2) Suppose we are unifying
            (forall a. x |> (...F a b...) ~ (forall a. y)
  We can't bind y :-> x |> (...F a b...), becuase of that free `a`.

  But if we later learn that b=Int, and F a Int = Bool,
  that free `a` might disappear, so we could unify with
      y :-> x |> (...Bool...)

  Conclusion: if there is a free forall-bound variable in a cast,
  return MaybeApart, with a MaybeApartReason of MARCast.

(KCU3) We thought, at one point, that this was all unnecessary: why should
    casts be in types in the first place? But they are sometimes. In
    dependent/should_compile/KindEqualities2, we see, for example the
    constraint Num (Int |> (blah ; sym blah)).  We naturally want to find
    a dictionary for that constraint, which requires dealing with
    coercions in this manner.

Note [Matching in the presence of casts (1)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

This all came up in #13910.  Because we match tycon arguments
left-to-right, the ambient substitution will already have a matching
substitution for any kinds; so there is an easy fix: just apply
the substitution-so-far to the coercion from the LHS.

Note that

* When matching, the first arg of unify_ty is always the template;
  we never swap round.

* The above argument is distressingly indirect. We seek a
  better way.

* One better way is to ensure that type patterns (the template
  in the matching process) have no casts.  See #14119.

Note [Matching in the presence of casts (2)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is another wrinkle (#17395).  Suppose (T :: forall k. k -> Type)
and we are matching
   tcMatchTy (T k (a::k))  (T j (b::j))

Then we'll match k :-> j, as expected. But then in unify_tys
we invoke
   unify_tys env (a::k) (b::j) (Refl j)

Although we have unified k and j, it's very important that we put
(Refl j), /not/ (Refl k) as the fourth argument to unify_tys.
If we put (Refl k) we'd end up with the substitution
  a :-> b |> Refl k
which is bogus because one of the template variables, k,
appears in the range of the substitution.  Eek.

Similar care is needed in unify_ty_app.


Note [Polykinded tycon applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose  T :: forall k. Type -> K
and we are unifying
  ty1:  T @Type         Int       :: Type
  ty2:  T @(Type->Type) Int Int   :: Type

These two TyConApps have the same TyCon at the front but they
(legitimately) have different numbers of arguments.  They
are surelyApart, so we can report that without looking any
further (see #15704).

Note [Unifying type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unifying type applications is quite subtle, as we found
in #23134 and #22647, when type families are involved.

Suppose
   type family F a :: Type -> Type
   type family G k :: k = r | r -> k

and consider these examples:

* F Int ~ F Char, where F is injective
  Since F is injective, we can reduce this to Int ~ Char,
  therefore SurelyApart.

* F Int ~ F Char, where F is not injective
  Without injectivity, return MaybeApart.

* G Type ~ G (Type -> Type) Int
  Even though G is injective and the arguments to G are different,
  we cannot deduce apartness because the RHS is oversaturated.
  For example, G might be defined as
    G Type = Maybe Int
    G (Type -> Type) = Maybe
  So we return MaybeApart.

* F Int Bool ~ F Int Char       -- SurelyApart (since Bool is apart from Char)
  F Int Bool ~ Maybe a          -- MaybeApart
  F Int Bool ~ a b              -- MaybeApart
  F Int Bool ~ Char -> Bool     -- MaybeApart
  An oversaturated type family can match an application,
  whether it's a TyConApp, AppTy or FunTy. Decompose.

* F Int ~ a b
  We cannot decompose a saturated, or under-saturated
  type family application. We return MaybeApart.

To handle all those conditions, unify_ty goes through
the following checks in sequence, where Fn is a type family
of arity n:

* (C1) Fn x_1 ... x_n ~ Fn y_1 .. y_n
  A saturated application.
  Here we can unify arguments in which Fn is injective.
* (C2) Fn x_1 ... x_n ~ anything, anything ~ Fn x_1 ... x_n
  A saturated type family can match anything - we return MaybeApart.
* (C3) Fn x_1 ... x_m ~ a b, a b ~ Fn x_1 ... x_m where m > n
  An oversaturated type family can be decomposed.
* (C4) Fn x_1 ... x_m ~ anything, anything ~ Fn x_1 ... x_m, where m > n
  If we couldn't decompose in the previous step, we return SurelyApart.

Afterwards, the rest of the code doesn't have to worry about type families.

Note [Unifying type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the task of unifying two 'Type's of the form

  TyConApp tc [] ~ TyConApp tc []

where `tc` is a type synonym. A naive way to perform this comparison these
would first expand the synonym and then compare the resulting expansions.

However, this is obviously wasteful and the RHS of `tc` may be large; it is
much better to rather compare the TyCons directly. Consequently, before
expanding type synonyms in type comparisons we first look for a nullary
TyConApp and simply compare the TyCons if we find one.

Of course, if we find that the TyCons are *not* equal then we still need to
perform the expansion as their RHSs may still be unifiable.  E.g
    type T = S (a->a)
    type S a = [a]
and consider
    T Int ~ S (Int -> Int)

We can't decompose non-nullary synonyms.  E.g.
    type R a = F a    -- Where F is a type family
and consider
    R (a->a) ~ R Int
We can't conclude that  (a->) ~ Int.  (There is a currently-missed opportunity
here; if we knew that R was /injective/, perhaps we could decompose.)

We perform the nullary-type-synonym optimisation in a number of places:

 * GHC.Core.Unify.unify_ty
 * GHC.Tc.Solver.Equality.can_eq_nc'
 * GHC.Tc.Utils.Unify.uType

This optimisation is especially helpful for the ubiquitous GHC.Types.Type,
since GHC prefers to use the type synonym over @TYPE 'LiftedRep@ applications
whenever possible. See Note [Using synonyms to compress types] in
GHC.Core.Type for details.

c.f. Note [Comparing type synonyms] in GHC.Core.TyCo.Compare
-}

-------------- unify_ty: the main workhorse -----------

type AmIUnifying = Bool   -- True  <=> Unifying
                          -- False <=> Matching

type InType      = Type       -- Before applying the RnEnv2
type OutCoercion = Coercion   -- After applying the RnEnv2


unify_ty :: UMEnv
         -> InType -> InType  -- Types to be unified
         -> OutCoercion       -- A nominal coercion between their kinds
                              -- OutCoercion: the RnEnv has already been applied
                              -- When matching, the coercion is in "target space",
                              --   not "template space"
                              -- See Note [Kind coercions in Unify]
         -> UM ()
-- Precondition: see (Unification Kind Invariant)
--
-- See Note [Specification of unification]
-- Respects newtypes, PredTypes
-- See Note [Computing equality on types] in GHC.Core.Type
unify_ty _env (TyConApp tc1 []) (TyConApp tc2 []) _kco
  -- See Note [Unifying type synonyms]
  | tc1 == tc2
  = return ()

unify_ty env ty1 ty2 kco
    -- Now handle the cases we can "look through": synonyms and casts.
  | Just ty1' <- coreView ty1 = unify_ty env ty1' ty2 kco
  | Just ty2' <- coreView ty2 = unify_ty env ty1 ty2' kco

unify_ty env (CastTy ty1 co1) ty2 kco
  | mentionsForAllBoundTyVarsL env (tyCoVarsOfCo co1)
    -- See (KCU1) in Note [Kind coercions in Unify]
  = maybeApart MARCast  -- See (KCU2)

  | um_unif env
  = unify_ty env ty1 ty2 (co1 `mkTransCo` kco)

  | otherwise -- We are matching, not unifying
  = do { subst <- getSubst env
       ; let co' = substCo subst co1
         -- We match left-to-right, so the free template vars of the
         -- coercion should already have been matched.
         -- See Note [Matching in the presence of casts (1)]
         -- NB: co1 does not mention forall-bound vars, so no need to rename
       ; unify_ty env ty1 ty2 (co' `mkTransCo` kco) }

unify_ty env ty1 (CastTy ty2 co2) kco
  | mentionsForAllBoundTyVarsR env (tyCoVarsOfCo co2)
    -- See (KCU1) in Note [Kind coercions in Unify]
  = maybeApart MARCast  -- See (KCU2)
  | otherwise
  = unify_ty env ty1 ty2 (kco `mkTransCo` mkSymCo co2)
    -- NB: co2 does not mention forall-bound variables

-- Applications need a bit of care!
-- They can match FunTy and TyConApp, so use splitAppTy_maybe
unify_ty env (AppTy ty1a ty1b) ty2 _kco
  | Just (ty2a, ty2b) <- tcSplitAppTyNoView_maybe ty2
  = unify_ty_app env ty1a [ty1b] ty2a [ty2b]

unify_ty env ty1 (AppTy ty2a ty2b) _kco
  | Just (ty1a, ty1b) <- tcSplitAppTyNoView_maybe ty1
  = unify_ty_app env ty1a [ty1b] ty2a [ty2b]

unify_ty _ (LitTy x) (LitTy y) _kco | x == y = return ()

unify_ty env (ForAllTy (Bndr tv1 _) ty1) (ForAllTy (Bndr tv2 _) ty2) kco
  -- ToDo: See Note [Unifying coercion-foralls]
  = do { unify_ty env (varType tv1) (varType tv2) (mkNomReflCo liftedTypeKind)
       ; let env' = umRnBndr2 env tv1 tv2
       ; unify_ty env' ty1 ty2 kco }

-- See Note [Matching coercion variables]
unify_ty env (CoercionTy co1) (CoercionTy co2) kco
  = do { c_subst <- getCvSubstEnv
       ; case co1 of
           CoVarCo cv
             | not (um_unif env)
             , not (cv `elemVarEnv` c_subst)   -- Not forall-bound
             , let (_mult_co, co_l, co_r) = decomposeFunCo kco
                     -- Because the coercion is used in a type, it should be safe to
                     -- ignore the multiplicity coercion, _mult_co
                      -- cv :: t1 ~ t2
                      -- co2 :: s1 ~ s2
                      -- co_l :: t1 ~ s1
                      -- co_r :: t2 ~ s2
                   rhs_co = co_l `mkTransCo` co2 `mkTransCo` mkSymCo co_r
             , BindMe <- um_bind_tv_fun env cv (CoercionTy rhs_co)
             -> if mentionsForAllBoundTyVarsR env (tyCoVarsOfCo co2)
                then surelyApart
                else extendCvEnv cv rhs_co

           _ -> return () }

unify_ty env (TyVarTy tv1) ty2 kco
  = uVarOrFam env (TyVarLHS tv1) ty2 kco

unify_ty env ty1 (TyVarTy tv2) kco
  | um_unif env  -- If unifying, can swap args; but not when matching
  = uVarOrFam (umSwapRn env) (TyVarLHS tv2) ty1 (mkSymCo kco)

-- Deal with TyConApps
unify_ty env ty1 ty2 kco
  -- Handle non-oversaturated type families first
  -- See Note [Unifying type applications]
  | Just (tc,tys) <- mb_sat_fam_app1
  = uVarOrFam env (TyFamLHS tc tys) ty2 kco

  | um_unif env
  , Just (tc,tys) <- mb_sat_fam_app2
  = uVarOrFam (umSwapRn env) (TyFamLHS tc tys) ty1 (mkSymCo kco)

  -- Handle oversaturated type families. Suppose we have
  --     (F a b) ~ (c d)    where F has arity 1
  -- We definitely want to decompose that type application! (#22647)
  --
  -- If there is no application, an oversaturated type family can only
  -- match a type variable or a saturated type family,
  -- both of which we handled earlier. So we can say surelyApart.
  | Just (tc1, _) <- mb_tc_app1
  , isTypeFamilyTyCon tc1
  = if | Just (ty1a, ty1b) <- tcSplitAppTyNoView_maybe ty1
       , Just (ty2a, ty2b) <- tcSplitAppTyNoView_maybe ty2
       -> unify_ty_app env ty1a [ty1b] ty2a [ty2b]            -- (C3)
       | otherwise -> surelyApart                             -- (C4)

  | Just (tc2, _) <- mb_tc_app2
  , isTypeFamilyTyCon tc2
  = if | Just (ty1a, ty1b) <- tcSplitAppTyNoView_maybe ty1
       , Just (ty2a, ty2b) <- tcSplitAppTyNoView_maybe ty2
       -> unify_ty_app env ty1a [ty1b] ty2a [ty2b]            -- (C3)
       | otherwise -> surelyApart                             -- (C4)

  -- At this point, neither tc1 nor tc2 can be a type family.
  | Just (tc1, tys1) <- mb_tc_app1
  , Just (tc2, tys2) <- mb_tc_app2
  , tc1 == tc2
  = do { massertPpr (isInjectiveTyCon tc1 Nominal) (ppr tc1)
       ; unify_tc_app env tc1 tys1 tys2
       }

  -- TYPE and CONSTRAINT are not Apart
  -- See Note [Type and Constraint are not apart] in GHC.Builtin.Types.Prim
  -- NB: at this point we know that the two TyCons do not match
  | Just (tc1,_) <- mb_tc_app1, let u1 = tyConUnique tc1
  , Just (tc2,_) <- mb_tc_app2, let u2 = tyConUnique tc2
  , (u1 == tYPETyConKey && u2 == cONSTRAINTTyConKey) ||
    (u2 == tYPETyConKey && u1 == cONSTRAINTTyConKey)
  = maybeApart MARTypeVsConstraint
    -- We don't bother to look inside; wrinkle (W3) in GHC.Builtin.Types.Prim
    -- Note [Type and Constraint are not apart]

  -- The arrow types are not Apart
  -- See Note [Type and Constraint are not apart] in GHC.Builtin.Types.Prim
  --     wrinkle (W2)
  -- NB1: at this point we know that the two TyCons do not match
  -- NB2: In the common FunTy/FunTy case you might wonder if we want to go via
  --      splitTyConApp_maybe.  But yes we do: we need to look at those implied
  --      kind argument in order to satisfy (Unification Kind Invariant)
  | FunTy {} <- ty1
  , FunTy {} <- ty2
  = maybeApart MARTypeVsConstraint
    -- We don't bother to look inside; wrinkle (W3) in GHC.Builtin.Types.Prim
    -- Note [Type and Constraint are not apart]

  where
    mb_tc_app1 = splitTyConApp_maybe ty1
    mb_tc_app2 = splitTyConApp_maybe ty2
    mb_sat_fam_app1 = isSatFamApp ty1
    mb_sat_fam_app2 = isSatFamApp ty2

unify_ty _ _ _ _ = surelyApart

-----------------------------
unify_tc_app :: UMEnv -> TyCon -> [Type] -> [Type] -> UM ()
-- Mainly just unifies the argument types;
-- but with a special case for fUNTyCon
unify_tc_app env tc tys1 tys2
  | tc == fUNTyCon
  , IgnoreMultiplicities <- um_arr_mult env
  , (_mult1 : no_mult_tys1) <- tys1
  , (_mult2 : no_mult_tys2) <- tys2
  = -- We're comparing function arrow types here (not constraint arrow
    -- types!), and they have at least one argument, which is the arrow's
    -- multiplicity annotation. The flag `um_arr_mult` instructs us to
    -- ignore multiplicities in this very case. This is a little tricky: see
    -- point (3) in Note [Rewrite rules ignore multiplicities in FunTy].
     unify_tys env no_mult_tys1 no_mult_tys2

  | otherwise
  = unify_tys env tys1 tys2

-----------------------------
unify_ty_app :: UMEnv -> Type -> [Type] -> Type -> [Type] -> UM ()
-- Deal with (t1 t1args) ~ (t2 t2args)
-- where   length t1args = length t2args
unify_ty_app env ty1 ty1args ty2 ty2args
  | Just (ty1', ty1a) <- splitAppTyNoView_maybe ty1
  , Just (ty2', ty2a) <- splitAppTyNoView_maybe ty2
  = unify_ty_app env ty1' (ty1a : ty1args) ty2' (ty2a : ty2args)

  | otherwise
  = do { let ki1 = typeKind ty1
             ki2 = typeKind ty2
           -- See Note [Kind coercions in Unify]
       ; unify_ty  env ki1 ki2 (mkNomReflCo liftedTypeKind)
       ; unify_ty  env ty1 ty2 (mkNomReflCo ki2)
                 -- Very important: 'ki2' not 'ki1'
                 -- See Note [Matching in the presence of casts (2)]
       ; unify_tys env ty1args ty2args }

-----------------------------
unify_tys :: UMEnv -> [Type] -> [Type] -> UM ()
-- Precondition: see (Unification Kind Invariant)
unify_tys env orig_xs orig_ys
  = go orig_xs orig_ys
  where
    go []     []     = return ()
    go (x:xs) (y:ys)
      -- See Note [Kind coercions in Unify]
      = do { unify_ty env x y (mkNomReflCo $ typeKind y)
                 -- Very important: 'y' not 'x'
                 -- See Note [Matching in the presence of casts (2)]
           ; go xs ys }
    go _ _ = surelyApart
      -- Possibly different saturations of a polykinded tycon
      -- See Note [Polykinded tycon applications]

---------------------------------
isSatFamApp :: Type -> Maybe (TyCon, [Type])
-- Return the argument if we have a saturated type family application
-- Why saturated?  See (ATF4) in Note [Apartness and type families]
isSatFamApp (TyConApp tc tys)
  |  isTypeFamilyTyCon tc
  && not (tys `lengthExceeds` tyConArity tc)  -- Not over-saturated
  = Just (tc, tys)
isSatFamApp _ = Nothing

---------------------------------
uVarOrFam :: UMEnv -> CanEqLHS -> InType -> OutCoercion -> UM ()
-- Invariants: (a) If ty1 is a TyFamLHS, then ty2 is NOT a TyVarTy
--             (b) both args have had coreView already applied
-- Why saturated?  See (ATF4) in Note [Apartness and type families]
uVarOrFam env ty1 ty2 kco
  = do { substs <- getSubstEnvs
       ; go NotSwapped substs ty1 ty2 kco }
  where
    -- `go` takes two bites at the cherry; if the first one fails
    -- it swaps the arguments and tries again; and then it fails.
    -- The SwapFlag argument tells `go` whether it is on the first
    -- bite (NotSwapped) or the second (IsSwapped).
    -- E.g.    a ~ F p q
    --         Starts with: go a (F p q)
    --         if `a` not bindable, swap to: go (F p q) a
    go swapped substs (TyVarLHS tv1) ty2 kco
      = go_tv swapped substs tv1 ty2 kco

    go swapped substs (TyFamLHS tc tys) ty2 kco
      = go_fam swapped substs tc tys ty2 kco

    -----------------------------
    -- go_tv: LHS is a type variable
    -- The sequence of tests is very similar to go_tv
    go_tv swapped substs tv1 ty2 kco
      | Just ty1' <- lookupVarEnv (um_tv_env substs) tv1'
      = -- We already have a substitution for tv1
        if | um_unif env                          -> unify_ty env ty1' ty2 kco
           | (ty1' `mkCastTy` kco) `tcEqType` ty2 -> return ()
           | otherwise                            -> surelyApart
           -- Unifying: recurse into unify_ty
           -- Matching: we /don't/ want to just recurse here, because the range of
           --    the subst is the target type, not the template type. So, just check
           --    for normal type equality.
           -- NB: it's important to use `tcEqType` instead of `eqType` here,
           -- otherwise we might not reject a substitution
           -- which unifies `Type` with `Constraint`, e.g.
           -- a call to tc_unify_tys with arguments
           --
           --   tys1 = [k,k]
           --   tys2 = [Type, Constraint]
           --
           -- See test cases: T11715b, T20521.

      -- If we are matching or unifying a ~ a, take care
      -- See Note [Self-substitution when unifying or matching]
      | TyVarTy tv2 <- ty2
      , let tv2' = umRnOccR env tv2
      , tv1' == tv2'
      = if | um_unif env     -> return ()
           | tv1_is_bindable -> extendTvEnv tv1' ty2
           | otherwise       -> return ()

      | tv1_is_bindable
      , not (mentionsForAllBoundTyVarsR env ty2_fvs)
            -- ty2_fvs: kco does not mention forall-bound vars
      , not occurs_check
      = -- No occurs check, nor skolem-escape; just bind the tv
        -- We don't need to rename `rhs` because it mentions no forall-bound vars
        extendTvEnv tv1' rhs     -- Bind tv1:=rhs and continue

      -- When unifying, try swapping:
      -- e.g.   a    ~ F p q       with `a` not bindable: we might succeed with go_fam
      -- e.g.   a    ~ beta        with `a` not bindable: we might be able to bind `beta`
      -- e.g.   beta ~ F beta Int  occurs check; but MaybeApart after swapping
      | um_unif env
      , NotSwapped <- swapped  -- If we have swapped already, don't do so again
      , Just lhs2 <- canEqLHS_maybe ty2
      = go IsSwapped substs lhs2 (mkTyVarTy tv1) (mkSymCo kco)

      | occurs_check = maybeApart MARInfinite   -- Occurs check
      | otherwise    = surelyApart

      where
        tv1'            = umRnOccL env tv1
        ty2_fvs         = tyCoVarsOfType ty2
        rhs_fvs         = ty2_fvs `unionVarSet` tyCoVarsOfCo kco
        rhs             = ty2 `mkCastTy` mkSymCo kco
        tv1_is_bindable | not (tv1' `elemVarSet` um_foralls env)
                          -- tv1' is not forall-bound, but tv1 can still differ
                          -- from tv1; see Note [Cloning the template binders]
                          -- in GHC.Core.Rules.  So give tv1' to um_bind_tv_fun.
                        , BindMe <- um_bind_tv_fun env tv1' rhs
                        = True
                        | otherwise
                        = False

        occurs_check = um_unif env &&
                       occursCheck (um_tv_env substs) tv1 rhs_fvs
          -- Occurs check, only when unifying
          -- see Note [Infinitary substitutions]
          -- Make sure you include `kco` in rhs_tvs #14846

    -----------------------------
    -- go_fam: LHS is a saturated type-family application
    -- Invariant: ty2 is not a TyVarTy
    go_fam swapped substs tc1 tys1 ty2 kco
      -- If we are under a forall, just give up and return MaybeApart
      -- see (ATF3) in Note [Apartness and type families]
      | not (isEmptyVarSet (um_foralls env))
      = maybeApart MARTypeFamily

      -- We are not under any foralls, so the RnEnv2 is empty
      -- Check if we have an existing substitution for the LHS; if so, recurse
      | Just ty1' <- lookupFamEnv (um_fam_env substs) tc1 tys1
      = if | um_unif env                          -> unify_ty env ty1' ty2 kco
           -- Below here we are matching
           -- The return () case deals with:
           --    Template:   (F a)..(F a)
           --    Target:     (F b)..(F b)
           -- This should match! With [a :-> b]
           | (ty1' `mkCastTy` kco) `tcEqType` ty2 -> return ()
           | otherwise                            -> maybeApart MARTypeFamily

      -- Check for equality  F tys1 ~ F tys2
      | Just (tc2, tys2) <- isSatFamApp ty2
      , tc1 == tc2
      = go_fam_fam tc1 tys1 tys2 kco

      -- Now check if we can bind the (F tys) to the RHS
      | BindMe <- um_bind_fam_fun env tc1 tys1 rhs
      = -- ToDo: do we need an occurs check here?
        do { extendFamEnv tc1 tys1 rhs
           ; maybeApart MARTypeFamily }

      -- Swap in case of (F a b) ~ (G c d e)
      -- Maybe um_bind_fam_fun is False of (F a b) but true of (G c d e)
      -- NB: a type family can appear on the template when matching
      --     see (ATF6) in Note [Apartness and type families]
      | um_unif env
      , NotSwapped <- swapped
      , Just lhs2 <- canEqLHS_maybe ty2
      = go IsSwapped substs lhs2 (mkTyConApp tc1 tys1) (mkSymCo kco)

      | otherwise   -- See (ATF5) in Note [Apartness and type families]
      = surelyApart

      where
        rhs = ty2 `mkCastTy` mkSymCo kco

    -----------------------------
    -- go_fam_fam: LHS and RHS are both saturated type-family applications,
    --             for the same type-family F
    go_fam_fam tc tys1 tys2 kco
      | tcEqTyConAppArgs tys1 tys2
      -- Detect (F tys ~ F tys); otherwise we'd build an infinite substitution
      = return ()

      | otherwise
       -- Decompose (F tys1 ~ F tys2): (ATF9)
       -- Use injectivity information of F: (ATF10)
       -- But first bind the type-fam if poss: (ATF11)
      = do { bind_fam_if_poss                 -- (ATF11)
           ; unify_tys env inj_tys1 inj_tys2  -- (ATF10)
           ; unless (um_inj_tf env) $         -- (ATF12)
             don'tBeSoSure MARTypeFamily $    -- (ATF9)
             unify_tys env noninj_tys1 noninj_tys2 }
     where
       inj = case tyConInjectivityInfo tc of
                NotInjective -> repeat False
                Injective bs -> bs

       (inj_tys1, noninj_tys1) = partitionByList inj tys1
       (inj_tys2, noninj_tys2) = partitionByList inj tys2

       bind_fam_if_poss | BindMe <- um_bind_fam_fun env tc tys1 rhs1
                        = extendFamEnv tc tys1 rhs1
                        | um_unif env
                        , BindMe <- um_bind_fam_fun env tc tys2 rhs2
                        = extendFamEnv tc tys2 rhs2
                        | otherwise
                        = return ()
       rhs1 = mkTyConApp tc tys2 `mkCastTy` mkSymCo kco
       rhs2 = mkTyConApp tc tys1 `mkCastTy` kco


occursCheck :: TvSubstEnv -> TyVar -> TyCoVarSet -> Bool
occursCheck env tv1 tvs
  = anyVarSet bad tvs
  where
    bad tv | Just ty <- lookupVarEnv env tv
           = anyVarSet bad (tyCoVarsOfType ty)
           | otherwise
           = tv == tv1

{- Note [Unifying coercion-foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we try to unify (forall cv. t1) ~ (forall cv. t2).
See Note [ForAllTy] in GHC.Core.TyCo.Rep.

The problem with coercion variables is that coercion abstraction is not erased:
the `kco` shouldn't propagate from outside the ForAllTy to inside. Instead, I think
the correct new `kco` for the recursive call is `mkNomReflCo liftedTypeKind` (but I'm
a little worried it might be Constraint sometimes).

This potential problem has been there a long time, and I'm going to let
sleeping dogs lie for now.
-}

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
            -- See (SI1) in Note [Specification of unification]

          , um_arr_mult :: MultiplicityFlag
            -- Whether to unify multiplicity arguments when unifying arrows.
            -- See Note [Rewrite rules ignore multiplicities in FunTy]

          , um_rn_env :: RnEnv2
            -- Renaming InTyVars to OutTyVars; this eliminates shadowing, and
            -- lines up matching foralls on the left and right
            -- See (CU2) in Note [The Core unifier]

          , um_foralls :: TyVarSet
            -- OutTyVars bound by a forall in this unification;
            -- Do not bind these in the substitution!
            -- See the function tvBindFlag

          , um_bind_tv_fun :: BindTvFun
            -- User-supplied BindFlag function, for variables not in um_foralls
            -- See (CU1) in Note [The Core unifier]

          , um_bind_fam_fun :: BindFamFun
            -- Similar to um_bind_tv_fun, but for type-family applications
            -- See (ATF8) in Note [Apartness and type families]
          }

type FamSubstEnv = TyConEnv (ListMap TypeMap Type)
  -- Map a TyCon and a list of types to a type
  -- Domain of FamSubstEnv is exactly-saturated type-family
  -- applications (F t1...tn)

lookupFamEnv :: FamSubstEnv -> TyCon -> [Type] -> Maybe Type
lookupFamEnv env tc tys
  = do { tys_map <- lookupTyConEnv env tc
       ; lookupTM tys tys_map }

data UMState = UMState
                   { um_tv_env   :: TvSubstEnv
                   , um_cv_env   :: CvSubstEnv
                   , um_fam_env  :: FamSubstEnv }
  -- um_tv_env, um_cv_env, um_fam_env are all "global" substitutions;
  -- that is, neither their domains nor their ranges mention any variables
  -- in um_foralls; i.e. variables bound by foralls inside the types being unified

  -- When /matching/ um_fam_env is usually empty; but not quite always.
  -- See (ATF6) and (ATF7) of Note [Apartness and type families]

newtype UM a
  = UM' { unUM :: UMState -> UnifyResultM (UMState, a) }
    -- See Note [The one-shot state monad trick] in GHC.Utils.Monad

pattern UM :: (UMState -> UnifyResultM (UMState, a)) -> UM a
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern UM m <- UM' m
  where
    UM m = UM' (oneShot m)
{-# COMPLETE UM #-}

instance Functor UM where
  fmap f (UM m) = UM (\s -> fmap (\(s', v) -> (s', f v)) (m s))

instance Applicative UM where
      pure a = UM (\s -> pure (s, a))
      (<*>)  = ap

instance Monad UM where
  {-# INLINE (>>=) #-}
  -- See Note [INLINE pragmas and (>>)] in GHC.Utils.Monad
  m >>= k  = UM (\state ->
                  do { (state', v) <- unUM m state
                     ; unUM (k v) state' })

instance MonadFail UM where
    fail _   = UM (\_ -> SurelyApart) -- failed pattern match

initUM :: TvSubstEnv  -- subst to extend
       -> CvSubstEnv
       -> UM ()
       -> UnifyResultM (TvSubstEnv, CvSubstEnv)
initUM subst_env cv_subst_env um
  = case unUM um state of
      Unifiable (state, _)    -> Unifiable (get state)
      MaybeApart r (state, _) -> MaybeApart r (get state)
      SurelyApart             -> SurelyApart
  where
    state = UMState { um_tv_env = subst_env
                    , um_cv_env = cv_subst_env
                    , um_fam_env = emptyTyConEnv }
    get (UMState { um_tv_env = tv_env, um_cv_env = cv_env }) = (tv_env, cv_env)

getTvSubstEnv :: UM TvSubstEnv
getTvSubstEnv = UM $ \state -> Unifiable (state, um_tv_env state)

getCvSubstEnv :: UM CvSubstEnv
getCvSubstEnv = UM $ \state -> Unifiable (state, um_cv_env state)

getSubstEnvs :: UM UMState
getSubstEnvs = UM $ \state -> Unifiable (state, state)

getSubst :: UMEnv -> UM Subst
getSubst env = do { tv_env <- getTvSubstEnv
                  ; cv_env <- getCvSubstEnv
                  ; let in_scope = rnInScopeSet (um_rn_env env)
                  ; return (mkTCvSubst in_scope tv_env cv_env) }

extendTvEnv :: TyVar -> Type -> UM ()
extendTvEnv tv ty = UM $ \state ->
  Unifiable (state { um_tv_env = extendVarEnv (um_tv_env state) tv ty }, ())

extendCvEnv :: CoVar -> Coercion -> UM ()
extendCvEnv cv co = UM $ \state ->
  Unifiable (state { um_cv_env = extendVarEnv (um_cv_env state) cv co }, ())

extendFamEnv :: TyCon -> [Type] -> Type -> UM ()
extendFamEnv tc tys ty = UM $ \state ->
  Unifiable (state { um_fam_env = extend (um_fam_env state) tc }, ())
  where
    extend :: FamSubstEnv -> TyCon -> FamSubstEnv
    extend = alterTyConEnv alter_tm

    alter_tm :: Maybe (ListMap TypeMap Type) -> Maybe (ListMap TypeMap Type)
    alter_tm m_elt = Just (alterTM tys (\_ -> Just ty) (m_elt `orElse` emptyTM))

umRnBndr2 :: UMEnv -> TyCoVar -> TyCoVar -> UMEnv
umRnBndr2 env v1 v2
  = env { um_rn_env = rn_env', um_foralls = um_foralls env `extendVarSet` v' }
  where
    (rn_env', v') = rnBndr2_var (um_rn_env env) v1 v2

mentionsForAllBoundTyVarsL, mentionsForAllBoundTyVarsR :: UMEnv -> VarSet -> Bool
-- See (CU2) in Note [The Core unifier]
mentionsForAllBoundTyVarsL = mentions_forall_bound_tvs inRnEnvL
mentionsForAllBoundTyVarsR = mentions_forall_bound_tvs inRnEnvR

mentions_forall_bound_tvs :: (RnEnv2 -> TyVar -> Bool) -> UMEnv -> VarSet -> Bool
mentions_forall_bound_tvs in_rn_env env varset
  | isEmptyVarSet (um_foralls env)               = False
  | anyVarSet (in_rn_env (um_rn_env env)) varset = True
  | otherwise                                    = False
    -- NB: That isEmptyVarSet guard is a critical optimization;
    -- it means we don't have to calculate the free vars of
    -- the type, often saving quite a bit of allocation.

-- | Converts any SurelyApart to a MaybeApart
don'tBeSoSure :: MaybeApartReason -> UM () -> UM ()
don'tBeSoSure r um = UM $ \ state ->
  case unUM um state of
    SurelyApart -> MaybeApart r (state, ())
    other       -> other

umRnOccL :: UMEnv -> TyVar -> TyVar
umRnOccL env v = rnOccL (um_rn_env env) v

umRnOccR :: UMEnv -> TyVar -> TyVar
umRnOccR env v = rnOccR (um_rn_env env) v

umSwapRn :: UMEnv -> UMEnv
umSwapRn env = env { um_rn_env = rnSwap (um_rn_env env) }

maybeApart :: MaybeApartReason -> UM ()
maybeApart r = UM (\state -> MaybeApart r (state, ()))

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
       ; return (LC (mkEmptySubst in_scope) cenv2) }
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
            -> Coercion   -- ^ co :: lty ~r rty, coercion to match against
            -> Coercion   -- ^ :: kind(lsubst(ty)) ~N kind(lty)
            -> Coercion   -- ^ :: kind(rsubst(ty)) ~N kind(rty)
            -> Maybe LiftCoEnv
   -- ^ Just env ==> liftCoSubst Nominal env ty == co, modulo roles.
   -- Also: Just env ==> lsubst(ty) == lty and rsubst(ty) == rty,
   -- where lsubst = lcSubstLeft(env) and rsubst = lcSubstRight(env)
ty_co_match menv subst ty co lkco rkco
  | Just ty' <- coreView ty = ty_co_match menv subst ty' co lkco rkco

  -- handle Refl case:
  | tyCoVarsOfType ty `isNotInDomainOf` subst
  , Just (ty', _) <- isReflCo_maybe co
  , ty `eqType` ty'
    -- Why `eqType` and not `tcEqType`? Because this function is only used
    -- during coercion optimisation, after type-checking has finished.
  = Just subst

  where
    isNotInDomainOf :: VarSet -> VarEnv a -> Bool
    isNotInDomainOf set env
      = noneSet (\v -> elemVarEnv v env) set

    noneSet :: (Var -> Bool) -> VarSet -> Bool
    noneSet f = allVarSet (not . f)

ty_co_match menv subst ty co lkco rkco
  | CastTy ty' co' <- ty
     -- See Note [Matching in the presence of casts (1)]
  = let empty_subst  = mkEmptySubst (rnInScopeSet (me_env menv))
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
                castCoercionKind co (mkSymCo lkco) (mkSymCo rkco)

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
  | Just (ty1a, ty1b) <- splitAppTyNoView_maybe ty1
       -- yes, the one from Type, not TcType; this is for coercion optimization
  = ty_co_match_app menv subst ty1a [ty1b] co2 [arg2]

ty_co_match menv subst (TyConApp tc1 tys) (TyConAppCo _ tc2 cos) _lkco _rkco
  = ty_co_match_tc menv subst tc1 tys tc2 cos

ty_co_match menv subst (FunTy { ft_mult = w, ft_arg = ty1, ft_res = ty2 })
            (FunCo { fco_mult = co_w, fco_arg = co1, fco_res = co2 }) _lkco _rkco
  = ty_co_match_args menv subst [w,    rep1,    rep2,    ty1, ty2]
                                [co_w, co1_rep, co2_rep, co1, co2]
  where
     rep1    = getRuntimeRep ty1
     rep2    = getRuntimeRep ty2
     co1_rep = mkRuntimeRepCo co1
     co2_rep = mkRuntimeRepCo co2
    -- NB: we include the RuntimeRep arguments in the matching;
    --     not doing so caused #21205.

ty_co_match menv subst (ForAllTy (Bndr tv1 vis1t) ty1)
                       (ForAllCo tv2 vis1c vis2c kind_co2 co2)
                       lkco rkco
  | isTyVar tv1 && isTyVar tv2
  , vis1t == vis1c && vis1c == vis2c -- Is this necessary?
      -- Is this visibility check necessary?  @rae says: yes, I think the
      -- check is necessary, if we're caring about visibility (and we are).
      -- But ty_co_match is a dark and not important corner.
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
--        eta1      = mkSelCo (SelTyCon 2 role) (downgradeRole r Nominal kind_co2)
--                 :: s1' ~ t1
--        eta2      = mkSelCo (SelTyCon 3 role) (downgradeRole r Nominal kind_co2)
--                 :: s2' ~ t2
--      Wanted:
--        subst1 <- ty_co_match menv subst  s1 eta1 kco1 kco2
--        subst2 <- ty_co_match menv subst1 s2 eta2 kco3 kco4
--      Question: How do we get kcoi?
--   2. Given:
--        lkco :: <*>    -- See Note [Weird typing rule for ForAllTy] in GHC.Core.TyCo.Rep
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
       ; ty_co_match_args menv subst tys1 cos2 }

ty_co_match_app :: MatchEnv -> LiftCoEnv
                -> Type -> [Type] -> Coercion -> [Coercion]
                -> Maybe LiftCoEnv
ty_co_match_app menv subst ty1 ty1args co2 co2args
  | Just (ty1', ty1a) <- splitAppTyNoView_maybe ty1
  , Just (co2', co2a) <- splitAppCo_maybe co2
  = ty_co_match_app menv subst ty1' (ty1a : ty1args) co2' (co2a : co2args)

  | otherwise
  = do { subst1 <- ty_co_match menv subst ki1 ki2 ki_ki_co ki_ki_co
       ; let Pair lkco rkco = mkNomReflCo <$> coercionKind ki2
       ; subst2 <- ty_co_match menv subst1 ty1 co2 lkco rkco
       ; ty_co_match_args menv subst2 ty1args co2args }
  where
    ki1 = typeKind ty1
    ki2 = promoteCoercion co2
    ki_ki_co = mkNomReflCo liftedTypeKind

ty_co_match_args :: MatchEnv -> LiftCoEnv -> [Type] -> [Coercion]
                 -> Maybe LiftCoEnv
ty_co_match_args menv subst (ty:tys) (arg:args)
  = do { let Pair lty rty = coercionKind arg
             lkco = mkNomReflCo (typeKind lty)
             rkco = mkNomReflCo (typeKind rty)
       ; subst' <- ty_co_match menv subst ty arg lkco rkco
       ; ty_co_match_args menv subst' tys args }
ty_co_match_args _    subst []       [] = Just subst
ty_co_match_args _    _     _        _  = Nothing

pushRefl :: Coercion -> Maybe Coercion
pushRefl co =
  case (isReflCo_maybe co) of
    Just (AppTy ty1 ty2, Nominal)
      -> Just (AppCo (mkReflCo Nominal ty1) (mkNomReflCo ty2))
    Just (FunTy af w ty1 ty2, r)
      ->  Just (FunCo r af af (mkReflCo r w) (mkReflCo r ty1) (mkReflCo r ty2))
    Just (TyConApp tc tys, r)
      -> Just (TyConAppCo r tc (zipWith mkReflCo (tyConRoleListX r tc) tys))
    Just (ForAllTy (Bndr tv vis) ty, r)
      -> Just (ForAllCo { fco_tcv = tv, fco_visL = vis, fco_visR = vis
                        , fco_kind = mkNomReflCo (varType tv)
                        , fco_body = mkReflCo r ty })
    _ -> Nothing
