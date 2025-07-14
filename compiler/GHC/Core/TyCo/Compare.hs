-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

-- | Type equality and comparison
module GHC.Core.TyCo.Compare (

    -- * Type equality
    eqType, eqTypeIgnoringMultiplicity, eqTypeX, eqTypes,
    eqVarBndrs,

    pickyEqType, tcEqType, tcEqKind, tcEqTypeNoKindCheck,
    tcEqTyConApps, tcEqTyConAppArgs,

    -- ** Dealing with invisible bits in types
    mayLookIdentical, pprWithInvisibleBits,
    InvisibleBit(..), InvisibleBits,

    -- * Type comparison
    nonDetCmpType,

   -- * Visiblity comparision
   eqForAllVis, cmpForAllVis

   ) where

import GHC.Prelude

import GHC.Core.Type( typeKind, coreView, tcSplitAppTyNoView_maybe, splitAppTyNoView_maybe
                    , isLevityTy, isRuntimeRepTy, isMultiplicityTy )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCon
import GHC.Core.Multiplicity( MultiplicityFlag(..) )

import GHC.Types.Var
import GHC.Types.Unique
import GHC.Types.Var.Env
import GHC.Types.Var.Set

import {-# SOURCE #-} GHC.Tc.Utils.TcType (isMetaTyVar)

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Base (reallyUnsafePtrEquality#)

import Control.Monad ( unless )
import Control.Monad.Trans.Writer.CPS ( Writer )
import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.Semigroup as S
import Data.Set ( Set )
import qualified Data.Set as Set

{- GHC.Core.TyCo.Compare overview
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements type equality and comparison

It uses a few functions from GHC.Core.Type, notably `typeKind`,
so it currently sits "on top of" GHC.Core.Type.
-}

{- *********************************************************************
*                                                                      *
                       Type equality

     We don't use (==) from class Eq, partly so that we know where
     type equality is called, and partly because there are multiple
     variants.
*                                                                      *
********************************************************************* -}

{- Note [Computing equality on types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements type equality, notably `eqType`. This is
"definitional equality" or just "equality" for short.

There are several places within GHC that depend on the precise choice of
definitional equality used. If we change that definition, all these places
must be updated. This Note merely serves as a place for all these places
to refer to, so searching for references to this Note will find every place
that needs to be updated.

* See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep.

* See Historical Note [Typechecker equality vs definitional equality]
  below

Note [Casts and coercions in type comparision]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As (EQTYPE) in Note [Non-trivial definitional equality] says, our
general plan, implemented by `fullEq`, is:
 (1) ignore both casts and coercions when comparing types,
 (2) instead, compare the /kinds/ of the two types,
     as well as the types themselves

If possible we want to avoid step (2), comparing the kinds; doing so involves
calling `typeKind` and doing another comparision.

When can we avoid doing so?  Answer: we can certainly avoid doing so if the
types we are comparing have no casts or coercions.  But we can do better.
Consider
    eqType (TyConApp T [s1, ..., sn]) (TyConApp T [t1, .., tn])
We are going to call (eqType s1 t1), (eqType s2 t2) etc.

The kinds of `s1` and `t1` must be equal, because these TyConApps are well-kinded,
and both TyConApps are headed by the same T. So the first recursive call to `eqType`
certainly doesn't need to check kinds. If that call returns False, we stop. Otherwise,
we know that `s1` and `t1` are themselves equal (not just their kinds). This
makes the kinds of `s2` and `t2` to be equal, because those kinds come from the
kind of T instantiated with `s1` and `t1` -- which are the same. Thus we do not
need to check the kinds of `s2` and `t2`. By induction, we don't need to check
the kinds of *any* of the types in a TyConApp, and we also do not need to check
the kinds of the TyConApps themselves.

Conclusion:

* casts and coercions under a TyConApp don't matter -- even including type synonyms

* In step (2), use `hasCasts` to tell if there are any casts to worry about. It
  does not look very deep, because TyConApps and FunTys are so common, and it
  doesn't allocate.  The only recursive cases are AppTy and ForAllTy.

Alternative implementation.  Instead of `hasCasts`, we could make the
generic_eq_type function return
  data EqResult = NotEq | EqWithNoCasts | EqWithCasts
Practically free; but stylistically I prefer useing `hasCasts`:
  * `generic_eq_type` can just uses familiar booleans
  * There is a lot more branching with the three-value variant.
  * It separates concerns.  No need to think about cast-tracking when doing the
    equality comparison.
  * Indeed sometimes we omit the kind check unconditionally, so tracking it is just wasted
    work.
I did try both; there was no perceptible perf difference so I chose `hasCasts` version.

Note [Equality on AppTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
In our cast-ignoring equality, we want to say that the following two
are equal:

  (Maybe |> co) (Int |> co')   ~?       Maybe Int

But the left is an AppTy while the right is a TyConApp. The solution is
to use splitAppTyNoView_maybe to break up the TyConApp into its pieces and
then continue. Easy to do, but also easy to forget to do.

Note [Comparing type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the task of testing equality between two 'Type's of the form

  TyConApp tc tys1  =  TyConApp tc tys2

where `tc` is a type synonym. A naive way to perform this comparison these
would first expand the synonym and then compare the resulting expansions.

However, this is obviously wasteful and the RHS of `tc` may be large. We'd
prefer to compare `tys1 = tys2`.  When is that sound?  Precisely when the
synonym is not /forgetful/; that is, all its type variables appear in its
RHS -- see `GHC.Core.TyCon.isForgetfulSynTyCon`.

Of course, if we find that the TyCons are *not* equal then we still need to
perform the expansion as their RHSs may still be equal.

This works fine for /equality/, but not for /comparison/.  Consider
   type S a b = (b, a)
Now consider
   S Int Bool `compare` S Char Char
The ordering may depend on whether we expand the synonym or not, and we
don't want the result to depend on that. So for comparison we stick to
/nullary/ synonyms only, which is still useful.

We perform this optimisation in a number of places:

 * GHC.Core.TyCo.Compare.eqType      (works for non-nullary synonyms)
 * GHC.Core.Map.TYpe.eqDeBruijnType  (works for non-nullary synonyms)
 * GHC.Core.Types.nonDetCmpType      (nullary only)

This optimisation is especially helpful for the ubiquitous GHC.Types.Type,
since GHC prefers to use the type synonym over @TYPE 'LiftedRep@ applications
whenever possible. See Note [Using synonyms to compress types] in
GHC.Core.Type for details.

Currently-missed opportunity (#25009):
* In the case of forgetful synonyms, we could still compare the args, pairwise,
  and then compare the RHS's with a suitably extended RnEnv2.  That would avoid
  comparing the same arg repeatedly.  e.g.
      type S a b = (a,a)
  Compare   S <big> y ~ S <big> y
  If we expand, we end up compare <big> with itself twice.

  But since forgetful synonyms are rare, we have not tried this.

Note [Type comparisons using object pointer comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quite often we substitute the type from a definition site into
occurrences without a change. This means for code like:
    \x -> (x,x,x)
The type of every `x` will often be represented by a single object
in the heap. We can take advantage of this by shortcutting the equality
check if two types are represented by the same pointer under the hood.
In some cases this reduces compiler allocations by ~2%.

See Note [Pointer comparison operations] in GHC.Builtin.primops.txt.pp

Note [Respecting multiplicity when comparing types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, we respect multiplicities (i.e. the linear part of the type
system) when comparing types.  Doing so is of course crucial during typechecking.

But for reasons described in Note [Linting linearity] in GHC.Core.Lint, it is hard
to ensure that Core is always type-correct when it comes to linearity.  So
* `eqTypeIgnoringMultiplicity` provides a way to compare types that /ignores/ multiplicities
* We use this multiplicity-blind comparison very occasionally, notably
    - in Core Lint: see Note [Linting linearity] in GHC.Core.Lint
    - in rule matching: see Note [Rewrite rules ignore multiplicities in FunTy]
      in GHC.Core.Unify
-}


tcEqKind :: HasDebugCallStack => Kind -> Kind -> Bool
tcEqKind = tcEqType

tcEqType :: HasDebugCallStack => Type -> Type -> Bool
tcEqType = eqType

-- | Just like 'tcEqType', but will return True for types of different kinds
-- as long as their non-coercion structure is identical.
tcEqTypeNoKindCheck :: Type -> Type -> Bool
tcEqTypeNoKindCheck = eqTypeNoKindCheck

-- | Check whether two TyConApps are the same; if the number of arguments
-- are different, just checks the common prefix of arguments.
tcEqTyConApps :: TyCon -> [Type] -> TyCon -> [Type] -> Bool
tcEqTyConApps tc1 args1 tc2 args2
  = tc1 == tc2 && tcEqTyConAppArgs args1 args2

tcEqTyConAppArgs :: [Type] -> [Type] -> Bool
-- Args do not have to have equal length;
-- we discard the excess of the longer one
tcEqTyConAppArgs args1 args2
  = and (zipWith tcEqTypeNoKindCheck args1 args2)
    -- No kind check necessary: if both arguments are well typed, then
    -- any difference in the kinds of later arguments would show up
    -- as differences in earlier (dependent) arguments

-- | Type equality on lists of types, looking through type synonyms
eqTypes :: [Type] -> [Type] -> Bool
eqTypes []       []       = True
eqTypes (t1:ts1) (t2:ts2) = eqType t1 t2 && eqTypes ts1 ts2
eqTypes _        _        = False

eqVarBndrs :: HasCallStack => RnEnv2 -> [Var] -> [Var] -> Maybe RnEnv2
-- Check that the var lists are the same length
-- and have matching kinds; if so, extend the RnEnv2
-- Returns Nothing if they don't match
eqVarBndrs env [] []
 = Just env
eqVarBndrs env (tv1:tvs1) (tv2:tvs2)
 | eqTypeX env (varType tv1) (varType tv2)
 = eqVarBndrs (rnBndr2 env tv1 tv2) tvs1 tvs2
eqVarBndrs _ _ _= Nothing

initRnEnv :: Type -> Type -> RnEnv2
initRnEnv ta tb = mkRnEnv2 $ mkInScopeSet $
                  tyCoVarsOfType ta `unionVarSet` tyCoVarsOfType tb

eqTypeNoKindCheck :: Type -> Type -> Bool
eqTypeNoKindCheck ty1 ty2 = eq_type_expand_respect ty1 ty2

-- | Type equality comparing both visible and invisible arguments,
-- expanding synonyms and respecting multiplicities.
eqType :: HasCallStack => Type -> Type -> Bool
eqType ta tb = fullEq eq_type_expand_respect ta tb

-- | Compare types with respect to a (presumably) non-empty 'RnEnv2'.
eqTypeX :: HasCallStack => RnEnv2 -> Type -> Type -> Bool
eqTypeX env ta tb = fullEq (eq_type_expand_respect_x env) ta tb

eqTypeIgnoringMultiplicity :: Type -> Type -> Bool
-- See Note [Respecting multiplicity when comparing types]
eqTypeIgnoringMultiplicity ta tb = fullEq eq_type_expand_ignore ta tb

-- | Like 'pickyEqTypeVis', but returns a Bool for convenience
pickyEqType :: Type -> Type -> Bool
-- Check when two types _look_ the same, _including_ synonyms.
-- So (pickyEqType String [Char]) returns False
-- This ignores kinds and coercions, because this is used only for printing.
pickyEqType ta tb = eq_type_keep_respect ta tb

{- Note [Specialising type equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type equality predicates in Type are hit pretty hard by GHC.  Consequently
we take pains to ensure that these paths are compiled to efficient,
minimally-allocating code.  Plan:

* The main workhorse is `inline_generic_eq_type_x`.  It is /non-recursive/
  and is marked INLINE.

* `inline_generic_eq_type_x` has various parameters that control what it does:
  * syn_flag::SynFlag            whether type synonyms are expanded or kept.
  * mult_flag::MultiplicityFlag  whether multiplicities are ignored or respected
  * mb_env::Maybe RnEnv2         an optional RnEnv2.

* `inline_generic_eq_type_x` has a handful of call sites, namely the ones
  in `eq_type_expand_respect`, `eq_type_expand_repect_x` etc.  It inlines
  at all these sites, specialising to the data values passed for the
  control parameters.

* All /other/ calls to `inline_generic_eq_type_x` go via
     generic_eq_type_x = inline_generic_eq_type_x
     {-# NOINLNE generic_eq_type_x #-}
  The idea is that all calls to `generic_eq_type_x` are specialised by the
  RULES, so this NOINLINE version is seldom, if ever, actually called.

* For each of specialised copy of `inline_generic_eq_type_x, there is a
  corresponding rewrite RULE that rewrites a call to (generic_eq_type_x args)
  into the appropriate specialied version.

See #19226.
-}

-- | This flag controls whether we expand synonyms during comparison
data SynFlag = ExpandSynonyms | KeepSynonyms

eq_type_expand_respect, eq_type_expand_ignore, eq_type_keep_respect
  :: Type -> Type -> Bool
eq_type_expand_respect_x, eq_type_expand_ignore_x, eq_type_keep_respect_x
   :: RnEnv2 -> Type -> Type -> Bool

eq_type_expand_respect       = inline_generic_eq_type_x ExpandSynonyms RespectMultiplicities Nothing
eq_type_expand_respect_x env = inline_generic_eq_type_x ExpandSynonyms RespectMultiplicities (Just env)
eq_type_expand_ignore        = inline_generic_eq_type_x ExpandSynonyms IgnoreMultiplicities  Nothing
eq_type_expand_ignore_x env  = inline_generic_eq_type_x ExpandSynonyms IgnoreMultiplicities  (Just env)
eq_type_keep_respect         = inline_generic_eq_type_x KeepSynonyms   RespectMultiplicities Nothing
eq_type_keep_respect_x env   = inline_generic_eq_type_x KeepSynonyms   RespectMultiplicities (Just env)

{-# RULES
"eqType1" generic_eq_type_x ExpandSynonyms RespectMultiplicities Nothing
             = eq_type_expand_respect
"eqType2" forall env. generic_eq_type_x ExpandSynonyms RespectMultiplicities (Just env)
             = eq_type_expand_respect_x env
"eqType3" generic_eq_type_x ExpandSynonyms IgnoreMultiplicities Nothing
             = eq_type_expand_ignore
"eqType4" forall env. generic_eq_type_x ExpandSynonyms IgnoreMultiplicities (Just env)
             = eq_type_expand_ignore_x env
"eqType5" generic_eq_type_x KeepSynonyms RespectMultiplicities Nothing
             = eq_type_keep_respect
"eqType6" forall env. generic_eq_type_x KeepSynonyms RespectMultiplicities (Just env)
             = eq_type_keep_respect_x env
 #-}

-- ---------------------------------------------------------------
-- | Real worker for 'eqType'. No kind check!
-- Inline it at the (handful of local) call sites
-- The "generic" bit refers to the flag paramerisation
-- See Note [Specialising type equality].
generic_eq_type_x, inline_generic_eq_type_x
  :: SynFlag -> MultiplicityFlag -> Maybe RnEnv2 -> Type -> Type -> Bool

{-# NOINLINE generic_eq_type_x #-}
generic_eq_type_x = inline_generic_eq_type_x
-- See Note [Computing equality on types] in Type

{-# INLINE inline_generic_eq_type_x #-}
-- This non-recursive function can inline at its (few) call sites.  The
-- recursion goes via generic_eq_type_x, which is the loop-breaker.
inline_generic_eq_type_x syn_flag mult_flag mb_env
  = \ t1 t2 -> t1 `seq` t2 `seq`
    let go = generic_eq_type_x syn_flag mult_flag mb_env
             -- Abbreviation for recursive calls

        gos []       []       = True
        gos (t1:ts1) (t2:ts2) = go t1 t2 && gos ts1 ts2
        gos _ _               = False

    in case (t1,t2) of
      _ | 1# <- reallyUnsafePtrEquality# t1 t2 -> True
      -- See Note [Type comparisons using object pointer comparisons]

      (TyConApp tc1 tys1, TyConApp tc2 tys2)
        | tc1 == tc2, not (isForgetfulSynTyCon tc1)   -- See Note [Comparing type synonyms]
        -> gos tys1 tys2

      _ | ExpandSynonyms <- syn_flag, Just t1' <- coreView t1 -> go t1' t2
        | ExpandSynonyms <- syn_flag, Just t2' <- coreView t2 -> go t1 t2'

      (TyConApp tc1 ts1, TyConApp tc2 ts2)
        | tc1 == tc2 -> gos ts1 ts2
        | otherwise  -> False

      (TyVarTy tv1, TyVarTy tv2)
        -> case mb_env of
              Nothing  -> tv1 == tv2
              Just env -> rnOccL env tv1 == rnOccR env tv2

      (LitTy lit1,    LitTy lit2)    -> lit1 == lit2
      (CastTy t1' _,   _)            -> go t1' t2 -- Ignore casts
      (_,             CastTy t2' _)  -> go t1 t2' -- Ignore casts
      (CoercionTy {}, CoercionTy {}) -> True      -- Ignore coercions

    -- Make sure we handle all FunTy cases since falling through to the
    -- AppTy case means that tcSplitAppTyNoView_maybe may see an unzonked
    -- kind variable, which causes things to blow up.
    -- See Note [Equality on FunTys] in GHC.Core.TyCo.Rep: we must check
    -- kinds here
      (FunTy _ w1 arg1 res1, FunTy _ w2 arg2 res2)
        ->   fullEq go arg1 arg2
          && fullEq go res1 res2
          && (case mult_flag of
                  RespectMultiplicities -> go w1 w2
                  IgnoreMultiplicities  -> True)

      -- See Note [Equality on AppTys] in GHC.Core.Type
      (AppTy s1 t1', _)
        | Just (s2, t2') <- tcSplitAppTyNoView_maybe t2
        -> go s1 s2 && go t1' t2'
      (_,  AppTy s2 t2')
        | Just (s1, t1') <- tcSplitAppTyNoView_maybe t1
        -> go s1 s2 && go t1' t2'

      (ForAllTy (Bndr tv1 vis1) body1, ForAllTy (Bndr tv2 vis2) body2)
        -> case mb_env of
              Nothing -> generic_eq_type_x syn_flag mult_flag
                            (Just (initRnEnv t1 t2)) t1 t2
              Just env
                | vis1 `eqForAllVis` vis2         -- See Note [ForAllTy and type equality]
                -> go (varType tv1) (varType tv2)  -- Always do kind-check
                   && generic_eq_type_x syn_flag mult_flag
                             (Just (rnBndr2 env tv1 tv2)) body1 body2
                | otherwise
                -> False

      _ -> False

fullEq :: (Type -> Type -> Bool) -> Type -> Type -> Bool
-- Do "full equality" including the kind check
-- See Note [Casts and coercions in type comparision]
{-# INLINE fullEq #-}
fullEq eq ty1 ty2
  = case eq ty1 ty2 of
          False    -> False
          True | hasCasts ty1 || hasCasts ty2
               -> eq (typeKind ty1) (typeKind ty2)
               | otherwise
               -> True

hasCasts :: Type -> Bool
-- Fast, does not look deep, does not allocate
hasCasts (CastTy {})     = True
hasCasts (CoercionTy {}) = True
hasCasts (AppTy t1 t2)   = hasCasts t1 || hasCasts t2
hasCasts (ForAllTy _ ty) = hasCasts ty
hasCasts _               = False   -- TyVarTy, TyConApp, FunTy, LitTy


{- *********************************************************************
*                                                                      *
                Comparing ForAllTyFlags
*                                                                      *
********************************************************************* -}

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
eqForAllVis :: ForAllTyFlag -> ForAllTyFlag -> Bool
-- See Note [ForAllTy and type equality]
eqForAllVis Required      Required      = True
eqForAllVis (Invisible _) (Invisible _) = True
eqForAllVis _             _             = False

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
cmpForAllVis :: ForAllTyFlag -> ForAllTyFlag -> Ordering
-- See Note [ForAllTy and type equality]
cmpForAllVis Required      Required       = EQ
cmpForAllVis Required      (Invisible {}) = LT
cmpForAllVis (Invisible _) Required       = GT
cmpForAllVis (Invisible _) (Invisible _)  = EQ


{- Note [ForAllTy and type equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we compare (ForAllTy (Bndr tv1 vis1) ty1)
         and    (ForAllTy (Bndr tv2 vis2) ty2)
what should we do about `vis1` vs `vis2`?

We had a long debate about this: see #22762 and GHC Proposal 558.
Here is the conclusion.

* In Haskell, we really do want (forall a. ty) and (forall a -> ty) to be
  distinct types, not interchangeable.  The latter requires a type argument,
  but the former does not.  See GHC Proposal 558.

* We /really/ do not want the typechecker and Core to have different notions of
  equality.  That is, we don't want `tcEqType` and `eqType` to differ.  Why not?
  Not so much because of code duplication but because it is virtually impossible
  to cleave the two apart. Here is one particularly awkward code path:
     The type checker calls `substTy`, which calls `mkAppTy`,
     which calls `mkCastTy`, which calls `isReflexiveCo`, which calls `eqType`.

* Moreover the resolution of the TYPE vs CONSTRAINT story was to make the
  typechecker and Core have a single notion of equality.

* So in GHC:
  - `tcEqType` and `eqType` implement the same equality
  - (forall a. ty) and (forall a -> ty) are distinct types in both Core and typechecker
  - That is, both `eqType` and `tcEqType` distinguish them.

* But /at representational role/ we can relate the types. That is,
    (forall a. ty) ~R (forall a -> ty)
  After all, since types are erased, they are represented the same way.
  See Note [ForAllCo] and the typing rule for ForAllCo given there

* What about (forall a. ty) and (forall {a}. ty)?  See Note [Comparing visibility].

Note [Comparing visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are sure that we want to distinguish (forall a. ty) and (forall a -> ty); see
Note [ForAllTy and type equality].  But we have /three/ settings for the ForAllTyFlag:
  * Specified: forall a. ty
  * Inferred:  forall {a}. ty
  * Required:  forall a -> ty

We could (and perhaps should) distinguish all three. But for now we distinguish
Required from Specified/Inferred, and ignore the distinction between Specified
and Inferred.

The answer doesn't matter too much, provided we are consistent. And we are consistent
because we always compare ForAllTyFlags with
  * `eqForAllVis`
  * `cmpForAllVis`.
(You can only really check this by inspecting all pattern matches on ForAllTyFlags.)
So if we change the decision, we just need to change those functions.

Why don't we distinguish all three? Should GHC type-check the following program
(adapted from #15740)?

  {-# LANGUAGE PolyKinds, ... #-}
  data D a
  type family F :: forall k. k -> Type
  type instance F = D

Due to the way F is declared, any instance of F must have a right-hand side
whose kind is equal to `forall k. k -> Type`. The kind of D is
`forall {k}. k -> Type`, which is very close, but technically uses distinct
Core:

  -----------------------------------------------------------
  | Source Haskell    | Core                                |
  -----------------------------------------------------------
  | forall  k.  <...> | ForAllTy (Bndr k Specified) (<...>) |
  | forall {k}. <...> | ForAllTy (Bndr k Inferred)  (<...>) |
  -----------------------------------------------------------

We could deem these kinds to be unequal, but that would imply rejecting
programs like the one above. Whether a kind variable binder ends up being
specified or inferred can be somewhat subtle, however, especially for kinds
that aren't explicitly written out in the source code (like in D above).

For now, we decide

    the specified/inferred status of an invisible type variable binder
    does not affect GHC's notion of equality.

That is, we have the following:

  --------------------------------------------------
  | Type 1            | Type 2            | Equal? |
  --------------------|-----------------------------
  | forall k. <...>   | forall k. <...>   | Yes    |
  |                   | forall {k}. <...> | Yes    |
  |                   | forall k -> <...> | No     |
  --------------------------------------------------
  | forall {k}. <...> | forall k. <...>   | Yes    |
  |                   | forall {k}. <...> | Yes    |
  |                   | forall k -> <...> | No     |
  --------------------------------------------------
  | forall k -> <...> | forall k. <...>   | No     |
  |                   | forall {k}. <...> | No     |
  |                   | forall k -> <...> | Yes    |
  --------------------------------------------------

Examples: T16946, T15079.

Historical Note [Typechecker equality vs definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes some history, in case there are vestiges of this
history lying around in the code.

Summary: prior to summer 2022, GHC had have two notions of equality
over Core types.  But now there is only one: definitional equality,
or just equality for short.

The old setup was:

* Definitional equality, as implemented by GHC.Core.Type.eqType.
  See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep.

* Typechecker equality, as implemented by tcEqType.
  GHC.Tc.Solver.Equality.canonicaliseEquality also respects typechecker equality.

Typechecker equality implied definitional equality: if two types are equal
according to typechecker equality, then they are also equal according to
definitional equality. The converse is not always true, as typechecker equality
is more finer-grained than definitional equality in two places:

* Constraint vs Type.  Definitional equality equated Type and
  Constraint, but typechecker treats them as distinct types.

* Unlike definitional equality, which does not care about the ForAllTyFlag of a
  ForAllTy, typechecker equality treats Required type variable binders as
  distinct from Invisible type variable binders.
  See Note [ForAllTy and type equality]


************************************************************************
*                                                                      *
                Comparison for types

      Not so heavily used, less carefully optimised
*                                                                      *
************************************************************************

-- Now here comes the real worker

Note [nonDetCmpType nondeterminism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nonDetCmpType is implemented in terms of nonDetCmpTypeX. nonDetCmpTypeX
uses nonDetCmpTc which compares TyCons by their Unique value. Using Uniques for
ordering leads to nondeterminism. We hit the same problem in the TyVarTy case,
comparing type variables is nondeterministic, note the call to nonDetCmpVar in
nonDetCmpTypeX.
See Note [Unique Determinism] for more details.
-}

nonDetCmpType :: Type -> Type -> Ordering
{-# INLINE nonDetCmpType #-}
nonDetCmpType !t1 !t2
  -- See Note [Type comparisons using object pointer comparisons]
  | 1# <- reallyUnsafePtrEquality# t1 t2
  = EQ
nonDetCmpType (TyConApp tc1 []) (TyConApp tc2 []) | tc1 == tc2
  = EQ
nonDetCmpType t1 t2
  -- we know k1 and k2 have the same kind, because they both have kind *.
  = nonDetCmpTypeX rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes [t1, t2]))

-- | An ordering relation between two 'Type's (known below as @t1 :: k1@
-- and @t2 :: k2@)
data TypeOrdering = TLT  -- ^ @t1 < t2@
                  | TEQ  -- ^ @t1 ~ t2@ and there are no casts in either,
                         -- therefore we can conclude @k1 ~ k2@
                  | TEQX -- ^ @t1 ~ t2@ yet one of the types contains a cast so
                         -- they may differ in kind.
                  | TGT  -- ^ @t1 > t2@
                  deriving (Eq, Ord, Enum, Bounded)

nonDetCmpTypeX :: RnEnv2 -> Type -> Type -> Ordering  -- Main workhorse
    -- See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep
    -- See Note [Computing equality on types]
    -- Always respects multiplicities, unlike eqType
nonDetCmpTypeX env orig_t1 orig_t2 =
    case go env orig_t1 orig_t2 of
      -- If there are casts then we also need to do a comparison of
      -- the kinds of the types being compared
      TEQX          -> toOrdering $ go env k1 k2
      ty_ordering   -> toOrdering ty_ordering
  where
    k1 = typeKind orig_t1
    k2 = typeKind orig_t2

    toOrdering :: TypeOrdering -> Ordering
    toOrdering TLT  = LT
    toOrdering TEQ  = EQ
    toOrdering TEQX = EQ
    toOrdering TGT  = GT

    liftOrdering :: Ordering -> TypeOrdering
    liftOrdering LT = TLT
    liftOrdering EQ = TEQ
    liftOrdering GT = TGT

    thenCmpTy :: TypeOrdering -> TypeOrdering -> TypeOrdering
    thenCmpTy TEQ  rel  = rel
    thenCmpTy TEQX rel  = hasCast rel
    thenCmpTy rel  _    = rel

    hasCast :: TypeOrdering -> TypeOrdering
    hasCast TEQ = TEQX
    hasCast rel = rel

    -- Returns both the resulting ordering relation between
    -- the two types and whether either contains a cast.
    go :: RnEnv2 -> Type -> Type -> TypeOrdering

    go _   (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = TEQ    -- See Note [Comparing type synonyms]

    go env t1 t2
      | Just t1' <- coreView t1 = go env t1' t2
      | Just t2' <- coreView t2 = go env t1 t2'

    go env (TyVarTy tv1)       (TyVarTy tv2)
      = liftOrdering $ rnOccL env tv1 `nonDetCmpVar` rnOccR env tv2
    go env (ForAllTy (Bndr tv1 vis1) t1) (ForAllTy (Bndr tv2 vis2) t2)
      = liftOrdering (vis1 `cmpForAllVis` vis2)   -- See Note [ForAllTy and type equality]
        `thenCmpTy` go env (varType tv1) (varType tv2)
        `thenCmpTy` go (rnBndr2 env tv1 tv2) t1 t2

        -- See Note [Equality on AppTys]
    go env (AppTy s1 t1) ty2
      | Just (s2, t2) <- splitAppTyNoView_maybe ty2
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env ty1 (AppTy s2 t2)
      | Just (s1, t1) <- splitAppTyNoView_maybe ty1
      = go env s1 s2 `thenCmpTy` go env t1 t2

    go env (FunTy _ w1 s1 t1) (FunTy _ w2 s2 t2)
        -- NB: nonDepCmpTypeX does the kind check requested by
        -- Note [Equality on FunTys] in GHC.Core.TyCo.Rep
      = liftOrdering (nonDetCmpTypeX env s1 s2 S.<> nonDetCmpTypeX env t1 t2)
          `thenCmpTy` go env w1 w2
        -- Comparing multiplicities last because the test is usually true

    go env (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      = liftOrdering (tc1 `nonDetCmpTc` tc2) `thenCmpTy` gos env tys1 tys2

    go _   (LitTy l1)          (LitTy l2)          = liftOrdering (nonDetCmpTyLit l1 l2)
    go env (CastTy t1 _)       t2                  = hasCast $ go env t1 t2
    go env t1                  (CastTy t2 _)       = hasCast $ go env t1 t2

    go _   (CoercionTy {})     (CoercionTy {})     = TEQ

        -- Deal with the rest: TyVarTy < CoercionTy < AppTy < LitTy < TyConApp < ForAllTy
    go _ ty1 ty2
      = liftOrdering $ (get_rank ty1) `compare` (get_rank ty2)
      where get_rank :: Type -> Int
            get_rank (CastTy {})
              = pprPanic "nonDetCmpTypeX.get_rank" (ppr [ty1,ty2])
            get_rank (TyVarTy {})    = 0
            get_rank (CoercionTy {}) = 1
            get_rank (AppTy {})      = 3
            get_rank (LitTy {})      = 4
            get_rank (TyConApp {})   = 5
            get_rank (FunTy {})      = 6
            get_rank (ForAllTy {})   = 7

    gos :: RnEnv2 -> [Type] -> [Type] -> TypeOrdering
    gos _   []         []         = TEQ
    gos _   []         _          = TLT
    gos _   _          []         = TGT
    gos env (ty1:tys1) (ty2:tys2) = go env ty1 ty2 `thenCmpTy` gos env tys1 tys2


-------------
-- | Compare two 'TyCon's.
-- See Note [nonDetCmpType nondeterminism]
nonDetCmpTc :: TyCon -> TyCon -> Ordering
nonDetCmpTc tc1 tc2
  = u1 `nonDetCmpUnique` u2
  where
    u1  = tyConUnique tc1
    u2  = tyConUnique tc2


{- *********************************************************************
*                                                                      *
                  mayLookIdentical
*                                                                      *
********************************************************************* -}

-- | Something in a type which might be invisible.
--
-- Used to avoid reporting confusing errors to the user, like:
--
-- > Couldn't match (a -> b) with (a -> b)
--
-- When in fact it is e.g. (a %1 -> b) vs (a %Many -> b), but the multiplicites
-- have been suppressed.
--
-- See Note [Showing invisible bits of types in error messages] in GHC.Tc.Errors.Ppr.
data InvisibleBit
  = InvisibleKind
  | InvisibleRuntimeRep
  | InvisibleMultiplicity
  deriving stock (Eq, Ord, Show)
instance Outputable InvisibleBit where
  ppr thing = text (show thing)

-- | A collection of 'InvisibleBit's.
type InvisibleBits = Set InvisibleBit

-- | Make the sure the given invisible bits are displayed.
--
-- See Note [Showing invisible bits of types in error messages] in GHC.Tc.Errors.Ppr.
pprWithInvisibleBits :: Set InvisibleBit -> SDoc -> SDoc
pprWithInvisibleBits invis_bits
  = updSDocContext $ \ctx ->
      ctx { sdocPrintExplicitKinds
              = show_kinds || sdocPrintExplicitKinds ctx
          , sdocPrintExplicitRuntimeReps
              = show_reps  || sdocPrintExplicitRuntimeReps ctx
          , sdocLinearTypes
              = show_mults || sdocLinearTypes ctx
  -- Question: what about 'sdocPrintExplicitCoercions'?
  -- Currently, we never enable that automatically; it's always up to the user
  -- to enable it.
          }
  where
    show_kinds = InvisibleKind         `Set.member` invis_bits
    show_reps  = InvisibleRuntimeRep   `Set.member` invis_bits
    show_mults = InvisibleMultiplicity `Set.member` invis_bits

mayLookIdentical :: Type -> Type -> InvisibleBits
-- | 'mayLookIdentical' returns:
--
--  - An empty set if the two types are distinct unequal, and remain distinct
--    even if we hide explicit kinds, runtime-reps, multiplicities.
--  - A non-empty set of 'invis_bits', if the two types might look equal, but
--    are in fact distinct in the returned 'invis_bits'.
--
-- See Note [mayLookIdentical], as well as
-- Note [Showing invisible bits of types in error messages] in GHC.Tc.Errors.Ppr.
mayLookIdentical orig_ty1 orig_ty2
  = case Writer.runWriter $ go orig_env True orig_ty1 orig_ty2 of
      (eq, invis_things) ->
        if eq
        then invis_things
        else Set.empty
  where
    orig_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [orig_ty1, orig_ty2]

    tell_on_mismatch :: InvisibleBit -> Writer InvisibleBits Bool -> Writer InvisibleBits Bool
    tell_on_mismatch what inner
      = do { let (inner_vis_ok, inner_invis) = Writer.runWriter inner
                 ok = inner_vis_ok && Set.null inner_invis
           ; unless ok $
               Writer.tell $
                 if inner_vis_ok
                 then -- If the inner mismatch is invisible,
                      -- we need to print those invisible bits as well.
                      --
                      -- See Note [mayLookIdentical]
                      Set.insert what inner_invis
                 else Set.singleton what
           ; return ok
           }

    -- See Note [mayLookIdentical]
    go :: RnEnv2
       -> Bool -- are we at the top-level? used only for Wrinkle [MetaTv mismatch at top-level only]
       -> Type -- lhs type
       -> Type -- rhs type
       -> Writer InvisibleBits Bool
        -- True  <=> types might be visibly equal
        -- False <=> types are definitely not visibly equal
        -- Invisible bits: the invisible bits in which the types differ (if any)
    go env _top (TyConApp tc1 ts1) (TyConApp tc2 ts2)
      | tc1 == tc2, not (isForgetfulSynTyCon tc1) -- See Note [Comparing type synonyms]
      = gos env (tyConBinders tc1) ts1 ts2

    go env top t1 t2 | Just t1' <- coreView t1 = go env top t1' t2
    go env top t1 t2 | Just t2' <- coreView t2 = go env top t1 t2'

    go env _top (TyVarTy tv1) (TyVarTy tv2)
      | rnOccL env tv1 == rnOccR env tv2
      = return True
    go env top (TyVarTy tv) ty
      | isDefaultableTyVar tv
      -- See Note [Defaultable tyvars in mayLookIdentical]
      = discard_defaultable_tyvar tv
      | isMetaTyVar tv
      -- See Note [Metavariables in mayLookIdentical]
      = if not top -- Wrinkle [MetaTv mismatch at top-level only]
        then return False
        else
          do { kind_ok <- tell_on_mismatch InvisibleKind $ go env False (tyVarKind tv) (typeKind ty)
             ; return $ not kind_ok }
    go env top ty (TyVarTy tv)
      | isDefaultableTyVar tv
      -- See Note [Defaultable tyvars in mayLookIdentical]
      = discard_defaultable_tyvar tv
      | isMetaTyVar tv
      -- See Note [Metavariables in mayLookIdentical]
      = if not top -- Wrinkle [MetaTv mismatch at top-level only]
        then return False
        else
          do { kind_ok <- tell_on_mismatch InvisibleKind $ go env False (typeKind ty) (tyVarKind tv)
             ; return $ not kind_ok }
    go _ _ (TyVarTy {}) (TyVarTy {}) = return False

    go _   _   (LitTy lit1)    (LitTy lit2)    = return $ lit1 == lit2
    go top vis (CastTy t1 _)   t2              = go top vis t1 t2
    go top vis t1              (CastTy t2 _)   = go top vis t1 t2
    go _   _   (CoercionTy {}) (CoercionTy {}) = return True

    go env _top (ForAllTy (Bndr tv1 vis1) ty1)
                (ForAllTy (Bndr tv2 vis2) ty2)
      = if vis1 `eqForAllVis` vis2  -- See Note [ForAllTy and type equality]
        then do { _kind_ok <- tell_on_mismatch InvisibleKind $ go env False (tyVarKind tv1) (tyVarKind tv2)
                ; go (rnBndr2 env tv1 tv2) False ty1 ty2 }
        else return False

    -- See Note [Defaultable tyvars in mayLookIdentical]
    go _ _ (ForAllTy b@(Bndr tv _) _) _ | isDefaultableBndr b = discard_defaultable_tyvar tv
    go _ _ _ (ForAllTy b@(Bndr tv _) _) | isDefaultableBndr b = discard_defaultable_tyvar tv

    go env _top (FunTy _ w1 arg1 res1) (FunTy _ w2 arg2 res2)
      = do { _mult_ok <- tell_on_mismatch InvisibleMultiplicity $ go env False w1 w2
           ; _reps_ok <- tell_on_mismatch InvisibleRuntimeRep $
               (&&) <$> go env False (typeKind arg1) (typeKind arg2)
                    <*> go env False (typeKind res1) (typeKind res2)
           ; (&&) <$> go env False arg1 arg2 <*> go env False res1 res2 }

      -- See Note [Equality on AppTys] in GHC.Core.Type
    go env _top (AppTy s1 t1) ty2
      | Just (s2, t2) <- tcSplitAppTyNoView_maybe ty2
      = (&&) <$> go env False s1 s2 <*> go env False t1 t2
    go env _top ty1 (AppTy s2 t2)
      | Just (s1, t1) <- tcSplitAppTyNoView_maybe ty1
      = (&&) <$> go env False s1 s2 <*> go env False t1 t2

    go _ _ _ _ = return False

    discard_defaultable_tyvar :: TyVar -> Writer InvisibleBits Bool
    discard_defaultable_tyvar tv =
      do { let what =
                 if isMultiplicityTy (tyVarKind tv)
                 then InvisibleMultiplicity
                 else InvisibleRuntimeRep
        ; Writer.tell $ Set.singleton what
        ; return True }

    gos :: RnEnv2 -> [TyConBinder] -> [Type] -> [Type] -> Writer InvisibleBits Bool
    gos _   _  []       []      = return True
    gos env bs (t1:ts1) (t2:ts2)
      | (invisible, bs') <- case bs of
                               []     -> (False,                    [])
                               (b:bs) -> (isInvisibleTyConBinder b, bs)
      = if invisible
        then do { _kind_ok <- tell_on_mismatch InvisibleKind $ go env False t1 t2
                ; gos env bs' ts1 ts2 }
        else do { (&&) <$> go env False t1 t2 <*> gos env bs' ts1 ts2 }

    gos _ _ _ _ = return False

{- Note [mayLookIdentical]
~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Showing invisible bits of types in error messages]
in GHC.Tc.Errors.Ppr, the job of 'mayLookIdentical' is to compute whether two
types that are not equal may in fact look identical when pretty-printed to the
user, due to the hiding of explicit kinds, runtime-reps or multiplicities.

'mayLookIdentical' can return:

  - an empty set:
      The types definitely don't look identical, even if we hide all
      RuntimeReps, kinds, multiplicities, etc.

  - a non-empty set of invis_bits:
      The types may look identical to the user, so please don't hide 'invis_bits'
      when pretty-printing them.

Note that 'mayLookIdentical' is conservative: it can sometimes return a non-empty
result when in fact the types might not look identical, even after hiding all the
invisible bits. That's OK: all that happens is that we show a bit of extra
clutter; better that than occasionally displaying very confusing error messages.

To compute which invisible bits should be shown, the main worker function 'go'
walks through the two input types in parallel, accumulating a set of 'InvisBits'
in the Writer monad.

  - If the two types are definitely visibly unequal, 'go' returns 'False'.
  - If the two types might be visibly equal, it returns 'True'.
    Sometimes 'go' can return 'True' even when the types aren't really visibly
    equal, e.g. because of Note [Defaultable tyvars in mayLookIdentical].
    That's OK; all that means is that we will sometimes unnecessarily switch
    on some explicit pretty-printing flags.
  - The 'invis_bits' that go computes in the monad are the invisible bits in
    which the two types are definitely unequal.
    This is implemented by using the 'tell_on_mismatch' on function when we
    look at an invisible bit of the types.

    For example, suppose we have (a %m -> b) vs (a %n -> b):

      * We recursively call 'go' on the multiplicities, which tells us
        that 'm' and 'n' are unequal.
      * We write in the monad that there is a mismatch in a multiplicity.

    Note that, when the inner types differ in yet another invisible component,
    e.g. an invisible kind argument inside a multiplicity:

      (a %(M @k1 t) -> b)   vs   (a %(M @k2 t) -> b)

    then we need to enable the pretty-printing of /both/ multiplicities and
    explicit kinds. This is handled by 'tell_on_mismatch'.

Note [Metavariables in mayLookIdentical]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type mismatch, like in the 'PprExplicitKinds' test:

  alpha[tau] :: TYPE (RR @k1)
  b[sk]      :: TYPE (RR @k2)

Of course, 'alpha' and 'b' don't really look identical: they are type variables
with different names. We don't need to display any invisible bits for that to
be apparent to the user.

However, the user might wonder: why didn't 'alpha' unify? In this case, the
reason is that the kinds don't match, so it makes sense to show explicit kinds
to communicate that to the user.

Conclusion: in 'mayLookIdentical', when one of the types is a metavariable and
the kinds don't match, return:

  1. Visibly identical == True (the 'lie')
  2. Parts in which they are invisibly different: the kinds.

Otherwise, if the kinds do match, then set 'visibly identical == False'.

This has the net effect of switching on -fprint-explicit-kinds in the error
message, as desired.

Wrinkle [MetaTv mismatch at top-level only]

  The logic described in the Note only applies for top-level mismatches
  such as

    alpha[tau] ~ b[sk]

  If the mismatch is nested more deeply inside the type, don't bother. This
  is to avoid needlessly cluttering the error message, e.g. in

    T alpha ~ T b

  we shouldn't turn on explicit kinds just because of the 'alpha' vs 'b'
  mismatch. Relevant test case: T8030.

Note [Defaultable tyvars in mayLookIdentical]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are dealing with the following type mismatch in 'mayLookIdentical':

  ty1: forall (r::RuntimeRep). TYPE r -> Type
  ty2:                         Type   -> Type

The type pretty-printer defaults 'RuntimeRep' foralls (see GHC.Iface.Type.hideNonStandardTypes),
which will make the two types identical.

Hence, whenever we see a 'forall' whose binder has kind RuntimeRep, bail out
and say:

  1. May look identical: True.
  2. Parts in which the types are invisibly different: the RuntimeReps.

Hence, we will pass -fprint-explicit-runtime-reps when displaying ty1, which
will avoid a confusing error message.

The same applies for multiplicity variables, e.g.

  ty1: forall (m :: Multiplicity). a %m -> b
  ty2:                             a    -> b

See Note [Showing invisible bits of types in error messages] in GHC.Tc.Errors.Ppr
-}

isDefaultableBndr :: ForAllTyBinder -> Bool
-- This function should line up with the defaulting done
--   by GHC.Iface.Type.defaultIfaceTyVarsOfKind
-- See Note [Showing invisible bits of types in error messages]
--   in GHC.Tc.Errors.Ppr
isDefaultableBndr (Bndr tv vis)
  = isInvisibleForAllTyFlag vis && isDefaultableTyVar tv
isDefaultableTyVar :: TyVar -> Bool
isDefaultableTyVar tv =
  isLevityTy ki || isRuntimeRepTy ki  || isMultiplicityTy ki
  where
    ki = tyVarKind tv