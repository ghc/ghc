-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

-- | Type equality and comparison
module GHC.Core.TyCo.Compare (

    -- * Type comparison
    eqType, eqTypeOpt, eqTypeX, eqTypes, nonDetCmpType, nonDetCmpTypes, nonDetCmpTypeX,
    nonDetCmpTypesX, nonDetCmpTc,
    eqVarBndrs,
    CmpTypeOpt (..), defaultCmpTypeOpt,

    pickyEqType, tcEqType, tcEqKind, tcEqTypeNoKindCheck,
    tcEqTyConApps,
    mayLookIdentical,

   -- * Visiblity comparision
   eqForAllVis, cmpForAllVis

   ) where

import GHC.Prelude

import GHC.Core.Type( typeKind, coreView, tcSplitAppTyNoView_maybe, splitAppTyNoView_maybe
                    , isLevityTy, isRuntimeRepTy, isMultiplicityTy )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCon

import GHC.Types.Var
import GHC.Types.Unique
import GHC.Types.Var.Env

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Base (reallyUnsafePtrEquality#)

import qualified Data.Semigroup as S
import GHC.Core.Multiplicity

{- GHC.Core.TyCo.Compare overview
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements type equality and comparison

It uses a few functions from GHC.Core.Type, notably `typeKind`,
so it currently sits "on top of" GHC.Core.Type.
-}

{- *********************************************************************
*                                                                      *
            Type equality
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

Note [Type comparisons using object pointer comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quite often we substitute the type from a definition site into
occurrences without a change. This means for code like:
    \x -> (x,x,x)
The type of every `x` will often be represented by a single object
in the heap. We can take advantage of this by shortcutting the equality
check if two types are represented by the same pointer under the hood.
In some cases this reduces compiler allocations by ~2%.
-}


tcEqKind :: HasDebugCallStack => Kind -> Kind -> Bool
tcEqKind = tcEqType

tcEqType :: HasDebugCallStack => Type -> Type -> Bool
-- ^ tcEqType implements typechecker equality
-- It behaves just like eqType, but is implemented
-- differently (for now)
tcEqType ty1 ty2
  =  tcEqTypeNoSyns ki1 ki2
  && tcEqTypeNoSyns ty1 ty2
  where
    ki1 = typeKind ty1
    ki2 = typeKind ty2

-- | Just like 'tcEqType', but will return True for types of different kinds
-- as long as their non-coercion structure is identical.
tcEqTypeNoKindCheck :: Type -> Type -> Bool
tcEqTypeNoKindCheck ty1 ty2
  = tcEqTypeNoSyns ty1 ty2

-- | Check whether two TyConApps are the same; if the number of arguments
-- are different, just checks the common prefix of arguments.
tcEqTyConApps :: TyCon -> [Type] -> TyCon -> [Type] -> Bool
tcEqTyConApps tc1 args1 tc2 args2
  = tc1 == tc2 &&
    and (zipWith tcEqTypeNoKindCheck args1 args2)
    -- No kind check necessary: if both arguments are well typed, then
    -- any difference in the kinds of later arguments would show up
    -- as differences in earlier (dependent) arguments

{-
Note [Specialising tc_eq_type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type equality predicates in Type are hit pretty hard during typechecking.
Consequently we take pains to ensure that these paths are compiled to
efficient, minimally-allocating code.

To this end we place an INLINE on tc_eq_type, ensuring that it is inlined into
its publicly-visible interfaces (e.g. tcEqType). In addition to eliminating
some dynamic branches, this allows the simplifier to eliminate the closure
allocations that would otherwise be necessary to capture the two boolean "mode"
flags. This reduces allocations by a good fraction of a percent when compiling
Cabal.

See #19226.
-}

mayLookIdentical :: Type -> Type -> Bool
-- | Returns True if the /visible/ part of the types
-- might look equal, even if they are really unequal (in the invisible bits)
--
-- This function is very similar to tc_eq_type but it is much more
-- heuristic.  Notably, it is always safe to return True, even with types
-- that might (in truth) be unequal  -- this affects error messages only
-- (Originally there were one function with an extra flag, but the result
--  was hard to understand.)
mayLookIdentical orig_ty1 orig_ty2
  = go orig_env orig_ty1 orig_ty2
  where
    orig_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [orig_ty1, orig_ty2]

    go :: RnEnv2 -> Type -> Type -> Bool
    -- See Note [Comparing nullary type synonyms]
    go _  (TyConApp tc1 []) (TyConApp tc2 []) | tc1 == tc2 = True

    go env t1 t2 | Just t1' <- coreView t1 = go env t1' t2
    go env t1 t2 | Just t2' <- coreView t2 = go env t1 t2'

    go env (TyVarTy tv1)   (TyVarTy tv2)   = rnOccL env tv1 == rnOccR env tv2
    go _   (LitTy lit1)    (LitTy lit2)    = lit1 == lit2
    go env (CastTy t1 _)   t2              = go env t1 t2
    go env t1              (CastTy t2 _)   = go env t1 t2
    go _   (CoercionTy {}) (CoercionTy {}) = True

    go env (ForAllTy (Bndr tv1 vis1) ty1)
           (ForAllTy (Bndr tv2 vis2) ty2)
      =  vis1 `eqForAllVis` vis2  -- See Note [ForAllTy and type equality]
      && go (rnBndr2 env tv1 tv2) ty1 ty2
         -- Visible stuff only: ignore kinds of binders

    -- If we have (forall (r::RunTimeRep). ty1  ~   blah) then respond
    -- with True.  Reason: the type pretty-printer defaults RuntimeRep
    -- foralls (see Ghc.Iface.Type.hideNonStandardTypes).  That can make,
    -- say (forall r. TYPE r -> Type) into (Type -> Type), so it looks the
    -- same as a very different type (#24553).  By responding True, we
    -- tell GHC (see calls of mayLookIdentical) to display without defaulting.
    -- See Note [Showing invisible bits of types in error messages]
    -- in GHC.Tc.Errors.Ppr
    go _ (ForAllTy b _) _ | isDefaultableBndr b = True
    go _ _ (ForAllTy b _) | isDefaultableBndr b = True

    go env (FunTy _ w1 arg1 res1) (FunTy _ w2 arg2 res2)
      = go env arg1 arg2 && go env res1 res2 && go env w1 w2
        -- Visible stuff only: ignore agg kinds

      -- See Note [Equality on AppTys] in GHC.Core.Type
    go env (AppTy s1 t1) ty2
      | Just (s2, t2) <- tcSplitAppTyNoView_maybe ty2
      = go env s1 s2 && go env t1 t2
    go env ty1 (AppTy s2 t2)
      | Just (s1, t1) <- tcSplitAppTyNoView_maybe ty1
      = go env s1 s2 && go env t1 t2

    go env (TyConApp tc1 ts1)   (TyConApp tc2 ts2)
      = tc1 == tc2 && gos env (tyConBinders tc1) ts1 ts2

    go _ _ _ = False

    gos :: RnEnv2 -> [TyConBinder] -> [Type] -> [Type] -> Bool
    gos _   _         []       []      = True
    gos env bs (t1:ts1) (t2:ts2)
      | (invisible, bs') <- case bs of
                               []     -> (False,                    [])
                               (b:bs) -> (isInvisibleTyConBinder b, bs)
      = (invisible || go env t1 t2) && gos env bs' ts1 ts2

    gos _ _ _ _ = False


-- | Type equality comparing both visible and invisible arguments and expanding
-- type synonyms.
tcEqTypeNoSyns :: Type -> Type -> Bool
tcEqTypeNoSyns ta tb = tc_eq_type False ta tb

-- | Like 'pickyEqTypeVis', but returns a Bool for convenience
pickyEqType :: Type -> Type -> Bool
-- Check when two types _look_ the same, _including_ synonyms.
-- So (pickyEqType String [Char]) returns False
-- This ignores kinds and coercions, because this is used only for printing.
pickyEqType ty1 ty2 = tc_eq_type True ty1 ty2

-- | Real worker for 'tcEqType'. No kind check!
tc_eq_type :: Bool          -- ^ True <=> do not expand type synonyms
           -> Type -> Type
           -> Bool
-- Flags False, False is the usual setting for tc_eq_type
-- See Note [Computing equality on types] in Type
{-# INLINE tc_eq_type #-} -- See Note [Specialising tc_eq_type].
tc_eq_type keep_syns orig_ty1 orig_ty2
  = go orig_env orig_ty1 orig_ty2
  where
    orig_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [orig_ty1, orig_ty2]

    go :: RnEnv2 -> Type -> Type -> Bool
    -- See Note [Comparing nullary type synonyms]
    go _ (TyConApp tc1 []) (TyConApp tc2 []) | tc1 == tc2 = True

    go env t1 t2 | not keep_syns, Just t1' <- coreView t1 = go env t1' t2
    go env t1 t2 | not keep_syns, Just t2' <- coreView t2 = go env t1 t2'

    go env (TyVarTy tv1)   (TyVarTy tv2)   = rnOccL env tv1 == rnOccR env tv2
    go _   (LitTy lit1)    (LitTy lit2)    = lit1 == lit2
    go env (CastTy t1 _)   t2              = go env t1 t2
    go env t1              (CastTy t2 _)   = go env t1 t2
    go _   (CoercionTy {}) (CoercionTy {}) = True

    go env (ForAllTy (Bndr tv1 vis1) ty1)
           (ForAllTy (Bndr tv2 vis2) ty2)
      =  vis1 `eqForAllVis` vis2  -- See Note [ForAllTy and type equality]
      && go env (varType tv1) (varType tv2)
      && go (rnBndr2 env tv1 tv2) ty1 ty2

    -- Make sure we handle all FunTy cases since falling through to the
    -- AppTy case means that tcSplitAppTyNoView_maybe may see an unzonked
    -- kind variable, which causes things to blow up.
    -- See Note [Equality on FunTys] in GHC.Core.TyCo.Rep: we must check
    -- kinds here
    go env (FunTy _ w1 arg1 res1) (FunTy _ w2 arg2 res2)
      = go env (typeKind arg1) (typeKind arg2) &&
        go env (typeKind res1) (typeKind res2) &&
        go env arg1 arg2 && go env res1 res2 && go env w1 w2

      -- See Note [Equality on AppTys] in GHC.Core.Type
    go env (AppTy s1 t1)        ty2
      | Just (s2, t2) <- tcSplitAppTyNoView_maybe ty2
      = go env s1 s2 && go env t1 t2
    go env ty1                  (AppTy s2 t2)
      | Just (s1, t1) <- tcSplitAppTyNoView_maybe ty1
      = go env s1 s2 && go env t1 t2

    go env (TyConApp tc1 ts1)   (TyConApp tc2 ts2)
      = tc1 == tc2 && gos env ts1 ts2

    go _ _ _ = False

    gos _   []       []       = True
    gos env (t1:ts1) (t2:ts2) = go env t1 t2 && gos env ts1 ts2
    gos _ _ _                 = False


isDefaultableBndr :: ForAllTyBinder -> Bool
-- This function should line up with the defaulting done
--   by GHC.Iface.Type.defaultIfaceTyVarsOfKind
-- See Note [Showing invisible bits of types in error messages]
--   in GHC.Tc.Errors.Ppr
isDefaultableBndr (Bndr tv vis)
  = isInvisibleForAllTyFlag vis && is_defaultable (tyVarKind tv)
  where
    is_defaultable ki = isLevityTy ki || isRuntimeRepTy ki  || isMultiplicityTy ki

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
        (We don't use instances so that we know where it happens)
*                                                                      *
************************************************************************

Note [Equality on AppTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
In our cast-ignoring equality, we want to say that the following two
are equal:

  (Maybe |> co) (Int |> co')   ~?       Maybe Int

But the left is an AppTy while the right is a TyConApp. The solution is
to use splitAppTyNoView_maybe to break up the TyConApp into its pieces and
then continue. Easy to do, but also easy to forget to do.

Note [Comparing nullary type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the task of testing equality between two 'Type's of the form

  TyConApp tc []

where @tc@ is a type synonym. A naive way to perform this comparison these
would first expand the synonym and then compare the resulting expansions.

However, this is obviously wasteful and the RHS of @tc@ may be large; it is
much better to rather compare the TyCons directly. Consequently, before
expanding type synonyms in type comparisons we first look for a nullary
TyConApp and simply compare the TyCons if we find one. Of course, if we find
that the TyCons are *not* equal then we still need to perform the expansion as
their RHSs may still be equal.

We perform this optimisation in a number of places:

 * GHC.Core.Types.eqType
 * GHC.Core.Types.nonDetCmpType
 * GHC.Core.Unify.unify_ty
 * GHC.Tc.Solver.Equality.can_eq_nc'
 * TcUnify.uType

This optimisation is especially helpful for the ubiquitous GHC.Types.Type,
since GHC prefers to use the type synonym over @TYPE 'LiftedRep@ applications
whenever possible. See Note [Using synonyms to compress types] in
GHC.Core.Type for details.

-}

eqType :: Type -> Type -> Bool
-- ^ Type equality on source types. Does not look through @newtypes@,
-- 'PredType's or type families, but it does look through type synonyms.
-- This first checks that the kinds of the types are equal and then
-- checks whether the types are equal, ignoring casts and coercions.
-- (The kind check is a recursive call, but since all kinds have type
-- @Type@, there is no need to check the types of kinds.)
-- See also Note [Non-trivial definitional equality] in "GHC.Core.TyCo.Rep".
eqType t1 t2 = isEqual $ nonDetCmpType t1 t2
  -- It's OK to use nonDetCmpType here and eqType is deterministic,
  -- nonDetCmpType does equality deterministically

eqTypeOpt :: CmpTypeOpt -> Type -> Type -> Bool
eqTypeOpt opt t1 t2 = isEqual $ nonDetCmpTypeOpt opt t1 t2

-- | Compare types with respect to a (presumably) non-empty 'RnEnv2'.
eqTypeX :: RnEnv2 -> Type -> Type -> Bool
eqTypeX env t1 t2 = isEqual $ nonDetCmpTypeX defaultCmpTypeOpt env t1 t2
  -- It's OK to use nonDetCmpType here and eqTypeX is deterministic,
  -- nonDetCmpTypeX does equality deterministically

-- | Type equality on lists of types, looking through type synonyms
-- but not newtypes.
eqTypes :: [Type] -> [Type] -> Bool
eqTypes tys1 tys2 = isEqual $ nonDetCmpTypes tys1 tys2
  -- It's OK to use nonDetCmpType here and eqTypes is deterministic,
  -- nonDetCmpTypes does equality deterministically

eqVarBndrs :: RnEnv2 -> [Var] -> [Var] -> Maybe RnEnv2
-- Check that the var lists are the same length
-- and have matching kinds; if so, extend the RnEnv2
-- Returns Nothing if they don't match
eqVarBndrs env [] []
 = Just env
eqVarBndrs env (tv1:tvs1) (tv2:tvs2)
 | eqTypeX env (varType tv1) (varType tv2)
 = eqVarBndrs (rnBndr2 env tv1 tv2) tvs1 tvs2
eqVarBndrs _ _ _= Nothing

-- Now here comes the real worker

{-
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
nonDetCmpType = nonDetCmpTypeOpt defaultCmpTypeOpt

nonDetCmpTypeOpt :: CmpTypeOpt -> Type -> Type -> Ordering
nonDetCmpTypeOpt _ !t1 !t2
  -- See Note [Type comparisons using object pointer comparisons]
  | 1# <- reallyUnsafePtrEquality# t1 t2
  = EQ
nonDetCmpTypeOpt _ (TyConApp tc1 []) (TyConApp tc2 []) | tc1 == tc2
  = EQ
nonDetCmpTypeOpt opt t1 t2
  -- we know k1 and k2 have the same kind, because they both have kind *.
  = nonDetCmpTypeX opt rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes [t1, t2]))
{-# INLINE nonDetCmpType #-}

nonDetCmpTypes :: [Type] -> [Type] -> Ordering
nonDetCmpTypes ts1 ts2 = nonDetCmpTypesX rn_env ts1 ts2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes (ts1 ++ ts2)))

-- | An ordering relation between two 'Type's (known below as @t1 :: k1@
-- and @t2 :: k2@)
data TypeOrdering = TLT  -- ^ @t1 < t2@
                  | TEQ  -- ^ @t1 ~ t2@ and there are no casts in either,
                         -- therefore we can conclude @k1 ~ k2@
                  | TEQX -- ^ @t1 ~ t2@ yet one of the types contains a cast so
                         -- they may differ in kind.
                  | TGT  -- ^ @t1 > t2@
                  deriving (Eq, Ord, Enum, Bounded)

nonDetCmpTypeX :: CmpTypeOpt -> RnEnv2 -> Type -> Type -> Ordering  -- Main workhorse
    -- See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep
    -- See Note [Computing equality on types]
nonDetCmpTypeX opt env orig_t1 orig_t2 =
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
    -- See Note [Comparing nullary type synonyms]
    go _   (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = TEQ
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
      = liftOrdering (nonDetCmpTypeX opt env s1 s2 S.<> nonDetCmpTypeX opt env t1 t2)
          `thenCmpTy` cmp_mults
        -- Comparing multiplicities last because the test is usually true
        where
          cmp_mults = case cmp_multiplicity_in_funty opt of
            RespectMultiplicities -> go env w1 w2
            IgnoreMultiplicities -> TEQ

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

{- Note [Respecting multiplicity when comparing types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, we respect multiplicities (i.e. the linear part of the type
system) when comparing types.  Doing so is of course crucial during typechecking.

But for reasons described in Note [Linting linearity] in GHC.Core.Lint, it is hard
to ensure that Core is always type-correct when it comes to linearity.  So
* `CmpTypeOpt` provides a way to compare types that /ignores/ multiplicities
* We use this multiplicity-blind comparison very occasionally, notably
    - in Core Lint: see Note [Linting linearity] in GHC.Core.Lint
    - in rule matching: see Note [Rewrite rules ignore multiplicities in FunTy]
      in GHC.Core.Unify
-}
data CmpTypeOpt = CmpTypeOpt
  { -- Whether to consider `a -> b` and `a %1 -> b` distinct or equal. Default:
    -- RespectMultiplicities. See Note [Respecting multiplicity when comparing types].
    cmp_multiplicity_in_funty :: MultiplicityFlag
  }

defaultCmpTypeOpt :: CmpTypeOpt
defaultCmpTypeOpt = CmpTypeOpt
  { cmp_multiplicity_in_funty = RespectMultiplicities }

-------------
nonDetCmpTypesX :: RnEnv2 -> [Type] -> [Type] -> Ordering
nonDetCmpTypesX _   []        []        = EQ
nonDetCmpTypesX env (t1:tys1) (t2:tys2) = nonDetCmpTypeX defaultCmpTypeOpt env t1 t2 S.<>
                                          nonDetCmpTypesX env tys1 tys2
nonDetCmpTypesX _   []        _         = LT
nonDetCmpTypesX _   _         []        = GT

-------------
-- | Compare two 'TyCon's.
-- See Note [nonDetCmpType nondeterminism]
nonDetCmpTc :: TyCon -> TyCon -> Ordering
nonDetCmpTc tc1 tc2
  = u1 `nonDetCmpUnique` u2
  where
    u1  = tyConUnique tc1
    u2  = tyConUnique tc2



