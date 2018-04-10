-- (c) The University of Glasgow 2006-2012

{-# LANGUAGE CPP #-}
module Kind (
        -- * Main data type
        Kind,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind,
        isConstraintKind,
        isTYPEApp,
        returnsTyCon, returnsConstraintKind,
        isConstraintKindCon,

        classifiesTypeWithValues,
        isStarKind, isStarKindSynonymTyCon,
        tcIsStarKind,
        isKindLevPoly
       ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} Type    ( coreView
                              , splitTyConApp_maybe )
import {-# SOURCE #-} DataCon ( DataCon )

import TyCoRep
import TyCon
import PrelNames

import Outputable
import Util

{-
************************************************************************
*                                                                      *
        Functions over Kinds
*                                                                      *
************************************************************************

Note [Kind Constraint and kind *]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The kind Constraint is the kind of classes and other type constraints.
The special thing about types of kind Constraint is that
 * They are displayed with double arrow:
     f :: Ord a => a -> a
 * They are implicitly instantiated at call sites; so the type inference
   engine inserts an extra argument of type (Ord a) at every call site
   to f.

However, once type inference is over, there is *no* distinction between
Constraint and *.  Indeed we can have coercions between the two. Consider
   class C a where
     op :: a -> a
For this single-method class we may generate a newtype, which in turn
generates an axiom witnessing
    C a ~ (a -> a)
so on the left we have Constraint, and on the right we have *.
See Trac #7451.

Bottom line: although '*' and 'Constraint' are distinct TyCons, with
distinct uniques, they are treated as equal at all times except
during type inference.
-}

isConstraintKind :: Kind -> Bool
isConstraintKindCon :: TyCon -> Bool

isConstraintKindCon   tc = tyConUnique tc == constraintKindTyConKey

isConstraintKind (TyConApp tc _) = isConstraintKindCon tc
isConstraintKind _               = False

isTYPEApp :: Kind -> Maybe DataCon
isTYPEApp (TyConApp tc args)
  | tc `hasKey` tYPETyConKey
  , [arg] <- args
  , Just (tc, []) <- splitTyConApp_maybe arg
  , Just dc <- isPromotedDataCon_maybe tc
  = Just dc
isTYPEApp _ = Nothing

-- | Does the given type "end" in the given tycon? For example @k -> [a] -> *@
-- ends in @*@ and @Maybe a -> [a]@ ends in @[]@.
returnsTyCon :: Unique -> Type -> Bool
returnsTyCon tc_u (ForAllTy _ ty)  = returnsTyCon tc_u ty
returnsTyCon tc_u (FunTy    _ ty)  = returnsTyCon tc_u ty
returnsTyCon tc_u (TyConApp tc' _) = tc' `hasKey` tc_u
returnsTyCon _  _                  = False

returnsConstraintKind :: Kind -> Bool
returnsConstraintKind = returnsTyCon constraintKindTyConKey

-- | Tests whether the given kind (which should look like @TYPE x@)
-- is something other than a constructor tree (that is, constructors at every node).
isKindLevPoly :: Kind -> Bool
isKindLevPoly k = ASSERT2( isStarKind k || _is_type, ppr k )
                      -- the isStarKind check is necessary b/c of Constraint
                  go k
  where
    go ty | Just ty' <- coreView ty = go ty'
    go TyVarTy{}         = True
    go AppTy{}           = True  -- it can't be a TyConApp
    go (TyConApp tc tys) = isFamilyTyCon tc || any go tys
    go ForAllTy{}        = True
    go (FunTy t1 t2)     = go t1 || go t2
    go LitTy{}           = False
    go CastTy{}          = True
    go CoercionTy{}      = True

    _is_type
      | TyConApp typ [_] <- k
      = typ `hasKey` tYPETyConKey
      | otherwise
      = False


-----------------------------------------
--              Subkinding
-- The tc variants are used during type-checking, where ConstraintKind
-- is distinct from all other kinds
-- After type-checking (in core), Constraint and liftedTypeKind are
-- indistinguishable

-- | Does this classify a type allowed to have values? Responds True to things
-- like *, #, TYPE Lifted, TYPE v, Constraint.
classifiesTypeWithValues :: Kind -> Bool
-- ^ True of any sub-kind of OpenTypeKind
classifiesTypeWithValues = isTYPE (const True)

-- | Is this kind equivalent to @*@?
--
-- This considers 'Constraint' to be distinct from @*@. For a version that
-- treats them as the same type, see 'isStarKind'.
tcIsStarKind :: Kind -> Bool
tcIsStarKind = tcIsTYPE is_lifted
  where
    is_lifted (TyConApp lifted_rep []) = lifted_rep `hasKey` liftedRepDataConKey
    is_lifted _                        = False

-- | Is this kind equivalent to @*@?
--
-- This considers 'Constraint' to be the same as @*@. For a version that
-- treats them as different types, see 'tcIsStarKind'.
isStarKind :: Kind -> Bool
isStarKind = isLiftedTypeKind
                              -- See Note [Kind Constraint and kind *]

-- | Is the tycon @Constraint@?
isStarKindSynonymTyCon :: TyCon -> Bool
isStarKindSynonymTyCon tc = tc `hasKey` constraintKindTyConKey
