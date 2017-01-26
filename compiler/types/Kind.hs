-- (c) The University of Glasgow 2006-2012

{-# LANGUAGE CPP #-}
module Kind (
        -- * Main data type
        Kind, typeKind,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind,
        isConstraintKind,
        returnsConstraintKind,
        okArrowArgKind, okArrowResultKind,

        classifiesTypeWithValues,
        isKindLevPoly
       ) where

#include "HsVersions.h"

import {-# SOURCE #-} Type       ( typeKind, coreView )

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

-}

-- | Does the given type "end" in the given tycon? For example @k -> [a] -> *@
-- ends in @*@ and @Maybe a -> [a]@ ends in @[]@.
returnsConstraintKind :: Type -> Bool
returnsConstraintKind = go
  where
    go (ForAllTy _ ty)  = go ty
    go (FunTy    _ ty)  = go ty
    go other            = isConstraintKind other

-- | Tests whether the given kind (which should look like @TYPE x@)
-- is something other than a constructor tree (that is, constructors at every node).
isKindLevPoly :: Kind -> Bool
isKindLevPoly k = ASSERT2( _is_type k, ppr k )
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

    _is_type ty
      | Just ty' <- coreView ty
      = _is_type ty'

      | TyConApp typ [_] <- ty
      = typ `hasKey` tYPETyConKey

      | otherwise
      = False


--------------------------------------------
--            Kinding for arrow (->)
-- Says when a kind is acceptable on lhs or rhs of an arrow
--     arg -> res
--
-- See Note [Levity polymorphism]

okArrowArgKind, okArrowResultKind :: Kind -> Bool
okArrowArgKind    = classifiesTypeWithValues
okArrowResultKind = classifiesTypeWithValues

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
classifiesTypeWithValues t | Just t' <- coreView t = classifiesTypeWithValues t'
classifiesTypeWithValues (TyConApp tc [_]) = tc `hasKey` tYPETyConKey
classifiesTypeWithValues _ = False

{- Note [Levity polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is this type legal?
   (a :: TYPE rep) -> Int
   where 'rep :: RuntimeRep'

You might think not, because no lambda can have a
runtime-rep-polymorphic binder.  So no lambda has the
above type.  BUT here's a way it can be useful (taken from
Trac #12708):

  data T rep (a :: TYPE rep)
     = MkT (a -> Int)

  x1 :: T LiftedRep Int
  x1 =  MkT LiftedRep Int  (\x::Int -> 3)

  x2 :: T IntRep Int#
  x2 = MkT IntRep Int# (\x:Int# -> 3)

Note that the lambdas are just fine!

Hence, okArrowArgKind and okArrowResultKind both just
check that the type is of the form (TYPE r) for some
representation type r.
-}
