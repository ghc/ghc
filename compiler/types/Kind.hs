-- (c) The University of Glasgow 2006-2012

{-# LANGUAGE CPP #-}
module Kind (
        -- * Main data type
        Kind,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind,
        isConstraintKindCon,

        classifiesTypeWithValues,
        isKindLevPoly
       ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} Type    ( coreView )

import TyCoRep
import TyCon
import PrelNames

import Outputable
import Util
import Data.Maybe( isJust )

{-
************************************************************************
*                                                                      *
        Functions over Kinds
*                                                                      *
************************************************************************

Note [Kind Constraint and kind Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The kind Constraint is the kind of classes and other type constraints.
The special thing about types of kind Constraint is that
 * They are displayed with double arrow:
     f :: Ord a => a -> a
 * They are implicitly instantiated at call sites; so the type inference
   engine inserts an extra argument of type (Ord a) at every call site
   to f.

However, once type inference is over, there is *no* distinction between
Constraint and Type. Indeed we can have coercions between the two. Consider
   class C a where
     op :: a -> a
For this single-method class we may generate a newtype, which in turn
generates an axiom witnessing
    C a ~ (a -> a)
so on the left we have Constraint, and on the right we have Type.
See Trac #7451.

Bottom line: although 'Type' and 'Constraint' are distinct TyCons, with
distinct uniques, they are treated as equal at all times except
during type inference.
-}

isConstraintKindCon :: TyCon -> Bool
isConstraintKindCon tc = tyConUnique tc == constraintKindTyConKey

-- | Tests whether the given kind (which should look like @TYPE x@)
-- is something other than a constructor tree (that is, constructors at every node).
-- E.g.  True of   TYPE k, TYPE (F Int)
--       False of  TYPE 'LiftedRep
isKindLevPoly :: Kind -> Bool
isKindLevPoly k = ASSERT2( isLiftedTypeKind k || _is_type, ppr k )
                    -- the isLiftedTypeKind check is necessary b/c of Constraint
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

    _is_type = classifiesTypeWithValues k

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
classifiesTypeWithValues k = isJust (kindRep_maybe k)
