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

-- All of the definitions in this module live in Type
import Type

