{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{-|
This module defines the semi-ring of multiplicities, and associated functions.
Multiplicities annotate arrow types to indicate the linearity of the
arrow (in the sense of linear types).

Mult is a type synonym for Type, used only when its kind is Multiplicity.
To simplify dealing with multiplicities, functions such as
mkMultMul perform simplifications such as Omega * x = Omega on the fly.
-}
module Multiplicity
  ( Mult
  , pattern One
  , pattern Omega
  , pattern MultMul
  , mkMultAdd
  , mkMultMul
  , mkMultSup
  , Scaled(..)
  , scaledMult
  , scaledThing
  , unrestricted
  , linear
  , tymult
  , irrelevantMult
  , mkScaled
  , scaledSet
  , scaleScaled
  , IsSubmult(..)
  , submult
  , mapScaledType) where

import GhcPrelude

import Data.Data
import Outputable
import {-# SOURCE #-} TyCoRep (Type)
import {-# SOURCE #-} TysWiredIn ( oneDataConTy, omegaDataConTy, multMulTyCon )
import {-# SOURCE #-} Type( eqType, splitTyConApp_maybe, mkTyConApp )
import PrelNames (multMulTyConKey)
import Unique (hasKey)

{-
Note [Adding new multiplicities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To add a new multiplicity, you need to:
* Add the new type with Multiplicity kind
* Update cases in mkMultAdd, mkMultMul, mkMultSup, submult, tcSubMult
* Check supUE function that computes sup of a multiplicity
  and Zero
-}

--
-- * Core properties of multiplicities
--

type Mult = Type

pattern One :: Mult
pattern One <- (eqType oneDataConTy -> True)
  where One = oneDataConTy

pattern Omega :: Mult
pattern Omega <- (eqType omegaDataConTy -> True)
  where Omega = omegaDataConTy

isMultMul :: Mult -> Maybe (Mult, Mult)
isMultMul ty | Just (tc, [x, y]) <- splitTyConApp_maybe ty
             , tc `hasKey` multMulTyConKey = Just (x, y)
             | otherwise = Nothing

pattern MultMul :: Mult -> Mult -> Mult
pattern MultMul p q <- (isMultMul -> Just (p,q))

{-
The functions mkMultAdd, mkMultMul, mkMultSup perform operations
on multiplicities. They can return overapproximations: their result
is merely guaranteed to be a submultiplicity of the actual value.

They should be used only when an upper bound is acceptable.
In most cases, they are used in usage environments (UsageEnv);
in usage environments, replacing a usage with a larger one can only
cause more programs to fail to typecheck.

In future work, instead of approximating we might add type families
and allow users to write types involving operations on multiplicities.
In this case, we could enforce more invariants in Mult, for example,
enforce that that it is in the form of a sum of  products, and even
that the sumands and factors are ordered somehow, to have more equalities.
-}

-- With only two multiplicities One and Omega, we can always replace
-- p + q by Omega.
mkMultAdd :: Mult -> Mult -> Mult
mkMultAdd _ _ = Omega

mkMultMul :: Mult -> Mult -> Mult
mkMultMul One p = p
mkMultMul p One = p
mkMultMul Omega _ = Omega
mkMultMul _ Omega = Omega
mkMultMul p q = mkTyConApp multMulTyCon [p, q]

-- | @mkMultSup w1 w2@ returns the smallest multiplicity larger than
-- or equal to both @w1@ and @w2@.
mkMultSup :: Mult -> Mult -> Mult
mkMultSup = mkMultMul
-- Note: If you are changing this logic, check 'supUE' in UsageEnv as well.

--
-- * Multiplicity ordering
--

data IsSubmult = Submult     -- Definitely a submult
               | Unknown     -- Could be a submult, need to ask the typechecker
               deriving (Show, Eq)

instance Outputable IsSubmult where
  ppr = text . show

-- | @submult w1 w2@ check whether a value of multiplicity @w1@ is allowed where a
-- value of multiplicity @w2@ is expected. This is a partial order.

submult :: Mult -> Mult -> IsSubmult
submult _     Omega = Submult
submult One   One   = Submult
-- The 1 <= p rule
submult One   _     = Submult
submult _     _     = Unknown

--
-- * Utilities
--

-- | A shorthand for data with an attached 'Mult' element (the multiplicity).
data Scaled a = Scaled Mult a
  deriving (Data)

scaledMult :: Scaled a -> Mult
scaledMult (Scaled m _) = m

scaledThing :: Scaled a -> a
scaledThing (Scaled _ t) = t

unrestricted, linear, tymult :: a -> Scaled a
unrestricted = Scaled Omega
linear = Scaled One

-- Used for type arguments in core
tymult = Scaled Omega

irrelevantMult :: Scaled a -> a
irrelevantMult = scaledThing

mkScaled :: Mult -> a -> Scaled a
mkScaled = Scaled

instance (Outputable a) => Outputable (Scaled a) where
   ppr (Scaled _cnt t) = ppr t
     -- Do not print the multiplicity here because it tends to be too verbose

scaledSet :: Scaled a -> b -> Scaled b
scaledSet (Scaled m _) b = Scaled m b

scaleScaled :: Mult -> Scaled a -> Scaled a
scaleScaled m' (Scaled m t) = Scaled (m' `mkMultMul` m) t

mapScaledType :: (Type -> Type) -> Scaled Type -> Scaled Type
mapScaledType f (Scaled m t) = Scaled (f m) (f t)
