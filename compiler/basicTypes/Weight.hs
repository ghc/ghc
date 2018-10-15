-- TODO: arnaud: copyright notice

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS -Wno-missing-methods #-}

-- | This module defines the semi-ring (aka Rig) of weights, and associated
-- functions. Weights annotate arrow types to indicate the linearity of the
-- arrow (in the sense of linear types).
module Weight
  ( GMult
  , pattern Zero
  , pattern One
  , pattern Omega
  , pattern RigAdd
  , pattern RigMul
  , pattern RigThing
  , Multable(..)
  , unsafeRigThing
  , sup
  , GWeighted(..)
  , unrestricted
  , linear
  , staticOnly
  , tyweight
  , knownOmega
  , irrelevantWeight
  , mkWeighted
  , weightedSet
  , setWeight
  , scaleWeighted ) where

import GhcPrelude

import Binary
import Control.Monad
import Data.Data
import Outputable

--
-- * Core properties of weights
--

data GMult a
  = Zero_
  | One_
  | Omega_
  | RigAdd_ (GMult a) (GMult a)
  | RigMul_ (GMult a) (GMult a)
  | RigThing_ a
  deriving (Data)

-- | The 'Multable' class describes the requirements that a type needs to be a
-- good citizen as an argument of 'Gmult'
class Outputable a => Multable a where
  -- | A way to relect multiplicities into @a@
  fromMult :: GMult a -> a

  -- | A way to reify multiplicities from a value of @a@. @fromMult . toMult@
  -- should be the identity for a suitable equality. It is also expected that
  -- @toMult t@ reifies @t@ as much as possible. A more formal requirement is
  -- that @m@ is a subtree of @toMult . fromMult m@.
  toMult :: a -> GMult a

  -- | A way to check the order of two values of @a@ seen as atomic multiplicity
  -- (that is, all the multiplicity structure has been reified)

  -- XXX:TODO

-- Note that pattern synonyms for One, Omega, and Zero are not necessary: we could just
-- export them as constructors. They are defined as pattern synonym for
-- symmetry. Following the principle of least surprise.

-- We may enforce more invariants in the type of GMult. For instance, we can
-- enforce that it is in the form of a sum of products, and even that the
-- sumands and factors are ordered somehow, to have more equalities.

pattern Zero :: GMult a
pattern Zero = Zero_

pattern One :: GMult a
pattern One = One_

pattern Omega :: GMult a
pattern Omega = Omega_

pattern RigMul :: GMult a -> GMult a -> GMult a
pattern RigMul p q <- RigMul_ p q where
  Zero `RigMul` _ = Zero
  _ `RigMul` Zero = Zero
  One `RigMul` p = p
  p `RigMul` One = p
  Omega `RigMul` p = Omega
  p `RigMul` Omega = Omega
  p `RigMul` q = RigMul_ p q

pattern RigAdd :: GMult a -> GMult a -> GMult a
pattern RigAdd p q <- RigAdd_ p q where
  Zero `RigAdd` p = p
  p `RigAdd` Zero = p
  One `RigAdd` One = Omega
  Omega `RigAdd` _ = Omega
  _ `RigAdd` Omega = Omega
  p `RigAdd` q = RigAdd_ p q

pattern RigThing :: Multable a => a -> GMult a
pattern RigThing a <- RigThing_ a where
  RigThing a = toMult a

-- | Used to defined 'Multable' instances. Requires that the argument cannot be
-- reified any further. There is probably no good reason to use it outside of a
-- 'Multable' instance definition.
unsafeRigThing :: a -> GMult a
unsafeRigThing = RigThing_

instance Num (GMult a) where
  (*) = RigMul
  (+) = RigAdd

instance Multable a => Outputable (GMult a) where
  ppr Zero = text "0"
  ppr One = text "1"
  ppr Omega = text "ω"
  ppr (RigAdd m1 m2) = parens (ppr m1 <+> text "+" <+> ppr m2)
  ppr (RigMul m1 m2) = parens (ppr m1 <+> text "*" <+> ppr m2)
  ppr (RigThing t) = ppr t

-- | @sup w1 w2@ returns the smallest weight larger than or equal to both @w1@
-- and @w2@.
sup :: GMult a -> GMult a -> GMult a
sup Zero  Zero  = Zero
sup One   One   = One
sup Omega Omega = Omega
sup _     _     = Omega
-- TODO: Arnaud: there cannot not be a bug here


--
-- * Utilities
--

-- | A shorthand for data with an attached 'Rig' element (the weight).
data GWeighted t a = Weighted {weightedWeight :: GMult t, weightedThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

unrestricted, linear, staticOnly, tyweight :: a -> GWeighted t a
unrestricted = Weighted Omega
linear = Weighted One
staticOnly = Weighted Zero

-- Used for type arguments in core
tyweight = Weighted Omega

knownOmega :: GWeighted t a -> a
knownOmega = weightedThing

irrelevantWeight :: GWeighted t a -> a
irrelevantWeight = weightedThing

mkWeighted :: GMult t -> a -> GWeighted t a
mkWeighted = Weighted

instance (Multable t, Outputable a) => Outputable (GWeighted t a) where
   ppr (Weighted _cnt t) = -- ppr cnt <> ppr t
                          ppr t

-- MattP: For now we don't print the weight by default as it creeps into

weightedSet :: GWeighted t a -> b -> GWeighted t b
weightedSet x b = fmap (\_->b) x

setWeight :: GMult t -> GWeighted t a -> GWeighted t a
setWeight r x = x { weightedWeight = r }

scaleWeighted :: GMult t -> GWeighted t a -> GWeighted t a
scaleWeighted w x =
  x { weightedWeight = w * weightedWeight x }
