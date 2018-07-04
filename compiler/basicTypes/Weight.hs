-- TODO: arnaud: copyright notice

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS -Wno-missing-methods #-}

-- | This module defines the semi-ring (aka Rig) of weights, and associated
-- functions. Weights annotate arrow types to indicate the linearity of the
-- arrow (in the sense of linear types).
module Weight where
  -- TODO: arnaud list of exports
  --

import GhcPrelude

import Binary
import Control.Monad
import Data.Data
import Outputable
import Name
import NameEnv
import {-# SOURCE #-} Var (Id)
import {-# SOURCE #-} TyCoRep

--
-- * Core properties of weights
--


data Rig = Zero
         | One
         | Omega
         | RigAdd Rig Rig
         | RigMul Rig Rig
         | RigTy Type
  deriving (Data)


instance Num Rig where
  Zero * _ = Zero
  _ * Zero = Zero
  p * One = p
  One * p = p
  p * Omega = Omega
  Omega * p = Omega
  p1 * p2 = RigMul p1 p2

  Zero + x = x
  x + Zero = x
  One + One = Omega
  One + Omega = Omega
  Omega + One = Omega
  m1 + m2 = RigAdd m1 m2

instance Outputable Rig where
  ppr Zero = text "0"
  ppr One = text "1"
  ppr Omega = text "ω"
  ppr (RigAdd m1 m2) = parens (ppr m1 <+> text "+" <+> ppr m2)
  ppr (RigMul m1 m2) = parens (ppr m1 <+> text "*" <+> ppr m2)
  ppr (RigTy ty) = pprType ty

-- | @sup w1 w2@ returns the smallest weight larger than or equal to both @w1@
-- and @w2@.
sup :: Rig -> Rig -> Rig
sup Zero  Zero  = Zero
sup One   One   = One
sup Omega Omega = Omega
sup _     _     = Omega


--
-- * Utilities
--

-- | A shorthand for data with an attached 'Rig' element (the weight).
data Weighted a = Weighted {weightedWeight :: Rig, weightedThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

unrestricted, linear, staticOnly, tyweight :: a -> Weighted a
unrestricted = Weighted Omega
linear = Weighted One
staticOnly = Weighted Zero

-- Used for type arguments in core
tyweight = Weighted Omega

knownOmega :: Weighted a -> a
knownOmega = weightedThing

irrelevantWeight :: Weighted a -> a
irrelevantWeight = weightedThing

mkWeighted :: Rig -> a -> Weighted a
mkWeighted = Weighted

instance Outputable a => Outputable (Weighted a) where
   ppr (Weighted _cnt t) = -- ppr cnt <> ppr t
                          ppr t

-- MattP: For now we don't print the weight by default as it creeps into

weightedSet :: Weighted a -> b -> Weighted b
weightedSet x b = fmap (\_->b) x

setWeight :: Rig -> Weighted a -> Weighted a
setWeight r x = x { weightedWeight = r }

scaleWeighted :: Rig -> Weighted a -> Weighted a
scaleWeighted w x =
  x { weightedWeight = w * weightedWeight x }


