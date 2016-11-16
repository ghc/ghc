-- TODO: arnaud: copyright notice

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | This module defines the semi-ring (aka Rig) of weights, and associated
-- functions. Weights annotate arrow types to indicate the linearity of the
-- arrow (in the sense of linear types).
module Weight where
  -- TODO: arnaud list of exports

import Data.Data
import Data.String
import Outputable

-- TODO: arnaud: clean up
data Rig =  -- Zero |
  One | Omega
  deriving (Eq,Ord,Data)

instance Num Rig where
  -- Zero * _ = Zero
  -- _ * Zero = Zero
  Omega * One = Omega
  One * Omega = Omega
  One * One   = One
  Omega * Omega = Omega

  -- Zero + x = x
  -- x + Zero = x
  _ + _ = Omega

instance Outputable Rig where
  ppr One = fromString "1"
  ppr Omega = fromString "ω"

data Weighted a = Weighted {weightedWeight :: Rig, weightedThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

unrestricted = Weighted Omega

instance Outputable a => Outputable (Weighted a) where
   ppr (Weighted cnt t) = ppr cnt <> ppr t

weightedSet :: Weighted a -> b -> Weighted b
weightedSet x b = fmap (\_->b) x
