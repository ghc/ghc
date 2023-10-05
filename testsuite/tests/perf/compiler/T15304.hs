{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}



--------------------------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2018 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
--
-- Library implementing standard functions for the <https://en.wikipedia.org/wiki/Algebra_of_physical_space Algebra of Physical Space> Cl(3,0)
--
---------------------------------------------------------------------------------------------


module T15304 where



data Cl3 where
  R   :: !Double -> Cl3 -- Real Scalar Sub-algebra (G0)
  V3  :: !Double -> !Double -> !Double -> Cl3 -- Vectors (G1)
  BV  :: !Double -> !Double -> !Double -> Cl3 -- Bivectors (G2)
  I   :: !Double -> Cl3 -- Trivector Imaginary Pseudo-Scalar (G3)
  PV  :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Paravector (G0 + G1)
  H   :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Quaternion Even Sub-algebra (G0 + G2)
  C   :: !Double -> !Double -> Cl3 -- Complex Sub-algebra (G0 + G3)
  BPV :: !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> Cl3 -- Biparavector (G1 + G2)
  ODD :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Odd (G1 + G3)
  TPV :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Triparavector (G2 + G3)
  APS :: !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> Cl3 -- Algebra of Physical Space (G0 + G1 + G2 + G3)


instance Num Cl3 where
  (R a0) + (R b0) = R (a0 + b0)

  (R a0) + (V3 b1 b2 b3) = PV a0 b1 b2 b3
  (R a0) + (BV b23 b31 b12) = H a0 b23 b31 b12
  (R a0) + (I b123) = C a0 b123
  (R a0) + (PV b0 b1 b2 b3) = PV (a0 + b0) b1 b2 b3
  (R a0) + (H b0 b23 b31 b12) = H (a0 + b0) b23 b31 b12
  (R a0) + (C b0 b123) = C (a0 + b0) b123
  (R a0) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 b1 b2 b3 b23 b31 b12 0
  (R a0) + (ODD b1 b2 b3 b123) = APS a0 b1 b2 b3 0 0 0 b123
  (R a0) + (TPV b23 b31 b12 b123) = APS a0 0 0 0 b23 b31 b12 b123
  (R a0) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) b1 b2 b3 b23 b31 b12 b123

  (V3 a1 a2 a3) + (R b0) = PV b0 a1 a2 a3
  (BV a23 a31 a12) + (R b0) = H b0 a23 a31 a12
  (I a123) + (R b0) = C b0 a123
  (PV a0 a1 a2 a3) + (R b0) = PV (a0 + b0) a1 a2 a3
  (H a0 a23 a31 a12) + (R b0) = H (a0 + b0) a23 a31 a12
  (C a0 a123) + (R b0) = C (a0 + b0) a123
  (BPV a1 a2 a3 a23 a31 a12) + (R b0) = APS b0 a1 a2 a3 a23 a31 a12 0
  (ODD a1 a2 a3 a123) + (R b0) = APS b0 a1 a2 a3 0 0 0 a123
  (TPV a23 a31 a12 a123) + (R b0) = APS b0 0 0 0 a23 a31 a12 a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (R b0) = APS (a0 + b0) a1 a2 a3 a23 a31 a12 a123

  (V3 a1 a2 a3) + (V3 b1 b2 b3) = V3 (a1 + b1) (a2 + b2) (a3 + b3)

  (V3 a1 a2 a3) + (BV b23 b31 b12) = BPV a1 a2 a3 b23 b31 b12
  (V3 a1 a2 a3) + (I b123) = ODD a1 a2 a3 b123
  (V3 a1 a2 a3) + (PV b0 b1 b2 b3) = PV b0 (a1 + b1) (a2 + b2) (a3 + b3)
  (V3 a1 a2 a3) + (H b0 b23 b31 b12) = APS b0 a1 a2 a3 b23 b31 b12 0
  (V3 a1 a2 a3) + (C b0 b123) = APS b0 a1 a2 a3 0 0 0 b123
  (V3 a1 a2 a3) + (BPV b1 b2 b3 b23 b31 b12) = BPV (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12
  (V3 a1 a2 a3) + (ODD b1 b2 b3 b123) = ODD (a1 + b1) (a2 + b2) (a3 + b3) b123
  (V3 a1 a2 a3) + (TPV b23 b31 b12 b123) = APS 0 a1 a2 a3 b23 b31 b12 b123
  (V3 a1 a2 a3) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 b123

  (BV a23 a31 a12) + (V3 b1 b2 b3) = BPV b1 b2 b3 a23 a31 a12
  (I a123) + (V3 b1 b2 b3) = ODD b1 b2 b3 a123
  (PV a0 a1 a2 a3) + (V3 b1 b2 b3) = PV a0 (a1 + b1) (a2 + b2) (a3 + b3)
  (H a0 a23 a31 a12) + (V3 b1 b2 b3) = APS a0 b1 b2 b3 a23 a31 a12 0
  (C a0 a123) + (V3 b1 b2 b3) = APS a0 b1 b2 b3 0 0 0 a123
  (BPV a1 a2 a3 a23 a31 a12) + (V3 b1 b2 b3) = BPV (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12
  (ODD a1 a2 a3 a123) + (V3 b1 b2 b3) = ODD (a1 + b1) (a2 + b2) (a3 + b3) a123
  (TPV a23 a31 a12 a123) + (V3 b1 b2 b3) = APS 0 b1 b2 b3 a23 a31 a12 a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (V3 b1 b2 b3) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 a123

  (BV a23 a31 a12) + (BV b23 b31 b12) = BV (a23 + b23) (a31 + b31) (a12 + b12)

  (BV a23 a31 a12) + (I b123) = TPV a23 a31 a12 b123
  (BV a23 a31 a12) + (PV b0 b1 b2 b3) = APS b0 b1 b2 b3 a23 a31 a12 0
  (BV a23 a31 a12) + (H b0 b23 b31 b12) = H b0 (a23 + b23) (a31 + b31) (a12 + b12)
  (BV a23 a31 a12) + (C b0 b123) = APS b0 0 0 0 a23 a31 a12 b123
  (BV a23 a31 a12) + (BPV b1 b2 b3 b23 b31 b12) = BPV b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12)
  (BV a23 a31 a12) + (ODD b1 b2 b3 b123) = APS 0 b1 b2 b3 a23 a31 a12 b123
  (BV a23 a31 a12) + (TPV b23 b31 b12 b123) = TPV (a23 + b23) (a31 + b31) (a12 + b12) b123
  (BV a23 a31 a12) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) b123

  (I a123) + (BV b23 b31 b12) = TPV b23 b31 b12 a123
  (PV a0 a1 a2 a3) + (BV b23 b31 b12) = APS a0 a1 a2 a3 b23 b31 b12 0
  (H a0 a23 a31 a12) + (BV b23 b31 b12) = H a0 (a23 + b23) (a31 + b31) (a12 + b12)
  (C a0 a123) + (BV b23 b31 b12) = APS a0 0 0 0 b23 b31 b12 a123
  (BPV a1 a2 a3 a23 a31 a12) + (BV b23 b31 b12) = BPV a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12)
  (ODD a1 a2 a3 a123) + (BV b23 b31 b12) = APS 0 a1 a2 a3 b23 b31 b12 a123
  (TPV a23 a31 a12 a123) + (BV b23 b31 b12) = TPV (a23 + b23) (a31 + b31) (a12 + b12) a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (BV b23 b31 b12) = APS a0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) a123

  (I a123) + (I b123) = I (a123 + b123)

  (I a123) + (PV b0 b1 b2 b3) = APS b0 b1 b2 b3 0 0 0 a123
  (I a123) + (H b0 b23 b31 b12) = APS b0 0 0 0 b23 b31 b12 a123
  (I a123) + (C b0 b123) = C b0 (a123 + b123)
  (I a123) + (BPV b1 b2 b3 b23 b31 b12) = APS 0 b1 b2 b3 b23 b31 b12 a123
  (I a123) + (ODD b1 b2 b3 b123) = ODD b1 b2 b3 (a123 + b123)
  (I a123) + (TPV b23 b31 b12 b123) = TPV b23 b31 b12 (a123 + b123)
  (I a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 b1 b2 b3 b23 b31 b12 (a123 + b123)

  (PV a0 a1 a2 a3) + (I b123) = APS a0 a1 a2 a3 0 0 0 b123
  (H a0 a23 a31 a12) + (I b123) = APS a0 0 0 0 a23 a31 a12 b123
  (C a0 a123) + (I b123) = C a0 (a123 + b123)
  (BPV a1 a2 a3 a23 a31 a12) + (I b123) = APS 0 a1 a2 a3 a23 a31 a12 b123
  (ODD a1 a2 a3 a123) + (I b123) = ODD a1 a2 a3 (a123 + b123)
  (TPV a23 a31 a12 a123) + (I b123) = TPV a23 a31 a12 (a123 + b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (I b123) = APS a0 a1 a2 a3 a23 a31 a12 (a123 + b123)

  (PV a0 a1 a2 a3) + (PV b0 b1 b2 b3) = PV (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)

  (PV a0 a1 a2 a3) + (H b0 b23 b31 b12) = APS (a0 + b0) a1 a2 a3 b23 b31 b12 0
  (PV a0 a1 a2 a3) + (C b0 b123) = APS (a0 + b0) a1 a2 a3 0 0 0 b123
  (PV a0 a1 a2 a3) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 0
  (PV a0 a1 a2 a3) + (ODD b1 b2 b3 b123) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) 0 0 0 b123
  (PV a0 a1 a2 a3) + (TPV b23 b31 b12 b123) = APS a0 a1 a2 a3 b23 b31 b12 b123
  (PV a0 a1 a2 a3) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 b123

  (H a0 a23 a31 a12) + (PV b0 b1 b2 b3) = APS (a0 + b0) b1 b2 b3 a23 a31 a12 0
  (C a0 a123) + (PV b0 b1 b2 b3) = APS (a0 + b0) b1 b2 b3 0 0 0 a123
  (BPV a1 a2 a3 a23 a31 a12) + (PV b0 b1 b2 b3) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 0
  (ODD a1 a2 a3 a123) + (PV b0 b1 b2 b3) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) 0 0 0 a123
  (TPV a23 a31 a12 a123) + (PV b0 b1 b2 b3) = APS b0 b1 b2 b3 a23 a31 a12 a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (PV b0 b1 b2 b3) = APS (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 a123

  (H a0 a23 a31 a12) + (H b0 b23 b31 b12) = H (a0 + b0) (a23 + b23) (a31 + b31) (a12 + b12)

  (H a0 a23 a31 a12) + (C b0 b123) = APS (a0 + b0) 0 0 0 a23 a31 a12 b123
  (H a0 a23 a31 a12) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) 0
  (H a0 a23 a31 a12) + (ODD b1 b2 b3 b123) = APS a0 b1 b2 b3 a23 a31 a12 b123
  (H a0 a23 a31 a12) + (TPV b23 b31 b12 b123) = APS a0 0 0 0 (a23 + b23) (a31 + b31) (a12 + b12) b123
  (H a0 a23 a31 a12) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) b123

  (C a0 a123) + (H b0 b23 b31 b12) = APS (a0 + b0) 0 0 0 b23 b31 b12 a123
  (BPV a1 a2 a3 a23 a31 a12) + (H b0 b23 b31 b12) = APS b0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) 0
  (ODD a1 a2 a3 a123) + (H b0 b23 b31 b12) = APS b0 a1 a2 a3 b23 b31 b12 a123
  (TPV a23 a31 a12 a123) + (H b0 b23 b31 b12) = APS b0 0 0 0 (a23 + b23) (a31 + b31) (a12 + b12) a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (H b0 b23 b31 b12) = APS (a0 + b0) a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) a123

  (C a0 a123) + (C b0 b123) = C (a0 + b0) (a123 + b123)

  (C a0 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 b1 b2 b3 b23 b31 b12 a123
  (C a0 a123) + (ODD b1 b2 b3 b123) = APS a0 b1 b2 b3 0 0 0 (a123 + b123)
  (C a0 a123) + (TPV b23 b31 b12 b123) = APS a0 0 0 0 b23 b31 b12 (a123 + b123)
  (C a0 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) b1 b2 b3 b23 b31 b12 (a123 + b123)

  (BPV a1 a2 a3 a23 a31 a12) + (C b0 b123) = APS b0 a1 a2 a3 a23 a31 a12 b123
  (ODD a1 a2 a3 a123) + (C b0 b123) = APS b0 a1 a2 a3 0 0 0 (a123 + b123)
  (TPV a23 a31 a12 a123) + (C b0 b123) = APS b0 0 0 0 a23 a31 a12 (a123 + b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (C b0 b123) = APS (a0 + b0) a1 a2 a3 a23 a31 a12 (a123 + b123)

  (BPV a1 a2 a3 a23 a31 a12) + (BPV b1 b2 b3 b23 b31 b12) = BPV (a1 + b1) (a2 + b2) (a3 + b3) (a23 + b23) (a31 + b31) (a12 + b12)

  (BPV a1 a2 a3 a23 a31 a12) + (ODD b1 b2 b3 b123) = APS 0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 b123
  (BPV a1 a2 a3 a23 a31 a12) + (TPV b23 b31 b12 b123) = APS 0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) b123
  (BPV a1 a2 a3 a23 a31 a12) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) (a23 + b23) (a31 + b31) (a12 + b12) b123

  (ODD a1 a2 a3 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS 0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 a123
  (TPV a23 a31 a12 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS 0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) (a23 + b23) (a31 + b31) (a12 + b12) a123

  (ODD a1 a2 a3 a123) + (ODD b1 b2 b3 b123) = ODD (a1 + b1) (a2 + b2) (a3 + b3) (a123 + b123)

  (ODD a1 a2 a3 a123) + (TPV b23 b31 b12 b123) = APS 0 a1 a2 a3 b23 b31 b12 (a123 + b123)
  (ODD a1 a2 a3 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 (a123 + b123)

  (TPV a23 a31 a12 a123) + (ODD b1 b2 b3 b123) = APS 0 b1 b2 b3 a23 a31 a12 (a123 + b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (ODD b1 b2 b3 b123) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 (a123 + b123)

  (TPV a23 a31 a12 a123) + (TPV b23 b31 b12 b123) = TPV (a23 + b23) (a31 + b31) (a12 + b12) (a123 + b123)

  (TPV a23 a31 a12 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) (a123 + b123)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (TPV b23 b31 b12 b123) = APS a0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) (a123 + b123)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0)
                                                                                (a1 + b1) (a2 + b2) (a3 + b3)
                                                                                (a23 + b23) (a31 + b31) (a12 + b12)
                                                                                (a123 + b123)

  -- | Multiplication Instance implementing a Geometric Product
  (R a0) * (R b0) = R (a0*b0)

  (R a0) * (V3 b1 b2 b3) = V3 (a0*b1) (a0*b2) (a0*b3)
  (R a0) * (BV b23 b31 b12) = BV (a0*b23) (a0*b31) (a0*b12)
  (R a0) * (I b123) = I (a0*b123)
  (R a0) * (PV b0 b1 b2 b3) = PV (a0*b0)
                                 (a0*b1) (a0*b2) (a0*b3)
  (R a0) * (H b0 b23 b31 b12) = H (a0*b0)
                                  (a0*b23) (a0*b31) (a0*b12)
  (R a0) * (C b0 b123) = C (a0*b0)
                           (a0*b123)
  (R a0) * (BPV b1 b2 b3 b23 b31 b12) = BPV (a0*b1) (a0*b2) (a0*b3)
                                            (a0*b23) (a0*b31) (a0*b12)
  (R a0) * (ODD b1 b2 b3 b123) = ODD (a0*b1) (a0*b2) (a0*b3)
                                     (a0*b123)
  (R a0) * (TPV b23 b31 b12 b123) = TPV (a0*b23) (a0*b31) (a0*b12)
                                        (a0*b123)
  (R a0) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0)
                                                    (a0*b1) (a0*b2) (a0*b3)
                                                    (a0*b23) (a0*b31) (a0*b12)
                                                    (a0*b123)

  (V3 a1 a2 a3) * (R b0) = V3 (a1*b0) (a2*b0) (a3*b0)
  (BV a23 a31 a12) * (R b0) = BV (a23*b0) (a31*b0) (a12*b0)
  (I a123) * (R b0) = I (a123*b0)
  (PV a0 a1 a2 a3) * (R b0) = PV (a0*b0)
                                 (a1*b0) (a2*b0) (a3*b0)
  (H a0 a23 a31 a12) * (R b0) = H (a0*b0)
                                  (a23*b0) (a31*b0) (a12*b0)
  (C a0 a123) * (R b0) = C (a0*b0)
                           (a123*b0)
  (BPV a1 a2 a3 a23 a31 a12) * (R b0) = BPV (a1*b0) (a2*b0) (a3*b0)
                                            (a23*b0) (a31*b0) (a12*b0)
  (ODD a1 a2 a3 a123) * (R b0) = ODD (a1*b0) (a2*b0) (a3*b0)
                                     (a123*b0)
  (TPV a23 a31 a12 a123) * (R b0) = TPV (a23*b0) (a31*b0) (a12*b0)
                                        (a123*b0)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (R b0) = APS (a0*b0)
                                                    (a1*b0) (a2*b0) (a3*b0)
                                                    (a23*b0) (a31*b0) (a12*b0)
                                                    (a123*b0)

  (V3 a1 a2 a3) * (V3 b1 b2 b3) = H (a1*b1 + a2*b2 + a3*b3)
                                    (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)

  (V3 a1 a2 a3) * (BV b23 b31 b12) = ODD (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                         (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (I b123) = BV (a1*b123) (a2*b123) (a3*b123)
  (V3 a1 a2 a3) * (PV b0 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                         (a1*b0) (a2*b0) (a3*b0)
                                         (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                         0
  (V3 a1 a2 a3) * (H b0 b23 b31 b12) = ODD (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                           (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (C b0 b123) = BPV (a1*b0) (a2*b0) (a3*b0)
                                    (a1*b123) (a2*b123) (a3*b123)
  (V3 a1 a2 a3) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3)
                                                   (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                                   (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                                   (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (ODD b1 b2 b3 b123) = H (a1*b1 + a2*b2 + a3*b3)
                                          (a1*b123 + a2*b3 - a3*b2) (a2*b123 - a1*b3 + a3*b1) (a3*b123 + a1*b2 - a2*b1)
  (V3 a1 a2 a3) * (TPV b23 b31 b12 b123) = APS 0
                                               (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                               (a1*b123) (a2*b123) (a3*b123)
                                               (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a1*b1 + a2*b2 + a3*b3)
                                                           (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                                           (a1*b123 + a2*b3 - a3*b2) (a3*b1 - a1*b3 + a2*b123) (a1*b2 - a2*b1 + a3*b123)
                                                           (a1*b23 + a2*b31 + a3*b12)

  (BV a23 a31 a12) * (V3 b1 b2 b3) = ODD (a12*b2  - a31*b3) (a23*b3 - a12*b1) (a31*b1  - a23*b2)
                                         (a23*b1  + a31*b2  + a12*b3)
  (I a123) * (V3 b1 b2 b3) = BV (a123*b1) (a123*b2) (a123*b3)
  (PV a0 a1 a2 a3) * (V3 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                         (a0*b1) (a0*b2) (a0*b3)
                                         (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                         0
  (H a0 a23 a31 a12) * (V3 b1 b2 b3) = ODD (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                           (a23*b1 + a31*b2 + a12*b3)
  (C a0 a123) * (V3 b1 b2 b3) = BPV (a0*b1) (a0*b2) (a0*b3)
                                    (a123*b1) (a123*b2) (a123*b3)
  (BPV a1 a2 a3 a23 a31 a12) * (V3 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                                   (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                                   (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                                   (a23*b1 + a31*b2 + a12*b3)
  (ODD a1 a2 a3 a123) * (V3 b1 b2 b3) = H (a1*b1 + a2*b2 + a3*b3)
                                          (a123*b1 + a2*b3 - a3*b2) (a123*b2 - a1*b3 + a3*b1) (a123*b3 + a1*b2 - a2*b1)
  (TPV a23 a31 a12 a123) * (V3 b1 b2 b3) = APS 0
                                               (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                               (a123*b1) (a123*b2) (a123*b3)
                                               (a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (V3 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                                           (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                                           (a123*b1 + a2*b3 - a3*b2) (a3*b1 - a1*b3 + a123*b2) (a1*b2 - a2*b1 + a123*b3)
                                                           (a23*b1 + a31*b2 + a12*b3)

  (BV a23 a31 a12) * (BV b23 b31 b12) = H (negate $ a23*b23 + a31*b31 + a12*b12)
                                          (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)

  (BV a23 a31 a12) * (I b123) = V3 (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
  (BV a23 a31 a12) * (PV b0 b1 b2 b3) = APS 0
                                            (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                            (a23*b0) (a31*b0) (a12*b0)
                                            (a23*b1 + a31*b2 + a12*b3)
  (BV a23 a31 a12) * (H b0 b23 b31 b12) = H (negate $ a23*b23 + a31*b31 + a12*b12)
                                            (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
  (BV a23 a31 a12) * (C b0 b123) = BPV (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                       (a23*b0) (a31*b0) (a12*b0)
  (BV a23 a31 a12) * (BPV b1 b2 b3 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                      (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                                      (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                      (a23*b1 + a31*b2 + a12*b3)
  (BV a23 a31 a12) * (ODD b1 b2 b3 b123) = ODD (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a12*b1 - a31*b123) (a31*b1 - a23*b2 - a12*b123)
                                               (a23*b1 + a31*b2 + a12*b3)
  (BV a23 a31 a12) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                  (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                                  (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                  0
  (BV a23 a31 a12) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                              (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a31*b123 - a12*b1) (a31*b1 - a23*b2 - a12*b123)
                                                              (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
                                                              (a23*b1 + a31*b2 + a12*b3)

  (I a123) * (BV b23 b31 b12) = V3 (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
  (PV a0 a1 a2 a3) * (BV b23 b31 b12) = APS 0
                                            (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                            (a0*b23) (a0*b31) (a0*b12)
                                            (a1*b23 + a2*b31 + a3*b12)
  (H a0 a23 a31 a12) * (BV b23 b31 b12) = H (negate $ a23*b23 + a31*b31 + a12*b12)
                                            (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
  (C a0 a123) * (BV b23 b31 b12) = BPV (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                       (a0*b23) (a0*b31) (a0*b12)
  (BPV a1 a2 a3 a23 a31 a12) * (BV b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                      (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                                      (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                      (a1*b23 + a2*b31 + a3*b12)
  (ODD a1 a2 a3 a123) * (BV b23 b31 b12) = ODD (negate $ a123*b23 + a2*b12 - a3*b31)
                                               (negate $ a123*b31 - a1*b12 + a3*b23)
                                               (negate $ a123*b12 + a1*b31 - a2*b23)
                                               (a1*b23 + a2*b31 + a3*b12)
  (TPV a23 a31 a12 a123) * (BV b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                  (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                                  (negate $ a31*b12 - a12*b31) (negate $ a12*b23 - a23*b12) (negate $ a23*b31 - a31*b23)
                                                  0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (BV b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                              (a3*b31 - a123*b23 - a2*b12) (a1*b12 - a3*b23 - a123*b31) (a2*b23 - a123*b12 - a1*b31)
                                                              (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
                                                              (a1*b23 + a2*b31 + a3*b12)

  (I a123) * (I b123) = R (negate $ a123*b123)

  (I a123) * (PV b0 b1 b2 b3) = TPV (a123*b1) (a123*b2) (a123*b3)
                                    (a123*b0)
  (I a123) * (H b0 b23 b31 b12) = ODD (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                      (a123*b0)
  (I a123) * (C b0 b123) = C (negate $ a123*b123)
                             (a123*b0)
  (I a123) * (BPV b1 b2 b3 b23 b31 b12) = BPV (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                              (a123*b1) (a123*b2) (a123*b3)
  (I a123) * (ODD b1 b2 b3 b123) = H (negate $ a123*b123)
                                     (a123*b1) (a123*b2) (a123*b3)
  (I a123) * (TPV b23 b31 b12 b123) = PV (negate $ a123*b123)
                                         (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
  (I a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (negate $ a123*b123)
                                                      (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                                      (a123*b1) (a123*b2) (a123*b3)
                                                      (a123*b0)

  (PV a0 a1 a2 a3) * (I b123) = TPV (a1*b123) (a2*b123) (a3*b123)
                                    (a0*b123)
  (H a0 a23 a31 a12) * (I b123) = ODD (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                      (a0*b123)
  (C a0 a123) * (I b123) = C (negate $ a123*b123)
                             (a0*b123)
  (BPV a1 a2 a3 a23 a31 a12) * (I b123) = BPV (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                              (a1*b123) (a2*b123) (a3*b123)
  (ODD a1 a2 a3 a123) * (I b123) = H (negate $ a123*b123)
                                     (a1*b123) (a2*b123) (a3*b123)
  (TPV a23 a31 a12 a123) * (I b123) = PV (negate $ a123*b123)
                                         (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (I b123) = APS (negate $ a123*b123)
                                                      (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                                      (a1*b123) (a2*b123) (a3*b123)
                                                      (a0*b123)


  (PV a0 a1 a2 a3) * (PV b0 b1 b2 b3) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3)
                                            (a0*b1 + a1*b0) (a0*b2 + a2*b0) (a0*b3 + a3*b0)
                                            (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                            0

  (PV a0 a1 a2 a3) * (H b0 b23 b31 b12) = APS (a0*b0)
                                              (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                              (a0*b23) (a0*b31) (a0*b12)
                                              (a1*b23 + a2*b31 + a3*b12)
  (PV a0 a1 a2 a3) * (C b0 b123) = APS (a0*b0)
                                       (a1*b0) (a2*b0) (a3*b0)
                                       (a1*b123) (a2*b123) (a3*b123)
                                       (a0*b123)
  (PV a0 a1 a2 a3) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3)
                                                      (a0*b1 - a2*b12 + a3*b31) (a0*b2 + a1*b12 - a3*b23) (a0*b3 - a1*b31 + a2*b23)
                                                      (a0*b23 + a2*b3 - a3*b2) (a0*b31 - a1*b3 + a3*b1) (a0*b12 + a1*b2 - a2*b1)
                                                      (a1*b23 + a2*b31 + a3*b12)
  (PV a0 a1 a2 a3) * (ODD b1 b2 b3 b123) = APS (a1*b1 + a2*b2 + a3*b3)
                                               (a0*b1) (a0*b2) (a0*b3)
                                               (a1*b123 + a2*b3 - a3*b2) (a2*b123 - a1*b3 + a3*b1) (a3*b123 + a1*b2 - a2*b1)
                                               (a0*b123)
  (PV a0 a1 a2 a3) * (TPV b23 b31 b12 b123) = APS 0
                                                  (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                                  (a0*b23 + a1*b123) (a0*b31 + a2*b123) (a0*b12 + a3*b123)
                                                  (a0*b123 + a1*b23 + a2*b31 + a3*b12)
  (PV a0 a1 a2 a3) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3)
                                                              (a0*b1 + a1*b0 - a2*b12 + a3*b31)
                                                              (a0*b2 + a2*b0 + a1*b12 - a3*b23)
                                                              (a0*b3 + a3*b0 - a1*b31 + a2*b23)
                                                              (a0*b23 + a1*b123 + a2*b3 - a3*b2)
                                                              (a0*b31 - a1*b3 + a3*b1 + a2*b123)
                                                              (a0*b12 + a1*b2 - a2*b1 + a3*b123)
                                                              (a0*b123 + a1*b23 + a2*b31 + a3*b12)

  (H a0 a23 a31 a12) * (PV b0 b1 b2 b3) = APS (a0*b0)
                                              (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                              (a23*b0) (a31*b0) (a12*b0)
                                              (a23*b1 + a31*b2 + a12*b3)
  (C a0 a123) * (PV b0 b1 b2 b3) = APS (a0*b0)
                                       (a0*b1) (a0*b2) (a0*b3)
                                       (a123*b1) (a123*b2) (a123*b3)
                                       (a123*b0)
  (BPV a1 a2 a3 a23 a31 a12) * (PV b0 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                                      (a1*b0 + a12*b2 - a31*b3) (a2*b0 - a12*b1 + a23*b3) (a3*b0 + a31*b1 - a23*b2)
                                                      (a23*b0 + a2*b3 - a3*b2) (a31*b0 - a1*b3 + a3*b1) (a12*b0 + a1*b2 - a2*b1)
                                                      (a23*b1 + a31*b2 + a12*b3)
  (ODD a1 a2 a3 a123) * (PV b0 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                               (a1*b0) (a2*b0) (a3*b0)
                                               (a123*b1 + a2*b3 - a3*b2)
                                               (a123*b2 - a1*b3 + a3*b1)
                                               (a123*b3 + a1*b2 - a2*b1)
                                               (a123*b0)
  (TPV a23 a31 a12 a123) * (PV b0 b1 b2 b3) = APS 0
                                                  (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                                  (a23*b0 + a123*b1) (a31*b0 + a123*b2) (a12*b0 + a123*b3)
                                                  (a123*b0 + a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (PV b0 b1 b2 b3) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3)
                                                              (a0*b1 + a1*b0 + a12*b2 - a31*b3)
                                                              (a0*b2 + a2*b0 - a12*b1 + a23*b3)
                                                              (a0*b3 + a3*b0 + a31*b1 - a23*b2)
                                                              (a23*b0 + a123*b1 + a2*b3 - a3*b2)
                                                              (a31*b0 - a1*b3 + a3*b1 + a123*b2)
                                                              (a12*b0 + a1*b2 - a2*b1 + a123*b3)
                                                              (a123*b0 + a23*b1 + a31*b2 + a12*b3)

  (H a0 a23 a31 a12) * (H b0 b23 b31 b12) = H (a0*b0 - a23*b23 - a31*b31 - a12*b12)
                                              (a0*b23 + a23*b0 - a31*b12 + a12*b31)
                                              (a0*b31 + a31*b0 + a23*b12 - a12*b23)
                                              (a0*b12 + a12*b0 - a23*b31 + a31*b23)

  (H a0 a23 a31 a12) * (C b0 b123) = APS (a0*b0)
                                         (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                         (a23*b0) (a31*b0) (a12*b0)
                                         (a0*b123)
  (H a0 a23 a31 a12) * (BPV b1 b2 b3 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                        (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                                        (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
                                                        (a23*b1 + a31*b2  + a12*b3)
  (H a0 a23 a31 a12) * (ODD b1 b2 b3 b123) = ODD (a0*b1 + a12*b2 - a31*b3 - a23*b123)
                                                 (a0*b2 - a12*b1 + a23*b3 - a31*b123)
                                                 (a0*b3 + a31*b1 - a23*b2 - a12*b123)
                                                 (a0*b123 + a23*b1 + a31*b2 + a12*b3)
  (H a0 a23 a31 a12) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                    (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                                    (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
                                                    (a0*b123)
  (H a0 a23 a31 a12) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 - a23*b23 - a31*b31 - a12*b12)
                                                                (a0*b1 + a12*b2 - a31*b3 - a23*b123)
                                                                (a0*b2 - a12*b1 + a23*b3 - a31*b123)
                                                                (a0*b3 + a31*b1 - a23*b2 - a12*b123)
                                                                (a0*b23 + a23*b0 - a31*b12 + a12*b31)
                                                                (a0*b31 + a31*b0 + a23*b12 - a12*b23)
                                                                (a0*b12 + a12*b0 - a23*b31 + a31*b23)
                                                                (a0*b123 + a23*b1 + a31*b2 + a12*b3)

  (C a0 a123) * (H b0 b23 b31 b12) = APS (a0*b0)
                                         (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                         (a0*b23) (a0*b31) (a0*b12)
                                         (a123*b0)
  (BPV a1 a2 a3 a23 a31 a12) * (H b0 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                        (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                                        (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
                                                        (a1*b23 + a2*b31 + a3*b12)
  (ODD a1 a2 a3 a123) * (H b0 b23 b31 b12) = ODD (a1*b0 - a2*b12 + a3*b31 - a123*b23)
                                                 (a2*b0 + a1*b12 - a3*b23 - a123*b31)
                                                 (a3*b0 - a1*b31 + a2*b23 - a123*b12)
                                                 (a123*b0 + a1*b23 + a2*b31 + a3*b12)
  (TPV a23 a31 a12 a123) * (H b0 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                    (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                                    (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
                                                    (a123*b0)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (H b0 b23 b31 b12) = APS (a0*b0 - a23*b23 - a31*b31 - a12*b12)
                                                                (a1*b0 - a2*b12 + a3*b31 - a123*b23)
                                                                (a2*b0 + a1*b12 - a3*b23 - a123*b31)
                                                                (a3*b0 - a1*b31 + a2*b23 - a123*b12)
                                                                (a0*b23 + a23*b0 - a31*b12 + a12*b31)
                                                                (a0*b31 + a31*b0 + a23*b12 - a12*b23)
                                                                (a0*b12 + a12*b0 - a23*b31 + a31*b23)
                                                                (a123*b0 + a1*b23 + a2*b31 + a3*b12)

  (C a0 a123) * (C b0 b123) = C (a0*b0 - a123*b123)
                                (a0*b123 + a123*b0)

  (C a0 a123) * (BPV b1 b2 b3 b23 b31 b12) = BPV (a0*b1 - a123*b23) (a0*b2 - a123*b31) (a0*b3 - a123*b12)
                                                 (a0*b23 + a123*b1) (a0*b31 + a123*b2) (a0*b12 + a123*b3)
  (C a0 a123) * (ODD b1 b2 b3 b123) = APS (negate $ a123*b123)
                                          (a0*b1) (a0*b2) (a0*b3)
                                          (a123*b1) (a123*b2) (a123*b3)
                                          (a0*b123)
  (C a0 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a123*b123)
                                             (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                             (a0*b23) (a0*b31) (a0*b12)
                                             (a0*b123)
  (C a0 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 - a123*b123)
                                                         (a0*b1 - a123*b23) (a0*b2 - a123*b31) (a0*b3 - a123*b12)
                                                         (a0*b23 + a123*b1) (a0*b31 + a123*b2) (a0*b12 + a123*b3)
                                                         (a0*b123 + a123*b0)

  (BPV a1 a2 a3 a23 a31 a12) * (C b0 b123) = BPV (a1*b0 - a23*b123) (a2*b0 - a31*b123) (a3*b0 - a12*b123)
                                                 (a23*b0 + a1*b123) (a31*b0 + a2*b123) (a12*b0 + a3*b123)
  (ODD a1 a2 a3 a123) * (C b0 b123) = APS (negate $ a123*b123)
                                          (a1*b0) (a2*b0) (a3*b0)
                                          (a1*b123) (a2*b123) (a3*b123)
                                          (a123*b0)
  (TPV a23 a31 a12 a123) * (C b0 b123) = APS (negate $ a123*b123)
                                             (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                             (a23*b0) (a31*b0) (a12*b0)
                                             (a123*b0)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (C b0 b123) = APS (a0*b0 - a123*b123)
                                                         (a1*b0 - a23*b123) (a2*b0 - a31*b123) (a3*b0 - a12*b123)
                                                         (a23*b0 + a1*b123) (a31*b0 + a2*b123) (a12*b0 + a3*b123)
                                                         (a0*b123 + a123*b0)

  (BPV a1 a2 a3 a23 a31 a12) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12)
                                                                (a12*b2 - a2*b12 + a3*b31 - a31*b3)
                                                                (a1*b12 - a12*b1 - a3*b23 + a23*b3)
                                                                (a31*b1 - a1*b31 + a2*b23 - a23*b2)
                                                                (a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                (a3*b1 - a1*b3 + a23*b12 - a12*b23)
                                                                (a1*b2 - a2*b1 - a23*b31 + a31*b23)
                                                                (a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)

  (BPV a1 a2 a3 a23 a31 a12) * (ODD b1 b2 b3 b123) = APS (a1*b1 + a2*b2 + a3*b3)
                                                         (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a12*b1 - a31*b123) (a31*b1 - a23*b2 - a12*b123)
                                                         (a1*b123 + a2*b3 - a3*b2) (a2*b123 - a1*b3 + a3*b1) (a3*b123 + a1*b2 - a2*b1)
                                                         (a23*b1 + a31*b2 + a12*b3)
  (BPV a1 a2 a3 a23 a31 a12) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                            (a3*b31 - a2*b12 - a23*b123) (a1*b12 - a3*b23 - a31*b123) (a2*b23 - a1*b31 - a12*b123)
                                                            (a1*b123 - a31*b12 + a12*b31) (a2*b123 + a23*b12 - a12*b23) (a3*b123 - a23*b31 + a31*b23)
                                                            (a1*b23 + a2*b31 + a3*b12)
  (BPV a1 a2 a3 a23 a31 a12) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12)
                                                                        (a1*b0 - a2*b12 + a12*b2 + a3*b31 - a31*b3 - a23*b123)
                                                                        (a2*b0 + a1*b12 - a12*b1 - a3*b23 + a23*b3 - a31*b123)
                                                                        (a3*b0 - a1*b31 + a31*b1 + a2*b23 - a23*b2 - a12*b123)
                                                                        (a23*b0 + a1*b123 + a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                        (a31*b0 - a1*b3 + a3*b1 + a2*b123 + a23*b12 - a12*b23)
                                                                        (a12*b0 + a1*b2 - a2*b1 + a3*b123 - a23*b31 + a31*b23)
                                                                        (a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)

  (ODD a1 a2 a3 a123) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3)
                                                         (a3*b31 - a2*b12 - a123*b23) (a1*b12 - a3*b23 - a123*b31) (a2*b23 - a1*b31 - a123*b12)
                                                         (a123*b1 + a2*b3 - a3*b2) (a123*b2 - a1*b3 + a3*b1) (a123*b3 + a1*b2 - a2*b1)
                                                         (a1*b23 + a2*b31 + a3*b12)
  (TPV a23 a31 a12 a123) * (BPV b1 b2 b3 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                            (a12*b2 - a31*b3 - a123*b23) (a23*b3 - a12*b1 - a123*b31) (a31*b1 - a23*b2 - a123*b12)
                                                            (a123*b1 - a31*b12 + a12*b31) (a123*b2 + a23*b12 - a12*b23) (a123*b3 - a23*b31 + a31*b23)
                                                            (a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12)
                                                                        (a0*b1 - a2*b12 + a12*b2 + a3*b31 - a31*b3 - a123*b23)
                                                                        (a0*b2 + a1*b12 - a12*b1 - a3*b23 + a23*b3 - a123*b31)
                                                                        (a0*b3 - a1*b31 + a31*b1 + a2*b23 - a23*b2 - a123*b12)
                                                                        (a0*b23 + a123*b1 + a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                        (a0*b31 - a1*b3 + a3*b1 + a123*b2 + a23*b12 - a12*b23)
                                                                        (a0*b12 + a1*b2 - a2*b1 + a123*b3 - a23*b31 + a31*b23)
                                                                        (a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)

  (ODD a1 a2 a3 a123) * (ODD b1 b2 b3 b123) = H (a1*b1 + a2*b2 + a3*b3 - a123*b123)
                                                (a1*b123 + a123*b1 + a2*b3 - a3*b2)
                                                (a2*b123 + a123*b2 - a1*b3 + a3*b1)
                                                (a3*b123 + a123*b3 + a1*b2 - a2*b1)

  (ODD a1 a2 a3 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a123*b123)
                                                     (a3*b31 - a2*b12 - a123*b23) (a1*b12 - a3*b23 - a123*b31) (a2*b23 - a1*b31 - a123*b12)
                                                     (a1*b123) (a2*b123) (a3*b123)
                                                     (a1*b23 + a2*b31 + a3*b12)
  (ODD a1 a2 a3 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a1*b1 + a2*b2 + a3*b3 - a123*b123)
                                                                 (a1*b0 - a2*b12 + a3*b31 - a123*b23)
                                                                 (a2*b0 + a1*b12 - a3*b23 - a123*b31)
                                                                 (a3*b0 - a1*b31 + a2*b23 - a123*b12)
                                                                 (a1*b123 + a123*b1 + a2*b3 - a3*b2)
                                                                 (a2*b123 + a123*b2 - a1*b3 + a3*b1)
                                                                 (a3*b123 + a123*b3 + a1*b2 - a2*b1)
                                                                 (a123*b0 + a1*b23 + a2*b31 + a3*b12)

  (TPV a23 a31 a12 a123) * (ODD b1 b2 b3 b123) = APS (negate $ a123*b123)
                                                     (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a12*b1 - a31*b123) (a31*b1 - a23*b2 - a12*b123)
                                                     (a123*b1) (a123*b2) (a123*b3)
                                                     (a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (ODD b1 b2 b3 b123) = APS (a1*b1 + a2*b2 + a3*b3 - a123*b123)
                                                                 (a0*b1 + a12*b2 - a31*b3 - a23*b123)
                                                                 (a0*b2 - a12*b1 + a23*b3 - a31*b123)
                                                                 (a0*b3 + a31*b1 - a23*b2 - a12*b123)
                                                                 (a1*b123 + a123*b1 + a2*b3 - a3*b2)
                                                                 (a2*b123 + a123*b2 - a1*b3 + a3*b1)
                                                                 (a3*b123 + a123*b3 + a1*b2 - a2*b1)
                                                                 (a0*b123 + a23*b1 + a31*b2 + a12*b3)

  (TPV a23 a31 a12 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12 + a123*b123)
                                                        (negate $ a23*b123 + a123*b23) (negate $ a31*b123 + a123*b31) (negate $ a12*b123 + a123*b12)
                                                        (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                        0

  (TPV a23 a31 a12 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12 + a123*b123)
                                                                    (a12*b2 - a31*b3 - a23*b123 - a123*b23)
                                                                    (a23*b3 - a12*b1 - a31*b123 - a123*b31)
                                                                    (a31*b1 - a23*b2 - a12*b123 - a123*b12)
                                                                    (a23*b0 + a123*b1 - a31*b12 + a12*b31)
                                                                    (a31*b0 + a123*b2 + a23*b12 - a12*b23)
                                                                    (a12*b0 + a123*b3 - a23*b31 + a31*b23)
                                                                    (a123*b0 + a23*b1 + a31*b2 + a12*b3)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12 + a123*b123)
                                                                    (a3*b31 - a2*b12 - a23*b123 - a123*b23)
                                                                    (a1*b12 - a3*b23 - a31*b123 - a123*b31)
                                                                    (a2*b23 - a1*b31 - a12*b123 - a123*b12)
                                                                    (a0*b23 + a1*b123 - a31*b12 + a12*b31)
                                                                    (a0*b31 + a2*b123 + a23*b12 - a12*b23)
                                                                    (a0*b12 + a3*b123 - a23*b31 + a31*b23)
                                                                    (a0*b123 + a1*b23 + a2*b31 + a3*b12)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12 - a123*b123)
                                                                                (a0*b1 + a1*b0 - a2*b12 + a12*b2 + a3*b31 - a31*b3 - a23*b123 - a123*b23)
                                                                                (a0*b2 + a2*b0 + a1*b12 - a12*b1 - a3*b23 + a23*b3 - a31*b123 - a123*b31)
                                                                                (a0*b3 + a3*b0 - a1*b31 + a31*b1 + a2*b23 - a23*b2 - a12*b123 - a123*b12)
                                                                                (a0*b23 + a23*b0 + a1*b123 + a123*b1 + a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                                (a0*b31 + a31*b0 - a1*b3 + a3*b1 + a2*b123 + a123*b2 + a23*b12 - a12*b23)
                                                                                (a0*b12 + a12*b0 + a1*b2 - a2*b1 + a3*b123 + a123*b3 - a23*b31 + a31*b23)
                                                                                (a0*b123 + a123*b0 + a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)


  -- |'abs' is the spectral norm aka the spectral radius
  -- it is the largest singular value. This function may need to be fiddled with
  -- to make the math a bit safer wrt overflows.  This makes use of the largest
  -- singular value, if the smallest singular value is zero then the element is not
  -- invertable, we can see here that R, C, V3, BV, and H are all invertable.
  abs (R a0) = R (abs a0) -- absolute value of a real number
  abs (V3 a1 a2 a3) = R (sqrt (a1^2 + a2^2 + a3^2)) -- magnitude of a vector
  abs (BV a23 a31 a12) = R (sqrt (a23^2 + a31^2 + a12^2)) -- magnitude of a bivector
  abs (I a123) = R (abs a123) -- magnitude of a Imaginary number
  abs (PV a0 a1 a2 a3) = R (sqrt (a0^2 + a1^2 + a2^2 + a3^2 + 2 * abs a0 * sqrt (a1^2 + a2^2 + a3^2)))
  abs (H a0 a23 a31 a12) = R (sqrt (a0^2 + a23^2 + a31^2 + a12^2)) -- largest singular value
  abs (C a0 a123) = R (sqrt (a0^2 + a123^2)) -- magnitude of a complex number
  abs (BPV a1 a2 a3 a23 a31 a12) = R (sqrt (a1^2 + a23^2 + a2^2 + a31^2 + a3^2 + a12^2 +
                                             2 * sqrt ((a1*a31 - a2*a23)^2 + (a1*a12 - a3*a23)^2 + (a2*a12 - a3*a31)^2)))
  abs (ODD a1 a2 a3 a123) = R (sqrt (a1^2 + a2^2 + a3^2 + a123^2))
  abs (TPV a23 a31 a12 a123) = R (sqrt (a23^2 + a31^2 + a12^2 + a123^2 + 2 * abs a123 * sqrt (a23^2 + a31^2 + a12^2)))
  abs (APS a0 a1 a2 a3 a23 a31 a12 a123) = R (sqrt (a0^2 + a1^2 + a2^2 + a3^2 + a23^2 + a31^2 + a12^2 + a123^2 +
                                                    2 * sqrt ((a0*a1 + a123*a23)^2 + (a0*a2 + a123*a31)^2 + (a0*a3 + a123*a12)^2 +
                                                              (a2*a12 - a3*a31)^2 + (a3*a23 - a1*a12)^2 + (a1*a31 - a2*a23)^2)))


  -- |'signum' satisfies the Law "abs x * signum x == x"
  -- kind of cool: signum of a vector is the unit vector.
  signum cliffor = let (R mag) = abs cliffor in cliffor * R (recip mag)


  -- |'fromInteger'
  fromInteger int = R (fromInteger int)


  -- |'negate' simply distributes into the grade components
  negate x = x

instance Fractional Cl3 where

instance Floating Cl3 where
  pi = R pi

  --
  exp (C a0 a123) =
    let expa0 = exp a0
    in C (expa0 * cos a123) (expa0 * sin a123)
  exp cliffor = reduce $! spectraldcmp exp cliffor

  --
  log (C a0 a123) = C (log (sqrt (a0^2 + a123^2))) (atan2 a123 a0)
  log cliffor = reduce $! spectraldcmp log cliffor

  --
  sqrt (C a0 a123) = C u (if a123 < 0 then -v else v)
                       where (u,v) = if a0 < 0 then (v',u') else (u',v')
                             v'    = abs a123 / (u'*2)
                             u'    = sqrt ((sqrt (a0^2 + a123^2) + abs a0) / 2)
  sqrt cliffor = reduce $! spectraldcmp sqrt cliffor

  --
  sin (C a0 a123) = C (sin a0 * cosh a123) (cos a0 * sinh a123)
  sin cliffor = reduce $! spectraldcmp sin cliffor

  --
  cos (C a0 a123) = C (cos a0 * cosh a123) (negate $ sin a0 * sinh a123)
  cos cliffor = reduce $! spectraldcmp cos cliffor

  --
  tan (C a0 a123) = C (sinx*coshy) (cosx*sinhy) / C (cosx*coshy) (negate $ sinx*sinhy)
                       where sinx  = sin a0
                             cosx  = cos a0
                             sinhy = sinh a123
                             coshy = cosh a123
  tan cliffor = reduce $! spectraldcmp tan cliffor

  --
  asin (C a0 a123) = C a123' (-a0')
                       where  (C a0' a123') = log (C (-a123) a0 + sqrt (1 - C a0 a123 * C a0 a123)) -- check this
  asin cliffor = reduce $! spectraldcmp asin cliffor

  --
  acos (C a0 a123) = C a123'' (-a0'')
               where (C a0'' a123'') = log (C a0 a123 + C (-a123') a0')  -- check this
                     (C a0' a123')   = sqrt (1 - C a0 a123 * C a0 a123)  -- check this
  acos cliffor = reduce $! spectraldcmp acos cliffor

  --
  atan (C a0 a123) = C a123' (-a0')
                       where (C a0' a123') = log (C (1-a123) a0 / sqrt (1 + C a0 a123 * C a0 a123))  -- check this
  atan cliffor = reduce $! spectraldcmp atan cliffor

  --
  sinh (C a0 a123) = C (cos a123 * sinh a0) (sin a123 * cosh a0)
  sinh cliffor = reduce $! spectraldcmp sinh cliffor

  --
  cosh (C a0 a123) = C (cos a123 * cosh a0) (sin a123 * sinh a0)
  cosh cliffor = reduce $! spectraldcmp cosh cliffor

  --
  tanh (C a0 a123) = C (cosy*sinhx) (siny*coshx) / C (cosy*coshx) (siny*sinhx)
                        where siny  = sin a123
                              cosy  = cos a123
                              sinhx = sinh a0
                              coshx = cosh a0
  tanh cliffor = reduce $! spectraldcmp tanh cliffor




--

spectraldcmp ::  (Cl3 -> Cl3) -> Cl3 -> Cl3
spectraldcmp function cliffor =
  let (eig1,eig2) = projEigs cliffor
  in function eig1 * cliffor + function eig2 * cliffor



projEigs :: Cl3 -> (Cl3,Cl3)
projEigs cliffor =
  let eig1 = 2 * (cliffor * cliffor * cliffor)
  in (eig1,eig1)

reduce :: Cl3 -> Cl3
reduce r@R{} = r
reduce aps@APS{} = aps
