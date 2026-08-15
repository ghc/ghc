{-# OPTIONS_GHC -ddump-simpl #-}

-- #7803: foo1 should be fully specialised at Double.  In the bug report
-- it ended up calling the overloaded exp5Tail (with a dictionary argument),
-- because the specialiser ran before the overloaded call was floated to
-- the top level.  So the dumped Core for this module must mention no
-- exp5Tail at all.
module T7803 (foo1) where

import T7803a

bar :: Floating a => a -> a -> a
bar z = subtract (evaluate integ z) . evaluate integ
  where
    integ = indefinite (Piecewise [Poly4 1])
{-# SPECIALIZE bar :: Double -> Double -> Double #-}

foo :: Floating a => a -> a -> a
foo x = bar x
{-# SPECIALIZE foo :: Double -> Double -> Double #-}

foo1 :: Double -> Double -> Double
foo1 x = foo x
