{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Escape where

data ExpGADT t where
  ExpInt :: Int -> ExpGADT Int

data Hidden = forall t . Hidden (ExpGADT t) (ExpGADT t)

hval = Hidden (ExpInt 0) (ExpInt 1)

-- With the type sig this is ok, but without it maybe
-- should be rejected because the result type is wobbly
--    weird1 :: ExpGADT Int
--
-- And indeed it is rejected by GHC 7.8 because OutsideIn
-- doesn't unify under an equality constraint.

weird1 = case (hval :: Hidden) of Hidden (ExpInt _) a -> a
  -- Hidden t (ExpInt (co :: t ~ Int) _ :: ExpGADT t) (a :: ExpGADT t)

weird2 :: ExpGADT Int
weird2 = case (hval :: Hidden) of Hidden (ExpInt _) a -> a
