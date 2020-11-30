{-# OPTIONS_GHC -O -fforce-recomp #-}
{-# LANGUAGE GADTs #-}

module T18982 where

data Box a where
  Box :: a -> Box a

data Ex a where
  Ex :: e -> a -> Ex a

data GADT a where
  GADT :: Int -> GADT Int

data ExGADT a where
  ExGADT :: (e ~ Int) => e -> Int -> ExGADT Int

f :: Box Int -> Int
f (Box n) = n + 1
{-# NOINLINE f #-}

g :: Ex Int -> Int
g (Ex e n) = e `seq` n + 1
{-# NOINLINE g #-}

h :: GADT a -> Int
h (GADT n) = n + 1
{-# NOINLINE h #-}

i :: ExGADT a -> Int
i (ExGADT e n) = e `seq` n + 1
{-# NOINLINE i #-}

