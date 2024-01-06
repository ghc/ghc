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

-- | Expected worker type:
-- $wf :: Int# -> Int#
f :: Box Int -> Int
f (Box n) = n + 1
{-# NOINLINE f #-}

-- | Expected worker type:
-- $wg :: forall {e}. e -> Int# -> Int#
g :: Ex Int -> Int
g (Ex e n) = e `seq` n + 1
{-# NOINLINE g #-}

-- | Expected worker type:
-- $wh :: Int# -> Int#
h :: GADT a -> Int
h (GADT n) = n + 1
{-# NOINLINE h #-}

-- | Expected worker type:
-- $wi :: forall {e}. e -> Int# -> Int#
i :: ExGADT a -> Int
i (ExGADT e n) = e `seq` n + 1
{-# NOINLINE i #-}

