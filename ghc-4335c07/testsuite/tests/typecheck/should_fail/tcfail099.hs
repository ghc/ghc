{-# LANGUAGE ExistentialQuantification #-}

-- This bogus program slipped past GHC 5.02!

module ShouldFail where

data DS = forall a. C (a -> Int)

call (C f) arg = f arg
