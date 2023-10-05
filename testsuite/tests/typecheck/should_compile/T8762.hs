{-# LANGUAGE UnboxedTuples #-}
module T8762 where

type Ty a = Int

bug :: Ty a -> (# Ty a, () #)
bug ty = (# ty, () #)

foo = let (# a, b #) = bug undefined
      in ()
