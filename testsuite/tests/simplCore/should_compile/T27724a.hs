module T27724a where

import GHC.Exts

class C a where
  op :: a -> Int

instance C Int where
  {-# INLINE op #-}
  op x = x

f :: C a => a -> Int -> Int
f x 0 = op x
f x n = op x + f x (n - 1)

foo :: IO ()
-- foo = print (f (1 :: Int) 10) -- Specialises
foo = print (noinline f (1 :: Int) 10) -- Doesn't specialise
