module ConflictingInlinePragmas0 where

{-# INLINE f #-}
f :: Int -> Int
f x
  = x
{-# NOINLINE f #-}
