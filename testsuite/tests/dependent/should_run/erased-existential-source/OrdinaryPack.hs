{-# LANGUAGE ExperimentalDependentTypes #-}
module OrdinaryPack where

data Wrapped = Pack Int

normal :: Int
normal = case Pack 23 of Pack value -> value

-- Keep an ordinary unfolding whose inlining decision examines its argument.
{-# INLINABLE observeArgument #-}
observeArgument :: a -> Int -> Int
observeArgument x choice = x `seq` case choice of
  0 -> 11
  1 -> 13
  2 -> 17
  3 -> 19
  _ -> choice + 23
