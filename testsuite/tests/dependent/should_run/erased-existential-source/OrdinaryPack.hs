{-# LANGUAGE ExperimentalDependentTypes #-}
module OrdinaryPack where

data Wrapped = Pack Int

normal :: Int
normal = case Pack 23 of Pack value -> value
