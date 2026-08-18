{-# LANGUAGE QuantifiedConstraints, UndecidableInstances, FlexibleInstances,
             UndecidableSuperClasses, FlexibleContexts, RankNTypes #-}
-- T27627b's class shape, with a method and a recursive instance.
module Main where

class Eq a => TC a
class (forall a. TC (f a)) => UQ f where { uqDummy :: f Int -> Int }

newtype Id a = MkId a
instance Eq (Id a) where _ == _ = True
instance UQ f => TC (f a)
instance UQ Id where uqDummy _ = 7

{-# NOINLINE dead #-}
dead :: TC a => a -> Int -> Int
dead _ n = n + 1

{-# NOINLINE useUQ #-}
useUQ :: UQ f => f Int -> Int -> Int
useUQ x n = dead x n

main :: IO ()
main = print (useUQ (MkId 3 :: Id Int) 41)
