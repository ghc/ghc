{-# LANGUAGE FlexibleContexts #-}

module T4398 where

{-# RULES "suspicious" forall (x :: a) y. f (x :: Ord a => a) y = g x y  #-}

{-# NOINLINE f #-}
f :: a -> a -> Bool
f x y = True

g :: Ord a => a -> a -> Bool
g = (<)

