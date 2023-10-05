{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module T15450 where

f :: (Int ~ Bool) => Bool -> a
f x = case x of {}

g :: (Int ~ Bool) => Bool -> a
g x = case x of True -> undefined
