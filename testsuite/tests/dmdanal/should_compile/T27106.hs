module T27106 where

{-# NOINLINE weird #-}
weird :: Int -> a
weird x = weird x
