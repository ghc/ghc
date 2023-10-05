{-# LANGUAGE MultiWayIf #-}

module T14773a where

foo :: Bool -> Int
foo b = if | b -> 1

bar :: Bool -> Int
bar b = if | b         -> 1
           | otherwise -> 2
