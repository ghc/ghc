module T23208_Lib (g) where

err :: Int -> b
err = error "really important message"

sg :: Int -> Int
sg n = err n
{-# NOINLINE sg #-}
g :: a -> a
g x = x
{-# NOINLINE g #-}
{-# RULES "g" g @Int = sg #-}
