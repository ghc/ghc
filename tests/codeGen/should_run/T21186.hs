import Data.Int

f :: Int32 -> String
f (-5) = "hello"
f (-4) = "world"
f (-3) = "figs"
f (-2) = "sparkle"
f (-1) = "ficus"
f (0) = "wombat"
f (1) = "turtle"
{-# NOINLINE f #-}

main = putStrLn (f (-5))
