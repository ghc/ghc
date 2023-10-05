module T21851_2a where

f :: (Num a, Show b) => a -> b -> String
{-# NOINLINE f #-}
f x y = "no"
{-# RULES "wombat"  f = wombat #-}

wombat :: Show b => Int -> b -> String
{-# INLINEABLE wombat #-}
wombat 0 y = ""
wombat n y = show y ++ wombat (n-1) y
