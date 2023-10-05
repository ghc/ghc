module T21763a where

{-# NOINLINE g_imp #-}
g_imp !x = not x

f3 :: (Bool -> Bool) -> Bool -> [Bool] -> (Bool, [Bool])
-- We want to specialize for `g` to turn it into a known call.
f3 g x []     = (g x, [])
f3 g x (_:ys) = f3 g x ys

foo3 :: [Bool] -> (Bool, [Bool])
foo3 ys = f3 g_imp True ys
