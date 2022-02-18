module T21261 where

-- README: The convention here is that bindings starting with 'yes' should be
-- eta-reduced and become trivial, while bindings starting with 'no' should not
-- be eta-reduced.

f1 :: (Int -> Int -> Int) -> Int
f1 c = c 1 2 + c 3 4
{-# NOINLINE f1 #-}
yes1 :: (Int -> Int -> Int) -> Int
yes1 c = f1 (\x -> c x)

f2 :: (Int -> Int -> Int) -> Int
f2 c = c 1 `seq` c 3 4
{-# NOINLINE f2 #-}
yes1or2 :: (Int -> Int -> Int) -> Int
yes1or2 c = f2 c

f3 :: (Int -> Int -> Int) -> Int
f3 c = c 1 2 + c 3 4
{-# NOINLINE f3 #-}
yes2 :: (Int -> Int -> Int) -> Int
yes2 c = f3 (\x y -> c x y)

f4 :: (Int -> Int -> Int -> Int) -> Int
f4 c = c 1 2 `seq` c 3 4 `seq` 42
{-# NOINLINE f4 #-}
no3 :: (Int -> Int -> Int -> Int) -> Int
no3 c = f4 (\x y z -> c x y z)

f5 :: (Int -> Int -> Int) -> Maybe Int
f5 c = Just (c 1 2 + c 3 4)
{-# NOINLINE f5 #-}
yes2_lazy :: (Int -> Int -> Int) -> Maybe Int
yes2_lazy c = f5 (\x y -> c x y)

f6 :: (Int -> Int -> Int) -> Maybe Int
f6 c = Just (c 1 `seq` c 3 4)
{-# NOINLINE f6 #-}
no2_lazy :: (Int -> Int -> Int) -> Maybe Int
no2_lazy c = f6 (\x y -> c x y)

f7 :: (Int -> Int -> Int) -> Int
f7 c = c 1 `seq` c 2 3
{-# NOINLINE f7 #-}
not_quite_eta :: (Int -> Int -> Int) -> Int
not_quite_eta c = f7 (\x y -> c x y)
