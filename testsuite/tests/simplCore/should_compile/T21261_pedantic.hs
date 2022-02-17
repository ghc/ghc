{-# OPTIONS_GHC -fpedantic-bottoms #-} -- This flag must inhibit eta reduction based on demands

module T21261_pedantic where

-- README: See T21261. These examples absolutely should not eta reduce with
-- -fpedantic-bottoms.

f1 :: (Int -> Int -> Int) -> Int
f1 c = c 1 `seq` c 2 3
{-# NOINLINE f1 #-}
no2 :: (Int -> Int -> Int) -> Int
no2 c = f1 (\x y -> c x y)

f2 :: (Int -> Int -> Int) -> Maybe Int
f2 c = Just (c 1 `seq` c 3 4)
{-# NOINLINE f2 #-}
no2_lazy :: (Int -> Int -> Int) -> Maybe Int
no2_lazy c = f2 (\x y -> c x y)
