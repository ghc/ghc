module OneShot1 where

import GHC.Base

-- This oneShot is a lie, and together with unsafePerformIO (in the form of
-- trace) in OneShot2, we can observe the difference.

-- Two modules to ensure that oneShot annotations survive interface files, both
-- in explicits unfoldings (foo) and in unannotated functions (baz)

foo :: Int -> Int -> Int
foo y = oneShot (\x -> x+y)
{-# INLINE foo #-}

bar :: Int -> Int -> Int
bar y = (\x -> y+x)
{-# INLINE bar #-}

baz :: Int -> Int -> Int
baz y = oneShot (\x -> x+y)

