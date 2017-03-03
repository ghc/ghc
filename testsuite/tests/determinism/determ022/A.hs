-- | Test that SPEC rules are named deterministically.
module A where

test1 :: Num a => a -> a -> a
test1 x y = x + 42 * y
{-# SPECIALISE test1 :: Int -> Int -> Int #-}
