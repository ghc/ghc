{-# LANGUAGE RankNTypes #-}

-- Dead arguments should be dropped in specialisations. See !2913.

module ShouldCompile where

foo :: () -> Show a => a -> String
foo _x y = show y ++ "!"
{-# NOINLINE[0] foo #-}

bar :: String
bar = foo () (42 :: Int)
