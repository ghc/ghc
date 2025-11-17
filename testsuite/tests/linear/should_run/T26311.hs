{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts ( Int# )

expensive :: Int -> Int#
expensive 0 = 2#
expensive i = expensive (i-1)

data D = MkD Int# Int

f :: a -> Bool
f _ = False
{-# NOINLINE f #-}

{-# RULES "f/MkD" forall x. f (MkD x) = True #-}

bar :: Bool
bar = f (MkD (expensive 10))

main :: IO ()
main = print bar
