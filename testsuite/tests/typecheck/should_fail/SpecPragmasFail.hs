
module SpecPragmasFail where

foo :: Num a => a -> a
foo x = x + 1

{-# SPECIALISE foo @Integer :: Int -> Int #-}

{-# SPECIALISE foo @Bool #-}

bar :: a ~ Int => a
bar = 3

{-# SPECIALISE bar @Char #-}
