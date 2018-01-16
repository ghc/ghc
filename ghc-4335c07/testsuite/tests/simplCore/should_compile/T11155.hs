{-# OPTIONS_GHC -O -fno-full-laziness #-}
module T11155 where

foo :: Bool
{-# NOINLINE foo #-}
foo = error "rk"

bar x = let t :: Char
            t = case foo of { True -> 'v'; False -> 'y' }
        in [t]

