module SpecNoinlineLib where

{-# SPECIALIZABLE target #-}
{-# NOINLINE target #-}
target :: Integral i => i -> i
target n = n + 2

