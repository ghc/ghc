{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction #-}
module Bug where

class Monad x => C x where
  c :: (x Char, x Char)

foo :: forall m. C m => m Char
foo = bar >> baz >> bar1 >> bar2
  where
    -- Should not get MR warning
    bar, baz :: m Char
    (bar, baz) = c

    -- Should not get MR warning
    (bar1, baz1) = c :: (m Char, m Char)

    -- Should get MR warning
    -- Natural type for the "whole binding": forall x. C x => (x Char, x Char)
    -- MR makes it less polymorphic => warning.
    (bar2, baz2) = c
