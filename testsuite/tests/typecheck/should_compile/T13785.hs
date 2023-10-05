{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction #-}
module Bug where

class Monad m => C m where
  c :: (m Char, m Char)

foo :: forall m. C m => m Char
foo = bar >> baz >> bar2
  where
    -- Should not get MR warning
    bar, baz :: m Char
    (bar, baz) = c

    -- Should get MR warning
    (bar2, baz2) = c
