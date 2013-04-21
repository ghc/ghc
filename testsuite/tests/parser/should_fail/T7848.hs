
module T7848 where

data A = (:&&) Int Int | A Int Int

x (+) ((&)@z) ((:&&) a b) (c :&& d) (e `A` f) (A g h) = y
  where infixl 3 `y`
        y _ = (&)
        {-# INLINE (&) #-}
        {-# SPECIALIZE (&) :: a #-}
        (&) = x
