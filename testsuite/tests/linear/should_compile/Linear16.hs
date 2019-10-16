{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
module Linear16 where

-- Rebindable do notation

(>>=) :: a ⊸ (a ⊸ b) ⊸ b
(>>=) x f = f x

-- `fail` is needed due to pattern matching on ();
-- ideally, it shouldn't be there.
fail :: a
fail = fail

correctDo = do
  x <- ()
  (y,z) <- ((),x)
  () <- y
  () <- z
  ()
