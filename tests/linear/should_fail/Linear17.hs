{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
module Linear17 where

-- Rebindable do notation

(>>=) :: a ⊸ (a ⊸ b) ⊸ b
(>>=) x f = f x

-- `fail` is needed due to pattern matching on ();
-- ideally, it shouldn't be there.
fail :: a
fail = fail

incorrectDo1 = do
  x <- ()
  (y,z) <- ((),())
  () <- y
  () <- z
  ()

incorrectDo2 = do
  x <- ()
  (y,z) <- ((),x)
  () <- y
  ()

incorrectDo3 = do
  x <- ()
  (y,z) <- (x,x)
  () <- y
  () <- z
  ()
