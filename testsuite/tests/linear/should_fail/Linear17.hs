{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
module Linear17 where

-- Rebindable do notation

(>>=) :: a ⊸ (a ⊸ b) ⊸ b
(>>=) x f = f x

-- Not sure why `fail` and return are needed in my examples
fail :: a
fail = error "fail"

return :: a ⊸ a
return x = x

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
