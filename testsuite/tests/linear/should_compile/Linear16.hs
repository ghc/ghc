{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
module Linear16 where

-- Rebindable do notation

(>>=) :: a ⊸ (a ⊸ b) ⊸ b
(>>=) x f = f x

-- Not sure why `fail` and return are needed in my examples
fail :: a
fail = error "fail"

return :: a ⊸ a
return x = x

correctDo = do
  x <- ()
  (y,z) <- ((),x)
  () <- y
  () <- z
  ()
