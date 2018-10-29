{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
module Linear16 where

-- Rebindable do notation

(>>=) :: a ⊸ (a ⊸ b) ⊸ b
(>>=) x f = f x

-- Not sure why `fail` and return are needed in my examples
-- TODO: 'return' shouldn't be neccessary since Trac #15607 was fixed,
-- revisit after merging with master.
fail :: a
fail = fail

return :: a ⊸ a
return x = x

correctDo = do
  x <- ()
  (y,z) <- ((),x)
  () <- y
  () <- z
  ()
