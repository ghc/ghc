{-# LANGUAGE ImplicitParams #-}
module T7525 where

hmm :: Bool
hmm = let ?a = True in ?a && ?b

{- The error message should say

    Could not deduce (?b::Bool)
      arising from a use of implicit parameter `?b'
    from the context (?a::Bool)

That is, the context should be (?a :: Bool) not (?a :: t0).
-}

