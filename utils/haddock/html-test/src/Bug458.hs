{-# LANGUAGE Haskell2010 #-}
module Bug458 where

-- | See the defn of @'⊆'@.
(⊆) :: () -> () -> ()
_ ⊆ _ = ()

