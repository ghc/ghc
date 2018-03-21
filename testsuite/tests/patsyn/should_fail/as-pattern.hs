{-# LANGUAGE PatternSynonyms #-}
module ShouldFail where

-- This is now ok (following GHC proposal #94)
pattern P x y <- x@(Just y)
