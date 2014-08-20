{-# LANGUAGE PatternSynonyms #-}
module ShouldFail where

pattern P x y <- x@(Just y)
