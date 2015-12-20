{-# LANGUAGE PatternSynonyms #-}
module T9793 where

pattern P :: [a] -> [a]
pattern P x <- x@(_:_)
