{-# LANGUAGE PatternSynonyms #-}

module Foo where

pattern P :: [a] -> [a]
pattern P x <- x@(y:_)
