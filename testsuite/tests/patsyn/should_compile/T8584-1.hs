{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern Single :: a -> [a]
pattern Single x = [x]
