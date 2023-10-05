{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile (pattern Single) where

pattern Single x <- [x]
