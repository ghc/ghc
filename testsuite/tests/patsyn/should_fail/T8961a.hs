{-# LANGUAGE PatternSynonyms #-}
module T8961a (pattern Single) where

pattern Single x <- [x]
