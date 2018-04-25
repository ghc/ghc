{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern Id x = x

Id x = True
