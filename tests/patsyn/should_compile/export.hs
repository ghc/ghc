{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module ShouldCompile (data Single) where

pattern Single x <- [x]
