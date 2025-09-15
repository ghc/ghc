{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
module T12746A where

pattern Foo :: Int
pattern Foo = 0x00000001
