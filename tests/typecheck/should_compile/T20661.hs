{-# LANGUAGE FunctionalDependencies #-}

module T20661 where

class C a b | a -> b
