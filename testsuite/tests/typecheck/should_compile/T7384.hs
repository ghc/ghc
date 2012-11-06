{-# LANGUAGE FunctionalDependencies, PolyKinds #-}

module T7384 where

class Baz a b | a -> b where
   bar :: a -> ()

instance Baz Bool Bool where
   bar _ = ()

foo = bar False
