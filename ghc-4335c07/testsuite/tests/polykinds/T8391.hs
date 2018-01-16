{-# LANGUAGE FunctionalDependencies, FlexibleInstances, 
             TypeFamilies,
             PolyKinds #-}

module T8391 where

type  Foo a = a

class Bar a b | a -> b

instance Bar a (Foo a)
