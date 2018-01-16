{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Bug where

class C a b | a -> b, b -> a where
    f :: a -> b

instance C Int Bool where
    f = undefined
instance (C a c, C b d) => C (a -> b) (c -> d) where
    f = undefined

foo :: Int -> Int
foo = undefined

bar = f foo
