{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main where

class Foo a b | a -> b where
    foo :: a -> Maybe b
    foo x = Nothing

    bar :: a -> b

instance Foo (Maybe a) a where
    bar (Just x) = x


main = do { print (foo (Just 'x'))
	  ; print (bar (Just 'y')) }
