{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- !!! Functional dependencies

module Main where

class Foo a b | a -> b where
    foo :: a -> b

instance Foo [a] (Maybe a) where
    foo []    = Nothing
    foo (x:_) = Just x

instance Foo (Maybe a) [a] where
    foo Nothing  = []
    foo (Just x) = [x]

test3:: [a] -> [a]
test3 = foo . foo
-- First foo must use the first instance,
-- second must use the second.  So we should
-- get in effect: 	test3 (x:xs) = [x]

main:: IO ()
main = print (test3 "foo")
