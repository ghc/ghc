{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

module Main where

class (There a ~ b, BackAgain b ~ a) => Foo a b where
     type There a
     type BackAgain b
     there :: a -> b
     back :: b -> a
     tickle :: b -> b

instance Foo Char Int where
     type There Char = Int
     type BackAgain Int = Char
     there = fromEnum
     back = toEnum
     tickle = (+1)

test :: (Foo a b) => a -> a
test = back . tickle . there

main :: IO ()
main = print $ test 'F'
