{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances,
    UndecidableInstances, FunctionalDependencies #-}

module Main where

class Container a b | a -> b where
    make :: b -> a

data Cont a = Cont a deriving (Show, Eq)

instance Container (Cont a) a where
    make x = Cont x

instance (Container a b, Show a, Eq a, Num b) => Num a where
    fromInteger x = make (fromInteger x)

d = fromInteger 3 :: Cont Integer

main = print d