
{-# LANGUAGE GADTSyntax #-}

module GadtSyntaxFail001 where

data Foo a b where
    C1 :: a -> Int -> b -> Foo b a
    C2 :: a -> Char -> Foo a Int
    Cs :: Foo a b
