
{-# LANGUAGE GADTSyntax #-}

module GadtSyntaxFail003 where

data Foo a b where
    C1 :: a -> Int -> c -> Foo b a
    C2 :: a -> Char -> Foo a b
    Cs :: Foo a b
