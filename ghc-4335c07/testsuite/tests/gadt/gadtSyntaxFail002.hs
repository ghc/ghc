
{-# LANGUAGE GADTSyntax #-}

module GadtSyntaxFail002 where

data Foo a b where
    C1 :: a -> Int -> b -> Foo b a
    C2 :: a -> Char -> Foo a a
    Cs :: Foo a b
