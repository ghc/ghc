{-# LANGUAGE LexicalNegation #-}

module LexicalNegation where

x :: Int
x = 42

negx :: Int
negx = f -x  where  f = (- 5)

subx :: Int -> Int
subx = (- x)

assertion1 :: Bool
assertion1 = (- x) -x  ==  x
