module Main where

def1 :: Int
def1 = 3

infixl 9 -->
(-->) :: Int -> Int -> Int
x --> y = x * y + y

interior z =
    let infixr 7 -->
        (-->) :: Float -> Float -> Float
        x --> y = y * x + x
    in 
    z --> 3.0

main = print (def1 --> 1)
