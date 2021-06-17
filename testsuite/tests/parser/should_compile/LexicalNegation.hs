{-# LANGUAGE LexicalNegation #-}

module LexicalNegation where

import Data.Ratio

x :: Int
x = 42

negx :: Int
negx = f -x  where  f = (- 5)

subx :: Int -> Int
subx = (- x)

assertion1 :: Bool
assertion1 = (- x) -x  ==  -(2*x)

bug19838 :: Rational
bug19838 = a % -b  where  a = 4; b = 6

infixr 6 +!   -- NB: (non-lexical) negation is infixl 6
(+!) = (+)

rfix :: Int
rfix = -x +! 2
