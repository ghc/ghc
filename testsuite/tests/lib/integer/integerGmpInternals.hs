{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main (main) where

import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals

gcdExtInteger' :: Integer -> Integer -> (Integer, Integer)
gcdExtInteger' a b = case gcdExtInteger a b of (# a, b #) -> (a,b)

powInteger' :: Integer -> Word -> Integer
powInteger' b (W# w#) = powInteger b w#

{- Reference implementation for 'powModInteger'

powModIntegerHs :: Integer -> Integer -> Integer -> Integer
powModIntegerHs b0 e0 m
  | e0 >= 0    = go b0 e0 1
  | otherwise  = error "non-neg exponent required"
  where
    go !b e !r
      | odd e     = go b' e' (r*b `mod` m)
      | e == 0    = r
      | otherwise = go b' e' r
      where
        b' = b*b `mod` m
        e' = e   `unsafeShiftR` 1 -- slightly faster than "e `div` 2"

-}

main :: IO ()
main = do
    print $ powModInteger b e m
    print $ gcdExtInteger' b e
    print $ gcdExtInteger' e b
    print $ gcdExtInteger' x y
    print $ gcdExtInteger' y x
    print $ powInteger' 12345 0
    print $ powInteger' 12345 1
    print $ powInteger' 12345 30
    print $ [ (x,i) | x <- [0..71], let i = recipModInteger x (2*3*11*11*17*17), i /= 0 ]
    return ()
  where
    b = 2988348162058574136915891421498819466320163312926952423791023078876139
    e = 2351399303373464486466122544523690094744975233415544072992656881240319
    m = 10^(40::Int)

    x = 5328841272400314897981163497728751426
    y = 32052182750761975518649228050096851724
