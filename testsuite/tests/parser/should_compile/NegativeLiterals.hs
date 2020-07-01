{-# LANGUAGE NegativeLiterals #-}

module NegativeLiterals where

b :: Bool
b = even -2   -- parsed as:  even (-2)

f :: Integer -> Integer
f x = x-1     -- parsed as:  (-) x 1
