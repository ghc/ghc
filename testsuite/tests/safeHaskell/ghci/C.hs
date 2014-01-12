{-# LANGUAGE Safe #-}
module C (a, C(), D(..)) where

a :: Int
a = 1

b :: Int
b = 2

data C a = C a Int

data D a = D a Int

