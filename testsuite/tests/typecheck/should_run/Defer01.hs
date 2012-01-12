-- Test -fdefer-type-errors
-- Should compile and run

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

t5624 :: IO ()
t5624 = putStr "Hello World" >> putStr ','

a :: Int
a = 'p'

data B = B
b :: B -> Bool
b x = x == x

data C a where
  C1 :: C Int
  C2 :: Bool -> C Bool

c :: C Int -> Bool
c (C2 x) = True

d :: a -> a
d = 1

e = 'p'
f = e 'q'

h :: a -> (Char,Char) 
h x = (x,'c')

data T a where 
  K  :: a -> T a

i a = seq (not (K a)) ()

class MyClass a where myOp :: a -> String

j = myOp 23 -- Two errors, should not combine them

k :: (Int ~ Bool) => Int -> Bool
k x = x

l :: IO ()
l = putChar >> putChar 'p'


main :: IO ()
main = print "No errors!"
