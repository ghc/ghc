{-# LANGUAGE BangPatterns, TypeApplications, UnboxedTuples, MagicHash,
             UnboxedSums, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unbanged-strict-patterns #-}

module Main where

import Control.Exception
import GHC.Exts

-- This stress-tests the semantics of strict patterns.

ok :: a -> IO ()
ok x = do
  evaluate x
  putStrLn "Evaluation successful."

bad :: a -> IO ()
bad x = do
  r <- try @SomeExceptionWithLocation $ evaluate x
  case r of
    Left _  -> putStrLn "Exception thrown as expected."
    Right _ -> putStrLn "Exception not thrown when expected."

-- OK
a = True
  where x :: Num a => a
        !x = undefined   -- x is a function. Should be OK.

-- should fail
b = True
  where x :: a
        !x = undefined

-- OK
c = True
  where I# _ = undefined

-- bad
d = True
  where I# _x = undefined

-- OK
e = True
  where _ = undefined :: Int#

-- bad
f = True
  where _x = undefined :: Int#

-- OK
g = True
  where (# _ #) = undefined

-- OK
h = True
  where (# _x #) = undefined

-- bad
i = True
  where (# _x #) = undefined :: (# Int# #)

-- bad
j = True
  where !True = False

-- OK
k = True
  where True = False

-- OK
l = True
  where 3# = 4#

-- bad
m = True
  where !3# = 4#

-- bad
n = True
  where _x = undefined :: (# () #)

-- OK
o = True
  where (# _x #) = undefined :: (# () #)

-- OK
p = True
  where (# _ | #) = (# | True #)

-- bad
q = True
  where (# _x | #) = (# | True #) :: (# Int# | Bool #)

-- OK
r = True
  where (# _x | #) = (# | True #)

-- bad
s = True
  where !(# x #) = undefined

main :: IO ()
main = do
  ok a
  bad b
  ok c
  bad d
  ok e
  bad f
  ok g
  ok h
  bad i
  bad j
  ok k
  ok l
  bad m
  bad n
  ok o
  ok p
  bad q
  ok r
  bad s
