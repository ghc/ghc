-- Case for general occurrences of selectors
{-# LANGUAGE PatternSynonyms #-}
module DsIncompleteRecSel1 where

data T = T1 { x :: Bool } | T2

f :: T -> Bool
f = x

f2 :: T -> Bool
f2 T2 = True
f2 a = d
 where
  d = x a

f3 :: T -> Bool
f3 T2 = False
f3 a = x (let b = a in b)


pattern N{n} = (n :: Int)

nat :: Int -> Int
nat 0   = 1
nat a@N{} = n a
