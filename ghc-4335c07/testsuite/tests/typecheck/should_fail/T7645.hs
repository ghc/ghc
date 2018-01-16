{-# LANGUAGE TypeOperators, KindSignatures #-}
module T7645 where

data (+) a b = P

f :: ((+) a (a :: *), Maybe)
f = undefined

