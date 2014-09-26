{-# LANGUAGE
     TypeFamilies
   , KindSignatures
   #-}

module T9433 where

type family Id x :: *
type instance Id a = a

type family Map (f :: * -> *) x :: *
type instance Map f [a] = [f a]

x :: Map Id [Bool]
x = []
