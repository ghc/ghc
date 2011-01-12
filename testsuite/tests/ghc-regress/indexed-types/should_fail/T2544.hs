{-# LANGUAGE TypeOperators, TypeFamilies #-}

module T2544 where

data (:|:) a b = Inl a | Inr b

class Ix i where
   type IxMap i :: * -> *
   empty  :: IxMap i [Int]

data BiApp a b c = BiApp (a c) (b c)

instance (Ix l, Ix r) => Ix (l :|: r) where
   type IxMap (l :|: r) = BiApp (IxMap l) (IxMap r)
   empty = BiApp empty empty