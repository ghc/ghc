{-# LANGUAGE RankNTypes, InstanceSigs #-}
module T7545 where

class C a where
   f :: a -> b

instance C (a -> b) where
   f :: x
   f = undefined
