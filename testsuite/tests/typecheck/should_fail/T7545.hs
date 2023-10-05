{-# LANGUAGE RankNTypes, InstanceSigs #-}
module T7545 where

class C a where
   f :: a -> b

-- This is now accepted because the instance signature is more general
instance C (a -> b) where
   f :: x
   f = undefined
