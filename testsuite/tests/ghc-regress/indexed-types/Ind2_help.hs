{-# OPTIONS -findexed-types #-}

module Ind2_help where

class C a where
  data T a :: *
  unT :: T a -> a
  mkT :: a -> T a

instance (C a, C b) => C (a,b) where
  data T (a,b) = TProd (T a) (T b)
  unT (TProd x y) = (unT x, unT y)
  mkT (x,y)       = TProd (mkT x) (mkT y)

