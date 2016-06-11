{-# LANGUAGE FlexibleInstances, TypeInType #-}
module T11399 where

import Data.Kind

newtype UhOh (k :: * -> *) (a :: k *) = UhOh (k *)

-- UhOh :: forall (k : * -> *). k * -> *

instance Functor a => Functor (UhOh a) where
