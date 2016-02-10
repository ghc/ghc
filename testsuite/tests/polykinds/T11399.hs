{-# LANGUAGE FlexibleInstances, TypeInType #-}
module T11399 where

import Data.Kind

newtype UhOh (k :: * -> *) (a :: k *) = UhOh (k *)
instance Functor k => Functor (UhOh k) where
