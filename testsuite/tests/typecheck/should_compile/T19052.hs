{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Overlap where

import Data.Kind (Type)

class Sub (xs :: [Type]) (ys :: [Type]) where
  subIndex :: Int
instance {-# OVERLAPPING #-} Sub xs xs where
  subIndex = 0
instance (ys ~ (y ': ys'), Sub xs ys') => Sub xs ys where
  subIndex = subIndex @xs @ys' + 1

subIndex1 :: forall (x :: Type) (xs :: [Type]). Int
subIndex1 = subIndex @xs @(x ': xs)
