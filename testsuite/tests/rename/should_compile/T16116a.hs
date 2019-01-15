{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T16616a where

class C a where
  type T a b

instance C (Maybe a) where
  type forall b. T (Maybe a) b = Either a b
