{-# LANGUAGE TypeFamilies, PolyKinds, MultiParamTypeClasses,
             FlexibleInstances, DataKinds #-}

module T7938 where

data KProxy (a :: *) = KP

class Foo (a :: k1) (b :: k2) where
  type Bar a

instance Foo (a :: k1) (b :: k2) where
  type Bar a = (KP :: KProxy k2)