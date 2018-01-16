{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T11407 where

import Data.Kind

type Const a b = a

data family UhOh (f :: k1) (a :: k2) (b :: k3)
data instance UhOh (f :: * -> * -> *) (a :: x a) (b :: Const * a) = UhOh
