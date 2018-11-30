{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module T14887 where

import Data.Kind
import Data.Type.Equality

type family Foo1 (e :: (a :: k) :~: (a :: k)) :: Type where
  Foo1 (e :: a :~: a) = a :~: a

type family Foo2 (k :: Type) (e :: (a :: k) :~: (a :: k)) :: Type where
  Foo2 k (e :: a :~: a) = a :~: a
