{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

import Data.Kind

type Const :: a -> b -> a
type family Const x y where
  Const x _ = x

type F :: (forall (b :: Bool) -> Const Type b) -> Type
data F f

type G :: forall (b :: Bool) -> Type
data G b

type H :: Type
type family H where
  H = F G
