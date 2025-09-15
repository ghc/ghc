{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeAbstractions #-}
module T16391a where

import Data.Kind

type Const (a :: Type) (b :: Type) = a
type family F :: Const Type a where
  F = Int
type TS :: forall a . Const Type a
type TS @a = (Int :: Const Type a)
data T1 :: Const Type a where
  MkT1 :: T1
data T2 :: Const Type a -> Type where
  MkT2 :: T2 b
