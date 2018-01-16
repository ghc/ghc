{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Kind

data family Sing (a :: k)

data HR (a :: j) (b :: k) where
  HRefl :: HR a a

data instance Sing (z :: HR a b) where
  SHRefl :: Sing HRefl

foo :: forall (j :: Type) (k :: Type) (a :: j)
              (b :: k) (r :: HR a b)
              (p :: forall (z :: Type) (y :: z). HR a y -> Type).
          Sing r
       -> App p HRefl
       -> HRApp p r
foo SHRefl pHRefl = pHRefl

type App f x = f x
type HRApp (f :: forall (z :: Type) (y :: z). HR a y -> Type)
           (x :: HR a b) = f x
