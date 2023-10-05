{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -ddump-deriv -fprint-explicit-kinds #-}
module T10604_deriving where

import GHC.Generics
import Data.Kind (Type)

data Empty (a :: Bool)
  deriving (Generic, Generic1)
data Proxy (a :: k) = Proxy
  deriving (Functor, Generic, Generic1)
data Wrap (a :: Type -> Type) = Wrap (Proxy a)
  deriving (Generic, Generic1)
data Wrap2 (a :: k -> Type) = Wrap2 (Proxy (Proxy a))
  deriving (Generic, Generic1)
data SumOfProducts a = Prod1 (Proxy a) (Proxy a) | Prod2 (Proxy a) (Proxy a)
  deriving (Generic, Generic1)

data Starify a = Starify1 a | Starify2 Int
  deriving (Generic, Generic1)
