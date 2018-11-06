{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module DerivingViaStandalone where

import Data.Kind (Type)
import Control.Applicative
import Data.Functor.Compose
import Data.Proxy
import Data.Semigroup

newtype App (f :: Type -> Type) a = App (f a)
  deriving newtype
    (Functor, Applicative)

instance (Applicative f, Semigroup a) => Semigroup (App f a) where
  (<>) = liftA2 (<>)

deriving via (App (Compose (f :: Type -> Type) g) a)
         instance (Applicative f, Applicative g, Semigroup a)
               => Semigroup (Compose f g a)

class C (a :: k -> Type)
instance C Proxy

newtype MyProxy a = MyProxy (Proxy a)
deriving via (Proxy :: Type -> Type) instance C MyProxy

class Z a b
data T a
data X1 a
data X2 a
data X3 a

deriving via (forall a. T a) instance           Z a (X1 b)
deriving via           (T a) instance forall b. Z a (X2 b)
deriving via (forall a. T a) instance forall b. Z a (X3 b)
