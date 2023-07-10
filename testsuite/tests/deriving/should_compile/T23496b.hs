{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T23496b where

import Data.Kind

class C a where
  type T a :: Type

instance C Int where
  type T Int = Bool

newtype N = MkN Int
deriving newtype instance C N

type F :: forall a. T a -> Type
type family F a where
  F @Int True  = Float
  F @N   False = Double
