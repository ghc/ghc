{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
module Bug where

import Data.Kind
import Data.Proxy

data Foo (x :: Type) :: forall (a :: x). Proxy a -> Type

data Bar :: Type -> Type where
    MkBar :: forall x arg.
             -- Commenting out the line below makes the issue go away
             Foo arg ~ Foo arg =>
             Bar x
