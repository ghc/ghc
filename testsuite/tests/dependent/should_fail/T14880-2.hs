{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Bug where

import Data.Kind
import Data.Proxy

data Foo (x :: Type) :: forall (a :: x). Proxy a -> Type

quux :: forall arg. Proxy (Foo arg) -> ()
quux (_ :: _) = ()
