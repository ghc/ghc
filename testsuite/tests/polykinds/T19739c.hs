{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Bug where

import Data.Kind
import Data.Proxy

type T :: (forall (x :: Type) -> x) -> forall (a :: Type) -> Type
newtype T f a = MkT (f (Type -> Type) a)
