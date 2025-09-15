{-# LANGUAGE TypeFamilies, TypeOperators, RankNTypes #-}

module HO where

import Data.IORef
import Data.Kind

type family SMRef    (m::(Type -> Type)) :: Type -> Type
type family SMMonad  (r::(Type -> Type)) :: Type -> Type

type instance SMRef   IO        = IORef
type instance SMMonad IORef     = IO


class SMMonad (SMRef m) ~ m => SM m where
        new   :: forall a. a -> m (SMRef m a)
        read  :: forall a. (SMRef m a) -> m a
        write :: forall a. (SMRef m a) -> a -> m ()

