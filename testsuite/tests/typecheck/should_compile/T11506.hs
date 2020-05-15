{-# LANGUAGE PolyKinds, ExistentialQuantification, ScopedTypeVariables,
             TypeFamilies, TypeInType #-}

module T11506 where

import Data.Proxy
import Data.Kind

type family ProxyType where ProxyType = (Proxy :: Type -> Type)

data T = forall a. MkT (ProxyType a)

foo (MkT (_ :: Proxy a)) = const True (undefined :: a)
