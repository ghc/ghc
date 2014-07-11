{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed1 where

-- Id is injective...
type family IdClosed a = result | result -> a where
    IdClosed a = a

-- ...but despite that we disallow a call to Id
type family IdProxyClosed a = r | r -> a where
    IdProxyClosed a = IdClosed a
