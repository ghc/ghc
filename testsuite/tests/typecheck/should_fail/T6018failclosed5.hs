{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed5 where

-- Id is injective...
type family IdClosed a = result | result -> a where
    IdClosed a = a

-- hiding a type family application behind a type synonym should be rejected
type MaybeSynClosed a = IdClosed a
type family LClosed a = r | r -> a where
 LClosed a = MaybeSynClosed a
