{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module T25882 where

import Data.Kind
import Data.Proxy
import Data.Typeable
import GHC.Exts

-- Check named default declarations for poly-kinded classes

-- Typeable :: forall k. k -> Constraint
default Typeable (Bool)

-- A tricky case involving type families in kinds
type F :: Type -> Type -> Type
type family F a b = r | r -> a b where
  F Bool Char = TYPE IntRep

type MyCls :: forall a b. F a b -> Constraint
class MyCls f where
  meth :: Proxy f -> Float

instance MyCls Int# where
  meth _ = 98.91

default MyCls(Int#)
