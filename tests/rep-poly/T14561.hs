{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module T14561 where

import GHC.Types
import Unsafe.Coerce

goodId :: forall (a :: Type). a -> a
goodId = unsafeCoerce#
-- This one is OK

badId :: forall r (a :: TYPE r). a -> a
badId = unsafeCoerce#
-- Un-saturated application of a representation-polymorphic
-- function that must be eta-expanded
