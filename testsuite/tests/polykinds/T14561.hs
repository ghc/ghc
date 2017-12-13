{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module T14561 where

import GHC.Types
import GHC.Prim

badId :: forall (a :: TYPE r). a -> a
badId = unsafeCoerce#
-- Un-saturated application of a levity-polymorphic
-- function that must be eta-expanded

goodId :: forall (a :: Type). a -> a
goodId = unsafeCoerce#
-- But this one is OK
