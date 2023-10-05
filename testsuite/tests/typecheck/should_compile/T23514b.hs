{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
module T23514b where

import GHC.Types

type F :: Type -> forall k. Maybe k
type family F x @k where
  F Int @Type = Just Bool
  F Int       = Just Either
