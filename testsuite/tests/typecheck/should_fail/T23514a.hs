{-# LANGUAGE TypeFamilies, DataKinds #-}
module T23514a where

import GHC.Types

type F :: Type -> forall k. Maybe k
type family F x where
  F Int @Type = Just Bool
  F Int       = Just Either
