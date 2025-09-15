{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}

module T20858 where

import Data.Kind
  ( Type )
import GHC.Exts
  ( Double#, Int#, Word# )

type GetFunKind :: k -> Type
type family GetFunKind x where
  forall arg_k res_k (a :: arg_k -> res_k) (b :: arg_k). GetFunKind (a b) = arg_k -> res_k

type GetFun :: forall res_k. forall (x :: res_k) -> GetFunKind x
type family GetFun x where
  GetFun (a b) = a

type S1 = GetFun (# Int# | Double# | Word# #)
type S2 = GetFun S1
type S3 = GetFun S2
