{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- A bound variable must have a fixed runtime representation.
module QDoFail006 where

import GHC.Exts
import qualified GHC.Internal.Unboxed.Do as Ubx
import GHC.Internal.Unboxed.Do (ST#)

bad :: forall s r (a :: TYPE r). ST# s a -> ST# s a
bad m = Ubx.do
  x <- m
  Ubx.pure x
