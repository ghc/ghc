{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestPrim
  (
    tpo#
  , tpo
  ) where

import GHC.Base -- Int and I#

foreign import prim "test_prim_op" tpo# :: Int# -> Int# -> Int#

tpo :: Int -> Int -> Int
tpo (I# a) (I# b) = I# (tpo# a b)
