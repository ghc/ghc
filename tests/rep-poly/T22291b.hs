{-# LANGUAGE DataKinds, MagicHash, UnboxedTuples #-}

module T22291b where

import GHC.Exts

indexArray :: forall l (a :: TYPE (BoxedRep l)). Array# a -> Int# -> (# a #)
indexArray = indexArray#
