{-# LANGUAGE MagicHash #-}

module T22781 where

import GHC.Exts

bar = I# (go 0# 1#)
  where
    -- SpecConstr should generate a specialization for the call
    --    (go 0# 1#) = $sgo
    -- looking like
    --    $sgo = / void -> 1#
    go :: Int# -> Int# -> Int#
    go 0# 1# = 1#
    go _  0# = 3#
    go n x = go n (x -# 1# )
