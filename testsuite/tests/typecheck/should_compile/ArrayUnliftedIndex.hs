{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module ArrayUnliftedIndex where

import GHC.Exts

indexHead :: Array# Bool -> Int# -> Bool
indexHead a x = case indexArray# a 0# of
  (# r #) -> r
