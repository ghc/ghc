{-# LANGUAGE MagicHash, BangPatterns #-}
module T5658b where
import GHC.Prim
import GHC.Exts ( isTrue# )

foo :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
foo xs ys m n = go 0# 0#
  where
    go i j = case isTrue# (i >=# m) of
      False -> let !x = indexIntArray# xs i in
        case isTrue# (j >=# n) of
          False -> case isTrue# (x ==# indexIntArray# ys j) of
            False -> False
            True  -> go (i +# 1#) (j +# 1#)
          True -> False
      True -> case isTrue# (j >=# n) of
        False -> let !y = indexIntArray# ys i in False
        True -> True
