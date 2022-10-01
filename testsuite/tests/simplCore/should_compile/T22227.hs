{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Unboxed (test) where

import GHC.Exts
import GHC.IO

data Step s a = Yield a s | Done

uninitialised = undefined

test :: Int# -> Int# -> Array# Double -> (# Int#, Int#, Array# Double #)
test off n oldArr = runRW# $ \s0 ->
  case newArray# n uninitialised s0
   of { (# s1, newArr #) ->
  let
    step' i
      | isTrue# (i >=# n) = Done
      | otherwise =
        let (# D# x #) = indexArray# oldArr (off +# i) in
        if isTrue# (x >## 10.0##)
        then Yield (D# x) (I# (i +# 1#))
        else step' (i +# 1#)
    loop i j s2 =
      case step' i of
        Yield x (I# s') ->
          case writeArray# newArr j (x + 1) s2
           of { s3 -> 
          loop s' (j +# 1#) s3
        }
        Done ->
          case unsafeFreezeArray# newArr s2
           of { (# s3, out #) ->
          (# 0#, j, out #)
        }
  in
  loop 0# 0# s1
  }
