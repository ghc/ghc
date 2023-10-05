{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- Test the function of the isPinnedByteArray# primop

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    -- Unpinned MutableByteArray
    r <- IO $ \s0 ->
      case newByteArray# 1024# s0 of
        (# s1, mba #) ->
           (# s1, isTrue# (isMutableByteArrayPinned# mba) #)
    print r

    -- Pinned MutableByteArray
    r <- IO $ \s0 ->
      case newPinnedByteArray# 1024# s0 of
        (# s1, mba #) ->
           (# s1, isTrue# (isMutableByteArrayPinned# mba) #)
    print r

    -- Pinned, Aligned MutableByteArray
    r <- IO $ \s0 ->
      case newAlignedPinnedByteArray# 1024# 16# s0 of
        (# s1, mba #) ->
           (# s1, isTrue# (isMutableByteArrayPinned# mba) #)
    print r

    -- Unpinned ByteArray
    r <- IO $ \s0 ->
      case newByteArray# 1024# s0 of
        (# s1, mba #) ->
          case unsafeFreezeByteArray# mba s1 of
             (# s2, ba #) ->
                (# s2, isTrue# (isByteArrayPinned# ba) #)
    print r

    -- Pinned ByteArray
    r <- IO $ \s0 ->
      case newPinnedByteArray# 1024# s0 of
        (# s1, mba #) ->
          case unsafeFreezeByteArray# mba s1 of
             (# s2, ba #) ->
                (# s2, isTrue# (isByteArrayPinned# ba) #)
    print r
