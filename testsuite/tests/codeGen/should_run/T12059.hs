{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- Test the function of the isPinnedByteArray# primop

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    r <- IO $ \s0 ->
      case newByteArray# 1024# s0 of
        (# s1, mba #) ->
            (# s1, isTrue# (isPinnedByteArray# mba) #)
    print r

    r <- IO $ \s0 ->
      case newPinnedByteArray# 1024# s0 of
        (# s1, mba #) ->
            (# s1, isTrue# (isPinnedByteArray# mba) #)
    print r

    r <- IO $ \s0 ->
      case newAlignedPinnedByteArray# 1024# 16# s0 of
        (# s1, mba #) ->
            (# s1, isTrue# (isPinnedByteArray# mba) #)
    print r
