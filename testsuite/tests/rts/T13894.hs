-- Test that isByteArray# returns False for large but not explicitly pinned byte
-- arrays, see #22255

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples  #-}

import Control.Monad
import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    pinned <- IO $ \s0 ->
      case newByteArray# 1000000# s0 of
        (# s1, arr# #) ->
            case isMutableByteArrayPinned# arr# of
              n# -> (# s1, isTrue# n# #)
    when pinned $ putStrLn "BAD"
