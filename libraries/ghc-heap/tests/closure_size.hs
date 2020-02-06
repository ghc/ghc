{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Exts
import GHC.IO
import ClosureSizeUtils

data A = A (Array# Int)
data MA = MA (MutableArray# RealWorld Int)
data BA = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)
data APC a = APC a


main :: IO ()
main = do
  assertSize 'a' 2
  assertSize (Just ()) 2
  assertSize (Nothing :: Maybe ()) 2
  assertSize ((1,2) :: (Int,Int)) 3
  assertSize ((1,2,3) :: (Int,Int,Int)) 4

  MA ma <- IO $ \s ->
      case newArray# 0# 0 s of
          (# s1, x #) -> (# s1, MA x #)

  A a <- IO $ \s ->
      case freezeArray# ma 0# 0# s of
          (# s1, x #) -> (# s1, A x #)

  MBA mba <- IO $ \s ->
      case newByteArray# 0# s of
          (# s1, x #) -> (# s1, MBA x #)

  BA ba <- IO $ \s ->
      case newByteArray# 0# s of
          (# s1, x #) ->
              case unsafeFreezeByteArray# x s1 of
                  (# s2, y #) -> (# s2, BA y #)

  assertSizeUnlifted ma 3
  assertSizeUnlifted a 3
  assertSizeUnlifted mba 2
  assertSizeUnlifted ba 2
