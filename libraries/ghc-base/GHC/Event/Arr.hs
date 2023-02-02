{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

module GHC.Event.Arr
    (
      Arr(..)
    , new
    , size
    , read
    , write
    ) where

import GHC.Base (($))
import GHC.Prim (MutableArray#, RealWorld, newArray#, readArray#,
                 sizeofMutableArray#, writeArray#)
import GHC.Types (IO(..), Int(..))

data Arr a = Arr (MutableArray# RealWorld a)

new :: a -> Int -> IO (Arr a)
new defval (I# n#) = IO $ \s0# ->
  case newArray# n# defval s0# of (# s1#, marr# #) -> (# s1#, Arr marr# #)

size :: Arr a -> Int
size (Arr a) = I# (sizeofMutableArray# a)

read :: Arr a -> Int -> IO a
read (Arr a) (I# n#) = IO $ \s0# ->
  case readArray# a n# s0# of (# s1#, val #) -> (# s1#, val #)

write :: Arr a -> Int -> a -> IO ()
write (Arr a) (I# n#) val = IO $ \s0# ->
  case writeArray# a n# val s0# of s1# -> (# s1#, () #)
