{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall #-}
module Bug (go_fast_end) where

import Control.Monad.ST (ST)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString (ByteString)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.Exts ( Int(..), Int#, Ptr(..), Word(..)
                , (<#), (>#), indexWord64OffAddr#, isTrue#, orI#
                )
import GHC.Word (Word8(..), Word64(..))
import System.IO.Unsafe (unsafeDupablePerformIO)

import GHC.Exts (word8ToWord#)

import GHC.Exts (byteSwap64#, int64ToInt#, word64ToInt64#, ltWord64#, wordToWord64#)

go_fast_end :: ByteString -> DecodeAction s a -> ST s (SlowPath s a)
go_fast_end !bs (ConsumeInt32 k) =
  case tryConsumeInt (BS.unsafeHead bs) bs of
    DecodeFailure           -> return $! SlowFail bs "expected int32"
    DecodedToken sz (I# n#) ->
      case (n# ># 0x7fffffff#) `orI#` (n# <# -0x80000000#) of
        0#                  -> k n# >>= go_fast_end (BS.unsafeDrop sz bs)
        _                   -> return $! SlowFail bs "expected int32"

data SlowPath s a = SlowFail {-# UNPACK #-} !ByteString String

data DecodeAction s a = ConsumeInt32 (Int# -> ST s (DecodeAction s a))

data DecodedToken a = DecodedToken !Int !a | DecodeFailure

tryConsumeInt :: Word8 -> ByteString -> DecodedToken Int
tryConsumeInt hdr !bs = case word8ToWord hdr of
  0x17 -> DecodedToken 1 23
  0x1b -> case word64ToInt (eatTailWord64 bs) of
            Just n  -> DecodedToken 9 n
            Nothing -> DecodeFailure
  _    -> DecodeFailure
{-# INLINE tryConsumeInt #-}

eatTailWord64 :: ByteString -> Word64
eatTailWord64 xs = withBsPtr grabWord64 (BS.unsafeTail xs)
{-# INLINE eatTailWord64 #-}

word64ToInt :: Word64 -> Maybe Int
word64ToInt (W64# w#) =
  case isTrue# (w# `ltWord64#` wordToWord64# 0x80000000##) of
    True  -> Just (I# (int64ToInt# (word64ToInt64# w#)))
    False -> Nothing
{-# INLINE word64ToInt #-}

withBsPtr :: (Ptr b -> a) -> ByteString -> a
withBsPtr f (BS.PS x off _) =
    unsafeDupablePerformIO $ withForeignPtr x $
        \(Ptr addr#) -> return $! (f (Ptr addr# `plusPtr` off))
{-# INLINE withBsPtr #-}

grabWord64 :: Ptr () -> Word64
grabWord64 (Ptr ip#) = W64# (byteSwap64# (indexWord64OffAddr# ip# 0#))
{-# INLINE grabWord64 #-}

word8ToWord :: Word8 -> Word
word8ToWord (W8# w#) = W# (word8ToWord# w#)
{-# INLINE word8ToWord #-}
