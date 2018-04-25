{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Char
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast character manipulation functions.
module Data.Text.Internal.Unsafe.Char
    (
      ord
    , unsafeChr
    , unsafeChr8
    , unsafeChr32
    , unsafeWrite
    -- , unsafeWriteRev
    ) where

#ifdef ASSERTS
import Control.Exception (assert)
#endif
import Control.Monad.ST (ST)
import Data.Bits ((.&.))
import Data.Text.Internal.Unsafe.Shift (shiftR)
import GHC.Exts (Char(..), Int(..), chr#, ord#, word2Int#)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import qualified Data.Text.Array as A

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

unsafeChr :: Word16 -> Char
unsafeChr (W16# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr8 #-}

unsafeChr32 :: Word32 -> Char
unsafeChr32 (W32# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr32 #-}

-- | Write a character into the array at the given offset.  Returns
-- the number of 'Word16's written.
unsafeWrite :: A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c
    | n < 0x10000 = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i < A.length marr) $ return ()
#endif
        A.unsafeWrite marr i (fromIntegral n)
        return 1
    | otherwise = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i < A.length marr - 1) $ return ()
#endif
        A.unsafeWrite marr i lo
        A.unsafeWrite marr (i+1) hi
        return 2
    where n = ord c
          m = n - 0x10000
          lo = fromIntegral $ (m `shiftR` 10) + 0xD800
          hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
{-# INLINE unsafeWrite #-}

{-
unsafeWriteRev :: A.MArray s Word16 -> Int -> Char -> ST s Int
unsafeWriteRev marr i c
    | n < 0x10000 = do
        assert (i >= 0) . assert (i < A.length marr) $
          A.unsafeWrite marr i (fromIntegral n)
        return (i-1)
    | otherwise = do
        assert (i >= 1) . assert (i < A.length marr) $
          A.unsafeWrite marr (i-1) lo
        A.unsafeWrite marr i hi
        return (i-2)
    where n = ord c
          m = n - 0x10000
          lo = fromIntegral $ (m `shiftR` 10) + 0xD800
          hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
{-# INLINE unsafeWriteRev #-}
-}
