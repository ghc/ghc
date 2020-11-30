{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ForeignPtr.Ops
-- Copyright   :  (c) The University of Glasgow, 1992-2003
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC's implementation of the 'ForeignPtr' data type.
--
-----------------------------------------------------------------------------

module GHC.ForeignPtr.Ops
  ( -- * Reading
    peekWord8ForeignPtr
  , peekWord16ForeignPtr
  , peekWord32ForeignPtr
  , peekWord64ForeignPtr
  , peekWordForeignPtr
  , peekInt8ForeignPtr
  , peekInt16ForeignPtr
  , peekInt32ForeignPtr
  , peekInt64ForeignPtr
  , peekIntForeignPtr
  , peekCharForeignPtr
    -- * Writing
  , pokeWord8ForeignPtr
  , pokeWord16ForeignPtr
  , pokeWord32ForeignPtr
  , pokeWord64ForeignPtr
  , pokeWordForeignPtr
  , pokeInt8ForeignPtr
  , pokeInt16ForeignPtr
  , pokeInt32ForeignPtr
  , pokeInt64ForeignPtr
  , pokeIntForeignPtr
  , pokeCharForeignPtr
  ) where

import GHC.Word
import GHC.Int
import GHC.Base
import GHC.ForeignPtr
import GHC.Ptr

withFP :: ForeignPtr a
       -> (Addr# -> State# RealWorld -> (# State# RealWorld, b #))
       -> IO b
withFP fp f =
  withForeignPtr fp (\(Ptr addr) -> IO (f addr))

peekWord8ForeignPtr :: ForeignPtr ty -> Int -> IO Word8
peekWord8ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readWord8OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, W8# (narrowWord8# r) #)

peekWord16ForeignPtr :: ForeignPtr ty -> Int -> IO Word16
peekWord16ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readWord16OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, W16# (narrowWord16# r) #)

peekWord32ForeignPtr :: ForeignPtr ty -> Int -> IO Word32
peekWord32ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readWord32OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, W32# (narrowWord32# r) #)

peekWord64ForeignPtr :: ForeignPtr ty -> Int -> IO Word64
peekWord64ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readWord64OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, W64# r #)

peekWordForeignPtr :: ForeignPtr ty -> Int -> IO Word
peekWordForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readWordOffAddr# addr d s0 of
      (# s1, r #) -> (# s1, W# r #)

peekInt8ForeignPtr :: ForeignPtr ty -> Int -> IO Int8
peekInt8ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readInt8OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, I8# (narrowInt8# r) #)

peekInt16ForeignPtr :: ForeignPtr ty -> Int -> IO Int16
peekInt16ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readInt16OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, I16# (narrowInt16# r) #)

peekInt32ForeignPtr :: ForeignPtr ty -> Int -> IO Int32
peekInt32ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readInt32OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, I32# (narrowInt32# r) #)

peekInt64ForeignPtr :: ForeignPtr ty -> Int -> IO Int64
peekInt64ForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readInt64OffAddr# addr d s0 of
      (# s1, r #) -> (# s1, I64# r #)

peekIntForeignPtr :: ForeignPtr ty -> Int -> IO Int
peekIntForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readIntOffAddr# addr d s0 of
      (# s1, r #) -> (# s1, I# r #)

peekCharForeignPtr :: ForeignPtr ty -> Int -> IO Char
peekCharForeignPtr fp (I# d) = withFP fp $ \addr s0 ->
    case readCharOffAddr# addr d s0 of
      (# s1, r #) -> (# s1, C# r #)

pokeWord8ForeignPtr :: ForeignPtr ty -> Int -> Word8 -> IO ()
pokeWord8ForeignPtr fp (I# d) (W8# n) = withFP fp $ \addr s0 ->
    case writeWord8OffAddr# addr d (extendWord8# n) s0 of
      s1 -> (# s1, () #)

pokeWord16ForeignPtr :: ForeignPtr ty -> Int -> Word16 -> IO ()
pokeWord16ForeignPtr fp (I# d) (W16# n) = withFP fp $ \addr s0 ->
    case writeWord16OffAddr# addr d (extendWord16# n) s0 of
      s1 -> (# s1, () #)

pokeWord32ForeignPtr :: ForeignPtr ty -> Int -> Word32 -> IO ()
pokeWord32ForeignPtr fp (I# d) (W32# n) = withFP fp $ \addr s0 ->
    case writeWord32OffAddr# addr d (extendWord32# n) s0 of
      s1 -> (# s1, () #)

pokeWord64ForeignPtr :: ForeignPtr ty -> Int -> Word64 -> IO ()
pokeWord64ForeignPtr fp (I# d) (W64# n) = withFP fp $ \addr s0 ->
    case writeWord64OffAddr# addr d n s0 of
      s1 -> (# s1, () #)

pokeWordForeignPtr :: ForeignPtr ty -> Int -> Word -> IO ()
pokeWordForeignPtr fp (I# d) (W# n) = withFP fp $ \addr s0 ->
    case writeWordOffAddr# addr d n s0 of
      s1 -> (# s1, () #)

pokeInt8ForeignPtr :: ForeignPtr ty -> Int -> Int8 -> IO ()
pokeInt8ForeignPtr fp (I# d) (I8# n) = withFP fp $ \addr s0 ->
    case writeInt8OffAddr# addr d (extendInt8# n) s0 of
      s1 -> (# s1, () #)

pokeInt16ForeignPtr :: ForeignPtr ty -> Int -> Int16 -> IO ()
pokeInt16ForeignPtr fp (I# d) (I16# n) = withFP fp $ \addr s0 ->
    case writeInt16OffAddr# addr d (extendInt16# n) s0 of
      s1 -> (# s1, () #)

pokeInt32ForeignPtr :: ForeignPtr ty -> Int -> Int32 -> IO ()
pokeInt32ForeignPtr fp (I# d) (I32# n) = withFP fp $ \addr s0 ->
    case writeInt32OffAddr# addr d (extendInt32# n) s0 of
      s1 -> (# s1, () #)

pokeInt64ForeignPtr :: ForeignPtr ty -> Int -> Int64 -> IO ()
pokeInt64ForeignPtr fp (I# d) (I64# n) = withFP fp $ \addr s0 ->
    case writeInt64OffAddr# addr d n s0 of
      s1 -> (# s1, () #)

pokeIntForeignPtr :: ForeignPtr ty -> Int -> Int -> IO ()
pokeIntForeignPtr fp (I# d) (I# n) = withFP fp $ \addr s0 ->
    case writeIntOffAddr# addr d n s0 of
      s1 -> (# s1, () #)

pokeCharForeignPtr :: ForeignPtr ty -> Int -> Char -> IO ()
pokeCharForeignPtr fp (I# d) (C# n) = withFP fp $ \addr s0 ->
    case writeCharOffAddr# addr d n s0 of
      s1 -> (# s1, () #)

