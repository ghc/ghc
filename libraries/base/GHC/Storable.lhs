\begin{code}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Storable
-- Copyright   :  (c) The FFI task force, 2000-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Helper functions for "Foreign.Storable"
--
-----------------------------------------------------------------------------

module GHC.Storable
        ( readWideCharOffPtr  
        , readIntOffPtr       
        , readWordOffPtr      
        , readPtrOffPtr       
        , readFunPtrOffPtr    
        , readFloatOffPtr     
        , readDoubleOffPtr    
        , readStablePtrOffPtr 
        , readInt8OffPtr      
        , readInt16OffPtr     
        , readInt32OffPtr     
        , readInt64OffPtr     
        , readWord8OffPtr     
        , readWord16OffPtr    
        , readWord32OffPtr    
        , readWord64OffPtr    
        , writeWideCharOffPtr 
        , writeIntOffPtr      
        , writeWordOffPtr     
        , writePtrOffPtr      
        , writeFunPtrOffPtr   
        , writeFloatOffPtr    
        , writeDoubleOffPtr   
        , writeStablePtrOffPtr
        , writeInt8OffPtr     
        , writeInt16OffPtr    
        , writeInt32OffPtr    
        , writeInt64OffPtr    
        , writeWord8OffPtr    
        , writeWord16OffPtr   
        , writeWord32OffPtr   
        , writeWord64OffPtr   
        ) where

import GHC.Stable ( StablePtr(..) )
import GHC.Int
import GHC.Word
import GHC.Ptr
import GHC.Base
\end{code}

\begin{code}

readWideCharOffPtr  :: Ptr Char          -> Int -> IO Char
readIntOffPtr       :: Ptr Int           -> Int -> IO Int
readWordOffPtr      :: Ptr Word          -> Int -> IO Word
readPtrOffPtr       :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
readFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> IO (FunPtr a)
readFloatOffPtr     :: Ptr Float         -> Int -> IO Float
readDoubleOffPtr    :: Ptr Double        -> Int -> IO Double
readStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
readInt8OffPtr      :: Ptr Int8          -> Int -> IO Int8
readInt16OffPtr     :: Ptr Int16         -> Int -> IO Int16
readInt32OffPtr     :: Ptr Int32         -> Int -> IO Int32
readInt64OffPtr     :: Ptr Int64         -> Int -> IO Int64
readWord8OffPtr     :: Ptr Word8         -> Int -> IO Word8
readWord16OffPtr    :: Ptr Word16        -> Int -> IO Word16
readWord32OffPtr    :: Ptr Word32        -> Int -> IO Word32
readWord64OffPtr    :: Ptr Word64        -> Int -> IO Word64

readWideCharOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWideCharOffAddr# a i s  of (# s2, x #) -> (# s2, C# x #)
readIntOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readIntOffAddr# a i s       of (# s2, x #) -> (# s2, I# x #)
readWordOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWordOffAddr# a i s      of (# s2, x #) -> (# s2, W# x #)
readPtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s      of (# s2, x #) -> (# s2, Ptr x #)
readFunPtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s      of (# s2, x #) -> (# s2, FunPtr x #)
readFloatOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readFloatOffAddr# a i s     of (# s2, x #) -> (# s2, F# x #)
readDoubleOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readDoubleOffAddr# a i s    of (# s2, x #) -> (# s2, D# x #)
readStablePtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readStablePtrOffAddr# a i s of (# s2, x #) -> (# s2, StablePtr x #)
readInt8OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt8OffAddr# a i s      of (# s2, x #) -> (# s2, I8# x #)
readWord8OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord8OffAddr# a i s     of (# s2, x #) -> (# s2, W8# x #)
readInt16OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt16OffAddr# a i s     of (# s2, x #) -> (# s2, I16# x #)
readWord16OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord16OffAddr# a i s    of (# s2, x #) -> (# s2, W16# x #)
readInt32OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt32OffAddr# a i s     of (# s2, x #) -> (# s2, I32# x #)
readWord32OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord32OffAddr# a i s    of (# s2, x #) -> (# s2, W32# x #)
readInt64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt64OffAddr# a i s     of (# s2, x #) -> (# s2, I64# x #)
readWord64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord64OffAddr# a i s    of (# s2, x #) -> (# s2, W64# x #)

writeWideCharOffPtr  :: Ptr Char          -> Int -> Char        -> IO ()
writeIntOffPtr       :: Ptr Int           -> Int -> Int         -> IO ()
writeWordOffPtr      :: Ptr Word          -> Int -> Word        -> IO ()
writePtrOffPtr       :: Ptr (Ptr a)       -> Int -> Ptr a       -> IO ()
writeFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> FunPtr a    -> IO ()
writeFloatOffPtr     :: Ptr Float         -> Int -> Float       -> IO ()
writeDoubleOffPtr    :: Ptr Double        -> Int -> Double      -> IO ()
writeStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
writeInt8OffPtr      :: Ptr Int8          -> Int -> Int8        -> IO ()
writeInt16OffPtr     :: Ptr Int16         -> Int -> Int16       -> IO ()
writeInt32OffPtr     :: Ptr Int32         -> Int -> Int32       -> IO ()
writeInt64OffPtr     :: Ptr Int64         -> Int -> Int64       -> IO ()
writeWord8OffPtr     :: Ptr Word8         -> Int -> Word8       -> IO ()
writeWord16OffPtr    :: Ptr Word16        -> Int -> Word16      -> IO ()
writeWord32OffPtr    :: Ptr Word32        -> Int -> Word32      -> IO ()
writeWord64OffPtr    :: Ptr Word64        -> Int -> Word64      -> IO ()

writeWideCharOffPtr (Ptr a) (I# i) (C# x)
  = IO $ \s -> case writeWideCharOffAddr# a i x s  of s2 -> (# s2, () #)
writeIntOffPtr (Ptr a) (I# i) (I# x)
  = IO $ \s -> case writeIntOffAddr# a i x s       of s2 -> (# s2, () #)
writeWordOffPtr (Ptr a) (I# i) (W# x)
  = IO $ \s -> case writeWordOffAddr# a i x s      of s2 -> (# s2, () #)
writePtrOffPtr (Ptr a) (I# i) (Ptr x)
  = IO $ \s -> case writeAddrOffAddr# a i x s      of s2 -> (# s2, () #)
writeFunPtrOffPtr (Ptr a) (I# i) (FunPtr x)
  = IO $ \s -> case writeAddrOffAddr# a i x s      of s2 -> (# s2, () #)
writeFloatOffPtr (Ptr a) (I# i) (F# x)
  = IO $ \s -> case writeFloatOffAddr# a i x s     of s2 -> (# s2, () #)
writeDoubleOffPtr (Ptr a) (I# i) (D# x)
  = IO $ \s -> case writeDoubleOffAddr# a i x s    of s2 -> (# s2, () #)
writeStablePtrOffPtr (Ptr a) (I# i) (StablePtr x)
  = IO $ \s -> case writeStablePtrOffAddr# a i x s of s2 -> (# s2 , () #)
writeInt8OffPtr (Ptr a) (I# i) (I8# x)
  = IO $ \s -> case writeInt8OffAddr# a i x s      of s2 -> (# s2, () #)
writeWord8OffPtr (Ptr a) (I# i) (W8# x)
  = IO $ \s -> case writeWord8OffAddr# a i x s     of s2 -> (# s2, () #)
writeInt16OffPtr (Ptr a) (I# i) (I16# x)
  = IO $ \s -> case writeInt16OffAddr# a i x s     of s2 -> (# s2, () #)
writeWord16OffPtr (Ptr a) (I# i) (W16# x)
  = IO $ \s -> case writeWord16OffAddr# a i x s    of s2 -> (# s2, () #)
writeInt32OffPtr (Ptr a) (I# i) (I32# x)
  = IO $ \s -> case writeInt32OffAddr# a i x s     of s2 -> (# s2, () #)
writeWord32OffPtr (Ptr a) (I# i) (W32# x)
  = IO $ \s -> case writeWord32OffAddr# a i x s    of s2 -> (# s2, () #)
writeInt64OffPtr (Ptr a) (I# i) (I64# x)
  = IO $ \s -> case writeInt64OffAddr# a i x s     of s2 -> (# s2, () #)
writeWord64OffPtr (Ptr a) (I# i) (W64# x)
  = IO $ \s -> case writeWord64OffAddr# a i x s    of s2 -> (# s2, () #)

\end{code}
