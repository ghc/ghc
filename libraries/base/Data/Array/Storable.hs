-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Storable
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A storable array is an IO-mutable array which stores its
-- contents in a contiguous memory block living in the C
-- heap. Elements are stored according to the class Storable.
-- You can obtain the pointer to the array contents to manipulate
-- elements from languages like C.
--
-- It's similar to IOUArray but slower. Its advantage is that
-- it's compatible with C.
--
-----------------------------------------------------------------------------

module Data.Array.Storable (
    
    -- Array type:
    StorableArray, -- data StorableArray index element
                   --     -- index type must be in class Ix
                   --     -- element type must be in class Storable
    
    -- Module MArray provides the interface of storable arrays.
    -- They are instances of class MArray (with IO monad).
    module Data.Array.MArray,
    
    withStorableArray,
    unsafeStorableArrayToPtr, touchStorableArray,
    unsafeStorableArrayToIOUArray
    )
    where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Exts
import GHC.IOBase	( IO(..) )
import GHC.Word
import GHC.Int
import GHC.Stable	( StablePtr(..) )
#endif

import Data.Array.Base
import Data.Array.MArray
import Data.Array.IO.Internals	( IOUArray(..) )
import Foreign hiding (newArray)

data StorableArray i e = StorableArray !i !i !(MutableByteArray# RealWorld)

instance HasBounds StorableArray where
    bounds (StorableArray l u _) = (l,u)

newStorableArray :: (Ix ix, Storable e) => (ix,ix) -> IO (StorableArray ix e)
#ifndef __HADDOCK__
newStorableArray (l,u) :: IO (StorableArray ix e) = IO $ \s1# ->
  case rangeSize (l,u)            of { I# n# ->
  let I# size = sizeOf (undefined :: e) in
  case newPinnedByteArray# (size *# n#) s1# of { (# s2#, marr# #) ->
  (# s2#, StorableArray l u marr# #) }}
#endif

-- | Convert a 'StorableArray' into a 'Ptr' for the duration of the
-- specified IO action.  The 'Ptr' is not valid outside the IO action, so
-- don't return it and use it later.
withStorableArray :: StorableArray i e -> (Ptr e -> IO a) -> IO a
withStorableArray arr f = do
  r <- f (unsafeStorableArrayToPtr arr)
  touchStorableArray arr
  return r

-- | Converts a 'StorableArray' into a 'Ptr'.  This function is unsafe, because
-- it does not ensure that the 'StorableArray' is kept alive.  Should be used 
-- in conjunction with 'touchStorableArray'.
unsafeStorableArrayToPtr :: StorableArray i e -> Ptr a
unsafeStorableArrayToPtr (StorableArray _ _ arr#) 
 = Ptr (byteArrayContents# (unsafeCoerce# arr#))

-- | For use in conjunction with 'unsafeStorableArrayToPtr'.  Applying
-- 'touchStorableArray' to the 'StorableArray' ensures that the array
-- will not be garbage collected before that point.  (NOTE: 'withStorableArray'
-- is preferable to 'unsafeStorableArrayToPtr'\/'touchStorableArray').
touchStorableArray :: StorableArray i e -> IO ()
touchStorableArray (StorableArray _ _ arr#) = IO $ \s ->
  case touch# arr# s of s2 -> (# s2, () #)

-- | Coerces a 'StorableArray' into an 'IOUArray'.  This is safe as
-- long as the representation of the elements is the same, which is
-- currently true for all element types except 'Bool'.
--
-- Going the other direction would be less safe, however, because the
-- byte array in an 'IOUArray' might not be pinned, so using 
-- 'withStorableArray' on the resulting 'StorableArray' would not be safe.
--
-- Bear in mind that you might not be able to /use/ the 'IOUArray' unless
-- the element type is supported by one of the available instances of
-- MArray.
unsafeStorableArrayToIOUArray :: StorableArray i e -> IOUArray i e
unsafeStorableArrayToIOUArray (StorableArray l u arr#)
  = IOUArray (STUArray l u arr#)

-- The general case
instance Storable e => MArray StorableArray e IO where
    newArray_ = newStorableArray
    unsafeRead  a i   = withStorableArray a $ \p -> peekElemOff p i
    unsafeWrite a i e = withStorableArray a $ \p -> pokeElemOff p i e

{-# RULES
"unsafeRead/StorableArray/Char"   unsafeRead  = unsafeReadChar
"unsafeWrite/StorableArray/Char"  unsafeWrite = unsafeWriteChar
"unsafeRead/StorableArray/Int"   unsafeRead  = unsafeReadInt
"unsafeWrite/StorableArray/Int"  unsafeWrite = unsafeWriteInt
"unsafeRead/StorableArray/Word"   unsafeRead  = unsafeReadWord
"unsafeWrite/StorableArray/Word"   unsafeWrite  = unsafeWriteWord
"unsafeRead/StorableArray/Ptr"   unsafeRead  = unsafeReadPtr
"unsafeWrite/StorableArray/Ptr"  unsafeWrite = unsafeWritePtr
"unsafeRead/StorableArray/FunPtr"   unsafeRead  = unsafeReadFunPtr
"unsafeWrite/StorableArray/FunPtr"  unsafeWrite = unsafeWriteFunPtr
"unsafeRead/StorableArray/Float"   unsafeRead  = unsafeReadFloat
"unsafeWrite/StorableArray/Float"  unsafeWrite = unsafeWriteFloat
"unsafeRead/StorableArray/Double"   unsafeRead  = unsafeReadDouble
"unsafeWrite/StorableArray/Double"  unsafeWrite = unsafeWriteDouble
"unsafeRead/StorableArray/StablePtr"   unsafeRead  = unsafeReadStablePtr
"unsafeWrite/StorableArray/StablePtr"  unsafeWrite = unsafeWriteStablePtr
"unsafeRead/StorableArray/Int8"    unsafeRead  = unsafeReadInt8
"unsafeWrite/StorableArray/Int8"   unsafeWrite = unsafeWriteInt8
"unsafeRead/StorableArray/Int16"   unsafeRead  = unsafeReadInt16
"unsafeWrite/StorableArray/Int16"  unsafeWrite = unsafeWriteInt16
"unsafeRead/StorableArray/Int32"   unsafeRead  = unsafeReadInt32
"unsafeWrite/StorableArray/Int32"  unsafeWrite = unsafeWriteInt32
"unsafeRead/StorableArray/Int64"   unsafeRead  = unsafeReadInt64
"unsafeWrite/StorableArray/Int64"  unsafeWrite = unsafeWriteInt64
"unsafeRead/StorableArray/Word8"   unsafeRead  = unsafeReadWord8
"unsafeWrite/StorableArray/Word8"  unsafeWrite = unsafeWriteWord8
"unsafeRead/StorableArray/Word16"  unsafeRead  = unsafeReadWord16
"unsafeWrite/StorableArray/Word16" unsafeWrite = unsafeWriteWord16
"unsafeRead/StorableArray/Word32"  unsafeRead  = unsafeReadWord32
"unsafeWrite/StorableArray/Word32" unsafeWrite = unsafeWriteWord32
"unsafeRead/StorableArray/Word64"  unsafeRead  = unsafeReadWord64
"unsafeWrite/StorableArray/Word64" unsafeWrite = unsafeWriteWord64
 #-}

{-# INLINE unsafeReadChar #-}
unsafeReadChar :: StorableArray ix Char -> Int -> IO Char
unsafeReadChar (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readWideCharArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, C# e# #) }

{-# INLINE unsafeReadInt #-}
unsafeReadInt :: StorableArray ix Int -> Int -> IO Int
unsafeReadInt (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readIntArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I# e# #) }

{-# INLINE unsafeReadWord #-}
unsafeReadWord :: StorableArray ix Word -> Int -> IO Word
unsafeReadWord (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readWordArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W# e# #) }

{-# INLINE unsafeReadPtr #-}
unsafeReadPtr :: StorableArray ix (Ptr a) -> Int -> IO (Ptr a)
unsafeReadPtr (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readAddrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, Ptr e# #) }

{-# INLINE unsafeReadFunPtr #-}
unsafeReadFunPtr :: StorableArray ix (FunPtr a) -> Int -> IO (FunPtr a)
unsafeReadFunPtr (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readAddrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, FunPtr e# #) }

{-# INLINE unsafeReadFloat #-}
unsafeReadFloat :: StorableArray ix Float -> Int -> IO Float
unsafeReadFloat (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readFloatArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, F# e# #) }

{-# INLINE unsafeReadDouble #-}
unsafeReadDouble :: StorableArray ix Double -> Int -> IO Double
unsafeReadDouble (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readDoubleArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, D# e# #) }

{-# INLINE unsafeReadStablePtr #-}
unsafeReadStablePtr :: StorableArray ix (StablePtr a) -> Int -> IO (StablePtr a)
unsafeReadStablePtr (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readStablePtrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, StablePtr e# #) }

{-# INLINE unsafeReadInt8 #-}
unsafeReadInt8 :: StorableArray ix Int8 -> Int -> IO Int8
unsafeReadInt8 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readInt8Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I8# e# #) }

{-# INLINE unsafeReadInt16 #-}
unsafeReadInt16 :: StorableArray ix Int16 -> Int -> IO Int16
unsafeReadInt16 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readInt16Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I16# e# #) }

{-# INLINE unsafeReadInt32 #-}
unsafeReadInt32 :: StorableArray ix Int32 -> Int -> IO Int32
unsafeReadInt32 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readInt32Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I32# e# #) }

{-# INLINE unsafeReadInt64 #-}
unsafeReadInt64 :: StorableArray ix Int64 -> Int -> IO Int64
unsafeReadInt64 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readInt64Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I64# e# #) }

{-# INLINE unsafeReadWord8 #-}
unsafeReadWord8 :: StorableArray ix Word8 -> Int -> IO Word8
unsafeReadWord8 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readWord8Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W8# e# #) }

{-# INLINE unsafeReadWord16 #-}
unsafeReadWord16 :: StorableArray ix Word16 -> Int -> IO Word16
unsafeReadWord16 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readWord16Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W16# e# #) }

{-# INLINE unsafeReadWord32 #-}
unsafeReadWord32 :: StorableArray ix Word32 -> Int -> IO Word32
unsafeReadWord32 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readWord32Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W32# e# #) }

{-# INLINE unsafeReadWord64 #-}
unsafeReadWord64 :: StorableArray ix Word64 -> Int -> IO Word64
unsafeReadWord64 (StorableArray _ _ marr#) (I# i#) = IO $ \s1# ->
        case readWord64Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W64# e# #) }

{-# INLINE unsafeWriteChar #-}
unsafeWriteChar :: StorableArray ix Char -> Int -> Char -> IO ()
unsafeWriteChar (StorableArray _ _ marr#) (I# i#) (C# e#) = IO $ \s1# ->
        case writeWideCharArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteInt #-}
unsafeWriteInt :: StorableArray ix Int -> Int -> Int -> IO ()
unsafeWriteInt (StorableArray _ _ marr#) (I# i#) (I# e#) = IO $ \s1# ->
        case writeIntArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteWord #-}
unsafeWriteWord :: StorableArray ix Word -> Int -> Word -> IO ()
unsafeWriteWord (StorableArray _ _ marr#) (I# i#) (W# e#) = IO $ \s1# ->
        case writeWordArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWritePtr #-}
unsafeWritePtr :: StorableArray ix (Ptr a) -> Int -> (Ptr a) -> IO ()
unsafeWritePtr (StorableArray _ _ marr#) (I# i#) (Ptr e#) = IO $ \s1# ->
        case writeAddrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteFunPtr #-}
unsafeWriteFunPtr :: StorableArray ix (FunPtr a) -> Int -> (FunPtr a) -> IO ()
unsafeWriteFunPtr (StorableArray _ _ marr#) (I# i#) (FunPtr e#) = IO $ \s1# ->
        case writeAddrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteFloat #-}
unsafeWriteFloat :: StorableArray ix Float -> Int -> Float -> IO ()
unsafeWriteFloat (StorableArray _ _ marr#) (I# i#) (F# e#) = IO $ \s1# ->
        case writeFloatArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteDouble #-}
unsafeWriteDouble :: StorableArray ix Double -> Int -> Double -> IO ()
unsafeWriteDouble (StorableArray _ _ marr#) (I# i#) (D# e#) = IO $ \s1# ->
        case writeDoubleArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteStablePtr #-}
unsafeWriteStablePtr :: StorableArray ix (StablePtr a) -> Int -> (StablePtr a) -> IO ()
unsafeWriteStablePtr (StorableArray _ _ marr#) (I# i#) (StablePtr e#) = IO $ \s1# ->
        case writeStablePtrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteInt8 #-}
unsafeWriteInt8 :: StorableArray ix Int8 -> Int -> Int8 -> IO ()
unsafeWriteInt8 (StorableArray _ _ marr#) (I# i#) (I8# e#) = IO $ \s1# ->
        case writeInt8Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteInt16 #-}
unsafeWriteInt16 :: StorableArray ix Int16 -> Int -> Int16 -> IO ()
unsafeWriteInt16 (StorableArray _ _ marr#) (I# i#) (I16# e#) = IO $ \s1# ->
        case writeInt16Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteInt32 #-}
unsafeWriteInt32 :: StorableArray ix Int32 -> Int -> Int32 -> IO ()
unsafeWriteInt32 (StorableArray _ _ marr#) (I# i#) (I32# e#) = IO $ \s1# ->
        case writeInt32Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteInt64 #-}
unsafeWriteInt64 :: StorableArray ix Int64 -> Int -> Int64 -> IO ()
unsafeWriteInt64 (StorableArray _ _ marr#) (I# i#) (I64# e#) = IO $ \s1# ->
        case writeInt64Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteWord8 #-}
unsafeWriteWord8 :: StorableArray ix Word8 -> Int -> Word8 -> IO ()
unsafeWriteWord8 (StorableArray _ _ marr#) (I# i#) (W8# e#) = IO $ \s1# ->
        case writeWord8Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteWord16 #-}
unsafeWriteWord16 :: StorableArray ix Word16 -> Int -> Word16 -> IO ()
unsafeWriteWord16 (StorableArray _ _ marr#) (I# i#) (W16# e#) = IO $ \s1# ->
        case writeWord16Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteWord32 #-}
unsafeWriteWord32 :: StorableArray ix Word32 -> Int -> Word32 -> IO ()
unsafeWriteWord32 (StorableArray _ _ marr#) (I# i#) (W32# e#) = IO $ \s1# ->
        case writeWord32Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeWriteWord64 #-}
unsafeWriteWord64 :: StorableArray ix Word64 -> Int -> Word64 -> IO ()
unsafeWriteWord64 (StorableArray _ _ marr#) (I# i#) (W64# e#) = IO $ \s1# ->
        case writeWord64Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }
