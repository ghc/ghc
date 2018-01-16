{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Text.Foreign
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for using 'Text' data with native code via the Haskell
-- foreign function interface.

module Data.Text.Foreign
    (
    -- * Interoperability with native code
    -- $interop
      I16
    -- * Safe conversion functions
    , fromPtr
    , useAsPtr
    , asForeignPtr
    -- ** Encoding as UTF-8
    , peekCStringLen
    , withCStringLen
    -- * Unsafe conversion code
    , lengthWord16
    , unsafeCopyToPtr
    -- * Low-level manipulation
    -- $lowlevel
    , dropWord16
    , takeWord16
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
#if __GLASGOW_HASKELL__ >= 702
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Internal (Text(..), empty)
import Data.Text.Unsafe (lengthWord16)
import Data.Word (Word16)
import Foreign.C.String (CStringLen)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import qualified Data.Text.Array as A

-- $interop
--
-- The 'Text' type is implemented using arrays that are not guaranteed
-- to have a fixed address in the Haskell heap. All communication with
-- native code must thus occur by copying data back and forth.
--
-- The 'Text' type's internal representation is UTF-16, using the
-- platform's native endianness.  This makes copied data suitable for
-- use with native libraries that use a similar representation, such
-- as ICU.  To interoperate with native libraries that use different
-- internal representations, such as UTF-8 or UTF-32, consider using
-- the functions in the 'Data.Text.Encoding' module.

-- | A type representing a number of UTF-16 code units.
newtype I16 = I16 Int
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | /O(n)/ Create a new 'Text' from a 'Ptr' 'Word16' by copying the
-- contents of the array.
fromPtr :: Ptr Word16           -- ^ source array
        -> I16                  -- ^ length of source array (in 'Word16' units)
        -> IO Text
fromPtr _   (I16 0)   = return empty
fromPtr ptr (I16 len) =
#if defined(ASSERTS)
    assert (len > 0) $
#endif
    return $! Text arr 0 len
  where
    arr = A.run (A.new len >>= copy)
    copy marr = loop ptr 0
      where
        loop !p !i | i == len = return marr
                   | otherwise = do
          A.unsafeWrite marr i =<< unsafeIOToST (peek p)
          loop (p `plusPtr` 2) (i + 1)

-- $lowlevel
--
-- Foreign functions that use UTF-16 internally may return indices in
-- units of 'Word16' instead of characters.  These functions may
-- safely be used with such indices, as they will adjust offsets if
-- necessary to preserve the validity of a Unicode string.

-- | /O(1)/ Return the prefix of the 'Text' of @n@ 'Word16' units in
-- length.
--
-- If @n@ would cause the 'Text' to end inside a surrogate pair, the
-- end of the prefix will be advanced by one additional 'Word16' unit
-- to maintain its validity.
takeWord16 :: I16 -> Text -> Text
takeWord16 (I16 n) t@(Text arr off len)
    | n <= 0               = empty
    | n >= len || m >= len = t
    | otherwise            = Text arr off m
  where
    m | w < 0xD800 || w > 0xDBFF = n
      | otherwise                = n+1
    w = A.unsafeIndex arr (off+n-1)

-- | /O(1)/ Return the suffix of the 'Text', with @n@ 'Word16' units
-- dropped from its beginning.
--
-- If @n@ would cause the 'Text' to begin inside a surrogate pair, the
-- beginning of the suffix will be advanced by one additional 'Word16'
-- unit to maintain its validity.
dropWord16 :: I16 -> Text -> Text
dropWord16 (I16 n) t@(Text arr off len)
    | n <= 0               = t
    | n >= len || m >= len = empty
    | otherwise            = Text arr (off+m) (len-m)
  where
    m | w < 0xD800 || w > 0xDBFF = n
      | otherwise                = n+1
    w = A.unsafeIndex arr (off+n-1)

-- | /O(n)/ Copy a 'Text' to an array.  The array is assumed to be big
-- enough to hold the contents of the entire 'Text'.
unsafeCopyToPtr :: Text -> Ptr Word16 -> IO ()
unsafeCopyToPtr (Text arr off len) ptr = loop ptr off
  where
    end = off + len
    loop !p !i | i == end  = return ()
               | otherwise = do
      poke p (A.unsafeIndex arr i)
      loop (p `plusPtr` 2) (i + 1)

-- | /O(n)/ Perform an action on a temporary, mutable copy of a
-- 'Text'.  The copy is freed as soon as the action returns.
useAsPtr :: Text -> (Ptr Word16 -> I16 -> IO a) -> IO a
useAsPtr t@(Text _arr _off len) action =
    allocaBytes (len * 2) $ \buf -> do
      unsafeCopyToPtr t buf
      action (castPtr buf) (fromIntegral len)

-- | /O(n)/ Make a mutable copy of a 'Text'.
asForeignPtr :: Text -> IO (ForeignPtr Word16, I16)
asForeignPtr t@(Text _arr _off len) = do
  fp <- mallocForeignPtrArray len
  withForeignPtr fp $ unsafeCopyToPtr t
  return (fp, I16 len)

-- | /O(n)/ Decode a C string with explicit length, which is assumed
-- to have been encoded as UTF-8. If decoding fails, a
-- 'UnicodeException' is thrown.
peekCStringLen :: CStringLen -> IO Text
peekCStringLen cs = do
  bs <- unsafePackCStringLen cs
  return $! decodeUtf8 bs

-- | Marshal a 'Text' into a C string encoded as UTF-8 in temporary
-- storage, with explicit length information. The encoded string may
-- contain NUL bytes, and is not followed by a trailing NUL byte.
--
-- The temporary storage is freed when the subcomputation terminates
-- (either normally or via an exception), so the pointer to the
-- temporary storage must /not/ be used after this function returns.
withCStringLen :: Text -> (CStringLen -> IO a) -> IO a
withCStringLen t act = unsafeUseAsCStringLen (encodeUtf8 t) act
