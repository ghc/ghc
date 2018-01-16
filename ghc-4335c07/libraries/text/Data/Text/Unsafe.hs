{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Text.Unsafe
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- A module containing unsafe 'Text' operations, for very very careful
-- use in heavily tested code.
module Data.Text.Unsafe
    (
      inlineInterleaveST
    , inlinePerformIO
    , unsafeDupablePerformIO
    , Iter(..)
    , iter
    , iter_
    , reverseIter
    , reverseIter_
    , unsafeHead
    , unsafeTail
    , lengthWord16
    , takeWord16
    , dropWord16
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Unsafe (inlineInterleaveST, inlinePerformIO)
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import qualified Data.Text.Array as A
import GHC.IO (unsafeDupablePerformIO)

-- | /O(1)/ A variant of 'head' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeHead :: Text -> Char
unsafeHead (Text arr off _len)
    | m < 0xD800 || m > 0xDBFF = unsafeChr m
    | otherwise                = chr2 m n
    where m = A.unsafeIndex arr off
          n = A.unsafeIndex arr (off+1)
{-# INLINE unsafeHead #-}

-- | /O(1)/ A variant of 'tail' for non-empty 'Text'. 'unsafeTail'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeTail :: Text -> Text
unsafeTail t@(Text arr off len) =
#if defined(ASSERTS)
    assert (d <= len) $
#endif
    Text arr (off+d) (len-d)
  where d = iter_ t 0
{-# INLINE unsafeTail #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-16
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter :: Text -> Int -> Iter
iter (Text arr off _len) i
    | m < 0xD800 || m > 0xDBFF = Iter (unsafeChr m) 1
    | otherwise                = Iter (chr2 m n) 2
  where m = A.unsafeIndex arr j
        n = A.unsafeIndex arr k
        j = off + i
        k = j + 1
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-16 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Text -> Int -> Int
iter_ (Text arr off _len) i | m < 0xD800 || m > 0xDBFF = 1
                            | otherwise                = 2
  where m = A.unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

-- | /O(1)/ Iterate one step backwards through a UTF-16 array,
-- returning the current character and the delta to add (i.e. a
-- negative number) to give the next offset to iterate at.
reverseIter :: Text -> Int -> (Char,Int)
reverseIter (Text arr off _len) i
    | m < 0xDC00 || m > 0xDFFF = (unsafeChr m, -1)
    | otherwise                = (chr2 n m,    -2)
  where m = A.unsafeIndex arr j
        n = A.unsafeIndex arr k
        j = off + i
        k = j - 1
{-# INLINE reverseIter #-}

-- | /O(1)/ Iterate one step backwards through a UTF-16 array,
-- returning the delta to add (i.e. a negative number) to give the
-- next offset to iterate at.
reverseIter_ :: Text -> Int -> Int
reverseIter_ (Text arr off _len) i
    | m < 0xDC00 || m > 0xDFFF = -1
    | otherwise                = -2
  where m = A.unsafeIndex arr (off+i)
{-# INLINE reverseIter_ #-}

-- | /O(1)/ Return the length of a 'Text' in units of 'Word16'.  This
-- is useful for sizing a target array appropriately before using
-- 'unsafeCopyToPtr'.
lengthWord16 :: Text -> Int
lengthWord16 (Text _arr _off len) = len
{-# INLINE lengthWord16 #-}

-- | /O(1)/ Unchecked take of 'k' 'Word16's from the front of a 'Text'.
takeWord16 :: Int -> Text -> Text
takeWord16 k (Text arr off _len) = Text arr off k
{-# INLINE takeWord16 #-}

-- | /O(1)/ Unchecked drop of 'k' 'Word16's from the front of a 'Text'.
dropWord16 :: Int -> Text -> Text
dropWord16 k (Text arr off len) = Text arr (off+k) (len-k)
{-# INLINE dropWord16 #-}
