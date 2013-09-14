{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- !!! simple tests of copying/cloning byte arrays
--

module Main ( main ) where

import GHC.Word
import GHC.Exts hiding (IsList(..))
import GHC.Prim
import GHC.ST
import GHC.IO
import GHC.Ptr

main = putStr
       (test_copyByteArray
        ++ "\n" ++ test_copyMutableByteArray
        ++ "\n" ++ test_copyMutableByteArrayOverlap
        ++ "\n" ++ test_copyByteArrayToAddr
        ++ "\n" ++ test_copyMutableByteArrayToAddr
        ++ "\n" ++ test_copyAddrToByteArray
        ++ "\n"
       )

------------------------------------------------------------------------
-- Constants

-- All allocated arrays are of this size
len :: Int
len = 130

-- We copy these many elements
copied :: Int
copied = len - 2

------------------------------------------------------------------------
-- copyByteArray#

-- Copy a slice of the source array into a destination array and check
-- that the copy succeeded.
test_copyByteArray :: String
test_copyByteArray =
    let dst = runST $ do
            src <- newByteArray len
            fill src 0 len
            src <- unsafeFreezeByteArray src
            dst <- newByteArray len
            -- Markers to detect errors
            writeWord8Array dst 0 255
            writeWord8Array dst (len-1) 255
            -- Leave the first and last element untouched
            copyByteArray src 1 dst 1 copied
            unsafeFreezeByteArray dst
    in shows (toList dst len) "\n"

------------------------------------------------------------------------
-- copyMutableByteArray#

-- Copy a slice of the source array into a destination array and check
-- that the copy succeeded.
test_copyMutableByteArray :: String
test_copyMutableByteArray =
    let dst = runST $ do
            src <- newByteArray len
            fill src 0 len
            dst <- newByteArray len
            -- Markers to detect errors
            writeWord8Array dst 0 255
            writeWord8Array dst (len-1) 255
            -- Leave the first and last element untouched
            copyMutableByteArray src 1 dst 1 copied
            unsafeFreezeByteArray dst
    in shows (toList dst len) "\n"

-- Perform a copy where the source and destination part overlap.
test_copyMutableByteArrayOverlap :: String
test_copyMutableByteArrayOverlap =
    let arr = runST $ do
            marr <- fromList inp
            -- Overlap of two elements
            copyMutableByteArray marr 5 marr 7 8
            unsafeFreezeByteArray marr
    in shows (toList arr (length inp)) "\n"
  where
     -- This case was known to fail at some point.
     inp = [0,169,196,9,16,25,36,16,25,81,100,121,144,169,196]

------------------------------------------------------------------------
-- copyByteArrayToAddr#

-- Copy a slice of the source array into a destination memory area and check
-- that the copy succeeded.
test_copyByteArrayToAddr :: String
test_copyByteArrayToAddr =
    let dst = runST $ do
            src <- newByteArray len
            fill src 0 len
            src <- unsafeFreezeByteArray src
            withNewPinnedByteArray len $ \dst dst_marr -> do
              -- Markers to detect errors
              writeWord8Array dst_marr 0 255
              writeWord8Array dst_marr (len-1) 255
              -- Leave the first and last element untouched
              copyByteArrayToAddr src 1 (dst `plusPtr` 1) copied
              unsafeFreezeByteArray dst_marr
    in shows (toList dst len) "\n"

------------------------------------------------------------------------
-- copyMutableByteArrayToAddr#

-- Copy a slice of the source array into a destination memory area and check
-- that the copy succeeded.
test_copyMutableByteArrayToAddr :: String
test_copyMutableByteArrayToAddr =
    let dst = runST $ do
            src <- newByteArray len
            fill src 0 len
            withNewPinnedByteArray len $ \dst dst_marr -> do
              -- Markers to detect errors
              writeWord8Array dst_marr 0 255
              writeWord8Array dst_marr (len-1) 255
              -- Leave the first and last element untouched
              copyMutableByteArrayToAddr src 1 (dst `plusPtr` 1) copied
              unsafeFreezeByteArray dst_marr
    in shows (toList dst len) "\n"

------------------------------------------------------------------------
-- copyAddrToByteArray#

-- Copy a slice of the source memory area into a destination array and check
-- that the copy succeeded.
test_copyAddrToByteArray :: String
test_copyAddrToByteArray =
    let dst = runST $
            withNewPinnedByteArray len $ \src src_marr -> do
              fill src_marr 0 len
              dst <- newByteArray len
              -- Markers to detect errors
              writeWord8Array dst 0 255
              writeWord8Array dst (len-1) 255
              -- Leave the first and last element untouched
              copyAddrToByteArray (src `plusPtr` 1) dst 1 copied
              unsafeFreezeByteArray dst
    in shows (toList dst len) "\n"

------------------------------------------------------------------------
-- Test helpers

-- Initialize the elements of this array, starting at the given
-- offset.  The last parameter specifies the number of elements to
-- initialize.  Element at index @i@ takes the value @i@ (i.e. the
-- first actually modified element will take value @off@).
fill :: MByteArray s -> Int -> Int -> ST s ()
fill marr off count = go 0
  where
    go i
        | i >= fromIntegral count = return ()
        | otherwise = do writeWord8Array marr (off + i) (fromIntegral i)
                         go (i + 1)

fromList :: [Word8] -> ST s (MByteArray s)
fromList xs0 = do
    marr <- newByteArray (length xs0)
    let go [] i = i `seq` return marr
        go (x:xs) i = writeWord8Array marr i x >> go xs (i + 1)
    go xs0 0

------------------------------------------------------------------------
-- Convenience wrappers for ByteArray# and MutableByteArray#

data ByteArray = ByteArray { unBA :: ByteArray# }
data MByteArray s = MByteArray { unMBA :: MutableByteArray# s }

newByteArray :: Int -> ST s (MByteArray s)
newByteArray (I# n#) = ST $ \s# -> case newByteArray# n# s# of
    (# s2#, marr# #) -> (# s2#, MByteArray marr# #)

newPinnedByteArray :: Int -> ST s (Ptr (), MByteArray s)
newPinnedByteArray (I# n#) = ST $ \s# ->
    case newPinnedByteArray# n# s# of
        (# s2#, marr# #) ->
          (# s2#, (Ptr (byteArrayContents# (unsafeCoerce# marr#)), 
                  MByteArray marr#) #)

withNewPinnedByteArray :: Int -> (Ptr () -> MByteArray s -> ST s a) -> ST s a
withNewPinnedByteArray n action = do
    (ptr, marr) <- newPinnedByteArray n
    x <- action ptr marr
    touch marr
    return x

touch :: a -> ST s ()
touch a = unsafeIOToST $ IO $ \s# ->
    case touch# a s# of
        s2# -> (# s2#, () #)

indexWord8Array :: ByteArray -> Int -> Word8
indexWord8Array arr (I# i#) = case indexWord8Array# (unBA arr) i# of
    a -> W8# a

writeWord8Array :: MByteArray s -> Int -> Word8 -> ST s ()
writeWord8Array marr (I# i#) (W8# a) = ST $ \ s# ->
    case writeWord8Array# (unMBA marr) i# a s# of
        s2# -> (# s2#, () #)

unsafeFreezeByteArray :: MByteArray s -> ST s (ByteArray)
unsafeFreezeByteArray marr = ST $ \ s# ->
    case unsafeFreezeByteArray# (unMBA marr) s# of
        (# s2#, arr# #) -> (# s2#, ByteArray arr# #)

copyByteArray :: ByteArray -> Int -> MByteArray s -> Int -> Int -> ST s ()
copyByteArray src (I# six#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copyByteArray# (unBA src) six# (unMBA dst) dix# n# s# of
        s2# -> (# s2#, () #)

copyMutableByteArray :: MByteArray s -> Int -> MByteArray s -> Int -> Int
                     -> ST s ()
copyMutableByteArray src (I# six#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copyMutableByteArray# (unMBA src) six# (unMBA dst) dix# n# s# of
        s2# -> (# s2#, () #)

copyAddrToByteArray :: Ptr () -> MByteArray s -> Int -> Int -> ST s ()
copyAddrToByteArray (Ptr src#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copyAddrToByteArray# src# (unMBA dst) dix# n# s# of
        s2# -> (# s2#, () #)

copyByteArrayToAddr :: ByteArray -> Int -> Ptr () -> Int -> ST s ()
copyByteArrayToAddr src (I# six#) (Ptr dst#) (I# n#) = ST $ \ s# ->
    case copyByteArrayToAddr# (unBA src) six# dst# n# s# of
        s2# -> (# s2#, () #)

copyMutableByteArrayToAddr :: MByteArray s -> Int -> Ptr () -> Int -> ST s ()
copyMutableByteArrayToAddr src (I# six#) (Ptr dst#) (I# n#) = ST $ \ s# ->
    case copyMutableByteArrayToAddr# (unMBA src) six# dst# n# s# of
        s2# -> (# s2#, () #)

toList :: ByteArray -> Int -> [Word8]
toList arr n = go 0
  where
    go i | i >= n = []
         | otherwise = indexWord8Array arr i : go (i+1)
