{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- !!! simple tests of copying/cloning primitive arrays
--

module Main ( main ) where

import GHC.Exts hiding (IsList(..))
import GHC.Prim
import GHC.ST

main = putStr
       (test_copyArray
        ++ "\n" ++ test_copyMutableArray
        ++ "\n" ++ test_copyMutableArrayOverlap
        ++ "\n" ++ test_cloneArray
        ++ "\n" ++ test_cloneMutableArray
        ++ "\n" ++ test_cloneMutableArrayEmpty
        ++ "\n" ++ test_freezeArray
        ++ "\n" ++ test_thawArray
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
-- copyArray#

-- Copy a slice of the source array into a destination array and check
-- that the copy succeeded.
test_copyArray :: String
test_copyArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            src <- unsafeFreezeArray src
            dst <- newArray len (-1)
            -- Leave the first and last element untouched
            copyArray src 1 dst 1 copied
            unsafeFreezeArray dst
    in shows (toList dst len) "\n"

------------------------------------------------------------------------
-- copyMutableArray#

-- Copy a slice of the source array into a destination array and check
-- that the copy succeeded.
test_copyMutableArray :: String
test_copyMutableArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            dst <- newArray len (-1)
            -- Leave the first and last element untouched
            copyMutableArray src 1 dst 1 copied
            unsafeFreezeArray dst
    in shows (toList dst len) "\n"

-- Perform a copy where the source and destination part overlap.
test_copyMutableArrayOverlap :: String
test_copyMutableArrayOverlap =
    let arr = runST $ do
            marr <- fromList inp
            -- Overlap of two elements
            copyMutableArray marr 5 marr 7 8
            unsafeFreezeArray marr
    in shows (toList arr (length inp)) "\n"
  where
     -- This case was known to fail at some point.
     inp = [0,169,196,9,16,25,36,16,25,81,100,121,144,169,196]

------------------------------------------------------------------------
-- cloneArray#

-- Clone a slice of the source array into a destination array and
-- check that the clone succeeded.
test_cloneArray :: String
test_cloneArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            src <- unsafeFreezeArray src
            -- Don't include the first and last element.
            return $ cloneArray src 1 copied
    in shows (toList dst copied) "\n"

------------------------------------------------------------------------
-- cloneMutableArray#

-- Clone a slice of the source array into a destination array and
-- check that the clone succeeded.
test_cloneMutableArray :: String
test_cloneMutableArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            -- Don't include the first and last element.
            dst <- cloneMutableArray src 1 copied
            unsafeFreezeArray dst
    in shows (toList dst copied) "\n"

-- Check that zero-length clones work.
test_cloneMutableArrayEmpty :: String
test_cloneMutableArrayEmpty =
    let dst = runST $ do
            src <- newArray len 0
            dst <- cloneMutableArray src 0 0
            unsafeFreezeArray dst
    in shows (toList dst 0) "\n"

------------------------------------------------------------------------
-- freezeArray#

-- Clone a slice of the source array into a destination array and
-- check that the clone succeeded.
test_freezeArray :: String
test_freezeArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            -- Don't include the first and last element.
            freezeArray src 1 copied
    in shows (toList dst copied) "\n"

------------------------------------------------------------------------
-- thawArray#

-- Clone a slice of the source array into a destination array and
-- check that the clone succeeded.
test_thawArray :: String
test_thawArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            src <- unsafeFreezeArray src
            -- Don't include the first and last element.
            dst <- thawArray src 1 copied
            unsafeFreezeArray dst
    in shows (toList dst copied) "\n"

------------------------------------------------------------------------
-- Test helpers

-- Initialize the elements of this array, starting at the given
-- offset.  The last parameter specifies the number of elements to
-- initialize.  Element at index @i@ takes the value @i*i@ (i.e. the
-- first actually modified element will take value @off*off@).
fill :: MArray s Int -> Int -> Int -> ST s ()
fill marr off count = go 0
  where
    go i
        | i >= count = return ()
        | otherwise = writeArray marr (off + i) (i*i) >> go (i + 1)

fromList :: [Int] -> ST s (MArray s Int)
fromList xs0 = do
    marr <- newArray (length xs0) bottomElem
    let go [] i = i `seq` return marr
        go (x:xs) i = writeArray marr i x >> go xs (i + 1)
    go xs0 0
  where
    bottomElem = error "undefined element"

------------------------------------------------------------------------
-- Convenience wrappers for Array# and MutableArray#

data Array a = Array { unArray :: Array# a }
data MArray s a = MArray { unMArray :: MutableArray# s a }

newArray :: Int -> a -> ST s (MArray s a)
newArray (I# n#) a = ST $ \s# -> case newArray# n# a s# of
    (# s2#, marr# #) -> (# s2#, MArray marr# #)

indexArray :: Array a -> Int -> a
indexArray arr (I# i#) = case indexArray# (unArray arr) i# of
    (# a #) -> a

writeArray :: MArray s a -> Int -> a -> ST s ()
writeArray marr (I# i#) a = ST $ \ s# ->
    case writeArray# (unMArray marr) i# a s# of
        s2# -> (# s2#, () #)

unsafeFreezeArray :: MArray s a -> ST s (Array a)
unsafeFreezeArray marr = ST $ \ s# ->
    case unsafeFreezeArray# (unMArray marr) s# of
        (# s2#, arr# #) -> (# s2#, Array arr# #)

copyArray :: Array a -> Int -> MArray s a -> Int -> Int -> ST s ()
copyArray src (I# six#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copyArray# (unArray src) six# (unMArray dst) dix# n# s# of
        s2# -> (# s2#, () #)

copyMutableArray :: MArray s a -> Int -> MArray s a -> Int -> Int -> ST s ()
copyMutableArray src (I# six#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copyMutableArray# (unMArray src) six# (unMArray dst) dix# n# s# of
        s2# -> (# s2#, () #)

cloneArray :: Array a -> Int -> Int -> Array a
cloneArray src (I# six#) (I# n#) = Array (cloneArray# (unArray src) six# n#)

cloneMutableArray :: MArray s a -> Int -> Int -> ST s (MArray s a)
cloneMutableArray src (I# six#) (I# n#) = ST $ \ s# ->
    case cloneMutableArray# (unMArray src) six# n# s# of
        (# s2#, marr# #) -> (# s2#, MArray marr# #)

freezeArray :: MArray s a -> Int -> Int -> ST s (Array a)
freezeArray src (I# six#) (I# n#) = ST $ \ s# ->
    case freezeArray# (unMArray src) six# n# s# of
        (# s2#, arr# #) -> (# s2#, Array arr# #)

thawArray :: Array a -> Int -> Int -> ST s (MArray s a)
thawArray src (I# six#) (I# n#) = ST $ \ s# ->
    case thawArray# (unArray src) six# n# s# of
        (# s2#, marr# #) -> (# s2#, MArray marr# #)

toList :: Array a -> Int -> [a]
toList arr n = go 0
  where
    go i | i >= n = []
         | otherwise = indexArray arr i : go (i+1)
