{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- !!! simple tests of copying/cloning primitive arrays
--

module Main ( main ) where

import GHC.Exts hiding (IsList(..))
import GHC.Prim
import GHC.ST

main :: IO ()
main = putStr
       (test_copyArray
        ++ "\n" ++ test_copyMutableArray
        ++ "\n" ++ test_copyMutableArrayOverlap
        ++ "\n" ++ test_cloneArray
        ++ "\n" ++ test_cloneArrayStatic
        ++ "\n" ++ test_cloneMutableArray
        ++ "\n" ++ test_cloneMutableArrayEmpty
        ++ "\n" ++ test_cloneMutableArrayStatic
        ++ "\n" ++ test_freezeArray
        ++ "\n" ++ test_freezeArrayStatic
        ++ "\n" ++ test_thawArray
        ++ "\n" ++ test_thawArrayStatic
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

copiedStatic :: Int
copiedStatic = 16
{-# INLINE copiedStatic #-}  -- to make sure optimization triggers

------------------------------------------------------------------------
-- copySmallArray#

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
-- copySmallMutableArray#

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
-- cloneSmallArray#

-- Clone a slice of the source array into a destination array and
-- check that the clone succeeded.
test_cloneArray :: String
test_cloneArray =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            src <- unsafeFreezeArray src
            -- Don't include the first and last element.
            return $! cloneArray src 1 copied
    in shows (toList dst copied) "\n"

--  Check that the static-size optimization works.
test_cloneArrayStatic :: String
test_cloneArrayStatic =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            src <- unsafeFreezeArray src
            -- Don't include the first and last element.
            return $! cloneArray src 1 copiedStatic
    in shows (toList dst copiedStatic) "\n"

------------------------------------------------------------------------
-- cloneMutableSmallArray#

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

--  Check that the static-size optimization works.
test_cloneMutableArrayStatic :: String
test_cloneMutableArrayStatic =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            -- Don't include the first and last element.
            dst <- cloneMutableArray src 1 copiedStatic
            unsafeFreezeArray dst
    in shows (toList dst copiedStatic) "\n"

------------------------------------------------------------------------
-- freezeSmallArray#

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

--  Check that the static-size optimization works.
test_freezeArrayStatic :: String
test_freezeArrayStatic =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            -- Don't include the first and last element.
            freezeArray src 1 copiedStatic
    in shows (toList dst copiedStatic) "\n"

------------------------------------------------------------------------
-- thawSmallArray#

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

--  Check that the static-size optimization works.
test_thawArrayStatic :: String
test_thawArrayStatic =
    let dst = runST $ do
            src <- newArray len 0
            fill src 0 len
            src <- unsafeFreezeArray src
            -- Don't include the first and last element.
            dst <- thawArray src 1 copiedStatic
            unsafeFreezeArray dst
    in shows (toList dst copiedStatic) "\n"

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
-- Convenience wrappers for SmallArray# and MutableSmallArray#

data Array a = Array { unArray :: SmallArray# a }
data MArray s a = MArray { unMArray :: SmallMutableArray# s a }

newArray :: Int -> a -> ST s (MArray s a)
newArray (I# n#) a = ST $ \s# -> case newSmallArray# n# a s# of
    (# s2#, marr# #) -> (# s2#, MArray marr# #)

indexArray :: Array a -> Int -> a
indexArray arr i@(I# i#)
  | i < 0 || i >= len =
      error $ "bounds error, offset " ++ show i ++ ", length " ++ show len
  | otherwise = case indexSmallArray# (unArray arr) i# of
      (# a #) -> a
  where len = lengthArray arr

writeArray :: MArray s a -> Int -> a -> ST s ()
writeArray marr i@(I# i#) a
  | i < 0 || i >= len =
      error $ "bounds error, offset " ++ show i ++ ", length " ++ show len
  | otherwise = ST $ \ s# ->
    case writeSmallArray# (unMArray marr) i# a s# of
        s2# -> (# s2#, () #)
  where len = lengthMArray marr

lengthArray :: Array a -> Int
lengthArray arr = I# (sizeofSmallArray# (unArray arr))

lengthMArray :: MArray s a -> Int
lengthMArray marr = I# (sizeofSmallMutableArray# (unMArray marr))

unsafeFreezeArray :: MArray s a -> ST s (Array a)
unsafeFreezeArray marr = ST $ \ s# ->
    case unsafeFreezeSmallArray# (unMArray marr) s# of
        (# s2#, arr# #) -> (# s2#, Array arr# #)

copyArray :: Array a -> Int -> MArray s a -> Int -> Int -> ST s ()
copyArray src (I# six#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copySmallArray# (unArray src) six# (unMArray dst) dix# n# s# of
        s2# -> (# s2#, () #)

copyMutableArray :: MArray s a -> Int -> MArray s a -> Int -> Int -> ST s ()
copyMutableArray src (I# six#) dst (I# dix#) (I# n#) = ST $ \ s# ->
    case copySmallMutableArray# (unMArray src) six# (unMArray dst) dix# n# s# of
        s2# -> (# s2#, () #)

cloneArray :: Array a -> Int -> Int -> Array a
cloneArray src (I# six#) (I# n#) = Array (cloneSmallArray# (unArray src) six# n#)
{-# INLINE cloneArray #-}  -- to make sure optimization triggers

cloneMutableArray :: MArray s a -> Int -> Int -> ST s (MArray s a)
cloneMutableArray src (I# six#) (I# n#) = ST $ \ s# ->
    case cloneSmallMutableArray# (unMArray src) six# n# s# of
        (# s2#, marr# #) -> (# s2#, MArray marr# #)
{-# INLINE cloneMutableArray #-}  -- to make sure optimization triggers

freezeArray :: MArray s a -> Int -> Int -> ST s (Array a)
freezeArray src (I# six#) (I# n#) = ST $ \ s# ->
    case freezeSmallArray# (unMArray src) six# n# s# of
        (# s2#, arr# #) -> (# s2#, Array arr# #)
{-# INLINE freezeArray #-}  -- to make sure optimization triggers

thawArray :: Array a -> Int -> Int -> ST s (MArray s a)
thawArray src (I# six#) (I# n#) = ST $ \ s# ->
    case thawSmallArray# (unArray src) six# n# s# of
        (# s2#, marr# #) -> (# s2#, MArray marr# #)
{-# INLINE thawArray #-}  -- to make sure optimization triggers

toList :: Array a -> Int -> [a]
toList arr n = go 0
  where
    go i | i >= n = []
         | otherwise = indexArray arr i : go (i+1)
