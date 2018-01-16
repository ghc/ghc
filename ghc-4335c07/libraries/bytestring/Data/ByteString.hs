{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples,
            NamedFieldPuns, BangPatterns #-}
{-# OPTIONS_HADDOCK prune #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005,
--               (c) Bjorn Bringert 2006,
--               (c) Don Stewart 2005-2008,
--               (c) Duncan Coutts 2006-2013
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time and space-efficient implementation of byte vectors using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements. Byte vectors
-- are encoded as strict 'Word8' arrays of bytes, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- The recomended way to assemble ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'ForeignPtr' by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
--

module Data.ByteString (

        -- * The @ByteString@ type
        ByteString,             -- abstract, instances: Eq, Ord, Show, Read, Data, Typeable, Monoid

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        singleton,              -- :: Word8   -> ByteString
        pack,                   -- :: [Word8] -> ByteString
        unpack,                 -- :: ByteString -> [Word8]

        -- * Basic interface
        cons,                   -- :: Word8 -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Word8 -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString
        head,                   -- :: ByteString -> Word8
        uncons,                 -- :: ByteString -> Maybe (Word8, ByteString)
        unsnoc,                 -- :: ByteString -> Maybe (ByteString, Word8)
        last,                   -- :: ByteString -> Word8
        tail,                   -- :: ByteString -> ByteString
        init,                   -- :: ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int

        -- * Transforming ByteStrings
        map,                    -- :: (Word8 -> Word8) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Word8 -> ByteString -> ByteString
        intercalate,            -- :: ByteString -> [ByteString] -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's (folds)
        foldl,                  -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldl',                 -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldl1',                -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

        foldr,                  -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldr',                 -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldr1',                -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Word8 -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        all,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Word8
        minimum,                -- :: ByteString -> Word8

        -- * Building ByteStrings
        -- ** Scans
        scanl,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
        scanl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
        scanr,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
        scanr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
        mapAccumR,              -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)

        -- ** Generating and unfolding ByteStrings
        replicate,              -- :: Int -> Word8 -> ByteString
        unfoldr,                -- :: (a -> Maybe (Word8, a)) -> a -> ByteString
        unfoldrN,               -- :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        span,                   -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,                -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        break,                  -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        breakEnd,               -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]
        stripPrefix,            -- :: ByteString -> ByteString -> Maybe ByteString
        stripSuffix,            -- :: ByteString -> ByteString -> Maybe ByteString

        -- ** Breaking into many substrings
        split,                  -- :: Word8 -> ByteString -> [ByteString]
        splitWith,              -- :: (Word8 -> Bool) -> ByteString -> [ByteString]

        -- * Predicates
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool
        isInfixOf,              -- :: ByteString -> ByteString -> Bool

        -- ** Search for arbitrary substrings
        breakSubstring,         -- :: ByteString -> ByteString -> (ByteString,ByteString)
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,                   -- :: Word8 -> ByteString -> Bool
        notElem,                -- :: Word8 -> ByteString -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Word8 -> Bool) -> ByteString -> Maybe Word8
        filter,                 -- :: (Word8 -> Bool) -> ByteString -> ByteString
        partition,              -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int -> Word8
        elemIndex,              -- :: Word8 -> ByteString -> Maybe Int
        elemIndices,            -- :: Word8 -> ByteString -> [Int]
        elemIndexEnd,           -- :: Word8 -> ByteString -> Maybe Int
        findIndex,              -- :: (Word8 -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Word8 -> Bool) -> ByteString -> [Int]
        count,                  -- :: Word8 -> ByteString -> Int

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Word8,Word8)]
        zipWith,                -- :: (Word8 -> Word8 -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Word8,Word8)] -> (ByteString,ByteString)

        -- * Ordered ByteStrings
        sort,                   -- :: ByteString -> ByteString

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,                   -- :: ByteString -> ByteString

        -- ** Packing 'CString's and pointers
        packCString,            -- :: CString -> IO ByteString
        packCStringLen,         -- :: CStringLen -> IO ByteString

        -- ** Using ByteStrings as 'CString's
        useAsCString,           -- :: ByteString -> (CString    -> IO a) -> IO a
        useAsCStringLen,        -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- * I\/O with 'ByteString's

        -- ** Standard input and output
        getLine,                -- :: IO ByteString
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()
        interact,               -- :: (ByteString -> ByteString) -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()
        appendFile,             -- :: FilePath -> ByteString -> IO ()

        -- ** I\/O with Handles
        hGetLine,               -- :: Handle -> IO ByteString
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hGetSome,               -- :: Handle -> Int -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()
        hPutNonBlocking,        -- :: Handle -> ByteString -> IO ByteString
        hPutStr,                -- :: Handle -> ByteString -> IO ()
        hPutStrLn,              -- :: Handle -> ByteString -> IO ()

        breakByte

  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,maximum
                                ,minimum,all,concatMap,foldl1,foldr1
                                ,scanl,scanl1,scanr,scanr1
                                ,readFile,writeFile,appendFile,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem
#if !MIN_VERSION_base(4,6,0)
                                ,catch
#endif
                                )

#if MIN_VERSION_base(4,7,0)
import Data.Bits                (finiteBitSize, shiftL, (.|.), (.&.))
#else
import Data.Bits                (bitSize, shiftL, (.|.), (.&.))
#endif

import Data.ByteString.Internal
import Data.ByteString.Unsafe

import qualified Data.List as List

import Data.Word                (Word8)
import Data.Maybe               (isJust)

import Control.Exception        (IOException, catch, finally, assert, throwIO)
import Control.Monad            (when)

import Foreign.C.String         (CString, CStringLen)
import Foreign.C.Types          (CSize)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr, touchForeignPtr)
#if MIN_VERSION_base(4,5,0)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr       (unsafeForeignPtrToPtr)
#endif
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Marshal.Array    (allocaArray)
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

-- hGetBuf and hPutBuf not available in yhc or nhc
import System.IO                (stdin,stdout,hClose,hFileSize
                                ,hGetBuf,hPutBuf,hGetBufNonBlocking
                                ,hPutBufNonBlocking,withBinaryFile
                                ,IOMode(..))
import System.IO.Error          (mkIOError, illegalOperationErrorType)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid              (Monoid(..))
#endif

#if MIN_VERSION_base(4,3,0)
import System.IO                (hGetBufSome)
#else
import System.IO                (hWaitForInput, hIsEOF)
#endif

import Data.IORef
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import GHC.IO.Buffer
import GHC.IO.BufferedIO as Buffered
import GHC.IO                   (unsafePerformIO, unsafeDupablePerformIO)
import Data.Char                (ord)
import Foreign.Marshal.Utils    (copyBytes)

import GHC.Prim                 (Word#)
import GHC.Base                 (build)
import GHC.Word hiding (Word8)

#if !(MIN_VERSION_base(4,7,0))
finiteBitSize = bitSize
#endif

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = PS nullForeignPtr 0 0

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton c = unsafeCreate 1 $ \p -> poke p c
{-# INLINE [1] singleton #-}

-- Inline [1] for intercalate rule

--
-- XXX The use of unsafePerformIO in allocating functions (unsafeCreate) is critical!
--
-- Otherwise:
--
--  singleton 255 `compare` singleton 127
--
-- is compiled to:
--
--  case mallocByteString 2 of
--      ForeignPtr f internals ->
--           case writeWord8OffAddr# f 0 255 of _ ->
--           case writeWord8OffAddr# f 0 127 of _ ->
--           case eqAddr# f f of
--                  False -> case compare (GHC.Prim.plusAddr# f 0)
--                                        (GHC.Prim.plusAddr# f 0)
--
--

-- | /O(n)/ Convert a @['Word8']@ into a 'ByteString'.
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck. In such cases, consider using packAddress (GHC only).
pack :: [Word8] -> ByteString
pack = packBytes

-- | /O(n)/ Converts a 'ByteString' to a @['Word8']@.
unpack :: ByteString -> [Word8]
unpack bs = build (unpackFoldr bs)
{-# INLINE unpack #-}

--
-- Have unpack fuse with good list consumers
--
unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr bs k z = foldr k z bs
{-# INLINE [0] unpackFoldr #-}

{-# RULES
"ByteString unpack-list" [1]  forall bs .
    unpackFoldr bs (:) [] = unpackBytes bs
 #-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (PS _ _ l) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

-- ---------------------------------------------------------------------
-- | /O(1)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length (PS _ _ l) = assert (l >= 0) l
{-# INLINE length #-}

------------------------------------------------------------------------

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Word8 -> ByteString -> ByteString
cons c (PS x s l) = unsafeCreate (l+1) $ \p -> withForeignPtr x $ \f -> do
        poke p c
        memcpy (p `plusPtr` 1) (f `plusPtr` s) (fromIntegral l)
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (PS x s l) c = unsafeCreate (l+1) $ \p -> withForeignPtr x $ \f -> do
        memcpy p (f `plusPtr` s) (fromIntegral l)
        poke (p `plusPtr` l) c
{-# INLINE snoc #-}

-- todo fuse

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
head :: ByteString -> Word8
head (PS x s l)
    | l <= 0    = errorEmptyList "head"
    | otherwise = accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
tail :: ByteString -> ByteString
tail (PS p s l)
    | l <= 0    = errorEmptyList "tail"
    | otherwise = PS p (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (PS x s l)
    | l <= 0    = Nothing
    | otherwise = Just (accursedUnutterablePerformIO $ withForeignPtr x
                                                     $ \p -> peekByteOff p s,
                        PS x (s+1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the last element of a ByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ByteString.
last :: ByteString -> Word8
last ps@(PS x s l)
    | null ps   = errorEmptyList "last"
    | otherwise = accursedUnutterablePerformIO $
                    withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
-- An exception will be thrown in the case of an empty ByteString.
init :: ByteString -> ByteString
init ps@(PS p s l)
    | null ps   = errorEmptyList "init"
    | otherwise = PS p s (l-1)
{-# INLINE init #-}

-- | /O(1)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc (PS x s l)
    | l <= 0    = Nothing
    | otherwise = Just (PS x s (l-1),
                        accursedUnutterablePerformIO $
                          withForeignPtr x $ \p -> peekByteOff p (s+l-1))
{-# INLINE unsnoc #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append = mappend
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map f (PS fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \a ->
    create len $ map_ 0 (a `plusPtr` s)
  where
    map_ :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
    map_ !n !p1 !p2
       | n >= len = return ()
       | otherwise = do
            x <- peekByteOff p1 n
            pokeByteOff p2 n (f x)
            map_ (n+1) p1 p2
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse (PS x s l) = unsafeCreate l $ \p -> withForeignPtr x $ \f ->
        c_reverse p (f `plusPtr` s) (fromIntegral l)

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse c ps@(PS x s l)
    | length ps < 2  = ps
    | otherwise      = unsafeCreate (2*l-1) $ \p -> withForeignPtr x $ \f ->
        c_intersperse p (f `plusPtr` s) (fromIntegral l) c

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose ps = P.map pack . List.transpose . P.map unpack $ ps

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
--
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z (PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` (off+len-1)) (p `plusPtr` (off-1))
    where
      -- not tail recursive; traverses array right to left
      go !p !q | p == q    = z
               | otherwise = let !x = accursedUnutterablePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                             in f (go (p `plusPtr` (-1)) q) x
{-# INLINE foldl #-}

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f v (PS fp off len) =
      accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
        go v (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      -- tail recursive; traverses array left to right
      go !z !p !q | p == q    = return z
                  | otherwise = do x <- peek p
                                   go (f z x) (p `plusPtr` 1) q
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z (PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      -- not tail recursive; traverses array left to right
      go !p !q | p == q    = z
               | otherwise = let !x = accursedUnutterablePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                              in k x (go (p `plusPtr` 1) q)
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' k v (PS fp off len) =
      accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
        go v (p `plusPtr` (off+len-1)) (p `plusPtr` (off-1))
    where
      -- tail recursive; traverses array right to left
      go !z !p !q | p == q    = return z
                  | otherwise = do x <- peek p
                                   go (k x z) (p `plusPtr` (-1)) q
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
-- An exception will be thrown in the case of an empty ByteString.
foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f ps
    | null ps   = errorEmptyList "foldl1"
    | otherwise = foldl f (unsafeHead ps) (unsafeTail ps)
{-# INLINE foldl1 #-}

-- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty ByteString.
foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' f ps
    | null ps   = errorEmptyList "foldl1'"
    | otherwise = foldl' f (unsafeHead ps) (unsafeTail ps)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
-- An exception will be thrown in the case of an empty ByteString.
foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f ps
    | null ps        = errorEmptyList "foldr1"
    | otherwise      = foldr f (unsafeLast ps) (unsafeInit ps)
{-# INLINE foldr1 #-}

-- | 'foldr1\'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' f ps
    | null ps        = errorEmptyList "foldr1"
    | otherwise      = foldr' f (unsafeLast ps) (unsafeInit ps)
{-# INLINE foldr1' #-}

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = mconcat

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = concat . foldr ((:) . f) []

-- foldr (append . f) empty

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any _ (PS _ _ 0) = False
any f (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        go !p !q | p == q    = return False
                 | otherwise = do c <- peek p
                                  if f c then return True
                                         else go (p `plusPtr` 1) q
{-# INLINE any #-}

-- todo fuse

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all _ (PS _ _ 0) = True
all f (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        go !p !q | p == q     = return True  -- end of list
                 | otherwise  = do c <- peek p
                                   if f c
                                      then go (p `plusPtr` 1) q
                                      else return False
{-# INLINE all #-}

------------------------------------------------------------------------

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
-- This function will fuse.
-- An exception will be thrown in the case of an empty ByteString.
maximum :: ByteString -> Word8
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
                      c_maximum (p `plusPtr` s) (fromIntegral l)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
-- This function will fuse.
-- An exception will be thrown in the case of an empty ByteString.
minimum :: ByteString -> Word8
minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
                      c_minimum (p `plusPtr` s) (fromIntegral l)
{-# INLINE minimum #-}

------------------------------------------------------------------------

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f acc (PS fp o len) = unsafeDupablePerformIO $ withForeignPtr fp $ \a -> do
    gp   <- mallocByteString len
    acc' <- withForeignPtr gp $ \p -> mapAccumL_ acc 0 (a `plusPtr` o) p
    return (acc', PS gp 0 len)
  where
    mapAccumL_ !s !n !p1 !p2
       | n >= len = return s
       | otherwise = do
            x <- peekByteOff p1 n
            let (s', y) = f s x
            pokeByteOff p2 n y
            mapAccumL_ s' (n+1) p1 p2
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f acc (PS fp o len) = unsafeDupablePerformIO $ withForeignPtr fp $ \a -> do
    gp   <- mallocByteString len
    acc' <- withForeignPtr gp $ \p -> mapAccumR_ acc (len-1) (a `plusPtr` o) p
    return $! (acc', PS gp 0 len)
  where
    mapAccumR_ !s !n !p !q
       | n <  0    = return s
       | otherwise = do
            x  <- peekByteOff p n
            let (s', y) = f s x
            pokeByteOff q n y
            mapAccumR_ s' (n-1) p q
{-# INLINE mapAccumR #-}

-- ---------------------------------------------------------------------
-- Building ByteStrings

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left. This function will fuse.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
--
scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString

scanl f v (PS fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \a ->
    create (len+1) $ \q -> do
        poke q v
        scanl_ v 0 (a `plusPtr` s) (q `plusPtr` 1)
  where
    scanl_ !z !n !p !q
        | n >= len  = return ()
        | otherwise = do
            x <- peekByteOff p n
            let z' = f z x
            pokeByteOff q n z'
            scanl_ z' (n+1) p q
{-# INLINE scanl #-}

    -- n.b. haskell's List scan returns a list one bigger than the
    -- input, so we need to snoc here to get some extra space, however,
    -- it breaks map/up fusion (i.e. scanl . map no longer fuses)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
-- This function will fuse.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 f ps
    | null ps   = empty
    | otherwise = scanl f (unsafeHead ps) (unsafeTail ps)
{-# INLINE scanl1 #-}

-- | scanr is the right-to-left dual of scanl.
scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanr f v (PS fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \a ->
    create (len+1) $ \q -> do
        poke (q `plusPtr` len) v
        scanr_ v (len-1) (a `plusPtr` s) q
  where
    scanr_ !z !n !p !q
        | n <  0    = return ()
        | otherwise = do
            x <- peekByteOff p n
            let z' = f x z
            pokeByteOff q n z'
            scanr_ z' (n-1) p q
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 f ps
    | null ps   = empty
    | otherwise = scanr f (unsafeLast ps) (unsafeInit ps)
{-# INLINE scanr1 #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Word8 -> ByteString
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreate w $ \ptr ->
                      memset ptr c (fromIntegral w) >> return ()

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, Nothing) -> s : []
            (s, Just x') -> s : unfoldChunk n' (n+n') x'
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
unfoldrN i f x0
    | i < 0     = (empty, Just x0)
    | otherwise = unsafePerformIO $ createAndTrim' i $ \p -> go p x0 0
  where
    go !p !x !n
      | n == i    = return (0, n, Just x)
      | otherwise = case f x of
                      Nothing     -> return (0, n, Nothing)
                      Just (w,x') -> do poke p w
                                        go (p `plusPtr` 1) x' (n+1)
{-# INLINE unfoldrN #-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n ps@(PS x s l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop n ps@(PS x s l)
    | n <= 0    = ps
    | n >= l    = empty
    | otherwise = PS x (s+n) (l-n)
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n ps@(PS x s l)
    | n <= 0    = (empty, ps)
    | n >= l    = (ps, empty)
    | otherwise = (PS x s n, PS x (s+n) (l-n))
{-# INLINE splitAt #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f ps = unsafeTake (findIndexOrEnd (not . f) ps) ps
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = unsafeDrop (findIndexOrEnd (not . f) ps) ps
{-# INLINE dropWhile #-}

-- instead of findIndexOrEnd, we could use memchr here.

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
--
-- Under GHC, a rewrite rule will transform break (==) into a
-- call to the specialised breakByte:
--
-- > break ((==) x) = breakByte x
-- > break (==x) = breakByte x
--
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrEnd p ps of n -> (unsafeTake n ps, unsafeDrop n ps)
{-# INLINE [1] break #-}

-- See bytestring #70
#if MIN_VERSION_base(4,9,0)
{-# RULES
"ByteString specialise break (x ==)" forall x.
    break (x `eqWord8`) = breakByte x
"ByteString specialise break (== x)" forall x.
    break (`eqWord8` x) = breakByte x
  #-}
#else
{-# RULES
"ByteString specialise break (x ==)" forall x.
    break (x ==) = breakByte x
"ByteString specialise break (== x)" forall x.
    break (== x) = breakByte x
  #-}
#endif

-- INTERNAL:

-- | 'breakByte' breaks its ByteString argument at the first occurence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (=='c') "abcd" == breakByte 'c' "abcd"
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (unsafeTake n p, unsafeDrop n p)
{-# INLINE breakByte #-}
{-# DEPRECATED breakByte "It is an internal function and should never have been exported. Use 'break (== x)' instead. (There are rewrite rules that handle this special case of 'break'.)" #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'ByteString'
--
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  p ps = splitAt (findFromEndUntil p ps) ps

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p ps = break (not . p) ps
{-# INLINE [1] span #-}

-- | 'spanByte' breaks its ByteString argument at the first
-- occurence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c ps@(PS x s l) =
    accursedUnutterablePerformIO $
      withForeignPtr x $ \p ->
        go (p `plusPtr` s) 0
  where
    go !p !i | i >= l    = return (ps, empty)
             | otherwise = do c' <- peekByteOff p i
                              if c /= c'
                                  then return (unsafeTake i ps, unsafeDrop i ps)
                                  else go p (i+1)
{-# INLINE spanByte #-}

-- See bytestring #70
#if MIN_VERSION_base(4,9,0)
{-# RULES
"ByteString specialise span (x ==)" forall x.
    span (x `eqWord8`) = spanByte x
"ByteString specialise span (== x)" forall x.
    span (`eqWord8` x) = spanByte x
  #-}
#else
{-# RULES
"ByteString specialise span (x ==)" forall x.
    span (x ==) = spanByte x
"ByteString specialise span (== x)" forall x.
    span (== x) = spanByte x
  #-}
#endif

-- | 'spanEnd' behaves like 'span' but from the end of the 'ByteString'.
-- We have
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x)
--
spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd  p ps = splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _pred (PS _  _   0) = []
splitWith pred_ (PS fp off len) = splitWith0 pred# off len fp
  where pred# c# = pred_ (W8# c#)

        splitWith0 !pred' !off' !len' !fp' =
          accursedUnutterablePerformIO $
            withForeignPtr fp $ \p ->
              splitLoop pred' p 0 off' len' fp'

        splitLoop :: (Word# -> Bool)
                  -> Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]

        splitLoop pred' p idx' off' len' fp'
            | idx' >= len'  = return [PS fp' off' idx']
            | otherwise = do
                w <- peekElemOff p (off'+idx')
                if pred' (case w of W8# w# -> w#)
                   then return (PS fp' off' idx' :
                              splitWith0 pred' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop pred' p (idx'+1) off' len' fp'
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteStrings' that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split _ (PS _ _ 0) = []
split w (PS x s l) = loop 0
    where
        loop !n =
            let q = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
                      memchr (p `plusPtr` (s+n))
                             w (fromIntegral (l-n))
            in if q == nullPtr
                then [PS x (s+n) (l-n)]
                else let i = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
                               return (q `minusPtr` (p `plusPtr` s))
                      in PS x (s+n) (i-n) : loop (i+1)

{-# INLINE split #-}


-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than
-- /groupBy (==)/
group :: ByteString -> [ByteString]
group xs
    | null xs   = []
    | otherwise = ys : group zs
    where
        (ys, zs) = spanByte (unsafeHead xs) xs

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k xs
    | null xs   = []
    | otherwise = unsafeTake n xs : groupBy k (unsafeDrop n xs)
    where
        n = 1 + findIndexOrEnd (not . k (unsafeHead xs)) (unsafeTail xs)

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ByteString -> [ByteString] -> ByteString
intercalate s = concat . List.intersperse s
{-# INLINE [1] intercalate #-}

{-# RULES
"ByteString specialise intercalate c -> intercalateByte" forall c s1 s2 .
    intercalate (singleton c) [s1, s2] = intercalateWithByte c s1 s2
  #-}

-- | /O(n)/ intercalateWithByte. An efficient way to join to two ByteStrings
-- with a char. Around 4 times faster than the generalised join.
--
intercalateWithByte :: Word8 -> ByteString -> ByteString -> ByteString
intercalateWithByte c f@(PS ffp s l) g@(PS fgp t m) = unsafeCreate len $ \ptr ->
    withForeignPtr ffp $ \fp ->
    withForeignPtr fgp $ \gp -> do
        memcpy ptr (fp `plusPtr` s) (fromIntegral l)
        poke (ptr `plusPtr` l) c
        memcpy (ptr `plusPtr` (l + 1)) (gp `plusPtr` t) (fromIntegral m)
    where
      len = length f + length g + 1
{-# INLINE intercalateWithByte #-}

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Word8
index ps n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length ps = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length ps))
    | otherwise      = ps `unsafeIndex` n
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \p -> do
    let p' = p `plusPtr` s
    q <- memchr p' c (fromIntegral l)
    return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p'
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs ==
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd ch (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
    go (p `plusPtr` s) (l-1)
  where
    go !p !i | i < 0     = return Nothing
             | otherwise = do ch' <- peekByteOff p i
                              if ch == ch'
                                  then return $ Just i
                                  else go p (i-1)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices w (PS x s l) = loop 0
    where
        loop !n = let q = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
                           memchr (p `plusPtr` (n+s))
                                                w (fromIntegral (l - n))
                  in if q == nullPtr
                        then []
                        else let i = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
                                       return (q `minusPtr` (p `plusPtr` s))
                             in i : loop (i+1)
{-# INLINE elemIndices #-}

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int
count w (PS x s m) = accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
    fmap fromIntegral $ c_count (p `plusPtr` s) (fromIntegral m) w
{-# INLINE count #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex k (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go !ptr !n | n >= l    = return Nothing
               | otherwise = do w <- peek ptr
                                if k w
                                  then return (Just n)
                                  else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices p ps = loop 0 ps
   where
     loop !n !qs | null qs           = []
                 | p (unsafeHead qs) = n : loop (n+1) (unsafeTail qs)
                 | otherwise         =     loop (n+1) (unsafeTail qs)

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem c ps = not (elem c ps)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter k ps@(PS x s l)
    | null ps   = ps
    | otherwise = unsafePerformIO $ createAndTrim l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p (f `plusPtr` (s + l))
        return $! t `minusPtr` p -- actual length
    where
        go !f !t !end | f == end  = return t
                      | otherwise = do
                          w <- peek f
                          if k w
                            then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) end
                            else             go (f `plusPtr` 1) t               end
{-# INLINE filter #-}

{-
--
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single byte. It is more efficient to use
-- /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w
{-# INLINE filterByte #-}

{-# RULES
"ByteString specialise filter (== x)" forall x.
    filter ((==) x) = filterByte x
"ByteString specialise filter (== x)" forall x.
    filter (== x) = filterByte x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f p = case findIndex f p of
                    Just n -> Just (p `unsafeIndex` n)
                    _      -> Nothing
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition f s = unsafeDupablePerformIO $
    do fp' <- mallocByteString len
       withForeignPtr fp' $ \p ->
           do let end = p `plusPtr` (len - 1)
              mid <- sep 0 p end
              rev mid end
              let i = mid `minusPtr` p
              return (PS fp' 0 i,
                      PS fp' i (len - i))
  where
    len  = length s
    incr = (`plusPtr` 1)
    decr = (`plusPtr` (-1))

    sep !i !p1 !p2
       | i == len  = return p1
       | f w       = do poke p1 w
                        sep (i + 1) (incr p1) p2
       | otherwise = do poke p2 w
                        sep (i + 1) p1 (decr p2)
      where
        w = s `unsafeIndex` i

    rev !p1 !p2
      | p1 >= p2  = return ()
      | otherwise = do a <- peek p1
                       b <- peek p2
                       poke p1 b
                       poke p2 a
                       rev (incr p1) (decr p2)

-- --------------------------------------------------------------------
-- Sarching for substrings

-- |/O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- if the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'stripPrefix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 0.10.8.0
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix bs1@(PS _ _ l1) bs2
   | bs1 `isPrefixOf` bs2 = Just (unsafeDrop l1 bs2)
   | otherwise = Nothing

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implemenation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2 `plusPtr` (l2 - l1)) (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'stripSuffix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix bs1@(PS _ _ l1) bs2@(PS _ _ l2)
   | bs1 `isSuffixOf` bs2 = Just (unsafeTake (l2 - l1) bs2)
   | otherwise = Nothing

-- | Check whether one string is a substring of another. @isInfixOf
-- p s@ is equivalent to @not (null (findSubstrings p s))@.
isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf p s = isJust (findSubstring p s)

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- and:
--
-- > findSubstring s l ==
-- >    if null s then Just 0
-- >              else case breakSubstring s l of
-- >                       (x,y) | null y    -> Nothing
-- >                             | otherwise -> Just (length x)
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
--
breakSubstring :: ByteString -- ^ String to search for
               -> ByteString -- ^ String to search in
               -> (ByteString,ByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat =
  case lp of
    0 -> \src -> (empty,src)
    1 -> breakByte (unsafeHead pat)
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
             then shift
             else karpRabin
  where
    unsafeSplitAt i s = (unsafeTake i s, unsafeDrop i s)
    lp                = length pat
    karpRabin :: ByteString -> (ByteString, ByteString)
    karpRabin src
        | length src < lp = (src,empty)
        | otherwise = search (rollingHash $ unsafeTake lp src) lp
      where
        k           = 2891336453 :: Word32
        rollingHash = foldl' (\h b -> h * k + fromIntegral b) 0
        hp          = rollingHash pat
        m           = k ^ lp
        get = fromIntegral . unsafeIndex src
        search !hs !i
            | hp == hs && pat == unsafeTake lp b = u
            | length src <= i                    = (src,empty) -- not found
            | otherwise                          = search hs' (i + 1)
          where
            u@(_, b) = unsafeSplitAt (i - lp) src
            hs' = hs * k +
                  get i -
                  m * get (i - lp)
    {-# INLINE karpRabin #-}

    shift :: ByteString -> (ByteString, ByteString)
    shift !src
        | length src < lp = (src,empty)
        | otherwise       = search (intoWord $ unsafeTake lp src) lp
      where
        intoWord :: ByteString -> Word
        intoWord = foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
        wp   = intoWord pat
        mask = (1 `shiftL` (8 * lp)) - 1
        search !w !i
            | w == wp         = unsafeSplitAt (i - lp) src
            | length src <= i = (src, empty)
            | otherwise       = search w' (i + 1)
          where
            b  = fromIntegral (unsafeIndex src i)
            w' = mask .&. ((w `shiftL` 8) .|. b)
    {-# INLINE shift #-}

-- | Get the first index of a substring in another string,
--   or 'Nothing' if the string is not found.
--   @findSubstring p s@ is equivalent to @listToMaybe (findSubstrings p s)@.
findSubstring :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to seach in.
              -> Maybe Int
findSubstring pat src
    | null pat && null src = Just 0
    | null b = Nothing
    | otherwise = Just (length a)
  where (a, b) = breakSubstring pat src

{-# DEPRECATED findSubstring "findSubstring is deprecated in favour of breakSubstring." #-}

-- | Find the indexes of all (possibly overlapping) occurances of a
-- substring in a string.
--
findSubstrings :: ByteString -- ^ String to search for.
               -> ByteString -- ^ String to seach in.
               -> [Int]
findSubstrings pat src
    | null pat        = [0 .. ls]
    | otherwise       = search 0
  where
    lp = length pat
    ls = length src
    search !n
        | (n > ls - lp) || null b = []
        | otherwise = let k = n + length a
                      in  k : search (k + lp)
      where
        (a, b) = breakSubstring pat (unsafeDrop n src)

{-# DEPRECATED findSubstrings "findSubstrings is deprecated in favour of breakSubstring." #-}

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip ps qs
    | null ps || null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs) : zip (unsafeTail ps) (unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f ps qs
    | null ps || null qs = []
    | otherwise = f (unsafeHead ps) (unsafeHead qs) : zipWith f (unsafeTail ps) (unsafeTail qs)
{-# NOINLINE [1] zipWith #-}

--
-- | A specialised version of zipWith for the common case of a
-- simultaneous map over two bytestrings, to build a 3rd. Rewrite rules
-- are used to automatically covert zipWith into zipWith' when a pack is
-- performed on the result of zipWith.
--
zipWith' :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
zipWith' f (PS fp s l) (PS fq t m) = unsafeDupablePerformIO $
    withForeignPtr fp $ \a ->
    withForeignPtr fq $ \b ->
    create len $ zipWith_ 0 (a `plusPtr` s) (b `plusPtr` t)
  where
    zipWith_ :: Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
    zipWith_ !n !p1 !p2 !r
       | n >= len = return ()
       | otherwise = do
            x <- peekByteOff p1 n
            y <- peekByteOff p2 n
            pokeByteOff r n (f x y)
            zipWith_ (n+1) p1 p2 r

    len = min l m
{-# INLINE zipWith' #-}

{-# RULES
"ByteString specialise zipWith" forall (f :: Word8 -> Word8 -> Word8) p q .
    zipWith f p q = unpack (zipWith' f p q)
  #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Special lists

-- | /O(n)/ Return all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
inits (PS x s l) = [PS x s n | n <- [0..l]]

-- | /O(n)/ Return all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
tails p | null p    = [empty]
        | otherwise = p : tails (unsafeTail p)

-- less efficent spacewise: tails (PS x s l) = [PS x (s+n) (l-n) | n <- [0..l]]

-- ---------------------------------------------------------------------
-- ** Ordered 'ByteString's

-- | /O(n)/ Sort a ByteString efficiently, using counting sort.
sort :: ByteString -> ByteString
sort (PS input s l) = unsafeCreate l $ \p -> allocaArray 256 $ \arr -> do

    _ <- memset (castPtr arr) 0 (256 * fromIntegral (sizeOf (undefined :: CSize)))
    withForeignPtr input (\x -> countOccurrences arr (x `plusPtr` s) l)

    let go 256 !_   = return ()
        go i   !ptr = do n <- peekElemOff arr i
                         when (n /= 0) $ memset ptr (fromIntegral i) n >> return ()
                         go (i + 1) (ptr `plusPtr` fromIntegral n)
    go 0 p
  where
    -- | Count the number of occurrences of each byte.
    -- Used by 'sort'
    --
    countOccurrences :: Ptr CSize -> Ptr Word8 -> Int -> IO ()
    countOccurrences !counts !str !len = go 0
     where
        go !i | i == len    = return ()
              | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                               x <- peekElemOff counts k
                               pokeElemOff counts k (x + 1)
                               go (i + 1)


-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (PS fp o l) action =
 allocaBytes (l+1) $ \buf ->
   withForeignPtr fp $ \p -> do
     memcpy buf (p `plusPtr` o) (fromIntegral l)
     pokeByteOff buf l (0::Word8)
     action (castPtr buf)

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @ByteString@.
-- It must not be stored or used after the subcomputation finishes.
useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen p@(PS _ _ l) f = useAsCString p $ \cstr -> f (cstr,l)

------------------------------------------------------------------------

-- | /O(n)./ Construct a new @ByteString@ from a @CString@. The
-- resulting @ByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
packCString :: CString -> IO ByteString
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ByteString@ from a @CStringLen@. The
-- resulting @ByteString@ is an immutable copy of the original @CStringLen@.
-- The @ByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
packCStringLen :: CStringLen -> IO ByteString
packCStringLen (cstr, len) | len >= 0 = create len $ \p ->
    memcpy p (castPtr cstr) (fromIntegral len)
packCStringLen (_, len) =
    moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

------------------------------------------------------------------------

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
-- This is mainly useful to allow the rest of the data pointed
-- to by the 'ByteString' to be garbage collected, for example
-- if a large string has been read in, and only a small part of it
-- is needed in the rest of the program.
--
copy :: ByteString -> ByteString
copy (PS x s l) = unsafeCreate l $ \p -> withForeignPtr x $ \f ->
    memcpy p (f `plusPtr` s) (fromIntegral l)

-- ---------------------------------------------------------------------
-- Line IO

-- | Read a line from stdin.
getLine :: IO ByteString
getLine = hGetLine stdin

-- | Read a line from a handle

hGetLine :: Handle -> IO ByteString
hGetLine h =
  wantReadableHandle_ "Data.ByteString.hGetLine" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
         then fill h_ buf 0 []
         else haveBuf h_ buf 0 []
 where

  fill h_@Handle__{haByteBuffer,haDevice} buf !len xss = do
    (r,buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss =
    do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if w == off + 1
                            then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                            else writeIORef haByteBuffer buf{ bufL = off + 1 }
                    mkBigPS new_len (xs:xss)
            else fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- find the end-of-line character, if there is one
  findEOL r w raw
        | r == w = return w
        | otherwise =  do
            c <- readWord8Buf raw r
            if c == fromIntegral (ord '\n')
                then return r -- NB. not r+1: don't include the '\n'
                else findEOL (r+1) w raw

mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 create len $ \p ->
   withRawBuffer buf $ \pbuf -> copyBytes p (pbuf `plusPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (P.reverse pss)

-- ---------------------------------------------------------------------
-- Block IO

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut _ (PS _  _ 0) = return ()
hPut h (PS ps s l) = withForeignPtr ps $ \p-> hPutBuf h (p `plusPtr` s) l

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking h bs@(PS ps s l) = do
  bytesWritten <- withForeignPtr ps $ \p-> hPutBufNonBlocking h (p `plusPtr` s) l
  return $! drop bytesWritten bs

-- | A synonym for @hPut@, for compatibility
hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- | Write a ByteString to a handle, appending a newline byte
hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps
    | length ps < 1024 = hPut h (ps `snoc` 0x0a)
    | otherwise        = hPut h ps >> hPut h (singleton (0x0a)) -- don't copy

-- | Write a ByteString to stdout
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | Write a ByteString to stdout, appending a newline byte
putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout

{-# DEPRECATED hPutStrLn
    "Use Data.ByteString.Char8.hPutStrLn instead. (Functions that rely on ASCII encodings belong in Data.ByteString.Char8)"
  #-}
{-# DEPRECATED putStrLn
    "Use Data.ByteString.Char8.putStrLn instead. (Functions that rely on ASCII encodings belong in Data.ByteString.Char8)"
  #-}

------------------------------------------------------------------------
-- Low level IO

-- | Read a 'ByteString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'. First argument is the Handle to read from,
-- and the second is the number of bytes to read. It returns the bytes
-- read, up to n, or 'empty' if EOF has been reached.
--
-- 'hGet' is implemented in terms of 'hGetBuf'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGet' will behave as if EOF was reached.
--
hGet :: Handle -> Int -> IO ByteString
hGet h i
    | i >  0    = createAndTrim i $ \p -> hGetBuf h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGet" i

-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns 'empty'.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hGet'.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking h i
    | i >  0    = createAndTrim i $ \p -> hGetBufNonBlocking h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGetNonBlocking" i

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
--
hGetSome :: Handle -> Int -> IO ByteString
hGetSome hh i
#if MIN_VERSION_base(4,3,0)
    | i >  0    = createAndTrim i $ \p -> hGetBufSome hh p i
#else
    | i >  0    = let
                   loop = do
                     s <- hGetNonBlocking hh i
                     if not (null s)
                        then return s
                        else do eof <- hIsEOF hh
                                if eof then return s
                                       else hWaitForInput hh (-1) >> loop
                                         -- for this to work correctly, the
                                         -- Handle should be in binary mode
                                         -- (see GHC ticket #3808)
                  in loop
#endif
    | i == 0    = return empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []


-- | Read a handle's entire contents strictly into a 'ByteString'.
--
-- This function reads chunks at a time, increasing the chunk size on each
-- read. The final string is then realloced to the appropriate size. For
-- files > half of available memory, this may lead to memory exhaustion.
-- Consider using 'readFile' in this case.
--
-- The Handle is closed once the contents have been read,
-- or if an exception is thrown.
--
hGetContents :: Handle -> IO ByteString
hGetContents hnd = do
    bs <- hGetContentsSizeHint hnd 1024 2048
            `finally` hClose hnd
    -- don't waste too much space for small files:
    if length bs < 900
      then return $! copy bs
      else return bs

hGetContentsSizeHint :: Handle
                     -> Int -- ^ first read size
                     -> Int -- ^ initial buffer size increment
                     -> IO ByteString
hGetContentsSizeHint hnd =
    readChunks []
  where
    readChunks chunks sz sz' = do
      fp        <- mallocByteString sz
      readcount <- withForeignPtr fp $ \buf -> hGetBuf hnd buf sz
      let chunk = PS fp 0 readcount
      -- We rely on the hGetBuf behaviour (not hGetBufSome) where it reads up
      -- to the size we ask for, or EOF. So short reads indicate EOF.
      if readcount < sz && sz > 0
        then return $! concat (P.reverse (chunk : chunks))
        else readChunks (chunk : chunks) sz' ((sz+sz') `min` 32752)
             -- we grow the buffer sizes, but not too huge
             -- we concatenate in the end anyway

-- | getContents. Read stdin strictly. Equivalent to hGetContents stdin
-- The 'Handle' is closed after the contents have been read.
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- | Read an entire file strictly into a 'ByteString'.
--
readFile :: FilePath -> IO ByteString
readFile f =
    withBinaryFile f ReadMode $ \h -> do
      -- hFileSize fails if file is not regular file (like
      -- /dev/null). Catch exception and try reading anyway.
      filesz <- catch (hFileSize h) useZeroIfNotRegularFile
      let readsz = (fromIntegral filesz `max` 0) + 1
      hGetContentsSizeHint h readsz (readsz `max` 255)
      -- Our initial size is one bigger than the file size so that in the
      -- typical case we will read the whole file in one go and not have
      -- to allocate any more chunks. We'll still do the right thing if the
      -- file size is 0 or is changed before we do the read.
  where
    useZeroIfNotRegularFile :: IOException -> IO Integer
    useZeroIfNotRegularFile _ = return 0

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

-- ---------------------------------------------------------------------
-- Internal utilities

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Word8 -> Bool) -> ByteString -> Int
findIndexOrEnd k (PS x s l) =
    accursedUnutterablePerformIO $
      withForeignPtr x $ \f ->
        go (f `plusPtr` s) 0
  where
    go !ptr !n | n >= l    = return l
               | otherwise = do w <- peek ptr
                                if k w
                                  then return n
                                  else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = moduleError fun "empty ByteString"
{-# NOINLINE errorEmptyList #-}

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString." ++ fun ++ ':':' ':msg

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(PS x s l)
  | null ps = 0
  | f (unsafeLast ps) = l
  | otherwise = findFromEndUntil f (PS x s (l - 1))
