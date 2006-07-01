{-# OPTIONS_GHC -cpp -optc-O1 -fffi -fglasgow-exts -fno-warn-incomplete-patterns #-}
--
-- -optc-O2 breaks with 4.0.4 gcc on debian
--
-- Module      : ByteString.Lazy
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

--
-- | A time and space-efficient implementation of lazy byte vectors
-- using lists of packed 'Word8' arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Byte vectors are encoded as lazy lists of strict 'Word8'
-- arrays of bytes. They provide a means to manipulate large byte vectors
-- without requiring the entire vector be resident in memory.
--
-- Some operations, such as concat, append, reverse and cons, have
-- better complexity than their "Data.ByteString" equivalents, as due to
-- optimisations resulting from the list spine structure. And for other
-- operations Lazy ByteStrings are usually within a few percent of
-- strict ones, but with better heap usage. For data larger than the
-- available memory, or if you have tight memory constraints, this
-- module will be the only option. The default chunk size is 64k, which
-- should be good in most circumstances. For people with large L2
-- caches, you may want to increase this to fit your cache.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Polished and extended by Don Stewart.
-- Lazy variant by Duncan Coutts and Don Stewart.
--

module Data.ByteString.Lazy (

        -- * The @ByteString@ type
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        singleton,               -- :: Word8   -> ByteString
        pack,                   -- :: [Word8] -> ByteString
        unpack,                 -- :: ByteString -> [Word8]
        packWith,               -- :: (a -> Word8) -> [a] -> ByteString
        unpackWith,             -- :: (Word8 -> a) -> ByteString -> [a]

        -- * Basic interface
        cons,                   -- :: Word8 -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Word8 -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString
        head,                   -- :: ByteString -> Word8
        last,                   -- :: ByteString -> Word8
        tail,                   -- :: ByteString -> ByteString
        init,                   -- :: ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int64

        -- * Transformating ByteStrings
        map,                    -- :: (Word8 -> Word8) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
--      intersperse,            -- :: Word8 -> ByteString -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's (folds)
        foldl,                  -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldl',                 -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldl1',                -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldr,                  -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

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
--      scanl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
--      scanr,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
--      scanr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString

        -- ** Accumulating maps
        mapAccumL,  -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
        mapIndexed, -- :: (Int64 -> Word8 -> Word8) -> ByteString -> ByteString

        -- ** Infinite ByteStrings
        repeat,                 -- :: Word8 -> ByteString
        replicate,              -- :: Int64 -> Word8 -> ByteString
        cycle,                  -- :: ByteString -> ByteString
        iterate,                -- :: (Word8 -> Word8) -> Word8 -> ByteString

        -- ** Unfolding
        unfoldr,                -- :: (a -> Maybe (Word8, a)) -> a -> ByteString

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int64 -> ByteString -> ByteString
        drop,                   -- :: Int64 -> ByteString -> ByteString
        splitAt,                -- :: Int64 -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        span,                   -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        break,                  -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]

        -- ** Breaking and dropping on specific bytes
        breakByte,              -- :: Word8 -> ByteString -> (ByteString, ByteString)
        spanByte,               -- :: Word8 -> ByteString -> (ByteString, ByteString)

        -- ** Breaking into many substrings
        split,                  -- :: Word8 -> ByteString -> [ByteString]
        splitWith,              -- :: (Word8 -> Bool) -> ByteString -> [ByteString]
        tokens,                 -- :: (Word8 -> Bool) -> ByteString -> [ByteString]

        -- ** Joining strings
        join,                   -- :: ByteString -> [ByteString] -> ByteString
        joinWithByte,           -- :: Word8 -> ByteString -> ByteString -> ByteString

        -- * Predicates
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
--      isSuffixOf,             -- :: ByteString -> ByteString -> Bool

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,                   -- :: Word8 -> ByteString -> Bool
        notElem,                -- :: Word8 -> ByteString -> Bool
        filterByte,             -- :: Word8 -> ByteString -> ByteString
        filterNotByte,          -- :: Word8 -> ByteString -> ByteString

        -- ** Searching with a predicate
        find,                   -- :: (Word8 -> Bool) -> ByteString -> Maybe Word8
        filter,                 -- :: (Word8 -> Bool) -> ByteString -> ByteString
--      partition               -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int64 -> Word8
        elemIndex,              -- :: Word8 -> ByteString -> Maybe Int64
        elemIndices,            -- :: Word8 -> ByteString -> [Int64]
        findIndex,              -- :: (Word8 -> Bool) -> ByteString -> Maybe Int64
        findIndices,            -- :: (Word8 -> Bool) -> ByteString -> [Int64]
        count,                  -- :: Word8 -> ByteString -> Int64

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Word8,Word8)]
        zipWith,                -- :: (Word8 -> Word8 -> c) -> ByteString -> ByteString -> [c]
--      unzip,                  -- :: [(Word8,Word8)] -> (ByteString,ByteString)

        -- * Ordered ByteStrings
--        sort,                   -- :: ByteString -> ByteString

        -- * I\/O with 'ByteString's

        -- ** Standard input and output
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()
        interact,               -- :: (ByteString -> ByteString) -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()
        appendFile,             -- :: FilePath -> ByteString -> IO ()

        -- ** I\/O with Handles
        hGetContents,           -- :: Handle -> IO ByteString
        hGetContentsN,          -- :: Int -> Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hGetN,                  -- :: Int -> Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()
#if defined(__GLASGOW_HASKELL__)
        hGetNonBlocking,        -- :: Handle -> IO ByteString
        hGetNonBlockingN,       -- :: Int -> Handle -> IO ByteString
#endif

  ) where

import qualified Prelude
import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)

import qualified Data.List              as L  -- L for list/lazy
import qualified Data.ByteString        as P  -- P for packed
import qualified Data.ByteString.Base   as P
import qualified Data.ByteString.Fusion as P
import Data.ByteString.Fusion (PairS(..),loopL)

import Data.Monoid              (Monoid(..))

import Data.Word                (Word8)
import Data.Int                 (Int64)
import System.IO (Handle,stdin,stdout,openBinaryFile,IOMode(..),hClose)
import System.IO.Unsafe
import Control.Exception        (bracket)

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data(..), Typeable(..))
#endif

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
newtype ByteString = LPS [P.ByteString] -- LPS for lazy packed string
    deriving (Show,Read
#if defined(__GLASGOW_HASKELL__)
                        ,Data, Typeable
#endif
             )

--
-- hmm, what about getting the PS constructor unpacked into the cons cell?
--
-- data List = Nil | Cons {-# UNPACK #-} !P.ByteString List
--
-- Would avoid one indirection per chunk.
--

unLPS :: ByteString -> [P.ByteString]
unLPS (LPS xs) = xs
{-# INLINE unLPS #-}

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = compareBytes

instance Monoid ByteString where
    mempty  = empty
    mappend = append
    mconcat = concat

------------------------------------------------------------------------

-- XXX
-- The data type invariant:
-- Every ByteString is either empty or consists of non-null ByteStrings.
-- All functions must preserve this, and the QC properties must check this.
--
_invariant :: ByteString -> Bool
_invariant (LPS []) = True
_invariant (LPS xs) = L.all (not . P.null) xs

-- In a form useful for QC testing
_checkInvariant :: ByteString -> ByteString
_checkInvariant lps
    | _invariant lps = lps
    | otherwise      = moduleError "invariant" ("violation: " ++ show lps)

-- The Data abstraction function
--
_abstr :: ByteString -> P.ByteString
_abstr (LPS []) = P.empty
_abstr (LPS xs) = P.concat xs

-- The representation uses lists of packed chunks. When we have to convert from
-- a lazy list to the chunked representation, then by default we'll use this
-- chunk size. Some functions give you more control over the chunk size.
--
-- Measurements here:
--  http://www.cse.unsw.edu.au/~dons/tmp/chunksize_v_cache.png
--
-- indicate that a value around 0.5 to 1 x your L2 cache is best.
-- The following value assumes people have something greater than 128k,
-- and need to share the cache with other programs.
--
defaultChunkSize :: Int
defaultChunkSize = 64 * k
   where k = 1024

smallChunkSize :: Int
smallChunkSize = 4 * k
   where k = 1024

-- defaultChunkSize = 1

------------------------------------------------------------------------

eq :: ByteString -> ByteString -> Bool
eq (LPS xs) (LPS ys) = eq' xs ys
  where eq' [] [] = True
        eq' [] _  = False
        eq' _  [] = False
        eq' (a:as) (b:bs) =
          case compare (P.length a) (P.length b) of
            LT -> a == (P.take (P.length a) b) && eq' as (P.drop (P.length a) b : bs)
            EQ -> a == b                       && eq' as bs
            GT -> (P.take (P.length b) a) == b && eq' (P.drop (P.length b) a : as) bs

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (LPS xs) (LPS ys) = cmp xs ys
  where cmp [] [] = EQ
        cmp [] _  = LT
        cmp _  [] = GT
        cmp (a:as) (b:bs) =
          case compare (P.length a) (P.length b) of
            LT -> case compare a (P.take (P.length a) b) of
                    EQ     -> cmp as (P.drop (P.length a) b : bs)
                    result -> result
            EQ -> case compare a b of
                    EQ     -> cmp as bs
                    result -> result
            GT -> case compare (P.take (P.length b) a) b of
                    EQ     -> cmp (P.drop (P.length b) a : as) bs
                    result -> result

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = LPS []
{-# NOINLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton c = LPS [P.singleton c]
{-# NOINLINE singleton #-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'. 
pack :: [Word8] -> ByteString
pack str = LPS $ L.map P.pack (chunk defaultChunkSize str)

-- ?
chunk :: Int -> [a] -> [[a]]
chunk _    [] = []
chunk size xs = case L.splitAt size xs of (xs', xs'') -> xs' : chunk size xs''

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
unpack :: ByteString -> [Word8]
unpack (LPS ss) = L.concatMap P.unpack ss
{-# INLINE unpack #-}

------------------------------------------------------------------------

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = LPS $ L.map (P.packWith k) (chunk defaultChunkSize str)
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k (LPS ss) = L.concatMap (P.unpackWith k) ss
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (LPS []) = True
null (_)      = False  -- TODO: guarantee this invariant is maintained
{-# INLINE null #-}

-- | /O(n\/c)/ 'length' returns the length of a ByteString as an 'Int64'
length :: ByteString -> Int64
length (LPS ss) = L.sum (L.map (fromIntegral.P.length) ss)

-- avoid the intermediate list?
-- length (LPS ss) = L.foldl lengthF 0 ss
--     where lengthF n s = let m = n + fromIntegral (P.length s) in m `seq` m
{-# INLINE length #-}

-- | /O(1)/ 'cons' is analogous to '(:)' for lists. Unlike '(:)' however it is
-- strict in the ByteString that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons c xs in xs
--
-- You can however use 'repeat' and 'cycle' to build infinite lazy ByteStrings.
--
cons :: Word8 -> ByteString -> ByteString
cons c (LPS (s:ss)) | P.length s <= 16 = LPS (P.cons c s : ss)
cons c (LPS ss)                        = LPS (P.singleton c : ss)
{-# INLINE cons #-}

-- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (LPS ss) c = LPS (ss ++ [P.singleton c])
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Word8
head (LPS [])    = errorEmptyList "head"
head (LPS (x:_)) = P.unsafeHead x
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
tail :: ByteString -> ByteString
tail (LPS [])     = errorEmptyList "tail"
tail (LPS (x:xs))
  | P.length x == 1 = LPS xs
  | otherwise       = LPS (P.unsafeTail x : xs)
{-# INLINE tail #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite and non-empty.
last :: ByteString -> Word8
last (LPS []) = errorEmptyList "last"
last (LPS xs) = P.last (L.last xs)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
init :: ByteString -> ByteString
init (LPS []) = errorEmptyList "init"
init (LPS xs)
    | P.length y == 1 = LPS ys
    | otherwise       = LPS (ys ++ [P.init y])
    where (y,ys) = (L.last xs, L.init xs)
{-# INLINE init #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append (LPS []) (LPS ys) = LPS ys
append (LPS xs) (LPS []) = LPS xs
append (LPS xs) (LPS ys) = LPS (xs ++ ys)
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ByteString -> ByteString
--map f (LPS xs) = LPS (L.map (P.map' f) xs)
map f = LPS . P.loopArr . loopL (P.mapEFL f) P.NoAcc . unLPS
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse (LPS xs) = LPS (L.reverse . L.map P.reverse $ xs)
{-# INLINE reverse #-}

-- The 'intersperse' function takes a 'Word8' and a 'ByteString' and
-- \`intersperses\' that byte between the elements of the 'ByteString'.
-- It is analogous to the intersperse function on Lists.
-- intersperse :: Word8 -> ByteString -> ByteString
-- intersperse = error "FIXME: not yet implemented"

{-
intersperse c (LPS [])     = LPS []
intersperse c (LPS (x:xs)) = LPS (P.intersperse c x : L.map intersperse')
  where intersperse' c ps@(PS x s l) =
          P.create (2*l) $ \p -> withForeignPtr x $ \f ->
                poke p c
                c_intersperse (p `plusPtr` 1) (f `plusPtr` s) l c
-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose s = L.map (\ss -> LPS [P.pack ss]) (L.transpose (L.map unpack s))

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
--foldl f z (LPS xs) = L.foldl (P.foldl f) z xs
foldl f z = P.loopAcc . loopL (P.foldEFL f) z . unLPS
{-# INLINE foldl #-}

-- | 'foldl\'' is like 'foldl', but strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
--foldl' f z (LPS xs) = L.foldl' (P.foldl' f) z xs
foldl' f z = P.loopAcc . loopL (P.foldEFL' f) z . unLPS
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z (LPS xs) = L.foldr (flip (P.foldr k)) z xs
{-# INLINE foldr #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
-- This function is subject to array fusion.
foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 _ (LPS []) = errorEmptyList "foldl1"
foldl1 f (LPS (x:xs)) = foldl f (P.unsafeHead x) (LPS (P.unsafeTail x : xs))

-- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' _ (LPS []) = errorEmptyList "foldl1'"
foldl1' f (LPS (x:xs)) = foldl' f (P.unsafeHead x) (LPS (P.unsafeTail x : xs))

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 _ (LPS []) = errorEmptyList "foldr1"
foldr1 f (LPS ps) = foldr1' ps
  where foldr1' (x:[]) = P.foldr1 f x
        foldr1' (x:xs) = P.foldr  f (foldr1' xs) x

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat lpss = LPS (L.concatMap (\(LPS xs) -> xs) lpss)

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f (LPS lps) = LPS (filterMap (P.concatMap k) lps)
    where
      k w = case f w of LPS xs -> P.concat xs

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any f (LPS xs) = L.or (L.map (P.any f) xs)
-- todo fuse

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all f (LPS xs) = L.and (L.map (P.all f) xs)
-- todo fuse

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Word8
maximum (LPS []) = errorEmptyList "maximum"
maximum (LPS xs) = L.maximum (L.map P.maximum xs)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Word8
minimum (LPS []) = errorEmptyList "minimum"
minimum (LPS xs) = L.minimum (L.map P.minimum xs)
{-# INLINE minimum #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f z = (\(a :*: ps) -> (a, LPS ps)) . loopL (P.mapAccumEFL f) z . unLPS

-- | /O(n)/ map Word8 functions, provided with the index at each position
mapIndexed :: (Int -> Word8 -> Word8) -> ByteString -> ByteString
mapIndexed f = LPS . P.loopArr . loopL (P.mapIndexEFL f) 0 . unLPS

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
scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanl f z ps = LPS . P.loopArr . loopL (P.scanEFL f) z . unLPS $ (ps `snoc` 0)
{-# INLINE scanl #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f = unfoldr (\x -> case f x of x' -> x' `seq` Just (x', x'))

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Word8 -> ByteString
repeat c = LPS (L.repeat block)
    where block =  P.replicate smallChunkSize c

-- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- the value of every element.
--
replicate :: Int64 -> Word8 -> ByteString
replicate w c
    | w <= 0             = empty
    | w < fromIntegral smallChunkSize = LPS [P.replicate (fromIntegral w) c]
    | r == 0             = LPS (L.genericReplicate q s) -- preserve invariant
    | otherwise          = LPS (P.unsafeTake (fromIntegral r) s : L.genericReplicate q s)
 where
    s      = P.replicate smallChunkSize c
    (q, r) = quotRem w (fromIntegral smallChunkSize)

-- | 'cycle' ties a finite ByteString into a circular one, or equivalently,
-- the infinite repetition of the original ByteString.
--
cycle :: ByteString -> ByteString
cycle (LPS []) = errorEmptyList "cycle"
cycle (LPS xs) = LPS (L.cycle xs)

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = LPS . unfoldChunk 32
  where unfoldChunk n x =
          case P.unfoldrN n f x of
            (s, Nothing)
              | P.null s  -> []
              | otherwise -> s : []
            (s, Just x')  -> s : unfoldChunk ((n*2) `min` smallChunkSize) x'

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n\/c)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int64 -> ByteString -> ByteString
take n _ | n < 0 = empty
take i (LPS ps)  = LPS (take' i ps)
  where take' _ []     = []
        take' 0 _      = []
        take' n (x:xs) =
          if n < fromIntegral (P.length x)
            then P.take (fromIntegral n) x : []
            else x : take' (n - fromIntegral (P.length x)) xs

-- | /O(n\/c)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int64 -> ByteString -> ByteString
drop i p | i <= 0 = p
drop i (LPS ps) = LPS (drop' i ps)
  where drop' _ []     = []
        drop' 0 xs     = xs
        drop' n (x:xs) =
          if n < fromIntegral (P.length x)
            then P.drop (fromIntegral n) x : xs
            else drop' (n - fromIntegral (P.length x)) xs

-- | /O(n\/c)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int64 -> ByteString -> (ByteString, ByteString)
splitAt i p        | i <= 0 = (empty, p)
splitAt i (LPS ps) = case splitAt' i ps of (a,b) -> (LPS a, LPS b)
  where splitAt' _ []     = ([], [])
        splitAt' 0 xs     = ([], xs)
        splitAt' n (x:xs) =
          if n < fromIntegral (P.length x)
            then (P.take (fromIntegral n) x : [], 
                  P.drop (fromIntegral n) x : xs)
            else let (xs', xs'') = splitAt' (n - fromIntegral (P.length x)) xs
                   in (x:xs', xs'')


-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f (LPS ps) = LPS (takeWhile' ps)
  where takeWhile' []     = []
        takeWhile' (x:xs) =
          case P.findIndexOrEnd (not . f) x of
            0                  -> []
            n | n < P.length x -> P.take n x : []
              | otherwise      -> x : takeWhile' xs

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f (LPS ps) = LPS (dropWhile' ps)
  where dropWhile' []     = []
        dropWhile' (x:xs) =
          case P.findIndexOrEnd (not . f) x of
            n | n < P.length x -> P.drop n x : xs
              | otherwise      -> dropWhile' xs

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break f (LPS ps) = case (break' ps) of (a,b) -> (LPS a, LPS b)
  where break' []     = ([], [])
        break' (x:xs) =
          case P.findIndexOrEnd f x of
            0                  -> ([], x : xs)
            n | n < P.length x -> (P.take n x : [], P.drop n x : xs)
              | otherwise      -> let (xs', xs'') = break' xs
                                   in (x : xs', xs'')

-- | 'breakByte' breaks its ByteString argument at the first occurence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakByte 'c' "abcd"
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c (LPS ps) = case (breakByte' ps) of (a,b) -> (LPS a, LPS b)
  where breakByte' []     = ([], [])
        breakByte' (x:xs) =
          case P.elemIndex c x of
            Just 0  -> ([], x : xs)
            Just n  -> (P.take n x : [], P.drop n x : xs)
            Nothing -> let (xs', xs'') = breakByte' xs
                        in (x : xs', xs'')

-- | 'spanByte' breaks its ByteString argument at the first
-- occurence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c (LPS ps) = case (spanByte' ps) of (a,b) -> (LPS a, LPS b)
  where spanByte' []     = ([], [])
        spanByte' (x:xs) =
          case P.spanByte c x of
            (x', x'') | P.null x'  -> ([], x : xs)
                      | P.null x'' -> let (xs', xs'') = spanByte' xs
                                       in (x : xs', xs'')
                      | otherwise  -> (x' : [], x'' : xs)

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ (LPS [])     = []
splitWith p (LPS (a:as)) = comb [] (P.splitWith p a) as

  where comb :: [P.ByteString] -> [P.ByteString] -> [P.ByteString] -> [ByteString]
        comb acc (s:[]) []     = LPS (L.reverse (cons' s acc)) : []
        comb acc (s:[]) (x:xs) = comb (cons' s acc) (P.splitWith p x) xs
        comb acc (s:ss) xs     = LPS (L.reverse (cons' s acc)) : comb [] ss xs

        cons' x xs | P.null x  = xs
                   | otherwise = x:xs
        {-# INLINE cons' #-}
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
-- > split 'x'  "x"          == ["",""]
-- 
-- and
--
-- > join [c] . split c == id
-- > split == splitWith . (==)
-- 
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteStrings' that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split _ (LPS [])     = []
split c (LPS (a:as)) = comb [] (P.split c a) as

  where comb :: [P.ByteString] -> [P.ByteString] -> [P.ByteString] -> [ByteString]
        comb acc (s:[]) []     = LPS (L.reverse (cons' s acc)) : []
        comb acc (s:[]) (x:xs) = comb (cons' s acc) (P.split c x) xs
        comb acc (s:ss) xs     = LPS (L.reverse (cons' s acc)) : comb [] ss xs

        cons' x xs | P.null x  = xs
                   | otherwise = x:xs
        {-# INLINE cons' #-}
{-# INLINE split #-}

-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Word8 -> Bool) -> ByteString -> [ByteString]
tokens f = L.filter (not.null) . splitWith f

-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
group :: ByteString -> [ByteString]
group (LPS [])     = []
group (LPS (a:as)) = group' [] (P.group a) as
  where group' :: [P.ByteString] -> [P.ByteString] -> [P.ByteString] -> [ByteString]
        group' acc@(s':_) ss@(s:_) xs
          | P.unsafeHead s'
         /= P.unsafeHead s       = LPS (L.reverse acc) : group' [] ss xs
        group' acc (s:[]) []     = LPS (L.reverse (s : acc)) : []
        group' acc (s:[]) (x:xs) = group' (s:acc) (P.group x) xs
        group' acc (s:ss) xs     = LPS (L.reverse (s : acc)) : group' [] ss xs

{-
TODO: check if something like this might be faster

group :: ByteString -> [ByteString]
group xs
    | null xs   = []
    | otherwise = ys : group zs
    where
        (ys, zs) = spanByte (unsafeHead xs) xs
-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
--
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy _ (LPS [])     = []
groupBy k (LPS (a:as)) = groupBy' [] 0 (P.groupBy k a) as
  where groupBy' :: [P.ByteString] -> Word8 -> [P.ByteString] -> [P.ByteString] -> [ByteString]
        groupBy' acc@(_:_) c ss@(s:_) xs
          | not (c `k` P.unsafeHead s) = LPS (L.reverse acc) : groupBy' [] 0 ss xs
        groupBy' acc _ (s:[]) []       = LPS (L.reverse (s : acc)) : []
        groupBy' []  _ (s:[]) (x:xs)   = groupBy' (s:[]) (P.unsafeHead s) (P.groupBy k x) xs
        groupBy' acc c (s:[]) (x:xs)   = groupBy' (s:acc) c (P.groupBy k x) xs
        groupBy' acc _ (s:ss) xs       = LPS (L.reverse (s : acc)) : groupBy' [] 0 ss xs

{-
TODO: check if something like this might be faster

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k xs
    | null xs   = []
    | otherwise = take n xs : groupBy k (drop n xs)
    where
        n = 1 + findIndexOrEnd (not . k (head xs)) (tail xs)
-}

-- | /O(n)/ The 'join' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
join :: ByteString -> [ByteString] -> ByteString
join s = concat . (L.intersperse s)

-- | /O(n)/ joinWithByte. An efficient way to join to two ByteStrings
-- with a char.
--
joinWithByte :: Word8 -> ByteString -> ByteString -> ByteString
joinWithByte c x y = append x (cons c y)

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(c)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int64 -> Word8
index _        i | i < 0 = moduleError "index" ("negative index: " ++ show i)
index (LPS ps) i         = index' ps i
  where index' []     n = moduleError "index" ("index too large: " ++ show n)
        index' (x:xs) n
          | n >= fromIntegral (P.length x) = 
              index' xs (n - fromIntegral (P.length x))
          | otherwise       = P.unsafeIndex x (fromIntegral n)

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. 
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int64
elemIndex c (LPS ps) = elemIndex' 0 ps
  where elemIndex' _ []     = Nothing
        elemIndex' n (x:xs) =
          case P.elemIndex c x of
            Nothing -> elemIndex' (n + fromIntegral (P.length x)) xs
            Just i  -> Just (n + fromIntegral i)

{-
-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd ch (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
    go (p `plusPtr` s) (l-1)
  where
    STRICT2(go)
    go p i | i < 0     = return Nothing
           | otherwise = do ch' <- peekByteOff p i
                            if ch == ch'
                                then return $ Just i
                                else go p (i-1)
-}
-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int64]
elemIndices c (LPS ps) = elemIndices' 0 ps
  where elemIndices' _ []     = []
        elemIndices' n (x:xs) = L.map ((+n).fromIntegral) (P.elemIndices c x)
                             ++ elemIndices' (n + fromIntegral (P.length x)) xs

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int64
count w (LPS xs) = L.sum (L.map (fromIntegral . P.count w) xs)

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndex k (LPS ps) = findIndex' 0 ps
  where findIndex' _ []     = Nothing
        findIndex' n (x:xs) =
          case P.findIndex k x of
            Nothing -> findIndex' (n + fromIntegral (P.length x)) xs
            Just i  -> Just (n + fromIntegral i)
{-# INLINE findIndex #-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f (LPS ps) = find' ps
  where find' []     = Nothing
        find' (x:xs) = case P.find f x of
            Nothing -> find' xs
            Just w  -> Just w
{-# INLINE find #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
findIndices k (LPS ps) = findIndices' 0 ps
  where findIndices' _ []     = []
        findIndices' n (x:xs) = L.map ((+n).fromIntegral) (P.findIndices k x)
                             ++ findIndices' (n + fromIntegral (P.length x)) xs

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem c ps = not (elem c ps)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
--filter f (LPS xs) = LPS (filterMap (P.filter' f) xs)
filter p = LPS . P.loopArr . loopL (P.filterEFL p) P.NoAcc . unLPS
{-# INLINE filter #-}

-- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- (==)/, for the common case of filtering a single byte. It is more
-- efficient to use /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w
-- filterByte w (LPS xs) = LPS (filterMap (P.filterByte w) xs)

-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single byte out of a list. It is more efficient
-- to use /filterNotByte/ in this case.
--
-- > filterNotByte == filter . (/=)
--
-- filterNotByte is around 2x faster than its filter equivalent.
filterNotByte :: Word8 -> ByteString -> ByteString
filterNotByte w (LPS xs) = LPS (filterMap (P.filterNotByte w) xs)

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (LPS as) (LPS bs) = isPrefixL as bs
  where isPrefixL [] _  = True
        isPrefixL _ []  = False
        isPrefixL (x:xs) (y:ys) | P.length x == P.length y = x == y  && isPrefixL xs ys
                                | P.length x <  P.length y = x == yh && isPrefixL xs (yt:ys)
                                | otherwise                = xh == y && isPrefixL (xt:xs) ys
          where (xh,xt) = P.splitAt (P.length y) x
                (yh,yt) = P.splitAt (P.length x) y

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
-- 
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implemenation uses memcmp to compare the end of the
-- string only, with no reverse required..
--
--isSuffixOf :: ByteString -> ByteString -> Bool
--isSuffixOf = error "not yet implemented"

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip = zipWith (,)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith _ (LPS [])     (LPS _)  = []
zipWith _ (LPS _)      (LPS []) = []
zipWith f (LPS (a:as)) (LPS (b:bs)) = zipWith' a as b bs
  where zipWith' x xs y ys =
          (f (P.unsafeHead x) (P.unsafeHead y) : zipWith'' (P.unsafeTail x) xs (P.unsafeTail y) ys)

        zipWith'' x []      _ _       | P.null x       = []
        zipWith'' _ _       y []      | P.null y       = []
        zipWith'' x xs      y ys      | not (P.null x)
                                     && not (P.null y) = zipWith' x  xs y  ys
        zipWith'' x xs      _ (y':ys) | not (P.null x) = zipWith' x  xs y' ys
        zipWith'' _ (x':xs) y ys      | not (P.null y) = zipWith' x' xs y  ys
        zipWith'' _ (x':xs) _ (y':ys)                  = zipWith' x' xs y' ys

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
{-
unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip _ls = error "not yet implemented"
{-# INLINE unzip #-}
-}

-- ---------------------------------------------------------------------
-- Special lists

-- | /O(n)/ Return all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
inits = (LPS [] :) . inits' . unLPS
  where inits' []     = []
        inits' (x:xs) = L.map (\x' -> LPS [x']) (L.tail (P.inits x))
                     ++ L.map (\(LPS xs') -> LPS (x:xs')) (inits' xs)

-- | /O(n)/ Return all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
tails = tails' . unLPS
  where tails' []           = LPS [] : []
        tails' xs@(x:xs')
          | P.length x == 1 = LPS xs : tails' xs'
          | otherwise       = LPS xs : tails' (P.unsafeTail x : xs')

-- ---------------------------------------------------------------------

-- TODO defrag func that concatenates block together that are below a threshold
-- defrag :: Int -> ByteString -> ByteString

-- ---------------------------------------------------------------------
-- Lazy ByteString IO

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, in @k@-sized chunks.
hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead >>= return . LPS
  where
    lazyRead = unsafeInterleaveIO $ do
        ps <- P.hGet h k
        case P.length ps of
            0         -> return []
            n | n < k -> return [ps]
            _         -> do pss <- lazyRead
                            return (ps : pss)

-- | Read @n@ bytes into a 'ByteString', directly from the
-- specified 'Handle', in chunks of size @k@.
hGetN :: Int -> Handle -> Int -> IO ByteString
hGetN _ _ 0 = return empty
hGetN k h n = readChunks n >>= return . LPS
  where
    STRICT1(readChunks)
    readChunks i = do
        ps <- P.hGet h (min k i)
        case P.length ps of
            0          -> return []
            m | m == i -> return [ps]
            m          -> do pss <- readChunks (i - m)
                             return (ps : pss)

#if defined(__GLASGOW_HASKELL__)
-- | hGetNonBlockingN is similar to 'hGetContentsN', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available. Chunks are read on demand, in @k@-sized chunks.
hGetNonBlockingN :: Int -> Handle -> Int -> IO ByteString
hGetNonBlockingN _ _ 0 = return empty
hGetNonBlockingN k h n = readChunks n >>= return . LPS
  where
    readChunks i = do
        ps <- P.hGetNonBlocking h (min k i)
        case P.length ps of
            0         -> return []
            m | fromIntegral m < i -> return [ps]
            m         -> do pss <- readChunks (i - m)
                            return (ps : pss)
#endif

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, using the default chunk size.
hGetContents :: Handle -> IO ByteString
hGetContents = hGetContentsN defaultChunkSize

-- | Read @n@ bytes into a 'ByteString', directly from the specified 'Handle'.
hGet :: Handle -> Int -> IO ByteString
hGet = hGetN defaultChunkSize

#if defined(__GLASGOW_HASKELL__)
-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking = hGetNonBlockingN defaultChunkSize
#endif


-- | Read an entire file /lazily/ into a 'ByteString'.
readFile :: FilePath -> IO ByteString
readFile f = openBinaryFile f ReadMode >>= hGetContents

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile f txt = bracket (openBinaryFile f WriteMode) hClose
    (\hdl -> hPut hdl txt)

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile f txt = bracket (openBinaryFile f AppendMode) hClose
    (\hdl -> hPut hdl txt)

-- | getContents. Equivalent to hGetContents stdin. Will read /lazily/
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut h (LPS xs) = mapM_ (P.hPut h) xs

-- | Write a ByteString to stdout
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | Write a ByteString to stdout, appending a newline byte
putStrLn :: ByteString -> IO ()
putStrLn ps = hPut stdout ps >> hPut stdout (singleton 0x0a)

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device. It's great for writing one line programs!
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = moduleError fun "empty ByteString"

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.ByteString.Lazy." ++ fun ++ ':':' ':msg)

-- A manually fused version of "filter (not.null) . map f", since they
-- don't seem to fuse themselves. Really helps out filter*, concatMap.
--
-- TODO fuse.
--
filterMap :: (P.ByteString -> P.ByteString) -> [P.ByteString] -> [P.ByteString]
filterMap _ []     = []
filterMap f (x:xs) = case f x of
                    y | P.null y  ->     filterMap f xs      -- manually fuse the invariant filter
                      | otherwise -> y : filterMap f xs
{-# INLINE filterMap #-}

