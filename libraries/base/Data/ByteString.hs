{-# OPTIONS_GHC -cpp -fffi -fglasgow-exts #-}
--
-- Module      : ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005
--               (c) Don Stewart 2005-2006
--               (c) Bjorn Bringert 2006
--
-- Array fusion code:
--               (c) 2001,2002 Manuel M T Chakravarty & Gabriele Keller
--		         (c) 2006      Manuel M T Chakravarty & Roman Leshchinskiy
--
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

--
-- | A time and space-efficient implementation of byte vectors using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements. Byte vectors
-- are encoded as strict Word8 arrays of bytes, held in a ForeignPtr,
-- and can be passed between C and Haskell with little effort.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- > import qualified Data.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Polished and extended by Don Stewart.
--

module Data.ByteString (

        -- * The @ByteString@ type
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        packByte,               -- :: Word8   -> ByteString
        pack,                   -- :: [Word8] -> ByteString
        unpack,                 -- :: ByteString -> [Word8]
        packWith,               -- :: (a -> Word8) -> [a] -> ByteString
        unpackWith,             -- :: (Word8 -> a) -> ByteString -> [a]

        -- * Basic interface
        cons,                   -- :: Word8 -> ByteString -> ByteString
        snoc,                   -- :: Word8 -> ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int
        head,                   -- :: ByteString -> Word8
        tail,                   -- :: ByteString -> ByteString
        last,                   -- :: ByteString -> Word8
        init,                   -- :: ByteString -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString

        -- * Special ByteStrings
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]
        elems,                  -- :: ByteString -> [ByteString]

        -- * Transformating ByteStrings
        map,                    -- :: (Word8 -> Word8) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Word8 -> ByteString -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's
        foldl,                  -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldr,                  -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Word8 -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        all,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Word8
        minimum,                -- :: ByteString -> Word8
        mapIndexed,             -- :: (Int -> Word8 -> Word8) -> ByteString -> ByteString

        -- * Generating and unfolding ByteStrings
        replicate,              -- :: Int -> Word8 -> ByteString
        unfoldrN,               -- :: (Word8 -> Maybe (Word8, Word8)) -> Word8 -> ByteString

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        break,                  -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        span,                   -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,                -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

        -- ** Breaking and dropping on specific bytes
        breakByte,              -- :: Word8 -> ByteString -> (ByteString, ByteString)
        spanByte,               -- :: Word8 -> ByteString -> (ByteString, ByteString)
        breakFirst,             -- :: Word8 -> ByteString -> Maybe (ByteString,ByteString)
        breakLast,              -- :: Word8 -> ByteString -> Maybe (ByteString,ByteString)

        -- ** Breaking into many substrings
        split,                  -- :: Word8 -> ByteString -> [ByteString]
        splitWith,              -- :: (Word8 -> Bool) -> ByteString -> [ByteString]
        tokens,                 -- :: (Word8 -> Bool) -> ByteString -> [ByteString]
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]

        -- ** Joining strings
        join,                   -- :: ByteString -> [ByteString] -> ByteString
        joinWithByte,           -- :: Word8 -> ByteString -> ByteString -> ByteString

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int -> Word8
        elemIndex,              -- :: Word8 -> ByteString -> Maybe Int
        elemIndices,            -- :: Word8 -> ByteString -> [Int]
        elemIndexLast,          -- :: Word8 -> ByteString -> Maybe Int
        findIndex,              -- :: (Word8 -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Word8 -> Bool) -> ByteString -> [Int]
        count,                  -- :: Word8 -> ByteString -> Int

        -- * Ordered ByteStrings
        sort,                   -- :: ByteString -> ByteString

        -- * Searching ByteStrings

        -- ** Searching by equality
        -- | These functions use memchr(3) to efficiently search the ByteString

        elem,                   -- :: Word8 -> ByteString -> Bool
        notElem,                -- :: Word8 -> ByteString -> Bool
        filterByte,             -- :: Word8 -> ByteString -> ByteString
        filterNotByte,          -- :: Word8 -> ByteString -> ByteString

        -- ** Searching with a predicate
        filter,                 -- :: (Word8 -> Bool) -> ByteString -> ByteString
        find,                   -- :: (Word8 -> Bool) -> ByteString -> Maybe Word8

        -- ** Prefixes and suffixes
        -- | These functions use memcmp(3) to efficiently compare substrings
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool

        -- ** Search for arbitrary substrings
        isSubstringOf,          -- :: ByteString -> ByteString -> Bool
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Word8,Word8)]
        zipWith,                -- :: (Word8 -> Word8 -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Word8,Word8)] -> (ByteString,ByteString)

        -- * Unchecked access
        unsafeHead,             -- :: ByteString -> Word8
        unsafeTail,             -- :: ByteString -> ByteString
        unsafeIndex,            -- :: ByteString -> Int -> Word8

        -- * Low level introduction and elimination
        generate,               -- :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> ByteString
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)
        skipIndex,              -- :: ByteString -> Int

        -- ** Packing CStrings and pointers
        packCString,            -- :: CString -> ByteString
        packCStringLen,         -- :: CString -> ByteString
        packMallocCString,      -- :: CString -> ByteString

#if defined(__GLASGOW_HASKELL__)
        packCStringFinalizer,   -- :: Ptr Word8 -> Int -> IO () -> IO ByteString
        packAddress,            -- :: Addr# -> ByteString
        unsafePackAddress,      -- :: Int -> Addr# -> ByteString
        unsafeFinalize,         -- :: ByteString -> IO ()
#endif

        -- ** Using ByteStrings as CStrings
        useAsCString,           -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCString,     -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,  -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- ** Copying ByteStrings
        -- | These functions perform memcpy(3) operations
        copy,                   -- :: ByteString -> ByteString
        copyCString,            -- :: CString -> ByteString
        copyCStringLen,         -- :: CStringLen -> ByteString

        -- * I\/O with @ByteString@s

        -- ** Standard input and output

#if defined(__GLASGOW_HASKELL__)
        getLine,                -- :: IO ByteString
#endif
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()
--      mmapFile,               -- :: FilePath -> IO ByteString

        -- ** I\/O with Handles
#if defined(__GLASGOW_HASKELL__)
        getArgs,                -- :: IO [ByteString]
        hGetLine,               -- :: Handle -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
#endif
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()

        -- * Fusion utilities
#if defined(__GLASGOW_HASKELL__)
        unpackList, -- eek, otherwise it gets thrown away by the simplifier
#endif

        noAL, NoAL, loopArr, loopAcc, loopSndAcc,
        loopU, mapEFL, filterEFL, foldEFL,
        filterF, mapF

  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,maximum
                                ,minimum,all,concatMap,foldl1,foldr1
                                ,readFile,writeFile,replicate
                                ,getContents,getLine,putStr,putStrLn
                                ,zip,zipWith,unzip,notElem)

import qualified Data.List as List

import Data.Char
import Data.Word                (Word8)
import Data.Maybe               (listToMaybe)
import Data.Array               (listArray)
import qualified Data.Array as Array ((!))

-- Control.Exception.bracket not available in yhc or nhc
import Control.Exception        (bracket)
import Control.Monad            (when)

import Foreign.C.String         (CString, CStringLen)
import Foreign.C.Types          (CSize, CInt)
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

-- hGetBuf and hPutBuf not available in yhc or nhc
import System.IO                (stdin,stdout,hClose,hFileSize
                                ,hGetBuf,hPutBuf,openBinaryFile
                                ,Handle,IOMode(..))

#if !defined(__GLASGOW_HASKELL__)
import System.IO.Unsafe
#endif

#if defined(__GLASGOW_HASKELL__)

import Data.Generics            (Data(..), Typeable(..))

import System.IO                (hGetBufNonBlocking)
import System.IO.Error          (isEOFError)

import Foreign.Marshal          (alloca)
import qualified Foreign.Concurrent as FC (newForeignPtr)

import GHC.Handle
import GHC.Prim                 (realWorld#, Addr#, Word#, (+#), writeWord8OffAddr#)
import GHC.Base                 (build, unsafeChr)
import GHC.Word hiding (Word8)
import GHC.Ptr                  (Ptr(..))
import GHC.ST                   (ST(..))
import GHC.IOBase

#endif

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

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
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int

#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = compareBytes

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackWith w2c ps) r

instance Read ByteString where
    readsPrec p str = [ (packWith c2w x, y) | (x, y) <- readsPrec p str ]

{-
instance Arbitrary PackedString where
    arbitrary = P.pack `fmap` arbitrary
    coarbitrary s = coarbitrary (P.unpack s)
-}

-- | /O(n)/ Equality on the 'ByteString' type.
eq :: ByteString -> ByteString -> Bool
eq a@(PS p s l) b@(PS p' s' l')
    | l /= l'            = False    -- short cut on length
    | p == p' && s == s' = True     -- short cut for the same string
    | otherwise          = compareBytes a b == EQ
{-# INLINE eq #-}

-- | /O(n)/ 'compareBytes' provides an 'Ordering' for 'ByteStrings' supporting slices. 
compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0  && l2 == 0               = EQ  -- short cut for empty strings
    | x1 == x2 && s1 == s2 && l1 == l2  = EQ  -- short cut for the same string
    | otherwise                         = inlinePerformIO $
        withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (min l1 l2)
            return $ case i `compare` 0 of
                        EQ  -> l1 `compare` l2
                        x   -> x
{-# INLINE compareBytes #-}

{-
--
-- About 4x slower over 32M
--
compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (PS fp1 off1 len1) (PS fp2 off2 len2) = inlinePerformIO $
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            cmp (p1 `plusPtr` off1)
                (p2 `plusPtr` off2) 0 len1 len2

cmp :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int-> IO Ordering
STRICT5(cmp)
cmp p1 p2 n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          (a :: Word8) <- peekByteOff p1 n
          (b :: Word8) <- peekByteOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1) len1 len2
                LT -> return LT
                GT -> return GT
{-# INLINE compareBytes #-}
-}

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = inlinePerformIO $ mallocByteString 1 >>= \fp -> return $ PS fp 0 0
{-# NOINLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
packByte :: Word8 -> ByteString
packByte c = inlinePerformIO $ mallocByteString 2 >>= \fp -> do
    withForeignPtr fp $ \p -> poke p c
    return $ PS fp 0 1
{-# NOINLINE packByte #-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'. 
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck. In such cases, consider using packAddress (GHC only).
pack :: [Word8] -> ByteString

#if !defined(__GLASGOW_HASKELL__)

pack str = create (P.length str) $ \p -> go p str
    where
        go _ []     = return ()
        go p (x:xs) = poke p x >> go (p `plusPtr` 1) xs -- less space than pokeElemOff

#else /* hack away */

pack str = create (P.length str) $ \(Ptr p) -> stToIO (go p 0# str)
    where
        go _ _ []        = return ()
        go p i (W8# c:cs) = writeByte p i c >> go p (i +# 1#) cs

        writeByte p i c = ST $ \s# ->
            case writeWord8OffAddr# p i c s# of s2# -> (# s2#, () #)

#endif

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
unpack :: ByteString -> [Word8]

#if !defined(__GLASGOW_HASKELL__)

unpack (PS _  _ 0) = []
unpack (PS ps s l) = inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = peek p          >>= \e -> return (e : acc)
        go p n acc = peekByteOff p n >>= \e -> go p (n-1) (e : acc)
{-# INLINE unpack #-}

#else

unpack ps = build (unpackFoldr ps)
{-# INLINE unpack #-}

unpackList :: ByteString -> [Word8]
unpackList (PS fp off len) = withPtr fp $ \p -> do
    let STRICT3(loop)
        loop _ (-1) acc = return acc
        loop q n acc = do
           a <- peekByteOff q n
           loop q (n-1) (a : acc)
    loop (p `plusPtr` off) (len-1) []

{-# RULES
"unpack-list"  [1]  forall p  . unpackFoldr p (:) [] = unpackList p
 #-}

unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr (PS fp off len) f ch = withPtr fp $ \p -> do
    let STRICT3(loop)
        loop _ (-1) acc = return acc
        loop q n    acc = do
           a <- peekByteOff q n
           loop q (n-1) (a `f` acc)
    loop (p `plusPtr` off) (len-1) ch
{-# INLINE [0] unpackFoldr #-}

#endif

------------------------------------------------------------------------

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = create (P.length str) $ \p -> go p str
    where
        STRICT2(go)
        go _ []     = return ()
        go p (x:xs) = poke p (k x) >> go (p `plusPtr` 1) xs -- less space than pokeElemOff
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith _ (PS _  _ 0) = []
unpackWith k (PS ps s l) = inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = peek p          >>= \e -> return (k e : acc)
        go p n acc = peekByteOff p n >>= \e -> go p (n-1) (k e : acc)
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (PS _ _ l) = l == 0
{-# INLINE null #-}

-- | /O(1)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length (PS _ _ l) = l
{-# INLINE length #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Word8 -> ByteString -> ByteString
cons c (PS x s l) = create (l+1) $ \p -> withForeignPtr x $ \f -> do
        memcpy (p `plusPtr` 1) (f `plusPtr` s) l
        poke p c
{-# INLINE cons #-}

-- todo fuse

-- | /O(n)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (PS x s l) c = create (l+1) $ \p -> withForeignPtr x $ \f -> do
        memcpy p (f `plusPtr` s) l
        poke (p `plusPtr` l) c
{-# INLINE snoc #-}

-- todo fuse

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Word8
head ps@(PS x s _)
    | null ps   = errorEmptyList "head"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
tail :: ByteString -> ByteString
tail (PS p s l)
    | l <= 0    = errorEmptyList "tail"
    | otherwise = PS p (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the last element of a ByteString, which must be finite and non-empty.
last :: ByteString -> Word8
last ps@(PS x s l)
    | null ps   = errorEmptyList "last"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
init :: ByteString -> ByteString
init (PS p s l)
    | l <= 0    = errorEmptyList "init"
    | otherwise = PS p s (l-1)
{-# INLINE init #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append xs ys | null xs   = ys
             | null ys   = xs
             | otherwise = concat [xs,ys]
{-# INLINE append #-}

{-
--
-- About 30% faster, but allocating in a big chunk isn't good for memory use
--
append :: ByteString -> ByteString -> ByteString
append xs@(PS ffp s l) ys@(PS fgp t m)
    | null xs   = ys
    | null ys   = xs
    | otherwise = create len $ \ptr ->
        withForeignPtr ffp $ \fp ->
        withForeignPtr fgp $ \gp -> do
            memcpy ptr               (fp `plusPtr` s) l
            memcpy (ptr `plusPtr` l) (gp `plusPtr` t) m
        where len = length xs + length ys
-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@. This function is subject to array fusion.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map f = loopArr . loopU (mapEFL f) noAL
{-# INLINE map #-}

-- | /O(n)/ Like 'map', but not fuseable. The benefit is that it is
-- slightly faster for one-shot cases.
mapF :: (Word8 -> Word8) -> ByteString -> ByteString
STRICT2(mapF)
mapF f (PS fp s len) = inlinePerformIO $ withForeignPtr fp $ \a -> do
    np <- mallocByteString (len+1)
    withForeignPtr np $ \p -> do
        map_ 0 (a `plusPtr` s) p
        return (PS np 0 len)
  where
    map_ :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
    STRICT3(map_)
    map_ n p1 p2
       | n >= len = return ()
       | otherwise = do
            x <- peekByteOff p1 n
            pokeByteOff p2 n (f x)
            map_ (n+1) p1 p2
{-# INLINE mapF #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse (PS x s l) = create l $ \p -> withForeignPtr x $ \f ->
        c_reverse p (f `plusPtr` s) l

{-
reverse = pack . P.reverse . unpack
-}

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse c ps@(PS x s l)
    | length ps < 2  = ps
    | otherwise      = create (2*l-1) $ \p -> withForeignPtr x $ \f ->
        c_intersperse p (f `plusPtr` s) l c

{-
intersperse c = pack . List.intersperse c . unpack
-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose ps = P.map pack (List.transpose (P.map unpack ps))

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
-- This function is subject to array fusion.
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z = loopAcc . loopU (foldEFL f) z
{-# INLINE foldl #-}

{-
--
-- About twice as fast with 6.4.1, but not fuseable
-- A simple fold . map is enough to make it worth while.
--
foldl f v (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        lgo v (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT3(lgo)
        lgo z p q | p == q    = return z
                  | otherwise = do c <- peek p
                                   lgo (f z c) (p `plusPtr` 1) q
-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT2(go)
        go p q | p == q    = return z
               | otherwise = do c  <- peek p
                                ws <- go (p `plusPtr` 1) q
                                return $ c `k` ws

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
-- This function is subject to array fusion.
foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f ps
    | null ps   = errorEmptyList "foldl1"
    | otherwise = foldl f (unsafeHead ps) (unsafeTail ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f ps
    | null ps        = errorEmptyList "foldr1"
    | otherwise      = foldr f (last ps) (init ps)

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat []     = empty
concat [ps]   = ps
concat xs     = inlinePerformIO $ do
    let start_size = 1024
    p <- mallocArray start_size
    f p 0 1024 xs

    where f ptr len _ [] = do
                ptr' <- reallocArray ptr (len+1)
                poke (ptr' `plusPtr` len) (0::Word8)    -- XXX so CStrings work
                fp   <- newForeignFreePtr ptr'
                return $ PS fp 0 len

          f ptr len to_go pss@(PS p s l:pss')
           | l <= to_go = do withForeignPtr p $ \pf ->
                                 memcpy (ptr `plusPtr` len)
                                          (pf `plusPtr` s) l
                             f ptr (len + l) (to_go - l) pss'

           | otherwise = do let new_total = ((len + to_go) * 2) `max` (len + l)
                            ptr' <- reallocArray ptr new_total
                            f ptr' len (new_total - len) pss

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = foldr (append . f) empty

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any _ (PS _ _ 0) = False
any f (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT2(go)
        go p q | p == q    = return False
               | otherwise = do c <- peek p
                                if f c then return True
                                       else go (p `plusPtr` 1) q

-- todo fuse

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all _ (PS _ _ 0) = True
all f (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT2(go)
        go p q | p == q     = return True  -- end of list
               | otherwise  = do c <- peek p
                                 if f c
                                    then go (p `plusPtr` 1) q
                                    else return False
-- todo fuse

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Word8
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
                    return $ c_maximum (p `plusPtr` s) l
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Word8
minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
                    return $ c_minimum (p `plusPtr` s) l
{-# INLINE minimum #-}

-- fusion is too slow here (10x)

{-
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> do
                        w <- peek p
                        maximum_ (p `plusPtr` s) 0 l w

maximum_ :: Ptr Word8 -> Int -> Int -> Word8 -> IO Word8
STRICT4(maximum_)
maximum_ ptr n m c
    | n >= m    = return c
    | otherwise = do w <- peekByteOff ptr n
                     maximum_ ptr (n+1) m (if w > c then w else c)

minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> do
                        w <- peek p
                        minimum_ (p `plusPtr` s) 0 l w

minimum_ :: Ptr Word8 -> Int -> Int -> Word8 -> IO Word8
STRICT4(minimum_)
minimum_ ptr n m c
    | n >= m    = return c
    | otherwise = do w <- peekByteOff ptr n
                     minimum_ ptr (n+1) m (if w < c then w else c)
-}

-- | /O(n)/ map Word8 functions, provided with the index at each position
mapIndexed :: (Int -> Word8 -> Word8) -> ByteString -> ByteString
mapIndexed k (PS ps s l) = create l $ \p -> withForeignPtr ps $ \f ->
    go 0 (f `plusPtr` s) p (f `plusPtr` s `plusPtr` l)
  where
    go :: Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
    STRICT4(go)
    go n f t p | f == p    = return ()
               | otherwise = do w <- peek f
                                ((poke t) . k n) w
                                go (n+1) (f `plusPtr` 1) (t `plusPtr` 1) p

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Word8 -> ByteString
replicate w c = create w $ \ptr -> memset ptr c (fromIntegral w) >> return ()

{-
-- About 5x slower
replicate w c = inlinePerformIO $ generate w $ \ptr -> go ptr w
    where
        STRICT2(go)
        go _   0 = return w
        go ptr n = poke ptr c >> go (ptr `plusPtr` 1) (n-1)
-}

-- | /O(n)/ The 'unfoldrN' function is analogous to the List \'unfoldr\'.
-- 'unfoldrN' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
--
-- To preven unfoldrN having /O(n^2)/ complexity (as prepending a
-- character to a ByteString is /O(n)/, this unfoldr requires a maximum
-- final size of the ByteString as an argument. 'cons' can then be
-- implemented in /O(1)/ (i.e.  a 'poke'), and the unfoldr itself has
-- linear complexity. The depth of the recursion is limited to this
-- size, but may be less. For lazy, infinite unfoldr, use
-- 'Data.List.unfoldr' (from 'Data.List').
--
-- Examples:
--
-- > unfoldrN 10 (\x -> Just (x, chr (ord x + 1))) '0' == "0123456789"
--
-- The following equation connects the depth-limited unfoldr to the List unfoldr:
--
-- > unfoldrN n == take n $ List.unfoldr
unfoldrN :: Int -> (Word8 -> Maybe (Word8, Word8)) -> Word8 -> ByteString
unfoldrN i f w = inlinePerformIO $ generate i $ \p -> go p w 0
    where
        STRICT3(go)
        go q c n | n == i    = return n      -- stop if we reach `i'
                 | otherwise = case f c of
                                   Nothing        -> return n
                                   Just (a,new_c) -> do
                                        poke q a
                                        go (q `plusPtr` 1) new_c (n+1)

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n ps@(PS x s l)
    | n < 0     = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop n ps@(PS x s l)
    | n <= 0    = ps
    | n >  l    = empty
    | otherwise = PS x (s+n) (l-n)
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt  n ps  = (take n ps, drop n ps)
{-# INLINE splitAt #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f ps = take (findIndexOrEnd (not . f) ps) ps
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = drop (findIndexOrEnd (not . f) ps) ps
{-# INLINE dropWhile #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrEnd p ps of n -> (take n ps, drop n ps)
{-# INLINE break #-}

-- | 'breakByte' breaks its ByteString argument at the first occurence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakByte 'c' "abcd"
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (take n p, drop n p)
{-# INLINE breakByte #-}

-- | 'spanByte' breaks its ByteString argument at the first
-- occurence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c ps@(PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
    go (p `plusPtr` s) 0
  where
    STRICT2(go)
    go p i | i >= l    = return (ps, empty)
           | otherwise = do c' <- peekByteOff p i
                            if c /= c'
                                then return (take i ps, drop i ps)
                                else go p (i+1)
{-# INLINE spanByte #-}

-- | /O(n)/ 'breakFirst' breaks the given ByteString on the first
-- occurence of @w@. It behaves like 'break', except the delimiter is
-- not returned, and @Nothing@ is returned if the delimiter is not in
-- the ByteString. I.e.
--
-- > breakFirst 'b' "aabbcc" == Just ("aa","bcc")
--
-- > breakFirst c xs ==
-- > let (x,y) = break (== c) xs 
-- > in if null y then Nothing else Just (x, drop 1 y))
--
breakFirst :: Word8 -> ByteString -> Maybe (ByteString,ByteString)
breakFirst c p = case elemIndex c p of
   Nothing -> Nothing
   Just n -> Just (take n p, drop (n+1) p)
{-# INLINE breakFirst #-}

-- | /O(n)/ 'breakLast' behaves like breakFirst, but from the end of the
-- ByteString.
--
-- > breakLast ('b') (pack "aabbcc") == Just ("aab","cc")
--
-- and the following are equivalent:
--
-- > breakLast 'c' "abcdef"
-- > let (x,y) = break (=='c') (reverse "abcdef") 
-- > in if null x then Nothing else Just (reverse (drop 1 y), reverse x)
--
breakLast :: Word8 -> ByteString -> Maybe (ByteString,ByteString)
breakLast c p = case elemIndexLast c p of
    Nothing -> Nothing
    Just n -> Just (take n p, drop (n+1) p)
{-# INLINE breakLast #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p ps = break (not . p) ps
{-# INLINE span #-}

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

#if defined(__GLASGOW_HASKELL__)
splitWith _pred (PS _  _   0) = []
splitWith pred_ (PS fp off len) = splitWith' pred# off len fp
  where pred# c# = pred_ (W8# c#)

        splitWith' pred' off' len' fp' = withPtr fp $ \p ->
            splitLoop pred' p 0 off' len' fp'

        splitLoop :: (Word# -> Bool)
                  -> Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]

        splitLoop pred' p idx' off' len' fp'
            | pred' `seq` p `seq` idx' `seq` off' `seq` len' `seq` fp' `seq` False = undefined
            | idx' >= len'  = return [PS fp' off' idx']
            | otherwise = do
                w <- peekElemOff p (off'+idx')
                if pred' (case w of W8# w# -> w#)
                   then return (PS fp' off' idx' :
                              splitWith' pred' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop pred' p (idx'+1) off' len' fp'
{-# INLINE splitWith #-}

#else
splitWith _ (PS _ _ 0) = []
splitWith p ps = splitWith' p ps
    where
        STRICT2(splitWith')
        splitWith' q qs = if null rest then [chunk]
                                       else chunk : splitWith' q (unsafeTail rest)
            where (chunk,rest) = break q qs
#endif

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
split _ (PS _ _ 0) = []
split w (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    let ptr = p `plusPtr` s

        STRICT1(loop)
        loop n = do
            let q = memchr (ptr `plusPtr` n) w (fromIntegral (l-n))
            if q == nullPtr
                then return [PS x (s+n) (l-n)]
                else do let i = q `minusPtr` ptr
                        ls <- loop (i+1)
                        return $! PS x (s+n) (i-n) : ls
    loop 0
{-# INLINE split #-}

{-
-- slower. but stays inside Haskell.
split _ (PS _  _   0) = []
split (W8# w#) (PS fp off len) = splitWith' off len fp
    where
        splitWith' off' len' fp' = withPtr fp $ \p ->
            splitLoop p 0 off' len' fp'

        splitLoop :: Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]

        STRICT5(splitLoop)
        splitLoop p idx' off' len' fp'
            | p `seq` idx' `seq` off' `seq` len' `seq` fp' `seq` False = undefined
            | idx' >= len'  = return [PS fp' off' idx']
            | otherwise = do
                (W8# x#) <- peekElemOff p (off'+idx')
                if word2Int# w# ==# word2Int# x#
                   then return (PS fp' off' idx' :
                              splitWith' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop p (idx'+1) off' len' fp'
-}

-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Word8 -> Bool) -> ByteString -> [ByteString]
tokens f = P.filter (not.null) . splitWith f

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
    | otherwise = take n xs : groupBy k (drop n xs)
    where
        n = 1 + findIndexOrEnd (not . k (unsafeHead xs)) (unsafeTail xs)

-- | /O(n)/ The 'join' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
join :: ByteString -> [ByteString] -> ByteString
join filler pss = concat (splice pss)
    where
        splice []  = []
        splice [x] = [x]
        splice (x:y:xs) = x:filler:splice (y:xs)

--
-- | /O(n)/ joinWithByte. An efficient way to join to two ByteStrings
-- with a char. Around 4 times faster than the generalised join.
--
joinWithByte :: Word8 -> ByteString -> ByteString -> ByteString
joinWithByte c f@(PS ffp s l) g@(PS fgp t m) = create len $ \ptr ->
    withForeignPtr ffp $ \fp ->
    withForeignPtr fgp $ \gp -> do
        memcpy ptr (fp `plusPtr` s) l
        poke (ptr `plusPtr` l) c
        memcpy (ptr `plusPtr` (l + 1)) (gp `plusPtr` t) m
    where
      len = length f + length g + 1
{-# INLINE joinWithByte #-}

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Word8
index ps n
    | n < 0          = error $ "ByteString.indexWord8: negative index: " ++ show n
    | n >= length ps = error $ "ByteString.indexWord8: index too large: " ++ show n
                                ++ ", length = " ++ show (length ps)
    | otherwise      = ps `unsafeIndex` n
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. 
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    let p' = p `plusPtr` s
        q  = memchr p' c (fromIntegral l)
    return $ if q == nullPtr then Nothing else Just $! q `minusPtr` p'
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexLast' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexLast c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexLast :: Word8 -> ByteString -> Maybe Int
elemIndexLast ch (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
    go (p `plusPtr` s) (l-1)
  where
    STRICT2(go)
    go p i | i < 0     = return Nothing
           | otherwise = do ch' <- peekByteOff p i
                            if ch == ch'
                                then return $ Just i
                                else go p (i-1)
{-# INLINE elemIndexLast #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices w (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    let ptr = p `plusPtr` s

        STRICT1(loop)
        loop n = do
                let q = memchr (ptr `plusPtr` n) w (fromIntegral (l - n))
                if q == nullPtr
                    then return []
                    else do let i = q `minusPtr` ptr
                            ls <- loop (i+1)
                            return $! i:ls
    loop 0

{-
-- much slower
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices c ps = loop 0 ps
   where STRICT2(loop)
         loop _ ps' | null ps'            = []
         loop n ps' | c == unsafeHead ps' = n : loop (n+1) (unsafeTail ps')
                    | otherwise           = loop (n+1) (unsafeTail ps')
-}

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int
count w (PS x s m) = inlinePerformIO $ withForeignPtr x $ \p ->
    return $ c_count (p `plusPtr` s) (fromIntegral m) w
{-# INLINE count #-}

{-
--
-- around 30% slower
--
count w (PS x s m) = inlinePerformIO $ withForeignPtr x $ \p ->
     go (p `plusPtr` s) (fromIntegral m) 0
    where
        go :: Ptr Word8 -> CSize -> Int -> IO Int
        STRICT3(go)
        go p l i = do
            let q = memchr p w l
            if q == nullPtr
                then return i
                else do let k = fromIntegral $ q `minusPtr` p
                        go (q `plusPtr` 1) (l-k-1) (i+1)
-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex = (listToMaybe .) . findIndices

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices p ps = loop 0 ps
   where
     STRICT2(loop)
     loop _ qs | null qs           = []
     loop n qs | p (unsafeHead qs) = n : loop (n+1) (unsafeTail qs)
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

--
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single byte. It is more efficient to use
-- /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w

--
-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single byte out of a list. It is more efficient
-- to use /filterNotByte/ in this case.
--
-- > filterNotByte == filter . (/=)
--
-- filterNotByte is around 3x faster, and uses much less space, than its
-- filter equivalent
filterNotByte :: Word8 -> ByteString -> ByteString
filterNotByte ch ps@(PS x s l)
    | null ps   = ps
    | otherwise = inlinePerformIO $ generate l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p l
        return (t `minusPtr` p) -- actual length
    where
        STRICT3(go)
        go _ t 0 = return t
        go f t e = do w <- peek f
                      if w /= ch
                        then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) (e-1)
                        else             go (f `plusPtr` 1) t               (e-1)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate. This function is subject to array fusion.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter p  = loopArr . loopU (filterEFL p) noAL
{-# INLINE filter #-}

filterF :: (Word8 -> Bool) -> ByteString -> ByteString
filterF k ps@(PS x s l)
    | null ps   = ps
    | otherwise = inlinePerformIO $ generate l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p l
        return (t `minusPtr` p) -- actual length
    where
        STRICT3(go)
        go _ t 0 = return t
        go f t e = do w <- peek f
                      if k w
                        then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) (e - 1)
                        else             go (f `plusPtr` 1) t               (e - 1)
{-# INLINE filterF #-}

-- Almost as good: pack $ foldl (\xs c -> if f c then c : xs else xs) [] ps

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find p ps = case filter p ps of
    q | null q -> Nothing
      | otherwise -> Just (unsafeHead q)

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = inlinePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) l1
            return (i == 0)

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
    | otherwise = inlinePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2 `plusPtr` (l2 - l1)) l1
            return (i == 0)

-- | Check whether one string is a substring of another. @isSubstringOf
-- p s@ is equivalent to @not (null (findSubstrings p s))@.
isSubstringOf :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to search in.
              -> Bool
isSubstringOf p s = not $ P.null $ findSubstrings p s

-- | Get the first index of a substring in another string,
--   or 'Nothing' if the string is not found.
--   @findSubstring p s@ is equivalent to @listToMaybe (findSubstrings p s)@.
findSubstring :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to seach in.
              -> Maybe Int
findSubstring = (listToMaybe .) . findSubstrings

-- | Find the indexes of all (possibly overlapping) occurances of a
-- substring in a string.  This function uses the Knuth-Morris-Pratt
-- string matching algorithm.
findSubstrings :: ByteString -- ^ String to search for.
               -> ByteString -- ^ String to seach in.
               -> [Int]

findSubstrings pat@(PS _ _ m) str@(PS _ _ n) = search 0 0
  where
      patc x = pat `unsafeIndex` x
      strc x = str `unsafeIndex` x

      -- maybe we should make kmpNext a UArray before using it in search?
      kmpNext = listArray (0,m) (-1:kmpNextL pat (-1))
      kmpNextL p _ | null p = []
      kmpNextL p j = let j' = next (unsafeHead p) j + 1
                         ps = unsafeTail p
                         x = if not (null ps) && unsafeHead ps == patc j'
                                then kmpNext Array.! j' else j'
                        in x:kmpNextL ps j'
      search i j = match ++ rest -- i: position in string, j: position in pattern
        where match = if j == m then [(i - j)] else []
              rest = if i == n then [] else search (i+1) (next (strc i) j + 1)
      next c j | j >= 0 && (j == m || c /= patc j) = next c (kmpNext Array.! j)
               | otherwise = j

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

-- | /O(n)/ breaks a ByteString to a list of ByteStrings, one byte each.
elems :: ByteString -> [ByteString]
elems (PS _ _ 0) = []
elems (PS x s l) = (PS x s 1:elems (PS x (s+1) (l-1)))
{-# INLINE elems #-}

-- ---------------------------------------------------------------------
-- ** Ordered 'ByteString's

-- | /O(n)/ Sort a ByteString efficiently, using counting sort.
sort :: ByteString -> ByteString
sort (PS input s l) = create l $ \p -> allocaArray 256 $ \arr -> do

    memset (castPtr arr) 0 (256 * fromIntegral (sizeOf (undefined :: CSize)))
    withForeignPtr input (\x -> countEach arr (x `plusPtr` s) l)

    let STRICT2(go)
        go 256 _   = return ()
        go i   ptr = do n <- peekElemOff arr i
                        when (n /= 0) $ memset ptr (fromIntegral i) n >> return ()
                        go (i + 1) (ptr `plusPtr` (fromIntegral n))
    go 0 p

-- "countEach counts str l" counts the number of occurences of each Word8 in
-- str, and stores the result in counts.
countEach :: Ptr CSize -> Ptr Word8 -> Int -> IO ()
STRICT3(countEach)
countEach counts str l = go 0
 where
    STRICT1(go)
    go i | i == l    = return ()
         | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                          x <- peekElemOff counts k
                          pokeElemOff counts k (x + 1)
                          go (i + 1)

{-
sort :: ByteString -> ByteString
sort (PS x s l) = create l $ \p -> withForeignPtr x $ \f -> do
        memcpy p (f `plusPtr` s) l
        c_qsort p l -- inplace
-}

{-
sort = pack . List.sort . unpack
-}

-- | The 'sortBy' function is the non-overloaded version of 'sort'.
--
-- Try some linear sorts: radix, counting
-- Or mergesort.
--
-- sortBy :: (Word8 -> Word8 -> Ordering) -> ByteString -> ByteString
-- sortBy f ps = undefined

-- ---------------------------------------------------------------------
--
-- Extensions to the basic interface
--

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the ByteString is non-empty.
unsafeHead :: ByteString -> Word8
unsafeHead (PS x s _) = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty ByteStrings. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeTail :: ByteString -> ByteString
unsafeTail (PS ps s l) = PS ps (s+1) (l-1)
{-# INLINE unsafeTail #-}

-- | Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a 'Word8'
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex (PS x s _) i = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+i)
{-# INLINE unsafeIndex #-}

-- ---------------------------------------------------------------------
-- Low level constructors

#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddress'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = packAddress "literal"#
--
packAddress :: Addr# -> ByteString
packAddress addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 (fromIntegral $ c_strlen cstr)
  where
    cstr = Ptr addr#
{-# INLINE packAddress #-}

-- | /O(1)/ 'unsafePackAddress' provides constant-time construction of
-- 'ByteStrings' -- which is ideal for string literals. It packs a
-- null-terminated sequence of bytes into a 'ByteString', given a raw
-- 'Addr\#' to the string, and the length of the string. Make sure the
-- length is correct, otherwise use the safer 'packAddress' (where the
-- length will be calculated once at runtime).
unsafePackAddress :: Int -> Addr# -> ByteString
unsafePackAddress len addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 len
    where cstr = Ptr addr#

#endif

-- | /O(1)/ Build a ByteString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8 -> Int -> ByteString
fromForeignPtr fp l = PS fp 0 l

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
toForeignPtr (PS ps s l) = (ps, s, l)

-- | /O(1)/ 'skipIndex' returns the internal skipped index of the
-- current 'ByteString' from any larger string it was created from, as
-- an 'Int'.
skipIndex :: ByteString -> Int
skipIndex (PS _ s _) = s
{-# INLINE skipIndex #-}

-- | /O(n)/ Build a @ByteString@ from a @CString@. This value will have /no/
-- finalizer associated to it. The ByteString length is calculated using
-- /strlen(3)/, and thus the complexity is a /O(n)/.
packCString :: CString -> ByteString
packCString cstr = inlinePerformIO $ do
    fp <- newForeignPtr_ (castPtr cstr)
    return $ PS fp 0 (fromIntegral $ c_strlen cstr)

-- | /O(1)/ Build a @ByteString@ from a @CStringLen@. This value will
-- have /no/ finalizer associated with it. This operation has /O(1)/
-- complexity as we already know the final size, so no /strlen(3)/ is
-- required.
packCStringLen :: CStringLen -> ByteString
packCStringLen (ptr,len) = inlinePerformIO $ do
    fp <- newForeignPtr_ (castPtr ptr)
    return $ PS fp 0 (fromIntegral len)

-- | /O(n)/ Build a @ByteString@ from a malloced @CString@. This value will
-- have a @free(3)@ finalizer associated to it.
packMallocCString :: CString -> ByteString
packMallocCString cstr = inlinePerformIO $ do
    fp <- newForeignFreePtr (castPtr cstr)
    return $ PS fp 0 (fromIntegral $ c_strlen cstr)

#if defined(__GLASGOW_HASKELL__)
-- | /O(1)/ Construct a 'ByteString' given a C Ptr Word8 buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
packCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
packCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l

-- | Explicitly run the finaliser associated with a 'ByteString'.
-- Further references to this value may generate invalid memory
-- references. This operation is unsafe, as there may be other
-- 'ByteStrings' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'ByteString's
-- ever generated from the underlying byte array are no longer live.
unsafeFinalize :: ByteString -> IO ()
unsafeFinalize (PS p _ _) = finalizeForeignPtr p

#endif

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a null-terminated @CString@.
--   The @CString@ should not be freed afterwards. This is a memcpy(3).
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (PS ps s l) = bracket alloc (c_free.castPtr)
    where
      alloc = withForeignPtr ps $ \p -> do
                buf <- c_malloc (fromIntegral l+1)
                memcpy (castPtr buf) (castPtr p `plusPtr` s) (fromIntegral l)
                poke (buf `plusPtr` l) (0::Word8)
                return $ castPtr buf

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a @CString@.
-- Warning: modifying the @CString@ will affect the @ByteString@.
-- Why is this function unsafe? It relies on the null byte at the end of
-- the ByteString to be there. This is /not/ the case if your ByteString
-- has been spliced from a larger string (i.e. with take or drop).
-- Unless you can guarantee the null byte, you should use the safe
-- version, which will copy the string first.
--
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a
unsafeUseAsCString (PS ps s _) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s)

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage. 
--   This is mainly useful to allow the rest of the data pointed
--   to by the 'ByteString' to be garbage collected, for example
--   if a large string has been read in, and only a small part of it 
--   is needed in the rest of the program.
copy :: ByteString -> ByteString
copy (PS x s l) = create l $ \p -> withForeignPtr x $ \f -> memcpy p (f `plusPtr` s) l

-- | /O(n)/ Duplicate a CString as a ByteString. Useful if you know the
-- CString is going to be deallocated from C land.
copyCString :: CString -> ByteString
copyCString cstr = copyCStringLen (cstr, (fromIntegral $ c_strlen cstr))

-- | /O(n)/ Same as copyCString, but saves a strlen call when the length is known.
copyCStringLen :: CStringLen -> ByteString
copyCStringLen (cstr, len) = inlinePerformIO $ do
    fp <- mallocForeignPtrArray (len+1)
    withForeignPtr fp $ \p -> do
        memcpy p (castPtr cstr) len
        poke (p `plusPtr` len) (0 :: Word8)
    return $! PS fp 0 len

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a @CStringLen@.
-- Warning: modifying the @CStringLen@ will affect the @ByteString@.
-- This is analogous to unsafeUseAsCString, and comes with the same
-- safety requirements.
--
unsafeUseAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen (PS ps s l) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s,l)

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, generate makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.  The
-- string is padded at the end with a null byte.
--
-- generate is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
--
generate :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
generate i f = do
    p <- mallocArray (i+1)
    i' <- f p
    p' <- reallocArray p (i'+1)
    poke (p' `plusPtr` i') (0::Word8)    -- XXX so CStrings work
    fp <- newForeignFreePtr p'
    return $ PS fp 0 i'

-- ---------------------------------------------------------------------
-- line IO

#if defined(__GLASGOW_HASKELL__)

-- | getLine, read a line from stdin.
getLine :: IO ByteString
getLine = hGetLine stdin

-- | hGetLine. read a ByteString from a handle
hGetLine :: Handle -> IO ByteString
hGetLine h = wantReadableHandle "Data.ByteString.hGetLine" h $ \ handle_ -> do
    case haBufferMode handle_ of
       NoBuffering -> error "no buffering"
       _other      -> hGetLineBuffered handle_

 where
    hGetLineBuffered handle_ = do
        let ref = haBuffer handle_
        buf <- readIORef ref
        hGetLineBufferedLoop handle_ ref buf 0 []

    hGetLineBufferedLoop handle_ ref
            buf@Buffer{ bufRPtr=r, bufWPtr=w, bufBuf=raw } len xss =
        len `seq` do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if (w == off + 1)
                            then writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
                            else writeIORef ref buf{ bufRPtr = off + 1 }
                    mkBigPS new_len (xs:xss)
            else do
                 maybe_buf <- maybeFillReadBuffer (haFD handle_) True (haIsStream handle_)
                                    buf{ bufWPtr=0, bufRPtr=0 }
                 case maybe_buf of
                    -- Nothing indicates we caught an EOF, and we may have a
                    -- partial line to return.
                    Nothing -> do
                         writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
                         if new_len > 0
                            then mkBigPS new_len (xs:xss)
                            else ioe_EOF
                    Just new_buf ->
                         hGetLineBufferedLoop handle_ ref new_buf new_len (xs:xss)

    -- find the end-of-line character, if there is one
    findEOL r w raw
        | r == w = return w
        | otherwise =  do
            (c,r') <- readCharFromBuffer raw r
            if c == '\n'
                then return r -- NB. not r': don't include the '\n'
                else findEOL r' w raw

    maybeFillReadBuffer fd is_line is_stream buf = catch
        (do buf' <- fillReadBuffer fd is_line is_stream buf
            return (Just buf'))
        (\e -> if isEOFError e then return Nothing else ioError e)

-- TODO, rewrite to use normal memcpy
mkPS :: RawBuffer -> Int -> Int -> IO ByteString
mkPS buf start end = do
    let len = end - start
    fp <- mallocByteString len
    withForeignPtr fp $ \p -> do
        memcpy_ptr_baoff p buf start (fromIntegral len)
        return (PS fp 0 len)

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (P.reverse pss)

#endif

-- ---------------------------------------------------------------------
-- Block IO

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut _ (PS _ _ 0)  = return ()
hPut h (PS ps 0 l) = withForeignPtr ps $ \p-> hPutBuf h p l
hPut h (PS ps s l) = withForeignPtr ps $ \p-> hPutBuf h (p `plusPtr` s) l

-- | Write a ByteString to stdout
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | Write a ByteString to stdout, appending a newline byte
putStrLn :: ByteString -> IO ()
putStrLn ps = hPut stdout ps >> hPut stdout nl
    where nl = packByte 0x0a

-- | Read a 'ByteString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'.
hGet :: Handle -> Int -> IO ByteString
hGet _ 0 = return empty
hGet h i = do fp <- mallocByteString i
              l  <- withForeignPtr fp $ \p-> hGetBuf h p i
              return $ PS fp 0 l

#if defined(__GLASGOW_HASKELL__)
-- | hGetNonBlocking is identical to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking _ 0 = return empty
hGetNonBlocking h i = do
    fp <- mallocByteString i
    l  <- withForeignPtr fp $ \p -> hGetBufNonBlocking h p i
    return $ PS fp 0 l
#endif

-- | Read entire handle contents into a 'ByteString'.
--
-- As with 'hGet', the string representation in the file is assumed to
-- be ISO-8859-1.
--
hGetContents :: Handle -> IO ByteString
hGetContents h = do
    let start_size = 1024
    p <- mallocArray start_size
    i <- hGetBuf h p start_size
    if i < start_size
        then do p' <- reallocArray p i
                fp <- newForeignFreePtr p'
                return $ PS fp 0 i
        else f p start_size
    where
        f p s = do
            let s' = 2 * s
            p' <- reallocArray p s'
            i  <- hGetBuf h (p' `plusPtr` s) s
            if i < s
                then do let i' = s + i
                        p'' <- reallocArray p' i'
                        fp  <- newForeignFreePtr p''
                        return $ PS fp 0 i'
                else f p' s'

-- | getContents. Equivalent to hGetContents stdin
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Read an entire file directly into a 'ByteString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet.
readFile :: FilePath -> IO ByteString
readFile f = do
    h <- openBinaryFile f ReadMode
    l <- hFileSize h
    s <- hGet h $ fromIntegral l
    hClose h
    return s

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile f ps = do
    h <- openBinaryFile f WriteMode
    hPut h ps
    hClose h

{-
--
-- Disable until we can move it into a portable .hsc file
--

-- | Like readFile, this reads an entire file directly into a
-- 'ByteString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents
-- of the file never need to be copied.  Also, under memory pressure the
-- page may simply be discarded, while in the case of readFile it would
-- need to be written to swap.  If you read many small files, mmapFile
-- will be less memory-efficient than readFile, since each mmapFile
-- takes up a separate page of memory.  Also, you can run into bus
-- errors if the file is modified.  As with 'readFile', the string
-- representation in the file is assumed to be ISO-8859-1.
--
-- On systems without mmap, this is the same as a readFile.
--
mmapFile :: FilePath -> IO ByteString
mmapFile f = mmap f >>= \(fp,l) -> return $ PS fp 0 l

mmap :: FilePath -> IO (ForeignPtr Word8, Int)
mmap f = do
    h <- openBinaryFile f ReadMode
    l <- fromIntegral `fmap` hFileSize h
    -- Don't bother mmaping small files because each mmapped file takes up
    -- at least one full VM block.
    if l < mmap_limit
       then do thefp <- mallocByteString l
               withForeignPtr thefp $ \p-> hGetBuf h p l
               hClose h
               return (thefp, l)
       else do
               -- unix only :(
               fd <- fromIntegral `fmap` handleToFd h
               p  <- my_mmap l fd
               fp <- if p == nullPtr
                     then do thefp <- mallocByteString l
                             withForeignPtr thefp $ \p' -> hGetBuf h p' l
                             return thefp
                     else do
                          -- The munmap leads to crashes on OpenBSD.
                          -- maybe there's a use after unmap in there somewhere?
#if !defined(__OpenBSD__)
                             let unmap = c_munmap p l >> return ()
#else
                             let unmap = return ()
#endif
                             fp <- FC.newForeignPtr p unmap
                             return fp
               c_close fd
               hClose h
               return (fp, l)
    where mmap_limit = 16*1024
-}

#if defined(__GLASGOW_HASKELL__)
--
-- | A ByteString equivalent for getArgs. More efficient for large argument lists
--
getArgs :: IO [ByteString]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
    getProgArgv p_argc p_argv
    p    <- fromIntegral `fmap` peek p_argc
    argv <- peek p_argv
    P.map packCString `fmap` peekArray (p - 1) (advancePtr argv 1)
#endif

-- ---------------------------------------------------------------------
-- Internal utilities

-- Unsafe conversion between 'Word8' and 'Char'. These are nops, and
-- silently truncate to 8 bits Chars > '\255'. They are provided as
-- convenience for ByteString construction.
w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- Wrapper of mallocForeignPtrArray. Any ByteString allocated this way
-- is padded with a null byte.
mallocByteString :: Int -> IO (ForeignPtr Word8)
mallocByteString l = do
    fp <- mallocForeignPtrArray (l+1)
    withForeignPtr fp $ \p -> poke (p `plusPtr` l) (0::Word8)
    return fp

-- | A way of creating ForeignPtrs outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike 'generate'
-- the ByteString is not reallocated if the final size is less than the
-- estimated size. Also, unlike 'generate' ByteString's created this way
-- are managed on the Haskell heap.
create :: Int -> (Ptr Word8 -> IO ()) -> ByteString
create l write_ptr = inlinePerformIO $ do
    fp <- mallocByteString (l+1)
    withForeignPtr fp $ \p -> write_ptr p
    return $ PS fp 0 l
{-# INLINE create #-}

-- | Perform an operation with a temporary ByteString
withPtr :: ForeignPtr a -> (Ptr a -> IO b) -> b
withPtr fp io = inlinePerformIO (withForeignPtr fp io)
{-# INLINE withPtr #-}

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.ByteString." ++ fun ++ ": empty ByteString")
{-# INLINE errorEmptyList #-}

-- 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Word8 -> Bool) -> ByteString -> Int
STRICT2(findIndexOrEnd)
findIndexOrEnd f ps
    | null ps           = 0
    | f (unsafeHead ps) = 0
    | otherwise         = 1 + findIndexOrEnd f (unsafeTail ps)
{-# INLINE findIndexOrEnd #-}

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
STRICT2(findFromEndUntil)
findFromEndUntil f ps@(PS x s l) =
    if null ps then 0
    else if f (last ps) then l
         else findFromEndUntil f (PS x s (l-1))

-- Just like inlinePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

{-# INLINE newForeignFreePtr #-}
newForeignFreePtr :: Ptr Word8 -> IO (ForeignPtr Word8)
#if defined(__GLASGOW_HASKELL__)
newForeignFreePtr p = FC.newForeignPtr p (c_free p)
#else
newForeignFreePtr p = newForeignPtr c_free_finalizer p
#endif

-- ---------------------------------------------------------------------
-- 
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> CInt

foreign import ccall unsafe "stdlib.h malloc" c_malloc
    :: CInt -> IO (Ptr Word8)

foreign import ccall unsafe "static stdlib.h free" c_free
    :: Ptr Word8 -> IO ()

#if !defined(__GLASGOW_HASKELL__)
foreign import ccall unsafe "static stdlib.h &free" c_free_finalizer
    :: FunPtr (Ptr Word8 -> IO ())
#endif

foreign import ccall unsafe "string.h memset" memset
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memchr" memchr
    :: Ptr Word8 -> Word8 -> CSize -> Ptr Word8

foreign import ccall unsafe "string.h memcmp" memcmp
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "string.h memcpy" memcpy
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

-- ---------------------------------------------------------------------
--
-- Uses our C code
--

foreign import ccall unsafe "static fpstring.h reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> Int -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h maximum" c_maximum
    :: Ptr Word8 -> Int -> Word8

foreign import ccall unsafe "static fpstring.h minimum" c_minimum
    :: Ptr Word8 -> Int -> Word8

foreign import ccall unsafe "static fpstring.h count" c_count
    :: Ptr Word8 -> Int -> Word8 -> Int

-- ---------------------------------------------------------------------
-- MMap

{-
foreign import ccall unsafe "static fpstring.h my_mmap" my_mmap
    :: Int -> Int -> IO (Ptr Word8)

foreign import ccall unsafe "static unistd.h close" c_close
    :: Int -> IO Int

#  if !defined(__OpenBSD__)
foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int
#  endif
-}

-- ---------------------------------------------------------------------
-- Internal GHC Haskell magic

#if defined(__GLASGOW_HASKELL__)
foreign import ccall unsafe "RtsAPI.h getProgArgv"
    getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> Int -> CSize -> IO (Ptr ())
#endif

-- ---------------------------------------------------------------------
--
-- Functional array fusion for ByteStrings. 
--
-- From the Data Parallel Haskell project, 
--      http://www.cse.unsw.edu.au/~chak/project/dph/
--

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAL) `seq` x = x@.
--
data NoAL = NoAL

-- | Special forms of loop arguments
--
-- * These are common special cases for the three function arguments of gen
--   and loop; we give them special names to make it easier to trigger RULES
--   applying in the special cases represented by these arguments.  The
--   "INLINE [1]" makes sure that these functions are only inlined in the last
--   two simplifier phases.
--
-- * In the case where the accumulator is not needed, it is better to always
--   explicitly return a value `()', rather than just copy the input to the
--   output, as the former gives GHC better local information.
-- 

-- | Element function expressing a mapping only
mapEFL :: (Word8 -> Word8) -> (NoAL -> Word8 -> (NoAL, Maybe Word8))
mapEFL f = \_ e -> (noAL, (Just $ f e))
{-# INLINE [1] mapEFL #-}

-- | Element function implementing a filter function only
filterEFL :: (Word8 -> Bool) -> (NoAL -> Word8 -> (NoAL, Maybe Word8))
filterEFL p = \_ e -> if p e then (noAL, Just e) else (noAL, Nothing)
{-# INLINE [1] filterEFL #-}

-- |Element function expressing a reduction only
foldEFL :: (acc -> Word8 -> acc) -> (acc -> Word8 -> (acc, Maybe Word8))
foldEFL f = \a e -> (f a e, Nothing)
{-# INLINE [1] foldEFL #-}

-- | No accumulator
noAL :: NoAL
noAL = NoAL
{-# INLINE [1] noAL #-}

-- | Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
loopArr :: (ByteString, acc) -> ByteString
loopArr (arr, _) = arr
{-# INLINE [1] loopArr #-}

loopAcc :: (ByteString, acc) -> acc
loopAcc (_, acc) = acc
{-# INLINE [1] loopAcc #-}

loopSndAcc :: (ByteString, (acc1, acc2)) -> (ByteString, acc2)
loopSndAcc (arr, (_, acc)) = (arr, acc)
{-# INLINE [1] loopSndAcc #-}

------------------------------------------------------------------------

-- | Iteration over over ByteStrings
loopU :: (acc -> Word8 -> (acc, Maybe Word8))  -- ^ mapping & folding, once per elem
      -> acc                                   -- ^ initial acc value
      -> ByteString                            -- ^ input ByteString
      -> (ByteString, acc)

loopU f start (PS fp s i) = inlinePerformIO $ withForeignPtr fp $ \a -> do
    p <- mallocArray (i+1)
    (acc, i') <- go (a `plusPtr` s) p start
    p' <- if i == i' then return p else reallocArray p (i'+1) -- avoid realloc for maps
    poke (p' `plusPtr` i') (0::Word8)
    fp' <- newForeignFreePtr p'
    return (PS fp' 0 i', acc)
  where
    go p ma = trans 0 0
        where
            STRICT3(trans)
            trans a_off ma_off acc
                | a_off >= i = return (acc, ma_off)
                | otherwise  = do
                    x <- peekByteOff p a_off
                    let (acc', oe) = f acc x
                    ma_off' <- case oe of
                        Nothing  -> return ma_off
                        Just e   -> do pokeByteOff ma ma_off e
                                       return $ ma_off + 1
                    trans (a_off+1) ma_off' acc'

{-# INLINE [1] loopU #-}

{-# RULES

"array fusion!" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    let em (acc1, acc2) e =
            case em1 acc1 e of
                (acc1', Nothing) -> ((acc1', acc2), Nothing)
                (acc1', Just e') ->
                    case em2 acc2 e' of
                        (acc2', res) -> ((acc1', acc2'), res)
    in loopSndAcc (loopU em (start1, start2) arr)

"loopArr/loopSndAcc" forall x.
  loopArr (loopSndAcc x) = loopArr x

-- orphan?
-- "seq/NoAL" forall (u::NoAL) e.
--   u `seq` e = e

 #-}

