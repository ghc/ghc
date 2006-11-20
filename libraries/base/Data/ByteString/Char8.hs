{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-- |
-- Module      : Data.ByteString.Char8
-- Copyright   : (c) Don Stewart 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
--
-- Manipulate 'ByteString's using 'Char' operations. All Chars will be
-- truncated to 8 bits. It can be expected that these functions will run
-- at identical speeds to their 'Word8' equivalents in "Data.ByteString".
--
-- More specifically these byte strings are taken to be in the
-- subset of Unicode covered by code points 0-255. This covers
-- Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls.
-- 
-- See: 
--
--  * <http://www.unicode.org/charts/>
--
--  * <http://www.unicode.org/charts/PDF/U0000.pdf>
--
--  * <http://www.unicode.org/charts/PDF/U0080.pdf>
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Char8 as B
--

module Data.ByteString.Char8 (

        -- * The @ByteString@ type
        ByteString,             -- abstract, instances: Eq, Ord, Show, Read, Data, Typeable, Monoid

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        singleton,              -- :: Char   -> ByteString
        pack,                   -- :: String -> ByteString
        unpack,                 -- :: ByteString -> String

        -- * Basic interface
        cons,                   -- :: Char -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Char -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString
        head,                   -- :: ByteString -> Char
        last,                   -- :: ByteString -> Char
        tail,                   -- :: ByteString -> ByteString
        init,                   -- :: ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int

        -- * Transformating ByteStrings
        map,                    -- :: (Char -> Char) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Char -> ByteString -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's (folds)
        foldl,                  -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldl',                 -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldl1',                -- :: (Char -> Char -> Char) -> ByteString -> Char

        foldr,                  -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldr',                 -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldr1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldr1',                -- :: (Char -> Char -> Char) -> ByteString -> Char

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Char -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Char -> Bool) -> ByteString -> Bool
        all,                    -- :: (Char -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Char
        minimum,                -- :: ByteString -> Char

        -- * Building ByteStrings
        -- ** Scans
        scanl,                  -- :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
        scanl1,                 -- :: (Char -> Char -> Char) -> ByteString -> ByteString
        scanr,                  -- :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
        scanr1,                 -- :: (Char -> Char -> Char) -> ByteString -> ByteString

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
        mapAccumR,              -- :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
        mapIndexed,             -- :: (Int -> Char -> Char) -> ByteString -> ByteString

        -- * Generating and unfolding ByteStrings
        replicate,              -- :: Int -> Char -> ByteString
        unfoldr,                -- :: (a -> Maybe (Char, a)) -> a -> ByteString
        unfoldrN,               -- :: Int -> (a -> Maybe (Char, a)) -> a -> (ByteString, Maybe a)

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        span,                   -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,                -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        break,                  -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        breakEnd,               -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]

        -- ** Breaking into many substrings
        split,                  -- :: Char -> ByteString -> [ByteString]
        splitWith,              -- :: (Char -> Bool) -> ByteString -> [ByteString]

        -- ** Breaking into lines and words
        lines,                  -- :: ByteString -> [ByteString]
        words,                  -- :: ByteString -> [ByteString]
        unlines,                -- :: [ByteString] -> ByteString
        unwords,                -- :: ByteString -> [ByteString]

        -- ** Joining strings
        join,                   -- :: ByteString -> [ByteString] -> ByteString

        -- ** Searching for substrings
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool
        isSubstringOf,          -- :: ByteString -> ByteString -> Bool
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,                   -- :: Char -> ByteString -> Bool
        notElem,                -- :: Char -> ByteString -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Char -> Bool) -> ByteString -> Maybe Char
        filter,                 -- :: (Char -> Bool) -> ByteString -> ByteString
--      partition               -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int -> Char
        elemIndex,              -- :: Char -> ByteString -> Maybe Int
        elemIndices,            -- :: Char -> ByteString -> [Int]
        elemIndexEnd,           -- :: Char -> ByteString -> Maybe Int
        findIndex,              -- :: (Char -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Char -> Bool) -> ByteString -> [Int]
        count,                  -- :: Char -> ByteString -> Int

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Char,Char)] -> (ByteString,ByteString)

        -- * Ordered ByteStrings
        sort,                   -- :: ByteString -> ByteString

        -- * Reading from ByteStrings
        readInt,                -- :: ByteString -> Maybe (Int, ByteString)
        readInteger,            -- :: ByteString -> Maybe (Integer, ByteString)

        -- * Low level CString conversions

        -- ** Packing CStrings and pointers
        packCString,            -- :: CString -> ByteString
        packCStringLen,         -- :: CString -> ByteString
        packMallocCString,      -- :: CString -> ByteString

        -- ** Using ByteStrings as CStrings
        useAsCString,           -- :: ByteString -> (CString -> IO a) -> IO a
        useAsCStringLen,        -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- * Copying ByteStrings
        copy,                   -- :: ByteString -> ByteString
        copyCString,            -- :: CString -> IO ByteString
        copyCStringLen,         -- :: CStringLen -> IO ByteString

        -- * I\/O with @ByteString@s

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
--      mmapFile,               -- :: FilePath -> IO ByteString

        -- ** I\/O with Handles
        hGetLine,               -- :: Handle -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()
        hPutStr,                -- :: Handle -> ByteString -> IO ()
        hPutStrLn,              -- :: Handle -> ByteString -> IO ()

#if defined(__GLASGOW_HASKELL__)
        -- * Low level construction
        -- | For constructors from foreign language types see "Data.ByteString"
        packAddress,            -- :: Addr# -> ByteString
        unsafePackAddress,      -- :: Int -> Addr# -> ByteString
#endif

        -- * Utilities (needed for array fusion)
#if defined(__GLASGOW_HASKELL__)
        unpackList,
#endif

    ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,unwords
                                ,words,maximum,minimum,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,appendFile,readFile,writeFile
                                ,foldl1,foldr1,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B

-- Listy functions transparently exported
import Data.ByteString (empty,null,length,tail,init,append
                       ,inits,tails,reverse,transpose
                       ,concat,take,drop,splitAt,join
                       ,sort,isPrefixOf,isSuffixOf,isSubstringOf,findSubstring
                       ,findSubstrings,copy,group

                       ,getLine, getContents, putStr, putStrLn, interact
                       ,hGetContents, hGet, hPut, hPutStr, hPutStrLn
                       ,hGetLine, hGetNonBlocking
                       ,packCString,packCStringLen, packMallocCString
                       ,useAsCString,useAsCStringLen, copyCString,copyCStringLen
#if defined(__GLASGOW_HASKELL__)
                       ,unpackList
#endif
                       )

import Data.ByteString.Base (
                        ByteString(..)
#if defined(__GLASGOW_HASKELL__)
                       ,packAddress, unsafePackAddress
#endif
                       ,c2w, w2c, unsafeTail, isSpaceWord8, inlinePerformIO
                       )

import Data.Char    ( isSpace )
import qualified Data.List as List (intersperse)

import System.IO                (openFile,hClose,hFileSize,IOMode(..))
import Control.Exception        (bracket)
import Foreign

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 (Char(..),unpackCString#,ord#,int2Word#)
import GHC.IOBase               (IO(..),stToIO)
import GHC.Prim                 (Addr#,writeWord8OffAddr#,plusAddr#)
import GHC.Ptr                  (Ptr(..))
import GHC.ST                   (ST(..))
#endif

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined

------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
singleton :: Char -> ByteString
singleton = B.singleton . c2w
{-# INLINE singleton #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck. In such cases, consider using packAddress (GHC only).
pack :: String -> ByteString
#if !defined(__GLASGOW_HASKELL__)

pack str = B.unsafeCreate (P.length str) $ \p -> go p str
    where go _ []     = return ()
          go p (x:xs) = poke p (c2w x) >> go (p `plusPtr` 1) xs

#else /* hack away */

pack str = B.unsafeCreate (P.length str) $ \(Ptr p) -> stToIO (go p str)
  where
    go :: Addr# -> [Char] -> ST a ()
    go _ []        = return ()
    go p (C# c:cs) = writeByte p (int2Word# (ord# c)) >> go (p `plusAddr#` 1#) cs

    writeByte p c = ST $ \s# ->
        case writeWord8OffAddr# p 0# c s# of s2# -> (# s2#, () #)
    {-# INLINE writeByte #-}
{-# INLINE [1] pack #-}

{-# RULES
    "FPS pack/packAddress" forall s .
       pack (unpackCString# s) = B.packAddress s
 #-}

#endif

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = P.map w2c . B.unpack
{-# INLINE unpack #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> ByteString -> ByteString
cons = B.cons . c2w
{-# INLINE cons #-}

-- | /O(n)/ Append a Char to the end of a 'ByteString'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: ByteString -> Char -> ByteString
snoc p = B.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Char
head = w2c . B.head
{-# INLINE head #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: ByteString -> Char
last = w2c . B.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> ByteString -> ByteString
map f = B.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'ByteString'
-- and \`intersperses\' that Char between the elements of the
-- 'ByteString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
intersperse = B.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = B.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl\'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = B.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = B.foldr (\c a -> f (w2c c) a)
{-# INLINE foldr #-}

-- | 'foldr\'' is a strict variant of foldr
foldr' :: (Char -> a -> a) -> a -> ByteString -> a
foldr' f = B.foldr' (\c a -> f (w2c c) a)
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | A strict version of 'foldl1'
foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (B.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (B.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | A strict variant of foldr1
foldr1' :: (Char -> Char -> Char) -> ByteString -> Char
foldr1' f ps = w2c (B.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1' #-}

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f = B.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = B.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum = w2c . B.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Char
minimum = w2c . B.minimum
{-# INLINE minimum #-}

-- | /O(n)/ map Char functions, provided with the index at each position
mapIndexed :: (Int -> Char -> Char) -> ByteString -> ByteString
mapIndexed f = B.mapIndexed (\i c -> c2w (f i (w2c c)))
{-# INLINE mapIndexed #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = B.mapAccumL (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = B.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = B.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanl1 f = B.scanl1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | scanr is the right-to-left dual of scanl.
scanr :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanr f z = B.scanr (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanr1 f = B.scanr1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Char -> ByteString
replicate w = B.replicate w . c2w
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr' 
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a 
-- ByteString from a seed value.  The function takes the element and 
-- returns 'Nothing' if it is done producing the ByteString or returns 
-- 'Just' @(a,b)@, in which case, @a@ is the next character in the string, 
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- > unfoldr (\x -> if x <= '9' then Just (x, succ x) else Nothing) '0' == "0123456789"
unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f x0 = B.unfoldr (fmap k . f) x0
    where k (i, j) = (c2w i, j)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > unfoldrN n f s == take n (unfoldr f s)
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> (ByteString, Maybe a)
unfoldrN n f w = B.unfoldrN n ((k `fmap`) . f) w
    where k (i,j) = (c2w i, j)
{-# INLINE unfoldrN #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = B.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = B.dropWhile (f . w2c)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] dropWhile #-}
#endif

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = B.break (f . w2c)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] break #-}
#endif

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = B.span (f . w2c)
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
spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd f = B.spanEnd (f . w2c)
{-# INLINE spanEnd #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'ByteString'
-- 
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd f = B.breakEnd (f . w2c)
{-# INLINE breakEnd #-}

{-
-- | 'breakChar' breaks its ByteString argument at the first occurence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar = B.breakByte . c2w
{-# INLINE breakChar #-}

-- | 'spanChar' breaks its ByteString argument at the first
-- occurence of a Char other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanChar :: Char -> ByteString -> (ByteString, ByteString)
spanChar = B.spanByte . c2w
{-# INLINE spanChar #-}
-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
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
split :: Char -> ByteString -> [ByteString]
split = B.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = B.splitWith (f . w2c)
{-# INLINE splitWith #-}
-- the inline makes a big difference here.

{-
-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> ByteString -> [ByteString]
tokens f = B.tokens (f . w2c)
{-# INLINE tokens #-}
-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = B.groupBy (\a b -> k (w2c a) (w2c b))

{-
-- | /O(n)/ joinWithChar. An efficient way to join to two ByteStrings with a
-- char. Around 4 times faster than the generalised join.
--
joinWithChar :: Char -> ByteString -> ByteString -> ByteString
joinWithChar = B.joinWithByte . c2w
{-# INLINE joinWithChar #-}
-}

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Char
index = (w2c .) . B.index
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int
elemIndex = B.elemIndex . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd = B.elemIndexEnd . c2w
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int]
elemIndices = B.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex f = B.findIndex (f . w2c)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices f = B.findIndices (f . w2c)

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
-- 
-- Also
--  
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> ByteString -> Int
count c = B.count (c2w c)

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem    c = B.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c = B.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = B.filter (f . w2c)
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` B.find (f . w2c) ps
{-# INLINE find #-}

{-
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single Char. It is more efficient to use
-- filterChar in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> ByteString -> ByteString
filterChar c = B.filterByte (c2w c)
{-# INLINE filterChar #-}

-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single Char out of a list. It is more efficient
-- to use /filterNotChar/ in this case.
--
-- > filterNotChar == filter . (/=)
--
-- filterNotChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterNotChar :: Char -> ByteString -> ByteString
filterNotChar c = B.filterNotByte (c2w c)
{-# INLINE filterNotChar #-}
-}

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of Chars. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte ByteStrings
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | B.null ps || B.null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs) : zip (B.unsafeTail ps) (B.unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = B.zipWith ((. w2c) . f . w2c)

-- | 'unzip' transforms a list of pairs of Chars into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits
-- the check for the empty case, which is good for performance, but
-- there is an obligation on the programmer to provide a proof that the
-- ByteString is non-empty.
unsafeHead :: ByteString -> Char
unsafeHead  = w2c . B.unsafeHead
{-# INLINE unsafeHead #-}

-- ---------------------------------------------------------------------
-- Things that depend on the encoding

{-# RULES
    "FPS specialise break -> breakSpace"
        break isSpace = breakSpace
  #-}

-- | 'breakSpace' returns the pair of ByteStrings when the argument is
-- broken at the first whitespace byte. I.e.
-- 
-- > break isSpace == breakSpace
--
breakSpace :: ByteString -> (ByteString,ByteString)
breakSpace (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstspace (p `plusPtr` s) 0 l
    return $! case () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

firstspace :: Ptr Word8 -> Int -> Int -> IO Int
STRICT3(firstspace)
firstspace ptr n m
    | n >= m    = return n
    | otherwise = do w <- peekByteOff ptr n
                     if (not . isSpaceWord8) w then firstspace ptr (n+1) m else return n

{-# RULES
    "FPS specialise dropWhile isSpace -> dropSpace"
        dropWhile isSpace = dropSpace
  #-}

-- | 'dropSpace' efficiently returns the 'ByteString' argument with
-- white space Chars removed from the front. It is more efficient than
-- calling dropWhile for removing whitespace. I.e.
-- 
-- > dropWhile isSpace == dropSpace
--
dropSpace :: ByteString -> ByteString
dropSpace (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstnonspace (p `plusPtr` s) 0 l
    return $! if i == l then empty else PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
STRICT3(firstnonspace)
firstnonspace ptr n m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then firstnonspace ptr (n+1) m else return n

{-
-- | 'dropSpaceEnd' efficiently returns the 'ByteString' argument with
-- white space removed from the end. I.e.
-- 
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
--
-- but it is more efficient than using multiple reverses.
--
dropSpaceEnd :: ByteString -> ByteString
dropSpaceEnd (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- lastnonspace (p `plusPtr` s) (l-1)
    return $! if i == (-1) then empty else PS x s (i+1)
{-# INLINE dropSpaceEnd #-}

lastnonspace :: Ptr Word8 -> Int -> IO Int
STRICT2(lastnonspace)
lastnonspace ptr n
    | n < 0     = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then lastnonspace ptr (n-1) else return n
-}

-- | 'lines' breaks a ByteString up into a list of ByteStrings at
-- newline Chars. The resulting strings do not contain newlines.
--
lines :: ByteString -> [ByteString]
lines ps
    | null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
    where search = elemIndex '\n'
{-# INLINE lines #-}

{-
-- Just as fast, but more complex. Should be much faster, I thought.
lines :: ByteString -> [ByteString]
lines (PS _ _ 0) = []
lines (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
        let ptr = p `plusPtr` s

            STRICT1(loop)
            loop n = do
                let q = memchr (ptr `plusPtr` n) 0x0a (fromIntegral (l-n))
                if q == nullPtr
                    then return [PS x (s+n) (l-n)]
                    else do let i = q `minusPtr` ptr
                            ls <- loop (i+1)
                            return $! PS x (s+n) (i-n) : ls
        loop 0
-}

-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [ByteString] -> ByteString
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where nl = singleton '\n'

-- | 'words' breaks a ByteString up into a list of words, which
-- were delimited by Chars representing white space. And
--
-- > tokens isSpace = words
--
words :: ByteString -> [ByteString]
words = P.filter (not . B.null) . B.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [ByteString] -> ByteString
unwords = join (singleton ' ')
{-# INLINE unwords #-}

-- ---------------------------------------------------------------------
-- Reading from ByteStrings

-- | readInt reads an Int from the beginning of the ByteString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise
-- it just returns the int read, and the rest of the string.
readInt :: ByteString -> Maybe (Int, ByteString)
readInt as
    | null as   = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> loop True  0 0 (unsafeTail as)
            '+' -> loop False 0 0 (unsafeTail as)
            _   -> loop False 0 0 as

    where loop :: Bool -> Int -> Int -> ByteString -> Maybe (Int, ByteString)
          STRICT4(loop)
          loop neg i n ps
              | null ps   = end neg i n ps
              | otherwise =
                  case B.unsafeHead ps of
                    w | w >= 0x30
                     && w <= 0x39 -> loop neg (i+1)
                                          (n * 10 + (fromIntegral w - 0x30))
                                          (unsafeTail ps)
                      | otherwise -> end neg i n ps

          end _    0 _ _  = Nothing
          end True _ n ps = Just (negate n, ps)
          end _    _ n ps = Just (n, ps)

-- | readInteger reads an Integer from the beginning of the ByteString.  If
-- there is no integer at the beginning of the string, it returns Nothing,
-- otherwise it just returns the int read, and the rest of the string.
readInteger :: ByteString -> Maybe (Integer, ByteString)
readInteger as
    | null as   = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> first (unsafeTail as) >>= \(n, bs) -> return (-n, bs)
            '+' -> first (unsafeTail as)
            _   -> first as

    where first ps | null ps   = Nothing
                   | otherwise =
                       case B.unsafeHead ps of
                        w | w >= 0x30 && w <= 0x39 -> Just $
                            loop 1 (fromIntegral w - 0x30) [] (unsafeTail ps)
                          | otherwise              -> Nothing

          loop :: Int -> Int -> [Integer]
               -> ByteString -> (Integer, ByteString)
          STRICT4(loop)
          loop d acc ns ps
              | null ps   = combine d acc ns empty
              | otherwise =
                  case B.unsafeHead ps of
                   w | w >= 0x30 && w <= 0x39 ->
                       if d == 9 then loop 1 (fromIntegral w - 0x30)
                                           (toInteger acc : ns)
                                           (unsafeTail ps)
                                 else loop (d+1)
                                           (10*acc + (fromIntegral w - 0x30))
                                           ns (unsafeTail ps)
                     | otherwise -> combine d acc ns ps

          combine _ acc [] ps = (toInteger acc, ps)
          combine d acc ns ps =
              ((10^d * combine1 1000000000 ns + toInteger acc), ps)

          combine1 _ [n] = n
          combine1 b ns  = combine1 (b*b) $ combine2 b ns

          combine2 b (n:m:ns) = let t = m*b + n in t `seq` (t : combine2 b ns)
          combine2 _ ns       = ns

-- | Read an entire file strictly into a 'ByteString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet.
readFile :: FilePath -> IO ByteString
readFile f = bracket (openFile f ReadMode) hClose
    (\h -> hFileSize h >>= hGet h . fromIntegral)

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile f txt = bracket (openFile f WriteMode) hClose
    (\h -> hPut h txt)

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile f txt = bracket (openFile f AppendMode) hClose
    (\h -> hPut h txt)

