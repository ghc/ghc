{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_HADDOCK prune #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString.Lazy.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- Manipulate /lazy/ 'ByteString's using 'Char' operations. All Chars will
-- be truncated to 8 bits. It can be expected that these functions will
-- run at identical speeds to their 'Data.Word.Word8' equivalents in
-- "Data.ByteString.Lazy".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy.Char8 as C
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings.
-- Use @{-\# LANGUAGE OverloadedStrings \#-}@ to enable this.
--

module Data.ByteString.Lazy.Char8 (

        -- * The @ByteString@ type
        ByteString,             -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        singleton,              -- :: Char   -> ByteString
        pack,                   -- :: String -> ByteString
        unpack,                 -- :: ByteString -> String
        fromChunks,             -- :: [Strict.ByteString] -> ByteString
        toChunks,               -- :: ByteString -> [Strict.ByteString]
        fromStrict,             -- :: Strict.ByteString -> ByteString
        toStrict,               -- :: ByteString -> Strict.ByteString

        -- * Basic interface
        cons,                   -- :: Char -> ByteString -> ByteString
        cons',                  -- :: Char -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Char -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString
        head,                   -- :: ByteString -> Char
        uncons,                 -- :: ByteString -> Maybe (Char, ByteString)
        last,                   -- :: ByteString -> Char
        tail,                   -- :: ByteString -> ByteString
        unsnoc,                 -- :: ByteString -> Maybe (ByteString, Char)
        init,                   -- :: ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int64

        -- * Transforming ByteStrings
        map,                    -- :: (Char -> Char) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Char -> ByteString -> ByteString
        intercalate,            -- :: ByteString -> [ByteString] -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's (folds)
        foldl,                  -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldl',                 -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldl1',                -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldr,                  -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldr1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char

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
--      scanl1,                 -- :: (Char -> Char -> Char) -> ByteString -> ByteString
--      scanr,                  -- :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
--      scanr1,                 -- :: (Char -> Char -> Char) -> ByteString -> ByteString

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
        mapAccumR,              -- :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)

        -- ** Infinite ByteStrings
        repeat,                 -- :: Char -> ByteString
        replicate,              -- :: Int64 -> Char -> ByteString
        cycle,                  -- :: ByteString -> ByteString
        iterate,                -- :: (Char -> Char) -> Char -> ByteString

        -- ** Unfolding ByteStrings
        unfoldr,                -- :: (a -> Maybe (Char, a)) -> a -> ByteString

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int64 -> ByteString -> ByteString
        drop,                   -- :: Int64 -> ByteString -> ByteString
        splitAt,                -- :: Int64 -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        span,                   -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        break,                  -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]
        stripPrefix,            -- :: ByteString -> ByteString -> Maybe ByteString
        stripSuffix,            -- :: ByteString -> ByteString -> Maybe ByteString

        -- ** Breaking into many substrings
        split,                  -- :: Char -> ByteString -> [ByteString]
        splitWith,              -- :: (Char -> Bool) -> ByteString -> [ByteString]

        -- ** Breaking into lines and words
        lines,                  -- :: ByteString -> [ByteString]
        words,                  -- :: ByteString -> [ByteString]
        unlines,                -- :: [ByteString] -> ByteString
        unwords,                -- :: ByteString -> [ByteString]

        -- * Predicates
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,                   -- :: Char -> ByteString -> Bool
        notElem,                -- :: Char -> ByteString -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Char -> Bool) -> ByteString -> Maybe Char
        filter,                 -- :: (Char -> Bool) -> ByteString -> ByteString
--      partition               -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int64 -> Char
        elemIndex,              -- :: Char -> ByteString -> Maybe Int64
        elemIndices,            -- :: Char -> ByteString -> [Int64]
        findIndex,              -- :: (Char -> Bool) -> ByteString -> Maybe Int64
        findIndices,            -- :: (Char -> Bool) -> ByteString -> [Int64]
        count,                  -- :: Char -> ByteString -> Int64

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> ByteString -> ByteString -> [c]
--      unzip,                  -- :: [(Char,Char)] -> (ByteString,ByteString)

        -- * Ordered ByteStrings
--        sort,                   -- :: ByteString -> ByteString

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,                   -- :: ByteString -> ByteString

        -- * Reading from ByteStrings
        readInt,
        readInteger,

        -- * I\/O with 'ByteString's
        -- | ByteString I/O uses binary mode, without any character decoding
        -- or newline conversion. The fact that it does not respect the Handle
        -- newline mode is considered a flaw and may be changed in a future version.

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
        hGet,                   -- :: Handle -> Int64 -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int64 -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()
        hPutNonBlocking,        -- :: Handle -> ByteString -> IO ByteString
        hPutStr,                -- :: Handle -> ByteString -> IO ()
        hPutStrLn,              -- :: Handle -> ByteString -> IO ()

  ) where

-- Functions transparently exported
import Data.ByteString.Lazy 
        (fromChunks, toChunks, fromStrict, toStrict
        ,empty,null,length,tail,init,append,reverse,transpose,cycle
        ,concat,take,drop,splitAt,intercalate
        ,isPrefixOf,isSuffixOf,group,inits,tails,copy
        ,stripPrefix,stripSuffix
        ,hGetContents, hGet, hPut, getContents
        ,hGetNonBlocking, hPutNonBlocking
        ,putStr, hPutStr, interact
        ,readFile,writeFile,appendFile)

-- Functions we need to wrap.
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S (ByteString) -- typename only
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Lazy.Internal

import Data.ByteString.Internal (w2c, c2w, isSpaceWord8)

import Data.Int (Int64)
import qualified Data.List as List

import Prelude hiding           
        (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
        ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter
        ,unwords,words,maximum,minimum,all,concatMap,scanl,scanl1,foldl1,foldr1
        ,readFile,writeFile,appendFile,replicate,getContents,getLine,putStr,putStrLn
        ,zip,zipWith,unzip,notElem,repeat,iterate,interact,cycle)

import System.IO            (Handle, stdout)

------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
singleton :: Char -> ByteString
singleton = L.singleton . c2w
{-# INLINE singleton #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'. 
pack :: [Char] -> ByteString
pack = packChars

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = unpackChars

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

-- | /O(1)/ 'cons' is analogous to '(:)' for lists.
cons :: Char -> ByteString -> ByteString
cons = L.cons . c2w
{-# INLINE cons #-}

-- | /O(1)/ Unlike 'cons', 'cons\'' is
-- strict in the ByteString that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons\' c xs in xs
--
-- You can however use 'cons', as well as 'repeat' and 'cycle', to build
-- infinite lazy ByteStrings.
--
cons' :: Char -> ByteString -> ByteString
cons' = L.cons' . c2w
{-# INLINE cons' #-}

-- | /O(n)/ Append a Char to the end of a 'ByteString'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: ByteString -> Char -> ByteString
snoc p = L.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Char
head = w2c . L.head
{-# INLINE head #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Char, ByteString)
uncons bs = case L.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')
{-# INLINE uncons #-}

-- | /O(n\/c)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bs = case L.unsnoc bs of
                  Nothing -> Nothing
                  Just (bs', w) -> Just (bs', w2c w)
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: ByteString -> Char
last = w2c . L.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> ByteString -> ByteString
map f = L.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'ByteString'
-- and \`intersperses\' that Char between the elements of the
-- 'ByteString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
intersperse = L.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = L.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl\'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = L.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = L.foldr (\c a -> f (w2c c) a)
{-# INLINE foldr #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (L.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (L.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (L.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = L.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f = L.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = L.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum = w2c . L.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Char
minimum = w2c . L.minimum
{-# INLINE minimum #-}

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
scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = L.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = L.mapAccumL (\a w -> case f a (w2c w) of (a',c) -> (a', c2w c))

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = L.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

------------------------------------------------------------------------
-- Generating and unfolding ByteStrings

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Char -> Char) -> Char -> ByteString
iterate f = L.iterate (c2w . f . w2c) . c2w

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Char -> ByteString
repeat = L.repeat . c2w

-- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- the value of every element.
--
replicate :: Int64 -> Char -> ByteString
replicate w c = L.replicate w (c2w c)

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f = L.unfoldr $ \a -> case f a of
                                    Nothing      -> Nothing
                                    Just (c, a') -> Just (c2w c, a')

------------------------------------------------------------------------

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = L.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = L.dropWhile (f . w2c)
{-# INLINE dropWhile #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = L.break (f . w2c)
{-# INLINE break #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = L.span (f . w2c)
{-# INLINE span #-}

{-
-- | 'breakChar' breaks its ByteString argument at the first occurence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar = L.breakByte . c2w
{-# INLINE breakChar #-}

-- | 'spanChar' breaks its ByteString argument at the first
-- occurence of a Char other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanChar :: Char -> ByteString -> (ByteString, ByteString)
spanChar = L.spanByte . c2w
{-# INLINE spanChar #-}
-}

--
-- TODO, more rules for breakChar*
--

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
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
split :: Char -> ByteString -> [ByteString]
split = L.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = L.splitWith (f . w2c)
{-# INLINE splitWith #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = L.groupBy (\a b -> k (w2c a) (w2c b))

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int64 -> Char
index = (w2c .) . L.index
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int64
elemIndex = L.elemIndex . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int64]
elemIndices = L.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int64
findIndex f = L.findIndex (f . w2c)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int64]
findIndices f = L.findIndices (f . w2c)

-- | count returns the number of times its argument appears in the ByteString
--
-- > count      == length . elemIndices
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> ByteString -> Int64
count c = L.count (c2w c)

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem c = L.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c = L.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = L.filter (f . w2c)
{-# INLINE filter #-}

{-
-- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- (==)/, for the common case of filtering a single Char. It is more
-- efficient to use /filterChar/ in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> ByteString -> ByteString
filterChar c ps = replicate (count c ps) c
{-# INLINE filterChar #-}

{-# RULES
  "ByteString specialise filter (== x)" forall x.
      filter ((==) x) = filterChar x
  #-}

{-# RULES
  "ByteString specialise filter (== x)" forall x.
     filter (== x) = filterChar x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` L.find (f . w2c) ps
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
filterChar c = L.filterByte (c2w c)
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
filterNotChar c = L.filterNotByte (c2w c)
{-# INLINE filterNotChar #-}
-}

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of Chars. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte ByteStrings
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | L.null ps || L.null qs = []
    | otherwise = (head ps, head qs) : zip (L.tail ps) (L.tail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = L.zipWith ((. w2c) . f . w2c)

-- | 'lines' breaks a ByteString up into a list of ByteStrings at
-- newline Chars. The resulting strings do not contain newlines.
--
-- As of bytestring 0.9.0.3, this function is stricter than its 
-- list cousin.
--
lines :: ByteString -> [ByteString]
lines Empty          = []
lines (Chunk c0 cs0) = loop0 c0 cs0
    where
    -- this is a really performance sensitive function but the
    -- chunked representation makes the general case a bit expensive
    -- however assuming a large chunk size and normalish line lengths
    -- we will find line endings much more frequently than chunk
    -- endings so it makes sense to optimise for that common case.
    -- So we partition into two special cases depending on whether we
    -- are keeping back a list of chunks that will eventually be output
    -- once we get to the end of the current line.

    -- the common special case where we have no existing chunks of
    -- the current line
    loop0 :: S.ByteString -> ByteString -> [ByteString]
    loop0 c cs =
        case B.elemIndex (c2w '\n') c of
            Nothing -> case cs of
                           Empty  | B.null c  ->                 []
                                  | otherwise -> Chunk c Empty : []
                           (Chunk c' cs')
                               | B.null c  -> loop0 c'     cs'
                               | otherwise -> loop  c' [c] cs'

            Just n | n /= 0    -> Chunk (B.unsafeTake n c) Empty
                                : loop0 (B.unsafeDrop (n+1) c) cs
                   | otherwise -> Empty
                                : loop0 (B.unsafeTail c) cs

    -- the general case when we are building a list of chunks that are
    -- part of the same line
    loop :: S.ByteString -> [S.ByteString] -> ByteString -> [ByteString]
    loop c line cs =
        case B.elemIndex (c2w '\n') c of
            Nothing ->
                case cs of
                    Empty -> let c' = revChunks (c : line)
                              in c' `seq` (c' : [])

                    (Chunk c' cs') -> loop c' (c : line) cs'

            Just n ->
                let c' = revChunks (B.unsafeTake n c : line)
                 in c' `seq` (c' : loop0 (B.unsafeDrop (n+1) c) cs)

{-

This function is too strict!  Consider,

> prop_lazy =
    (L.unpack . head . lazylines $ L.append (L.pack "a\nb\n") (error "failed"))
  ==
    "a"

fails.  Here's a properly lazy version of 'lines' for lazy bytestrings

    lazylines           :: L.ByteString -> [L.ByteString]
    lazylines s
        | L.null s  = []
        | otherwise =
            let (l,s') = L.break ((==) '\n') s
            in l : if L.null s' then []
                                else lazylines (L.tail s')

we need a similarly lazy, but efficient version.

-}


-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [ByteString] -> ByteString
unlines [] = empty
unlines ss = concat (List.intersperse nl ss) `append` nl -- half as much space
    where nl = singleton '\n'

-- | 'words' breaks a ByteString up into a list of words, which
-- were delimited by Chars representing white space. And
--
-- > tokens isSpace = words
--
words :: ByteString -> [ByteString]
words = List.filter (not . L.null) . L.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [ByteString] -> ByteString
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | readInt reads an Int from the beginning of the ByteString.  If
-- there is no integer at the beginning of the string, it returns
-- Nothing, otherwise it just returns the int read, and the rest of the
-- string.

readInt :: ByteString -> Maybe (Int, ByteString)
{-# INLINE readInt #-}
readInt Empty        = Nothing
readInt (Chunk x xs) = case w2c (B.unsafeHead x) of
    '-' -> loop True  0 0 (B.unsafeTail x) xs
    '+' -> loop False 0 0 (B.unsafeTail x) xs
    _   -> loop False 0 0 x xs

    where loop :: Bool -> Int -> Int
                -> S.ByteString -> ByteString -> Maybe (Int, ByteString)
          loop neg !i !n !c cs
              | B.null c = case cs of
                             Empty          -> end  neg i n c  cs
                             (Chunk c' cs') -> loop neg i n c' cs'
              | otherwise =
                  case B.unsafeHead c of
                    w | w >= 0x30
                     && w <= 0x39 -> loop neg (i+1)
                                          (n * 10 + (fromIntegral w - 0x30))
                                          (B.unsafeTail c) cs
                      | otherwise -> end neg i n c cs

          {-# INLINE end #-}
          end _   0 _ _  _ = Nothing
          end neg _ n c cs = e
                where n' = if neg then negate n else n
                      c' = chunk c cs
                      e  = n' `seq` c' `seq` Just (n',c')
         --                  in n' `seq` c' `seq` JustS n' c'


-- | readInteger reads an Integer from the beginning of the ByteString.  If
-- there is no integer at the beginning of the string, it returns Nothing,
-- otherwise it just returns the int read, and the rest of the string.
readInteger :: ByteString -> Maybe (Integer, ByteString)
readInteger Empty = Nothing
readInteger (Chunk c0 cs0) =
        case w2c (B.unsafeHead c0) of
            '-' -> first (B.unsafeTail c0) cs0 >>= \(n, cs') -> return (-n, cs')
            '+' -> first (B.unsafeTail c0) cs0
            _   -> first c0 cs0

    where first c cs
              | B.null c = case cs of
                  Empty          -> Nothing
                  (Chunk c' cs') -> first' c' cs'
              | otherwise = first' c cs

          first' c cs = case B.unsafeHead c of
              w | w >= 0x30 && w <= 0x39 -> Just $
                  loop 1 (fromIntegral w - 0x30) [] (B.unsafeTail c) cs
                | otherwise              -> Nothing

          loop :: Int -> Int -> [Integer]
               -> S.ByteString -> ByteString -> (Integer, ByteString)
          loop !d !acc ns !c cs
              | B.null c = case cs of
                             Empty          -> combine d acc ns c cs
                             (Chunk c' cs') -> loop d acc ns c' cs'
              | otherwise =
                  case B.unsafeHead c of
                   w | w >= 0x30 && w <= 0x39 ->
                       if d < 9 then loop (d+1)
                                          (10*acc + (fromIntegral w - 0x30))
                                          ns (B.unsafeTail c) cs
                                else loop 1 (fromIntegral w - 0x30)
                                          (fromIntegral acc : ns)
                                          (B.unsafeTail c) cs
                     | otherwise -> combine d acc ns c cs

          combine _ acc [] c cs = end (fromIntegral acc) c cs
          combine d acc ns c cs =
              end (10^d * combine1 1000000000 ns + fromIntegral acc) c cs

          combine1 _ [n] = n
          combine1 b ns  = combine1 (b*b) $ combine2 b ns

          combine2 b (n:m:ns) = let t = n+m*b in t `seq` (t : combine2 b ns)
          combine2 _ ns       = ns

          end n c cs = let c' = chunk c cs
                        in c' `seq` (n, c')


-- | Write a ByteString to a handle, appending a newline byte
--
hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps = hPut h ps >> hPut h (L.singleton 0x0a)

-- | Write a ByteString to stdout, appending a newline byte
--
putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout

-- ---------------------------------------------------------------------
-- Internal utilities

-- reverse a list of possibly-empty chunks into a lazy ByteString
revChunks :: [S.ByteString] -> ByteString
revChunks cs = List.foldl' (flip chunk) Empty cs
