-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Highly random utility functions
--
module Util (
        -- * Flags dependent on the compiler build
        ghciSupported, debugIsOn,
        isWindowsHost, isDarwinHost,

        -- * Miscellaneous higher-order functions
        applyWhen, nTimes,

        -- * General list processing
        zipEqual, zipWithEqual, zipWith3Equal, zipWith4Equal,
        zipLazy, stretchZipWith, zipWithAndUnzip, zipAndUnzip,

        zipWithLazy, zipWith3Lazy,

        filterByList, filterByLists, partitionByList,

        unzipWith,

        mapFst, mapSnd, chkAppend,
        mapAndUnzip, mapAndUnzip3, mapAccumL2,
        filterOut, partitionWith,

        dropWhileEndLE, spanEnd, last2, lastMaybe,

        foldl1', foldl2, count, countWhile, all2,

        lengthExceeds, lengthIs, lengthIsNot,
        lengthAtLeast, lengthAtMost, lengthLessThan,
        listLengthCmp, atLength,
        equalLength, compareLength, leLength, ltLength,

        isSingleton, only, singleton,
        notNull, snocView,

        isIn, isn'tIn,

        chunkList,

        changeLast,

        whenNonEmpty,

        -- * Tuples
        fstOf3, sndOf3, thdOf3,
        firstM, first3M, secondM,
        fst3, snd3, third3,
        uncurry3,
        liftFst, liftSnd,

        -- * List operations controlled by another list
        takeList, dropList, splitAtList, split,
        dropTail, capitalise,

        -- * Sorting
        sortWith, minWith, nubSort, ordNub,

        -- * Comparisons
        isEqual, eqListBy, eqMaybeBy,
        thenCmp, cmpList,
        removeSpaces,
        (<&&>), (<||>),

        -- * Edit distance
        fuzzyMatch, fuzzyLookup,

        -- * Transitive closures
        transitiveClosure,

        -- * Strictness
        seqList, strictMap,

        -- * Module names
        looksLikeModuleName,
        looksLikePackageName,

        -- * Argument processing
        getCmd, toCmdArgs, toArgs,

        -- * Integers
        exactLog2,

        -- * Floating point
        readRational,
        readHexRational,

        -- * IO-ish utilities
        doesDirNameExist,
        getModificationUTCTime,
        modificationTimeIfExists,
        withAtomicRename,

        global, consIORef, globalM,
        sharedGlobal, sharedGlobalM,

        -- * Filenames and paths
        Suffix,
        splitLongestPrefix,
        escapeSpaces,
        Direction(..), reslash,
        makeRelativeTo,

        -- * Utils for defining Data instances
        abstractConstr, abstractDataType, mkNoRepType,

        -- * Utils for printing C code
        charToC,

        -- * Hashing
        hashString,

        -- * Call stacks
        HasCallStack,
        HasDebugCallStack,

        -- * Utils for flags
        OverridingBool(..),
        overrideWith,
    ) where

#include "HsVersions.h"

import GhcPrelude

import Exception
import PlainPanic

import Data.Data
import Data.IORef       ( IORef, newIORef, atomicModifyIORef' )
import System.IO.Unsafe ( unsafePerformIO )
import Data.List        hiding (group)
import Data.List.NonEmpty  ( NonEmpty(..) )

import GHC.Exts
import GHC.Stack (HasCallStack)

import Control.Applicative ( liftA2 )
import Control.Monad    ( liftM, guard )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import GHC.Conc.Sync ( sharedCAF )
import System.IO.Error as IO ( isDoesNotExistError )
import System.Directory ( doesDirectoryExist, getModificationTime, renameFile )
import System.FilePath

import Data.Char        ( isUpper, isAlphaNum, isSpace, chr, ord, isDigit, toUpper
                        , isHexDigit, digitToInt )
import Data.Int
import Data.Ratio       ( (%) )
import Data.Ord         ( comparing )
import Data.Bits
import Data.Word
import qualified Data.IntMap as IM
import qualified Data.Set as Set

import Data.Time

#if defined(DEBUG)
import {-# SOURCE #-} Outputable ( warnPprTrace, text )
#endif

infixr 9 `thenCmp`

{-
************************************************************************
*                                                                      *
\subsection{Is DEBUG on, are we on Windows, etc?}
*                                                                      *
************************************************************************

These booleans are global constants, set by CPP flags.  They allow us to
recompile a single module (this one) to change whether or not debug output
appears. They sometimes let us avoid even running CPP elsewhere.

It's important that the flags are literal constants (True/False). Then,
with -0, tests of the flags in other modules will simplify to the correct
branch of the conditional, thereby dropping debug code altogether when
the flags are off.
-}

ghciSupported :: Bool
#if defined(HAVE_INTERNAL_INTERPRETER)
ghciSupported = True
#else
ghciSupported = False
#endif

debugIsOn :: Bool
#if defined(DEBUG)
debugIsOn = True
#else
debugIsOn = False
#endif

isWindowsHost :: Bool
#if defined(mingw32_HOST_OS)
isWindowsHost = True
#else
isWindowsHost = False
#endif

isDarwinHost :: Bool
#if defined(darwin_HOST_OS)
isDarwinHost = True
#else
isDarwinHost = False
#endif

{-
************************************************************************
*                                                                      *
\subsection{Miscellaneous higher-order functions}
*                                                                      *
************************************************************************
-}

-- | Apply a function iff some condition is met.
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen _    _ x = x

-- | A for loop: Compose a function with itself n times.  (nth rather than twice)
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

fstOf3   :: (a,b,c) -> a
sndOf3   :: (a,b,c) -> b
thdOf3   :: (a,b,c) -> c
fstOf3      (a,_,_) =  a
sndOf3      (_,b,_) =  b
thdOf3      (_,_,c) =  c

fst3 :: (a -> d) -> (a, b, c) -> (d, b, c)
fst3 f (a, b, c) = (f a, b, c)

snd3 :: (b -> d) -> (a, b, c) -> (a, d, c)
snd3 f (a, b, c) = (a, f b, c)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (a,c) = (f a, c)

liftSnd :: (a -> b) -> (c, a) -> (c, b)
liftSnd f (c,a) = (c, f a)

firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f (x, y) = liftM (\x' -> (x', y)) (f x)

first3M :: Monad m => (a -> m d) -> (a, b, c) -> m (d, b, c)
first3M f (x, y, z) = liftM (\x' -> (x', y, z)) (f x)

secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (x, y) = (x,) <$> f y

{-
************************************************************************
*                                                                      *
\subsection[Utils-lists]{General list processing}
*                                                                      *
************************************************************************
-}

filterOut :: (a->Bool) -> [a] -> [a]
-- ^ Like filter, only it reverses the sense of the test
filterOut _ [] = []
filterOut p (x:xs) | p x       = filterOut p xs
                   | otherwise = x : filterOut p xs

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
-- ^ Uses a function to determine which of two output lists an input element should join
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

chkAppend :: [a] -> [a] -> [a]
-- Checks for the second argument being empty
-- Used in situations where that situation is common
chkAppend xs ys
  | null ys   = xs
  | otherwise = xs ++ ys

{-
A paranoid @zip@ (and some @zipWith@ friends) that checks the lists
are of equal length.  Alastair Reid thinks this should only happen if
DEBUGging on; hey, why not?
-}

zipEqual        :: String -> [a] -> [b] -> [(a,b)]
zipWithEqual    :: String -> (a->b->c) -> [a]->[b]->[c]
zipWith3Equal   :: String -> (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith4Equal   :: String -> (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]

#if !defined(DEBUG)
zipEqual      _ = zip
zipWithEqual  _ = zipWith
zipWith3Equal _ = zipWith3
zipWith4Equal _ = zipWith4
#else
zipEqual _   []     []     = []
zipEqual msg (a:as) (b:bs) = (a,b) : zipEqual msg as bs
zipEqual msg _      _      = panic ("zipEqual: unequal lists: "++msg)

zipWithEqual msg z (a:as) (b:bs)=  z a b : zipWithEqual msg z as bs
zipWithEqual _   _ [] []        =  []
zipWithEqual msg _ _ _          =  panic ("zipWithEqual: unequal lists: "++msg)

zipWith3Equal msg z (a:as) (b:bs) (c:cs)
                                =  z a b c : zipWith3Equal msg z as bs cs
zipWith3Equal _   _ [] []  []   =  []
zipWith3Equal msg _ _  _   _    =  panic ("zipWith3Equal: unequal lists: "++msg)

zipWith4Equal msg z (a:as) (b:bs) (c:cs) (d:ds)
                                =  z a b c d : zipWith4Equal msg z as bs cs ds
zipWith4Equal _   _ [] [] [] [] =  []
zipWith4Equal msg _ _  _  _  _  =  panic ("zipWith4Equal: unequal lists: "++msg)
#endif

-- | 'zipLazy' is a kind of 'zip' that is lazy in the second list (observe the ~)
zipLazy :: [a] -> [b] -> [(a,b)]
zipLazy []     _       = []
zipLazy (x:xs) ~(y:ys) = (x,y) : zipLazy xs ys

-- | 'zipWithLazy' is like 'zipWith' but is lazy in the second list.
-- The length of the output is always the same as the length of the first
-- list.
zipWithLazy :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLazy _ []     _       = []
zipWithLazy f (a:as) ~(b:bs) = f a b : zipWithLazy f as bs

-- | 'zipWith3Lazy' is like 'zipWith3' but is lazy in the second and third lists.
-- The length of the output is always the same as the length of the first
-- list.
zipWith3Lazy :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3Lazy _ []     _       _       = []
zipWith3Lazy f (a:as) ~(b:bs) ~(c:cs) = f a b c : zipWith3Lazy f as bs cs

-- | 'filterByList' takes a list of Bools and a list of some elements and
-- filters out these elements for which the corresponding value in the list of
-- Bools is False. This function does not check whether the lists have equal
-- length.
filterByList :: [Bool] -> [a] -> [a]
filterByList (True:bs)  (x:xs) = x : filterByList bs xs
filterByList (False:bs) (_:xs) =     filterByList bs xs
filterByList _          _      = []

-- | 'filterByLists' takes a list of Bools and two lists as input, and
-- outputs a new list consisting of elements from the last two input lists. For
-- each Bool in the list, if it is 'True', then it takes an element from the
-- former list. If it is 'False', it takes an element from the latter list.
-- The elements taken correspond to the index of the Bool in its list.
-- For example:
--
-- @
-- filterByLists [True, False, True, False] \"abcd\" \"wxyz\" = \"axcz\"
-- @
--
-- This function does not check whether the lists have equal length.
filterByLists :: [Bool] -> [a] -> [a] -> [a]
filterByLists (True:bs)  (x:xs) (_:ys) = x : filterByLists bs xs ys
filterByLists (False:bs) (_:xs) (y:ys) = y : filterByLists bs xs ys
filterByLists _          _      _      = []

-- | 'partitionByList' takes a list of Bools and a list of some elements and
-- partitions the list according to the list of Bools. Elements corresponding
-- to 'True' go to the left; elements corresponding to 'False' go to the right.
-- For example, @partitionByList [True, False, True] [1,2,3] == ([1,3], [2])@
-- This function does not check whether the lists have equal
-- length; when one list runs out, the function stops.
partitionByList :: [Bool] -> [a] -> ([a], [a])
partitionByList = go [] []
  where
    go trues falses (True  : bs) (x : xs) = go (x:trues) falses bs xs
    go trues falses (False : bs) (x : xs) = go trues (x:falses) bs xs
    go trues falses _ _ = (reverse trues, reverse falses)

stretchZipWith :: (a -> Bool) -> b -> (a->b->c) -> [a] -> [b] -> [c]
-- ^ @stretchZipWith p z f xs ys@ stretches @ys@ by inserting @z@ in
-- the places where @p@ returns @True@

stretchZipWith _ _ _ []     _ = []
stretchZipWith p z f (x:xs) ys
  | p x       = f x z : stretchZipWith p z f xs ys
  | otherwise = case ys of
                []     -> []
                (y:ys) -> f x y : stretchZipWith p z f xs ys

mapFst :: (a->c) -> [(a,b)] -> [(c,b)]
mapSnd :: (b->c) -> [(a,b)] -> [(a,c)]

mapFst f xys = [(f x, y) | (x,y) <- xys]
mapSnd f xys = [(x, f y) | (x,y) <- xys]

mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])

mapAndUnzip _ [] = ([], [])
mapAndUnzip f (x:xs)
  = let (r1,  r2)  = f x
        (rs1, rs2) = mapAndUnzip f xs
    in
    (r1:rs1, r2:rs2)

mapAndUnzip3 :: (a -> (b, c, d)) -> [a] -> ([b], [c], [d])

mapAndUnzip3 _ [] = ([], [], [])
mapAndUnzip3 f (x:xs)
  = let (r1,  r2,  r3)  = f x
        (rs1, rs2, rs3) = mapAndUnzip3 f xs
    in
    (r1:rs1, r2:rs2, r3:rs3)

zipWithAndUnzip :: (a -> b -> (c,d)) -> [a] -> [b] -> ([c],[d])
zipWithAndUnzip f (a:as) (b:bs)
  = let (r1,  r2)  = f a b
        (rs1, rs2) = zipWithAndUnzip f as bs
    in
    (r1:rs1, r2:rs2)
zipWithAndUnzip _ _ _ = ([],[])

-- | This has the effect of making the two lists have equal length by dropping
-- the tail of the longer one.
zipAndUnzip :: [a] -> [b] -> ([a],[b])
zipAndUnzip (a:as) (b:bs)
  = let (rs1, rs2) = zipAndUnzip as bs
    in
    (a:rs1, b:rs2)
zipAndUnzip _ _ = ([],[])

mapAccumL2 :: (s1 -> s2 -> a -> (s1, s2, b)) -> s1 -> s2 -> [a] -> (s1, s2, [b])
mapAccumL2 f s1 s2 xs = (s1', s2', ys)
  where ((s1', s2'), ys) = mapAccumL (\(s1, s2) x -> case f s1 s2 x of
                                                       (s1', s2', y) -> ((s1', s2'), y))
                                     (s1, s2) xs

-- | @atLength atLen atEnd ls n@ unravels list @ls@ to position @n@. Precisely:
--
-- @
--  atLength atLenPred atEndPred ls n
--   | n < 0         = atLenPred ls
--   | length ls < n = atEndPred (n - length ls)
--   | otherwise     = atLenPred (drop n ls)
-- @
atLength :: ([a] -> b)   -- Called when length ls >= n, passed (drop n ls)
                         --    NB: arg passed to this function may be []
         -> b            -- Called when length ls <  n
         -> [a]
         -> Int
         -> b
atLength atLenPred atEnd ls0 n0
  | n0 < 0    = atLenPred ls0
  | otherwise = go n0 ls0
  where
    -- go's first arg n >= 0
    go 0 ls     = atLenPred ls
    go _ []     = atEnd           -- n > 0 here
    go n (_:xs) = go (n-1) xs

-- Some special cases of atLength:

-- | @(lengthExceeds xs n) = (length xs > n)@
lengthExceeds :: [a] -> Int -> Bool
lengthExceeds lst n
  | n < 0
  = True
  | otherwise
  = atLength notNull False lst n

-- | @(lengthAtLeast xs n) = (length xs >= n)@
lengthAtLeast :: [a] -> Int -> Bool
lengthAtLeast = atLength (const True) False

-- | @(lengthIs xs n) = (length xs == n)@
lengthIs :: [a] -> Int -> Bool
lengthIs lst n
  | n < 0
  = False
  | otherwise
  = atLength null False lst n

-- | @(lengthIsNot xs n) = (length xs /= n)@
lengthIsNot :: [a] -> Int -> Bool
lengthIsNot lst n
  | n < 0 = True
  | otherwise = atLength notNull True lst n

-- | @(lengthAtMost xs n) = (length xs <= n)@
lengthAtMost :: [a] -> Int -> Bool
lengthAtMost lst n
  | n < 0
  = False
  | otherwise
  = atLength null True lst n

-- | @(lengthLessThan xs n) == (length xs < n)@
lengthLessThan :: [a] -> Int -> Bool
lengthLessThan = atLength (const False) True

listLengthCmp :: [a] -> Int -> Ordering
listLengthCmp = atLength atLen atEnd
 where
  atEnd = LT    -- Not yet seen 'n' elts, so list length is < n.

  atLen []     = EQ
  atLen _      = GT

equalLength :: [a] -> [b] -> Bool
-- ^ True if length xs == length ys
equalLength []     []     = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength _      _      = False

compareLength :: [a] -> [b] -> Ordering
compareLength []     []     = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength []     _      = LT
compareLength _      []     = GT

leLength :: [a] -> [b] -> Bool
-- ^ True if length xs <= length ys
leLength xs ys = case compareLength xs ys of
                   LT -> True
                   EQ -> True
                   GT -> False

ltLength :: [a] -> [b] -> Bool
-- ^ True if length xs < length ys
ltLength xs ys = case compareLength xs ys of
                   LT -> True
                   EQ -> False
                   GT -> False

----------------------------
singleton :: a -> [a]
singleton x = [x]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

notNull :: [a] -> Bool
notNull [] = False
notNull _  = True

only :: [a] -> a
#if defined(DEBUG)
only [a] = a
#else
only (a:_) = a
#endif
only _ = panic "Util: only"

-- Debugging/specialising versions of \tr{elem} and \tr{notElem}

# if !defined(DEBUG)
isIn, isn'tIn :: Eq a => String -> a -> [a] -> Bool
isIn    _msg x ys = x `elem` ys
isn'tIn _msg x ys = x `notElem` ys

# else /* DEBUG */
isIn, isn'tIn :: (HasDebugCallStack, Eq a) => String -> a -> [a] -> Bool
isIn msg x ys
  = elem100 0 x ys
  where
    elem100 :: Eq a => Int -> a -> [a] -> Bool
    elem100 _ _ [] = False
    elem100 i x (y:ys)
      | i > 100 = WARN(True, text ("Over-long elem in " ++ msg)) (x `elem` (y:ys))
      | otherwise = x == y || elem100 (i + 1) x ys

isn'tIn msg x ys
  = notElem100 0 x ys
  where
    notElem100 :: Eq a => Int -> a -> [a] -> Bool
    notElem100 _ _ [] =  True
    notElem100 i x (y:ys)
      | i > 100 = WARN(True, text ("Over-long notElem in " ++ msg)) (x `notElem` (y:ys))
      | otherwise = x /= y && notElem100 (i + 1) x ys
# endif /* DEBUG */


-- | Split a list into chunks of /n/ elements
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

-- | Replace the last element of a list with another element.
changeLast :: [a] -> a -> [a]
changeLast []     _  = panic "changeLast"
changeLast [_]    x  = [x]
changeLast (x:xs) x' = x : changeLast xs x'

whenNonEmpty :: Applicative m => [a] -> (NonEmpty a -> m ()) -> m ()
whenNonEmpty []     _ = pure ()
whenNonEmpty (x:xs) f = f (x :| xs)

{-
************************************************************************
*                                                                      *
\subsubsection{Sort utils}
*                                                                      *
************************************************************************
-}

minWith :: Ord b => (a -> b) -> [a] -> a
minWith get_key xs = ASSERT( not (null xs) )
                     head (sortWith get_key xs)

nubSort :: Ord a => [a] -> [a]
nubSort = Set.toAscList . Set.fromList

-- | Remove duplicates but keep elements in order.
--   O(n * log n)
ordNub :: Ord a => [a] -> [a]
ordNub xs
  = go Set.empty xs
  where
    go _ [] = []
    go s (x:xs)
      | Set.member x s = go s xs
      | otherwise = x : go (Set.insert x s) xs

{-
************************************************************************
*                                                                      *
\subsection[Utils-transitive-closure]{Transitive closure}
*                                                                      *
************************************************************************

This algorithm for transitive closure is straightforward, albeit quadratic.
-}

transitiveClosure :: (a -> [a])         -- Successor function
                  -> (a -> a -> Bool)   -- Equality predicate
                  -> [a]
                  -> [a]                -- The transitive closure

transitiveClosure succ eq xs
 = go [] xs
 where
   go done []                      = done
   go done (x:xs) | x `is_in` done = go done xs
                  | otherwise      = go (x:done) (succ x ++ xs)

   _ `is_in` []                 = False
   x `is_in` (y:ys) | eq x y    = True
                    | otherwise = x `is_in` ys

{-
************************************************************************
*                                                                      *
\subsection[Utils-accum]{Accumulating}
*                                                                      *
************************************************************************

A combination of foldl with zip.  It works with equal length lists.
-}

foldl2 :: (acc -> a -> b -> acc) -> acc -> [a] -> [b] -> acc
foldl2 _ z [] [] = z
foldl2 k z (a:as) (b:bs) = foldl2 k (k z a b) as bs
foldl2 _ _ _      _      = panic "Util: foldl2"

all2 :: (a -> b -> Bool) -> [a] -> [b] -> Bool
-- True if the lists are the same length, and
-- all corresponding elements satisfy the predicate
all2 _ []     []     = True
all2 p (x:xs) (y:ys) = p x y && all2 p xs ys
all2 _ _      _      = False

-- Count the number of times a predicate is true

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

countWhile :: (a -> Bool) -> [a] -> Int
-- Length of an /initial prefix/ of the list satisfying p
countWhile p = go 0
  where go !n (x:xs) | p x = go (n+1) xs
        go !n _            = n

{-
@splitAt@, @take@, and @drop@ but with length of another
list giving the break-off point:
-}

takeList :: [b] -> [a] -> [a]
-- (takeList as bs) trims bs to the be same length
-- as as, unless as is longer in which case it's a no-op
takeList [] _ = []
takeList (_:xs) ls =
   case ls of
     [] -> []
     (y:ys) -> y : takeList xs ys

dropList :: [b] -> [a] -> [a]
dropList [] xs    = xs
dropList _  xs@[] = xs
dropList (_:xs) (_:ys) = dropList xs ys


splitAtList :: [b] -> [a] -> ([a], [a])
splitAtList [] xs     = ([], xs)
splitAtList _ xs@[]   = (xs, xs)
splitAtList (_:xs) (y:ys) = (y:ys', ys'')
    where
      (ys', ys'') = splitAtList xs ys

-- drop from the end of a list
dropTail :: Int -> [a] -> [a]
-- Specification: dropTail n = reverse . drop n . reverse
-- Better implemention due to Joachim Breitner
-- http://www.joachim-breitner.de/blog/archives/600-On-taking-the-last-n-elements-of-a-list.html
dropTail n xs
  = go (drop n xs) xs
  where
    go (_:ys) (x:xs) = x : go ys xs
    go _      _      = []  -- Stop when ys runs out
                           -- It'll always run out before xs does

-- dropWhile from the end of a list. This is similar to Data.List.dropWhileEnd,
-- but is lazy in the elements and strict in the spine. For reasonably short lists,
-- such as path names and typical lines of text, dropWhileEndLE is generally
-- faster than dropWhileEnd. Its advantage is magnified when the predicate is
-- expensive--using dropWhileEndLE isSpace to strip the space off a line of text
-- is generally much faster than using dropWhileEnd isSpace for that purpose.
-- Specification: dropWhileEndLE p = reverse . dropWhile p . reverse
-- Pay attention to the short-circuit (&&)! The order of its arguments is the only
-- difference between dropWhileEnd and dropWhileEndLE.
dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []

-- | @spanEnd p l == reverse (span p (reverse l))@. The first list
-- returns actually comes after the second list (when you look at the
-- input list).
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = go l [] [] l
  where go yes _rev_yes rev_no [] = (yes, reverse rev_no)
        go yes rev_yes  rev_no (x:xs)
          | p x       = go yes (x : rev_yes) rev_no                  xs
          | otherwise = go xs  []            (x : rev_yes ++ rev_no) xs

-- | Get the last two elements in a list. Partial!
{-# INLINE last2 #-}
last2 :: [a] -> (a,a)
last2 = foldl' (\(_,x2) x -> (x2,x)) (partialError,partialError)
  where
    partialError = panic "last2 - list length less than two"

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

-- | Split a list into its last element and the initial part of the list.
-- @snocView xs = Just (init xs, last xs)@ for non-empty lists.
-- @snocView xs = Nothing@ otherwise.
-- Unless both parts of the result are guaranteed to be used
-- prefer separate calls to @last@ + @init@.
-- If you are guaranteed to use both, this will
-- be more efficient.
snocView :: [a] -> Maybe ([a],a)
snocView [] = Nothing
snocView xs
    | (xs,x) <- go xs
    = Just (xs,x)
  where
    go :: [a] -> ([a],a)
    go [x] = ([],x)
    go (x:xs)
        | !(xs',x') <- go xs
        = (x:xs', x')
    go [] = error "impossible"

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

-- | Convert a word to title case by capitalising the first letter
capitalise :: String -> String
capitalise [] = []
capitalise (c:cs) = toUpper c : cs


{-
************************************************************************
*                                                                      *
\subsection[Utils-comparison]{Comparisons}
*                                                                      *
************************************************************************
-}

isEqual :: Ordering -> Bool
-- Often used in (isEqual (a `compare` b))
isEqual GT = False
isEqual EQ = True
isEqual LT = False

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering

eqListBy :: (a->a->Bool) -> [a] -> [a] -> Bool
eqListBy _  []     []     = True
eqListBy eq (x:xs) (y:ys) = eq x y && eqListBy eq xs ys
eqListBy _  _      _      = False

eqMaybeBy :: (a ->a->Bool) -> Maybe a -> Maybe a -> Bool
eqMaybeBy _  Nothing  Nothing  = True
eqMaybeBy eq (Just x) (Just y) = eq x y
eqMaybeBy _  _        _        = False

cmpList :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
    -- `cmpList' uses a user-specified comparer

cmpList _   []     [] = EQ
cmpList _   []     _  = LT
cmpList _   _      [] = GT
cmpList cmp (a:as) (b:bs)
  = case cmp a b of { EQ -> cmpList cmp as bs; xxx -> xxx }

removeSpaces :: String -> String
removeSpaces = dropWhileEndLE isSpace . dropWhile isSpace

-- Boolean operators lifted to Applicative
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&> -- same as (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||> -- same as (||)

{-
************************************************************************
*                                                                      *
\subsection{Edit distance}
*                                                                      *
************************************************************************
-}

-- | Find the "restricted" Damerau-Levenshtein edit distance between two strings.
-- See: <http://en.wikipedia.org/wiki/Damerau-Levenshtein_distance>.
-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing
-- Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and
--     http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
restrictedDamerauLevenshteinDistance :: String -> String -> Int
restrictedDamerauLevenshteinDistance str1 str2
  = restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  where
    m = length str1
    n = length str2

restrictedDamerauLevenshteinDistanceWithLengths
  :: Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  | m <= n
  = if n <= 32 -- n must be larger so this check is sufficient
    then restrictedDamerauLevenshteinDistance' (undefined :: Word32) m n str1 str2
    else restrictedDamerauLevenshteinDistance' (undefined :: Integer) m n str1 str2

  | otherwise
  = if m <= 32 -- m must be larger so this check is sufficient
    then restrictedDamerauLevenshteinDistance' (undefined :: Word32) n m str2 str1
    else restrictedDamerauLevenshteinDistance' (undefined :: Integer) n m str2 str1

restrictedDamerauLevenshteinDistance'
  :: (Bits bv, Num bv) => bv -> Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistance' _bv_dummy m n str1 str2
  | [] <- str1 = n
  | otherwise  = extractAnswer $
                 foldl' (restrictedDamerauLevenshteinDistanceWorker
                             (matchVectors str1) top_bit_mask vector_mask)
                        (0, 0, m_ones, 0, m) str2
  where
    m_ones@vector_mask = (2 ^ m) - 1
    top_bit_mask = (1 `shiftL` (m - 1)) `asTypeOf` _bv_dummy
    extractAnswer (_, _, _, _, distance) = distance

restrictedDamerauLevenshteinDistanceWorker
      :: (Bits bv, Num bv) => IM.IntMap bv -> bv -> bv
      -> (bv, bv, bv, bv, Int) -> Char -> (bv, bv, bv, bv, Int)
restrictedDamerauLevenshteinDistanceWorker str1_mvs top_bit_mask vector_mask
                                           (pm, d0, vp, vn, distance) char2
  = seq str1_mvs $ seq top_bit_mask $ seq vector_mask $
    seq pm' $ seq d0' $ seq vp' $ seq vn' $
    seq distance'' $ seq char2 $
    (pm', d0', vp', vn', distance'')
  where
    pm' = IM.findWithDefault 0 (ord char2) str1_mvs

    d0' = ((((sizedComplement vector_mask d0) .&. pm') `shiftL` 1) .&. pm)
      .|. ((((pm' .&. vp) + vp) .&. vector_mask) `xor` vp) .|. pm' .|. vn
          -- No need to mask the shiftL because of the restricted range of pm

    hp' = vn .|. sizedComplement vector_mask (d0' .|. vp)
    hn' = d0' .&. vp

    hp'_shift = ((hp' `shiftL` 1) .|. 1) .&. vector_mask
    hn'_shift = (hn' `shiftL` 1) .&. vector_mask
    vp' = hn'_shift .|. sizedComplement vector_mask (d0' .|. hp'_shift)
    vn' = d0' .&. hp'_shift

    distance' = if hp' .&. top_bit_mask /= 0 then distance + 1 else distance
    distance'' = if hn' .&. top_bit_mask /= 0 then distance' - 1 else distance'

sizedComplement :: Bits bv => bv -> bv -> bv
sizedComplement vector_mask vect = vector_mask `xor` vect

matchVectors :: (Bits bv, Num bv) => String -> IM.IntMap bv
matchVectors = snd . foldl' go (0 :: Int, IM.empty)
  where
    go (ix, im) char = let ix' = ix + 1
                           im' = IM.insertWith (.|.) (ord char) (2 ^ ix) im
                       in seq ix' $ seq im' $ (ix', im')

{-# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance'
                      :: Word32 -> Int -> Int -> String -> String -> Int #-}
{-# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance'
                      :: Integer -> Int -> Int -> String -> String -> Int #-}

{-# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker
               :: IM.IntMap Word32 -> Word32 -> Word32
               -> (Word32, Word32, Word32, Word32, Int)
               -> Char -> (Word32, Word32, Word32, Word32, Int) #-}
{-# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker
               :: IM.IntMap Integer -> Integer -> Integer
               -> (Integer, Integer, Integer, Integer, Int)
               -> Char -> (Integer, Integer, Integer, Integer, Int) #-}

{-# SPECIALIZE INLINE sizedComplement :: Word32 -> Word32 -> Word32 #-}
{-# SPECIALIZE INLINE sizedComplement :: Integer -> Integer -> Integer #-}

{-# SPECIALIZE matchVectors :: String -> IM.IntMap Word32 #-}
{-# SPECIALIZE matchVectors :: String -> IM.IntMap Integer #-}

fuzzyMatch :: String -> [String] -> [String]
fuzzyMatch key vals = fuzzyLookup key [(v,v) | v <- vals]

-- | Search for possible matches to the users input in the given list,
-- returning a small number of ranked results
fuzzyLookup :: String -> [(String,a)] -> [a]
fuzzyLookup user_entered possibilites
  = map fst $ take mAX_RESULTS $ sortBy (comparing snd)
    [ (poss_val, distance) | (poss_str, poss_val) <- possibilites
                       , let distance = restrictedDamerauLevenshteinDistance
                                            poss_str user_entered
                       , distance <= fuzzy_threshold ]
  where
    -- Work out an appropriate match threshold:
    -- We report a candidate if its edit distance is <= the threshold,
    -- The threshold is set to about a quarter of the # of characters the user entered
    --   Length    Threshold
    --     1         0          -- Don't suggest *any* candidates
    --     2         1          -- for single-char identifiers
    --     3         1
    --     4         1
    --     5         1
    --     6         2
    --
    fuzzy_threshold = truncate $ fromIntegral (length user_entered + 2) / (4 :: Rational)
    mAX_RESULTS = 3

{-
************************************************************************
*                                                                      *
\subsection[Utils-pairs]{Pairs}
*                                                                      *
************************************************************************
-}

unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs

seqList :: [a] -> b -> b
seqList [] b = b
seqList (x:xs) b = x `seq` seqList xs b

strictMap :: (a -> b) -> [a] -> [b]
strictMap _ [] = []
strictMap f (x : xs) =
  let
    !x' = f x
    !xs' = strictMap f xs
  in
    x' : xs'

{-
************************************************************************
*                                                                      *
                        Globals and the RTS
*                                                                      *
************************************************************************

When a plugin is loaded, it currently gets linked against a *newly
loaded* copy of the GHC package. This would not be a problem, except
that the new copy has its own mutable state that is not shared with
that state that has already been initialized by the original GHC
package.

(Note that if the GHC executable was dynamically linked this
wouldn't be a problem, because we could share the GHC library it
links to; this is only a problem if DYNAMIC_GHC_PROGRAMS=NO.)

The solution is to make use of @sharedCAF@ through @sharedGlobal@
for globals that are shared between multiple copies of ghc packages.
-}

-- Global variables:

global :: a -> IORef a
global a = unsafePerformIO (newIORef a)

consIORef :: IORef [a] -> a -> IO ()
consIORef var x = do
  atomicModifyIORef' var (\xs -> (x:xs,()))

globalM :: IO a -> IORef a
globalM ma = unsafePerformIO (ma >>= newIORef)

-- Shared global variables:

sharedGlobal :: a -> (Ptr (IORef a) -> IO (Ptr (IORef a))) -> IORef a
sharedGlobal a get_or_set = unsafePerformIO $
  newIORef a >>= flip sharedCAF get_or_set

sharedGlobalM :: IO a -> (Ptr (IORef a) -> IO (Ptr (IORef a))) -> IORef a
sharedGlobalM ma get_or_set = unsafePerformIO $
  ma >>= newIORef >>= flip sharedCAF get_or_set

-- Module names:

looksLikeModuleName :: String -> Bool
looksLikeModuleName [] = False
looksLikeModuleName (c:cs) = isUpper c && go cs
  where go [] = True
        go ('.':cs) = looksLikeModuleName cs
        go (c:cs)   = (isAlphaNum c || c == '_' || c == '\'') && go cs

-- Similar to 'parse' for Distribution.Package.PackageName,
-- but we don't want to depend on Cabal.
looksLikePackageName :: String -> Bool
looksLikePackageName = all (all isAlphaNum <&&> not . (all isDigit)) . split '-'

{-
Akin to @Prelude.words@, but acts like the Bourne shell, treating
quoted strings as Haskell Strings, and also parses Haskell [String]
syntax.
-}

getCmd :: String -> Either String             -- Error
                           (String, String) -- (Cmd, Rest)
getCmd s = case break isSpace $ dropWhile isSpace s of
           ([], _) -> Left ("Couldn't find command in " ++ show s)
           res -> Right res

toCmdArgs :: String -> Either String             -- Error
                              (String, [String]) -- (Cmd, Args)
toCmdArgs s = case getCmd s of
              Left err -> Left err
              Right (cmd, s') -> case toArgs s' of
                                 Left err -> Left err
                                 Right args -> Right (cmd, args)

toArgs :: String -> Either String   -- Error
                           [String] -- Args
toArgs str
    = case dropWhile isSpace str of
      s@('[':_) -> case reads s of
                   [(args, spaces)]
                    | all isSpace spaces ->
                       Right args
                   _ ->
                       Left ("Couldn't read " ++ show str ++ " as [String]")
      s -> toArgs' s
 where
  toArgs' :: String -> Either String [String]
  -- Remove outer quotes:
  -- > toArgs' "\"foo\" \"bar baz\""
  -- Right ["foo", "bar baz"]
  --
  -- Keep inner quotes:
  -- > toArgs' "-DFOO=\"bar baz\""
  -- Right ["-DFOO=\"bar baz\""]
  toArgs' s = case dropWhile isSpace s of
              [] -> Right []
              ('"' : _) -> do
                    -- readAsString removes outer quotes
                    (arg, rest) <- readAsString s
                    (arg:) `fmap` toArgs' rest
              s' -> case break (isSpace <||> (== '"')) s' of
                    (argPart1, s''@('"':_)) -> do
                        (argPart2, rest) <- readAsString s''
                        -- show argPart2 to keep inner quotes
                        ((argPart1 ++ show argPart2):) `fmap` toArgs' rest
                    (arg, s'') -> (arg:) `fmap` toArgs' s''

  readAsString :: String -> Either String (String, String)
  readAsString s = case reads s of
                [(arg, rest)]
                    -- rest must either be [] or start with a space
                    | all isSpace (take 1 rest) ->
                    Right (arg, rest)
                _ ->
                    Left ("Couldn't read " ++ show s ++ " as String")
-----------------------------------------------------------------------------
-- Integers

-- | Determine the $\log_2$ of exact powers of 2
exactLog2 :: Integer -> Maybe Integer
exactLog2 x
   | x <= 0                               = Nothing
   | x > fromIntegral (maxBound :: Int32) = Nothing
   | x' .&. (-x') /= x'                   = Nothing
   | otherwise                            = Just (fromIntegral c)
      where
         x' = fromIntegral x :: Int32
         c = countTrailingZeros x'

{-
-- -----------------------------------------------------------------------------
-- Floats
-}

readRational__ :: ReadS Rational -- NB: doesn't handle leading "-"
readRational__ r = do
     (n,d,s) <- readFix r
     (k,t)   <- readExp s
     return ((n%1)*10^^(k-d), t)
 where
     readFix r = do
        (ds,s)  <- lexDecDigits r
        (ds',t) <- lexDotDigits s
        return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` "eE" = readExp' s
     readExp s                     = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do (k,t) <- readDec s
                           return (-k,t)
     readExp' s       = readDec s

     readDec s = do
        (ds,r) <- nonnull isDigit s
        return (foldl1 (\n d -> n * 10 + d) [ ord d - ord '0' | d <- ds ],
                r)

     lexDecDigits = nonnull isDigit

     lexDotDigits ('.':s) = return (span' isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span' p s)
                      return (cs,t)

     span' _ xs@[]         =  (xs, xs)
     span' p xs@(x:xs')
               | x == '_'  = span' p xs'   -- skip "_" (#14473)
               | p x       =  let (ys,zs) = span' p xs' in (x:ys,zs)
               | otherwise =  ([],xs)

readRational :: String -> Rational -- NB: *does* handle a leading "-"
readRational top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case (do { (x,"") <- readRational__ s ; return x }) of
          [x] -> x
          []  -> error ("readRational: no parse:"        ++ top_s)
          _   -> error ("readRational: ambiguous parse:" ++ top_s)


readHexRational :: String -> Rational
readHexRational str =
  case str of
    '-' : xs -> - (readMe xs)
    xs       -> readMe xs
  where
  readMe as =
    case readHexRational__ as of
      Just n -> n
      _      -> error ("readHexRational: no parse:" ++ str)


readHexRational__ :: String -> Maybe Rational
readHexRational__ ('0' : x : rest)
  | x == 'X' || x == 'x' =
  do let (front,rest2) = span' isHexDigit rest
     guard (not (null front))
     let frontNum = steps 16 0 front
     case rest2 of
       '.' : rest3 ->
          do let (back,rest4) = span' isHexDigit rest3
             guard (not (null back))
             let backNum = steps 16 frontNum back
                 exp1    = -4 * length back
             case rest4 of
               p : ps | isExp p -> fmap (mk backNum . (+ exp1)) (getExp ps)
               _ -> return (mk backNum exp1)
       p : ps | isExp p -> fmap (mk frontNum) (getExp ps)
       _ -> Nothing

  where
  isExp p = p == 'p' || p == 'P'

  getExp ('+' : ds) = dec ds
  getExp ('-' : ds) = fmap negate (dec ds)
  getExp ds         = dec ds

  mk :: Integer -> Int -> Rational
  mk n e = fromInteger n * 2^^e

  dec cs = case span' isDigit cs of
             (ds,"") | not (null ds) -> Just (steps 10 0 ds)
             _ -> Nothing

  steps base n ds = foldl' (step base) n ds
  step  base n d  = base * n + fromIntegral (digitToInt d)

  span' _ xs@[]         =  (xs, xs)
  span' p xs@(x:xs')
            | x == '_'  = span' p xs'   -- skip "_"  (#14473)
            | p x       =  let (ys,zs) = span' p xs' in (x:ys,zs)
            | otherwise =  ([],xs)

readHexRational__ _ = Nothing

-----------------------------------------------------------------------------
-- Verify that the 'dirname' portion of a FilePath exists.
--
doesDirNameExist :: FilePath -> IO Bool
doesDirNameExist fpath = doesDirectoryExist (takeDirectory fpath)

-----------------------------------------------------------------------------
-- Backwards compatibility definition of getModificationTime

getModificationUTCTime :: FilePath -> IO UTCTime
getModificationUTCTime = getModificationTime

-- --------------------------------------------------------------
-- check existence & modification time at the same time

modificationTimeIfExists :: FilePath -> IO (Maybe UTCTime)
modificationTimeIfExists f = do
  (do t <- getModificationUTCTime f; return (Just t))
        `catchIO` \e -> if isDoesNotExistError e
                        then return Nothing
                        else ioError e

-- --------------------------------------------------------------
-- atomic file writing by writing to a temporary file first (see #14533)
--
-- This should be used in all cases where GHC writes files to disk
-- and uses their modification time to skip work later,
-- as otherwise a partially written file (e.g. due to crash or Ctrl+C)
-- also results in a skip.

withAtomicRename :: (MonadIO m) => FilePath -> (FilePath -> m a) -> m a
withAtomicRename targetFile f = do
  -- The temp file must be on the same file system (mount) as the target file
  -- to result in an atomic move on most platforms.
  -- The standard way to ensure that is to place it into the same directory.
  -- This can still be fooled when somebody mounts a different file system
  -- at just the right time, but that is not a case we aim to cover here.
  let temp = targetFile <.> "tmp"
  res <- f temp
  liftIO $ renameFile temp targetFile
  return res

-- --------------------------------------------------------------
-- split a string at the last character where 'pred' is True,
-- returning a pair of strings. The first component holds the string
-- up (but not including) the last character for which 'pred' returned
-- True, the second whatever comes after (but also not including the
-- last character).
--
-- If 'pred' returns False for all characters in the string, the original
-- string is returned in the first component (and the second one is just
-- empty).
splitLongestPrefix :: String -> (Char -> Bool) -> (String,String)
splitLongestPrefix str pred
  | null r_pre = (str,           [])
  | otherwise  = (reverse (tail r_pre), reverse r_suf)
                           -- 'tail' drops the char satisfying 'pred'
  where (r_suf, r_pre) = break pred (reverse str)

escapeSpaces :: String -> String
escapeSpaces = foldr (\c s -> if isSpace c then '\\':c:s else c:s) ""

type Suffix = String

--------------------------------------------------------------
-- * Search path
--------------------------------------------------------------

data Direction = Forwards | Backwards

reslash :: Direction -> FilePath -> FilePath
reslash d = f
    where f ('/'  : xs) = slash : f xs
          f ('\\' : xs) = slash : f xs
          f (x    : xs) = x     : f xs
          f ""          = ""
          slash = case d of
                  Forwards -> '/'
                  Backwards -> '\\'

makeRelativeTo :: FilePath -> FilePath -> FilePath
this `makeRelativeTo` that = directory </> thisFilename
    where (thisDirectory, thisFilename) = splitFileName this
          thatDirectory = dropFileName that
          directory = joinPath $ f (splitPath thisDirectory)
                                   (splitPath thatDirectory)

          f (x : xs) (y : ys)
           | x == y = f xs ys
          f xs ys = replicate (length ys) ".." ++ xs

{-
************************************************************************
*                                                                      *
\subsection[Utils-Data]{Utils for defining Data instances}
*                                                                      *
************************************************************************

These functions helps us to define Data instances for abstract types.
-}

abstractConstr :: String -> Constr
abstractConstr n = mkConstr (abstractDataType n) ("{abstract:"++n++"}") [] Prefix

abstractDataType :: String -> DataType
abstractDataType n = mkDataType n [abstractConstr n]

{-
************************************************************************
*                                                                      *
\subsection[Utils-C]{Utils for printing C code}
*                                                                      *
************************************************************************
-}

charToC :: Word8 -> String
charToC w =
  case chr (fromIntegral w) of
        '\"' -> "\\\""
        '\'' -> "\\\'"
        '\\' -> "\\\\"
        c | c >= ' ' && c <= '~' -> [c]
          | otherwise -> ['\\',
                         chr (ord '0' + ord c `div` 64),
                         chr (ord '0' + ord c `div` 8 `mod` 8),
                         chr (ord '0' + ord c         `mod` 8)]

{-
************************************************************************
*                                                                      *
\subsection[Utils-Hashing]{Utils for hashing}
*                                                                      *
************************************************************************
-}

-- | A sample hash function for Strings.  We keep multiplying by the
-- golden ratio and adding.  The implementation is:
--
-- > hashString = foldl' f golden
-- >   where f m c = fromIntegral (ord c) * magic + hashInt32 m
-- >         magic = 0xdeadbeef
--
-- Where hashInt32 works just as hashInt shown above.
--
-- Knuth argues that repeated multiplication by the golden ratio
-- will minimize gaps in the hash space, and thus it's a good choice
-- for combining together multiple keys to form one.
--
-- Here we know that individual characters c are often small, and this
-- produces frequent collisions if we use ord c alone.  A
-- particular problem are the shorter low ASCII and ISO-8859-1
-- character strings.  We pre-multiply by a magic twiddle factor to
-- obtain a good distribution.  In fact, given the following test:
--
-- > testp :: Int32 -> Int
-- > testp k = (n - ) . length . group . sort . map hs . take n $ ls
-- >   where ls = [] : [c : l | l <- ls, c <- ['\0'..'\xff']]
-- >         hs = foldl' f golden
-- >         f m c = fromIntegral (ord c) * k + hashInt32 m
-- >         n = 100000
--
-- We discover that testp magic = 0.
hashString :: String -> Int32
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + hashInt32 m
         magic = fromIntegral (0xdeadbeef :: Word32)

golden :: Int32
golden = 1013904242 -- = round ((sqrt 5 - 1) * 2^32) :: Int32
-- was -1640531527 = round ((sqrt 5 - 1) * 2^31) :: Int32
-- but that has bad mulHi properties (even adding 2^32 to get its inverse)
-- Whereas the above works well and contains no hash duplications for
-- [-32767..65536]

-- | A sample (and useful) hash function for Int32,
-- implemented by extracting the uppermost 32 bits of the 64-bit
-- result of multiplying by a 33-bit constant.  The constant is from
-- Knuth, derived from the golden ratio:
--
-- > golden = round ((sqrt 5 - 1) * 2^32)
--
-- We get good key uniqueness on small inputs
-- (a problem with previous versions):
--  (length $ group $ sort $ map hashInt32 [-32767..65536]) == 65536 + 32768
--
hashInt32 :: Int32 -> Int32
hashInt32 x = mulHi x golden + x

-- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
   where r :: Int64
         r = fromIntegral a * fromIntegral b

-- | A call stack constraint, but only when 'isDebugOn'.
#if defined(DEBUG)
type HasDebugCallStack = HasCallStack
#else
type HasDebugCallStack = (() :: Constraint)
#endif

data OverridingBool
  = Auto
  | Always
  | Never
  deriving Show

overrideWith :: Bool -> OverridingBool -> Bool
overrideWith b Auto   = b
overrideWith _ Always = True
overrideWith _ Never  = False
