%
% (c) The University of Glasgow 2006
%

\begin{code}

-- | Highly random utility functions
--
module Util (
        -- * Flags dependent on the compiler build
        ghciSupported, debugIsOn, ncgDebugIsOn,
        ghciTablesNextToCode,
        isWindowsHost, isDarwinHost,

        -- * General list processing
        zipEqual, zipWithEqual, zipWith3Equal, zipWith4Equal,
        zipLazy, stretchZipWith,

        unzipWith,

        mapFst, mapSnd,
        mapAndUnzip, mapAndUnzip3, mapAccumL2,
        nOfThem, filterOut, partitionWith, splitEithers,

        foldl1', foldl2, count, all2,

        lengthExceeds, lengthIs, lengthAtLeast,
        listLengthCmp, atLength, equalLength, compareLength,

        isSingleton, only, singleton,
        notNull, snocView,

        isIn, isn'tIn,

        -- * Tuples
        fstOf3, sndOf3, thirdOf3,
        firstM, first3M,
        third3,
        uncurry3,

        -- * List operations controlled by another list
        takeList, dropList, splitAtList, split,
        dropTail,

        -- * For loop
        nTimes,

        -- * Sorting
        sortWith, minWith,

        -- * Comparisons
        isEqual, eqListBy, eqMaybeBy,
        thenCmp, cmpList,
        removeSpaces,

        -- * Edit distance
        fuzzyMatch, fuzzyLookup,

        -- * Transitive closures
        transitiveClosure,

        -- * Strictness
        seqList,

        -- * Module names
        looksLikeModuleName,

        -- * Argument processing
        getCmd, toCmdArgs, toArgs,

        -- * Floating point
        readRational,

        -- * read helpers
        maybeRead, maybeReadFuzzy,

        -- * IO-ish utilities
        doesDirNameExist,
        getModificationUTCTime,
        modificationTimeIfExists,

        global, consIORef, globalM,

        -- * Filenames and paths
        Suffix,
        splitLongestPrefix,
        escapeSpaces,
        parseSearchPath,
        Direction(..), reslash,
        makeRelativeTo,

        -- * Utils for defining Data instances
        abstractConstr, abstractDataType, mkNoRepType,

        -- * Utils for printing C code
        charToC,

        -- * Hashing
        hashString,
    ) where

#include "HsVersions.h"

import Exception
import Panic

import Data.Data
import Data.IORef       ( IORef, newIORef, atomicModifyIORef )
import System.IO.Unsafe ( unsafePerformIO )
import Data.List        hiding (group)

#ifdef DEBUG
import FastTypes
#endif

import Control.Monad    ( liftM )
import System.IO.Error as IO ( isDoesNotExistError )
import System.Directory ( doesDirectoryExist, getModificationTime )
import System.FilePath

import Data.Char        ( isUpper, isAlphaNum, isSpace, chr, ord, isDigit )
import Data.Int
import Data.Ratio       ( (%) )
import Data.Ord         ( comparing )
import Data.Bits
import Data.Word
import qualified Data.IntMap as IM

import Data.Time
#if __GLASGOW_HASKELL__ < 705
import Data.Time.Clock.POSIX
import System.Time
#endif

infixr 9 `thenCmp`
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Is DEBUG on, are we on Windows, etc?}
%*                                                                      *
%************************************************************************

These booleans are global constants, set by CPP flags.  They allow us to
recompile a single module (this one) to change whether or not debug output
appears. They sometimes let us avoid even running CPP elsewhere.

It's important that the flags are literal constants (True/False). Then,
with -0, tests of the flags in other modules will simplify to the correct
branch of the conditional, thereby dropping debug code altogether when
the flags are off.

\begin{code}
ghciSupported :: Bool
#ifdef GHCI
ghciSupported = True
#else
ghciSupported = False
#endif

debugIsOn :: Bool
#ifdef DEBUG
debugIsOn = True
#else
debugIsOn = False
#endif

ncgDebugIsOn :: Bool
#ifdef NCG_DEBUG
ncgDebugIsOn = True
#else
ncgDebugIsOn = False
#endif

ghciTablesNextToCode :: Bool
#ifdef GHCI_TABLES_NEXT_TO_CODE
ghciTablesNextToCode = True
#else
ghciTablesNextToCode = False
#endif

isWindowsHost :: Bool
#ifdef mingw32_HOST_OS
isWindowsHost = True
#else
isWindowsHost = False
#endif

isDarwinHost :: Bool
#ifdef darwin_HOST_OS
isDarwinHost = True
#else
isDarwinHost = False
#endif
\end{code}

%************************************************************************
%*                                                                      *
\subsection{A for loop}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Compose a function with itself n times.  (nth rather than twice)
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f
\end{code}

\begin{code}
fstOf3   :: (a,b,c) -> a
sndOf3   :: (a,b,c) -> b
thirdOf3 :: (a,b,c) -> c
fstOf3      (a,_,_) =  a
sndOf3      (_,b,_) =  b
thirdOf3    (_,_,c) =  c

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
\end{code}

\begin{code}
firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f (x, y) = liftM (\x' -> (x', y)) (f x)

first3M :: Monad m => (a -> m d) -> (a, b, c) -> m (d, b, c)
first3M f (x, y, z) = liftM (\x' -> (x', y, z)) (f x)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-lists]{General list processing}
%*                                                                      *
%************************************************************************

\begin{code}
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

splitEithers :: [Either a b] -> ([a], [b])
-- ^ Teases a list of 'Either's apart into two lists
splitEithers [] = ([],[])
splitEithers (e : es) = case e of
                        Left x -> (x:xs, ys)
                        Right y -> (xs, y:ys)
    where (xs,ys) = splitEithers es
\end{code}

A paranoid @zip@ (and some @zipWith@ friends) that checks the lists
are of equal length.  Alastair Reid thinks this should only happen if
DEBUGging on; hey, why not?

\begin{code}
zipEqual        :: String -> [a] -> [b] -> [(a,b)]
zipWithEqual    :: String -> (a->b->c) -> [a]->[b]->[c]
zipWith3Equal   :: String -> (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith4Equal   :: String -> (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]

#ifndef DEBUG
zipEqual      _ = zip
zipWithEqual  _ = zipWith
zipWith3Equal _ = zipWith3
zipWith4Equal _ = zipWith4
#else
zipEqual _   []     []     = []
zipEqual msg (a:as) (b:bs) = (a,b) : zipEqual msg as bs
zipEqual msg _      _      = panic ("zipEqual: unequal lists:"++msg)

zipWithEqual msg z (a:as) (b:bs)=  z a b : zipWithEqual msg z as bs
zipWithEqual _   _ [] []        =  []
zipWithEqual msg _ _ _          =  panic ("zipWithEqual: unequal lists:"++msg)

zipWith3Equal msg z (a:as) (b:bs) (c:cs)
                                =  z a b c : zipWith3Equal msg z as bs cs
zipWith3Equal _   _ [] []  []   =  []
zipWith3Equal msg _ _  _   _    =  panic ("zipWith3Equal: unequal lists:"++msg)

zipWith4Equal msg z (a:as) (b:bs) (c:cs) (d:ds)
                                =  z a b c d : zipWith4Equal msg z as bs cs ds
zipWith4Equal _   _ [] [] [] [] =  []
zipWith4Equal msg _ _  _  _  _  =  panic ("zipWith4Equal: unequal lists:"++msg)
#endif
\end{code}

\begin{code}
-- | 'zipLazy' is a kind of 'zip' that is lazy in the second list (observe the ~)
zipLazy :: [a] -> [b] -> [(a,b)]
zipLazy []     _       = []
zipLazy (x:xs) ~(y:ys) = (x,y) : zipLazy xs ys
\end{code}


\begin{code}
stretchZipWith :: (a -> Bool) -> b -> (a->b->c) -> [a] -> [b] -> [c]
-- ^ @stretchZipWith p z f xs ys@ stretches @ys@ by inserting @z@ in
-- the places where @p@ returns @True@

stretchZipWith _ _ _ []     _ = []
stretchZipWith p z f (x:xs) ys
  | p x       = f x z : stretchZipWith p z f xs ys
  | otherwise = case ys of
                []     -> []
                (y:ys) -> f x y : stretchZipWith p z f xs ys
\end{code}


\begin{code}
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

mapAccumL2 :: (s1 -> s2 -> a -> (s1, s2, b)) -> s1 -> s2 -> [a] -> (s1, s2, [b])
mapAccumL2 f s1 s2 xs = (s1', s2', ys)
  where ((s1', s2'), ys) = mapAccumL (\(s1, s2) x -> case f s1 s2 x of
                                                       (s1', s2', y) -> ((s1', s2'), y))
                                     (s1, s2) xs
\end{code}

\begin{code}
nOfThem :: Int -> a -> [a]
nOfThem n thing = replicate n thing

-- | @atLength atLen atEnd ls n@ unravels list @ls@ to position @n@. Precisely:
--
-- @
--  atLength atLenPred atEndPred ls n
--   | n < 0         = atLenPred n
--   | length ls < n = atEndPred (n - length ls)
--   | otherwise     = atLenPred (drop n ls)
-- @
atLength :: ([a] -> b)
         -> (Int -> b)
         -> [a]
         -> Int
         -> b
atLength atLenPred atEndPred ls n
  | n < 0     = atEndPred n
  | otherwise = go n ls
  where
    go n [] = atEndPred n
    go 0 ls = atLenPred ls
    go n (_:xs) = go (n-1) xs

-- Some special cases of atLength:

lengthExceeds :: [a] -> Int -> Bool
-- ^ > (lengthExceeds xs n) = (length xs > n)
lengthExceeds = atLength notNull (const False)

lengthAtLeast :: [a] -> Int -> Bool
lengthAtLeast = atLength notNull (== 0)

lengthIs :: [a] -> Int -> Bool
lengthIs = atLength null (==0)

listLengthCmp :: [a] -> Int -> Ordering
listLengthCmp = atLength atLen atEnd
 where
  atEnd 0      = EQ
  atEnd x
   | x > 0     = LT -- not yet seen 'n' elts, so list length is < n.
   | otherwise = GT

  atLen []     = EQ
  atLen _      = GT

equalLength :: [a] -> [b] -> Bool
equalLength []     []     = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength _      _      = False

compareLength :: [a] -> [b] -> Ordering
compareLength []     []     = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength []     _      = LT
compareLength _      []     = GT

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
#ifdef DEBUG
only [a] = a
#else
only (a:_) = a
#endif
only _ = panic "Util: only"
\end{code}

Debugging/specialising versions of \tr{elem} and \tr{notElem}

\begin{code}
isIn, isn'tIn :: Eq a => String -> a -> [a] -> Bool

# ifndef DEBUG
isIn    _msg x ys = x `elem` ys
isn'tIn _msg x ys = x `notElem` ys

# else /* DEBUG */
isIn msg x ys
  = elem100 (_ILIT(0)) x ys
  where
    elem100 _ _ []        = False
    elem100 i x (y:ys)
      | i ># _ILIT(100) = trace ("Over-long elem in " ++ msg)
                                (x `elem` (y:ys))
      | otherwise       = x == y || elem100 (i +# _ILIT(1)) x ys

isn'tIn msg x ys
  = notElem100 (_ILIT(0)) x ys
  where
    notElem100 _ _ [] =  True
    notElem100 i x (y:ys)
      | i ># _ILIT(100) = trace ("Over-long notElem in " ++ msg)
                                (x `notElem` (y:ys))
      | otherwise      =  x /= y && notElem100 (i +# _ILIT(1)) x ys
# endif /* DEBUG */
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Sort utils}
%*                                                                      *
%************************************************************************

\begin{code}
sortWith :: Ord b => (a->b) -> [a] -> [a]
sortWith get_key xs = sortBy (comparing get_key) xs

minWith :: Ord b => (a -> b) -> [a] -> a
minWith get_key xs = ASSERT( not (null xs) )
                     head (sortWith get_key xs)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-transitive-closure]{Transitive closure}
%*                                                                      *
%************************************************************************

This algorithm for transitive closure is straightforward, albeit quadratic.

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-accum]{Accumulating}
%*                                                                      *
%************************************************************************

A combination of foldl with zip.  It works with equal length lists.

\begin{code}
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
\end{code}

Count the number of times a predicate is true

\begin{code}
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count p (x:xs) | p x       = 1 + count p xs
               | otherwise = count p xs
\end{code}

@splitAt@, @take@, and @drop@ but with length of another
list giving the break-off point:

\begin{code}
takeList :: [b] -> [a] -> [a]
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
dropTail n = reverse . drop n . reverse

snocView :: [a] -> Maybe ([a],a)
        -- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
            where
                -- Invariant: second arg is non-empty
              go acc [x]    = Just (reverse acc, x)
              go acc (x:xs) = go (x:acc) xs
              go _   []     = panic "Util: snocView"

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s
\end{code}


%************************************************************************
%*                                                                      *
\subsection[Utils-comparison]{Comparisons}
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

\begin{code}
removeSpaces :: String -> String
removeSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Edit distance}
%*                                                                      *
%************************************************************************

\begin{code}
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

#ifdef __GLASGOW_HASKELL__
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
#endif

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
    -- Work out an approriate match threshold:
    -- We report a candidate if its edit distance is <= the threshold,
    -- The threshhold is set to about a quarter of the # of characters the user entered
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-pairs]{Pairs}
%*                                                                      *
%************************************************************************

\begin{code}
unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs
\end{code}

\begin{code}
seqList :: [a] -> b -> b
seqList [] b = b
seqList (x:xs) b = x `seq` seqList xs b
\end{code}

Global variables:

\begin{code}
global :: a -> IORef a
global a = unsafePerformIO (newIORef a)
\end{code}

\begin{code}
consIORef :: IORef [a] -> a -> IO ()
consIORef var x = do
  atomicModifyIORef var (\xs -> (x:xs,()))
\end{code}

\begin{code}
globalM :: IO a -> IORef a
globalM ma = unsafePerformIO (ma >>= newIORef)
\end{code}

Module names:

\begin{code}
looksLikeModuleName :: String -> Bool
looksLikeModuleName [] = False
looksLikeModuleName (c:cs) = isUpper c && go cs
  where go [] = True
        go ('.':cs) = looksLikeModuleName cs
        go (c:cs)   = (isAlphaNum c || c == '_' || c == '\'') && go cs
\end{code}

Akin to @Prelude.words@, but acts like the Bourne shell, treating
quoted strings as Haskell Strings, and also parses Haskell [String]
syntax.

\begin{code}
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
                       Left ("Couldn't read " ++ show str ++ "as [String]")
      s -> toArgs' s
 where
  toArgs' s = case dropWhile isSpace s of
              [] -> Right []
              ('"' : _) -> case reads s of
                           [(arg, rest)]
                              -- rest must either be [] or start with a space
                            | all isSpace (take 1 rest) ->
                               case toArgs' rest of
                               Left err -> Left err
                               Right args -> Right (arg : args)
                           _ ->
                               Left ("Couldn't read " ++ show s ++ "as String")
              s' -> case break isSpace s' of
                    (arg, s'') -> case toArgs' s'' of
                                  Left err -> Left err
                                  Right args -> Right (arg : args)
\end{code}

-- -----------------------------------------------------------------------------
-- Floats

\begin{code}
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

     lexDotDigits ('.':s) = return (span isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span p s)
                      return (cs,t)

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


-----------------------------------------------------------------------------
-- read helpers

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                [(x, "")] -> Just x
                _         -> Nothing

maybeReadFuzzy :: Read a => String -> Maybe a
maybeReadFuzzy str = case reads str of
                     [(x, s)]
                      | all isSpace s ->
                         Just x
                     _ ->
                         Nothing

-----------------------------------------------------------------------------
-- Verify that the 'dirname' portion of a FilePath exists.
--
doesDirNameExist :: FilePath -> IO Bool
doesDirNameExist fpath = case takeDirectory fpath of
                         "" -> return True -- XXX Hack
                         _  -> doesDirectoryExist (takeDirectory fpath)

-----------------------------------------------------------------------------
-- Backwards compatibility definition of getModificationTime

getModificationUTCTime :: FilePath -> IO UTCTime
#if __GLASGOW_HASKELL__ < 705
getModificationUTCTime f = do
    TOD secs _ <- getModificationTime f
    return $ posixSecondsToUTCTime (realToFrac secs)
#else
getModificationUTCTime = getModificationTime
#endif

-- --------------------------------------------------------------
-- check existence & modification time at the same time

modificationTimeIfExists :: FilePath -> IO (Maybe UTCTime)
modificationTimeIfExists f = do
  (do t <- getModificationUTCTime f; return (Just t))
        `catchIO` \e -> if isDoesNotExistError e
                        then return Nothing
                        else ioError e

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

-- | The function splits the given string to substrings
-- using the 'searchPathSeparator'.
parseSearchPath :: String -> [FilePath]
parseSearchPath path = split path
  where
    split :: String -> [String]
    split s =
      case rest' of
        []     -> [chunk]
        _:rest -> chunk : split rest
      where
        chunk =
          case chunk' of
#ifdef mingw32_HOST_OS
            ('\"':xs@(_:_)) | last xs == '\"' -> init xs
#endif
            _                                 -> chunk'

        (chunk', rest') = break isSearchPathSeparator s

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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-Data]{Utils for defining Data instances}
%*                                                                      *
%************************************************************************

These functions helps us to define Data instances for abstract types.

\begin{code}
abstractConstr :: String -> Constr
abstractConstr n = mkConstr (abstractDataType n) ("{abstract:"++n++"}") [] Prefix
\end{code}

\begin{code}
abstractDataType :: String -> DataType
abstractDataType n = mkDataType n [abstractConstr n]
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-C]{Utils for printing C code}
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-Hashing]{Utils for hashing}
%*                                                                      *
%************************************************************************

\begin{code}
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
         magic = 0xdeadbeef

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
\end{code}

