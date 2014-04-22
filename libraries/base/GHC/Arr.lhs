\begin{code}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Arr
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC\'s array implementation.
--
-----------------------------------------------------------------------------

module GHC.Arr (
        Ix(..), Array(..), STArray(..),

        indexError, hopelessIndexError,
        arrEleBottom, array, listArray,
        (!), safeRangeSize, negRange, safeIndex, badSafeIndex,
        bounds, numElements, numElementsSTArray, indices, elems,
        assocs, accumArray, adjust, (//), accum,
        amap, ixmap,
        eqArray, cmpArray, cmpIntArray,
        newSTArray, boundsSTArray,
        readSTArray, writeSTArray,
        freezeSTArray, thawSTArray,

        -- * Unsafe operations
        fill, done,
        unsafeArray, unsafeArray',
        lessSafeIndex, unsafeAt, unsafeReplace,
        unsafeAccumArray, unsafeAccumArray', unsafeAccum,
        unsafeReadSTArray, unsafeWriteSTArray,
        unsafeFreezeSTArray, unsafeThawSTArray,
    ) where

import GHC.Enum
import GHC.Num
import GHC.ST
import GHC.Base
import GHC.List
import GHC.Real( fromIntegral )
import GHC.Show

infixl 9  !, //

default ()
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Ix@ class}
%*                                                      *
%*********************************************************

\begin{code}
-- | The 'Ix' class is used to map a contiguous subrange of values in
-- a type onto integers.  It is used primarily for array indexing
-- (see the array package).
--
-- The first argument @(l,u)@ of each of these operations is a pair
-- specifying the lower and upper bounds of a contiguous subrange of values.
--
-- An implementation is entitled to assume the following laws about these
-- operations:
--
-- * @'inRange' (l,u) i == 'elem' i ('range' (l,u))@ @ @
--
-- * @'range' (l,u) '!!' 'index' (l,u) i == i@, when @'inRange' (l,u) i@
--
-- * @'map' ('index' (l,u)) ('range' (l,u))) == [0..'rangeSize' (l,u)-1]@ @ @
--
-- * @'rangeSize' (l,u) == 'length' ('range' (l,u))@ @ @
--
-- Minimal complete instance: 'range', 'index' and 'inRange'.
--
class (Ord a) => Ix a where
    -- | The list of values in the subrange defined by a bounding pair.
    range               :: (a,a) -> [a]
    -- | The position of a subscript in the subrange.
    index               :: (a,a) -> a -> Int
    -- | Like 'index', but without checking that the value is in range.
    unsafeIndex         :: (a,a) -> a -> Int
    -- | Returns 'True' the given subscript lies in the range defined
    -- the bounding pair.
    inRange             :: (a,a) -> a -> Bool
    -- | The size of the subrange defined by a bounding pair.
    rangeSize           :: (a,a) -> Int
    -- | like 'rangeSize', but without checking that the upper bound is
    -- in range.
    unsafeRangeSize     :: (a,a) -> Int

        -- Must specify one of index, unsafeIndex

	-- 'index' is typically over-ridden in instances, with essentially
	-- the same code, but using indexError instead of hopelessIndexError
	-- Reason: we have 'Show' at the instances
    {-# INLINE index #-}  -- See Note [Inlining index]
    index b i | inRange b i = unsafeIndex b i
              | otherwise   = hopelessIndexError

    unsafeIndex b i = index b i

    rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
                       | otherwise   = 0        -- This case is only here to
                                                -- check for an empty range
        -- NB: replacing (inRange b h) by (l <= h) fails for
        --     tuples.  E.g.  (1,2) <= (2,1) but the range is empty

    unsafeRangeSize b@(_l,h) = unsafeIndex b h + 1
\end{code}

Note that the following is NOT right
        rangeSize (l,h) | l <= h    = index b h + 1
                        | otherwise = 0

Because it might be the case that l<h, but the range
is nevertheless empty.  Consider
        ((1,2),(2,1))
Here l<h, but the second index ranges from 2..1 and
hence is empty

%*********************************************************
%*                                                      *
\subsection{Instances of @Ix@}
%*                                                      *
%*********************************************************

Note [Inlining index]
~~~~~~~~~~~~~~~~~~~~~
We inline the 'index' operation,

 * Partly because it generates much faster code
   (although bigger); see Trac #1216

 * Partly because it exposes the bounds checks to the simplifier which
   might help a big.

If you make a per-instance index method, you may consider inlining it.

Note [Double bounds-checking of index values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When you index an array, a!x, there are two possible bounds checks we might make:

  (A) Check that (inRange (bounds a) x) holds.

      (A) is checked in the method for 'index'

  (B) Check that (index (bounds a) x) lies in the range 0..n,
      where n is the size of the underlying array

      (B) is checked in the top-level function (!), in safeIndex.

Of course it *should* be the case that (A) holds iff (B) holds, but that
is a property of the particular instances of index, bounds, and inRange,
so GHC cannot guarantee it.

 * If you do (A) and not (B), then you might get a seg-fault,
   by indexing at some bizarre location.  Trac #1610

 * If you do (B) but not (A), you may get no complaint when you index
   an array out of its semantic bounds.  Trac #2120

At various times we have had (A) and not (B), or (B) and not (A); both
led to complaints.  So now we implement *both* checks (Trac #2669).

For 1-d, 2-d, and 3-d arrays of Int we have specialised instances to avoid this.

Note [Out-of-bounds error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default method for 'index' generates hoplelessIndexError, because
Ix doesn't have Show as a superclass.  For particular base types we
can do better, so we override the default method for index.

\begin{code}
-- Abstract these errors from the relevant index functions so that
-- the guts of the function will be small enough to inline.

{-# NOINLINE indexError #-}
indexError :: Show a => (a,a) -> a -> String -> b
indexError rng i tp
  = error (showString "Ix{" . showString tp . showString "}.index: Index " .
           showParen True (showsPrec 0 i) .
           showString " out of range " $
           showParen True (showsPrec 0 rng) "")

hopelessIndexError :: Int -- Try to use 'indexError' instead!
hopelessIndexError = error "Error in array index"

----------------------------------------------------------------------
instance  Ix Char  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = fromEnum i - fromEnum m

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Char"

    inRange (m,n) i     =  m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Int  where
    {-# INLINE range #-}
        -- The INLINE stops the build in the RHS from getting inlined,
        -- so that callers can fuse with the result of range
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = i - m

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Int"

    {-# INLINE inRange #-}
    inRange (I# m,I# n) (I# i) =  isTrue# (m <=# i) && isTrue# (i <=# n)

instance Ix Word where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Integer  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i   = fromInteger (i - m)

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Integer"

    inRange (m,n) i     =  m <= i && i <= n

----------------------------------------------------------------------
instance Ix Bool where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Bool"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix Ordering where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Ordering"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix () where
    {-# INLINE range #-}
    range   ((), ())    = [()]
    {-# INLINE unsafeIndex #-}
    unsafeIndex   ((), ()) () = 0
    {-# INLINE inRange #-}
    inRange ((), ()) () = True

    {-# INLINE index #-}  -- See Note [Inlining index]
    index b i = unsafeIndex b i

----------------------------------------------------------------------
instance (Ix a, Ix b) => Ix (a, b) where -- as derived
    {-# SPECIALISE instance Ix (Int,Int) #-}

    {-# INLINE range #-}
    range ((l1,l2),(u1,u2)) =
      [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

    {-# INLINE unsafeIndex #-}
    unsafeIndex ((l1,l2),(u1,u2)) (i1,i2) =
      unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2

    {-# INLINE inRange #-}
    inRange ((l1,l2),(u1,u2)) (i1,i2) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2

    -- Default method for index

----------------------------------------------------------------------
instance  (Ix a1, Ix a2, Ix a3) => Ix (a1,a2,a3)  where
    {-# SPECIALISE instance Ix (Int,Int,Int) #-}

    range ((l1,l2,l3),(u1,u2,u3)) =
        [(i1,i2,i3) | i1 <- range (l1,u1),
                      i2 <- range (l2,u2),
                      i3 <- range (l3,u3)]

    unsafeIndex ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))

    inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3

    -- Default method for index

----------------------------------------------------------------------
instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
    range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
      [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                       i2 <- range (l2,u2),
                       i3 <- range (l3,u3),
                       i4 <- range (l4,u4)]

    unsafeIndex ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))

    inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1,a2,a3,a4,a5)  where
    range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
      [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
                          i2 <- range (l2,u2),
                          i3 <- range (l3,u3),
                          i4 <- range (l4,u4),
                          i5 <- range (l5,u5)]

    unsafeIndex ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))

    inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5

    -- Default method for index
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Array@ types}
%*                                                      *
%*********************************************************

\begin{code}
-- | The type of immutable non-strict (boxed) arrays
-- with indices in @i@ and elements in @e@.
data Array i e
   = Array            !i         -- the lower bound, l
                      !i         -- the upper bound, u
       {-# UNPACK #-} !Int       -- A cache of (rangeSize (l,u))
                                 -- used to make sure an index is
                                 -- really in range
                      (Array# e) -- The actual elements

-- | Mutable, boxed, non-strict arrays in the 'ST' monad.  The type
-- arguments are as follows:
--
--  * @s@: the state variable argument for the 'ST' type
--
--  * @i@: the index type of the array (should be an instance of 'Ix')
--
--  * @e@: the element type of the array.
--
data STArray s i e
  = STArray           !i               -- the lower bound, l
                      !i               -- the upper bound, u
      {-# UNPACK #-}  !Int             -- A cache of (rangeSize (l,u))
                                       -- used to make sure an index is
                                       -- really in range
                   (MutableArray# s e) -- The actual elements
        -- No Ix context for STArray.  They are stupid,
        -- and force an Ix context on the equality instance.

-- Just pointer equality on mutable arrays:
instance Eq (STArray s i e) where
    STArray _ _ _ arr1# == STArray _ _ _ arr2# =
        isTrue# (sameMutableArray# arr1# arr2#)
\end{code}


%*********************************************************
%*                                                      *
\subsection{Operations on immutable arrays}
%*                                                      *
%*********************************************************

\begin{code}
{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"

-- | Construct an array with the specified bounds and containing values
-- for given indices within these bounds.
--
-- The array is undefined (i.e. bottom) if any index in the list is
-- out of bounds.  The Haskell 2010 Report further specifies that if any
-- two associations in the list have the same index, the value at that
-- index is undefined (i.e. bottom).  However in GHC's implementation,
-- the value at such an index is the value part of the last association
-- with that index in the list.
--
-- Because the indices must be checked for these errors, 'array' is
-- strict in the bounds argument and in the indices of the association
-- list, but non-strict in the values.  Thus, recurrences such as the
-- following are possible:
--
-- > a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])
--
-- Not every index within the bounds of the array need appear in the
-- association list, but the values associated with indices that do not
-- appear will be undefined (i.e. bottom).
--
-- If, in any dimension, the lower bound is greater than the upper bound,
-- then the array is legal, but empty.  Indexing an empty array always
-- gives an array-bounds error, but 'bounds' still yields the bounds
-- with which the array was constructed.
{-# INLINE array #-}
array :: Ix i
        => (i,i)        -- ^ a pair of /bounds/, each of the index type
                        -- of the array.  These bounds are the lowest and
                        -- highest indices in the array, in that order.
                        -- For example, a one-origin vector of length
                        -- '10' has bounds '(1,10)', and a one-origin '10'
                        -- by '10' matrix has bounds '((1,1),(10,10))'.
        -> [(i, e)]     -- ^ a list of /associations/ of the form
                        -- (/index/, /value/).  Typically, this list will
                        -- be expressed as a comprehension.  An
                        -- association '(i, x)' defines the value of
                        -- the array at index 'i' to be 'x'.
        -> Array i e
array (l,u) ies
    = let n = safeRangeSize (l,u)
      in unsafeArray' (l,u) n
                      [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeArray #-}
unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray b ies = unsafeArray' b (rangeSize b) ies

{-# INLINE unsafeArray' #-}
unsafeArray' :: Ix i => (i,i) -> Int -> [(Int, e)] -> Array i e
unsafeArray' (l,u) n@(I# n#) ies = runST (ST $ \s1# ->
    case newArray# n# arrEleBottom s1# of
        (# s2#, marr# #) ->
            foldr (fill marr#) (done l u n marr#) ies s2#)

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
-- NB: put the \s after the "=" so that 'fill'
--     inlines when applied to three args
fill marr# (I# i#, e) next
 = \s1# -> case writeArray# marr# i# e s1# of
             s2# -> next s2#

{-# INLINE done #-}
done :: Ix i => i -> i -> Int -> MutableArray# s e -> STRep s (Array i e)
-- See NB on 'fill'
done l u n marr#
  = \s1# -> case unsafeFreezeArray# marr# s1# of
              (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

-- This is inefficient and I'm not sure why:
-- listArray (l,u) es = unsafeArray (l,u) (zip [0 .. rangeSize (l,u) - 1] es)
-- The code below is better. It still doesn't enable foldr/build
-- transformation on the list of elements; I guess it's impossible
-- using mechanisms currently available.

-- | Construct an array from a pair of bounds and a list of values in
-- index order.
{-# INLINE listArray #-}
listArray :: Ix i => (i,i) -> [e] -> Array i e
listArray (l,u) es = runST (ST $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let fillFromList i# xs s3# | isTrue# (i# ==# n#) = s3#
                               | otherwise = case xs of
            []   -> s3#
            y:ys -> case writeArray# marr# i# y s3# of { s4# ->
                    fillFromList (i# +# 1#) ys s4# } in
    case fillFromList 0# es s2#         of { s3# ->
    done l u n marr# s3# }}})

-- | The value at the given index in an array.
{-# INLINE (!) #-}
(!) :: Ix i => Array i e -> i -> e
arr@(Array l u n _) ! i = unsafeAt arr $ safeIndex (l,u) n i

{-# INLINE safeRangeSize #-}
safeRangeSize :: Ix i => (i, i) -> Int
safeRangeSize (l,u) = let r = rangeSize (l, u)
                      in if r < 0 then negRange
                                  else r

-- Don't inline this error message everywhere!!
negRange :: Int	  -- Uninformative, but Ix does not provide Show
negRange = error "Negative range size"

{-# INLINE[1] safeIndex #-}
-- See Note [Double bounds-checking of index values]
-- Inline *after* (!) so the rules can fire
safeIndex :: Ix i => (i, i) -> Int -> i -> Int
safeIndex (l,u) n i = let i' = index (l,u) i
                      in if (0 <= i') && (i' < n)
                         then i'
                         else badSafeIndex i' n

-- See Note [Double bounds-checking of index values]
{-# RULES
"safeIndex/I"       safeIndex = lessSafeIndex :: (Int,Int) -> Int -> Int -> Int
"safeIndex/(I,I)"   safeIndex = lessSafeIndex :: ((Int,Int),(Int,Int)) -> Int -> (Int,Int) -> Int
"safeIndex/(I,I,I)" safeIndex = lessSafeIndex :: ((Int,Int,Int),(Int,Int,Int)) -> Int -> (Int,Int,Int) -> Int
  #-}

lessSafeIndex :: Ix i => (i, i) -> Int -> i -> Int
-- See Note [Double bounds-checking of index values]
-- Do only (A), the semantic check
lessSafeIndex (l,u) _ i = index (l,u) i

-- Don't inline this long error message everywhere!!
badSafeIndex :: Int -> Int -> Int
badSafeIndex i' n = error ("Error in array index; " ++ show i' ++
                        " not in range [0.." ++ show n ++ ")")

{-# INLINE unsafeAt #-}
unsafeAt :: Ix i => Array i e -> Int -> e
unsafeAt (Array _ _ _ arr#) (I# i#) =
    case indexArray# arr# i# of (# e #) -> e

-- | The bounds with which an array was constructed.
{-# INLINE bounds #-}
bounds :: Ix i => Array i e -> (i,i)
bounds (Array l u _ _) = (l,u)

-- | The number of elements in the array.
{-# INLINE numElements #-}
numElements :: Ix i => Array i e -> Int
numElements (Array _ _ n _) = n

-- | The list of indices of an array in ascending order.
{-# INLINE indices #-}
indices :: Ix i => Array i e -> [i]
indices (Array l u _ _) = range (l,u)

-- | The list of elements of an array in index order.
{-# INLINE elems #-}
elems :: Ix i => Array i e -> [e]
elems arr@(Array _ _ n _) =
    [unsafeAt arr i | i <- [0 .. n - 1]]

-- | The list of associations of an array in index order.
{-# INLINE assocs #-}
assocs :: Ix i => Array i e -> [(i, e)]
assocs arr@(Array l u _ _) =
    [(i, arr ! i) | i <- range (l,u)]

-- | The 'accumArray' function deals with repeated indices in the association
-- list using an /accumulating function/ which combines the values of
-- associations with the same index.
-- For example, given a list of values of some index type, @hist@
-- produces a histogram of the number of occurrences of each index within
-- a specified range:
--
-- > hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
-- > hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]
--
-- If the accumulating function is strict, then 'accumArray' is strict in
-- the values, as well as the indices, in the association list.  Thus,
-- unlike ordinary arrays built with 'array', accumulated arrays should
-- not in general be recursive.
{-# INLINE accumArray #-}
accumArray :: Ix i
        => (e -> a -> e)        -- ^ accumulating function
        -> e                    -- ^ initial value
        -> (i,i)                -- ^ bounds of the array
        -> [(i, a)]             -- ^ association list
        -> Array i e
accumArray f initial (l,u) ies =
    let n = safeRangeSize (l,u)
    in unsafeAccumArray' f initial (l,u) n
                         [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeAccumArray #-}
unsafeAccumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(Int, a)] -> Array i e
unsafeAccumArray f initial b ies = unsafeAccumArray' f initial b (rangeSize b) ies

{-# INLINE unsafeAccumArray' #-}
unsafeAccumArray' :: Ix i => (e -> a -> e) -> e -> (i,i) -> Int -> [(Int, a)] -> Array i e
unsafeAccumArray' f initial (l,u) n@(I# n#) ies = runST (ST $ \s1# ->
    case newArray# n# initial s1#          of { (# s2#, marr# #) ->
    foldr (adjust f marr#) (done l u n marr#) ies s2# })

{-# INLINE adjust #-}
adjust :: (e -> a -> e) -> MutableArray# s e -> (Int, a) -> STRep s b -> STRep s b
-- See NB on 'fill'
adjust f marr# (I# i#, new) next
  = \s1# -> case readArray# marr# i# s1# of
        	(# s2#, old #) ->
        	    case writeArray# marr# i# (f old new) s2# of
        	        s3# -> next s3#

-- | Constructs an array identical to the first argument except that it has
-- been updated by the associations in the right argument.
-- For example, if @m@ is a 1-origin, @n@ by @n@ matrix, then
--
-- > m//[((i,i), 0) | i <- [1..n]]
--
-- is the same matrix, except with the diagonal zeroed.
--
-- Repeated indices in the association list are handled as for 'array':
-- Haskell 2010 specifies that the resulting array is undefined (i.e. bottom),
-- but GHC's implementation uses the last association for each index.
{-# INLINE (//) #-}
(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
arr@(Array l u n _) // ies =
    unsafeReplace arr [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeReplace #-}
unsafeReplace :: Ix i => Array i e -> [(Int, e)] -> Array i e
unsafeReplace arr ies = runST (do
    STArray l u n marr# <- thawSTArray arr
    ST (foldr (fill marr#) (done l u n marr#) ies))

-- | @'accum' f@ takes an array and an association list and accumulates
-- pairs from the list into the array with the accumulating function @f@.
-- Thus 'accumArray' can be defined using 'accum':
--
-- > accumArray f z b = accum f (array b [(i, z) | i <- range b])
--
{-# INLINE accum #-}
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f arr@(Array l u n _) ies =
    unsafeAccum f arr [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeAccum #-}
unsafeAccum :: Ix i => (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f arr ies = runST (do
    STArray l u n marr# <- thawSTArray arr
    ST (foldr (adjust f marr#) (done l u n marr#) ies))

{-# INLINE amap #-}
amap :: Ix i => (a -> b) -> Array i a -> Array i b
amap f arr@(Array l u n _) =
    unsafeArray' (l,u) n [(i, f (unsafeAt arr i)) | i <- [0 .. n - 1]]

-- | 'ixmap' allows for transformations on array indices.
-- It may be thought of as providing function composition on the right
-- with the mapping that the original array embodies.
--
-- A similar transformation of array values may be achieved using 'fmap'
-- from the 'Array' instance of the 'Functor' class.
{-# INLINE ixmap #-}
ixmap :: (Ix i, Ix j) => (i,i) -> (i -> j) -> Array j e -> Array i e
ixmap (l,u) f arr =
    array (l,u) [(i, arr ! f i) | i <- range (l,u)]

{-# INLINE eqArray #-}
eqArray :: (Ix i, Eq e) => Array i e -> Array i e -> Bool
eqArray arr1@(Array l1 u1 n1 _) arr2@(Array l2 u2 n2 _) =
    if n1 == 0 then n2 == 0 else
    l1 == l2 && u1 == u2 &&
    and [unsafeAt arr1 i == unsafeAt arr2 i | i <- [0 .. n1 - 1]]

{-# INLINE [1] cmpArray #-}
cmpArray :: (Ix i, Ord e) => Array i e -> Array i e -> Ordering
cmpArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntArray #-}
cmpIntArray :: Ord e => Array Int e -> Array Int e -> Ordering
cmpIntArray arr1@(Array l1 u1 n1 _) arr2@(Array l2 u2 n2 _) =
    if n1 == 0 then
        if n2 == 0 then EQ else LT
    else if n2 == 0 then GT
    else case compare l1 l2 of
             EQ    -> foldr cmp (compare u1 u2) [0 .. (n1 `min` n2) - 1]
             other -> other
  where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpArray/Int" cmpArray = cmpIntArray #-}
\end{code}


%*********************************************************
%*                                                      *
\subsection{Array instances}
%*                                                      *
%*********************************************************

\begin{code}
instance Ix i => Functor (Array i) where
    fmap = amap

instance (Ix i, Eq e) => Eq (Array i e) where
    (==) = eqArray

instance (Ix i, Ord e) => Ord (Array i e) where
    compare = cmpArray

instance (Ix a, Show a, Show b) => Show (Array a b) where
    showsPrec p a =
        showParen (p > appPrec) $
        showString "array " .
        showsPrec appPrec1 (bounds a) .
        showChar ' ' .
        showsPrec appPrec1 (assocs a)
        -- Precedence of 'array' is the precedence of application

-- The Read instance is in GHC.Read
\end{code}


%*********************************************************
%*                                                      *
\subsection{Operations on mutable arrays}
%*                                                      *
%*********************************************************

Idle ADR question: What's the tradeoff here between flattening these
datatypes into @STArray ix ix (MutableArray# s elt)@ and using
it as is?  As I see it, the former uses slightly less heap and
provides faster access to the individual parts of the bounds while the
code used has the benefit of providing a ready-made @(lo, hi)@ pair as
required by many array-related functions.  Which wins? Is the
difference significant (probably not).

Idle AJG answer: When I looked at the outputted code (though it was 2
years ago) it seems like you often needed the tuple, and we build
it frequently. Now we've got the overloading specialiser things
might be different, though.

\begin{code}
{-# INLINE newSTArray #-}
newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
newSTArray (l,u) initial = ST $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray# n# initial s1#       of { (# s2#, marr# #) ->
    (# s2#, STArray l u n marr# #) }}

{-# INLINE boundsSTArray #-}
boundsSTArray :: STArray s i e -> (i,i)
boundsSTArray (STArray l u _ _) = (l,u)

{-# INLINE numElementsSTArray #-}
numElementsSTArray :: STArray s i e -> Int
numElementsSTArray (STArray _ _ n _) = n

{-# INLINE readSTArray #-}
readSTArray :: Ix i => STArray s i e -> i -> ST s e
readSTArray marr@(STArray l u n _) i =
    unsafeReadSTArray marr (safeIndex (l,u) n i)

{-# INLINE unsafeReadSTArray #-}
unsafeReadSTArray :: Ix i => STArray s i e -> Int -> ST s e
unsafeReadSTArray (STArray _ _ _ marr#) (I# i#)
    = ST $ \s1# -> readArray# marr# i# s1#

{-# INLINE writeSTArray #-}
writeSTArray :: Ix i => STArray s i e -> i -> e -> ST s ()
writeSTArray marr@(STArray l u n _) i e =
    unsafeWriteSTArray marr (safeIndex (l,u) n i) e

{-# INLINE unsafeWriteSTArray #-}
unsafeWriteSTArray :: Ix i => STArray s i e -> Int -> e -> ST s ()
unsafeWriteSTArray (STArray _ _ _ marr#) (I# i#) e = ST $ \s1# ->
    case writeArray# marr# i# e s1# of
        s2# -> (# s2#, () #)
\end{code}


%*********************************************************
%*                                                      *
\subsection{Moving between mutable and immutable}
%*                                                      *
%*********************************************************

\begin{code}
freezeSTArray :: Ix i => STArray s i e -> ST s (Array i e)
freezeSTArray (STArray l u n@(I# n#) marr#) = ST $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr'# #) ->
    let copy i# s3# | isTrue# (i# ==# n#) = s3#
                    | otherwise =
            case readArray# marr# i# s3# of { (# s4#, e #) ->
            case writeArray# marr'# i# e s4# of { s5# ->
            copy (i# +# 1#) s5# }} in
    case copy 0# s2#                    of { s3# ->
    case unsafeFreezeArray# marr'# s3#  of { (# s4#, arr# #) ->
    (# s4#, Array l u n arr# #) }}}

{-# INLINE unsafeFreezeSTArray #-}
unsafeFreezeSTArray :: Ix i => STArray s i e -> ST s (Array i e)
unsafeFreezeSTArray (STArray l u n marr#) = ST $ \s1# ->
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    (# s2#, Array l u n arr# #) }

thawSTArray :: Ix i => Array i e -> ST s (STArray s i e)
thawSTArray (Array l u n@(I# n#) arr#) = ST $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let copy i# s3# | isTrue# (i# ==# n#) = s3#
                    | otherwise =
            case indexArray# arr# i#    of { (# e #) ->
            case writeArray# marr# i# e s3# of { s4# ->
            copy (i# +# 1#) s4# }} in
    case copy 0# s2#                    of { s3# ->
    (# s3#, STArray l u n marr# #) }}

{-# INLINE unsafeThawSTArray #-}
unsafeThawSTArray :: Ix i => Array i e -> ST s (STArray s i e)
unsafeThawSTArray (Array l u n arr#) = ST $ \s1# ->
    case unsafeThawArray# arr# s1#      of { (# s2#, marr# #) ->
    (# s2#, STArray l u n marr# #) }
\end{code}
