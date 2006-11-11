\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude -fno-bang-patterns #-}
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

-- #hide
module GHC.Arr where

import {-# SOURCE #-} GHC.Err ( error )
import GHC.Enum
import GHC.Num
import GHC.ST
import GHC.Base
import GHC.List
import GHC.Show

infixl 9  !, //

default ()
\end{code}


%*********************************************************
%*							*
\subsection{The @Ix@ class}
%*							*
%*********************************************************

\begin{code}
-- | The 'Ix' class is used to map a contiguous subrange of values in
-- a type onto integers.  It is used primarily for array indexing
-- (see "Data.Array", "Data.Array.IArray" and "Data.Array.MArray").
--
-- The first argument @(l,u)@ of each of these operations is a pair
-- specifying the lower and upper bounds of a contiguous subrange of values.
--
-- An implementation is entitled to assume the following laws about these
-- operations:
--
-- * @'inRange' (l,u) i == 'elem' i ('range' (l,u))@
--
-- * @'range' (l,u) '!!' 'index' (l,u) i == i@, when @'inRange' (l,u) i@
--
-- * @'map' ('index' (l,u)) ('range' (l,u))) == [0..'rangeSize' (l,u)-1]@
--
-- * @'rangeSize' (l,u) == 'length' ('range' (l,u))@
--
-- Minimal complete instance: 'range', 'index' and 'inRange'.
--
class (Ord a) => Ix a where
    -- | The list of values in the subrange defined by a bounding pair.
    range		:: (a,a) -> [a]
    -- | The position of a subscript in the subrange.
    index		:: (a,a) -> a -> Int
    -- | Like 'index', but without checking that the value is in range.
    unsafeIndex		:: (a,a) -> a -> Int
    -- | Returns 'True' the given subscript lies in the range defined
    -- the bounding pair.
    inRange		:: (a,a) -> a -> Bool
    -- | The size of the subrange defined by a bounding pair.
    rangeSize		:: (a,a) -> Int
    -- | like 'rangeSize', but without checking that the upper bound is
    -- in range.
    unsafeRangeSize     :: (a,a) -> Int

	-- Must specify one of index, unsafeIndex
    index b i | inRange b i = unsafeIndex b i	
	      | otherwise   = error "Error in array index"
    unsafeIndex b i = index b i

    rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
		       | otherwise   = 0	-- This case is only here to
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
%*							*
\subsection{Instances of @Ix@}
%*							*
%*********************************************************

\begin{code}
-- abstract these errors from the relevant index functions so that
-- the guts of the function will be small enough to inline.

{-# NOINLINE indexError #-}
indexError :: Show a => (a,a) -> a -> String -> b
indexError rng i tp
  = error (showString "Ix{" . showString tp . showString "}.index: Index " .
           showParen True (showsPrec 0 i) .
	   showString " out of range " $
	   showParen True (showsPrec 0 rng) "")

----------------------------------------------------------------------
instance  Ix Char  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = fromEnum i - fromEnum m

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Char"

    inRange (m,n) i	=  m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Int  where
    {-# INLINE range #-}
	-- The INLINE stops the build in the RHS from getting inlined,
	-- so that callers can fuse with the result of range
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = i - m

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Int"

    {-# INLINE inRange #-}
    inRange (I# m,I# n) (I# i) =  m <=# i && i <=# n

----------------------------------------------------------------------
instance  Ix Integer  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i   = fromInteger (i - m)

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Integer"

    inRange (m,n) i	=  m <= i && i <= n

----------------------------------------------------------------------
instance Ix Bool where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Bool"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix Ordering where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

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
    {-# INLINE index #-}
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
%*							*
\subsection{The @Array@ types}
%*							*
%*********************************************************

\begin{code}
type IPr = (Int, Int)

-- | The type of immutable non-strict (boxed) arrays
-- with indices in @i@ and elements in @e@.
data Ix i => Array     i e = Array   !i !i (Array# e)

-- | Mutable, boxed, non-strict arrays in the 'ST' monad.  The type
-- arguments are as follows:
--
--  * @s@: the state variable argument for the 'ST' type
--
--  * @i@: the index type of the array (should be an instance of 'Ix')
--
--  * @e@: the element type of the array.
--
data         STArray s i e = STArray !i !i (MutableArray# s e)
	-- No Ix context for STArray.  They are stupid,
	-- and force an Ix context on the equality instance.

-- Just pointer equality on mutable arrays:
instance Eq (STArray s i e) where
    STArray _ _ arr1# == STArray _ _ arr2# =
        sameMutableArray# arr1# arr2#
\end{code}


%*********************************************************
%*							*
\subsection{Operations on immutable arrays}
%*							*
%*********************************************************

\begin{code}
{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"

-- | Construct an array with the specified bounds and containing values
-- for given indices within these bounds.
--
-- The array is undefined (i.e. bottom) if any index in the list is
-- out of bounds.  The Haskell 98 Report further specifies that if any
-- two associations in the list have the same index, the value at that
-- index is undefined (i.e. bottom).  However in GHC's implementation,
-- the value at such an index is the value part of the last association
-- with that index in the list.
--
-- Because the indices must be checked for these errors, 'array' is
-- strict in the bounds argument and in the indices of the association
-- list, but nonstrict in the values.  Thus, recurrences such as the
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
	=> (i,i)	-- ^ a pair of /bounds/, each of the index type
			-- of the array.  These bounds are the lowest and
			-- highest indices in the array, in that order.
			-- For example, a one-origin vector of length
			-- '10' has bounds '(1,10)', and a one-origin '10'
			-- by '10' matrix has bounds '((1,1),(10,10))'.
	-> [(i, e)]	-- ^ a list of /associations/ of the form
			-- (/index/, /value/).  Typically, this list will
			-- be expressed as a comprehension.  An
			-- association '(i, x)' defines the value of
			-- the array at index 'i' to be 'x'.
	-> Array i e
array (l,u) ies = unsafeArray (l,u) [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE unsafeArray #-}
unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray (l,u) ies = runST (ST $ \s1# ->
    case rangeSize (l,u)                of { I# n# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    foldr (fill marr#) (done l u marr#) ies s2# }})

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) next s1# =
    case writeArray# marr# i# e s1#     of { s2# ->
    next s2# }

{-# INLINE done #-}
done :: Ix i => i -> i -> MutableArray# s e -> STRep s (Array i e)
done l u marr# s1# =
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    (# s2#, Array l u arr# #) }

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
    case rangeSize (l,u)                of { I# n# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let fillFromList i# xs s3# | i# ==# n# = s3#
                               | otherwise = case xs of
            []   -> s3#
            y:ys -> case writeArray# marr# i# y s3# of { s4# ->
                    fillFromList (i# +# 1#) ys s4# } in
    case fillFromList 0# es s2#         of { s3# ->
    done l u marr# s3# }}})

-- | The value at the given index in an array.
{-# INLINE (!) #-}
(!) :: Ix i => Array i e -> i -> e
arr@(Array l u _) ! i = unsafeAt arr (index (l,u) i)

{-# INLINE unsafeAt #-}
unsafeAt :: Ix i => Array i e -> Int -> e
unsafeAt (Array _ _ arr#) (I# i#) =
    case indexArray# arr# i# of (# e #) -> e

-- | The bounds with which an array was constructed.
{-# INLINE bounds #-}
bounds :: Ix i => Array i e -> (i,i)
bounds (Array l u _) = (l,u)

-- | The list of indices of an array in ascending order.
{-# INLINE indices #-}
indices :: Ix i => Array i e -> [i]
indices (Array l u _) = range (l,u)

-- | The list of elements of an array in index order.
{-# INLINE elems #-}
elems :: Ix i => Array i e -> [e]
elems arr@(Array l u _) =
    [unsafeAt arr i | i <- [0 .. rangeSize (l,u) - 1]]

-- | The list of associations of an array in index order.
{-# INLINE assocs #-}
assocs :: Ix i => Array i e -> [(i, e)]
assocs arr@(Array l u _) =
    [(i, unsafeAt arr (unsafeIndex (l,u) i)) | i <- range (l,u)]

-- | The 'accumArray' deals with repeated indices in the association
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
	=> (e -> a -> e)	-- ^ accumulating function
	-> e			-- ^ initial value
	-> (i,i)		-- ^ bounds of the array
	-> [(i, a)]		-- ^ association list
	-> Array i e
accumArray f init (l,u) ies =
    unsafeAccumArray f init (l,u) [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE unsafeAccumArray #-}
unsafeAccumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(Int, a)] -> Array i e
unsafeAccumArray f init (l,u) ies = runST (ST $ \s1# ->
    case rangeSize (l,u)                of { I# n# ->
    case newArray# n# init s1#          of { (# s2#, marr# #) ->
    foldr (adjust f marr#) (done l u marr#) ies s2# }})

{-# INLINE adjust #-}
adjust :: (e -> a -> e) -> MutableArray# s e -> (Int, a) -> STRep s b -> STRep s b
adjust f marr# (I# i#, new) next s1# =
    case readArray# marr# i# s1#        of { (# s2#, old #) ->
    case writeArray# marr# i# (f old new) s2# of { s3# ->
    next s3# }}

-- | Constructs an array identical to the first argument except that it has
-- been updated by the associations in the right argument.
-- For example, if @m@ is a 1-origin, @n@ by @n@ matrix, then
--
-- > m//[((i,i), 0) | i <- [1..n]]
--
-- is the same matrix, except with the diagonal zeroed.
--
-- Repeated indices in the association list are handled as for 'array':
-- Haskell 98 specifies that the resulting array is undefined (i.e. bottom),
-- but GHC's implementation uses the last association for each index.
{-# INLINE (//) #-}
(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
arr@(Array l u _) // ies =
    unsafeReplace arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE unsafeReplace #-}
unsafeReplace :: Ix i => Array i e -> [(Int, e)] -> Array i e
unsafeReplace arr@(Array l u _) ies = runST (do
    STArray _ _ marr# <- thawSTArray arr
    ST (foldr (fill marr#) (done l u marr#) ies))

-- | @'accum' f@ takes an array and an association list and accumulates
-- pairs from the list into the array with the accumulating function @f@.
-- Thus 'accumArray' can be defined using 'accum':
--
-- > accumArray f z b = accum f (array b [(i, z) | i <- range b])
--
{-# INLINE accum #-}
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f arr@(Array l u _) ies =
    unsafeAccum f arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE unsafeAccum #-}
unsafeAccum :: Ix i => (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f arr@(Array l u _) ies = runST (do
    STArray _ _ marr# <- thawSTArray arr
    ST (foldr (adjust f marr#) (done l u marr#) ies))

{-# INLINE amap #-}
amap :: Ix i => (a -> b) -> Array i a -> Array i b
amap f arr@(Array l u _) =
    unsafeArray (l,u) [(i, f (unsafeAt arr i)) | i <- [0 .. rangeSize (l,u) - 1]]

-- | 'ixmap' allows for transformations on array indices.
-- It may be thought of as providing function composition on the right
-- with the mapping that the original array embodies.
--
-- A similar transformation of array values may be achieved using 'fmap'
-- from the 'Array' instance of the 'Functor' class.
{-# INLINE ixmap #-}
ixmap :: (Ix i, Ix j) => (i,i) -> (i -> j) -> Array j e -> Array i e
ixmap (l,u) f arr =
    unsafeArray (l,u) [(unsafeIndex (l,u) i, arr ! f i) | i <- range (l,u)]

{-# INLINE eqArray #-}
eqArray :: (Ix i, Eq e) => Array i e -> Array i e -> Bool
eqArray arr1@(Array l1 u1 _) arr2@(Array l2 u2 _) =
    if rangeSize (l1,u1) == 0 then rangeSize (l2,u2) == 0 else
    l1 == l2 && u1 == u2 &&
    and [unsafeAt arr1 i == unsafeAt arr2 i | i <- [0 .. rangeSize (l1,u1) - 1]]

{-# INLINE cmpArray #-}
cmpArray :: (Ix i, Ord e) => Array i e -> Array i e -> Ordering
cmpArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntArray #-}
cmpIntArray :: Ord e => Array Int e -> Array Int e -> Ordering
cmpIntArray arr1@(Array l1 u1 _) arr2@(Array l2 u2 _) =
    if rangeSize (l1,u1) == 0 then if rangeSize (l2,u2) == 0 then EQ else LT else
    if rangeSize (l2,u2) == 0 then GT else
    case compare l1 l2 of
        EQ    -> foldr cmp (compare u1 u2) [0 .. rangeSize (l1, min u1 u2) - 1]
        other -> other
    where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpArray/Int" cmpArray = cmpIntArray #-}
\end{code}


%*********************************************************
%*							*
\subsection{Array instances}
%*							*
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
%*							*
\subsection{Operations on mutable arrays}
%*							*
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
newSTArray (l,u) init = ST $ \s1# ->
    case rangeSize (l,u)                of { I# n# ->
    case newArray# n# init s1#          of { (# s2#, marr# #) ->
    (# s2#, STArray l u marr# #) }}

{-# INLINE boundsSTArray #-}
boundsSTArray :: STArray s i e -> (i,i)  
boundsSTArray (STArray l u _) = (l,u)

{-# INLINE readSTArray #-}
readSTArray :: Ix i => STArray s i e -> i -> ST s e
readSTArray marr@(STArray l u _) i =
    unsafeReadSTArray marr (index (l,u) i)

{-# INLINE unsafeReadSTArray #-}
unsafeReadSTArray :: Ix i => STArray s i e -> Int -> ST s e
unsafeReadSTArray (STArray _ _ marr#) (I# i#) = ST $ \s1# ->
    readArray# marr# i# s1#

{-# INLINE writeSTArray #-}
writeSTArray :: Ix i => STArray s i e -> i -> e -> ST s () 
writeSTArray marr@(STArray l u _) i e =
    unsafeWriteSTArray marr (index (l,u) i) e

{-# INLINE unsafeWriteSTArray #-}
unsafeWriteSTArray :: Ix i => STArray s i e -> Int -> e -> ST s () 
unsafeWriteSTArray (STArray _ _ marr#) (I# i#) e = ST $ \s1# ->
    case writeArray# marr# i# e s1#     of { s2# ->
    (# s2#, () #) }
\end{code}


%*********************************************************
%*							*
\subsection{Moving between mutable and immutable}
%*							*
%*********************************************************

\begin{code}
freezeSTArray :: Ix i => STArray s i e -> ST s (Array i e)
freezeSTArray (STArray l u marr#) = ST $ \s1# ->
    case rangeSize (l,u)                of { I# n# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr'# #) ->
    let copy i# s3# | i# ==# n# = s3#
                    | otherwise =
            case readArray# marr# i# s3# of { (# s4#, e #) ->
            case writeArray# marr'# i# e s4# of { s5# ->
            copy (i# +# 1#) s5# }} in
    case copy 0# s2#                    of { s3# ->
    case unsafeFreezeArray# marr'# s3#  of { (# s4#, arr# #) ->
    (# s4#, Array l u arr# #) }}}}

{-# INLINE unsafeFreezeSTArray #-}
unsafeFreezeSTArray :: Ix i => STArray s i e -> ST s (Array i e)
unsafeFreezeSTArray (STArray l u marr#) = ST $ \s1# ->
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    (# s2#, Array l u arr# #) }

thawSTArray :: Ix i => Array i e -> ST s (STArray s i e)
thawSTArray (Array l u arr#) = ST $ \s1# ->
    case rangeSize (l,u)                of { I# n# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let copy i# s3# | i# ==# n# = s3#
                    | otherwise =
            case indexArray# arr# i#    of { (# e #) ->
            case writeArray# marr# i# e s3# of { s4# ->
            copy (i# +# 1#) s4# }} in
    case copy 0# s2#                    of { s3# ->
    (# s3#, STArray l u marr# #) }}}

{-# INLINE unsafeThawSTArray #-}
unsafeThawSTArray :: Ix i => Array i e -> ST s (STArray s i e)
unsafeThawSTArray (Array l u arr#) = ST $ \s1# ->
    case unsafeThawArray# arr# s1#      of { (# s2#, marr# #) ->
    (# s2#, STArray l u marr# #) }
\end{code}
