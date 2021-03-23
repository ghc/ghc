{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, RoleAnnotations #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

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

        arrEleBottom, array, listArray,
        (!), safeRangeSize, negRange, safeIndex, badSafeIndex,
        bounds, numElements, numElementsSTArray, indices, elems,
        assocs, accumArray, adjust, (//), accum,
        amap, ixmap,
        eqArray, cmpArray, cmpIntArray,
        newSTArray, boundsSTArray,
        readSTArray, writeSTArray, copySTArray,
        freezeSTArray, shrinkFreezeSTArrayInt, thawSTArray,
        foldlElems, foldlElems', foldl1Elems,
        foldrElems, foldrElems', foldr1Elems,

        -- * Unsafe operations
        fill, done,
        unsafeArray, unsafeArray',
        lessSafeIndex, unsafeAt, unsafeReplace,
        unsafeAccumArray, unsafeAccumArray', unsafeAccum,
        unsafeReadSTArray, unsafeWriteSTArray,
        unsafeFreezeSTArray, unsafeThawSTArray,
    ) where

import GHC.Num
import GHC.ST
import GHC.Base
import GHC.List
import GHC.Ix
import GHC.Show

infixl 9  !, //

default ()

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

-- Index types should have nominal role, because of Ix class. See also #9220.
type role Array nominal representational
type role STArray nominal nominal representational

-- Just pointer equality on mutable arrays:
-- | @since 2.01
instance Eq (STArray s i e) where
    STArray _ _ _ arr1# == STArray _ _ _ arr2# =
        isTrue# (sameMutableArray# arr1# arr2#)

----------------------------------------------------------------------
-- Operations on immutable arrays

{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = errorWithoutStackTrace "(Array.!): undefined array element"

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
                        -- @10@ has bounds @(1,10)@, and a one-origin @10@
                        -- by @10@ matrix has bounds @((1,1),(10,10))@.
        -> [(i, e)]     -- ^ a list of /associations/ of the form
                        -- (/index/, /value/).  Typically, this list will
                        -- be expressed as a comprehension.  An
                        -- association @(i, x)@ defines the value of
                        -- the array at index @i@ to be @x@.
        -> Array i e
array (l,u) ies
    = let n = safeRangeSize (l,u)
      in unsafeArray' (l,u) n
                      [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeArray #-}
unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray b ies = unsafeArray' b (rangeSize b) ies

{-# INLINE unsafeArray' #-}
unsafeArray' :: (i,i) -> Int -> [(Int, e)] -> Array i e
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
done :: i -> i -> Int -> MutableArray# s e -> STRep s (Array i e)
-- See NB on 'fill'
-- Make sure it is strict in 'n'
done l u n@(I# _) marr#
  = \s1# -> case unsafeFreezeArray# marr# s1# of
              (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

-- | Construct an array from a pair of bounds and a list of values in
-- index order.
{-# INLINE listArray #-}
listArray :: Ix i => (i,i) -> [e] -> Array i e
listArray (l,u) es = runST (ST $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
      let
        go y r = \ i# s3# ->
            case writeArray# marr# i# y s3# of
              s4# -> if (isTrue# (i# ==# n# -# 1#))
                     then s4#
                     else r (i# +# 1#) s4#
      in
        done l u n marr# (
          if n == 0
          then s2#
          else foldr go (\_ s# -> s#) es 0# s2#)}})

-- | The value at the given index in an array.
{-# INLINE (!) #-}
(!) :: Ix i => Array i e -> i -> e
(!) arr@(Array l u n _) i = unsafeAt arr $ safeIndex (l,u) n i

{-# INLINE (!#) #-}
(!#) :: Ix i => Array i e -> i -> (# e #)
(!#) arr@(Array l u n _) i = unsafeAt# arr $ safeIndex (l,u) n i

{-# INLINE safeRangeSize #-}
safeRangeSize :: Ix i => (i, i) -> Int
safeRangeSize (l,u) = let r = rangeSize (l, u)
                      in if r < 0 then negRange
                                  else r

-- Don't inline this error message everywhere!!
negRange :: Int   -- Uninformative, but Ix does not provide Show
negRange = errorWithoutStackTrace "Negative range size"

{-# INLINE[1] safeIndex #-}
-- See Note [Double bounds-checking of index values]
-- Inline *after* (!) so the rules can fire
-- Make sure it is strict in n
safeIndex :: Ix i => (i, i) -> Int -> i -> Int
safeIndex (l,u) n@(I# _) i
  | (0 <= i') && (i' < n) = i'
  | otherwise             = badSafeIndex i' n
  where
    i' = index (l,u) i

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
badSafeIndex i' n = errorWithoutStackTrace ("Error in array index; " ++ show i' ++
                        " not in range [0.." ++ show n ++ ")")

{-# INLINE unsafeAt #-}
unsafeAt :: Array i e -> Int -> e
unsafeAt (Array _ _ _ arr#) (I# i#) =
    case indexArray# arr# i# of (# e #) -> e

-- | Look up an element in an array without forcing it
unsafeAt# :: Array i e -> Int -> (# e #)
unsafeAt# (Array _ _ _ arr#) (I# i#) = indexArray# arr# i#

-- | A convenient version of unsafeAt#
unsafeAtA :: Applicative f
          => Array i e -> Int -> f e
unsafeAtA ary i = case unsafeAt# ary i of (# e #) -> pure e

-- | The bounds with which an array was constructed.
{-# INLINE bounds #-}
bounds :: Array i e -> (i,i)
bounds (Array l u _ _) = (l,u)

-- | The number of elements in the array.
{-# INLINE numElements #-}
numElements :: Array i e -> Int
numElements (Array _ _ n _) = n

-- | The list of indices of an array in ascending order.
{-# INLINE indices #-}
indices :: Ix i => Array i e -> [i]
indices (Array l u _ _) = range (l,u)

-- | The list of elements of an array in index order.
{-# INLINE elems #-}
elems :: Array i e -> [e]
elems arr@(Array _ _ n _) =
    [e | i <- [0 .. n - 1], e <- unsafeAtA arr i]

-- | A right fold over the elements
{-# INLINABLE foldrElems #-}
foldrElems :: (a -> b -> b) -> b -> Array i a -> b
foldrElems f b0 = \ arr@(Array _ _ n _) ->
  let
    go i | i == n    = b0
         | (# e #) <- unsafeAt# arr i
         = f e (go (i+1))
  in go 0

-- | A left fold over the elements
{-# INLINABLE foldlElems #-}
foldlElems :: (b -> a -> b) -> b -> Array i a -> b
foldlElems f b0 = \ arr@(Array _ _ n _) ->
  let
    go i | i == (-1) = b0
         | (# e #) <- unsafeAt# arr i
         = f (go (i-1)) e
  in go (n-1)

-- | A strict right fold over the elements
{-# INLINABLE foldrElems' #-}
foldrElems' :: (a -> b -> b) -> b -> Array i a -> b
foldrElems' f b0 = \ arr@(Array _ _ n _) ->
  let
    go i a | i == (-1) = a
           | (# e #) <- unsafeAt# arr i
           = go (i-1) (f e $! a)
  in go (n-1) b0

-- | A strict left fold over the elements
{-# INLINABLE foldlElems' #-}
foldlElems' :: (b -> a -> b) -> b -> Array i a -> b
foldlElems' f b0 = \ arr@(Array _ _ n _) ->
  let
    go i a | i == n    = a
           | (# e #) <- unsafeAt# arr i
           = go (i+1) (a `seq` f a e)
  in go 0 b0

-- | A left fold over the elements with no starting value
{-# INLINABLE foldl1Elems #-}
foldl1Elems :: (a -> a -> a) -> Array i a -> a
foldl1Elems f = \ arr@(Array _ _ n _) ->
  let
    go i | i == 0    = unsafeAt arr 0
         | (# e #) <- unsafeAt# arr i
         = f (go (i-1)) e
  in
    if n == 0 then errorWithoutStackTrace "foldl1: empty Array" else go (n-1)

-- | A right fold over the elements with no starting value
{-# INLINABLE foldr1Elems #-}
foldr1Elems :: (a -> a -> a) -> Array i a -> a
foldr1Elems f = \ arr@(Array _ _ n _) ->
  let
    go i | i == n-1  = unsafeAt arr i
         | (# e #) <- unsafeAt# arr i
         = f e (go (i + 1))
  in
    if n == 0 then errorWithoutStackTrace "foldr1: empty Array" else go 0

-- | The list of associations of an array in index order.
{-# INLINE assocs #-}
assocs :: Ix i => Array i e -> [(i, e)]
assocs arr@(Array l u _ _) =
    [(i, e) | i <- range (l,u), let !(# e #) = arr !# i]

-- | The 'accumArray' function deals with repeated indices in the association
-- list using an /accumulating function/ which combines the values of
-- associations with the same index.
--
-- For example, given a list of values of some index type, @hist@
-- produces a histogram of the number of occurrences of each index within
-- a specified range:
--
-- > hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
-- > hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]
--
-- @accumArray@ is strict in each result of applying the accumulating
-- function, although it is lazy in the initial value. Thus, unlike
-- arrays built with 'array', accumulated arrays should not in general
-- be recursive.
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
unsafeAccumArray' :: (e -> a -> e) -> e -> (i,i) -> Int -> [(Int, a)] -> Array i e
unsafeAccumArray' f initial (l,u) n@(I# n#) ies = runST (ST $ \s1# ->
    case newArray# n# initial s1#          of { (# s2#, marr# #) ->
    foldr (adjust' f marr#) (done l u n marr#) ies s2# })

{-# INLINE adjust #-}
adjust :: (e -> a -> e) -> MutableArray# s e -> (Int, a) -> STRep s b -> STRep s b
-- See NB on 'fill'
adjust f marr# (I# i#, new) next
  = \s1# -> case readArray# marr# i# s1# of
                (# s2#, old #) ->
                    case writeArray# marr# i# (f old new) s2# of
                        s3# -> next s3#

{-# INLINE adjust' #-}
adjust' :: (e -> a -> e)
        -> MutableArray# s e
        -> (Int, a)
        -> STRep s b -> STRep s b
adjust' f marr# (I# i#, new) next
  = \s1# -> case readArray# marr# i# s1# of
                (# s2#, old #) ->
                    let !combined = f old new
                    in next (writeArray# marr# i# combined s2#)


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
unsafeReplace :: Array i e -> [(Int, e)] -> Array i e
unsafeReplace arr ies = runST (do
    STArray l u n marr# <- thawSTArray arr
    ST (foldr (fill marr#) (done l u n marr#) ies))

-- | @'accum' f@ takes an array and an association list and accumulates
-- pairs from the list into the array with the accumulating function @f@.
-- Thus 'accumArray' can be defined using 'accum':
--
-- > accumArray f z b = accum f (array b [(i, z) | i <- range b])
--
-- @accum@ is strict in all the results of applying the accumulation.
-- However, it is lazy in the initial values of the array.
{-# INLINE accum #-}
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f arr@(Array l u n _) ies =
    unsafeAccum f arr [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeAccum #-}
unsafeAccum :: (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f arr ies = runST (do
    STArray l u n marr# <- thawSTArray arr
    ST (foldr (adjust' f marr#) (done l u n marr#) ies))

{-# INLINE [1] amap #-}  -- See Note [amap]
amap :: (a -> b) -> Array i a -> Array i b
amap f arr@(Array l u n@(I# n#) _) = runST (ST $ \s1# ->
    case newArray# n# arrEleBottom s1# of
        (# s2#, marr# #) ->
          let go i s#
                | i == n    = done l u n marr# s#
                | (# e #) <- unsafeAt# arr i
                = fill marr# (i, f e) (go (i+1)) s#
          in go 0 s2# )

{- Note [amap]
~~~~~~~~~~~~~~
amap was originally defined like this:

 amap f arr@(Array l u n _) =
     unsafeArray' (l,u) n [(i, f (unsafeAt arr i)) | i <- [0 .. n - 1]]

There are two problems:

1. The enumFromTo implementation produces (spurious) code for the impossible
   case of n<0 that ends up duplicating the array freezing code.

2. This implementation relies on list fusion for efficiency. In order
   to implement the "amap/coerce" rule, we need to delay inlining amap
   until simplifier phase 1, which is when the eftIntList rule kicks
   in and makes that impossible.  (c.f. #8767)
-}


-- See Breitner, Eisenberg, Peyton Jones, and Weirich, "Safe Zero-cost
-- Coercions for Haskell", section 6.5:
--   http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf
{-# RULES
"amap/coerce" amap coerce = coerce  -- See Note [amap]
 #-}

-- Second functor law:
{-# RULES
"amap/amap" forall f g a . amap f (amap g a) = amap (f . g) a
 #-}

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

{-# INLINE [2] cmpArray #-}
-- See Note [Allow time for type-specialisation rules to fire] in GHC.Real
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

----------------------------------------------------------------------
-- Array instances

-- | @since 2.01
instance Functor (Array i) where
    fmap = amap

    {-# INLINE (<$) #-}
    x <$ Array l u n@(I# n#) _ =
        -- Sadly we can't just use 'newSTArray' (with 'unsafeFreezeSTArray')
        -- since that would require proof that the indices of the original array
        -- are instances of 'Ix'.
        runST $ ST $ \s1# ->
            case newArray# n# x s1# of
                (# s2#, marr# #) -> done l u n marr# s2#

-- | @since 2.01
instance (Ix i, Eq e) => Eq (Array i e) where
    (==) = eqArray

-- | @since 2.01
instance (Ix i, Ord e) => Ord (Array i e) where
    compare = cmpArray

-- | @since 2.01
instance (Ix a, Show a, Show b) => Show (Array a b) where
    showsPrec p a =
        showParen (p > appPrec) $
        showString "array " .
        showsPrec appPrec1 (bounds a) .
        showChar ' ' .
        showsPrec appPrec1 (assocs a)
        -- Precedence of 'array' is the precedence of application

-- The Read instance is in GHC.Read

----------------------------------------------------------------------
-- Operations on mutable arrays

{-
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
-}

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
unsafeReadSTArray :: STArray s i e -> Int -> ST s e
unsafeReadSTArray (STArray _ _ _ marr#) (I# i#)
    = ST $ \s1# -> readArray# marr# i# s1#

{-# INLINE writeSTArray #-}
writeSTArray :: Ix i => STArray s i e -> i -> e -> ST s ()
writeSTArray marr@(STArray l u n _) i e =
    unsafeWriteSTArray marr (safeIndex (l,u) n i) e

{-# INLINE unsafeWriteSTArray #-}
unsafeWriteSTArray :: STArray s i e -> Int -> e -> ST s ()
unsafeWriteSTArray (STArray _ _ _ marr#) (I# i#) e = ST $ \s1# ->
    case writeArray# marr# i# e s1# of
        s2# -> (# s2#, () #)

{-# INLINE copySTArray #-}
copySTArray :: STArray s i e -> Int -> STArray s i e -> Int -> Int -> ST s ()
copySTArray (STArray _ _ _ src#) (I# src_offs#) (STArray _ _ _ dst#) (I# dst_offs#) (I# n#) = ST $ \s1# ->
    case copyMutableArray# src# src_offs# dst# dst_offs# n# s1# of
        s2# -> (# s2#, () #)

----------------------------------------------------------------------
-- Moving between mutable and immutable

freezeSTArray :: STArray s i e -> ST s (Array i e)
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

{-# INLINE shrinkFreezeSTArrayInt #-}
shrinkFreezeSTArrayInt :: STArray s Int e -> (Int, Int) -> ST s (Array Int e)
shrinkFreezeSTArrayInt (STArray l u _ marr#) (l', u') = ST $ \s1# ->
    let !(I# offs#)   = index (l, u) l'
        !n'@(I# len#) = rangeSize (l', u') in
    case freezeArray# marr# offs# len# s1#   of { (# s2#, arr# #) ->
    (# s2#, Array l' u' n' arr# #) }

{-# INLINE unsafeFreezeSTArray #-}
unsafeFreezeSTArray :: STArray s i e -> ST s (Array i e)
unsafeFreezeSTArray (STArray l u n marr#) = ST $ \s1# ->
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    (# s2#, Array l u n arr# #) }

thawSTArray :: Array i e -> ST s (STArray s i e)
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
unsafeThawSTArray :: Array i e -> ST s (STArray s i e)
unsafeThawSTArray (Array l u n arr#) = ST $ \s1# ->
    case unsafeThawArray# arr# s1#      of { (# s2#, marr# #) ->
    (# s2#, STArray l u n marr# #) }
