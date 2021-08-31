{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Ix
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC\'s Ix typeclass implementation.
--
-----------------------------------------------------------------------------

module GHC.Ix (
        Ix(..), indexError
    ) where

import GHC.Enum
import GHC.Num
import GHC.Base
import GHC.Real( fromIntegral )
import GHC.Show
import GHC.Tuple (Solo (..))

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
class (Ord a) => Ix a where
    {-# MINIMAL range, (index | unsafeIndex), inRange #-}

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

{-
Note that the following is NOT right
        rangeSize (l,h) | l <= h    = index b h + 1
                        | otherwise = 0

Because it might be the case that l<h, but the range
is nevertheless empty.  Consider
        ((1,2),(2,1))
Here l<h, but the second index ranges from 2..1 and
hence is empty


Note [Inlining index]
~~~~~~~~~~~~~~~~~~~~~
We inline the 'index' operation,

 * Partly because it generates much faster code
   (although bigger); see #1216

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
   by indexing at some bizarre location.  #1610

 * If you do (B) but not (A), you may get no complaint when you index
   an array out of its semantic bounds.  #2120

At various times we have had (A) and not (B), or (B) and not (A); both
led to complaints.  So now we implement *both* checks (#2669).

For 1-d, 2-d, and 3-d arrays of Int we have specialised instances to avoid this.

Note [Out-of-bounds error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default method for 'index' generates hoplelessIndexError, because
Ix doesn't have Show as a superclass.  For particular base types we
can do better, so we override the default method for index.
-}

-- Abstract these errors from the relevant index functions so that
-- the guts of the function will be small enough to inline.

{-# NOINLINE indexError #-}
indexError :: Show a => (a,a) -> a -> String -> b
indexError rng i tp
  = errorWithoutStackTrace (showString "Ix{" . showString tp . showString "}.index: Index " .
           showParen True (showsPrec 0 i) .
           showString " out of range " $
           showParen True (showsPrec 0 rng) "")

hopelessIndexError :: Int -- Try to use 'indexError' instead!
hopelessIndexError = errorWithoutStackTrace "Error in array index"

----------------------------------------------------------------------
-- | @since 2.01
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
-- | @since 2.01
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

-- | @since 4.6.0.0
instance Ix Word where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

----------------------------------------------------------------------
-- | @since 2.01
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
-- | @since 4.8.0.0
instance Ix Natural where
    range (m,n) = [m..n]
    inRange (m,n) i = m <= i && i <= n
    unsafeIndex (m,_) i = fromIntegral (i-m)
    index b i | inRange b i = unsafeIndex b i
              | otherwise   = indexError b i "Natural"

----------------------------------------------------------------------
-- | @since 2.01
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
-- | @since 2.01
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
-- | @since 2.01
instance Ix () where
    {-# INLINE range #-}
    range   ((), ())    = [()]
    {-# INLINE unsafeIndex #-}
    unsafeIndex   ((), ()) () = 0
    {-# INLINE inRange #-}
    inRange ((), ()) () = True

    {-# INLINE index #-}  -- See Note [Inlining index]
    index b i = unsafeIndex b i

instance Ix a => Ix (Solo a) where -- as derived
    {-# SPECIALISE instance Ix (Solo Int) #-}

    {-# INLINE range #-}
    range (Solo l, Solo u) =
      [ Solo i | i <- range (l,u) ]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (Solo l, Solo u) (Solo i) =
      unsafeIndex (l,u) i

    {-# INLINE inRange #-}
    inRange (Solo l, Solo u) (Solo i) =
      inRange (l, u) i

    -- Default method for index

----------------------------------------------------------------------
-- | @since 2.01
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
-- | @since 2.01
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
-- | @since 2.01
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

----------------------------------------------------------------------
-- | @since 2.01
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

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6) =>
      Ix (a1,a2,a3,a4,a5,a6)  where
    range ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) =
      [(i1,i2,i3,i4,i5,i6) | i1 <- range (l1,u1),
                             i2 <- range (l2,u2),
                             i3 <- range (l3,u3),
                             i4 <- range (l4,u4),
                             i5 <- range (l5,u5),
                             i6 <- range (l6,u6)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) (i1,i2,i3,i4,i5,i6) =
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))

    inRange ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) (i1,i2,i3,i4,i5,i6) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7) =>
      Ix (a1,a2,a3,a4,a5,a6,a7)  where
    range ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7)) =
      [(i1,i2,i3,i4,i5,i6,i7) | i1 <- range (l1,u1),
                                i2 <- range (l2,u2),
                                i3 <- range (l3,u3),
                                i4 <- range (l4,u4),
                                i5 <- range (l5,u5),
                                i6 <- range (l6,u6),
                                i7 <- range (l7,u7)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7))
        (i1,i2,i3,i4,i5,i6,i7) =
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7))
        (i1,i2,i3,i4,i5,i6,i7) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8) | i1 <- range (l1,u1),
                                   i2 <- range (l2,u2),
                                   i3 <- range (l3,u3),
                                   i4 <- range (l4,u4),
                                   i5 <- range (l5,u5),
                                   i6 <- range (l6,u6),
                                   i7 <- range (l7,u7),
                                   i8 <- range (l8,u8)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8))
        (i1,i2,i3,i4,i5,i6,i7,i8) =
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8))
        (i1,i2,i3,i4,i5,i6,i7,i8) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9) | i1 <- range (l1,u1),
                                      i2 <- range (l2,u2),
                                      i3 <- range (l3,u3),
                                      i4 <- range (l4,u4),
                                      i5 <- range (l5,u5),
                                      i6 <- range (l6,u6),
                                      i7 <- range (l7,u7),
                                      i8 <- range (l8,u8),
                                      i9 <- range (l9,u9)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9) =
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA),(u1,u2,u3,u4,u5,u6,u7,u8,u9,uA)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA) | i1 <- range (l1,u1),
                                         i2 <- range (l2,u2),
                                         i3 <- range (l3,u3),
                                         i4 <- range (l4,u4),
                                         i5 <- range (l5,u5),
                                         i6 <- range (l6,u6),
                                         i7 <- range (l7,u7),
                                         i8 <- range (l8,u8),
                                         i9 <- range (l9,u9),
                                         iA <- range (lA,uA)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA) =
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA),(u1,u2,u3,u4,u5,u6,u7,u8,u9,uA))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB) | i1 <- range (l1,u1),
                                            i2 <- range (l2,u2),
                                            i3 <- range (l3,u3),
                                            i4 <- range (l4,u4),
                                            i5 <- range (l5,u5),
                                            i6 <- range (l6,u6),
                                            i7 <- range (l7,u7),
                                            i8 <- range (l8,u8),
                                            i9 <- range (l9,u9),
                                            iA <- range (lA,uA),
                                            iB <- range (lB,uB)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB) =
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC) | i1 <- range (l1,u1),
                                               i2 <- range (l2,u2),
                                               i3 <- range (l3,u3),
                                               i4 <- range (l4,u4),
                                               i5 <- range (l5,u5),
                                               i6 <- range (l6,u6),
                                               i7 <- range (l7,u7),
                                               i8 <- range (l8,u8),
                                               i9 <- range (l9,u9),
                                               iA <- range (lA,uA),
                                               iB <- range (lB,uB),
                                               iC <- range (lC,uC)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC) =
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD) | i1 <- range (l1,u1),
                                                  i2 <- range (l2,u2),
                                                  i3 <- range (l3,u3),
                                                  i4 <- range (l4,u4),
                                                  i5 <- range (l5,u5),
                                                  i6 <- range (l6,u6),
                                                  i7 <- range (l7,u7),
                                                  i8 <- range (l8,u8),
                                                  i9 <- range (l9,u9),
                                                  iA <- range (lA,uA),
                                                  iB <- range (lB,uB),
                                                  iC <- range (lC,uC),
                                                  iD <- range (lD,uD)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD) =
      unsafeIndex (lD,uD) iD + unsafeRangeSize (lD,uD) * (
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC &&
      inRange (lD,uD) iD

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD, Ix aE) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD,aE)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE) | i1 <- range (l1,u1),
                                                     i2 <- range (l2,u2),
                                                     i3 <- range (l3,u3),
                                                     i4 <- range (l4,u4),
                                                     i5 <- range (l5,u5),
                                                     i6 <- range (l6,u6),
                                                     i7 <- range (l7,u7),
                                                     i8 <- range (l8,u8),
                                                     i9 <- range (l9,u9),
                                                     iA <- range (lA,uA),
                                                     iB <- range (lB,uB),
                                                     iC <- range (lC,uC),
                                                     iD <- range (lD,uD),
                                                     iE <- range (lE,uE)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE) =
      unsafeIndex (lE,uE) iE + unsafeRangeSize (lE,uE) * (
      unsafeIndex (lD,uD) iD + unsafeRangeSize (lD,uD) * (
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC &&
      inRange (lD,uD) iD && inRange (lE,uE) iE

    -- Default method for index

----------------------------------------------------------------------
-- | @since 4.15.0.0
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD, Ix aE, Ix aF) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD,aE,aF)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) | i1 <- range (l1,u1),
                                                        i2 <- range (l2,u2),
                                                        i3 <- range (l3,u3),
                                                        i4 <- range (l4,u4),
                                                        i5 <- range (l5,u5),
                                                        i6 <- range (l6,u6),
                                                        i7 <- range (l7,u7),
                                                        i8 <- range (l8,u8),
                                                        i9 <- range (l9,u9),
                                                        iA <- range (lA,uA),
                                                        iB <- range (lB,uB),
                                                        iC <- range (lC,uC),
                                                        iD <- range (lD,uD),
                                                        iE <- range (lE,uE),
                                                        iF <- range (lF,uF)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) =
      unsafeIndex (lF,uF) iF + unsafeRangeSize (lF,uF) * (
      unsafeIndex (lE,uE) iE + unsafeRangeSize (lE,uE) * (
      unsafeIndex (lD,uD) iD + unsafeRangeSize (lD,uD) * (
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC &&
      inRange (lD,uD) iD && inRange (lE,uE) iE &&
      inRange (lF,uF) iF

    -- Default method for index
