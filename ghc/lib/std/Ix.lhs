%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Ix]{Module @Ix@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Ix 
    (
	Ix
	  ( range	-- :: (Ix a) => (a,a) -> [a]
	  , index       -- :: (Ix a) => (a,a) -> a   -> Int
	  , inRange     -- :: (Ix a) => (a,a) -> a   -> Bool
	  )
    ,	rangeSize       -- :: (Ix a) => (a,a) -> Int
    -- Ix instances:
    --
    --  Ix Char
    --  Ix Int
    --  Ix Integer
    --  Ix Bool
    --  Ix Ordering
    --  Ix ()
    --  (Ix a, Ix b) => Ix (a, b)
    --  ...

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.
    ) where

#ifndef __HUGS__
import {-# SOURCE #-} PrelErr ( error )
import PrelTup
import PrelBase
import PrelList( null )
import PrelEnum
import PrelShow
import PrelNum
\end{code}

%*********************************************************
%*							*
\subsection{The @Ix@ class}
%*							*
%*********************************************************

\begin{code}
class  (Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index, unsafeIndex	:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool

	-- Must specify one of index, unsafeIndex
    index b i | inRange b i = unsafeIndex b i
	      | otherwise   = error "Error in array index"
    unsafeIndex b i = index b i
\end{code}

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

    {- INLINE range #-}
    range ((l1,l2),(u1,u2)) =
      [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

    {- INLINE unsafeIndex #-}
    unsafeIndex ((l1,l2),(u1,u2)) (i1,i2) =
      unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2

    {- INLINE inRange #-}
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

%********************************************************
%*							*
\subsection{Size of @Ix@ interval}
%*							*
%********************************************************

The @rangeSize@ operator returns the number of elements
in the range for an @Ix@ pair.

\begin{code}
{-# SPECIALISE unsafeRangeSize :: (Int,Int) -> Int #-}
{-# SPECIALISE unsafeRangeSize :: ((Int,Int),(Int,Int)) -> Int #-}
unsafeRangeSize :: (Ix a) => (a,a) -> Int
unsafeRangeSize b@(_l,h) = unsafeIndex b h + 1

{-# SPECIALISE rangeSize :: (Int,Int) -> Int #-}
{-# SPECIALISE rangeSize :: ((Int,Int),(Int,Int)) -> Int #-}
rangeSize :: (Ix a) => (a,a) -> Int
rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
		   | otherwise   = 0

-- Note that the following is NOT right
--	rangeSize (l,h) | l <= h    = index b h + 1
--			| otherwise = 0
--
-- Because it might be the case that l<h, but the range
-- is nevertheless empty.  Consider
--	((1,2),(2,1))
-- Here l<h, but the second index ranges from 2..1 and
-- hence is empty
\end{code}

\begin{code}
#else
-- This module is empty; Ix is currently defined in the prelude, but should
-- eventually be moved to this library file instead.
#endif
\end{code}
