%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Ix]{Module @Ix@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Ix 
    (
	Ix(range, index, inRange),
	rangeSize
    ) where

import {-# SOURCE #-} PrelErr ( error )
import PrelTup
import PrelBase
\end{code}

%*********************************************************
%*							*
\subsection{The @Ix@ class}
%*							*
%*********************************************************

\begin{code}
class  (Show a, Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index		:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool
\end{code}


%*********************************************************
%*							*
\subsection{Instances of @Ix@}
%*							*
%*********************************************************

\begin{code}
instance  Ix Char  where
    range (c,c')	=  [c..c']
    index b@(c,c') ci
	| inRange b ci	=  fromEnum ci - fromEnum c
	| otherwise	=  indexCharError ci b
    inRange (c,c') ci	=  fromEnum c <= i && i <= fromEnum c'
			   where i = fromEnum ci

instance  Ix Int  where
    range (m,n)		=  [m..n]
    index b@(m,n) i
	| inRange b i	=  i - m
	| otherwise	=  indexIntError i b
    inRange (m,n) i	=  m <= i && i <= n

-- abstract these errors from the relevant index functions so that
-- the guts of the function will be small enough to inline.

{-# NOINLINE indexCharError #-}
indexCharError :: Char -> (Char,Char) -> a
indexCharError ci b 
  = error (showString "Ix{Char}.index: Index " .
	   showParen True (showsPrec 0 ci) .
	   showString " out of range " $
 	   showParen True (showsPrec 0 b) "")

{-# NOINLINE indexIntError #-}
indexIntError :: Int -> (Int,Int) -> a
indexIntError i b
  = error (showString "Ix{Int}.index: Index " .
   	   showParen True (showsPrec 0 i) .
           showString " out of range " $
	   showParen True (showsPrec 0 b) "")

-- Integer instance is in PrelNum

----------------------------------------------------------------------
instance Ix Bool where -- as derived
    range   (l,u)   = map toEnum [fromEnum l .. fromEnum u]
    index   (l,u) i = fromEnum i - fromEnum l
    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix Ordering where -- as derived
    range   (l,u)   = map toEnum [fromEnum l .. fromEnum u]
    index   (l,u) i = fromEnum i - fromEnum l
    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix () where
    {-# INLINE range #-}
    range   ((), ())    = [()]
    {-# INLINE index #-}
    index   ((), ()) () = 0
    {-# INLINE inRange #-}
    inRange ((), ()) () = True

----------------------------------------------------------------------
instance (Ix a, Ix b) => Ix (a, b) where -- as derived
    {- INLINE range #-}
    range ((l1,l2),(u1,u2)) =
      [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

    {- INLINE index #-}
    index ((l1,l2),(u1,u2)) (i1,i2) =
      index (l1,u1) i1 * rangeSize (l2,u2) + index (l2,u2) i2

    {- INLINE inRange #-}
    inRange ((l1,l2),(u1,u2)) (i1,i2) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2

instance  (Ix a1, Ix a2, Ix a3) => Ix (a1,a2,a3)  where
    range ((l1,l2,l3),(u1,u2,u3)) =
        [(i1,i2,i3) | i1 <- range (l1,u1),
                      i2 <- range (l2,u2),
                      i3 <- range (l3,u3)]

    index ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      index (l3,u3) i3 + rangeSize (l3,u3) * (
      index (l2,u2) i2 + rangeSize (l2,u2) * (
      index (l1,u1) i1))

    inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3

instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
    range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
      [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                       i2 <- range (l2,u2),
                       i3 <- range (l3,u3),
                       i4 <- range (l4,u4)]

    index ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      index (l4,u4) i4 + rangeSize (l4,u4) * (
      index (l3,u3) i3 + rangeSize (l3,u3) * (
      index (l2,u2) i2 + rangeSize (l2,u2) * (
      index (l1,u1) i1)))

    inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1,a2,a3,a4,a5)  where
    range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
      [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
                          i2 <- range (l2,u2),
                          i3 <- range (l3,u3),
                          i4 <- range (l4,u4),
                          i5 <- range (l5,u5)]

    index ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      index (l5,u5) i5 + rangeSize (l5,u5) * (
      index (l4,u4) i4 + rangeSize (l4,u4) * (
      index (l3,u3) i3 + rangeSize (l3,u3) * (
      index (l2,u2) i2 + rangeSize (l2,u2) * (
      index (l1,u1) i1))))

    inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 && 
      inRange (l5,u5) i5
\end{code}

%********************************************************
%*							*
\subsection{Size of @Ix@ interval}
%*							*
%********************************************************

The @rangeSize@ operator returns the number of elements
in the range for an @Ix@ pair:

\begin{code}
{-# SPECIALISE rangeSize :: (Int,Int) -> Int #-}
rangeSize :: (Ix a) => (a,a) -> Int
rangeSize b@(l,h)
 | l > h     = 0
 | otherwise = index b h + 1

\end{code}
