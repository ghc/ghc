module PreludeBuiltin where

import Prel		( (&&), atan2 )
import Cls
import Core
import IBool
import IChar
import IComplex
import IDouble
import IFloat
import IInt
import IInteger
import ITup2
import List		( (++) )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

instance (Eq a) => Eq [a]  where
    []     == []     = True	
    (x:xs) == (y:ys) = x == y && xs == ys
    []     == ys     = False			
    xs     == []     = False			
    xs     /= ys     = if (xs == ys) then False else True

instance (Ord a) => Ord [a] where
    a <  b  = case _tagCmp a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a <= b  = case _tagCmp a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a >= b  = case _tagCmp a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >  b  = case _tagCmp a b of { _LT -> False; _EQ -> False; _GT -> True  }

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp []     []     = _EQ
    _tagCmp (x:xs) []     = _GT
    _tagCmp []     (y:ys) = _LT
    _tagCmp (x:xs) (y:ys) = case _tagCmp x y of
                                 _LT -> _LT	
			         _GT -> _GT		
				 _EQ -> _tagCmp xs ys

instance  (Text a) => Text [a]  where
    readsPrec p    = readList
    showsPrec p xs = showList xs
    readList	   = _readList (readsPrec 0)
    showList       = _showList (showsPrec 0)

{-# SPECIALIZE instance Eq   [Char] #-}
{-# SPECIALIZE instance Ord  [Char] #-}
{-# SPECIALIZE instance Text [Char] #-}

{-# SPECIALIZE instance Eq   [Int] #-}
{-# SPECIALIZE instance Ord  [Int] #-}
{-# SPECIALIZE instance Text [Int] #-}

{-# SPECIALIZE instance Eq   [Integer] #-}
{-# SPECIALIZE instance Ord  [Integer] #-}
{-# SPECIALIZE instance Text [Integer] #-}

--NO:{-# SPECIALIZE instance Eq   [Float] #-}
--NO:{-# SPECIALIZE instance Ord  [Float] #-}
--NO:{-# SPECIALIZE instance Text [Float] #-}

{-# SPECIALIZE instance Eq   [Double] #-}
{-# SPECIALIZE instance Ord  [Double] #-}
{-# SPECIALIZE instance Text [Double] #-}

{-# SPECIALIZE instance Eq   [Bool] #-}
{-# SPECIALIZE instance Ord  [Bool] #-}
{-# SPECIALIZE instance Text [Bool] #-}

{-# SPECIALIZE instance Eq   [[Char]] #-}
{-# SPECIALIZE instance Ord  [[Char]] #-}
{-# SPECIALIZE instance Text [[Char]] #-}

{-# SPECIALIZE instance Eq   [[Int]] #-}
{-# SPECIALIZE instance Ord  [[Int]] #-}
{-# SPECIALIZE instance Text [[Int]] #-}

{-# SPECIALIZE instance Eq   [Complex Double] #-}
{-# SPECIALIZE instance Text [Complex Double] #-}

{-# SPECIALIZE instance Eq   [(Int,Int)] #-}

#if defined(__OVERLAPPING_INSTANCES__)
instance _CCallable   [Char]
instance _CReturnable [Char]
#endif

#if defined(__UNBOXED_INSTANCES__)

{-# GENERATE_SPECS instance a :: Eq   [a] #-}
{-# GENERATE_SPECS instance a :: Ord  [a] #-}
{-# GENERATE_SPECS instance a :: Text [a] #-}

#if defined(__OVERLAPPING_INSTANCES__)
instance _CCallable   [Char#]
instance _CReturnable [Char#]
#endif

#endif




