module PreludeBuiltin where

import Prel		( (&&) )
import Cls
import Core
import IChar
import IInt
import List		( (++) )
import PS		( _PackedString, _unpackPS )
import Text

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
    readsPrec p = readList
    showsPrec p = showList

{-# SPECIALIZE instance Eq   [Char] #-}
{-# SPECIALIZE instance Ord  [Char] #-}
{-# SPECIALIZE instance Text [Char] #-}

#if defined(__OVERLAPPING_INSTANCES__)
instance _CCallable   [Char]
instance _CReturnable [Char]
#endif

{-# SPECIALIZE instance Eq   [Int]  #-}
{-# SPECIALIZE instance Ord  [Int] #-}
{-# SPECIALIZE instance Text [Int] #-}


#if defined(__UNBOXED_INSTANCES__)

{-# GENERATE_SPECS instance a :: Eq   [a] #-}
{-# GENERATE_SPECS instance a :: Ord  [a] #-}
{-# GENERATE_SPECS instance a :: Text [a] #-}

instance _CCallable   [Char#]
instance _CReturnable [Char#]

#endif
