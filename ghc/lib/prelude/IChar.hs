module PreludeCore ( Char(..) ) where

import Prel		( (.), (&&), chr, ord, otherwise, maxChar, minChar, not )
import Cls
import Core
import IInt
import IList
import List		( (++), map, takeWhile )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

gtChar	(C# x) (C# y) = gtChar# x y
geChar	(C# x) (C# y) = geChar# x y
eqChar	(C# x) (C# y) = eqChar# x y
neChar	(C# x) (C# y) = neChar# x y
ltChar	(C# x) (C# y) = ltChar# x y
leChar	(C# x) (C# y) = leChar# x y

---------------------------------------------------------------

instance  Eq Char  where
    (==) x y = eqChar x y
    (/=) x y = neChar x y

instance  Ord Char  where
    (<=) x y = leChar x y
    (<)	 x y = ltChar x y
    (>=) x y = geChar x y
    (>)  x y = gtChar x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp (C# a#) (C# b#)
      = if      (eqChar# a# b#) then _EQ
	else if (ltChar# a# b#) then _LT else _GT

instance  Ix Char  where
    range (c,c')	=  [c..c']
    index b@(c,c') ci
	| inRange b ci	=  ord ci - ord c
	| otherwise	=  error "Ix.Char.index{PreludeCore}: Index out of range\n"
    inRange (c,c') ci	=  ord c <= i && i <= ord c'
			   where i = ord ci

instance  Enum Char  where
    enumFrom c		 =  map chr [ord c .. ord maxChar]
    enumFromThen c c'	 =  map chr [ord c, ord c' .. ord lastChar]
			    where lastChar = if c' < c then minChar else maxChar
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Text Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> [(c,t) | ('\'':s,t)<- lex r,
					    (c,_)     <- readLitChar s])

    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s ])
	       where readl ('"':s)	= [("",s)]
		     readl ('\\':'&':s)	= readl s
		     readl s		= [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t	      ]

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':cs) = showString "\\\"" . showl cs
		       showl (c:cs)   = showLitChar c . showl cs

instance _CCallable   Char
instance _CReturnable Char

#if defined(__UNBOXED_INSTANCES__)
---------------------------------------------------------------
-- Instances for Char#
---------------------------------------------------------------

instance  Eq Char#  where
    (==) x y = eqChar# x y
    (/=) x y = neChar# x y

instance  Ord Char#  where
    (<=) x y = leChar# x y
    (<)	 x y = ltChar# x y
    (>=) x y = geChar# x y
    (>)  x y = gtChar# x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp a b
      = if      (eqChar# a b) then _EQ
	else if (ltChar# a b) then _LT else _GT

instance  Ix Char#  where
    range (c,c')	=  [c..c']
    index b@(c,c') ci
	| inRange b ci	=  I# (ord# ci - ord# c)
	| otherwise	=  error "Ix.Char#.index{PreludeCore}: Index out of range\n"
    inRange (c,c') ci	=  ord# c <= i && i <= ord# c'
			   where i = ord# ci

instance  Enum Char#  where
    enumFrom c		 =  map chr# [ord# c .. ord# '\255'#]
    enumFromThen c c'	 =  map chr# [ord# c, ord# c' .. ord# lastChar#]
			    where lastChar# = if c' < c then '\0'# else '\255'#
    enumFromTo n m	 =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

-- ToDo: efficient Text Char# instance
instance  Text Char#  where
    readsPrec p s = map (\ (C# c#, s) -> (c#, s)) (readsPrec p s)
    showsPrec p c = showsPrec p (C# c)
    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0)

instance _CCallable   Char#
instance _CReturnable Char#

#endif {-UNBOXED INSTANCES-}
