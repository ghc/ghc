%
% (c) The GRASP Project, Glasgow University, 1994-1995
%
\section[BitSet]{An implementation of very small sets}

Bit sets are a fast implementation of sets of integers ranging from 0
to one less than the number of bits in a machine word (typically 31).
If any element exceeds the maximum value for a particular machine
architecture, the results of these operations are undefined.  You have
been warned.  If you put any safety checks in this code, I will have
to kill you.

Note: the Yale Haskell implementation won't provide a full 32 bits.
However, if you can handle the performance loss, you could change to
Integer and get virtually unlimited sets.

\begin{code}

module BitSet (
	BitSet,		-- abstract type
	mkBS, listBS, emptyBS, singletonBS,
	unionBS, minusBS
#if ! defined(COMPILING_GHC)
	, elementBS, intersectBS, isEmptyBS
#endif
    ) where

#ifdef __GLASGOW_HASKELL__
-- nothing to import
#elif defined(__YALE_HASKELL__)
{-hide import from mkdependHS-}
import
        LogOpPrims
#else
{-hide import from mkdependHS-}
import
	Word
#endif

#ifdef __GLASGOW_HASKELL__

data BitSet = MkBS Word#

emptyBS :: BitSet 
emptyBS = MkBS (int2Word# 0#)

mkBS :: [Int] -> BitSet
mkBS xs = foldr (unionBS . singletonBS) emptyBS xs

singletonBS :: Int -> BitSet
singletonBS x = case x of
    I# i# -> MkBS ((int2Word# 1#) `shiftL#` i#)

unionBS :: BitSet -> BitSet -> BitSet
unionBS (MkBS x#) (MkBS y#) = MkBS (x# `or#` y#)

minusBS :: BitSet -> BitSet -> BitSet
minusBS (MkBS x#) (MkBS y#) = MkBS (x# `and#` (not# y#))

#if ! defined(COMPILING_GHC)
-- not used in GHC
isEmptyBS :: BitSet -> Bool
isEmptyBS (MkBS s#) = 
    case word2Int# s# of
    	0# -> True
    	_  -> False

intersectBS :: BitSet -> BitSet -> BitSet
intersectBS (MkBS x#) (MkBS y#) = MkBS (x# `and#` y#)

elementBS :: Int -> BitSet -> Bool
elementBS x (MkBS s#) = case x of
    I# i# -> case word2Int# (((int2Word# 1#) `shiftL#` i#) `and#` s#) of
    	    	    0# -> False
    	    	    _  -> True
#endif

listBS :: BitSet -> [Int]
listBS s = listify s 0
    where listify (MkBS s#) n = 
    	    case word2Int# s# of
    	        0# -> []
    	        _  -> let s' = (MkBS (s# `shiftr` 1#))
    	    	    	  more = listify s' (n + 1)
    	    	      in case word2Int# (s# `and#` (int2Word# 1#)) of
    	    	    	  0# -> more
    	    	    	  _  -> n : more
# if __GLASGOW_HASKELL__ >= 23
	  shiftr x y = shiftRL# x y
# else
	  shiftr x y = shiftR#  x y
# endif

#elif defined(__YALE_HASKELL__)

data BitSet = MkBS Int

emptyBS :: BitSet 
emptyBS = MkBS 0

mkBS :: [Int] -> BitSet
mkBS xs = foldr (unionBS . singletonBS) emptyBS xs

singletonBS :: Int -> BitSet
singletonBS x = MkBS (1 `ashInt` x)

unionBS :: BitSet -> BitSet -> BitSet
unionBS (MkBS x) (MkBS y) = MkBS (x `logiorInt` y)

#if ! defined(COMPILING_GHC)
-- not used in GHC
isEmptyBS :: BitSet -> Bool
isEmptyBS (MkBS s) = 
    case s of
    	0 -> True
    	_ -> False

intersectBS :: BitSet -> BitSet -> BitSet
intersectBS (MkBS x) (MkBS y) = MkBS (x `logandInt` y)

elementBS :: Int -> BitSet -> Bool
elementBS x (MkBS s) = 
    case logbitpInt x s of
    	0 -> False
    	_ -> True
#endif

minusBS :: BitSet -> BitSet -> BitSet
minusBS (MkBS x) (MkBS y) = MkBS (x `logandc2Int` y)

-- rewritten to avoid right shifts (which would give nonsense on negative 
-- values.
listBS :: BitSet -> [Int]
listBS (MkBS s) = listify s 0 1
    where listify s n m = 
    	    case s of
    	        0 -> []
    	        _ -> let n' = n+1; m' = m+m in
                     case logbitpInt s m of
		     0 -> listify s n' m'
		     _ -> n : listify (s `logandc2Int` m) n' m'

#else	/* HBC, perhaps? */    

data BitSet = MkBS Word

emptyBS :: BitSet 
emptyBS = MkBS 0

mkBS :: [Int] -> BitSet
mkBS xs = foldr (unionBS . singletonBS) emptyBS xs

singletonBS :: Int -> BitSet
singletonBS x = MkBS (1 `bitLsh` x)

unionBS :: BitSet -> BitSet -> BitSet
unionBS (MkBS x) (MkBS y) = MkBS (x `bitOr` y)

#if ! defined(COMPILING_GHC)
-- not used in GHC
isEmptyBS :: BitSet -> Bool
isEmptyBS (MkBS s) = 
    case s of
    	0 -> True
    	_ -> False

intersectBS :: BitSet -> BitSet -> BitSet
intersectBS (MkBS x) (MkBS y) = MkBS (x `bitAnd` y)

elementBS :: Int -> BitSet -> Bool
elementBS x (MkBS s) = 
    case (1 `bitLsh` x) `bitAnd` s of
    	0 -> False
    	_ -> True
#endif

minusBS :: BitSet -> BitSet -> BitSet
minusBS (MkBS x) (MkBS y) = MkBS (x `bitAnd` (bitCompl y))

listBS :: BitSet -> [Int]
listBS (MkBS s) = listify s 0
    where listify s n = 
    	    case s of
    	        0 -> []
    	        _ -> let s' = s `bitRsh` 1
    	    	         more = listify s' (n + 1)
    	    	     in case (s `bitAnd` 1) of
    	    	    	    0 -> more
    	    	    	    _ -> n : more

#endif

\end{code}




