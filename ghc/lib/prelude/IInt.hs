module PreludeCore ( Int(..), rangeComplaint_Ix_Int#{-see comment later-} ) where

import Cls
import Core
import IInteger		-- instances
import IRatio		( (%) )
import ITup2
import List		( (++), foldr )
import Prel		( otherwise, (&&), (||), chr, ord )
import PS		( _PackedString, _unpackPS )
import Text

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusInt	(I# x) (I# y) = I# (plusInt# x y)
minusInt(I# x) (I# y) = I# (minusInt# x y)
timesInt(I# x) (I# y) = I# (timesInt# x y)
quotInt	(I# x) (I# y) = I# (quotInt# x y)
remInt	(I# x) (I# y) = I# (remInt# x y)
negateInt (I# x)      = I# (negateInt# x)
gtInt	(I# x) (I# y) = gtInt# x y
geInt	(I# x) (I# y) = geInt# x y
eqInt	(I# x) (I# y) = eqInt# x y
neInt	(I# x) (I# y) = neInt# x y
ltInt	(I# x) (I# y) = ltInt# x y
leInt	(I# x) (I# y) = leInt# x y

---------------------------------------------------------------

instance  Eq Int  where
    (==) x y = eqInt x y
    (/=) x y = neInt x y

instance  Ord Int  where
    (<=) x y = leInt x y
    (<)	 x y = ltInt x y
    (>=) x y = geInt x y
    (>)	 x y = gtInt x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp (I# a#) (I# b#)
      = if      (a# ==# b#) then _EQ
	else if (a#  <# b#) then _LT else _GT

instance  Num Int  where
    (+)	   x y =  plusInt x y
    (-)	   x y =  minusInt x y
    negate x   =  negateInt x
    (*)	   x y =  timesInt x y
    abs    n   = if n `geInt` 0 then n else (negateInt n)

    signum n | n `ltInt` 0 = negateInt 1
	     | n `eqInt` 0 = 0
	     | otherwise   = 1

    fromInteger (J# a# s# d#)
      = case (integer2Int# a# s# d#) of { i# -> I# i# }

    fromInt n		= n

instance  Real Int  where
    toRational x	=  toInteger x % 1

instance  Integral Int	where
    a@(I# _) `quotRem` b@(I# _)	= (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

    -- following chks for zero divisor are non-standard (WDP)
    a `quot` b		=  if b /= 0
			   then a `quotInt` b
			   else error "Integral.Int.quot{PreludeCore}: divide by 0\n"
    a `rem` b		=  if b /= 0
			   then a `remInt` b
			   else error "Integral.Int.rem{PreludeCore}: divide by 0\n"

    x `div` y = if x > 0 && y < 0	then quotInt (x-y-1) y
		else if x < 0 && y > 0	then quotInt (x-y+1) y
		else quotInt x y
    x `mod` y = if x > 0 && y < 0 || x < 0 && y > 0 then
		    if r/=0 then r+y else 0
	    	else
		    r
	      where r = remInt x y

    divMod x@(I# _) y@(I# _) = (x `div` y, x `mod` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)

    even x = eqInt (x `mod` 2) 0
    odd x  = neInt (x `mod` 2) 0

    toInteger (I# n#) = int2Integer# n#  -- give back a full-blown Integer
    toInt x	      = x

rangeComplaint_Ix_Int# i m n -- export it so it will *not* be floated inwards
  = error ("Ix.Int.index2{PreludeCore}: Index "
	   ++ show (I# i) ++ " outside the range " 
	   ++ show (I# m,I# n) ++ ".\n")

instance  Ix Int  where
    range (m,n)		=  [m..n]
    index b@(I# m, I# n) (I# i)
	| inRange b (I# i)  =  I# (i -# m)
	| otherwise	    =  rangeComplaint_Ix_Int# i m n
    inRange (I# m, I# n) (I# i) =  m <=# i && i <=# n

instance  Enum Int  where
{- RAW PRELUDE ************************
    enumFrom		=  numericEnumFrom
    enumFromThen	=  numericEnumFromThen
-}
#ifndef USE_FOLDR_BUILD
    enumFrom x = x : enumFrom (x `plusInt` 1)
#else
    {-# INLINE enumFromTo #-}
    {-# INLINE enumFrom #-}
    enumFromTo x y	 = _build (\ c n ->
	let g x = if x <= y then x `c` g (x `plusInt` 1) else n in g x)
    enumFrom x           = _build (\ c _ -> 
	let g x = x `c` g (x `plusInt` 1) in g x)
#endif
    enumFromThen m n = en' m (n `minusInt` m)
	    where en' m n = m : en' (m `plusInt` n) n

instance  Text Int  where
    readsPrec p x = readSigned readDec x
    showsPrec x   = showSigned showInt x

---------------------------------------------------------------
instance _CCallable   Int
instance _CReturnable Int

#if defined(__UNBOXED_INSTANCES__)
---------------------------------------------------------------
-- Instances for Int#
---------------------------------------------------------------

instance  Eq Int#  where
    (==) x y = eqInt# x y
    (/=) x y = neInt# x y

instance  Ord Int#  where
    (<=) x y = leInt# x y
    (<)	 x y = ltInt# x y
    (>=) x y = geInt# x y
    (>)	 x y = gtInt# x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp a b
      = if      (a `eqInt#` b) then _EQ
	else if (a `ltInt#` b) then _LT else _GT

instance  Num Int#  where
    (+)	   x y = plusInt# x y
    (-)	   x y = minusInt# x y
    negate x   = negateInt# x
    (*)	   x y = timesInt# x y
    abs    n   = if n `geInt#` 0 then n else (negateInt# n)

    signum n | n `ltInt#` 0 = negateInt# 1
	     | n `eqInt#` 0 = 0
	     | otherwise    = 1

    fromInteger (J# a# s# d#)
      = integer2Int# a# s# d#

    fromInt (I# i#) = i#

instance  Real Int#  where
    toRational x	=  toInteger x % 1

instance  Integral Int#  where
    a `quotRem` b	=  (a `quotInt#` b, a `remInt#` b)

    -- following chks for zero divisor are non-standard (WDP)
    a `quot` b		=  if b /= 0
			   then a `quotInt#` b
			   else error "Integral.Int#.quot{PreludeCore}: divide by 0\n"
    a `rem` b		=  if b /= 0
			   then a `remInt#` b
			   else error "Integral.Int#.rem{PreludeCore}: divide by 0\n"

    x `div` y = if x > 0 && y < 0	then quotInt# (x-y-1) y
		else if x < 0 && y > 0	then quotInt# (x-y+1) y
		else quotInt# x y
    x `mod` y = if x > 0 && y < 0 || x < 0 && y > 0 then
		    if r/=0 then r+y else 0
	    	else
		    r
	      where r = remInt# x y

    divMod x y = (x `div` y, x `mod` y)

    even x = eqInt# (x `mod` 2) 0
    odd x  = neInt# (x `mod` 2) 0

    toInteger n# = int2Integer# n#  -- give back a full-blown Integer
    toInt n#	 = I# n#

instance  Ix Int#  where
    range (m,n)		=  [m..n]
    index b@(m, n) i
	| inRange b i   =  I# (i -# m)
	| otherwise	=  rangeComplaint_Ix_Int# i m n
    inRange (m, n) i    =  m <=# i && i <=# n

instance  Enum Int#  where
    enumFrom x           =  x : enumFrom (x `plusInt#` 1)
    enumFromThen m n     =  en' m (n `minusInt#` m)
	                    where en' m n = m : en' (m `plusInt#` n) n
    -- default methods not specialised!
    enumFromTo n m	 =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

-- ToDo: efficient Text Int# instance
instance  Text Int#  where
    readsPrec p s = map (\ (I# i#, s) -> (i#, s)) (readsPrec p s)
    showsPrec p x = showsPrec p (I# x)
    readList s = map (\ (x, s) -> (map (\ (I# i#) -> i#) x, s)) (readList s)
    showList l = showList (map I# l)

instance _CCallable   Int#
instance _CReturnable Int#

#endif {-UNBOXED INSTANCES-}

---------------------------------------------------------------
-- Instances for Addr Word etc #
---------------------------------------------------------------

instance _CCallable _Addr
instance _CCallable _Word
instance _CCallable _MallocPtr

instance _CReturnable _Addr
instance _CReturnable _Word
instance _CReturnable ()
instance _CReturnable _MallocPtr

#ifndef __PARALLEL_HASKELL__
instance _CCallable (_StablePtr a)
instance _CReturnable (_StablePtr a)
#endif

---------------------------------------------------------------
gtAddr	(A# x) (A# y) = gtAddr# x y
geAddr	(A# x) (A# y) = geAddr# x y
eqAddr	(A# x) (A# y) = eqAddr# x y
neAddr	(A# x) (A# y) = neAddr# x y
ltAddr	(A# x) (A# y) = ltAddr# x y
leAddr	(A# x) (A# y) = leAddr# x y

instance  Eq _Addr  where
    (==) x y = eqAddr x y
    (/=) x y = neAddr x y

instance  Ord _Addr  where
    (<=) x y = leAddr x y
    (<)	 x y = ltAddr x y
    (>=) x y = geAddr x y
    (>)	 x y = gtAddr x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp (A# a#) (A# b#)
      = if      (eqAddr# a# b#) then _EQ
	else if (ltAddr# a# b#) then _LT else _GT

---------------------------------------------------------------
gtWord	(W# x) (W# y) = gtWord# x y
geWord	(W# x) (W# y) = geWord# x y
eqWord	(W# x) (W# y) = eqWord# x y
neWord	(W# x) (W# y) = neWord# x y
ltWord	(W# x) (W# y) = ltWord# x y
leWord	(W# x) (W# y) = leWord# x y

instance  Eq _Word  where
    (==) x y = eqWord x y
    (/=) x y = neWord x y

instance  Ord _Word  where
    (<=) x y = leWord x y
    (<)	 x y = ltWord x y
    (>=) x y = geWord x y
    (>)	 x y = gtWord x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp (W# a#) (W# b#)
      = if      (eqWord# a# b#) then _EQ
	else if (ltWord# a# b#) then _LT else _GT
