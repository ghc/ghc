module Prelude (

#include "../includes/ieee-flpt.h"

--partain:    module PreludeList,
   head, last, tail, init, null, length, (!!),
   foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
   iterate, repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break,
   lines, words, unlines, unwords, reverse, and, or,
   any, all, elem, notElem, lookup,
   sum, product, maximum, minimum, concatMap, 
   zip, zip3, zipWith, zipWith3, unzip, unzip3,

--partain:module PreludeText,
        ReadS, ShowS,
        Read(readsPrec, readList),
        Show(showsPrec, showList),
        reads, shows, show, read, lex,
        showChar, showString, readParen, showParen,
--partain:module PreludeIO,
      FilePath, IOError, fail, userError, catch,
      putChar, putStr, putStrLn, print,
      getChar, getLine, getContents, interact,
      readFile, writeFile, appendFile, readIO, readLn,

    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right), either,
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, IO, Void,
    [](..), --  List type
    ()(..), --  Trivial type
    --  Tuple types: (,), (,,), etc.
    (,)(..),
    (,,)(..),
    (,,,)(..),
    (,,,,)(..),
    (,,,,,)(..),
    (,,,,,,)(..),
    (,,,,,,,)(..),
    (,,,,,,,,)(..),
    (,,,,,,,,,)(..),
    (,,,,,,,,,,)(..),
    (,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
    (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)(..),
--  Functions: (->)
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
    Eval(..{-seq, strict-}), seq, strict, -- NB: glasgow hack
    Num((+), (-), (*), negate, abs, signum, fromInteger, fromInt{-partain-}),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero),
    Monad((>>=), (>>), return),
    MonadZero(zero),
    MonadPlus((++)),
    Functor(map),
    succ, pred,
    mapM, mapM_, guard, accumulate, sequence, filter, concat, applyM,
    maybe,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, fromRealFrac, atan2,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined ) where

import GHCbase	-- all the GHC basics
import GHCio	-- I/O basics
import Ratio(Ratio, Rational, (%), numerator, denominator)

--PreludeText:
import Char	( isSpace )
import IO	( hPutChar, hPutStr, hGetChar, hGetContents )

infixl 9  !!
infix  4 `elem`, `notElem`
{- :PreludeList -}

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixr 1  >>, >>=
infixr 0  $

-- Standard types, classes, instances and related functions

-- Equality and Ordered classes

class  Eq a  where
    (==), (/=)		:: a -> a -> Bool

    x /= y		=  not (x == y)

class  (Eq a) => Ord a  where
    compare             :: a -> a -> Ordering
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min		:: a -> a -> a

-- An instance of Ord should define either compare or <=
-- Using compare can be more efficient for complex types.
    compare x y
	    | x == y    = EQ
	    | x <= y    = LT
	    | otherwise = GT

    x <= y  = compare x y /= GT
    x <	 y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >	 y  = compare x y == GT
    max x y = case (compare x y) of { LT -> y ; EQ -> x ; GT -> x }
    min x y = case (compare x y) of { LT -> x ; EQ -> x ; GT -> y }

-- Enumeration and Bounded classes

class  (Ord a) => Enum a	where
    toEnum              :: Int -> a
    fromEnum            :: a -> Int
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

    enumFromTo n m      =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n n' m
                        =  takeWhile (if n' >= n then (<= m) else (>= m))
                                     (enumFromThen n n')

succ, pred              :: Enum a => a -> a
succ                    =  toEnum . (+1) . fromEnum
pred                    =  toEnum . (subtract 1) . fromEnum

class  Bounded a  where
    minBound, maxBound :: a

-- Numeric classes

class  (Eq a, Show a, Eval a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a -- partain: Glasgow extension

    x - y		=  x + negate y
    fromInt i		= fromInteger (int2Integer i)
			where
			  int2Integer (I# i#) = int2Integer# i#
					-- Go via the standard class-op if the
					-- non-standard one ain't provided

class  (Num a, Ord a) => Real a  where
    toRational		::  a -> Rational

class  (Real a, Enum a) => Integral a  where
    quot, rem, div, mod	:: a -> a -> a
    quotRem, divMod	:: a -> a -> (a,a)
    toInteger		:: a -> Integer

    n `quot` d		=  q  where (q,r) = quotRem n d
    n `rem` d		=  r  where (q,r) = quotRem n d
    n `div` d		=  q  where (q,r) = divMod n d
    n `mod` d		=  r  where (q,r) = divMod n d
    divMod n d 		=  if signum r == - signum d then (q-1, r+d) else qr
			   where qr@(q,r) = quotRem n d

class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    recip		:: a -> a
    fromRational	:: Rational -> a

    recip x		=  1 / x

class  (Fractional a) => Floating a  where
    pi			:: a
    exp, log, sqrt	:: a -> a
    (**), logBase	:: a -> a -> a
    sin, cos, tan	:: a -> a
    asin, acos, atan	:: a -> a
    sinh, cosh, tanh	:: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y		=  exp (log x * y)
    logBase x y		=  log y / log x
    sqrt x		=  x ** 0.5
    tan  x		=  sin  x / cos  x
    tanh x		=  sinh x / cosh x

class  (Real a, Fractional a) => RealFrac a  where
    properFraction	:: (Integral b) => a -> (b,a)
    truncate, round	:: (Integral b) => a -> b
    ceiling, floor	:: (Integral b) => a -> b

    truncate x		=  m  where (m,_) = properFraction x
    
    round x		=  let (n,r) = properFraction x
    			       m     = if r < 0 then n - 1 else n + 1
    			   in case signum (abs r - 0.5) of
    				-1 -> n
    			 	0  -> if even n then n else m
    				1  -> m
    
    ceiling x		=  if r > 0 then n + 1 else n
    			   where (n,r) = properFraction x
    
    floor x		=  if r < 0 then n - 1 else n
    			   where (n,r) = properFraction x

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix		:: a -> Integer
    floatDigits		:: a -> Int
    floatRange		:: a -> (Int,Int)
    decodeFloat		:: a -> (Integer,Int)
    encodeFloat		:: Integer -> Int -> a
    exponent		:: a -> Int
    significand		:: a -> a
    scaleFloat		:: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                        :: a -> Bool

    exponent x		=  if m == 0 then 0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (- floatDigits x)
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x

-- Numeric functions

{-# GENERATE_SPECS subtract a{Int#,Double#,Int,Double,Complex(Double#),Complex(Double)} #-}
subtract	:: (Num a) => a -> a -> a
subtract x y	=  y - x

even, odd	:: (Integral a) => a -> Bool
even n		=  n `rem` 2 == 0
odd		=  not . even

{-# GENERATE_SPECS gcd a{Int#,Int,Integer} #-}
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' x 0  =  x
			 gcd' x y  =  gcd' y (x `rem` y)

{-# GENERATE_SPECS lcm a{Int#,Int,Integer} #-}
lcm		:: (Integral a) => a -> a -> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)

(^)		:: (Num a, Integral b) => a -> b -> a
x ^ 0		=  1
x ^ n | n > 0	=  f x (n-1) x
		   where f _ 0 y = y
		         f x n y = g x n  where
			           g x n | even n  = g (x*x) (n `quot` 2)
				         | otherwise = f x (n-1) (x*y)
_ ^ _		= error "Prelude.^: negative exponent"

(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(-n))

fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational

atan2		:: (RealFloat a) => a -> a -> a
atan2 y x	=  case (signum y, signum x) of
			( 0, 1) ->  0
			( 1, 0) ->  pi/2
			( 0,-1) ->  pi
			(-1, 0) -> -pi/2
			( _, 1) ->  atan (y/x)
			( _,-1) ->  atan (y/x) + pi
			( 0, 0) ->  error "Prelude.atan2: atan2 of origin"


-- Monadic classes

class  Functor f  where
    map         :: (a -> b) -> f a -> f b

class  Monad m  where
    (>>=)       :: m a -> (a -> m b) -> m b
    (>>)        :: m a -> m b -> m b
    return      :: a -> m a

    m >> k      =  m >>= \_ -> k

class  (Monad m) => MonadZero m  where
    zero        :: m a

class  (MonadZero m) => MonadPlus m where
   (++)         :: m a -> m a -> m a

accumulate      :: Monad m => [m a] -> m [a] 
accumulate []     = return []
accumulate (m:ms) = do { x <- m; xs <- accumulate ms; return (x:xs) }
{- partain: this may be right, but I'm going w/ a more-certainly-right version
accumulate      =  foldr mcons (return [])
                   where mcons p q = p >>= \x -> q >>= \y -> return (x:y)
-}
sequence        :: Monad m => [m a] -> m () 
sequence        =  foldr (>>) (return ())

mapM            :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as       =  accumulate (map f as)

mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as      =  sequence (map f as)

guard           :: MonadZero m => Bool -> m ()
guard p         =  if p then return () else zero

-- This subsumes the list-based filter function.

filter          :: MonadZero m => (a -> Bool) -> m a -> m a
filter p        =  applyM (\x -> if p x then return x else zero)

-- This subsumes the list-based concat function.

concat          :: MonadPlus m => [m a] -> m a
concat          =  foldr (++) zero
 
applyM          :: Monad m => (a -> m b) -> m a -> m b
applyM f x      =  x >>= f


-- Eval Class

class Eval a {-not Glasgow: where
   seq         :: a -> b -> b
   strict      :: (a -> b) -> a -> b
   strict f x  =  x `seq` f x -}

-- seq: in GHCbase
strict      :: Eval a => (a -> b) -> a -> b
strict f x  = x `seq` f x

---------------------------------------------------------------
-- Trivial type

data  ()  =  ()  --easier to do explicitly: deriving (Eq, Ord, Enum, Bounded)
		 -- (avoids weird-named functions, e.g., con2tag_()#

instance CReturnable () -- Why, exactly?

instance Eq () where
    () == () = True
    () /= () = False

instance Ord () where
    () <= () = True
    () <  () = False
    () >= () = True
    () >  () = False
    max () () = ()
    min () () = ()
    compare () () = EQ

instance Enum () where
    toEnum 0    = ()
    toEnum _	= error "Prelude.Enum.().toEnum: argument not 0"
    fromEnum () = 0
    enumFrom () 	= [()]
    enumFromThen () () 	= [()]
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = [()]

instance Bounded () where
    minBound = ()
    maxBound = ()

instance  Show ()  where
    showsPrec p () = showString "()"

instance Read () where
    readsPrec p    = readParen False
                            (\r -> [((),t) | ("(",s) <- lex r,
                                             (")",t) <- lex s ] )

---------------------------------------------------------------
-- Function type

--data a -> b  -- No constructor for functions is exported.

instance  Show (a -> b)  where
    showsPrec p f  =  showString "<<function>>"
    showList	   = showList__ (showsPrec 0)

---------------------------------------------------------------
-- Empty type

--partain:data Void      -- No constructor for Void is exported.  Import/Export
               -- lists must use Void instead of Void(..) or Void()

---------------------------------------------------------------
-- Boolean type

data  Bool  =  False | True	deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- Boolean functions

(&&), (||)		:: Bool -> Bool -> Bool
True  && x		=  x
False && _		=  False
True  || _		=  True
False || x		=  x

not			:: Bool -> Bool
not True		=  False
not False		=  True

otherwise		:: Bool
otherwise 		=  True

---------------------------------------------------------------
-- Character type

data Char = C# Char# deriving (Eq, Ord)
--partain:data Char = ... 'a' | 'b' ... -- 265 ISO values
instance CCallable Char
instance CReturnable Char

instance  Enum Char  where
    toEnum   (I# i) | i >=# 0# && i <=# 255# =  C# (chr# i)
		    | otherwise = error "Prelude.Enum.Char.toEnum:out of range"
    fromEnum (C# c)     =  I# (ord# c)
    enumFrom c		=  map toEnum [fromEnum c .. fromEnum (maxBound::Char)]
    enumFromThen c c'	=  map toEnum [fromEnum c,
                                       fromEnum c' .. fromEnum lastChar]
			   where lastChar :: Char
                                 lastChar | c' < c    = minBound
                                          | otherwise = maxBound

instance  Bounded Char  where
    minBound            =  '\0'
    maxBound            =  '\255'

instance  Read Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> [(c,t) | ('\'':s,t)<- lex r,
					    (c,_)     <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s ])
	       where readl ('"':s)	= [("",s)]
		     readl ('\\':'&':s)	= readl s
		     readl s		= [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t	      ]
instance  Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':cs) = showString "\\\"" . showl cs
		       showl (c:cs)   = showLitChar c . showl cs

type  String = [Char]

---------------------------------------------------------------
-- Maybe type

data  Maybe a  =  Nothing | Just a	deriving (Eq, Ord, Read, Show)

maybe                   :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing       =  n
maybe n f (Just x)      =  f x

instance  Functor Maybe  where
    map f Nothing       = Nothing
    map f (Just a)      = Just (f a)

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= k      = Nothing
    return              = Just

instance  MonadZero Maybe  where
    zero                = Nothing

instance  MonadPlus Maybe  where
    Nothing ++ ys       = ys
    xs      ++ ys       = xs

---------------------------------------------------------------
-- Either type

data  Either a b  =  Left a | Right b	deriving (Eq, Ord, Read, Show)

either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)     =  f x
either f g (Right y)    =  g y

---------------------------------------------------------------
-- IO type: moved to GHCbase

--partain: data IO a =  -- abstract

---------------------------------------------------------------
-- Ordering type

data Ordering = LT | EQ | GT  deriving (Eq, Ord, Enum, Read, Show, Bounded)

---------------------------------------------------------------
-- Standard numeric types.  The data declarations for these types
-- cannot be expressed directly in (standard) Haskell since the
-- constructor lists would be far too large.

---------------------------------------------------------------
data Int = I# Int# deriving (Eq,Ord)
--partain:data Int  =  minBound ... -1 | 0 | 1 ... maxBound

instance CCallable   Int
instance CReturnable Int

instance  Bounded Int where
    minBound =  -2147483647	-- **********************
    maxBound =  2147483647	-- **********************

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

--OLD:   even x = eqInt (x `mod` 2) 0
--OLD:   odd x  = neInt (x `mod` 2) 0

    toInteger (I# n#) = int2Integer# n#  -- give back a full-blown Integer
--  toInt x	      = x

instance  Enum Int  where
    toEnum   x = x
    fromEnum x = x
#ifndef USE_FOLDR_BUILD
    enumFrom x           =  x : enumFrom (x `plusInt` 1)
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
#else
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}
    enumFrom x           = _build (\ c _ -> 
	let g x = x `c` g (x `plusInt` 1) in g x)
    enumFromTo x y	 = _build (\ c n ->
	let g x = if x <= y then x `c` g (x `plusInt` 1) else n in g x)
#endif
    enumFromThen m n     =  en' m (n `minusInt` m)
	                    where en' m n = m : en' (m `plusInt` n) n
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Read Int  where
    readsPrec p x = readSigned readDec x
    readList = readList__ (readsPrec 0)

instance  Show Int  where
    showsPrec x   = showSigned showInt x
    showList = showList__ (showsPrec 0) 

---------------------------------------------------------------
data Integer = J# Int# Int# ByteArray#
--partain:data Integer = ... -1 | 0 | 1 ...

instance  Eq Integer  where
    (J# a1 s1 d1) == (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) ==# 0#

    (J# a1 s1 d1) /= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) /=# 0#

instance  Ord Integer  where
    (J# a1 s1 d1) <= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) <=# 0#

    (J# a1 s1 d1) <  (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) <# 0#

    (J# a1 s1 d1) >= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#

    (J# a1 s1 d1) >  (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) ># 0#

    x@(J# a1 s1 d1) `max` y@(J# a2 s2 d2)
      = if ((cmpInteger# a1 s1 d1 a2 s2 d2) ># 0#) then x else y

    x@(J# a1 s1 d1) `min` y@(J# a2 s2 d2)
      = if ((cmpInteger# a1 s1 d1 a2 s2 d2) <# 0#) then x else y

    compare (J# a1 s1 d1) (J# a2 s2 d2)
       = case cmpInteger# a1 s1 d1 a2 s2 d2 of { res# ->
	 if res# <# 0# then LT else 
	 if res# ># 0# then GT else EQ
	 }

instance  Num Integer  where
    (+) (J# a1 s1 d1) (J# a2 s2 d2)
      = plusInteger# a1 s1 d1 a2 s2 d2

    (-) (J# a1 s1 d1) (J# a2 s2 d2)
      = minusInteger# a1 s1 d1 a2 s2 d2

    negate (J# a s d) = negateInteger# a s d

    (*) (J# a1 s1 d1) (J# a2 s2 d2)
      = timesInteger# a1 s1 d1 a2 s2 d2

    -- ORIG: abs n = if n >= 0 then n else -n

    abs n@(J# a1 s1 d1)
      = case 0 of { J# a2 s2 d2 ->
	if (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#
	then n
	else negateInteger# a1 s1 d1
	}

    signum n@(J# a1 s1 d1)
      = case 0 of { J# a2 s2 d2 ->
	let
	    cmp = cmpInteger# a1 s1 d1 a2 s2 d2
	in
	if      cmp >#  0# then 1
	else if cmp ==# 0# then 0
	else			-1
	}

    fromInteger	x	=  x

    fromInt (I# n#)	=  int2Integer# n# -- gives back a full-blown Integer

instance  Real Integer  where
    toRational x	=  x % 1

instance  Integral Integer where
    quotRem (J# a1 s1 d1) (J# a2 s2 d2)
      = case (quotRemInteger# a1 s1 d1 a2 s2 d2) of
	  Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)

{- USING THE UNDERLYING "GMP" CODE IS DUBIOUS FOR NOW:

    divMod (J# a1 s1 d1) (J# a2 s2 d2)
      = case (divModInteger# a1 s1 d1 a2 s2 d2) of
	  Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)
-}
    toInteger n	     = n
--  toInt (J# a s d) = case (integer2Int# a s d) of { n# -> I# n# }

    -- the rest are identical to the report default methods;
    -- you get slightly better code if you let the compiler
    -- see them right here:
    n `quot` d	=  q  where (q,r) = quotRem n d
    n `rem` d	=  r  where (q,r) = quotRem n d
    n `div` d	=  q  where (q,r) = divMod n d
    n `mod` d	=  r  where (q,r) = divMod n d

    divMod n d 	=  case (quotRem n d) of { qr@(q,r) ->
		   if signum r == - signum d then (q - 1, r+d) else qr }
		   -- Case-ified by WDP 94/10

instance  Enum Integer  where
    enumFrom n           =  n : enumFrom (n + 1)
    enumFromThen m n     =  en' m (n - m)
	                    where en' m n = m : en' (m + n) n
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Read Integer  where
    readsPrec p x = readSigned readDec x
    readList = readList__ (readsPrec 0)

instance  Show Integer  where
    showsPrec   x = showSigned showInt x
    showList = showList__ (showsPrec 0) 

---------------------------------------------------------------
data Float  = F# Float# deriving (Eq, Ord)
instance CCallable   Float
instance CReturnable Float

---------------------------------------------------------------

instance  Num Float  where
    (+)		x y 	=  plusFloat x y
    (-)		x y 	=  minusFloat x y
    negate	x  	=  negateFloat x
    (*)		x y 	=  timesFloat x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateFloat x
    signum x | x == 0.0	 =  0
	     | x > 0.0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt i		=  int2Float i

instance  Real Float  where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Float  where
    (/) x y		=  divideFloat x y
    fromRational x	=  fromRational__ x
    recip x		=  1.0 / x

instance  Floating Float  where
    pi			=  3.141592653589793238
    exp x		=  expFloat x
    log	x	 	=  logFloat x
    sqrt x		=  sqrtFloat x
    sin	x		=  sinFloat x
    cos	x		=  cosFloat x
    tan	x		=  tanFloat x
    asin x		=  asinFloat x
    acos x		=  acosFloat x
    atan x		=  atanFloat x
    sinh x		=  sinhFloat x
    cosh x	 	=  coshFloat x
    tanh x		=  tanhFloat x
    (**) x y		=  powerFloat x y
    logBase x y		=  log y / log x

    asinh x = log (x + sqrt (1.0+x*x))
    acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
    atanh x = log ((x+1.0) / sqrt (1.0-x*x))

instance  RealFrac Float  where

    {-# SPECIALIZE properFraction :: Float -> (Int, Float) #-}
    {-# SPECIALIZE truncate :: Float -> Int #-}
    {-# SPECIALIZE round    :: Float -> Int #-}
    {-# SPECIALIZE ceiling  :: Float -> Int #-}
    {-# SPECIALIZE floor    :: Float -> Int #-}

    {-# SPECIALIZE properFraction :: Float -> (Integer, Float) #-}
    {-# SPECIALIZE truncate :: Float -> Integer #-}
    {-# SPECIALIZE round    :: Float -> Integer #-}
    {-# SPECIALIZE ceiling  :: Float -> Integer #-}
    {-# SPECIALIZE floor    :: Float -> Integer #-}

    properFraction x
      = case (decodeFloat x)      of { (m,n) ->
    	let  b = floatRadix x     in
    	if n >= 0 then
	    (fromInteger m * fromInteger b ^ n, 0.0)
    	else
	    case (quotRem m (b^(-n))) of { (w,r) ->
	    (fromInteger w, encodeFloat r n)
	    }
        }

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < 0.0 then n - 1 else n + 1
		  	      	half_down = abs r - 0.5
    		   	      in
    		   	      case (compare half_down 0.0) of
      		     		LT -> n
      		     		EQ -> if even n then n else m
      		     		GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > 0.0 then n + 1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < 0.0 then n - 1 else n

instance  RealFloat Float  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  FLT_MANT_DIG	    -- ditto
    floatRange _	=  (FLT_MIN_EXP, FLT_MAX_EXP) -- ditto

    decodeFloat (F# f#)
      = case decodeFloat# f#	of
	  ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = case encodeFloat# a# s# d# e# of { flt# -> F# flt# }

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (- (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)

instance  Read Float  where
    readsPrec p x = readSigned readFloat x
    readList = readList__ (readsPrec 0)

instance  Show Float  where
    showsPrec   x = showSigned showFloat x
    showList = showList__ (showsPrec 0) 

---------------------------------------------------------------
data Double = D# Double# deriving (Eq, Ord)
instance CCallable   Double
instance CReturnable Double

---------------------------------------------------------------

instance  Num Double  where
    (+)		x y 	=  plusDouble x y
    (-)		x y 	=  minusDouble x y
    negate	x  	=  negateDouble x
    (*)		x y 	=  timesDouble x y
    abs x | x >= 0.0	=  x
	  | otherwise	=  negateDouble x
    signum x | x == 0.0	 =  0
	     | x > 0.0	 =  1
	     | otherwise = -1
    fromInteger n	=  encodeFloat n 0
    fromInt (I# n#)	=  case (int2Double# n#) of { d# -> D# d# }

instance  Real Double  where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Double  where
    (/) x y		=  divideDouble x y
    fromRational x	=  fromRational__ x
    recip x		=  1.0 / x

instance  Floating Double  where
    pi			=  3.141592653589793238
    exp	x		=  expDouble x
    log	x		=  logDouble x
    sqrt x		=  sqrtDouble x
    sin	 x		=  sinDouble x
    cos	 x		=  cosDouble x
    tan	 x		=  tanDouble x
    asin x		=  asinDouble x
    acos x	 	=  acosDouble x
    atan x		=  atanDouble x
    sinh x		=  sinhDouble x
    cosh x		=  coshDouble x
    tanh x		=  tanhDouble x
    (**) x y		=  powerDouble x y
    logBase x y		=  log y / log x

    asinh x = log (x + sqrt (1.0+x*x))
    acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
    atanh x = log ((x+1.0) / sqrt (1.0-x*x))

instance  RealFrac Double  where

    {-# SPECIALIZE properFraction :: Double -> (Int, Double) #-}
    {-# SPECIALIZE truncate :: Double -> Int #-}
    {-# SPECIALIZE round    :: Double -> Int #-}
    {-# SPECIALIZE ceiling  :: Double -> Int #-}
    {-# SPECIALIZE floor    :: Double -> Int #-}

    {-# SPECIALIZE properFraction :: Double -> (Integer, Double) #-}
    {-# SPECIALIZE truncate :: Double -> Integer #-}
    {-# SPECIALIZE round    :: Double -> Integer #-}
    {-# SPECIALIZE ceiling  :: Double -> Integer #-}
    {-# SPECIALIZE floor    :: Double -> Integer #-}

#if defined(__UNBOXED_INSTANCES__)
    {-# SPECIALIZE properFraction :: Double -> (Int#, Double) #-}
    {-# SPECIALIZE truncate :: Double -> Int# #-}
    {-# SPECIALIZE round    :: Double -> Int# #-}
    {-# SPECIALIZE ceiling  :: Double -> Int# #-}
    {-# SPECIALIZE floor    :: Double -> Int# #-}
#endif

    properFraction x
      = case (decodeFloat x)      of { (m,n) ->
    	let  b = floatRadix x     in
    	if n >= 0 then
	    (fromInteger m * fromInteger b ^ n, 0.0)
    	else
	    case (quotRem m (b^(-n))) of { (w,r) ->
	    (fromInteger w, encodeFloat r n)
	    }
        }

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < 0.0 then n - 1 else n + 1
		  	      	half_down = abs r - 0.5
    		   	      in
    		   	      case (compare half_down 0.0) of
      		     		LT -> n
      		     		EQ -> if even n then n else m
      		     		GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > 0.0 then n + 1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < 0.0 then n - 1 else n

instance  RealFloat Double  where
    floatRadix _	=  FLT_RADIX	    -- from float.h
    floatDigits _	=  DBL_MANT_DIG	    -- ditto
    floatRange _	=  (DBL_MIN_EXP, DBL_MAX_EXP) -- ditto

    decodeFloat (D# d#)
      = case decodeDouble# d#	of
	  ReturnIntAndGMP exp# a# s# d# ->
	    (J# a# s# d#, I# exp#)

    encodeFloat (J# a# s# d#) (I# e#)
      = case encodeDouble# a# s# d# e#	of { dbl# -> D# dbl# }

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (- (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)

instance  Read Double  where
    readsPrec p x = readSigned readFloat x
    readList = readList__ (readsPrec 0)

instance  Show Double  where
    showsPrec   x = showSigned showFloat x
    showList = showList__ (showsPrec 0) 

---------------------------------------------------------------
-- The Enum instances for Floats and Doubles are slightly unusual.
-- The `toEnum' function truncates numbers to Int.  The definitions
-- of enumFrom and enumFromThen allow floats to be used in arithmetic
-- series: [0,0.1 .. 1.0].  However, roundoff errors make these somewhat
-- dubious.  This example may have either 10 or 11 elements, depending on
-- how 0.1 is represented.

instance  Enum Float  where
    toEnum              =  fromIntegral
    fromEnum            =  fromInteger . truncate   -- may overflow
    enumFrom		=  numericEnumFrom
    enumFromThen	=  numericEnumFromThen

instance  Enum Double  where
    toEnum              =  fromIntegral
    fromEnum            =  fromInteger . truncate   -- may overflow
    enumFrom		=  numericEnumFrom
    enumFromThen	=  numericEnumFromThen

numericEnumFrom		:: (Real a) => a -> [a]
numericEnumFromThen	:: (Real a) => a -> a -> [a]
numericEnumFrom		=  iterate (+1)
numericEnumFromThen n m	=  iterate (+(m-n)) n

---------------------------------------------------------------
-- Lists

data [] a = [] | a : [a]  -- do explicitly: deriving (Eq, Ord)
			  -- to avoid weird names like con2tag_[]#

instance CCallable   [Char]
instance CReturnable [Char]

instance (Eq a) => Eq [a]  where
    []     == []     = True	
    (x:xs) == (y:ys) = x == y && xs == ys
    []     == ys     = False			
    xs     == []     = False			
    xs     /= ys     = if (xs == ys) then False else True

instance (Ord a) => Ord [a] where
    a <  b  = case compare a b of { LT -> True;  EQ -> False; GT -> False }
    a <= b  = case compare a b of { LT -> True;  EQ -> True;  GT -> False }
    a >= b  = case compare a b of { LT -> False; EQ -> True;  GT -> True  }
    a >  b  = case compare a b of { LT -> False; EQ -> False; GT -> True  }

    max a b = case compare a b of { LT -> b; EQ -> a;  GT -> a }
    min a b = case compare a b of { LT -> a; EQ -> a;  GT -> b }

    compare []     []     = EQ
    compare (x:xs) []     = GT
    compare []     (y:ys) = LT
    compare (x:xs) (y:ys) = case compare x y of
                                 LT -> LT	
			         GT -> GT		
				 EQ -> compare xs ys

instance Functor [] where
    map f []             =  []
    map f (x:xs)         =  f x : map f xs

instance  Monad []  where
    m >>= k             = concat (map k m)
    return x            = [x]

instance  MonadZero []  where
    zero                = []

instance  MonadPlus []  where
    xs ++ ys            =  foldr (:) ys xs
    
instance  (Show a) => Show [a]  where
    showsPrec p         = showList
    showList		= showList__ (showsPrec 0)

instance  (Read a) => Read [a]  where
    readsPrec p         = readList
    readList		= readList__ (readsPrec 0)

---------------------------------------------------------------
-- Tuples

data (,) a b = (,) a b   deriving (Eq, Ord, Bounded)
data (,,) a b c = (,,) a b c deriving (Eq, Ord, Bounded)
data (,,,) a b c d = (,,,) a b c d deriving (Eq, Ord, Bounded)
data (,,,,) a b c d e = (,,,,) a b c d e deriving (Eq, Ord, Bounded)
data (,,,,,) a b c d e f = (,,,,,) a b c d e f
data (,,,,,,) a b c d e f g = (,,,,,,) a b c d e f g
data (,,,,,,,) a b c d e f g h = (,,,,,,,) a b c d e f g h
data (,,,,,,,,) a b c d e f g h i = (,,,,,,,,) a b c d e f g h i
data (,,,,,,,,,) a b c d e f g h i j = (,,,,,,,,,) a b c d e f g h i j
data (,,,,,,,,,,) a b c d e f g h i j k = (,,,,,,,,,,) a b c d e f g h i j k
data (,,,,,,,,,,,) a b c d e f g h i j k l = (,,,,,,,,,,,) a b c d e f g h i j k l
data (,,,,,,,,,,,,) a b c d e f g h i j k l m = (,,,,,,,,,,,,) a b c d e f g h i j k l m
data (,,,,,,,,,,,,,) a b c d e f g h i j k l m n = (,,,,,,,,,,,,,) a b c d e f g h i j k l m n
data (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o = (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o
data (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p = (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p
data (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q
 = (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q
data (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r
 = (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r
data (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s
 = (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s
data (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t
 = (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t
data (,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u
 = (,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u
data (,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v
 = (,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v
data (,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w
 = (,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w
data (,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x
 = (,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x
data (,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y
 = (,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y
data (,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z
 = (,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z
data (,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p = readParen False
                            (\r -> [((x,y), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (")",w) <- lex v ] )
    readList	= readList__ (readsPrec 0)

instance (Read a, Read b, Read c) => Read (a, b, c) where
    readsPrec p = readParen False
			(\a -> [((x,y,z), h) | ("(",b) <- lex a,
					       (x,c)   <- readsPrec 0 b,
					       (",",d) <- lex c,
					       (y,e)   <- readsPrec 0 d,
					       (",",f) <- lex e,
					       (z,g)   <- readsPrec 0 f,
					       (")",h) <- lex g ] )
    readList	= readList__ (readsPrec 0)

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
    readsPrec p = readParen False
		    (\a -> [((w,x,y,z), j) | ("(",b) <- lex a,
					     (w,c)   <- readsPrec 0 b,
					     (",",d) <- lex c,
					     (x,e)   <- readsPrec 0 d,
					     (",",f) <- lex e,
					     (y,g)   <- readsPrec 0 f,
					     (",",h) <- lex g,
					     (z,i)   <- readsPrec 0 h,
					     (")",j) <- lex i ] )
    readList	= readList__ (readsPrec 0)

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
    readsPrec p = readParen False
		    (\a -> [((w,x,y,z,v), l) | ("(",b) <- lex a,
					       (w,c)   <- readsPrec 0 b,
					       (",",d) <- lex c,
					       (x,e)   <- readsPrec 0 d,
					       (",",f) <- lex e,
					       (y,g)   <- readsPrec 0 f,
					       (",",h) <- lex g,
					       (z,i)   <- readsPrec 0 h,
					       (",",j) <- lex i,
					       (v,k)   <- readsPrec 0 j,
					       (")",l) <- lex k ] )
    readList	= readList__ (readsPrec 0)

instance  (Show a, Show b) => Show (a,b)  where
    showsPrec p (x,y) = showChar '(' . shows x . showString ", " .
                                       shows y . showChar ')'
    showList	= showList__ (showsPrec 0) 

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showsPrec p (x,y,z) = showChar '(' . showsPrec 0 x . showString ", " .
					 showsPrec 0 y . showString ", " .
					 showsPrec 0 z . showChar ')'
    showList	= showList__ (showsPrec 0) 

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showsPrec p (w,x,y,z) = showChar '(' . showsPrec 0 w . showString ", " .
					   showsPrec 0 x . showString ", " .
					   showsPrec 0 y . showString ", " .
					   showsPrec 0 z . showChar ')'

    showList	= showList__ (showsPrec 0) 

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showsPrec p (v,w,x,y,z) = showChar '(' . showsPrec 0 v . showString ", " .
					     showsPrec 0 w . showString ", " .
					     showsPrec 0 x . showString ", " .
					     showsPrec 0 y . showString ", " .
					     showsPrec 0 z . showChar ')'
    showList	= showList__ (showsPrec 0) 

---------------------------------------------------------------------
-- component projections for pairs:
-- (NB: not provided for triples, quadruples, etc.)
fst			:: (a,b) -> a
fst (x,y)		=  x

snd			:: (a,b) -> b
snd (x,y)		=  y

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

-- Functions

-- Standard value bindings

-- identity function
id			:: a -> a
id x			=  x

-- constant function
const			:: a -> b -> a
const x _		=  x

-- function composition
{-# INLINE (.) #-}
{-# GENERATE_SPECS (.) a b c #-}
(.)			:: (b -> c) -> (a -> b) -> a -> c
f . g			=  \ x -> f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.
flip			:: (a -> b -> c) -> b -> a -> c
flip f x y		=  f y x

-- right-associating infix application operator (useful in continuation-
-- passing style)
($)			:: (a -> b) -> a -> b
f $ x			=  f x

-- until p f  yields the result of applying f until p holds.
until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf		:: a -> a -> a
asTypeOf		=  const

-- error stops execution and displays an error message

error :: String -> a
error s = error__ ( \ x -> _ccall_ ErrorHdrHook x ) s

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"

-- ============================================================
-- Standard list functions
-- ============================================================

{- module PreludeList -}

-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.

head                    :: [a] -> a
head (x:_)              =  x
head []                 =  error "PreludeList.head: empty list"

last                    :: [a] -> a
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  error "PreludeList.last: empty list"

tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  error "PreludeList.tail: empty list"

init                    :: [a] -> [a]
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  error "PreludeList.init: empty list"

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.
length                  :: [a] -> Int
length []               =  0
length (_:l)            =  1 + length l

-- List index (subscript) operator, 0-origin
(!!)                    :: [a] -> Int -> a
(x:_)  !! 0             =  x
(_:xs) !! n | n > 0     =  xs !! (n-1)
(_:_)  !! _             =  error "PreludeList.!!: negative index"
[]     !! _             =  error "PreludeList.!!: index too large"

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--      scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--      scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

foldl                   :: (a -> b -> a) -> a -> [b] -> a
foldl f z []            =  z
foldl f z (x:xs)        =  foldl f (f z x) xs

foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  error "PreludeList.foldl1: empty list"

scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs            =  q : (case xs of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  error "PreludeList.scanl1: empty list"

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

foldr                   :: (a -> b -> b) -> b -> [a] -> b
foldr f z []            =  z
foldr f z (x:xs)        =  f x (foldr f z xs)

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 f [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  error "PreludeList.foldr1: empty list"

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs 

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]           =  [x]
scanr1 f  (x:xs)        =  f x q : qs
                           where qs@(q:_) = scanr1 f xs 
scanr1 _ []             =  error "PreludeList.scanr1: empty list"

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]
iterate                 :: (a -> a) -> a -> [a]
iterate f x             =  x : iterate f (f x)

-- repeat x is an infinite list, with x the value of every element.
repeat                  :: a -> [a]
repeat x                =  xs where xs = x:xs

-- replicate n x is a list of length n with x the value of every element
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle                   :: [a] -> [a]
cycle xs                =  xs' where xs' = xs ++ xs'

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).

take                   :: Int -> [a] -> [a]
take 0 _               =  []
take _ []              =  []
take n (x:xs) | n > 0  =  x : take (n-1) xs
take _     _           =  error "PreludeList.take: negative argument"

drop                   :: Int -> [a] -> [a]
drop 0 xs              =  xs
drop _ []              =  []
drop n (_:xs) | n > 0  =  drop (n-1) xs
drop _     _           =  error "PreludeList.drop: negative argument"

splitAt                   :: Int -> [a] -> ([a],[a])
splitAt 0 xs              =  ([],xs)
splitAt _ []              =  ([],[])
splitAt n (x:xs) | n > 0  =  (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs
splitAt _     _           =  error "PreludeList.splitAt: negative argument"

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  Span p xs is equivalent to 
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile p []          =  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile p []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span p []               =  ([],[])
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)
break p                 =  span (not . p)

-- lines breaks a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.  Similary, words
-- breaks a string up into a list of words, which were delimited by
-- white space.  unlines and unwords are the inverse operations.
-- unlines joins lines with terminating newlines, and unwords joins
-- words with separating spaces.

lines			:: String -> [String]
lines ""		=  []
lines s			=  let (l, s') = break (== '\n') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> lines s''

words			:: String -> [String]
words s			=  case dropWhile {-partain:Char.-}isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = 
                                             break {-partain:Char.-}isSpace s'

unlines			:: [String] -> String
unlines			=  concatMap (++ "\n")

unwords			:: [String] -> String
unwords []		=  ""
unwords ws		=  foldr1 (\w s -> w ++ ' ':s) ws

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.
reverse                 :: [a] -> [a]
reverse                 =  foldl (flip (:)) []

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.
and, or                 :: [Bool] -> Bool
and                     =  foldr (&&) True
or                      =  foldr (||) False

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any, all                :: (a -> Bool) -> [a] -> Bool
any p                   =  or . map p
all p                   =  and . map p

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.
elem, notElem           :: (Eq a) => a -> [a] -> Bool
elem x                  =  any (== x)
notElem x               =  all (not . (/= x))

-- lookup key assocs looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []           =  Nothing
lookup key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- sum and product compute the sum or product of a finite list of numbers.
sum, product            :: (Num a) => [a] -> a
sum                     =  foldl (+) 0  
product                 =  foldl (*) 1

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
maximum, minimum        :: (Ord a) => [a] -> a
maximum []              =  error "PreludeList.maximum: empty list"
maximum xs              =  foldl1 max xs

minimum []              =  error "PreludeList.minimum: empty list"
minimum xs              =  foldl1 min xs

concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  concat . map f

-- zip takes two lists and returns a list of corresponding pairs.  If one
-- input list is short, excess elements of the longer list are discarded.
-- zip3 takes three lists and returns a list of triples.  Zips for larger
-- tuples are in the List library

zip                     :: [a] -> [b] -> [(a,b)]
zip                     =  zipWith (,)

zip3                    :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3                    =  zipWith3 (,,)

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.

zipWith                 :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith _ _ _           =  []

zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []


-- unzip transforms a list of pairs into a pair of lists.  

unzip                   :: [(a,b)] -> ([a],[b])
unzip                   =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

unzip3                  :: [(a,b,c)] -> ([a],[b],[c])
unzip3                  =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                                 ([],[],[])

{- module  PreludeText -}

type  ReadS a   = String -> [(a,String)]
type  ShowS     = String -> String

class  Read a  where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]

    readList    = readParen False (\r -> [pr | ("[",s)  <- lex r,
                                               pr       <- readl s])
                  where readl  s = [([],t)   | ("]",t)  <- lex s] ++
                                   [(x:xs,u) | (x,t)    <- reads s,
                                               (xs,u)   <- readl' t]
                        readl' s = [([],t)   | ("]",t)  <- lex s] ++
                                   [(x:xs,v) | (",",t)  <- lex s,
                                               (x,u)    <- reads t,
                                               (xs,v)   <- readl' u]

class  Show a  where
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS

    showList [] = showString "[]"
    showList (x:xs)
                = showChar '[' . shows x . showl xs
                  where showl []     = showChar ']'
                        showl (x:xs) = showString ", " . shows x . showl xs

reads           :: (Read a) => ReadS a
reads           =  readsPrec 0

shows           :: (Show a) => a -> ShowS
shows           =  showsPrec 0

read            :: (Read a) => String -> a
read s          =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> x
                        []  -> error "PreludeText.read: no parse"
                        _   -> error "PreludeText.read: ambiguous parse"

show            :: (Show a) => a -> String
show x          =  shows x ""

showChar        :: Char -> ShowS
showChar        =  (:)

showString      :: String -> ShowS
showString      =  (++)

showParen       :: Bool -> ShowS -> ShowS
showParen b p   =  if b then showChar '(' . p . showChar ')' else p

readParen       :: Bool -> ReadS a -> ReadS a
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = [(x,u) | ("(",s) <- lex r,
                                                (x,t)   <- optional s,
                                                (")",u) <- lex t    ]

-- lex: moved to GHCbase

{- module PreludeIO -}

-- in GHCio: type FilePath   =  String

fail            :: IOError -> IO a 
fail err	=  IO $ ST $ \ s -> (Left err, s)

userError       :: String  -> IOError
userError str	=  UserError str

catch           :: IO a    -> (IOError -> IO a) -> IO a 
catch (IO (ST m)) k  = IO $ ST $ \ s ->
  case (m s) of { (r, new_s) ->
  case r of
    Right  _ -> (r, new_s)
    Left err -> case (k err) of { IO (ST k_err) ->
		(k_err new_s) }}

putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putChar '\n'

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

getChar         :: IO Char
getChar         =  hGetChar stdin

getLine         :: IO String
getLine         =  do c <- getChar
                      if c == '\n' then return "" else 
                         do s <- getLine
                            return (c:s)
            
getContents     :: IO String
getContents     =  hGetContents stdin

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

readFile        :: FilePath -> IO String
readFile name	=  openFile name ReadMode >>= hGetContents

writeFile       :: FilePath -> String -> IO ()
writeFile name str
  = openFile name WriteMode >>= \hdl -> hPutStr hdl str >> hClose hdl

appendFile      :: FilePath -> String -> IO ()
appendFile name str
  = openFile name AppendMode >>= \hdl -> hPutStr hdl str >> hClose hdl

readIO          :: Read a => String -> IO a
  -- raises an exception instead of an error
readIO s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> return x
                        []  -> fail (userError "PreludeIO.readIO: no parse")
                        _   -> fail (userError 
                                      "PreludeIO.readIO: ambiguous parse")

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r
