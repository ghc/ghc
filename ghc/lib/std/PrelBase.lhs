%
% (c) The GRAP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelBase]{Module @PrelBase@}


The overall structure of the GHC Prelude is a bit tricky.

  a) We want to avoid "orphan modules", i.e. ones with instance
	decls that don't belong either to a tycon or a class
	defined in the same module

  b) We want to avoid giant modules

So the rough structure is as follows, in (linearised) dependency order


PrelGHC		Has no implementation.  It defines built-in things, and
		by importing it you bring them into scope.
		The source file is PrelGHC.hi-boot, which is just
		copied to make PrelGHC.hi

		Classes: CCallable, CReturnable

PrelBase	Classes: Eq, Ord, Functor, Monad
		Types:   list, (), Int, Bool, Ordering, Char, String

PrelTup		Types: tuples, plus instances for PrelBase classes

PrelShow	Class: Show, plus instances for PrelBase/PrelTup types

PrelEnum	Class: Enum,  plus instances for PrelBase/PrelTup types

PrelMaybe	Type: Maybe, plus instances for PrelBase classes

PrelNum		Class: Num, plus instances for Int
		Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

		Integer is needed here because it is mentioned in the signature
		of 'fromInteger' in class Num

PrelReal	Classes: Real, Integral, Fractional, RealFrac
			 plus instances for Int, Integer
		Types:  Ratio, Rational
			plus intances for classes so far

		Rational is needed here because it is mentioned in the signature
		of 'toRational' in class Real

Ix		Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

PrelArr		Types: Array, MutableArray, MutableVar

		Does *not* contain any ByteArray stuff (see PrelByteArr)
		Arrays are used by a function in PrelFloat

PrelFloat	Classes: Floating, RealFloat
		Types:   Float, Double, plus instances of all classes so far

		This module contains everything to do with floating point.
		It is a big module (900 lines)
		With a bit of luck, many modules can be compiled without ever reading PrelFloat.hi

PrelByteArr	Types: ByteArray, MutableByteArray
		
		We want this one to be after PrelFloat, because it defines arrays
		of unboxed floats.


Other Prelude modules are much easier with fewer complex dependencies.


\begin{code}
{-# OPTIONS -fcompiling-prelude -fno-implicit-prelude #-}

module PrelBase
	(
	module PrelBase,
	module PrelGHC		-- Re-export PrelGHC, to avoid lots of people 
				-- having to import it explicitly
  ) 
	where

import {-# SOURCE #-} PrelErr ( error )
import {-# SOURCE #-} PrelNum ( addr2Integer )
  -- Otherwise the system import of addr2Integer looks for PrelNum.hi

import PrelGHC

infixr 9  .
infixr 5  ++, :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 0  $

default ()		-- Double isn't available yet
\end{code}


%*********************************************************
%*							*
\subsection{DEBUGGING STUFF}
%*  (for use when compiling PrelBase itself doesn't work)
%*							*
%*********************************************************

\begin{code}
{-		
data  Bool  =  False | True
data Ordering = LT | EQ | GT 
data Char = C# Char#
type  String = [Char]
data Int = I# Int#
data  ()  =  ()
-- data [] a = MkNil

not True = False
(&&) True True = True
otherwise = True

build = error "urk"
foldr = error "urk"

unpackCString#  :: Addr# -> [Char]
unpackFoldrCString#  :: Addr# -> (Char  -> a -> a) -> a -> a 
unpackAppendCString# :: Addr# -> [Char] -> [Char]
unpackNBytes#      :: Addr# -> Int#   -> [Char]
unpackNBytes# a b = error "urk"
unpackCString# a = error "urk"
unpackFoldrCString# a = error "urk"
unpackAppendCString# a = error "urk"
-}
\end{code}


%*********************************************************
%*							*
\subsection{Standard classes @Eq@, @Ord@}
%*							*
%*********************************************************

\begin{code}
class  Eq a  where
    (==), (/=)		:: a -> a -> Bool

--    x /= y		= not (x == y)
--    x == y		= not (x /= y)
--    x /= y		=  True
    (/=) x y            = not  ((==) x y)
    x == y		=  True

class  (Eq a) => Ord a  where
    compare             :: a -> a -> Ordering
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min		:: a -> a -> a

-- An instance of Ord should define either compare or <=
-- Using compare can be more efficient for complex types.
    compare x y
	    | x == y    = EQ
	    | x <= y    = LT	-- NB: must be '<=' not '<' to validate the
				-- above claim about the minimal things that can
				-- be defined for an instance of Ord
	    | otherwise = GT

    x <= y  = case compare x y of { GT -> False; _other -> True }
    x <	 y  = case compare x y of { LT -> True;  _other -> False }
    x >= y  = case compare x y of { LT -> False; _other -> True }
    x >	 y  = case compare x y of { GT -> True;  _other -> False }

	-- These two default methods use '>' rather than compare
	-- because the latter is often more expensive
    max x y = if x > y then x else y
    min x y = if x > y then y else x
\end{code}

%*********************************************************
%*							*
\subsection{Monadic classes @Functor@, @Monad@ }
%*							*
%*********************************************************

\begin{code}
class  Functor f  where
    fmap         :: (a -> b) -> f a -> f b

class  Monad m  where
    (>>=)       :: m a -> (a -> m b) -> m b
    (>>)        :: m a -> m b -> m b
    return      :: a -> m a
    fail	:: String -> m a

    m >> k      =  m >>= \_ -> k
    fail s      = error s

\end{code}


%*********************************************************
%*							*
\subsection{The list type}
%*							*
%*********************************************************

\begin{code}
data [] a = [] | a : [a]  -- do explicitly: deriving (Eq, Ord)
			  -- to avoid weird names like con2tag_[]#


instance (Eq a) => Eq [a]  where
{-
    {-# SPECIALISE instance Eq [Char] #-}
-}
    []     == []     = True	
    (x:xs) == (y:ys) = x == y && xs == ys
    _xs    == _ys    = False			

    xs     /= ys     = if (xs == ys) then False else True

instance (Ord a) => Ord [a] where
{-
    {-# SPECIALISE instance Ord [Char] #-}
-}
    a <  b  = case compare a b of { LT -> True;  EQ -> False; GT -> False }
    a <= b  = case compare a b of { LT -> True;  EQ -> True;  GT -> False }
    a >= b  = case compare a b of { LT -> False; EQ -> True;  GT -> True  }
    a >  b  = case compare a b of { LT -> False; EQ -> False; GT -> True  }

    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare []     (_:_)  = LT
    compare (x:xs) (y:ys) = case compare x y of
                                 LT -> LT	
			         GT -> GT		
				 EQ -> compare xs ys

instance Functor [] where
    fmap = map

instance  Monad []  where
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]
    fail _		= []
\end{code}

A few list functions that appear here because they are used here.
The rest of the prelude list functions are in PrelList.

----------------------------------------------
--	foldr/build/augment
----------------------------------------------
  
\begin{code}
foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE foldr #-}
foldr k z xs = go xs
	     where
	       go []     = z
	       go (x:xs) = x `k` go xs

build 	:: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE 2 build #-}
	-- The INLINE is important, even though build is tiny,
 	-- because it prevents [] getting inlined in the version that
	-- appears in the interface file.  If [] *is* inlined, it
	-- won't match with [] appearing in rules in an importing module.
	--
	-- The "2" says to inline in phase 2

build g = g (:) []

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE 2 augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build" 	forall k z (g::forall b. (a->b->b) -> b -> b) . 
		foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) . 
		foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"    	foldr (:) [] = \x->x
"foldr/app"    	forall xs ys. foldr (:) ys xs = append xs ys

"foldr/cons"	forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)
"foldr/nil"	forall k z.	 foldr k z []     = z 

"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
		       (h::forall b. (a->b->b) -> b -> b) .
		       augment g (build h) = build (\c n -> g c (h c n))
"augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
			augment g [] = build g
 #-}

-- This rule is true, but not (I think) useful:
--	augment g (augment h t) = augment (\cn -> g c (h c n)) t
\end{code}


----------------------------------------------
--		map	
----------------------------------------------

\begin{code}
map :: (a -> b) -> [a] -> [b]
map = mapList

-- Note eta expanded
mapFB c f x ys = c (f x) ys

mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

{-# RULES
"map"	    forall f xs.	map f xs		= build (\c n -> foldr (mapFB c f) n xs)
"mapFB"	    forall c f g.	mapFB (mapFB c f) g	= mapFB c (f.g) 
"mapList"   forall f.		foldr (mapFB (:) f) []	= mapList f
 #-}
\end{code}


----------------------------------------------
--		append	
----------------------------------------------
\begin{code}
(++) :: [a] -> [a] -> [a]
(++) = append

{-# RULES
  "++"	forall xs ys. (++) xs ys = augment (\c n -> foldr c n xs) ys
 #-}

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys
\end{code}


%*********************************************************
%*							*
\subsection{Type @Bool@}
%*							*
%*********************************************************

\begin{code}
data  Bool  =  False | True  deriving (Eq, Ord)
	-- Read in PrelRead, Show in PrelShow

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
\end{code}


%*********************************************************
%*							*
\subsection{The @()@ type}
%*							*
%*********************************************************

The Unit type is here because virtually any program needs it (whereas
some programs may get away without consulting PrelTup).  Furthermore,
the renamer currently *always* asks for () to be in scope, so that
ccalls can use () as their default type; so when compiling PrelBase we
need ().  (We could arrange suck in () only if -fglasgow-exts, but putting
it here seems more direct.)

\begin{code}
data  ()  =  ()

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
\end{code}


%*********************************************************
%*							*
\subsection{Type @Ordering@}
%*							*
%*********************************************************

\begin{code}
data Ordering = LT | EQ | GT deriving (Eq, Ord)
	-- Read in PrelRead, Show in PrelShow
\end{code}


%*********************************************************
%*							*
\subsection{Type @Char@ and @String@}
%*							*
%*********************************************************

\begin{code}
type  String = [Char]

data Char = C# Char#

-- We don't use deriving for Eq and Ord, because for Ord the derived
-- instance defines only compare, which takes two primops.  Then
-- '>' uses compare, and therefore takes two primops instead of one.

instance Eq Char where
  (C# c1) == (C# c2) = c1 `eqChar#` c2
  (C# c1) /= (C# c2) = c1 `neChar#` c2

instance Ord Char where
  (C# c1) >  (C# c2) = c1 `gtChar#` c2
  (C# c1) >= (C# c2) = c1 `geChar#` c2
  (C# c1) <= (C# c2) = c1 `leChar#` c2
  (C# c1) <  (C# c2) = c1 `ltChar#` c2

chr :: Int -> Char
chr (I# i) | i >=# 0# && i <=# 255# = C# (chr# i)
	   | otherwise = error ("Prelude.chr: bad argument")

unsafeChr :: Int -> Char
unsafeChr (I# i) =  C# (chr# i)

ord :: Char -> Int
ord (C# c) =  I# (ord# c)
\end{code}


%*********************************************************
%*							*
\subsection{Type @Int@}
%*							*
%*********************************************************

\begin{code}
data Int = I# Int#

zeroInt, oneInt, twoInt, maxInt, minInt :: Int
zeroInt = I# 0#
oneInt  = I# 1#
twoInt  = I# 2#
minInt  = I# (-2147483648#)	-- GHC <= 2.09 had this at -2147483647
maxInt  = I# 2147483647#

instance Eq Int where
    (==) x y = x `eqInt` y
    (/=) x y = x `neInt` y

instance Ord Int where
    compare x y = compareInt x y 

    (<)  x y = ltInt x y
    (<=) x y = leInt x y
    (>=) x y = geInt x y
    (>)  x y = gtInt x y

compareInt :: Int -> Int -> Ordering
(I# x) `compareInt` (I# y) | x <# y    = LT
			   | x ==# y   = EQ
			   | otherwise = GT
\end{code}


%*********************************************************
%*							*
\subsection{The function type}
%*							*
%*********************************************************

\begin{code}
-- identity function
id			:: a -> a
id x			=  x

-- constant function
const			:: a -> b -> a
const x _		=  x

-- function composition
{-# INLINE (.) #-}
(.)	  :: (b -> c) -> (a -> b) -> a -> c
(.) f g	x = f (g x)

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
\end{code}

%*********************************************************
%*							*
\subsection{CCallable instances}
%*							*
%*********************************************************

Defined here to avoid orphans

\begin{code}
instance CCallable Char
instance CReturnable Char

instance CCallable   Int
instance CReturnable Int

instance CReturnable () -- Why, exactly?
\end{code}


%*********************************************************
%*							*
\subsection{Numeric primops}
%*							*
%*********************************************************

Definitions of the boxed PrimOps; these will be
used in the case of partial applications, etc.

\begin{code}
{-# INLINE eqInt #-}
{-# INLINE neInt #-}
{-# INLINE gtInt #-}
{-# INLINE geInt #-}
{-# INLINE ltInt #-}
{-# INLINE leInt #-}
{-# INLINE plusInt #-}
{-# INLINE minusInt #-}
{-# INLINE timesInt #-}
{-# INLINE quotInt #-}
{-# INLINE remInt #-}
{-# INLINE negateInt #-}

plusInt, minusInt, timesInt, quotInt, remInt, gcdInt :: Int -> Int -> Int
plusInt	(I# x) (I# y) = I# (x +# y)
minusInt(I# x) (I# y) = I# (x -# y)
timesInt(I# x) (I# y) = I# (x *# y)
quotInt	(I# x) (I# y) = I# (quotInt# x y)
remInt	(I# x) (I# y) = I# (remInt# x y)
gcdInt (I# a)  (I# b) = I# (gcdInt# a b)

negateInt :: Int -> Int
negateInt (I# x)      = I# (negateInt# x)

divInt, modInt :: Int -> Int -> Int
x `divInt` y 
  | x > zeroInt && y < zeroInt = quotInt ((x `minusInt` y) `minusInt` oneInt) y
  | x < zeroInt && y > zeroInt = quotInt ((x `minusInt` y) `plusInt`  oneInt) y
  | otherwise	   = quotInt x y

x `modInt` y 
  | x > zeroInt && y < zeroInt || 
    x < zeroInt && y > zeroInt  = if r/=zeroInt then r `plusInt` y else zeroInt
  | otherwise	   		= r
  where
    r = remInt x y

gtInt, geInt, eqInt, neInt, ltInt, leInt :: Int -> Int -> Bool
gtInt	(I# x) (I# y) = x ># y
geInt	(I# x) (I# y) = x >=# y
eqInt	(I# x) (I# y) = x ==# y
neInt	(I# x) (I# y) = x /=# y
ltInt	(I# x) (I# y) = x <# y
leInt	(I# x) (I# y) = x <=# y
\end{code}


%********************************************************
%*							*
\subsection{Unpacking C strings}
%*							*
%********************************************************

This code is needed for virtually all programs, since it's used for
unpacking the strings of error messages.

\begin{code}
unpackCString#  :: Addr# -> [Char]
unpackCString# a = unpackCStringList# a

unpackCStringList#  :: Addr# -> [Char]
unpackCStringList# addr 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackAppendCString# :: Addr# -> [Char] -> [Char]
unpackAppendCString# addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackFoldrCString#  :: Addr# -> (Char  -> a -> a) -> a -> a 
unpackFoldrCString# addr f z 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | otherwise	   = C# ch `f` unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackNBytes#      :: Addr# -> Int#   -> [Char]
  -- This one is called by the compiler to unpack literal 
  -- strings with NULs in them; rare. It's strict!
  -- We don't try to do list deforestation for this one

unpackNBytes# _addr 0#   = []
unpackNBytes#  addr len# = unpack [] (len# -# 1#)
    where
     unpack acc i#
      | i# <# 0#  = acc
      | otherwise = 
	 case indexCharOffAddr# addr i# of
	    ch -> unpack (C# ch : acc) (i# -# 1#)

{-# RULES
"unpack"	 forall a   . unpackCString# a		   = build (unpackFoldrCString# a)
"unpack-list"    forall a   . unpackFoldrCString# a (:) [] = unpackCStringList# a
"unpack-append"  forall a n . unpackFoldrCString# a (:) n  = unpackAppendCString# a n

-- There's a built-in rule (in PrelRules.lhs) for
-- 	unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n

  #-}

\end{code}
