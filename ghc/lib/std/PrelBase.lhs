%
% (c) The GRAP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelBase]{Module @PrelBase@}


\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelBase
	(
	module PrelBase,
	module PrelGHC		-- Re-export PrelGHC, to avoid lots of people 
				-- having to import it explicitly
  ) 
	where

import {-# SOURCE #-} PrelErr ( error )
import PrelGHC

infixr 9  .
infixr 5  ++, :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 0  $
\end{code}


%*********************************************************
%*							*
\subsection{Standard classes @Eq@, @Ord@, @Bounded@
%*							*
%*********************************************************

\begin{code}
class  Eq a  where
    (==), (/=)		:: a -> a -> Bool

    x /= y		=  not (x == y)
    x == y		= not  (x /= y)

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

    x <= y  = case compare x y of { GT -> False; other -> True }
    x <	 y  = case compare x y of { LT -> True;  other -> False }
    x >= y  = case compare x y of { LT -> False; other -> True }
    x >	 y  = case compare x y of { GT -> True;  other -> False }

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
    []     == []     = True	
    (x:xs) == (y:ys) = x == y && xs == ys
    _xs    == _ys    = False			

    xs     /= ys     = if (xs == ys) then False else True

instance (Ord a) => Ord [a] where
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
foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)

build 	:: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE build #-}
	-- The INLINE is important, even though build is tiny,
 	-- because it prevents [] getting inlined in the version that
	-- appears in the interface file.  If [] *is* inlined, it
	-- won't match with [] appearing in rules in an importing module.
build g = g (:) []

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build" 	forall k,z,g::forall b. (a->b->b) -> b -> b . 
		foldr k z (build g) = g k z

"foldr/augment" forall k,z,xs,g::forall b. (a->b->b) -> b -> b . 
		foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"    	foldr (:) [] = \x->x
"foldr/app"    	forall xs, ys. foldr (:) ys xs = append xs ys

"foldr/cons"	forall k,z,x,xs. foldr k z (x:xs) = k x (foldr k z xs)
"foldr/nil"	forall k,z.	 foldr k z []     = z 
 #-}
\end{code}


----------------------------------------------
--		map	
----------------------------------------------

\begin{code}
map :: (a -> b) -> [a] -> [b]
{-# INLINE map #-}
map f xs = build (\c n -> foldr (mapFB c f) n xs)

mapFB c f xs = c (f xs)

mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

{-# RULES
"mapFB"	    forall c,f,g.	mapFB (mapFB c f) g	= mapFB c (f.g) 
"mapList"   forall f.		foldr (mapFB (:) f) []	= mapList f
 #-}
\end{code}


----------------------------------------------
--		append	
----------------------------------------------
\begin{code}
(++) :: [a] -> [a] -> [a]
{-# INLINE (++) #-}
xs ++ ys = augment (\c n -> foldr c n xs) ys

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
it here seems more direct.

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

data Char = C# Char#	deriving (Eq, Ord)

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
maxInt  = I# (-2147483648#)	-- GHC <= 2.09 had this at -2147483647
minInt  = I# 2147483647#

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
\subsection{Type @Integer@, @Float@, @Double@}
%*							*
%*********************************************************

\begin{code}
data Float	= F# Float#
data Double	= D# Double#

data Integer	
   = S# Int#				-- small integers
   | J# Int# ByteArray#			-- large integers

instance  Eq Integer  where
    (S# i)     ==  (S# j)     = i ==# j
    (S# i)     ==  (J# s d)   = cmpIntegerInt# s d i ==# 0#
    (J# s d)   ==  (S# i)     = cmpIntegerInt# s d i ==# 0#
    (J# s1 d1) ==  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ==# 0#

    (S# i)     /=  (S# j)     = i /=# j
    (S# i)     /=  (J# s d)   = cmpIntegerInt# s d i /=# 0#
    (J# s d)   /=  (S# i)     = cmpIntegerInt# s d i /=# 0#
    (J# s1 d1) /=  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) /=# 0#
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

plusInt, minusInt, timesInt, quotInt, remInt :: Int -> Int -> Int
plusInt	(I# x) (I# y) = I# (x +# y)
minusInt(I# x) (I# y) = I# (x -# y)
timesInt(I# x) (I# y) = I# (x *# y)
quotInt	(I# x) (I# y) = I# (quotInt# x y)
remInt	(I# x) (I# y) = I# (remInt# x y)

negateInt :: Int -> Int
negateInt (I# x)      = I# (negateInt# x)

gtInt, geInt, eqInt, neInt, ltInt, leInt :: Int -> Int -> Bool
gtInt	(I# x) (I# y) = x ># y
geInt	(I# x) (I# y) = x >=# y
eqInt	(I# x) (I# y) = x ==# y
neInt	(I# x) (I# y) = x /=# y
ltInt	(I# x) (I# y) = x <# y
leInt	(I# x) (I# y) = x <=# y
\end{code}

Convenient boxed Integer PrimOps.  These are 'thin-air' Ids, so
it's nice to have them in PrelBase.

\begin{code}
{-# INLINE int2Integer #-}
{-# INLINE addr2Integer #-}
int2Integer :: Int# -> Integer
int2Integer  i = S# i
addr2Integer :: Addr# -> Integer
addr2Integer x = case addr2Integer# x of (# s, d #) -> J# s d
\end{code}
