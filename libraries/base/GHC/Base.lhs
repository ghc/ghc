\section[GHC.Base]{Module @GHC.Base@}

The overall structure of the GHC Prelude is a bit tricky.

  a) We want to avoid "orphan modules", i.e. ones with instance
	decls that don't belong either to a tycon or a class
	defined in the same module

  b) We want to avoid giant modules

So the rough structure is as follows, in (linearised) dependency order


GHC.Prim		Has no implementation.  It defines built-in things, and
		by importing it you bring them into scope.
		The source file is GHC.Prim.hi-boot, which is just
		copied to make GHC.Prim.hi

GHC.Base	Classes: Eq, Ord, Functor, Monad
		Types:   list, (), Int, Bool, Ordering, Char, String

Data.Tuple	Types: tuples, plus instances for GHC.Base classes

GHC.Show	Class: Show, plus instances for GHC.Base/GHC.Tup types

GHC.Enum	Class: Enum,  plus instances for GHC.Base/GHC.Tup types

Data.Maybe	Type: Maybe, plus instances for GHC.Base classes

GHC.List	List functions

GHC.Num		Class: Num, plus instances for Int
		Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

		Integer is needed here because it is mentioned in the signature
		of 'fromInteger' in class Num

GHC.Real	Classes: Real, Integral, Fractional, RealFrac
			 plus instances for Int, Integer
		Types:  Ratio, Rational
			plus intances for classes so far

		Rational is needed here because it is mentioned in the signature
		of 'toRational' in class Real

GHC.ST	The ST monad, instances and a few helper functions

Ix		Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

GHC.Arr		Types: Array, MutableArray, MutableVar

		Arrays are used by a function in GHC.Float

GHC.Float	Classes: Floating, RealFloat
		Types:   Float, Double, plus instances of all classes so far

		This module contains everything to do with floating point.
		It is a big module (900 lines)
		With a bit of luck, many modules can be compiled without ever reading GHC.Float.hi


Other Prelude modules are much easier with fewer complex dependencies.

\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic data types and classes.
-- 
-----------------------------------------------------------------------------

#include "MachDeps.h"

-- #hide
module GHC.Base
	(
	module GHC.Base,
	module GHC.Prim,	-- Re-export GHC.Prim and GHC.Err, to avoid lots
	module GHC.Err          -- of people having to import it explicitly
  ) 
	where

import GHC.Prim
import {-# SOURCE #-} GHC.Err

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
%*  (for use when compiling GHC.Base itself doesn't work)
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
data [] a = MkNil

not True = False
(&&) True True = True
otherwise = True

build = error "urk"
foldr = error "urk"

unpackCString# :: Addr# -> [Char]
unpackFoldrCString# :: Addr# -> (Char  -> a -> a) -> a -> a 
unpackAppendCString# :: Addr# -> [Char] -> [Char]
unpackCStringUtf8# :: Addr# -> [Char]
unpackCString# a = error "urk"
unpackFoldrCString# a = error "urk"
unpackAppendCString# a = error "urk"
unpackCStringUtf8# a = error "urk"
-}
\end{code}


%*********************************************************
%*							*
\subsection{Standard classes @Eq@, @Ord@}
%*							*
%*********************************************************

\begin{code}

-- | The 'Eq' class defines equality ('==') and inequality ('/=').
-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
-- and 'Eq' may be derived for any datatype whose constituents are also
-- instances of 'Eq'.
--
-- Minimal complete definition: either '==' or '/='.
--
class  Eq a  where
    (==), (/=)		 :: a -> a -> Bool

    x /= y		 = not (x == y)
    x == y		 = not (x /= y)

-- | The 'Ord' class is used for totally ordered datatypes.
--
-- Instances of 'Ord' can be derived for any user-defined
-- datatype whose constituent types are in 'Ord'.  The declared order
-- of the constructors in the data declaration determines the ordering
-- in derived 'Ord' instances.  The 'Ordering' datatype allows a single
-- comparison to determine the precise ordering of two objects.
--
-- Minimal complete definition: either 'compare' or '<='.
-- Using 'compare' can be more efficient for complex types.
--
class  (Eq a) => Ord a  where
    compare		 :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min		 :: a -> a -> a

    compare x y
	| x == y    = EQ
	| x <= y    = LT	-- NB: must be '<=' not '<' to validate the
				-- above claim about the minimal things that
				-- can be defined for an instance of Ord
	| otherwise = GT

    x <	 y = case compare x y of { LT -> True;  _other -> False }
    x <= y = case compare x y of { GT -> False; _other -> True }
    x >	 y = case compare x y of { GT -> True;  _other -> False }
    x >= y = case compare x y of { LT -> False; _other -> True }

	-- These two default methods use '<=' rather than 'compare'
	-- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
\end{code}

%*********************************************************
%*							*
\subsection{Monadic classes @Functor@, @Monad@ }
%*							*
%*********************************************************

\begin{code}
{- | The 'Functor' class is used for types that can be mapped over.
Instances of 'Functor' should satisfy the following laws:

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g

The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}

class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Minimal complete definition: '>>=' and 'return'.

Instances of 'Monad' should satisfy the following laws:

> return a >>= k  ==  k a
> m >>= return  ==  m
> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

Instances of both 'Monad' and 'Functor' should additionally satisfy the law:

> fmap f xs  ==  xs >>= return . f

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}

class  Monad m  where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
	-- Explicit for-alls so that we know what order to
	-- give type arguments when desugaring

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    fail	:: String -> m a

    m >> k      = m >>= \_ -> k
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


instance (Eq a) => Eq [a] where
    {-# SPECIALISE instance Eq [Char] #-}
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _xs    == _ys    = False

instance (Ord a) => Ord [a] where
    {-# SPECIALISE instance Ord [Char] #-}
    compare []     []     = EQ
    compare []     (_:_)  = LT
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = case compare x y of
                                EQ    -> compare xs ys
                                other -> other

instance Functor [] where
    fmap = map

instance  Monad []  where
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]
    fail _		= []
\end{code}

A few list functions that appear here because they are used here.
The rest of the prelude list functions are in GHC.List.

----------------------------------------------
--	foldr/build/augment
----------------------------------------------
  
\begin{code}
-- | 'foldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a list, reduces the list
-- using the binary operator, from right to left:
--
-- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- Inline only in the final stage, after the foldr/cons rule has had a chance
foldr k z xs = go xs
	     where
	       go []     = z
	       go (y:ys) = y `k` go ys

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >	build g = g (:) []
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('build' g)@, which may arise after inlining, to @g k z@,
-- which avoids producing an intermediate list.

build 	:: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
	-- The INLINE is important, even though build is tiny,
 	-- because it prevents [] getting inlined in the version that
	-- appears in the interface file.  If [] *is* inlined, it
	-- won't match with [] appearing in rules in an importing module.
	--
	-- The "1" says to inline in phase 1

build g = g (:) []

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >	augment g xs = g (:) xs
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('augment' g xs)@, which may arise after inlining, to
-- @g k ('foldr' k z xs)@, which avoids producing an intermediate list.

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build" 	forall k z (g::forall b. (a->b->b) -> b -> b) . 
		foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) . 
		foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"    			  foldr (:) [] = \x  -> x
"foldr/app"    	[1] forall ys. foldr (:) ys = \xs -> xs ++ ys
	-- Only activate this from phase 1, because that's
	-- when we disable the rule that expands (++) into foldr

-- The foldr/cons rule looks nice, but it can give disastrously
-- bloated code when commpiling
--	array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- i.e. when there are very very long literal lists
-- So I've disabled it for now. We could have special cases
-- for short lists, I suppose.
-- "foldr/cons"	forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)

"foldr/single"	forall k z x. foldr k z [x] = k x z
"foldr/nil"	forall k z.   foldr k z []  = z 

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
-- | 'map' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- > map f [x1, x2, ...] == [f x1, f x2, ...]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f x ys = c (f x) ys

-- The rules for map work like this.
-- 
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold 
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- thing back into plain map.  
--
-- It's important that these two rules aren't both active at once 
-- (along with build's unfolding) else we'd get an infinite loop 
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimises compositions of map.
--
-- This same pattern is followed by many other functions: 
-- e.g. append, filter, iterate, repeat, etc.

{-# RULES
"map"	    [~1] forall f xs.	map f xs		= build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.	foldr (mapFB (:) f) []	= map f
"mapFB"	    forall c f g.	mapFB (mapFB c f) g	= mapFB c (f.g) 
  #-}
\end{code}


----------------------------------------------
--		append	
----------------------------------------------
\begin{code}
-- | Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
--
-- If the first list is not finite, the result is the first list.

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++"	[~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}

\end{code}


%*********************************************************
%*							*
\subsection{Type @Bool@}
%*							*
%*********************************************************

\begin{code}
-- |The 'Bool' type is an enumeration.  It is defined with 'False'
-- first so that the corresponding 'Prelude.Enum' instance will give
-- 'Prelude.fromEnum' 'False' the value zero, and
-- 'Prelude.fromEnum' 'True' the value 1.
data  Bool  =  False | True  deriving (Eq, Ord)
	-- Read in GHC.Read, Show in GHC.Show

-- Boolean functions

-- | Boolean \"and\"
(&&)			:: Bool -> Bool -> Bool
True  && x		=  x
False && _		=  False

-- | Boolean \"or\"
(||)			:: Bool -> Bool -> Bool
True  || _		=  True
False || x		=  x

-- | Boolean \"not\"
not			:: Bool -> Bool
not True		=  False
not False		=  True

-- |'otherwise' is defined as the value 'True'.  It helps to make
-- guards more readable.  eg.
--
-- >  f x | x < 0     = ...
-- >      | otherwise = ...
otherwise		:: Bool
otherwise 		=  True
\end{code}


%*********************************************************
%*							*
\subsection{The @()@ type}
%*							*
%*********************************************************

The Unit type is here because virtually any program needs it (whereas
some programs may get away without consulting GHC.Tup).  Furthermore,
the renamer currently *always* asks for () to be in scope, so that
ccalls can use () as their default type; so when compiling GHC.Base we
need ().  (We could arrange suck in () only if -fglasgow-exts, but putting
it here seems more direct.)

\begin{code}
-- | The unit datatype @()@ has one non-undefined member, the nullary
-- constructor @()@.
data () = ()

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
-- | Represents an ordering relationship between two values: less
-- than, equal to, or greater than.  An 'Ordering' is returned by
-- 'compare'.
data Ordering = LT | EQ | GT deriving (Eq, Ord)
	-- Read in GHC.Read, Show in GHC.Show
\end{code}


%*********************************************************
%*							*
\subsection{Type @Char@ and @String@}
%*							*
%*********************************************************

\begin{code}
-- | A 'String' is a list of characters.  String constants in Haskell are values
-- of type 'String'.
--
type String = [Char]

{-| The character type 'Char' is an enumeration whose values represent
Unicode (or equivalently ISO\/IEC 10646) characters
(see <http://www.unicode.org/> for details).
This set extends the ISO 8859-1 (Latin-1) character set
(the first 256 charachers), which is itself an extension of the ASCII
character set (the first 128 characters).
A character literal in Haskell has type 'Char'.

To convert a 'Char' to or from the corresponding 'Int' value defined
by Unicode, use 'Prelude.toEnum' and 'Prelude.fromEnum' from the
'Prelude.Enum' class respectively (or equivalently 'ord' and 'chr').
-}
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

{-# RULES
"x# `eqChar#` x#" forall x#. x# `eqChar#` x# = True
"x# `neChar#` x#" forall x#. x# `neChar#` x# = False
"x# `gtChar#` x#" forall x#. x# `gtChar#` x# = False
"x# `geChar#` x#" forall x#. x# `geChar#` x# = True
"x# `leChar#` x#" forall x#. x# `leChar#` x# = True
"x# `ltChar#` x#" forall x#. x# `ltChar#` x# = False
  #-}

-- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
chr :: Int -> Char
chr (I# i#) | int2Word# i# `leWord#` int2Word# 0x10FFFF# = C# (chr# i#)
            | otherwise                                  = error "Prelude.chr: bad argument"

unsafeChr :: Int -> Char
unsafeChr (I# i#) = C# (chr# i#)

-- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
\end{code}

String equality is used when desugaring pattern-matches against strings.

\begin{code}
eqString :: String -> String -> Bool
eqString []       []	   = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString cs1      cs2	   = False

{-# RULES "eqString" (==) = eqString #-}
-- eqString also has a BuiltInRule in PrelRules.lhs:
--	eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2
\end{code}


%*********************************************************
%*							*
\subsection{Type @Int@}
%*							*
%*********************************************************

\begin{code}
data Int = I# Int#
-- ^A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.

zeroInt, oneInt, twoInt, maxInt, minInt :: Int
zeroInt = I# 0#
oneInt  = I# 1#
twoInt  = I# 2#

{- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}
#if WORD_SIZE_IN_BITS == 31
minInt  = I# (-0x40000000#)
maxInt  = I# 0x3FFFFFFF#
#elif WORD_SIZE_IN_BITS == 32
minInt  = I# (-0x80000000#)
maxInt  = I# 0x7FFFFFFF#
#else 
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#
#endif

instance Eq Int where
    (==) = eqInt
    (/=) = neInt

instance Ord Int where
    compare = compareInt
    (<)     = ltInt
    (<=)    = leInt
    (>=)    = geInt
    (>)     = gtInt

compareInt :: Int -> Int -> Ordering
(I# x#) `compareInt` (I# y#) = compareInt# x# y#

compareInt# :: Int# -> Int# -> Ordering
compareInt# x# y#
    | x# <#  y# = LT
    | x# ==# y# = EQ
    | otherwise = GT
\end{code}


%*********************************************************
%*							*
\subsection{The function type}
%*							*
%*********************************************************

\begin{code}
-- | Identity function.
id			:: a -> a
id x			=  x

-- | The call '(lazy e)' means the same as 'e', but 'lazy' has a 
-- magical strictness property: it is lazy in its first argument, 
-- even though its semantics is strict.
lazy :: a -> a
lazy x = x
-- Implementation note: its strictness and unfolding are over-ridden
-- by the definition in MkId.lhs; in both cases to nothing at all.
-- That way, 'lazy' does not get inlined, and the strictness analyser
-- sees it as lazy.  Then the worker/wrapper phase inlines it.
-- Result: happiness


-- | The call '(inline f)' reduces to 'f', but 'inline' has a BuiltInRule
-- that tries to inline 'f' (if it has an unfolding) unconditionally
-- The 'NOINLINE' pragma arranges that inline only gets inlined (and
-- hence eliminated) late in compilation, after the rule has had
-- a god chance to fire.
inline :: a -> a
{-# NOINLINE[0] inline #-}
inline x = x

-- Assertion function.  This simply ignores its boolean argument.
-- The compiler may rewrite it to @('assertError' line)@.

-- | If the first argument evaluates to 'True', then the result is the
-- second argument.  Otherwise an 'AssertionFailed' exception is raised,
-- containing a 'String' with the source file and line number of the
-- call to 'assert'.
--
-- Assertions can normally be turned on or off with a compiler flag
-- (for GHC, assertions are normally on unless optimisation is turned on 
-- with @-O@ or the @-fignore-asserts@
-- option is given).  When assertions are turned off, the first
-- argument to 'assert' is ignored, and the second argument is
-- returned as the result.

-- 	SLPJ: in 5.04 etc 'assert' is in GHC.Prim,
--	but from Template Haskell onwards it's simply
--	defined here in Base.lhs
assert :: Bool -> a -> a
assert pred r = r

breakpoint :: a -> a
breakpoint r = r

breakpointCond :: Bool -> a -> a
breakpointCond _ r = r

data Unknown 
data Unknown1 a
data Unknown2 a b
data Unknown3 a b c
data Unknown4 a b c d

data Opaque = forall a. O a

-- | Constant function.
const			:: a -> b -> a
const x _		=  x

-- | Function composition.
{-# INLINE (.) #-}
(.)	  :: (b -> c) -> (a -> b) -> a -> c
(.) f g	x = f (g x)

-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
flip			:: (a -> b -> c) -> b -> a -> c
flip f x y		=  f y x

-- | Application operator.  This operator is redundant, since ordinary
-- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- low, right-associative binding precedence, so it sometimes allows
-- parentheses to be omitted; for example:
--
-- >     f $ g $ h x  =  f (g (h x))
--
-- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- or @'Data.List.zipWith' ('$') fs xs@.
{-# INLINE ($) #-}
($)			:: (a -> b) -> a -> b
f $ x			=  f x

-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)

-- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- used as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf		:: a -> a -> a
asTypeOf		=  const
\end{code}

%*********************************************************
%*							*
\subsection{Generics}
%*							*
%*********************************************************

\begin{code}
data Unit = Unit
#ifndef __HADDOCK__
data (:+:) a b = Inl a | Inr b
data (:*:) a b = a :*: b
#endif
\end{code}

%*********************************************************
%*							*
\subsection{@getTag@}
%*							*
%*********************************************************

Returns the 'tag' of a constructor application; this function is used
by the deriving code for Eq, Ord and Enum.

The primitive dataToTag# requires an evaluated constructor application
as its argument, so we provide getTag as a wrapper that performs the
evaluation before calling dataToTag#.  We could have dataToTag#
evaluate its argument, but we prefer to do it this way because (a)
dataToTag# can be an inline primop if it doesn't need to do any
evaluation, and (b) we want to expose the evaluation to the
simplifier, because it might be possible to eliminate the evaluation
in the case when the argument is already known to be evaluated.

\begin{code}
{-# INLINE getTag #-}
getTag :: a -> Int#
getTag x = x `seq` dataToTag# x
\end{code}

%*********************************************************
%*							*
\subsection{Numeric primops}
%*							*
%*********************************************************

\begin{code}
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
	-- Be careful NOT to overflow if we do any additional arithmetic
	-- on the arguments...  the following  previous version of this
	-- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    | (x# ># 0#) && (y# <# 0#) = ((x# -# 1#) `quotInt#` y#) -# 1#
    | (x# <# 0#) && (y# ># 0#) = ((x# +# 1#) `quotInt#` y#) -# 1#
    | otherwise                = x# `quotInt#` y#

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    | (x# ># 0#) && (y# <# 0#) ||
      (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
    | otherwise                   = r#
    where
    r# = x# `remInt#` y#
\end{code}

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

plusInt, minusInt, timesInt, quotInt, remInt, divInt, modInt, gcdInt :: Int -> Int -> Int
(I# x) `plusInt`  (I# y) = I# (x +# y)
(I# x) `minusInt` (I# y) = I# (x -# y)
(I# x) `timesInt` (I# y) = I# (x *# y)
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)

{-# RULES
"x# +# 0#" forall x#. x# +# 0# = x#
"0# +# x#" forall x#. 0# +# x# = x#
"x# -# 0#" forall x#. x# -# 0# = x#
"x# -# x#" forall x#. x# -# x# = 0#
"x# *# 0#" forall x#. x# *# 0# = 0#
"0# *# x#" forall x#. 0# *# x# = 0#
"x# *# 1#" forall x#. x# *# 1# = x#
"1# *# x#" forall x#. 1# *# x# = x#
  #-}

gcdInt (I# a) (I# b) = g a b
   where g 0# 0# = error "GHC.Base.gcdInt: gcd 0 0 is undefined"
         g 0# _  = I# absB
         g _  0# = I# absA
         g _  _  = I# (gcdInt# absA absB)

         absInt x = if x <# 0# then negateInt# x else x

         absA     = absInt a
         absB     = absInt b

negateInt :: Int -> Int
negateInt (I# x) = I# (negateInt# x)

gtInt, geInt, eqInt, neInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = x >#  y
(I# x) `geInt` (I# y) = x >=# y
(I# x) `eqInt` (I# y) = x ==# y
(I# x) `neInt` (I# y) = x /=# y
(I# x) `ltInt` (I# y) = x <#  y
(I# x) `leInt` (I# y) = x <=# y

{-# RULES
"x# ># x#"  forall x#. x# >#  x# = False
"x# >=# x#" forall x#. x# >=# x# = True
"x# ==# x#" forall x#. x# ==# x# = True
"x# /=# x#" forall x#. x# /=# x# = False
"x# <# x#"  forall x#. x# <#  x# = False
"x# <=# x#" forall x#. x# <=# x# = True
  #-}

{-# RULES
"plusFloat x 0.0"   forall x#. plusFloat#  x#   0.0# = x#
"plusFloat 0.0 x"   forall x#. plusFloat#  0.0# x#   = x#
"minusFloat x 0.0"  forall x#. minusFloat# x#   0.0# = x#
"minusFloat x x"    forall x#. minusFloat# x#   x#   = 0.0#
"timesFloat x 0.0"  forall x#. timesFloat# x#   0.0# = 0.0#
"timesFloat0.0 x"   forall x#. timesFloat# 0.0# x#   = 0.0#
"timesFloat x 1.0"  forall x#. timesFloat# x#   1.0# = x#
"timesFloat 1.0 x"  forall x#. timesFloat# 1.0# x#   = x#
"divideFloat x 1.0" forall x#. divideFloat# x#  1.0# = x#
  #-}

{-# RULES
"plusDouble x 0.0"   forall x#. (+##) x#    0.0## = x#
"plusDouble 0.0 x"   forall x#. (+##) 0.0## x#    = x#
"minusDouble x 0.0"  forall x#. (-##) x#    0.0## = x#
"minusDouble x x"    forall x#. (-##) x#    x#    = 0.0##
"timesDouble x 0.0"  forall x#. (*##) x#    0.0## = 0.0##
"timesDouble 0.0 x"  forall x#. (*##) 0.0## x#    = 0.0##
"timesDouble x 1.0"  forall x#. (*##) x#    1.0## = x#
"timesDouble 1.0 x"  forall x#. (*##) 1.0## x#    = x#
"divideDouble x 1.0" forall x#. (/##) x#    1.0## = x#
  #-}

-- Wrappers for the shift operations.  The uncheckedShift# family are
-- undefined when the amount being shifted by is greater than the size
-- in bits of Int#, so these wrappers perform a check and return
-- either zero or -1 appropriately.
--
-- Note that these wrappers still produce undefined results when the
-- second argument (the shift amount) is negative.

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
shiftL# :: Word# -> Int# -> Word#
a `shiftL#` b   | b >=# WORD_SIZE_IN_BITS# = int2Word# 0#
	        | otherwise                = a `uncheckedShiftL#` b

-- | Shift the argument right by the specified number of bits
-- (which must be non-negative).
shiftRL# :: Word# -> Int# -> Word#
a `shiftRL#` b  | b >=# WORD_SIZE_IN_BITS# = int2Word# 0#
	        | otherwise                = a `uncheckedShiftRL#` b

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
iShiftL# :: Int# -> Int# -> Int#
a `iShiftL#` b  | b >=# WORD_SIZE_IN_BITS# = 0#
	        | otherwise                = a `uncheckedIShiftL#` b

-- | Shift the argument right (signed) by the specified number of bits
-- (which must be non-negative).
iShiftRA# :: Int# -> Int# -> Int#
a `iShiftRA#` b | b >=# WORD_SIZE_IN_BITS# = if a <# 0# then (-1#) else 0#
	        | otherwise                = a `uncheckedIShiftRA#` b

-- | Shift the argument right (unsigned) by the specified number of bits
-- (which must be non-negative).
iShiftRL# :: Int# -> Int# -> Int#
a `iShiftRL#` b | b >=# WORD_SIZE_IN_BITS# = 0#
	        | otherwise                = a `uncheckedIShiftRL#` b

#if WORD_SIZE_IN_BITS == 32
{-# RULES
"narrow32Int#"  forall x#. narrow32Int#   x# = x#
"narrow32Word#" forall x#. narrow32Word#   x# = x#
   #-}
#endif

{-# RULES
"int2Word2Int"  forall x#. int2Word# (word2Int# x#) = x#
"word2Int2Word" forall x#. word2Int# (int2Word# x#) = x#
  #-}
\end{code}


%********************************************************
%*							*
\subsection{Unpacking C strings}
%*							*
%********************************************************

This code is needed for virtually all programs, since it's used for
unpacking the strings of error messages.

\begin{code}
unpackCString# :: Addr# -> [Char]
{-# NOINLINE [1] unpackCString# #-}
unpackCString# addr 
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

unpackFoldrCString# :: Addr# -> (Char  -> a -> a) -> a -> a 
{-# NOINLINE [0] unpackFoldrCString# #-}
-- Don't inline till right at the end;
-- usually the unpack-list rule turns it into unpackCStringList
-- It also has a BuiltInRule in PrelRules.lhs:
-- 	unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
--	  =  unpackFoldrCString# "foobaz" c n
unpackFoldrCString# addr f z 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | otherwise	   = C# ch `f` unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackCStringUtf8# :: Addr# -> [Char]
unpackCStringUtf8# addr 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'#   = []
      | ch `leChar#` '\x7F'# = C# ch : unpack (nh +# 1#)
      | ch `leChar#` '\xDF'# =
          C# (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#))) :
          unpack (nh +# 2#)
      | ch `leChar#` '\xEF'# =
          C# (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#))) :
          unpack (nh +# 3#)
      | otherwise            =
          C# (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 3#)) -# 0x80#))) :
          unpack (nh +# 4#)
      where
	ch = indexCharOffAddr# addr nh

unpackNBytes# :: Addr# -> Int# -> [Char]
unpackNBytes# _addr 0#   = []
unpackNBytes#  addr len# = unpack [] (len# -# 1#)
    where
     unpack acc i#
      | i# <# 0#  = acc
      | otherwise = 
	 case indexCharOffAddr# addr i# of
	    ch -> unpack (C# ch : acc) (i# -# 1#)

{-# RULES
"unpack"       [~1] forall a   . unpackCString# a		   = build (unpackFoldrCString# a)
"unpack-list"  [1]  forall a   . unpackFoldrCString# a (:) [] = unpackCString# a
"unpack-append"     forall a n . unpackFoldrCString# a (:) n  = unpackAppendCString# a n

-- There's a built-in rule (in PrelRules.lhs) for
-- 	unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n

  #-}
\end{code}

#ifdef __HADDOCK__
\begin{code}
-- | A special argument for the 'Control.Monad.ST.ST' type constructor,
-- indexing a state embedded in the 'Prelude.IO' monad by
-- 'Control.Monad.ST.stToIO'.
data RealWorld
\end{code}
#endif
