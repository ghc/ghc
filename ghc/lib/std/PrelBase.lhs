%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelBase]{Module @PrelBase@}


\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelBase(
	module PrelBase,
	module PrelGHC		-- Re-export PrelGHC, to avoid lots of people 
				-- having to import it explicitly
  ) where

import {-# SOURCE #-} PrelErr ( error )
import PrelGHC

infixr 9  .
infixl 9  !!
infixl 7  *
infixl 6  +, -
infixr 5  ++, :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 0  $
\end{code}


\begin{code}
{-
class Eval a
data Bool = False | True
data Int = I# Int#
data Double	= D# Double#
data  ()  =  ()  --easier to do explicitly: deriving (Eq, Ord, Enum, Show, Bounded)
		 -- (avoids weird-named functions, e.g., con2tag_()#

data  Maybe a  =  Nothing | Just a	
data Ordering = LT | EQ | GT	 deriving( Eq )

type  String = [Char]

data Char = C# Char#	
data [] a = [] | a : [a]  -- do explicitly: deriving (Eq, Ord)
			  -- to avoid weird names like con2tag_[]#


-------------- Stage 2 -----------------------
not True = False
not False = True
True  && x		=  x
False && x		=  False
otherwise = True

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

-------------- Stage 3 -----------------------
class  Eq a  where
    (==), (/=)		:: a -> a -> Bool

    x /= y		=  not (x == y)

-- f :: Eq a => a -> a -> Bool
f x y = x == y

g :: Eq a => a -> a -> Bool
g x y =  f x y 

-------------- Stage 4 -----------------------

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

eqInt	(I# x) (I# y) = x ==# y

instance Eq Int where
    (==) x y = x `eqInt` y

instance Ord Int where
    compare x y = error "help"
  
class  Bounded a  where
    minBound, maxBound :: a


type  ShowS     = String -> String

class  Show a  where
    showsPrec :: Bool -> a -> ShowS
    showList  :: [a] -> ShowS

    showList ls = showList__ (showsPrec True) ls 

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ showx []     = showString "[]"

showString      :: String -> ShowS
showString      =  (++)

[] ++ [] = []

shows           :: (Show a) => a -> ShowS
shows           =  showsPrec True

-- show            :: (Show a) => a -> String
--show x          =  shows x ""
-}
\end{code}


%*********************************************************
%*							*
\subsection{Standard classes @Eq@, @Ord@, @Bounded@, @Eval@}
%*							*
%*********************************************************

\begin{code}
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

class  Bounded a  where
    minBound, maxBound :: a

class Eval a
\end{code}

%*********************************************************
%*							*
\subsection{Monadic classes @Functor@, @Monad@, @MonadZero@, @MonadPlus@}
%*							*
%*********************************************************

\begin{code}
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
\end{code}


%*********************************************************
%*							*
\subsection{Classes @Num@ and @Enum@}
%*							*
%*********************************************************

\begin{code}
class  Enum a	where
    toEnum              :: Int -> a
    fromEnum            :: a -> Int
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

    enumFromTo n m      =  map toEnum [fromEnum n .. fromEnum m]
    enumFromThenTo n n' m
                        =  map toEnum [fromEnum n, fromEnum n' .. fromEnum m]

class  (Eq a, Show a, Eval a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a -- partain: Glasgow extension

    x - y		=  x + negate y
    fromInt (I# i#)	= fromInteger (int2Integer# i#)
					-- Go via the standard class-op if the
					-- non-standard one ain't provided
\end{code}

\begin{code}
{-# SPECIALISE succ :: Int -> Int #-}
{-# SPECIALISE pred :: Int -> Int #-}
succ, pred              :: Enum a => a -> a
succ                    =  toEnum . (+1) . fromEnum
pred                    =  toEnum . (subtract 1) . fromEnum

chr = (toEnum   :: Int  -> Char)
ord = (fromEnum :: Char -> Int)

ord_0 :: Num a => a
ord_0 = fromInt (ord '0')

{-# SPECIALISE subtract :: Int -> Int -> Int #-}
subtract	:: (Num a) => a -> a -> a
subtract x y	=  y - x
\end{code}


%*********************************************************
%*							*
\subsection{The @Show@ class}
%*							*
%*********************************************************

\begin{code}
type  ShowS     = String -> String

class  Show a  where
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS

    showList ls = showList__ (showsPrec 0) ls 
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
    xs     == ys     = False			
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
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]

instance  MonadZero []  where
    zero                = []

instance  MonadPlus []  where
#ifdef USE_REPORT_PRELUDE
    xs ++ ys            =  foldr (:) ys xs
#else
    [] ++ ys            =  ys
    (x:xs) ++ ys        =  x : (xs ++ ys)
#endif

instance  (Show a) => Show [a]  where
    showsPrec p         = showList
    showList  ls	= showList__ (showsPrec 0) ls
\end{code}

\end{code}

A few list functions that appear here because they are used here.
The rest of the prelude list functions are in PrelList.

\begin{code}
foldr                   :: (a -> b -> b) -> b -> [a] -> b
foldr f z []            =  z
foldr f z (x:xs)        =  f x (foldr f z xs)

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

-- List index (subscript) operator, 0-origin
(!!)                    :: [a] -> Int -> a
#ifdef USE_REPORT_PRELUDE
(x:_)  !! 0             =  x
(_:xs) !! n | n > 0     =  xs !! (n-1)
(_:_)  !! _             =  error "PreludeList.!!: negative index"
[]     !! _             =  error "PreludeList.!!: index too large"
#else
-- HBC version (stolen), then unboxified
-- The semantics is not quite the same for error conditions
-- in the more efficient version.
--
_      !! n | n < 0  =  error "(!!){PreludeList}: negative index\n"
xs     !! n          =  sub xs (case n of { I# n# -> n# })
                           where sub :: [a] -> Int# -> a
                                 sub []      _ = error "(!!){PreludeList}: index too large\n"
                                 sub (x:xs) n# = if n# ==# 0#
						 then x
						 else sub xs (n# -# 1#)
#endif
\end{code}


%*********************************************************
%*							*
\subsection{Type @Void@}
%*							*
%*********************************************************

The type @Void@ is built in, but it needs a @Show@ instance.

\begin{code}
void :: Void
void = error "You tried to evaluate void"

instance  Show Void  where
    showsPrec p f  =  showString "<<void>>"
    showList ls    = showList__ (showsPrec 0) ls
\end{code}


%*********************************************************
%*							*
\subsection{Type @Bool@}
%*							*
%*********************************************************

\begin{code}
data  Bool  =  False | True	deriving (Eq, Ord, Enum, Bounded, Show {- Read -})

-- Boolean functions

(&&), (||)		:: Bool -> Bool -> Bool
True  && x		=  x
False && x		=  False
True  || x		=  True
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
data  ()  =  ()  --easier to do explicitly: deriving (Eq, Ord, Enum, Show, Bounded)
		 -- (avoids weird-named functions, e.g., con2tag_()#

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

instance  Show ()  where
    showsPrec p () = showString "()"
    showList ls    = showList__ (showsPrec 0) ls
\end{code}

%*********************************************************
%*							*
\subsection{Type @Ordering@}
%*							*
%*********************************************************

\begin{code}
data Ordering = LT | EQ | GT	deriving (Eq, Ord, Enum, Bounded, Show {- Read -})
\end{code}


%*********************************************************
%*							*
\subsection{Type @Char@ and @String@}
%*							*
%*********************************************************

\begin{code}
type  String = [Char]

data Char = C# Char#	deriving (Eq, Ord)

instance  Enum Char  where
    toEnum   (I# i) | i >=# 0# && i <=# 255# =  C# (chr# i)
		    | otherwise = error ("Prelude.Enum.Char.toEnum:out of range: " ++ show (I# i))
    fromEnum (C# c)     	 =  I# (ord# c)

    enumFrom   (C# c)	       =  efttCh (ord# c)  1#   (># 255#)
    enumFromTo (C# c1) (C# c2) = efttCh (ord# c1) 1#  (># (ord# c2))

    enumFromThen (C# c1) (C# c2)
	| c1 `leChar#` c2 = efttCh (ord# c1) (ord# c2 -# ord# c1) (># 255#)
	| otherwise       = efttCh (ord# c1) (ord# c2 -# ord# c1) (<# 0#)

    enumFromThenTo (C# c1) (C# c2) (C# c3)
	| c1 `leChar#` c2 = efttCh (ord# c1) (ord# c2 -# ord# c1) (># (ord# c3))
	| otherwise       = efttCh (ord# c1) (ord# c2 -# ord# c1) (<# (ord# c3))

efttCh :: Int# -> Int# -> (Int# -> Bool) -> [Char]
efttCh now step done 
  = go now
  where
    go now | done now  = []
	   | otherwise = C# (chr# now) : go (now +# step)

instance  Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':cs) = showString "\\\"" . showl cs
		       showl (c:cs)   = showLitChar c . showl cs
\end{code}


\begin{code}
isAscii, isLatin1, isControl, isPrint, isSpace, isUpper,
 isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphanum :: Char -> Bool
isAscii c	 	=  fromEnum c < 128
isLatin1 c              =  c <= '\xff'
isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c		=  not (isControl c)

-- isSpace includes non-breaking space
-- Done with explicit equalities both for efficiency, and to avoid a tiresome
-- recursion with PrelList elem
isSpace c		=  c == ' '	||
			   c == '\t'	||
			   c == '\n'	||
			   c == '\r'	||
			   c == '\f'	||
			   c == '\v'	||
			   c == '\xa0'

-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper c		=  c >= 'A' && c <= 'Z' || 
                           c >= '\xC0' && c <= '\xD6' ||
                           c >= '\xD8' && c <= '\xDE'
-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
isLower c		=  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'
isAlpha c		=  isLower c || isUpper c
isDigit c		=  c >= '0' && c <= '9'
isOctDigit c		=  c >= '0' && c <= '7'
isHexDigit c		=  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'
isAlphanum c		=  isAlpha c || isDigit c

-- Case-changing operations

toUpper, toLower	:: Char -> Char
toUpper c | isLower c	&& c /= '\xDF' && c /= '\xFF'
 =  toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  | otherwise	=  c

toLower c | isUpper c	=  toEnum (fromEnum c - fromEnum 'A' 
                                              + fromEnum 'a')
	  | otherwise	=  c

asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 
\end{code}

%*********************************************************
%*							*
\subsection{Type @Int@}
%*							*
%*********************************************************

\begin{code}
data Int = I# Int#

instance Eq Int where
    (==) x y = x `eqInt` y
    (/=) x y = x `neInt` y

instance Ord Int where
    compare x y = compareInt x y 

    (<)  x y = ltInt x y
    (<=) x y = leInt x y
    (>=) x y = geInt x y
    (>)  x y = gtInt x y
    max x y = case (compareInt x y) of { LT -> y ; EQ -> x ; GT -> x }
    min x y = case (compareInt x y) of { LT -> x ; EQ -> x ; GT -> y }

(I# x) `compareInt` (I# y) | x <# y    = LT
			   | x ==# y   = EQ
			   | otherwise = GT

instance  Enum Int  where
    toEnum   x = x
    fromEnum x = x

#ifndef USE_FOLDR_BUILD
    enumFrom     (I# c)	         = eftInt c  1#
    enumFromTo   (I# c1) (I# c2) = efttInt c1 1#  (># c2)
    enumFromThen (I# c1) (I# c2) = eftInt c1 (c2 -# c1)

    enumFromThenTo (I# c1) (I# c2) (I# c3)
	| c1 <=# c2 = efttInt c1 (c2 -# c1) (># c3)
	| otherwise = efttInt c1 (c2 -# c1) (<# c3)

#else
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}
    enumFrom x           = build (\ c _ -> 
	let g x = x `c` g (x `plusInt` 1) in g x)
    enumFromTo x y	 = build (\ c n ->
	let g x = if x <= y then x `c` g (x `plusInt` 1) else n in g x)
#endif

efttInt :: Int# -> Int# -> (Int# -> Bool) -> [Int]
efttInt now step done
  = go now
  where
    go now | done now  = []
	   | otherwise = I# now : go (now +# step)

eftInt :: Int# -> Int# -> [Int]
eftInt now step
  = go now
  where
    go now = I# now : go (now +# step)


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

instance  Show Int  where
    showsPrec p n = showSignedInt p n
    showList ls   = showList__ (showsPrec 0)  ls
\end{code}


%*********************************************************
%*							*
\subsection{Type @Integer@, @Float@, @Double@}
%*							*
%*********************************************************

Just the type declarations.  If we don't actually use any @Integers@ we'd
rather not link the @Integer@ module at all; and the default-decl stuff
in the renamer tends to slurp in @Double@ regardless.

\begin{code}
data Float	= F# Float#
data Double	= D# Double#
data Integer	= J# Int# Int# ByteArray#
\end{code}


%*********************************************************
%*							*
\subsection{The function type}
%*							*
%*********************************************************

\begin{code}
-- The current implementation of seq# cannot handle function types,
-- so we leave this instance out rather than make false promises.
--
-- instance Eval (a -> b) 

instance  Show (a -> b)  where
    showsPrec p f  =  showString "<<function>>"
    showList ls	   = showList__ (showsPrec 0) ls


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
\subsection{Miscellaneous}
%*							*
%*********************************************************


\begin{code}
data Lift a = Lift a
\end{code}




%*********************************************************
%*							*
\subsection{Support code for @Show@}
%*							*
%*********************************************************

\begin{code}
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec 0

show            :: (Show a) => a -> String
show x          =  shows x ""

showChar        :: Char -> ShowS
showChar        =  (:)

showString      :: String -> ShowS
showString      =  (++)

showParen       :: Bool -> ShowS -> ShowS
showParen b p   =  if b then showChar '(' . p . showChar ')' else p

showList__ :: (a -> ShowS) ->  [a] -> ShowS

showList__ showx []     = showString "[]"
showList__ showx (x:xs) = showChar '[' . showx x . showl xs
  where
    showl []     = showChar ']'
    showl (x:xs) = showChar ',' . showx x . showl xs

showSpace :: ShowS
showSpace = {-showChar ' '-} \ xs -> ' ' : xs
\end{code}

Code specific for characters

\begin{code}
showLitChar 		   :: Char -> ShowS
showLitChar c | c > '\DEL' =  showChar '\\' . protectEsc isDigit (shows (ord c))
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : asciiTab!!ord c)

protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

intToDigit :: Int -> Char
intToDigit i
 | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
 | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i -10)
 | otherwise		=  error ("Char.intToDigit: not a digit" ++ show i)

\end{code}

Code specific for Ints.

\begin{code}
showSignedInt :: Int -> Int -> ShowS
showSignedInt p (I# n) r
  = -- from HBC version; support code follows
    if n <# 0# && p > 6 then '(':itos n++(')':r) else itos n ++ r

itos :: Int# -> String
itos n =
    if n <# 0# then
	if negateInt# n <# 0# then
	    -- n is minInt, a difficult number
	    itos (n `quotInt#` 10#) ++ itos' (negateInt# (n `remInt#` 10#)) []
	else
	    '-':itos' (negateInt# n) []
    else 
	itos' n []
  where
    itos' :: Int# -> String -> String
    itos' n cs = 
	if n <# 10# then
	    C# (chr# (n +# ord# '0'#)) : cs
	else 
	    itos' (n `quotInt#` 10#) (C# (chr# (n `remInt#` 10# +# ord# '0'#)) : cs)
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

plusInt	(I# x) (I# y) = I# (x +# y)
minusInt(I# x) (I# y) = I# (x -# y)
timesInt(I# x) (I# y) = I# (x *# y)
quotInt	(I# x) (I# y) = I# (quotInt# x y)
remInt	(I# x) (I# y) = I# (remInt# x y)
negateInt (I# x)      = I# (negateInt# x)
gtInt	(I# x) (I# y) = x ># y
geInt	(I# x) (I# y) = x >=# y
eqInt	(I# x) (I# y) = x ==# y
neInt	(I# x) (I# y) = x /=# y
ltInt	(I# x) (I# y) = x <# y
leInt	(I# x) (I# y) = x <=# y
\end{code}
