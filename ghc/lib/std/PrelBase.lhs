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

    x <= y  = compare x y /= GT
    x <	 y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >	 y  = compare x y == GT
    max x y = case (compare x y) of { LT -> y ; EQ -> x ; GT -> x }
    min x y = case (compare x y) of { LT -> x ; EQ -> x ; GT -> y }

class  Bounded a  where
    minBound, maxBound :: a
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
\subsection{Classes @Num@ and @Enum@}
%*							*
%*********************************************************

\begin{code}
class  Enum a	where
    succ, pred		:: a -> a
    toEnum              :: Int -> a
    fromEnum            :: a -> Int
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

    succ		= toEnum . (+1) . fromEnum
    pred		= toEnum . (+(-1)) . fromEnum
    enumFromTo n m      =  map toEnum [fromEnum n .. fromEnum m]
    enumFromThenTo n n' m
                        =  map toEnum [fromEnum n, fromEnum n' .. fromEnum m]

class  (Eq a, Show a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a -- partain: Glasgow extension

    x - y		= x + negate y
    negate x		= 0 - x
    fromInt (I# i#)	= fromInteger (S# i#)
					-- Go via the standard class-op if the
					-- non-standard one ain't provided
\end{code}

\begin{code}
chr :: Int -> Char
chr = toEnum
ord :: Char -> Int
ord = fromEnum

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
    show      :: a   -> String
    showList  :: [a] -> ShowS

    showList ls     = showList__ (showsPrec 0) ls 
    showsPrec _ x s = show x ++ s
    show x          = showsPrec 0 x ""
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

    max a b = case compare a b of { LT -> b; EQ -> a;  GT -> a }
    min a b = case compare a b of { LT -> a; EQ -> a;  GT -> b }

    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare []     (_:_)  = LT
    compare (x:xs) (y:ys) = case compare x y of
                                 LT -> LT	
			         GT -> GT		
				 EQ -> compare xs ys

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

instance Functor [] where
    fmap = map

instance  Monad []  where
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]
    fail _		= []

instance  (Show a) => Show [a]  where
    showsPrec _         = showList
    showList  ls	= showList__ (showsPrec 0) ls
\end{code}

\end{code}

A few list functions that appear here because they are used here.
The rest of the prelude list functions are in PrelList.

\begin{code}
foldr                   :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []            =  z
foldr f z (x:xs)        =  f x (foldr f z xs)

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  Span p xs is equivalent to 
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

-- List index (subscript) operator, 0-origin
(!!)                    :: [a] -> Int -> a
#ifdef USE_REPORT_PRELUDE
(x:_)  !! 0             =  x
(_:xs) !! n | n > 0     =  xs !! (n-1)
(_:_)  !! _             =  error "Prelude.(!!): negative index"
[]     !! _             =  error "Prelude.(!!): index too large"
#else
-- HBC version (stolen), then unboxified
-- The semantics is not quite the same for error conditions
-- in the more efficient version.
--
_      !! n | n < 0  =  error "Prelude.(!!): negative index\n"
xs     !! n          =  sub xs (case n of { I# n# -> n# })
                           where sub :: [a] -> Int# -> a
                                 sub []      _ = error "Prelude.(!!): index too large\n"
                                 sub (y:ys) n# = if n# ==# 0#
						 then y
						 else sub ys (n# -# 1#)
#endif
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
    succ x      = error "Prelude.Enum.succ{()}: not possible"
    pred x      = error "Prelude.Enum.pred{()}: not possible"
    toEnum 0    = ()
    toEnum _	= error "Prelude.Enum.toEnum{()}: argument not 0"
    fromEnum () = 0
    enumFrom () 	= [()]
    enumFromThen () () 	= [()]
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = [()]

instance  Show ()  where
    showsPrec _ () = showString "()"
    showList ls    = showList__ (showsPrec 0) ls
\end{code}

%*********************************************************
%*							*
\subsection{Type @Ordering@}
%*							*
%*********************************************************

\begin{code}
data Ordering = LT | EQ | GT	deriving (Eq, Ord, Enum, Bounded, Show {- in PrelRead: Read -})
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
    succ     c@(C# c#)
       | not (ord# c# ==# 255#) = C# (chr# (ord# c# +# 1#))
       | otherwise	        = error ("Prelude.Enum.succ{Char}: tried to take `succ' of maxBound")
    pred     c@(C# c#)
       | not (ord# c# ==# 0#)   = C# (chr# (ord# c# -# 1#))
       | otherwise	        = error ("Prelude.Enum.pred{Char}: tried to to take `pred' of minBound")

    toEnum   (I# i) | i >=# 0# && i <=# 255# =  C# (chr# i)
		    | otherwise = error ("Prelude.Enum.toEnum{Char}: out of range: " ++ show (I# i))
    fromEnum (C# c) =  I# (ord# c)

    enumFrom   (C# c)	       = efttCh (ord# c)  1#   (># 255#)
    enumFromTo (C# c1) (C# c2) 
        | c1 `leChar#` c2 = efttCh (ord# c1) 1#               (># (ord# c2))
        | otherwise       = []

    enumFromThen (C# c1) (C# c2)
	| c1 `leChar#` c2 = efttCh (ord# c1) (ord# c2 -# ord# c1) (># 255#)
	| otherwise       = efttCh (ord# c1) (ord# c2 -# ord# c1) (<# 0#)

    enumFromThenTo (C# c1) (C# c2) (C# c3)
	| c1 `leChar#` c2 = efttCh (ord# c1) (ord# c2 -# ord# c1) (># (ord# c3))
	| otherwise       = efttCh (ord# c1) (ord# c2 -# ord# c1) (<# (ord# c3))

efttCh :: Int# -> Int# -> (Int# -> Bool) -> [Char]
efttCh init step done 
  = go init
  where
    go now | done now  = []
	   | otherwise = C# (chr# now) : go (now +# step)

instance  Show Char  where
    showsPrec _ '\'' = showString "'\\''"
    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':xs) = showString "\\\"" . showl xs
		       showl (x:xs)   = showLitChar x . showl xs
\end{code}


\begin{code}
isAscii, isLatin1, isControl, isPrint, isSpace, isUpper,
 isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum :: Char -> Bool
isAscii c	 	=  c <  '\x80'
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
isAsciiLower c          =  c >= 'a' && c <= 'z'
isAsciiUpper c          =  c >= 'A' && c <= 'Z'

isAlpha c		=  isLower c || isUpper c
isDigit c		=  c >= '0' && c <= '9'
isOctDigit c		=  c >= '0' && c <= '7'
isHexDigit c		=  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'
isAlphaNum c		=  isAlpha c || isDigit c

-- Case-changing operations

toUpper, toLower	:: Char -> Char
toUpper c@(C# c#)
  | isAsciiLower c    = C# (chr# (ord# c# -# 32#))
  | isAscii c         = c
    -- fall-through to the slower stuff.
  | isLower c	&& c /= '\xDF' && c /= '\xFF'
  = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  | otherwise
  = c



toLower c@(C# c#)
  | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
  | isAscii c      = c
  | isUpper c	   =  toEnum (fromEnum c - fromEnum 'A' 
                                              + fromEnum 'a')
  | otherwise	   =  c

asciiTab :: [String]
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

compareInt :: Int -> Int -> Ordering
(I# x) `compareInt` (I# y) | x <# y    = LT
			   | x ==# y   = EQ
			   | otherwise = GT

instance  Bounded Int where
    minBound =  -2147483648		-- GHC <= 2.09 had this at -2147483647
    maxBound =   2147483647

instance  Enum Int  where
    succ x  
       | x == maxBound  = error "Prelude.Enum.succ{Int}: tried to take `succ' of maxBound"
       | otherwise      = x+1
    pred x
       | x == minBound  = error "Prelude.Enum.pred{Int}: tried to take `pred' of minBound"
       | otherwise      = x-1

    toEnum   x = x
    fromEnum x = x

#ifndef USE_FOLDR_BUILD
    enumFrom     (I# c)	= efttInt True c 1# (\ _ -> False)

    enumFromTo   (I# c1) (I# c2) 
        | c1 <=# c2 = efttInt True  c1 1#              (># c2)
	| otherwise = []

    enumFromThen (I# c1) (I# c2) 
        | c1 <# c2  = efttInt True  c1 (c2 -# c1) (\ _ -> False)
	| otherwise = efttInt False c1 (c2 -# c1) (\ _ -> False)

    enumFromThenTo (I# c1) (I# c2) (I# c3)
	| c1 <=# c2 = efttInt True  c1 (c2 -# c1) (># c3)
	| otherwise = efttInt False c1 (c2 -# c1) (<# c3)

#else
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}
    enumFrom x           = build (\ c _ -> 
	let g x = x `c` g (x `plusInt` 1) in g x)
    enumFromTo x y	 = build (\ c n ->
	let g x = if x <= y then x `c` g (x `plusInt` 1) else n in g x)
#endif

efttInt :: Bool -> Int# -> Int# -> (Int# -> Bool) -> [Int]
efttInt increasing init step done = go init
  where
    go now 
     | done now                     = []    
     | increasing     && now ># nxt = [I# now] -- overflowed
     | not increasing && now <# nxt = [I# now] -- underflowed
     | otherwise	            = I# now : go nxt
     where
      nxt = now +# step

instance  Num Int  where
    (+)	   x y =  plusInt x y
    (-)	   x y =  minusInt x y
    negate x   =  negateInt x
    (*)	   x y =  timesInt x y
    abs    n   = if n `geInt` 0 then n else (negateInt n)

    signum n | n `ltInt` 0 = negateInt 1
	     | n `eqInt` 0 = 0
	     | otherwise   = 1

    fromInteger (S# i#) = I# i#
    fromInteger (J# s# d#)
      = case (integer2Int# s# d#) of { i# -> I# i# }

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
\subsection{Support code for @Show@}
%*							*
%*********************************************************

\begin{code}
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec 0

showChar        :: Char -> ShowS
showChar        =  (:)

showString      :: String -> ShowS
showString      =  (++)

showParen       :: Bool -> ShowS -> ShowS
showParen b p   =  if b then showChar '(' . p . showChar ')' else p

showList__ :: (a -> ShowS) ->  [a] -> ShowS

showList__ _ []         = showString "[]"
showList__ showx (x:xs) = showChar '[' . showx x . showl xs
  where
    showl []     = showChar ']'
    showl (y:ys) = showChar ',' . showx y . showl ys

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

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

intToDigit :: Int -> Char
intToDigit i
 | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
 | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
 | otherwise		=  error ("Char.intToDigit: not a digit " ++ show i)

\end{code}

Code specific for Ints.

\begin{code}
showSignedInt :: Int -> Int -> ShowS
showSignedInt p (I# n) r
  | n <# 0# && p > 6 = '(':itos n (')':r)
  | otherwise	     = itos n r

itos :: Int# -> String -> String
itos n r
  | n >=# 0#		= itos' n r
  | negateInt# n <# 0#  = -- n is minInt, a difficult number
	    itos (n `quotInt#` 10#) (itos' (negateInt# (n `remInt#` 10#)) r)
  | otherwise = '-':itos' (negateInt# n) r
 where
   itos' :: Int# -> String -> String
   itos' x cs 
     | x <# 10#  = C# (chr# (x +# ord# '0'#)) : cs
     | otherwise = itos' (x `quotInt#` 10#) 
		         (C# (chr# (x `remInt#` 10# +# ord# '0'#)) : cs)
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
