{----------------------------------------------------------------------------
__   __ __  __  ____   ___    _______________________________________________
||   || ||  || ||  || ||__    Hugs 98: The Nottingham and Yale Haskell system
||___|| ||__|| ||__||  __||   Copyright (c) 1994-1999
||---||         ___||         World Wide Web: http://haskell.org/hugs
||   ||                       Report bugs to: hugs-bugs@haskell.org
||   || Version: January 1999 _______________________________________________

 This is the Hugs 98 Standard Prelude, based very closely on the Standard
 Prelude for Haskell 98.

 WARNING: This file is an integral part of the Hugs source code.  Changes to
 the definitions in this file without corresponding modifications in other
 parts of the program may cause the interpreter to fail unexpectedly.  Under
 normal circumstances, you should not attempt to modify this file in any way!

-----------------------------------------------------------------------------
 Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale Haskell
 Group 1994-99, and is distributed as Open Source software under the
 Artistic License; see the file "Artistic" that is included in the
 distribution for details.
----------------------------------------------------------------------------}

module Prelude (
--  module PreludeList,
    map, (++), concat, filter,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
--  module PreludeText, 
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(show, showsPrec, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen,
--  module PreludeIO,
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,
--  module Ix,
    Ix(range, index, inRange, rangeSize),
--  module Char,
    isAscii, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,
--  module Numeric
    showSigned, showInt,
    readSigned, readInt,
    readDec, readOct, readHex, readSigned,
    readFloat, lexDigits, 
--  module Ratio,
    Ratio, Rational, (%), numerator, denominator, approxRational,
--  Non-standard exports
    IO(..), IOResult(..), Addr, StablePtr,
    makeStablePtr, freeStablePtr, deRefStablePtr,

    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, IO,
--  List type: []((:), [])
    (:),
--  Tuple types: (,), (,,), etc.
--  Trivial type: ()
--  Functions: (->)
    Rec, EmptyRec, EmptyRow, -- non-standard, should only be exported if TREX
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
--  Num((+), (-), (*), negate, abs, signum, fromInteger),
    Num((+), (-), (*), negate, abs, signum, fromInteger, fromInt),
    Real(toRational),
--  Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Integral(quot, rem, div, mod, quotRem, divMod, even, odd, toInteger, toInt),
--  Fractional((/), recip, fromRational),
    Fractional((/), recip, fromRational, fromDouble),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero),
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, accumulate, sequence, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac, atan2,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)

    ,trace
    -- Arrrggghhh!!! Help! Help! Help!
    -- What?!  Prelude.hs doesn't even _define_ most of these things!
    ,primCompAux,PrimArray,primRunST,primNewArray,primWriteArray
    ,primUnsafeFreezeArray,primIndexArray,primGetRawArgs,primGetEnv
    ,nh_stdin,nh_stdout,nh_stderr,copy_String_to_cstring,nh_open
    ,nh_free,nh_close,nh_errno,nh_flush,nh_read,primIntToChar
    ,unsafeInterleaveIO,nh_write,primCharToInt

    -- debugging hacks
    --,ST(..)
    --,primIntToAddr
    --,primGetArgc
    --,primGetArgv
  ) where

-- Standard value bindings {Prelude} ----------------------------------------

infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, %
infixl 6  +, -
--infixr 5  :    -- this fixity declaration is hard-wired into Hugs
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`

-- Equality and Ordered classes ---------------------------------------------

class Eq a where
    (==), (/=) :: a -> a -> Bool

    -- Minimal complete definition: (==) or (/=)
    x == y      = not (x/=y)
    x /= y      = not (x==y)

class (Eq a) => Ord a where
    compare                :: a -> a -> Ordering
    (<), (<=), (>=), (>)   :: a -> a -> Bool
    max, min               :: a -> a -> a

    -- Minimal complete definition: (<=) or compare
    -- using compare can be more efficient for complex types
    compare x y | x==y      = EQ
		| x<=y      = LT
		| otherwise = GT

    x <= y                  = compare x y /= GT
    x <  y                  = compare x y == LT
    x >= y                  = compare x y /= LT
    x >  y                  = compare x y == GT

    max x y   | x >= y      = x
	      | otherwise   = y
    min x y   | x <= y      = x
	      | otherwise   = y

class Bounded a where
    minBound, maxBound :: a
    -- Minimal complete definition: All

-- Numeric classes ----------------------------------------------------------

class (Eq a, Show a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
    fromInt        :: Int -> a

    -- Minimal complete definition: All, except negate or (-)
    x - y           = x + negate y
    fromInt         = fromIntegral
    negate x        = 0 - x

class (Num a, Ord a) => Real a where
    toRational     :: a -> Rational

class (Real a, Enum a) => Integral a where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod     :: a -> a -> (a,a)
    even, odd           :: a -> Bool
    toInteger           :: a -> Integer
    toInt               :: a -> Int

    -- Minimal complete definition: quotRem and toInteger
    n `quot` d           = q where (q,r) = quotRem n d
    n `rem` d            = r where (q,r) = quotRem n d
    n `div` d            = q where (q,r) = divMod n d
    n `mod` d            = r where (q,r) = divMod n d
    divMod n d           = if signum r == - signum d then (q-1, r+d) else qr
			   where qr@(q,r) = quotRem n d
    even n               = n `rem` 2 == 0
    odd                  = not . even
    toInt                = toInt . toInteger

class (Num a) => Fractional a where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: Rational -> a
    fromDouble   :: Double -> a

    -- Minimal complete definition: fromRational and ((/) or recip)
    recip x       = 1 / x
    fromDouble    = fromRational . toRational
    x / y         = x * recip y


class (Fractional a) => Floating a where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    -- Minimal complete definition: pi, exp, log, sin, cos, sinh, cosh,
    --				    asinh, acosh, atanh
    x ** y               = exp (log x * y)
    logBase x y          = log y / log x
    sqrt x               = x ** 0.5
    tan x                = sin x / cos x
    sinh x               = (exp x - exp (-x)) / 2
    cosh x               = (exp x + exp (-x)) / 2
    tanh x               = sinh x / cosh x
    asinh x              = log (x + sqrt (x*x + 1))
    acosh x              = log (x + sqrt (x*x - 1))
    atanh x              = (log (1 + x) - log (1 - x)) / 2

class (Real a, Fractional a) => RealFrac a where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

    -- Minimal complete definition: properFraction
    truncate x        = m where (m,_) = properFraction x

    round x           = let (n,r) = properFraction x
			    m     = if r < 0 then n - 1 else n + 1
			in case signum (abs r - 0.5) of
			    -1 -> n
			    0  -> if even n then n else m
			    1  -> m

    ceiling x         = if r > 0 then n + 1 else n
			where (n,r) = properFraction x

    floor x           = if r < 0 then n - 1 else n
			where (n,r) = properFraction x

class (RealFrac a, Floating a) => RealFloat a where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
		     :: a -> Bool
    atan2	     :: a -> a -> a

    -- Minimal complete definition: All, except exponent, signficand,
    --				    scaleFloat, atan2
    exponent x        = if m==0 then 0 else n + floatDigits x
			where (m,n) = decodeFloat x
    significand x     = encodeFloat m (- floatDigits x)
			where (m,_) = decodeFloat x
    scaleFloat k x    = encodeFloat m (n+k)
			where (m,n) = decodeFloat x
    atan2 y x
      | x>0           = atan (y/x)
      | x==0 && y>0   = pi/2
      | x<0 && y>0    = pi + atan (y/x)
      | (x<=0 && y<0) ||
        (x<0 && isNegativeZero y) ||
        (isNegativeZero x && isNegativeZero y)
		      = - atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
		      = pi    -- must be after the previous test on zero y
      | x==0 && y==0  = y     -- must be after the other double zero tests
      | otherwise     = x + y -- x or y is a NaN, return a NaN (via +)

-- Numeric functions --------------------------------------------------------

subtract       :: Num a => a -> a -> a
subtract        = flip (-)

gcd            :: Integral a => a -> a -> a
gcd 0 0         = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         = gcd' (abs x) (abs y)
		  where gcd' x 0 = x
			gcd' x y = gcd' y (x `rem` y)

lcm            :: (Integral a) => a -> a -> a
lcm _ 0         = 0
lcm 0 _         = 0
lcm x y         = abs ((x `quot` gcd x y) * y)

(^)            :: (Num a, Integral b) => a -> b -> a
x ^ 0           = 1
x ^ n  | n > 0  = f x (n-1) x
		  where f _ 0 y = y
			f x n y = g x n where
				  g x n | even n    = g (x*x) (n`quot`2)
					| otherwise = f x (n-1) (x*y)
_ ^ _           = error "Prelude.^: negative exponent"

(^^)           :: (Fractional a, Integral b) => a -> b -> a
x ^^ n          = if n >= 0 then x ^ n else recip (x^(-n))

fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral    = fromInteger . toInteger

realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      = fromRational . toRational

-- Index and Enumeration classes --------------------------------------------

class (Ord a) => Ix a where
    range                :: (a,a) -> [a]
    index                :: (a,a) -> a -> Int
    inRange              :: (a,a) -> a -> Bool
    rangeSize            :: (a,a) -> Int

    rangeSize r@(l,u)
             | l > u      = 0
             | otherwise  = index r u + 1

class Enum a where
    succ, pred           :: a -> a
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

    -- Minimal complete definition: toEnum, fromEnum
    succ                  = toEnum . (1+)       . fromEnum
    pred                  = toEnum . subtract 1 . fromEnum
    enumFromTo x y        = map toEnum [ fromEnum x .. fromEnum y ]
    enumFromThenTo x y z  = map toEnum [ fromEnum x, fromEnum y .. fromEnum z ]

-- Read and Show classes ------------------------------------------------------

type ReadS a = String -> [(a,String)]
type ShowS   = String -> String

class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]

    -- Minimal complete definition: readsPrec
    readList   = readParen False (\r -> [pr | ("[",s) <- lex r,
					      pr      <- readl s ])
		 where readl  s = [([],t)   | ("]",t) <- lex s] ++
				  [(x:xs,u) | (x,t)   <- reads s,
					      (xs,u)  <- readl' t]
		       readl' s = [([],t)   | ("]",t) <- lex s] ++
				  [(x:xs,v) | (",",t) <- lex s,
					      (x,u)   <- reads t,
					      (xs,v)  <- readl' u]

class Show a where
    show      :: a -> String
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS

    -- Minimal complete definition: show or showsPrec
    show x          = showsPrec 0 x ""
    showsPrec _ x s = show x ++ s
    showList []     = showString "[]"
    showList (x:xs) = showChar '[' . shows x . showl xs
		      where showl []     = showChar ']'
			    showl (x:xs) = showChar ',' . shows x . showl xs

-- Monad classes ------------------------------------------------------------

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a

    -- Minimal complete definition: (>>=), return
    p >> q  = p >>= \ _ -> q
    fail s  = error s

accumulate       :: Monad m => [m a] -> m [a]
accumulate []     = return []
accumulate (c:cs) = do x  <- c
		       xs <- accumulate cs
		       return (x:xs)

sequence         :: Monad m => [m a] -> m ()
sequence          = foldr (>>) (return ())

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f            = accumulate . map f

mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f           = sequence . map f

(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x           = x >>= f

-- Evaluation and strictness ------------------------------------------------

seq           :: a -> b -> b
seq x y       =  primSeq x y

($!)          :: (a -> b) -> a -> b
f $! x        =  x `primSeq` f x

-- Trivial type -------------------------------------------------------------

-- data () = () deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

instance Eq () where
    () == ()  =  True

instance Ord () where
    compare () () = EQ

instance Ix () where
    range ((),())      = [()]
    index ((),()) ()   = 0
    inRange ((),()) () = True

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]
    enumFromThen () () = [()]

instance Read () where
    readsPrec p = readParen False (\r -> [((),t) | ("(",s) <- lex r,
						   (")",t) <- lex s ])

instance Show () where
    showsPrec p () = showString "()"

instance Bounded () where
    minBound = ()
    maxBound = ()

-- Boolean type -------------------------------------------------------------

data Bool    = False | True
	       deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

(&&), (||)  :: Bool -> Bool -> Bool
False && x   = False
True  && x   = x
False || x   = x
True  || x   = True

not         :: Bool -> Bool
not True     = False
not False    = True

otherwise   :: Bool
otherwise    = True

-- Character type -----------------------------------------------------------

data Char               -- builtin datatype of ISO Latin characters
type String = [Char]    -- strings are lists of characters

instance Eq Char  where (==) = primEqChar
instance Ord Char where (<=) = primLeChar

instance Enum Char where
    toEnum           = primIntToChar
    fromEnum         = primCharToInt
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Char)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (lastChar::Char)]
		       where lastChar = if d < c then minBound else maxBound

instance Ix Char where
    range (c,c')      = [c..c']
    index b@(c,c') ci
       | inRange b ci = fromEnum ci - fromEnum c
       | otherwise    = error "Ix.index: Index out of range."
    inRange (c,c') ci = fromEnum c <= i && i <= fromEnum c'
			where i = fromEnum ci

instance Read Char where
    readsPrec p      = readParen False
			    (\r -> [(c,t) | ('\'':s,t) <- lex r,
					    (c,"\'")   <- readLitChar s ])
    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s ])
	       where readl ('"':s)      = [("",s)]
		     readl ('\\':'&':s) = readl s
		     readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t ]
instance Show Char where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs   = showChar '"' . showl cs
		    where showl ""       = showChar '"'
			  showl ('"':cs) = showString "\\\"" . showl cs
			  showl (c:cs)   = showLitChar c . showl cs

instance Bounded Char where
    minBound = '\0'
    maxBound = '\255'

isAscii, isControl, isPrint, isSpace            :: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphaNum  :: Char -> Bool

isAscii c              =  fromEnum c < 128
isControl c            =  c < ' ' ||  c == '\DEL'
isPrint c              =  c >= ' ' &&  c <= '~'
isSpace c              =  c == ' ' || c == '\t' || c == '\n' ||
			  c == '\r' || c == '\f' || c == '\v'
isUpper c              =  c >= 'A'   &&  c <= 'Z'
isLower c              =  c >= 'a'   &&  c <= 'z'
isAlpha c              =  isUpper c  ||  isLower c
isDigit c              =  c >= '0'   &&  c <= '9'
isAlphaNum c           =  isAlpha c  ||  isDigit c

-- Digit conversion operations
digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "Char.intToDigit: not a digit"

toUpper, toLower      :: Char -> Char
toUpper c | isLower c  = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
	  | otherwise  = c

toLower c | isUpper c  = toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
	  | otherwise  = c

ord         	      :: Char -> Int
ord         	       = fromEnum

chr                   :: Int -> Char
chr                    = toEnum

-- Maybe type ---------------------------------------------------------------

data Maybe a = Nothing | Just a
	       deriving (Eq, Ord, Read, Show)

maybe             :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Monad Maybe where
    Just x  >>= k = k x
    Nothing >>= k = Nothing
    return        = Just
    fail s        = Nothing

-- Either type --------------------------------------------------------------

data Either a b = Left a | Right b
		  deriving (Eq, Ord, Read, Show)

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT
		deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

-- Lists --------------------------------------------------------------------

--data [a] = [] | a : [a] deriving (Eq, Ord)

instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

instance Functor [] where
    fmap = map

instance Monad [ ] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []

instance Read a => Read [a]  where
    readsPrec p = readList

instance Show a => Show [a]  where
    showsPrec p = showList

-- Tuples -------------------------------------------------------------------

-- data (a,b) = (a,b) deriving (Eq, Ord, Ix, Read, Show)
-- etc..

-- Functions ----------------------------------------------------------------

instance Show (a -> b) where
    showsPrec p f = showString "<<function>>"

instance Functor ((->) a) where
    fmap = (.)

-- Standard Integral types --------------------------------------------------

data Int      -- builtin datatype of fixed size integers
data Integer  -- builtin datatype of arbitrary size integers

instance Eq Integer where 
    (==) x y = primCompareInteger x y == 0

instance Ord Integer where 
    compare x y = case primCompareInteger x y of
                      -1 -> LT
                      0  -> EQ
                      1  -> GT

instance Eq Int where 
    (==)          = primEqInt
    (/=)          = primNeInt

instance Ord Int     where 
    (<)           = primLtInt
    (<=)          = primLeInt
    (>=)          = primGeInt
    (>)           = primGtInt

instance Num Int where
    (+)           = primPlusInt
    (-)           = primMinusInt
    negate        = primNegateInt
    (*)           = primTimesInt
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToInt
    fromInt x     = x

instance Bounded Int where
    minBound = primMinInt
    maxBound = primMaxInt

instance Num Integer where
    (+)           = primPlusInteger
    (-)           = primMinusInteger
    negate        = primNegateInteger
    (*)           = primTimesInteger
    abs           = absReal
    signum        = signumReal
    fromInteger x = x
    fromInt       = primIntToInteger

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

instance Real Int where
    toRational x = toInteger x % 1

instance Real Integer where
    toRational x = x % 1

instance Integral Int where
    quotRem   = primQuotRemInt
    toInteger = primIntToInteger
    toInt x   = x

instance Integral Integer where
    quotRem       = primQuotRemInteger 
    --divMod        = primDivModInteger 
    toInteger     = id
    toInt         = primIntegerToInt

instance Ix Int where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = i - m
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Ix Integer where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = fromInteger (i - m)
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int where
    toEnum               = id
    fromEnum             = id
    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

instance Enum Integer where
    toEnum         = primIntToInteger
    fromEnum       = primIntegerToInt
    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

numericEnumFrom        :: Real a => a -> [a]
numericEnumFromThen    :: Real a => a -> a -> [a]
numericEnumFromTo      :: Real a => a -> a -> [a]
numericEnumFromThenTo  :: Real a => a -> a -> a -> [a]
numericEnumFrom n            = n : (numericEnumFrom $! (n+1))
numericEnumFromThen n m      = iterate ((m-n)+) n
numericEnumFromTo n m        = takeWhile (<= m) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                               where p | n' >= n   = (<= m)
				       | otherwise = (>= m)

instance Read Int where
    readsPrec p = readSigned readDec

instance  Show Int  where
    showsPrec p n 
      | n == minBound = showSigned showInt p (toInteger n)
      | otherwise     = showSigned showInt p n

instance Read Integer where
    readsPrec p = readSigned readDec

instance Show Integer where
    showsPrec   = showSigned showInt


-- Standard Floating types --------------------------------------------------

data Float     -- builtin datatype of single precision floating point numbers
data Double    -- builtin datatype of double precision floating point numbers

instance Eq  Float  where 
    (==)          = primEqFloat
    (/=)          = primNeFloat

instance Ord Float  where 
    (<)           = primLtFloat
    (<=)          = primLeFloat
    (>=)          = primGeFloat
    (>)           = primGtFloat

instance Num Float where
    (+)           = primPlusFloat
    (-)           = primMinusFloat
    negate        = primNegateFloat
    (*)           = primTimesFloat
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToFloat
    fromInt       = primIntToFloat



instance Eq  Double  where 
    (==)         = primEqDouble
    (/=)         = primNeDouble

instance Ord Double  where 
    (<)          = primLtDouble
    (<=)         = primLeDouble
    (>=)         = primGeDouble
    (>)          = primGtDouble

instance Num Double where
    (+)          = primPlusDouble
    (-)          = primMinusDouble
    negate       = primNegateDouble
    (*)          = primTimesDouble
    abs          = absReal
    signum       = signumReal
    fromInteger  = primIntegerToDouble
    fromInt      = primIntToDouble



instance Real Float where
    toRational = floatToRational

instance Real Double where
    toRational = doubleToRational

-- Calls to these functions are optimised when passed as arguments to
-- fromRational.
floatToRational  :: Float  -> Rational
doubleToRational :: Double -> Rational
floatToRational  x = realFloatToRational x 
doubleToRational x = realFloatToRational x

realFloatToRational x = (m%1)*(b%1)^^n
			where (m,n) = decodeFloat x
			      b     = floatRadix x

instance Fractional Float where
    (/)           = primDivideFloat
    fromRational  = rationalToRealFloat
    fromDouble    = primDoubleToFloat


instance Fractional Double where
    (/)          = primDivideDouble
    fromRational = rationalToRealFloat
    fromDouble x = x

rationalToRealFloat x = x'
 where x'    = f e
       f e   = if e' == e then y else f e'
	       where y      = encodeFloat (round (x * (1%b)^^e)) e
		     (_,e') = decodeFloat y
       (_,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
			     / fromInteger (denominator x))
       b     = floatRadix x'

instance Floating Float where
    pi    = 3.14159265358979323846
    exp   = primExpFloat
    log   = primLogFloat
    sqrt  = primSqrtFloat
    sin   = primSinFloat
    cos   = primCosFloat
    tan   = primTanFloat
    asin  = primAsinFloat
    acos  = primAcosFloat
    atan  = primAtanFloat

instance Floating Double where
    pi    = 3.14159265358979323846
    exp   = primExpDouble
    log   = primLogDouble
    sqrt  = primSqrtDouble
    sin   = primSinDouble
    cos   = primCosDouble
    tan   = primTanDouble
    asin  = primAsinDouble
    acos  = primAcosDouble
    atan  = primAtanDouble

instance RealFrac Float where
    properFraction = floatProperFraction

instance RealFrac Double where
    properFraction = floatProperFraction

floatProperFraction x
   | n >= 0      = (fromInteger m * fromInteger b ^ n, 0)
   | otherwise   = (fromInteger w, encodeFloat r n)
		   where (m,n) = decodeFloat x
			 b     = floatRadix x
			 (w,r) = quotRem m (b^(-n))

instance RealFloat Float where
    floatRadix  _ = toInteger primRadixFloat
    floatDigits _ = primDigitsFloat
    floatRange  _ = (primMinExpFloat,primMaxExpFloat)
    encodeFloat   = primEncodeFloatZ
    decodeFloat   = primDecodeFloatZ
    isNaN         = primIsNaNFloat
    isInfinite    = primIsInfiniteFloat    
    isDenormalized= primIsDenormalizedFloat
    isNegativeZero= primIsNegativeZeroFloat
    isIEEE        = const primIsIEEEFloat

instance RealFloat Double where
    floatRadix  _ = toInteger primRadixDouble
    floatDigits _ = primDigitsDouble
    floatRange  _ = (primMinExpDouble,primMaxExpDouble)
    encodeFloat   = primEncodeDoubleZ
    decodeFloat   = primDecodeDoubleZ
    isNaN         = primIsNaNDouble
    isInfinite    = primIsInfiniteDouble    
    isDenormalized= primIsDenormalizedDouble
    isNegativeZero= primIsNegativeZeroDouble
    isIEEE        = const primIsIEEEDouble        

instance Enum Float where
    toEnum		  = primIntToFloat
    fromEnum		  = truncate
    enumFrom		  = numericEnumFrom
    enumFromThen	  = numericEnumFromThen
    enumFromTo n m	  = numericEnumFromTo n (m+1/2)
    enumFromThenTo n n' m = numericEnumFromThenTo n n' (m + (n'-n)/2)

instance Enum Double where
    toEnum		  = primIntToDouble
    fromEnum		  = truncate
    enumFrom		  = numericEnumFrom
    enumFromThen	  = numericEnumFromThen
    enumFromTo n m	  = numericEnumFromTo n (m+1/2)
    enumFromThenTo n n' m = numericEnumFromThenTo n n' (m + (n'-n)/2)

instance Read Float where
    readsPrec p = readSigned readFloat

instance Show Float where
    showsPrec p = showSigned showFloat p

instance Read Double where
    readsPrec p = readSigned readFloat

instance Show Double where
    showsPrec p = showSigned showFloat p


-- Some standard functions --------------------------------------------------

fst            :: (a,b) -> a
fst (x,_)       = x

snd            :: (a,b) -> b
snd (_,y)       = y

curry          :: ((a,b) -> c) -> (a -> b -> c)
curry f x y     = f (x,y)

uncurry        :: (a -> b -> c) -> ((a,b) -> c)
uncurry f p     = f (fst p) (snd p)

id             :: a -> a
id    x         = x

const          :: a -> b -> a
const k _       = k

(.)            :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x       = f (g x)

flip           :: (a -> b -> c) -> b -> a -> c
flip f x y      = f y x

($)            :: (a -> b) -> a -> b
f $ x           = f x

until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)

asTypeOf       :: a -> a -> a
asTypeOf        = const

error          :: String -> a
error msg      =  primRaise (ErrorCall msg)

undefined         :: a
undefined | False = undefined

-- Standard functions on rational numbers {PreludeRatio} --------------------

data Integral a => Ratio a = a :% a deriving (Eq)
type Rational              = Ratio Integer

(%)                       :: Integral a => a -> a -> Ratio a
x % y                      = reduce (x * signum y) (abs y)

reduce                    :: Integral a => a -> a -> Ratio a
reduce x y | y == 0        = error "Ratio.%: zero denominator"
	   | otherwise     = (x `quot` d) :% (y `quot` d)
			     where d = gcd x y

numerator, denominator    :: Integral a => Ratio a -> a
numerator (x :% y)         = x
denominator (x :% y)       = y

instance Integral a => Ord (Ratio a) where
    compare (x:%y) (x':%y') = compare (x*y') (x'*y)

instance Integral a => Num (Ratio a) where
    (x:%y) + (x':%y') = reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y') = reduce (x*x') (y*y')
    negate (x :% y)   = negate x :% y
    abs (x :% y)      = abs x :% y
    signum (x :% y)   = signum x :% 1
    fromInteger x     = fromInteger x :% 1
    fromInt           = intToRatio

-- Hugs optimises code of the form fromRational (intToRatio x)
intToRatio :: Integral a => Int -> Ratio a
intToRatio x = fromInt x :% 1

instance Integral a => Real (Ratio a) where
    toRational (x:%y) = toInteger x :% toInteger y

instance Integral a => Fractional (Ratio a) where
    (x:%y) / (x':%y')   = (x*y') % (y*x')
    recip (x:%y)        = if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) = fromInteger x :% fromInteger y
    fromDouble 		= doubleToRatio

-- Hugs optimises code of the form fromRational (doubleToRatio x)
doubleToRatio :: Integral a => Double -> Ratio a
doubleToRatio x
	    | n>=0      = (fromInteger m * fromInteger b ^ n) % 1
	    | otherwise = fromInteger m % (fromInteger b ^ (-n))
			  where (m,n) = decodeFloat x
				b     = floatRadix x

instance Integral a => RealFrac (Ratio a) where
    properFraction (x:%y) = (fromIntegral q, r:%y)
			    where (q,r) = quotRem x y

instance Integral a => Enum (Ratio a) where
    toEnum       = fromInt
    fromEnum     = truncate
    enumFrom     = numericEnumFrom
    enumFromThen = numericEnumFromThen

instance (Read a, Integral a) => Read (Ratio a) where
    readsPrec p = readParen (p > 7)
			    (\r -> [(x%y,u) | (x,s)   <- reads r,
					      ("%",t) <- lex s,
					      (y,u)   <- reads t ])

instance Integral a => Show (Ratio a) where
    showsPrec p (x:%y) = showParen (p > 7)
			     (shows x . showString " % " . shows y)

approxRational      :: RealFrac a => a -> a -> Rational
approxRational x eps = simplest (x-eps) (x+eps)
 where simplest x y | y < x     = simplest y x
		    | x == y    = xr
		    | x > 0     = simplest' n d n' d'
		    | y < 0     = - simplest' (-n') d' (-n) d
		    | otherwise = 0 :% 1
				  where xr@(n:%d) = toRational x
					(n':%d')  = toRational y
       simplest' n d n' d'        -- assumes 0 < n%d < n'%d'
		    | r == 0    = q :% 1
		    | q /= q'   = (q+1) :% 1
		    | otherwise = (q*n''+d'') :% n''
				  where (q,r)      = quotRem n d
					(q',r')    = quotRem n' d'
					(n'':%d'') = simplest' d' r' d r

-- Standard list functions {PreludeList} ------------------------------------

head             :: [a] -> a
head (x:_)        = x

last             :: [a] -> a
last [x]          = x
last (_:xs)       = last xs

tail             :: [a] -> [a]
tail (_:xs)       = xs

init             :: [a] -> [a]
init [x]          = []
init (x:xs)       = x : init xs

null             :: [a] -> Bool
null []           = True
null (_:_)        = False

(++)             :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

map              :: (a -> b) -> [a] -> [b]
--map f xs          = [ f x | x <- xs ]
map f []     = []
map f (x:xs) = f x : map f xs


filter           :: (a -> Bool) -> [a] -> [a]
--filter p xs       = [ x | x <- xs, p x ]
filter p [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs


concat           :: [[a]] -> [a]
--concat            = foldr (++) []
concat []       = []
concat (xs:xss) = xs ++ concat xss

length           :: [a] -> Int
--length            = foldl' (\n _ -> n + 1) 0
length []     = 0
length (x:xs) = let n = length xs in primSeq n (1+n)

(!!)             :: [b] -> Int -> b
(x:_)  !! 0       = x
(_:xs) !! n | n>0 = xs !! (n-1)
(_:_)  !! _       = error "Prelude.!!: negative index"
[]     !! _       = error "Prelude.!!: index too large"

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []      = z
foldl f z (x:xs)  = foldl f (f z x) xs

foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)   = foldl f x xs

scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs      = q : (case xs of
			 []   -> []
			 x:xs -> scanl f (f q x) xs)

scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)   = scanl f x xs

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]      = x
foldr1 f (x:xs)   = f x (foldr1 f xs)

scanr            :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     = [q0]
scanr f q0 (x:xs) = f x q : qs
		    where qs@(q:_) = scanr f q0 xs

scanr1           :: (a -> a -> a) -> [a] -> [a]
scanr1 f [x]      = [x]
scanr1 f (x:xs)   = f x q : qs
		    where qs@(q:_) = scanr1 f xs

iterate          :: (a -> a) -> a -> [a]
iterate f x       = x : iterate f (f x)

repeat           :: a -> [a]
repeat x          = xs where xs = x:xs

replicate        :: Int -> a -> [a]
replicate n x     = take n (repeat x)

cycle            :: [a] -> [a]
cycle []          = error "Prelude.cycle: empty list"
cycle xs          = xs' where xs'=xs++xs'

take                :: Int -> [a] -> [a]
take 0 _             = []
take _ []            = []
take n (x:xs) | n>0  = x : take (n-1) xs
take _ _             = error "Prelude.take: negative argument"

drop                :: Int -> [a] -> [a]
drop 0 xs            = xs
drop _ []            = []
drop n (_:xs) | n>0  = drop (n-1) xs
drop _ _             = error "Prelude.drop: negative argument"

splitAt               :: Int -> [a] -> ([a], [a])
splitAt 0 xs           = ([],xs)
splitAt _ []           = ([],[])
splitAt n (x:xs) | n>0 = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs
splitAt _ _            = error "Prelude.splitAt: negative argument"

takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
	 | p x       = x : takeWhile p xs
	 | otherwise = []

dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
	 | p x       = dropWhile p xs'
	 | otherwise = xs

span, break         :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
	 | p x       = (x:ys, zs)
	 | otherwise = ([],xs)
                       where (ys,zs) = span p xs'
break p              = span (not . p)

lines     :: String -> [String]
lines ""   = []
lines s    = let (l,s') = break ('\n'==) s
             in l : case s' of []      -> []
                               (_:s'') -> lines s''

words     :: String -> [String]
words s    = case dropWhile isSpace s of
		  "" -> []
		  s' -> w : words s''
			where (w,s'') = break isSpace s'

unlines   :: [String] -> String
unlines    = concatMap (\l -> l ++ "\n")

unwords   :: [String] -> String
unwords [] = []
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws

reverse   :: [a] -> [a]
--reverse    = foldl (flip (:)) []
reverse xs = ri [] xs
             where ri acc []     = acc
                   ri acc (x:xs) = ri (x:acc) xs

and, or   :: [Bool] -> Bool
--and        = foldr (&&) True
--or         = foldr (||) False
and []     = True
and (x:xs) = if x then and xs else x
or  []     = False
or  (x:xs) = if x then x else or xs

any, all  :: (a -> Bool) -> [a] -> Bool
--any p      = or  . map p
--all p      = and . map p
any p []     = False
any p (x:xs) = if p x then True else any p xs
all p []     = True
all p (x:xs) = if p x then all p xs else False

elem, notElem    :: Eq a => a -> [a] -> Bool
--elem              = any . (==)
--notElem           = all . (/=)
elem x []        = False
elem x (y:ys)    = if x==y then True else elem x ys
notElem x []     = True
notElem x (y:ys) = if x==y then False else notElem x ys

lookup           :: Eq a => a -> [(a,b)] -> Maybe b
lookup k []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys

sum, product     :: Num a => [a] -> a
sum               = foldl' (+) 0
product           = foldl' (*) 1

maximum, minimum :: Ord a => [a] -> a
maximum           = foldl1 max
minimum           = foldl1 min

concatMap        :: (a -> [b]) -> [a] -> [b]
concatMap f       = concat . map f

zip              :: [a] -> [b] -> [(a,b)]
zip               = zipWith  (\a b -> (a,b))

zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3              = zipWith3 (\a b c -> (a,b,c))

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
			  = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []

unzip                    :: [(a,b)] -> ([a],[b])
unzip                     = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], [])

unzip3                   :: [(a,b,c)] -> ([a],[b],[c])
unzip3                    = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
				  ([],[],[])

-- PreludeText ----------------------------------------------------------------

reads        :: Read a => ReadS a
reads         = readsPrec 0

shows        :: Show a => a -> ShowS
shows         = showsPrec 0

read         :: Read a => String -> a
read s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
		      [x] -> x
		      []  -> error "Prelude.read: no parse"
		      _   -> error "Prelude.read: ambiguous parse"

showChar     :: Char -> ShowS
showChar      = (:)

showString   :: String -> ShowS
showString    = (++)

showParen    :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

showField    :: Show a => String -> a -> ShowS
showField m v = showString m . showChar '=' . shows v

readParen    :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
		where optional r  = g r ++ mandatory r
		      mandatory r = [(x,u) | ("(",s) <- lex r,
					     (x,t)   <- optional s,
					     (")",u) <- lex t    ]


readField    :: Read a => String -> ReadS a
readField m s0 = [ r | (t,  s1) <- lex s0, t == m,
                       ("=",s2) <- lex s1,
                       r        <- reads s2 ]

lex                    :: ReadS String
lex ""                  = [("","")]
lex (c:s) | isSpace c   = lex (dropWhile isSpace s)
lex ('\'':s)            = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
					       ch /= "'"                ]
lex ('"':s)             = [('"':str, t)      | (str,t) <- lexString s]
			  where
			  lexString ('"':s) = [("\"",s)]
			  lexString s = [(ch++str, u)
						| (ch,t)  <- lexStrItem s,
						  (str,u) <- lexString t  ]

			  lexStrItem ('\\':'&':s) = [("\\&",s)]
			  lexStrItem ('\\':c:s) | isSpace c
			      = [("",t) | '\\':t <- [dropWhile isSpace s]]
			  lexStrItem s            = lexLitChar s

lex (c:s) | isSingle c  = [([c],s)]
	  | isSym c     = [(c:sym,t)         | (sym,t) <- [span isSym s]]
	  | isAlpha c   = [(c:nam,t)         | (nam,t) <- [span isIdChar s]]
	  | isDigit c   = [(c:ds++fe,t)      | (ds,s)  <- [span isDigit s],
					       (fe,t)  <- lexFracExp s     ]
	  | otherwise   = []    -- bad character
		where
		isSingle c  =  c `elem` ",;()[]{}_`"
		isSym c     =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
		isIdChar c  =  isAlphaNum c || c `elem` "_'"

		lexFracExp ('.':s) = [('.':ds++e,u) | (ds,t) <- lexDigits s,
						      (e,u)  <- lexExp t    ]
		lexFracExp s       = [("",s)]

		lexExp (e:s) | e `elem` "eE"
			 = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
						   (ds,u) <- lexDigits t] ++
			   [(e:ds,t)   | (ds,t) <- lexDigits s]
		lexExp s = [("",s)]

lexDigits               :: ReadS String
lexDigits               =  nonnull isDigit

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

lexLitChar              :: ReadS String
lexLitChar ('\\':s)     =  [('\\':esc, t) | (esc,t) <- lexEsc s] 
	where
	lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]    -- "
        lexEsc ('^':c:s) | c >= '@' && c <= '_'    = [(['^',c],s)]
	lexEsc s@(d:_)   | isDigit d               = lexDigits s
        lexEsc s@(c:_)   | isUpper c
                          = let table = ('\DEL',"DEL") : asciiTab
			    in case [(mne,s') | (c, mne) <- table,
				 	        ([],s') <- [lexmatch mne s]]
			       of (pr:_) -> [pr]
			          []     -> []
	lexEsc _                                   = []
lexLitChar (c:s)        =  [([c],s)]
lexLitChar ""           =  []

isOctDigit c  =  c >= '0' && c <= '7'
isHexDigit c  =  isDigit c || c >= 'A' && c <= 'F'
			   || c >= 'a' && c <= 'f'

lexmatch                   :: (Eq a) => [a] -> [a] -> ([a],[a])
lexmatch (x:xs) (y:ys) | x == y  =  lexmatch xs ys
lexmatch xs     ys               =  (xs,ys)

asciiTab = zip ['\NUL'..' ']
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
	    "SP"]

readLitChar            :: ReadS Char
readLitChar ('\\':s)    = readEsc s
 where
       readEsc ('a':s)  = [('\a',s)]
       readEsc ('b':s)  = [('\b',s)]
       readEsc ('f':s)  = [('\f',s)]
       readEsc ('n':s)  = [('\n',s)]
       readEsc ('r':s)  = [('\r',s)]
       readEsc ('t':s)  = [('\t',s)]
       readEsc ('v':s)  = [('\v',s)]
       readEsc ('\\':s) = [('\\',s)]
       readEsc ('"':s)  = [('"',s)]
       readEsc ('\'':s) = [('\'',s)]
       readEsc ('^':c:s) | c >= '@' && c <= '_'
			= [(toEnum (fromEnum c - fromEnum '@'), s)]
       readEsc s@(d:_) | isDigit d
			= [(toEnum n, t) | (n,t) <- readDec s]
       readEsc ('o':s)  = [(toEnum n, t) | (n,t) <- readOct s]
       readEsc ('x':s)  = [(toEnum n, t) | (n,t) <- readHex s]
       readEsc s@(c:_) | isUpper c
			= let table = ('\DEL',"DEL") : asciiTab
			  in case [(c,s') | (c, mne) <- table,
					    ([],s') <- [lexmatch mne s]]
			     of (pr:_) -> [pr]
				[]     -> []
       readEsc _        = []
readLitChar (c:s)       = [(c,s)]

showLitChar               :: Char -> ShowS
showLitChar c | c > '\DEL' = showChar '\\' .
			     protectEsc isDigit (shows (fromEnum c))
showLitChar '\DEL'         = showString "\\DEL"
showLitChar '\\'           = showString "\\\\"
showLitChar c | c >= ' '   = showChar c
showLitChar '\a'           = showString "\\a"
showLitChar '\b'           = showString "\\b"
showLitChar '\f'           = showString "\\f"
showLitChar '\n'           = showString "\\n"
showLitChar '\r'           = showString "\\r"
showLitChar '\t'           = showString "\\t"
showLitChar '\v'           = showString "\\v"
showLitChar '\SO'          = protectEsc ('H'==) (showString "\\SO")
showLitChar c              = showString ('\\' : snd (asciiTab!!fromEnum c))

protectEsc p f             = f . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s

-- Unsigned readers for various bases
readDec, readOct, readHex :: Integral a => ReadS a
readDec = readInt 10 isDigit (\d -> fromEnum d - fromEnum '0')
readOct = readInt  8 isOctDigit (\d -> fromEnum d - fromEnum '0')
readHex = readInt 16 isHexDigit hex
	  where hex d = fromEnum d -
			(if isDigit d
			   then fromEnum '0'
			   else fromEnum (if isUpper d then 'A' else 'a') - 10)

-- readInt reads a string of digits using an arbitrary base.  
-- Leading minus signs must be handled elsewhere.

readInt :: Integral a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

-- showInt is used for positive numbers only
showInt    :: Integral a => a -> ShowS
showInt n r 
   | n < 0 
   = error "Numeric.showInt: can't show negative numbers"
   | otherwise 
{-
   = let (n',d) = quotRem n 10
         r'     = toEnum (fromEnum '0' + fromIntegral d) : r
     in  if n' == 0 then r' else showInt n' r'
-}
   = case quotRem n 10 of { (n',d) ->
     let r' = toEnum (fromEnum '0' + fromIntegral d) : r
     in  if n' == 0 then r' else showInt n' r'
     }


readSigned:: Real a => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(-x,t) | ("-",s) <- lex r,
						(x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
						(n,"")  <- readPos str]

showSigned    :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = if x < 0 then showParen (p > 6)
						 (showChar '-' . showPos (-x))
				  else showPos x

readFloat     :: RealFloat a => ReadS a
readFloat r    = [(fromRational ((n%1)*10^^(k-d)),t) | (n,d,s) <- readFix r,
						       (k,t)   <- readExp s]
		 where readFix r = [(read (ds++ds'), length ds', t)
					| (ds, s) <- lexDigits r
                                        , (ds',t) <- lexFrac s   ]

                       lexFrac ('.':s) = lexDigits s
		       lexFrac s       = [("",s)]

		       readExp (e:s) | e `elem` "eE" = readExp' s
		       readExp s                     = [(0,s)]

		       readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
		       readExp' ('+':s) = readDec s
		       readExp' s       = readDec s


-- Hooks for primitives: -----------------------------------------------------
-- Do not mess with these!

primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

primPmInt        :: Num a => Int -> a -> Bool
primPmInt n x     = fromInt n == x

primPmInteger    :: Num a => Integer -> a -> Bool
primPmInteger n x = fromInteger n == x

primPmFlt        :: Fractional a => Double -> a -> Bool
primPmFlt n x     = fromDouble n == x

-- ToDo: make the message more informative.
primPmFail       :: a
primPmFail        = error "Pattern Match Failure"

-- used in desugaring Foreign functions
primMkIO :: (RealWorld -> (a,RealWorld)) -> IO a
primMkIO = ST

primCreateAdjThunk :: (a -> b) -> String -> Char -> IO Addr
primCreateAdjThunk fun typestr callconv
   = do sp <- makeStablePtr fun
        p  <- copy_String_to_cstring typestr  -- is never freed
        a  <- primCreateAdjThunkARCH sp p callconv
        return a

-- The following primitives are only needed if (n+k) patterns are enabled:
primPmNpk        :: Integral a => Int -> a -> Maybe a
primPmNpk n x     = if n'<=x then Just (x-n') else Nothing
		    where n' = fromInt n

primPmSub        :: Integral a => Int -> a -> a
primPmSub n x     = x - fromInt n

-- Unpack strings generated by the Hugs code generator.
-- Strings can contain \0 provided they're coded right.
-- 
-- ToDo: change this (and Hugs code generator) to use ByteArrays

primUnpackString :: Addr -> String
primUnpackString a = unpack 0
 where
  -- The following decoding is based on evalString in the old machine.c
  unpack i
    | c == '\0' = []
    | c == '\\' = if '\\' == primIndexCharOffAddr a (i+1)
                  then '\\' : unpack (i+2)
                  else '\0' : unpack (i+2)
    | otherwise = c : unpack (i+1)
   where
    c = primIndexCharOffAddr a i


-- Monadic I/O: --------------------------------------------------------------

type FilePath = String

--data IOError = ...
--instance Eq IOError ...
--instance Show IOError ...

data IOError = IOError String
instance Show IOError where
   showsPrec _ (IOError s) = showString ("I/O error: " ++ s)

ioError :: IOError -> IO a
ioError (IOError s) = primRaise (IOExcept s)

userError :: String -> IOError
userError s = primRaise (ErrorCall s)

catch :: IO a -> (IOError -> IO a) -> IO a
catch m k 
  = ST (\s -> unST m s `primCatch` \ err -> unST (k (e2ioe err)) s)
    where
       e2ioe (IOExcept s) = IOError s
       e2ioe other        = IOError (show other)

putChar :: Char -> IO ()
putChar c = nh_stdout >>= \h -> nh_write h (primCharToInt c)

putStr :: String -> IO ()
putStr s = --mapM_ putChar s -- correct, but slow
           nh_stdout >>= \h -> 
           let loop []     = return ()
               loop (c:cs) = nh_write h (primCharToInt c) >> loop cs
           in  loop s

putStrLn :: String -> IO ()
putStrLn s = do { putStr s; putChar '\n' }

print :: Show a => a -> IO ()
print = putStrLn . show

getChar :: IO Char
getChar = unsafeInterleaveIO (
          nh_stdin  >>= \h -> 
          nh_read h >>= \ci -> 
          return (primIntToChar ci)
          )

getLine :: IO String
getLine    = do c <- getChar
		if c=='\n' then return ""
			   else do cs <- getLine
				   return (c:cs)

getContents :: IO String
getContents = nh_stdin >>= \h -> readfromhandle h

interact  :: (String -> String) -> IO ()
interact f = getContents >>= (putStr . f)

readFile :: FilePath -> IO String
readFile fname
   = copy_String_to_cstring fname  >>= \ptr ->
     nh_open ptr 0                 >>= \h ->
     nh_free ptr                   >>
     nh_errno                      >>= \errno ->
     if   (h == 0 || errno /= 0)
     then (ioError.IOError) ("readFile: can't open file " ++ fname)
     else readfromhandle h

writeFile :: FilePath -> String -> IO ()
writeFile fname contents
   = copy_String_to_cstring fname  >>= \ptr ->
     nh_open ptr 1                 >>= \h ->
     nh_free ptr                   >>
     nh_errno                      >>= \errno ->
     if   (h == 0 || errno /= 0)
     then (ioError.IOError) ("writeFile: can't create file " ++ fname)
     else writetohandle fname h contents

appendFile :: FilePath -> String -> IO ()
appendFile fname contents
   = copy_String_to_cstring fname  >>= \ptr ->
     nh_open ptr 2                 >>= \h ->
     nh_free ptr                   >>
     nh_errno                      >>= \errno ->
     if   (h == 0 || errno /= 0)
     then (ioError.IOError) ("appendFile: can't open file " ++ fname)
     else writetohandle fname h contents


-- raises an exception instead of an error
readIO          :: Read a => String -> IO a
readIO s         = case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> return x
                        []  -> ioError (userError "PreludeIO.readIO: no parse")
                        _   -> ioError (userError 
                                       "PreludeIO.readIO: ambiguous parse")

readLn          :: Read a => IO a
readLn           = do l <- getLine
                      r <- readIO l
                      return r


-- End of Hugs standard prelude ----------------------------------------------

data Exception 
   = ErrorCall String
   | IOExcept  String 

instance Show Exception where
   showsPrec _ (ErrorCall s) = showString ("error: " ++ s)
   showsPrec _ (IOExcept s)  = showString ("I/O error: " ++ s)

data IOResult  = IOResult  deriving (Show)

type FILE_STAR = Int   -- FILE *

foreign import "nHandle" "nh_stdin"  nh_stdin  :: IO FILE_STAR
foreign import "nHandle" "nh_stdout" nh_stdout :: IO FILE_STAR
foreign import "nHandle" "nh_stderr" nh_stderr :: IO FILE_STAR
foreign import "nHandle" "nh_write"  nh_write  :: FILE_STAR -> Int -> IO ()
foreign import "nHandle" "nh_read"   nh_read   :: FILE_STAR -> IO Int
foreign import "nHandle" "nh_open"   nh_open   :: Addr -> Int -> IO FILE_STAR
foreign import "nHandle" "nh_flush"  nh_flush  :: FILE_STAR -> IO ()
foreign import "nHandle" "nh_close"  nh_close  :: FILE_STAR -> IO ()
foreign import "nHandle" "nh_errno"  nh_errno  :: IO Int

foreign import "nHandle" "nh_malloc" nh_malloc :: Int -> IO Addr
foreign import "nHandle" "nh_free"   nh_free   :: Addr -> IO ()
foreign import "nHandle" "nh_store"  nh_store  :: Addr -> Int -> IO ()
foreign import "nHandle" "nh_load"   nh_load   :: Addr -> IO Int

--foreign import "nHandle" "nh_argc"   nh_argc   :: IO Int
--foreign import "nHandle" "nh_argvb"  nh_argvb  :: Int -> Int -> IO Int
foreign import "nHandle" "nh_getenv" nh_getenv :: Addr -> IO Addr

copy_String_to_cstring :: String -> IO Addr
copy_String_to_cstring s
   = nh_malloc (1 + length s) >>= \ptr0 -> 
     let loop ptr []     = nh_store ptr 0 >> return ptr0
         loop ptr (c:cs) = nh_store ptr (primCharToInt c) >> loop (incAddr ptr) cs
     in
         if   isNullAddr ptr0
         then error "copy_String_to_cstring: malloc failed"
         else loop ptr0 s

copy_cstring_to_String :: Addr -> IO String
copy_cstring_to_String ptr
   = nh_load ptr >>= \ci ->
     if   ci == 0 
     then return []
     else copy_cstring_to_String (incAddr ptr) >>= \cs -> 
          return ((primIntToChar ci) : cs)

readfromhandle :: FILE_STAR -> IO String
readfromhandle h
   = unsafeInterleaveIO (
     nh_read h >>= \ci ->
     if ci == -1 {-EOF-} then return "" else
     readfromhandle h >>= \restOfFile -> return ((primIntToChar ci) : restOfFile)
     )

writetohandle :: String -> FILE_STAR -> String -> IO ()
writetohandle fname h []
   = nh_close h                  >>
     nh_errno                    >>= \errno ->
     if   errno == 0 
     then return ()
     else error ( "writeFile/appendFile: error closing file " ++ fname)
writetohandle fname h (c:cs)
   = nh_write h (primCharToInt c) >> 
     writetohandle fname h cs

primGetRawArgs :: IO [String]
primGetRawArgs
   = primGetArgc >>= \argc ->
     accumulate (map get_one_arg [0 .. argc-1])
     where
        get_one_arg :: Int -> IO String
        get_one_arg argno
           = primGetArgv argno >>= \a ->
             copy_cstring_to_String a

primGetEnv :: String -> IO String
primGetEnv v
   = copy_String_to_cstring v     >>= \ptr ->
     nh_getenv ptr                >>= \ptr2 ->
     nh_free ptr                  >>
     if   isNullAddr ptr2
     then return []
     else
     copy_cstring_to_String ptr2  >>= \result ->
     return result


------------------------------------------------------------------------------
-- ST, IO --------------------------------------------------------------------
------------------------------------------------------------------------------

newtype ST s a = ST (s -> (a,s))

data RealWorld
type IO a = ST RealWorld a


--primRunST :: (forall s. ST s a) -> a
primRunST :: ST RealWorld a -> a
primRunST m = fst (unST m theWorld)
   where
      theWorld :: RealWorld
      theWorld = error "primRunST: entered the RealWorld"

unST (ST a) = a

instance Functor (ST s) where
   fmap f x  = x >>= (return . f)

instance Monad (ST s) where
   m >> k    = ST (\s -> case unST m s of { (a,s') -> unST k s' })
   return x  = ST (\s -> (x,s))
   m >>= k   = ST (\s -> case unST m s of { (a,s') -> unST (k a) s' })


-- used when Hugs invokes top level function
primRunIO :: IO () -> ()
primRunIO m
   = protect (fst (unST m realWorld))
     where
        realWorld = error "primRunIO: entered the RealWorld"
        protect :: () -> ()
        protect comp 
           = primCatch comp (\e -> fst (unST (putStr (show e ++ "\n")) realWorld))

trace :: String -> a -> a
trace s x
   = (primRunST (putStr ("trace: " ++ s ++ "\n"))) `seq` x

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST m = ST (\ s -> (fst (unST m s), s))

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = unsafeInterleaveST


------------------------------------------------------------------------------
-- Word, Addr, StablePtr, Prim*Array -----------------------------------------
------------------------------------------------------------------------------

data Addr

nullAddr     =  primIntToAddr 0
incAddr a    =  primIntToAddr (1 + primAddrToInt a)
isNullAddr a =  0 == primAddrToInt a

instance Eq Addr where 
  (==)            = primEqAddr
  (/=)            = primNeAddr
                  
instance Ord Addr where 
  (<)             = primLtAddr
  (<=)            = primLeAddr
  (>=)            = primGeAddr
  (>)             = primGtAddr


data Word

instance Eq Word where 
  (==)            = primEqWord
  (/=)            = primNeWord
                  
instance Ord Word where 
  (<)             = primLtWord
  (<=)            = primLeWord
  (>=)            = primGeWord
  (>)             = primGtWord


data StablePtr a

makeStablePtr   :: a -> IO (StablePtr a)
makeStablePtr    = primMakeStablePtr
deRefStablePtr  :: StablePtr a -> IO a
deRefStablePtr   = primDeRefStablePtr
freeStablePtr   :: StablePtr a -> IO ()
freeStablePtr    = primFreeStablePtr


data PrimArray              a -- immutable arrays with Int indices
data PrimByteArray

data Ref                  s a -- mutable variables
data PrimMutableArray     s a -- mutable arrays with Int indices
data PrimMutableByteArray s



-- showFloat ------------------------------------------------------------------

showEFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFloat      :: (RealFloat a) => a -> ShowS

showEFloat d x =  showString (formatRealFloat FFExponent d x)
showFFloat d x =  showString (formatRealFloat FFFixed d x)
showGFloat d x =  showString (formatRealFloat FFGeneric d x)
showFloat      =  showGFloat Nothing 

-- These are the format types.  This type is not exported.

data FFFormat = FFExponent | FFFixed | FFGeneric

formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x = s
  where base = 10
        s = if isNaN x then 
                "NaN"
            else if isInfinite x then 
                if x < 0 then "-Infinity" else "Infinity"
            else if x < 0 || isNegativeZero x then 
                '-' : doFmt fmt (floatToDigits (toInteger base) (-x))
            else 
                doFmt fmt (floatToDigits (toInteger base) x)
        doFmt fmt (is, e) =
            let ds = map intToDigit is
            in  case fmt of
                FFGeneric ->
                    doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
                          (is, e)
                FFExponent ->
                    case decs of
                    Nothing ->
                        case ds of
                         ['0'] -> "0.0e0"
                         [d]   -> d : ".0e" ++ show (e-1)
                         d:ds  -> d : '.' : ds ++ 'e':show (e-1)
                    Just dec ->
                        let dec' = max dec 1 in
                        case is of
                         [0] -> '0':'.':take dec' (repeat '0') ++ "e0"
                         _ ->
                          let (ei, is') = roundTo base (dec'+1) is
                              d:ds = map intToDigit
                                         (if ei > 0 then init is' else is')
                          in d:'.':ds  ++ "e" ++ show (e-1+ei)
                FFFixed ->
                    case decs of
                    Nothing ->
                        let f 0 s ds = mk0 s ++ "." ++ mk0 ds
                            f n s "" = f (n-1) (s++"0") ""
                            f n s (d:ds) = f (n-1) (s++[d]) ds
                            mk0 "" = "0"
                            mk0 s = s
                        in  f e "" ds
                    Just dec ->
                        let dec' = max dec 0 in
                        if e >= 0 then
                            let (ei, is') = roundTo base (dec' + e) is
                                (ls, rs) = splitAt (e+ei) (map intToDigit is')
                            in  (if null ls then "0" else ls) ++ 
                                (if null rs then "" else '.' : rs)
                        else
                            let (ei, is') = roundTo base dec'
                                              (replicate (-e) 0 ++ is)
                                d : ds = map intToDigit
                                            (if ei > 0 then is' else 0:is')
                            in  d : '.' : ds

roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base d is = case f d is of
                (0, is) -> (0, is)
                (1, is) -> (1, 1 : is)
  where b2 = base `div` 2
        f n [] = (0, replicate n 0)
        f 0 (i:_) = (if i >= b2 then 1 else 0, [])
        f d (i:is) = 
            let (c, ds) = f (d-1) is
                i' = c + i
            in  if i' == base then (1, 0:ds) else (0, i':ds)

-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R. K. Dybvig, in PLDI 96.
-- This version uses a much slower logarithm estimator.  It should be improved.

-- This function returns a list of digits (Ints in [0..base-1]) and an
-- exponent.

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)

floatToDigits _ 0 = ([0], 0)
floatToDigits base x =
    let (f0, e0) = decodeFloat x
        (minExp0, _) = floatRange x
        p = floatDigits x
        b = floatRadix x
        minExp = minExp0 - p            -- the real minimum exponent
        -- Haskell requires that f be adjusted so denormalized numbers
        -- will have an impossibly low exponent.  Adjust for this.
        (f, e) = let n = minExp - e0
                 in  if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)

        (r, s, mUp, mDn) =
           if e >= 0 then
               let be = b^e in
               if f == b^(p-1) then
                   (f*be*b*2, 2*b, be*b, b)
               else
                   (f*be*2, 2, be, be)
           else
               if e > minExp && f == b^(p-1) then
                   (f*b*2, b^(-e+1)*2, b, 1)
               else
                   (f*2, b^(-e)*2, 1, 1)
        k = 
            let k0 =
                    if b == 2 && base == 10 then
                         -- logBase 10 2 is slightly bigger than 3/10 so
                         -- the following will err on the low side.  Ignoring
                         -- the fraction will make it err even more.
                         -- Haskell promises that p-1 <= logBase b f < p.
                         (p - 1 + e0) * 3 `div` 10
                    else
                         ceiling ((log (fromInteger (f+1)) +
                                  fromInt e * log (fromInteger b)) /
                                   log (fromInteger base))
                fixup n =
                    if n >= 0 then
                        if r + mUp <= expt base n * s then n else fixup (n+1)
                    else
                        if expt base (-n) * (r + mUp) <= s then n
                                                           else fixup (n+1)
            in  fixup k0

        gen ds rn sN mUpN mDnN =
            let (dn, rn') = (rn * base) `divMod` sN
                mUpN' = mUpN * base
                mDnN' = mDnN * base
            in  case (rn' < mDnN', rn' + mUpN' > sN) of
                (True,  False) -> dn : ds
                (False, True)  -> dn+1 : ds
                (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
                (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'
        rds =
            if k >= 0 then
                gen [] r (s * expt base k) mUp mDn
            else
                let bk = expt base (-k)
                in  gen [] (r * bk) s (mUp * bk) (mDn * bk)
    in  (map toInt (reverse rds), k)


-- Exponentiation with a cache for the most common numbers.
minExpt = 0::Int
maxExpt = 1100::Int
expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts !! (n-minExpt)
    else
        base^n

expts :: [Integer]
expts = [2^n | n <- [minExpt .. maxExpt]]

