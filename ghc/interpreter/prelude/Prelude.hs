#include "options.h"

#if BIGNUM_IS_INT64
#define primToBignum(t)   prim/**/t/**/ToInt64
#define primFromBignum(t) primInt64To/**/t
#define primInt64ToInt64 id
#define	primEncodeFloat primEncodeFloatz
#define	primDecodeFloat	primDecodeFloatz
#define	primEncodeDouble primEncodeDoublez
#define	primDecodeDouble primDecodeDoublez
#elif BIGNUM_IS_INTEGER
#define primToBignum(t)   prim/**/t/**/ToInteger
#define primFromBignum(t) primIntegerTo/**/t
#define primIntegerToInteger id
#define	primEncodeFloat primEncodeFloatZ
#define	primDecodeFloat	primDecodeFloatZ
#define	primEncodeDouble primEncodeDoubleZ
#define	primDecodeDouble primDecodeDoubleZ
#else
#warning No BIGNUM type
#endif

#ifdef HEAD
module Prelude (
    module PreludeList, module PreludeText, module PreludeIO,
    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, 
#ifdef PROVIDE_INTEGER
    Integer,
#endif
    Float, Double, IO, 
#if STD_PRELUDE
#else
    Void,
#endif
    Ratio, Rational, 
#if STD_PRELUDE
--  List type: []((:), [])
#else
    (:),
#endif
--  Tuple types: (,), (,,), etc.
--  Trivial type: ()
--  Functions: (->)
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
#if EVAL_INSTANCES
    Eval(seq, strict),
#else
    seq, strict,
#endif
    Num((+), (-), (*), negate, abs, signum, fromInteger),
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
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, fromRealFrac, atan2,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined ) where

import PreludeBuiltin  -- Contains all `prim' values
import PreludeList
import PreludeText
import PreludeIO
import Ratio(Ratio, Rational, (%), numerator, denominator)

#endif /* HEAD */
#ifdef BODY
module PreludeBuiltin 
	( module PreludeBuiltin
	) where

#if STD_PRELUDE
import PreludeBuiltin  -- Contains all `prim' values
import PreludeList
import PreludeText
import PreludeIO
import Ratio(Ratio, Rational, (%), numerator, denominator)
#endif

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 0  $, `seq`

#if STD_PRELUDE
#else
-- Fixities from List
infix  5  \\
-- Fixities from PreludeList
infixl 9  !!
infix  4 `elem`, `notElem`
-- Fixities from Ratio (why do I have the :% fixity??)
infixl 7  %, :%
-- Fixities from Array
infixl 9  !, //

#include "PreludeList.hs"
#include "PreludeText.hs"
#include "PreludeIO.hs"
#include "Ratio.hs"
#include "Ix.hs"
#include "Char.hs"
#include "Numeric.hs"
#include "Array.hs"
#include "List.hs"
#include "Maybe.hs"
#include "UnicodePrims.hs"
#include "PreludePackString.hs"
#include "PrelConc.hs"

-- The following bits of GHC are too good to pass up!
#include "PrelIOBase.unlit"
#include "PrelHandle.unlit"
#include "PrelException.unlit"
#include "PrelDynamic.unlit"
#include "IO.unlit"
#endif

-- Standard types, classes, instances and related functions

-- Equality and Ordered classes

class  Eq a  where
    (==), (/=)       :: a -> a -> Bool

    x /= y           =  not (x == y)
    x == y           =  not (x /= y)

class  (Eq a) => Ord a  where
    compare          :: a -> a -> Ordering
    (<), (<=),
     (>=), (>)       :: a -> a -> Bool
    max, min         :: a -> a -> a

-- An instance of Ord should define either compare or <=
-- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y 
         | x >= y    =  x
         | otherwise =  y
    min x y
         | x <  y    =  x
         | otherwise =  y

-- Enumeration and Bounded classes

class  Enum a  where
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x y z = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]

succ, pred           :: Enum a => a -> a
succ                 =  toEnum . (+1) . fromEnum
pred                 =  toEnum . (subtract 1) . fromEnum

class  Bounded a  where
    minBound         :: a
    maxBound         :: a

-- Numeric classes

#if EVAL_INSTANCES
class  (Eq a, Show a, Eval a) => Num a  where
#else
class  (Eq a, Show a) => Num a  where
#endif
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: BIGNUMTYPE -> a
#if STD_PRELUDE
#else
    fromInt          :: Int -> a
    fromInt          =  fromInteger . primToBignum(Int)
#endif

    x - y            =  x + negate y

class  (Num a, Ord a) => Real a  where
    toRational       :: a -> Rational
#if STD_PRELUDE
#else
    toDouble         :: a -> Double
    toDouble         =  rationalToRealFloat . toRational
#endif

class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a   
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> BIGNUMTYPE
#if STD_PRELUDE	     
#else		     
    toInt            :: a -> Int
    toInt            =  fromInteger . toInteger
#endif

    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  if signum r == - signum d then (q-1, r+d) else qr
                        where qr@(q,r) = quotRem n d

class  (Num a) => Fractional a  where
    (/)              :: a -> a -> a
    recip            :: a -> a
    fromRational     :: Rational -> a
#if STD_PRELUDE	     
#else		     
    fromDouble       :: Double -> a
    fromDouble       =  fromRational . realFloatToRational
#endif		     

    recip x          =  1 / x

class  (Fractional a) => Floating a  where
    pi               :: a
    exp, log, sqrt   :: a -> a
    (**), logBase    :: a -> a -> a
    sin, cos, tan    :: a -> a
    asin, acos, atan :: a -> a
    sinh, cosh, tanh :: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    sqrt x           =  x ** 0.5
    tan  x           =  sin  x / cos  x
    tanh x           =  sinh x / cosh x

class  (Real a, Fractional a) => RealFrac a  where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

    truncate x       =  m  where (m,_) = properFraction x
    
    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m
    
    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x
    
    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix       :: a -> BIGNUMTYPE
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (BIGNUMTYPE,Int)
    encodeFloat      :: BIGNUMTYPE -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool

    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

-- Numeric functions

subtract         :: (Num a) => a -> a -> a
subtract         =  flip (-)

even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

gcd              :: (Integral a) => a -> a -> a
gcd 0 0          =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y          =  gcd' (abs x) (abs y)
                    where gcd' x 0  =  x
                          gcd' x y  =  gcd' y (x `rem` y)

lcm              :: (Integral a) => a -> a -> a
lcm _ 0          =  0
lcm 0 _          =  0
lcm x y          =  abs ((x `quot` (gcd x y)) * y)

(^)              :: (Num a, Integral b) => a -> b -> a
x ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n  where
                                    g x n | even n  = g (x*x) (n `quot` 2)
                                          | otherwise = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"

(^^)             :: (Fractional a, Integral b) => a -> b -> a
x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))

fromIntegral     :: (Integral a, Num b) => a -> b
fromIntegral     =  fromInteger . toInteger

fromRealFrac     :: (RealFrac a, Fractional b) => a -> b
fromRealFrac     =  fromRational . toRational

atan2            :: (RealFloat a) => a -> a -> a
atan2 y x        =  case (signum y, signum x) of
                         ( 0, 1) ->  0
                         ( 1, 0) ->  pi/2
                         ( 0,-1) ->  pi
                         (-1, 0) -> -pi/2
                         ( _, 1) ->  atan (y/x)
                         ( _,-1) ->  atan (y/x) + pi
                         ( 0, 0) ->  error "Prelude.atan2: atan2 of origin"


-- Monadic classes

class  Functor f  where
    map              :: (a -> b) -> f a -> f b

class  Monad m  where
    (>>=)            :: m a -> (a -> m b) -> m b
    (>>)             :: m a -> m b -> m b
    return           :: a -> m a

    m >> k           =  m >>= \_ -> k

class  (Monad m) => MonadZero m  where
    zero             :: m a

class  (MonadZero m) => MonadPlus m  where
    (++)             :: m a -> m a -> m a

accumulate       :: Monad m => [m a] -> m [a] 
accumulate       =  foldr mcons (return [])
                    where mcons p q = p >>= \x -> q >>= \y -> return (x:y)

sequence         :: Monad m => [m a] -> m () 
sequence         =  foldr (>>) (return ())

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as        =  accumulate (map f as)

mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as       =  sequence (map f as)

guard            :: MonadZero m => Bool -> m ()
guard p          =  if p then return () else zero

-- This subsumes the list-based filter function.

filter           :: MonadZero m => (a -> Bool) -> m a -> m a
filter p         =  applyM (\x -> if p x then return x else zero)

-- This subsumes the list-based concat function.

concat           :: MonadPlus m => [m a] -> m a
concat           =  foldr (++) zero
 
applyM           :: Monad m => (a -> m b) -> m a -> m b
applyM f x       =  x >>= f

#if EVAL_INSTANCES
-- Eval Class

class  Eval a  where
    seq              :: a -> b -> b
    strict           :: (a -> b) -> a -> b

    seq x y          =  case primForce x of () -> y
    strict f x       =  case primForce x of () -> f x

#else

seq              :: a -> b -> b
strict           :: (a -> b) -> a -> b

seq x y          =  case primForce x of () -> y
strict f x       =  case primForce x of () -> f x

#endif

-- Trivial type

#if STD_PRELUDE
data  ()  =  ()  deriving (Eq, Ord, Enum, Bounded)
#else
data  () => ()  =  ()  deriving (Eq, Ord, Enum, Bounded)
#endif

-- Function type

#if STD_PRELUDE
data a -> b  -- No constructor for functions is exported.
#endif

-- identity function
id               :: a -> a
id x             =  x

-- constant function
const            :: a -> b -> a
const x _        =  x

-- function composition
(.)              :: (b -> c) -> (a -> b) -> a -> c
f . g            =  \ x -> f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.
flip             :: (a -> b -> c) -> b -> a -> c
flip f x y       =  f y x

-- right-associating infix application operator (useful in continuation-
-- passing style)
($)              :: (a -> b) -> a -> b
f $ x            =  f x

#if STD_PRELUDE
#else
-- Empty type

data Void      -- No constructor for Void is exported.  Import/Export
               -- lists must use Void instead of Void(..) or Void()
#endif

-- Boolean type

data  Bool  =  False | True     deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- Boolean functions

(&&), (||)       :: Bool -> Bool -> Bool
True  && x       =  x
False && _       =  False
True  || _       =  True
False || x       =  x
		  			
not              :: Bool -> Bool
not True         =  False
not False        =  True

otherwise        :: Bool
otherwise        =  True


-- Character type

#if STD_PRELUDE
data Char = ... 'a' | 'b' ... -- 2^16 unicode values
#else
data Char
#endif

instance  Eq Char  where
    c == c'          =  fromEnum c == fromEnum c'
#if STD_PRELUDE
#else
--#warning "Could use primEqChar and primNeChar"
#endif

instance  Ord Char  where
    c <= c'          =  fromEnum c <= fromEnum c'
#if STD_PRELUDE
#else
--#warning "Could use primLeChar and friends"
#endif

instance  Enum Char  where
    toEnum           =  primIntToChar
    fromEnum         =  primCharToInt
    enumFrom c       =  map toEnum [fromEnum c .. fromEnum (maxBound::Char)]
    enumFromThen c c' =  map toEnum [fromEnum c,
                                     fromEnum c' .. fromEnum lastChar]
                         where lastChar :: Char
                               lastChar | c' < c    = minBound
                                        | otherwise = maxBound

instance  Bounded Char  where
    minBound            =  '\0'
#if STD_PRELUDE
    maxBound            =  '\xffff'
#else
--#warning "literal char constants too small"
    maxBound            =  '\xff'
#endif

type  String = [Char]


-- Maybe type

data  Maybe a  =  Nothing | Just a      deriving (Eq, Ord, Read, Show)

maybe              :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  =  n
maybe n f (Just x) =  f x

instance  Functor Maybe  where
    map f Nothing    =  Nothing
    map f (Just x)   =  Just (f x)

instance  Monad Maybe  where
    (Just x) >>= k   =  k x
    Nothing  >>= k   =  Nothing
    return           =  Just

instance  MonadZero Maybe  where
    zero             = Nothing

instance  MonadPlus Maybe  where
    Nothing ++ ys    =  ys
    xs      ++ ys    =  xs

-- Either type

data  Either a b  =  Left a | Right b   deriving (Eq, Ord, Read, Show)

either               :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  =  f x
either f g (Right y) =  g y

-- IO type

#if STD_PRELUDE
data  IO a  -- abstract

instance  Functor IO where
   map f x           =  x >>= (return . f)

instance  Monad IO  where ...
#else
newtype ST s a = ST (s -> (a,s))

runST :: (forall s. ST s a) -> a
runST m = fst (unST m theWorld)
 where
  theWorld :: RealWorld
  theWorld = error "runST: entered the world"

unST (ST a) = a

instance  Functor (ST s) where
   map f x = x >>= (return . f)

instance  Monad (ST s) where
    m >> k      =  m >>= \ _ -> k
    return x    =  ST $ \ s -> (x,s)
    m >>= k = ST $ \s -> case unST m s of { (a,s') -> unST (k a) s' }

fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let
        result = unST (k (fst result)) s
    in
    result

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST m = ST (\ s -> (fst (unST m s), s))

fixIO :: (a -> IO a) -> IO a
fixIO = fixST

unsafePerformIO :: IO a -> a
unsafePerformIO m = fst (unST m realWorld)
 where
  realWorld :: RealWorld
  realWorld = error "panic: Hugs shouldnae enter the real world"

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = unsafeInterleaveST

-- This is one of the main uses of unsafeInterleaveIO
mkLazyList :: IO (Maybe a) -> IO [a]
mkLazyList m = unsafeInterleaveIO $ do
	     mx  <- m
             case mx of
             Nothing -> return []
	     Just x  -> do
               xs <- mkLazyList m
	       return (x:xs)

-- used in desugaring Foreign functions
primMkIO :: (RealWorld -> (a,RealWorld)) -> IO a
primMkIO = ST

-- used when Hugs invokes top level function
primRunIO :: IO () -> ()
primRunIO m = fst (unST (protect 5 m) realWorld)
 where
  realWorld :: RealWorld
  realWorld = error "panic: Hugs shouldnae enter the real world"

  -- make sure there's always an error handler on the stack
  protect :: Int -> IO () -> IO ()
  protect 0     m = putStr "\nProgram error: too many nested errors\n"
  protect (n+1) m = m `catchException` \ e -> protect n (putStr "\nProgram error: " >> print e)

data RealWorld -- no constructors
type IO a = ST RealWorld a
#endif

-- Ordering type

data  Ordering  =  LT | EQ | GT
          deriving (Eq, Ord, Enum, Read, Show, Bounded)


-- Standard numeric types.  The data declarations for these types cannot
-- be expressed directly in Haskell since the constructor lists would be
-- far too large.

#if STD_PRELUDE
data  Int  =  minBound ... -1 | 0 | 1 ... maxBound
instance  Eq       Int  where ...
instance  Ord      Int  where ...
instance  Num      Int  where ...
instance  Real     Int  where ...
instance  Integral Int  where ...
instance  Enum     Int  where ...
instance  Bounded  Int  where ...
#else
data  Int

instance Eq  Int     where 
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
    fromInteger   = primFromBignum(Int)
    fromInt       = id

instance Real Int where
    toRational x  = toInteger x % 1

instance Integral Int where
    quotRem       = primQuotRemInt
    toInteger     = primToBignum(Int)
    toInt x       = x

instance Enum Int where
    toEnum        = id
    fromEnum      = id
    enumFrom      = numericEnumFrom
    enumFromThen  = numericEnumFromThen
    enumFromTo    = numericEnumFromTo
    enumFromThenTo= numericEnumFromThenTo

instance Bounded Int where
    minBound      = primMinInt
    maxBound      = primMaxInt
#endif

#ifdef PROVIDE_WORD
data  Word

instance Eq  Word     where 
  (==)            = primEqWord
  (/=)            = primNeWord
                  
instance Ord Word     where 
  (<)             = primLtWord
  (<=)            = primLeWord
  (>=)            = primGeWord
  (>)             = primGtWord

--and     = primAndWord
--or      = primOrWord
--not     = primNotWord
--shiftL  = primShiftL
--shiftRA = primShiftRA
--shiftRL = primShiftRL
--toInt   = primWord2Int
--fromInt = primInt2Word
#endif

#ifdef PROVIDE_ADDR
data  Addr

nullAddr = primIntToAddr 0

instance Eq  Addr     where 
  (==)            = primEqAddr
  (/=)            = primNeAddr
                  
instance Ord Addr     where 
  (<)             = primLtAddr
  (<=)            = primLeAddr
  (>=)            = primGeAddr
  (>)             = primGtAddr

--toInt   = addr2Int
--fromInt = int2Addr
#endif

#if STD_PRELUDE
data  Integer  =  ... -1 | 0 | 1 ...
instance  Eq       Integer  where ...
instance  Ord      Integer  where ...
instance  Num      Integer  where ...
instance  Real     Integer  where ...
instance  Integral Integer  where ...
instance  Enum     Integer  where ...
#else
#ifdef PROVIDE_INTEGER
data  Integer

instance Eq  Integer     where 
    (==) x y      = primCompareInteger x y == 0

instance Ord Integer     where 
    compare x y   = case primCompareInteger x y of
                    -1 -> LT
                    0  -> EQ
                    1  -> GT

instance Num Integer where
    (+)           = primPlusInteger
    (-)           = primMinusInteger
    negate        = primNegateInteger
    (*)           = primTimesInteger
    abs           = absReal
    signum        = signumReal
    fromInteger   = primFromBignum(Integer)
    fromInt       = primIntToInteger

instance Real Integer where
    toRational x  = toInteger x % 1

instance Integral Integer where
    quotRem       = primQuotRemInteger 
    divMod        = primDivModInteger 
    toInteger     = primToBignum(Integer)
    toInt         = primIntegerToInt

instance Enum Integer where
    toEnum        = primIntToInteger
    fromEnum      = primIntegerToInt
    enumFrom      = numericEnumFrom
    enumFromThen  = numericEnumFromThen
    enumFromTo    = numericEnumFromTo
    enumFromThenTo= numericEnumFromThenTo
#endif /* PROVIDE_INTEGER */
#endif

#ifdef PROVIDE_INT64
data  Int64

instance Eq  Int64     where 
    (==)          = primEqInt64
    (/=)          = primNeInt64

instance Ord Int64     where 
    (<)           = primLtInt64
    (<=)          = primLeInt64
    (>=)          = primGeInt64
    (>)           = primGtInt64
    compare x y
      | x `primLtInt64` y = LT
      | x `primEqInt64` y = EQ
      | otherwise         = GT

instance Num Int64 where
    (+)           = primPlusInt64
    (-)           = primMinusInt64
    negate        = primNegateInt64
    (*)           = primTimesInt64
    abs           = absReal
    signum        = signumReal
    fromInteger   = primFromBignum(Int64)
    fromInt       = primIntToInt64

instance Real Int64 where
    toRational x  = toInteger x % 1

instance Integral Int64 where
    quotRem       = primQuotRemInt64 
    toInteger     = primToBignum(Int64)
    toInt         = primInt64ToInt

instance Enum Int64 where
    toEnum        = primIntToInt64
    fromEnum      = primInt64ToInt
    enumFrom      = numericEnumFrom
    enumFromThen  = numericEnumFromThen
    enumFromTo    = numericEnumFromTo
    enumFromThenTo= numericEnumFromThenTo
#endif /* PROVIDE_INT64 */

#if STD_PRELUDE
#else
absReal x    | x >= 0    = x
             | otherwise = -x

signumReal x | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1
#endif

#if STD_PRELUDE
data  Float
instance  Eq         Float  where ...
instance  Ord        Float  where ...
instance  Num        Float  where ...
instance  Real       Float  where ...
instance  Fractional Float  where ...
instance  Floating   Float  where ...
instance  RealFrac   Float  where ...
instance  RealFloat  Float  where ...
#else
data  Float

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
    fromInteger   = primFromBignum(Float)
    fromInt       = primIntToFloat

instance Bounded Float where
    minBound      = primMinFloat
    maxBound      = primMaxFloat

instance Real Float where
    toRational    = realFloatToRational

instance Fractional Float where
    (/)           = primDivideFloat
    fromRational  = rationalToRealFloat
    fromDouble    = primDoubleToFloat

instance Floating Float where
    pi            = 3.14159265358979323846
    exp           = primExpFloat
    log           = primLogFloat
    sqrt          = primSqrtFloat
    sin           = primSinFloat
    cos           = primCosFloat
    tan           = primTanFloat
    asin          = primAsinFloat
    acos          = primAcosFloat
    atan          = primAtanFloat

instance RealFrac Float where
    properFraction = floatProperFraction

instance RealFloat Float where
    floatRadix  _ = toInteger primRadixFloat
    floatDigits _ = primDigitsFloat
    floatRange  _ = (primMinExpFloat,primMaxExpFloat)
    encodeFloat   = primEncodeFloat
    decodeFloat   = primDecodeFloat
    isNaN         = primIsNaNFloat
    isInfinite    = primIsInfiniteFloat    
    isDenormalized= primIsDenormalizedFloat
    isNegativeZero= primIsNegativeZeroFloat
    isIEEE        = const primIsIEEEFloat        
#endif

#if STD_PRELUDE
data  Double
instance  Eq         Double  where ...
instance  Ord        Double  where ...
instance  Num        Double  where ...
instance  Real       Double  where ...
instance  Fractional Double  where ...
instance  Floating   Double  where ...
instance  RealFrac   Double  where ...
instance  RealFloat  Double  where ...
#else
data  Double

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
    fromInteger  = primFromBignum(Double)
    fromInt      = primIntToDouble

instance Bounded Double where
    minBound     = primMinDouble
    maxBound     = primMaxDouble

instance Real Double where
    toRational   = realFloatToRational

realFloatToRational x = (m%1)*(b%1)^^n
                          where (m,n) = decodeFloat x
                                b     = floatRadix x

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

instance RealFrac Double where
    properFraction = floatProperFraction

floatProperFraction x
   | n >= 0      = (fromInteger m * fromInteger b ^ n, 0)
   | otherwise   = (fromInteger w, encodeFloat r n)
                     where (m,n) = decodeFloat x
                           b     = floatRadix x
                           (w,r) = quotRem m (b^(-n))

instance RealFloat Double where
    floatRadix  _ = toInteger primRadixDouble
    floatDigits _ = primDigitsDouble
    floatRange  _ = (primMinExpDouble,primMaxExpDouble)
    encodeFloat   = primEncodeDouble
    decodeFloat   = primDecodeDouble
    isNaN         = primIsNaNDouble
    isInfinite    = primIsInfiniteDouble    
    isDenormalized= primIsDenormalizedDouble
    isNegativeZero= primIsNegativeZeroDouble
    isIEEE        = const primIsIEEEDouble        
#endif

-- The Enum instances for Floats and Doubles are slightly unusual.
-- The `toEnum' function truncates numbers to Int.  The definitions
-- of enumFrom and enumFromThen allow floats to be used in arithmetic
-- series: [0,0.1 .. 1.0].  However, roundoff errors make these somewhat
-- dubious.  This example may have either 10 or 11 elements, depending on
-- how 0.1 is represented.

instance  Enum Float  where
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo

instance  Enum Double  where
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo

numericEnumFrom         :: (Real a) => a -> [a]
numericEnumFromThen     :: (Real a) => a -> a -> [a]
numericEnumFromTo       :: (Real a) => a -> a -> [a]
numericEnumFromThenTo   :: (Real a) => a -> a -> a -> [a]
numericEnumFrom         =  iterate (+1)
numericEnumFromThen n m =  iterate (+(m-n)) n
numericEnumFromTo n m   =  takeWhile (<= m) (numericEnumFrom n)
numericEnumFromThenTo n n' m
			=  takeWhile (if n' >= n then (<= m) else (>= m))
				     (numericEnumFromThen n n')


-- Lists

#if STD_PRELUDE
data  [a]  =  [] | a : [a]  deriving (Eq, Ord)
#else
data  () => [a]  =  [] | a : [a]  deriving (Eq, Ord)
#endif

instance Functor [] where
    map f []         =  []
    map f (x:xs)     =  f x : map f xs

instance  Monad []  where
    m >>= k          =  concat (map k m)
    return x         =  [x]

instance  MonadZero []  where
    zero             =  []

instance  MonadPlus []  where
    xs ++ ys         =  foldr (:) ys xs
    
-- Tuples

#if STD_PRELUDE
data  (a,b)   =  (a,b)    deriving (Eq, Ord, Bounded)
data  (a,b,c) =  (a,b,c)  deriving (Eq, Ord, Bounded)
#endif


-- component projections for pairs:
-- (NB: not provided for triples, quadruples, etc.)
fst              :: (a,b) -> a
fst (x,y)        =  x

snd              :: (a,b) -> b
snd (x,y)        =  y

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.
curry            :: ((a, b) -> c) -> a -> b -> c
curry f x y      =  f (x, y)

uncurry          :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p      =  f (fst p) (snd p)

-- Misc functions

-- until p f  yields the result of applying f until p holds.
until            :: (a -> Bool) -> (a -> a) -> a -> a
until p f x 
     | p x       =  x
     | otherwise =  until p f (f x)

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf         :: a -> a -> a
asTypeOf         =  const

-- error stops execution and displays an error message

#if STD_PRELUDE
error            :: String -> a
error            =  primError
#else
error            :: String -> a
error msg        =  primRaise (IOException (userError msg))
#endif

-- It is expected that compilers will recognize this and insert error
-- messages that are more appropriate to the context in which undefined 
-- appears. 

undefined        :: a
undefined        =  error "Prelude.undefined"

#if STD_PRELUDE
#else
--Missing primOps and magic funs

-- Used for pattern match failure.
-- ToDo: make the message more informative.
primPmFail :: a
primPmFail = error "Pattern Match Failure"

-- used in derived compare functions, must be exported from Prelude
primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

-- used in derived show functions, must be exported from Prelude
primShowField    :: Show a => String -> a -> ShowS
primShowField m v = showString m . showChar '=' . shows v

-- used in derived read functions, must be exported from Prelude
primReadField    :: Read a => String -> ReadS a
primReadField m s0 = [ r | (t,  s1) <- lex s0, t == m,
                           ("=",s2) <- lex s1,
                           r        <- readsPrec 10 s2 ]

-- These 4 primitives are used in pattern matching.
primPmInt :: Num a => Int -> a -> Bool
primPmInt x y = fromInt x == y

primPmInteger :: Num a => BIGNUMTYPE -> a -> Bool
primPmInteger x y = fromInteger x == y

primPmDouble :: Fractional a => Double -> a -> Bool
primPmDouble x y = fromDouble x == y

-- The following primitives are only needed if (n+k) patterns are enabled
-- The first two look trivial but they're selecting a method from a 
-- superclass of their argument...
primPmLe        :: Integral a => a -> a -> Bool
primPmLe x y     = x <= y

primPmSubtract   :: Integral a => a -> a -> a
primPmSubtract x y = x - y

primPmFromInteger :: Integral a => BIGNUMTYPE -> a
primPmFromInteger = fromInteger

primPmSub        :: Integral a => Int -> a -> a
primPmSub n x     = x - fromInt n

#ifdef PROVIDE_STABLE
data StablePtr a
#endif
#ifdef PROVIDE_FOREIGN
data ForeignObj

makeForeignObj :: Addr -> IO ForeignObj
makeForeignObj = primMakeForeignObj

#endif
#ifdef PROVIDE_WEAK
data Weak a

mkWeak  :: k				-- key
	-> v				-- value
	-> IO ()			-- finaliser
	-> IO (Weak v)			-- weak pointer

mkWeak k v f = primMakeWeak k v (unsafePerformIO f)

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak w = do
  { (stillThere,v) <- primDeRefWeak w
  -- Warning: you'd better ignore v unless stillThere is 1
  ; return (if stillThere == 0 then Nothing else Just v)
  }

mkWeakPtr :: k -> IO () -> IO (Weak k)
mkWeakPtr key finaliser = mkWeak key key finaliser

mkWeakPair :: k -> v -> IO () -> IO (Weak (k,v))
mkWeakPair key val finaliser = mkWeak key (key,val) finaliser

addFinaliser :: key -> IO () -> IO ()
addFinaliser key finaliser = do
   mkWeakPtr key finaliser		-- throw it away
   return ()

addForeignFinaliser :: ForeignObj -> IO () -> IO ()
addForeignFinaliser fo finaliser = addFinaliser fo finaliser

{-
finalise :: Weak v -> IO ()
finalise (Weak w) = finaliseWeak# w

instance Eq (Weak v) where
  (Weak w1) == (Weak w2) = w1 `sameWeak#` w2
-}

#endif

#endif
#endif /* BODY */
