/* -----------------------------------------------------------------------------
 * $Id: CTypes.h,v 1.1 2001/06/28 14:15:04 simonmar Exp $
 *
 * Dirty CPP hackery for CTypes/CTypesISO
 *
 * (c) The FFI task force, 2000
 * -------------------------------------------------------------------------- */

#include "MachDeps.h"

/* As long as there is no automatic derivation of classes for newtypes we resort
   to extremely dirty cpp-hackery.   :-P   Some care has to be taken when the
   macros below are modified, otherwise the layout rule will bite you. */

/* A hacked version for GHC follows the Haskell 98 version... */
#ifndef __GLASGOW_HASKELL__

#define NUMERIC_TYPE(T,C,S,B) \
newtype T = T B deriving (Eq, Ord) ; \
INSTANCE_NUM(T) ; \
INSTANCE_READ(T) ; \
INSTANCE_SHOW(T) ; \
INSTANCE_ENUM(T) ; \
INSTANCE_TYPEABLE(T,C,S) ;

#define INTEGRAL_TYPE(T,C,S,B) \
NUMERIC_TYPE(T,C,S,B) ; \
INSTANCE_BOUNDED(T) ; \
INSTANCE_REAL(T) ; \
INSTANCE_INTEGRAL(T) ; \
INSTANCE_BITS(T)

#define FLOATING_TYPE(T,C,S,B) \
NUMERIC_TYPE(T,C,S,B) ; \
INSTANCE_REAL(T) ; \
INSTANCE_FRACTIONAL(T) ; \
INSTANCE_FLOATING(T) ; \
INSTANCE_REALFRAC(T) ; \
INSTANCE_REALFLOAT(T)

#define INSTANCE_READ(T) \
instance Read T where { \
   readsPrec p s = fakeMap (\(x, t) -> (T x, t)) (readsPrec p s) }

#define INSTANCE_SHOW(T) \
instance Show T where { \
   showsPrec p (T x) = showsPrec p x }

#define INSTANCE_NUM(T) \
instance Num T where { \
   (T i) + (T j) = T (i + j) ; \
   (T i) - (T j) = T (i - j) ; \
   (T i) * (T j) = T (i * j) ; \
   negate  (T i) = T (negate i) ; \
   abs     (T i) = T (abs    i) ; \
   signum  (T i) = T (signum i) ; \
   fromInteger x = T (fromInteger x) }

#define INSTANCE_TYPEABLE(T,C,S) \
C :: TyCon ; \
C = mkTyCon S ; \
instance Typeable T where { \
  typeOf _ = mkAppTy C [] }

#define INSTANCE_BOUNDED(T) \
instance Bounded T where { \
   minBound = T minBound ; \
   maxBound = T maxBound }

#define INSTANCE_ENUM(T) \
instance Enum T where { \
   succ           (T i)             = T (succ i) ; \
   pred           (T i)             = T (pred i) ; \
   toEnum               x           = T (toEnum x) ; \
   fromEnum       (T i)             = fromEnum i ; \
   enumFrom       (T i)             = fakeMap T (enumFrom i) ; \
   enumFromThen   (T i) (T j)       = fakeMap T (enumFromThen i j) ; \
   enumFromTo     (T i) (T j)       = fakeMap T (enumFromTo i j) ; \
   enumFromThenTo (T i) (T j) (T k) = fakeMap T (enumFromThenTo i j k) }

#define INSTANCE_REAL(T) \
instance Real T where { \
   toRational (T i) = toRational i }

#define INSTANCE_INTEGRAL(T) \
instance Integral T where { \
   (T i) `quot`    (T j) = T (i `quot` j) ; \
   (T i) `rem`     (T j) = T (i `rem`  j) ; \
   (T i) `div`     (T j) = T (i `div`  j) ; \
   (T i) `mod`     (T j) = T (i `mod`  j) ; \
   (T i) `quotRem` (T j) = let (q,r) = i `quotRem` j in (T q, T r) ; \
   (T i) `divMod`  (T j) = let (d,m) = i `divMod`  j in (T d, T m) ; \
   toInteger (T i)       = toInteger i }

#define INSTANCE_BITS(T) \
instance Bits T where { \
  (T x) .&.     (T y)   = T (x .&.   y) ; \
  (T x) .|.     (T y)   = T (x .|.   y) ; \
  (T x) `xor`   (T y)   = T (x `xor` y) ; \
  complement    (T x)   = T (complement x) ; \
  shift         (T x) n = T (shift x n) ; \
  rotate        (T x) n = T (rotate x n) ; \
  bit                 n = T (bit n) ; \
  setBit        (T x) n = T (setBit x n) ; \
  clearBit      (T x) n = T (clearBit x n) ; \
  complementBit (T x) n = T (complementBit x n) ; \
  testBit       (T x) n = testBit x n ; \
  bitSize       (T x)   = bitSize x ; \
  isSigned      (T x)   = isSigned x }

#define INSTANCE_FRACTIONAL(T) \
instance Fractional T where { \
   (T x) / (T y)  = T (x / y) ; \
   recip   (T x)  = T (recip x) ; \
   fromRational	r = T (fromRational r) }

#define INSTANCE_FLOATING(T) \
instance Floating T where { \
   pi                    = pi ; \
   exp   (T x)           = T (exp   x) ; \
   log   (T x)           = T (log   x) ; \
   sqrt  (T x)           = T (sqrt  x) ; \
   (T x) **        (T y) = T (x ** y) ; \
   (T x) `logBase` (T y) = T (x `logBase` y) ; \
   sin   (T x)           = T (sin   x) ; \
   cos   (T x)           = T (cos   x) ; \
   tan   (T x)           = T (tan   x) ; \
   asin  (T x)           = T (asin  x) ; \
   acos  (T x)           = T (acos  x) ; \
   atan  (T x)           = T (atan  x) ; \
   sinh  (T x)           = T (sinh  x) ; \
   cosh  (T x)           = T (cosh  x) ; \
   tanh  (T x)           = T (tanh  x) ; \
   asinh (T x)           = T (asinh x) ; \
   acosh (T x)           = T (acosh x) ; \
   atanh (T x)           = T (atanh x) }

#define INSTANCE_REALFRAC(T) \
instance RealFrac T where { \
   properFraction (T x) = let (m,y) = properFraction x in (m, T y) ; \
   truncate (T x) = truncate x ; \
   round    (T x) = round x ; \
   ceiling  (T x) = ceiling x ; \
   floor    (T x) = floor x }

#define INSTANCE_REALFLOAT(T) \
instance RealFloat T where { \
   floatRadix     (T x) = floatRadix x ; \
   floatDigits    (T x) = floatDigits x ; \
   floatRange     (T x) = floatRange x ; \
   decodeFloat    (T x) = decodeFloat x ; \
   encodeFloat m n      = T (encodeFloat m n) ; \
   exponent       (T x) = exponent x ; \
   significand    (T x) = T (significand  x) ; \
   scaleFloat n   (T x) = T (scaleFloat n x) ; \
   isNaN          (T x) = isNaN x ; \
   isInfinite     (T x) = isInfinite x ; \
   isDenormalized (T x) = isDenormalized x ; \
   isNegativeZero (T x) = isNegativeZero x ; \
   isIEEE         (T x) = isIEEE x ; \
   (T x) `atan2`  (T y) = T (x `atan2` y) }

#else /* __GLASGOW_HASKELL__ */

/* On GHC, we just cast the type of each method to the underlying
 * type.  This means that GHC only needs to generate the dictionary
 * for each instance, rather than a new function for each method (the
 * simplifier currently isn't clever enough to reduce a method that
 * simply deconstructs a newtype and calls the underlying method into
 * an indirection to the underlying method, so that's what we're doing
 * here). 
 */

#define NUMERIC_TYPE(T,C,S,B) \
newtype T = T B ; \
INSTANCE_EQ(T,B) ; \
INSTANCE_ORD(T,B) ; \
INSTANCE_NUM(T,B) ; \
INSTANCE_READ(T,B) ; \
INSTANCE_SHOW(T,B) ; \
INSTANCE_ENUM(T,B) 

#define INTEGRAL_TYPE(T,C,S,B) \
NUMERIC_TYPE(T,C,S,B) ;  \
INSTANCE_BOUNDED(T,B) ; \
INSTANCE_REAL(T,B) ; \
INSTANCE_INTEGRAL(T,B) ; \
INSTANCE_BITS(T,B)

#define FLOATING_TYPE(T,C,S,B) \
NUMERIC_TYPE(T,C,S,B) ; \
INSTANCE_REAL(T,B) ; \
INSTANCE_FRACTIONAL(T,B) ; \
INSTANCE_FLOATING(T,B) ; \
INSTANCE_REALFRAC(T) ; \
INSTANCE_REALFLOAT(T,B)

#define INSTANCE_EQ(T,B) \
instance Eq T where { \
   (==)	 	= unsafeCoerce# ((==) :: B -> B -> Bool); \
   (/=)	 	= unsafeCoerce# ((/=) :: B -> B -> Bool); }

#define INSTANCE_ORD(T,B) \
instance Ord T where { \
   compare		= unsafeCoerce# (compare :: B -> B -> Ordering); \
   (<)			= unsafeCoerce# ((<) :: B -> B -> Bool); \
   (<=)			= unsafeCoerce# ((<=) :: B -> B -> Bool); \
   (>=)			= unsafeCoerce# ((>=) :: B -> B -> Bool); \
   (>)			= unsafeCoerce# ((>) :: B -> B -> Bool); \
   max			= unsafeCoerce# (max :: B -> B -> B); \
   min			= unsafeCoerce# (min :: B -> B -> B); }

#define INSTANCE_READ(T,B) \
instance Read T where { \
   readsPrec		= unsafeCoerce# (readsPrec :: Int -> ReadS B); \
   readList		= unsafeCoerce# (readList  :: ReadS [B]); }

#define INSTANCE_SHOW(T,B) \
instance Show T where { \
   showsPrec		= unsafeCoerce# (showsPrec :: Int -> B -> ShowS); \
   show			= unsafeCoerce# (show :: B -> String); \
   showList		= unsafeCoerce# (showList :: [B] -> ShowS); }

#define INSTANCE_NUM(T,B) \
instance Num T where { \
   (+)			= unsafeCoerce# ((+) :: B -> B -> B); \
   (-)			= unsafeCoerce# ((-) :: B -> B -> B); \
   (*)			= unsafeCoerce# ((*) :: B -> B -> B); \
   negate		= unsafeCoerce# (negate :: B -> B); \
   abs			= unsafeCoerce# (abs :: B -> B); \
   signum		= unsafeCoerce# (signum :: B -> B); \
   fromInteger		= unsafeCoerce# (fromInteger :: Integer -> B); }

#define INSTANCE_BOUNDED(T,B) \
instance Bounded T where { \
   minBound = T minBound ; \
   maxBound = T maxBound }

#define INSTANCE_ENUM(T,B) \
instance Enum T where { \
    succ		= unsafeCoerce# (succ :: B -> B); \
    pred		= unsafeCoerce# (pred :: B -> B); \
    toEnum              = unsafeCoerce# (toEnum :: Int -> B); \
    fromEnum            = unsafeCoerce# (fromEnum :: B -> Int); \
    enumFrom		= unsafeCoerce# (enumFrom :: B -> [B]); \
    enumFromThen	= unsafeCoerce# (enumFromThen :: B -> B -> [B]); \
    enumFromTo		= unsafeCoerce# (enumFromTo :: B -> B -> [B]); \
    enumFromThenTo	= unsafeCoerce# (enumFromThenTo :: B -> B -> B -> [B]);}

#define INSTANCE_REAL(T,B) \
instance Real T where { \
   toRational = unsafeCoerce# (toRational :: B -> Rational) }

#define INSTANCE_INTEGRAL(T,B) \
instance Integral T where { \
    quot		= unsafeCoerce# (quot:: B -> B -> B); \
    rem			= unsafeCoerce# (rem:: B -> B -> B); \
    div			= unsafeCoerce# (div:: B -> B -> B); \
    mod			= unsafeCoerce# (mod:: B -> B -> B); \
    quotRem		= unsafeCoerce# (quotRem:: B -> B -> (B,B)); \
    divMod		= unsafeCoerce# (divMod:: B -> B -> (B,B)); \
    toInteger		= unsafeCoerce# (toInteger:: B -> Integer); }

#define INSTANCE_BITS(T,B) \
instance Bits T where { \
  (.&.)			= unsafeCoerce# ((.&.) :: B -> B -> B); \
  (.|.)			= unsafeCoerce# ((.|.) :: B -> B -> B); \
  xor			= unsafeCoerce# (xor:: B -> B -> B); \
  complement    	= unsafeCoerce# (complement:: B -> B); \
  shift         	= unsafeCoerce# (shift:: B -> Int -> B); \
  rotate        	= unsafeCoerce# (rotate:: B -> Int -> B); \
  bit           	= unsafeCoerce# (bit:: Int -> B); \
  setBit        	= unsafeCoerce# (setBit:: B -> Int -> B); \
  clearBit      	= unsafeCoerce# (clearBit:: B -> Int -> B); \
  complementBit 	= unsafeCoerce# (complementBit:: B -> Int -> B); \
  testBit       	= unsafeCoerce# (testBit:: B -> Int -> Bool); \
  bitSize       	= unsafeCoerce# (bitSize:: B -> Int); \
  isSigned      	= unsafeCoerce# (isSigned:: B -> Bool); }

#define INSTANCE_FRACTIONAL(T,B) \
instance Fractional T where { \
    (/)			= unsafeCoerce# ((/) :: B -> B -> B); \
    recip		= unsafeCoerce# (recip :: B -> B); \
    fromRational	= unsafeCoerce# (fromRational :: Rational -> B); }

#define INSTANCE_FLOATING(T,B) \
instance Floating T where { \
    pi			= unsafeCoerce# (pi :: B); \
    exp			= unsafeCoerce# (exp :: B -> B); \
    log			= unsafeCoerce# (log :: B -> B); \
    sqrt		= unsafeCoerce# (sqrt :: B -> B); \
    (**) 		= unsafeCoerce# ((**) :: B -> B -> B); \
    logBase		= unsafeCoerce# (logBase :: B -> B -> B); \
    sin			= unsafeCoerce# (sin :: B -> B); \
    cos			= unsafeCoerce# (cos :: B -> B); \
    tan			= unsafeCoerce# (tan :: B -> B); \
    asin		= unsafeCoerce# (asin :: B -> B); \
    acos		= unsafeCoerce# (acos :: B -> B); \
    atan		= unsafeCoerce# (atan :: B -> B); \
    sinh		= unsafeCoerce# (sinh :: B -> B); \
    cosh		= unsafeCoerce# (cosh :: B -> B); \
    tanh		= unsafeCoerce# (tanh :: B -> B); \
    asinh		= unsafeCoerce# (asinh :: B -> B); \
    acosh		= unsafeCoerce# (acosh :: B -> B); \
    atanh		= unsafeCoerce# (atanh :: B -> B); }

/* The coerce trick doesn't work for RealFrac, these methods are
 * polymorphic and overloaded.
 */
#define INSTANCE_REALFRAC(T) \
instance RealFrac T where { \
   properFraction (T x) = let (m,y) = properFraction x in (m, T y) ; \
   truncate (T x) = truncate x ; \
   round    (T x) = round x ; \
   ceiling  (T x) = ceiling x ; \
   floor    (T x) = floor x }

#define INSTANCE_REALFLOAT(T,B) \
instance RealFloat T where { \
    floatRadix		= unsafeCoerce# (floatRadix :: B -> Integer); \
    floatDigits		= unsafeCoerce# (floatDigits :: B -> Int); \
    floatRange		= unsafeCoerce# (floatRange :: B -> (Int,Int)); \
    decodeFloat		= unsafeCoerce# (decodeFloat :: B -> (Integer,Int)); \
    encodeFloat		= unsafeCoerce# (encodeFloat :: Integer -> Int -> B); \
    exponent		= unsafeCoerce# (exponent :: B -> Int); \
    significand		= unsafeCoerce# (significand :: B -> B); \
    scaleFloat		= unsafeCoerce# (scaleFloat :: Int -> B -> B); \
    isNaN		= unsafeCoerce# (isNaN :: B -> Bool); \
    isInfinite		= unsafeCoerce# (isInfinite :: B -> Bool); \
    isDenormalized	= unsafeCoerce# (isDenormalized :: B -> Bool); \
    isNegativeZero	= unsafeCoerce# (isNegativeZero :: B -> Bool); \
    isIEEE		= unsafeCoerce# (isIEEE :: B -> Bool); \
    atan2	        = unsafeCoerce# (atan2 :: B -> B -> B); }

#endif /* __GLASGOW_HASKELL__ */
