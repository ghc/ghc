module BasicNumber (BasicNumber (..), makeReal, makeRational, RealT{-partain-}) where

import RealM
import Data.Ratio

data BasicNumber = BasIntegerC Integer
                 | BasRationalC Rational
                 | BasRealC RealT
     deriving ()


makeReal (a@(BasRealC _)) = a
makeReal (BasRationalC a) = if (numerator a) == 0
                            then BasRealC (int2Real 0)
                            else BasRealC (divReal (int2Real (numerator a))
                                                   (int2Real (denominator a)))
makeReal (BasIntegerC a)  = BasRealC (int2Real a)
-------------------------------------------------------------------------------

makeRational (a@(BasRationalC _)) = a
makeRational (BasIntegerC a)      = BasRationalC (a % 1)
-------------------------------------------------------------------------------

instance Eq BasicNumber where

    (BasRealC _) == _                    = error "(==) : Real Numbers cannot\
                                                  \ be compared"
    _ == (BasRealC _)                    = error "(==) : Real Numbers cannot\
                                                  \ be compared"
    (BasRationalC a) == (BasRationalC b) = a == b
    (BasRationalC a) == (BasIntegerC b)  = a == (b % 1)
    (BasIntegerC a) == (BasRationalC b)  = (a % 1) == b
    (BasIntegerC a) == (BasIntegerC b)   = a == b
-------------------------------------------------------------------------------

instance Ord BasicNumber where

    (BasRealC _) < _                    = error "(<) : Real Numbers cannot\
                                                 \ be compared"
    _ < (BasRealC _)                    = error "(==) : Real Numbers cannot\
                                                 \ be compared"
    (BasRationalC a) < (BasRationalC b) = a < b
    (BasRationalC a) < (BasIntegerC b)  = a < (b % 1)
    (BasIntegerC a) < (BasRationalC b)  = (a % 1) < b
    (BasIntegerC a) < (BasIntegerC b)   = a < b
    ---------------------------------------------------------------------------

    (BasRealC _) <= _                    = error "(<=) : Real Numbers cannot\
                                                  \ be compared"
    _ <= (BasRealC _)                    = error "(<=) : Real Numbers cannot\
                                                  \ be compared"
    (BasRationalC a) <= (BasRationalC b) = a <= b
    (BasRationalC a) <= (BasIntegerC b)  = a <= (b % 1)
    (BasIntegerC a) <= (BasRationalC b)  = (a % 1) <= b
    (BasIntegerC a) <= (BasIntegerC b)   = a <= b
-------------------------------------------------------------------------------

instance Num BasicNumber where

    (BasRealC a) + b                  = BasRealC (addReal a c)
                                        where 
                                          (BasRealC c) = makeReal b
    a + (BasRealC b)                  = BasRealC (addReal c b)
                                        where 
                                          (BasRealC c) = makeReal a
    (BasRationalC a) + b              = BasRationalC (a + c)
                                        where 
                                          (BasRationalC c) = makeRational b
    a + (BasRationalC b)              = BasRationalC (c + b)
                                        where 
                                          (BasRationalC c) = makeRational a
    (BasIntegerC a) + (BasIntegerC b) = BasIntegerC (a + b)
    ---------------------------------------------------------------------------

    (BasRealC a) - b                  = BasRealC (subReal a c)
                                        where 
                                          (BasRealC c) = makeReal b
    a - (BasRealC b)                  = BasRealC (subReal c b)
                                        where 
                                          (BasRealC c) = makeReal a
    (BasRationalC a) - b              = BasRationalC (a - c)
                                        where 
                                          (BasRationalC c) = makeRational b
    a - (BasRationalC b)              = BasRationalC (c - b)
                                        where 
                                          (BasRationalC c) = makeRational a
    (BasIntegerC a) - (BasIntegerC b) = BasIntegerC (a - b)
    ---------------------------------------------------------------------------

    negate a = (BasIntegerC 0) - a
    ---------------------------------------------------------------------------

    (BasRealC a) * b                  = BasRealC (mulReal a c)
                                        where 
                                          (BasRealC c) = makeReal b
    a * (BasRealC b)                  = BasRealC (mulReal c b)
                                        where 
                                          (BasRealC c) = makeReal a
    (BasRationalC a) * b              = BasRationalC (a * c)
                                        where 
                                          (BasRationalC c) = makeRational b
    a * (BasRationalC b)              = BasRationalC (c * b)
                                        where 
                                          (BasRationalC c) = makeRational a
    (BasIntegerC a) * (BasIntegerC b) = BasIntegerC (a * b)
    ---------------------------------------------------------------------------

    abs (BasRealC _)     = error "abs : Operation not defined on reals"
    abs (BasRationalC a) = BasRationalC (abs a)
    abs (BasIntegerC a)  = BasIntegerC (abs a)
    ---------------------------------------------------------------------------

    signum (BasRealC _)     = error "signum : Operation not defined on reals"
    signum (BasRationalC a) = BasRationalC (signum a)
    signum (BasIntegerC a)  = BasIntegerC (signum a)
    ---------------------------------------------------------------------------

    fromInteger n = BasIntegerC n
-------------------------------------------------------------------------------

instance Enum BasicNumber where
    enumFrom n       = iterate (+1) n
    enumFromThen n m = iterate (+(m-n)) n

instance Real BasicNumber where

    toRational (BasRealC _)     = error "toRational : Real cannot be coerced\
                                         \ to rational"
    toRational (BasRationalC a) = a
    toRational (BasIntegerC a)  = a % 1
-------------------------------------------------------------------------------

instance Fractional BasicNumber where

    (BasRealC a) / b                  = BasRealC (divReal a c)
                                        where 
                                          (BasRealC c) = makeReal b
    a / (BasRealC b)                  = BasRealC (divReal c b)
                                        where 
                                          (BasRealC c) = makeReal a
    (BasRationalC a) / b              = BasRationalC (a / c)
                                        where 
                                          (BasRationalC c) = makeRational b
    a / (BasRationalC b)              = BasRationalC (c / b)
                                        where 
                                          (BasRationalC c) = makeRational a
    (BasIntegerC a) / (BasIntegerC b) = BasRationalC (a % b)
    ---------------------------------------------------------------------------
    
    fromRational a = BasRationalC a
-------------------------------------------------------------------------------

instance Floating BasicNumber where

    sqrt a   = BasRealC (sqrtReal b)
               where
                 (BasRealC b) = makeReal a
    ---------------------------------------------------------------------------

    pi       = error "pi : Not yet implemented"
    exp      = error "exp : Not yet implemented"
    log      = error "log : Not yet implemented"
    sin      = error "sin : Not yet implemented"
    cos      = error "cos : Not yet implemented"
    asin     = error "asin : Not yet implemented"
    acos     = error "acos : Not yet implemented"
    atan     = error "atan : Not yet implemented"
    sinh     = error "sinh : Not yet implemented"
    cosh     = error "cosh : Not yet implemented"
    asinh    = error "asinh : Not yet implemented"
    acosh    = error "acosh : Not yet implemented"
    atanh    = error "atanh : Not yet implemented"
-------------------------------------------------------------------------------

instance Show BasicNumber where

    showsPrec _ (BasRealC x) s  = intPart ++ "." ++ fracPart ++ s
                                where 
                                  evalX = show (evalReal x (-10))
                                  lenBeforeDecimal = (length evalX) - 10
                                  intPart = if lenBeforeDecimal <= 0
                                            then "0"
                                            else take lenBeforeDecimal
                                                      evalX
                                  fracPart = if lenBeforeDecimal < 0
                                             then (pad (- lenBeforeDecimal)
                                                       '0') ++
                                                  evalX
                                             else drop lenBeforeDecimal
                                                       evalX

                                  pad 0 a     = []
                                  --WAS:pad (n+1) a = a:(pad n a)
                                  pad n a = a:(pad (n-1) a)
    showsPrec _ (BasRationalC x) s = shows x s
    showsPrec _ (BasIntegerC x) s  = shows x s
    ---------------------------------------------------------------------------
instance Read BasicNumber where
    
    readsPrec p s = if allZeros frac
                    then map int2BasNum (readsPrec p int)
                    else map rat2BasNum (readsPrec p s)
                    where
		      (int, frac) = span (\c -> c /= '.') s

                      allZeros ""                                = True
                      allZeros (c:fs) | (c >= '1') && (c <= '9') = False
                      allZeros (c:fs)                            = allZeros fs

                      int2BasNum (num, s) = (BasIntegerC num, s)
                      rat2BasNum (num, s) = (BasRationalC 
                                               (approxRational num 0), s) 
-------------------------------------------------------------------------------
                                        
