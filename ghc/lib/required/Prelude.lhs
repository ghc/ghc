
\begin{code}
module Prelude (

	-- Everything from these modules
    module PrelList,
    module PrelIO,
    module PrelTup,

	-- From PrelBase
    Eq(..), 
    Ord(..), Ordering(..), 
    Bounded(..), 
    Enum(..), succ, pred, 
    Show(..), ShowS, shows, show, showChar, showString, showParen,
    Num(..), 
    Eval(..), seq, strict,
    Bool(..), (&&), (||), not, otherwise,
    Char, String, Int, Integer, Float, Double, Void,
    Maybe(..), maybe,
    Either(..), either,
    ()(..),		-- The unit type

    
    id, const, (.), flip, ($), until, asTypeOf, undefined,

	-- From IOBase
    error,

	-- From Monad
    Functor(..), Monad(..), MonadZero(..), MonadPlus(..),
    accumulate, sequence, mapM, mapM_, guard, filter, concat, applyM,

	-- From PrelRead
    ReadS, Read(readsPrec, readList),
    reads, read, lex, readParen, 

	-- From PrelShow

	-- From PrelNum
    Ratio, Rational, 
    (%), numerator, denominator, approxRational,

    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger, toInt{-partain-}),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero),
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, fromRealFrac, atan2
  ) where

import PrelBase
import PrelList
import PrelIO
import PrelRead
import PrelNum
import PrelTup
import Monad
import Maybe
import IOBase	( error )
import GHCerr

-- These can't conveniently be defined in PrelBase because they use numbers,
-- or I/O, so here's a convenient place to do them.

strict      :: Eval a => (a -> b) -> a -> b
strict f x  = x `seq` f x

{-# INLINE seq  #-}
seq :: Eval a => a -> b -> b
#ifdef __CONCURRENT_HASKELL__
seq  x y = case (seq#  x) of { 0# -> parError; _ -> y }
#else
seq  x y = y		-- WRONG!
#endif

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"
\end{code}



