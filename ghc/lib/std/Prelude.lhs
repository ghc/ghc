We add the option -fno-implicit-prelude here to tell the reader that
special names such as () and -> shouldn't be resolved to Prelude.()
and Prelude.-> (as they are normally). -- SDM 8/10/97

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Prelude (

	-- Everything from these modules
    module PrelList,
    module PrelTup,

	-- From PrelBase
    (->),
    Eq(..), 
    Ord(..), Ordering(..), 
    Bounded(..), 
    Enum(..), succ, pred, 
    Show(..), ShowS, shows, show, showChar, showString, showParen,
    seq, strict,
    Bool(..), (&&), (||), not, otherwise,
    Char, String, Int, Integer, Float, Double, Void,
    Maybe(..), maybe,
    Either(..), either,
    ()(..),		-- The unit type

    
    id, const, (.), flip, ($), until, asTypeOf, undefined,

	-- From Error
    error,

	-- From Monad
    Functor(..), Monad(..), MonadZero(..), MonadPlus(..),
    accumulate, sequence, mapM, mapM_, guard, filter, concat, applyM,

	-- From PrelRead
    ReadS, Read(readsPrec, readList),
    reads, read, lex, readParen, 

        -- From IO
    IO, FilePath, IOError,
    fail, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,

	-- From PrelNum
    Ratio, Rational, 
    (%), numerator, denominator, approxRational,

    Num((+), (-), (*), negate, abs, signum, fromInteger, fromInt{-glaExt-}),
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
import PrelRead
import PrelNum
import PrelNumExtra
import PrelTup
import PrelMaybe
import PrelEither
import PrelBounded
import PrelConc
import Monad
import Maybe
import PrelErr   ( error )
import IO

-- These can't conveniently be defined in PrelBase because they use numbers,
-- or I/O, so here's a convenient place to do them.

strict      :: (a -> b) -> a -> b
strict f x  = x `seq` f x

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"
\end{code}



