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
    Eval(..), seq, strict,
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
import PrelTup
import PrelMaybe
import PrelEither
import PrelBounded
import Monad
import Maybe
import PrelErr   ( error, seqError )
import IO

-- These can't conveniently be defined in PrelBase because they use numbers,
-- or I/O, so here's a convenient place to do them.

strict      :: Eval a => (a -> b) -> a -> b
strict f x  = x `seq` f x


-- "seq" is defined a bit wierdly (see below)
--
-- The reason for the strange "0# -> parError" case is that
-- it fools the compiler into thinking that seq is non-strict in
-- its second argument (even if it inlines seq at the call site).
-- If it thinks seq is strict in "y", then it often evaluates
-- "y" before "x", which is totally wrong.  
--
-- Just before converting from Core to STG there's a bit of magic
-- that recognises the seq# and eliminates the duff case.

{-# INLINE seq  #-}
seq :: Eval a => a -> b -> b
seq  x y = case (seq#  x) of { 0# -> seqError; _ -> y }

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"
\end{code}



