{-# OPTIONS_GHC -XNoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Prelude: a standard module imported by default into all Haskell
-- modules.  For more documentation, see the Haskell 98 Report
-- <http://www.haskell.org/onlinereport/>.
--
-----------------------------------------------------------------------------

module Prelude (

    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Maybe(Nothing, Just),
    maybe,

    Either(Left, Right),
    either,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry,

#if defined(__NHC__)
    []((:), []),        -- Not legal Haskell 98;
                        -- ... available through built-in syntax
    module Data.Tuple,  -- Includes tuple types
    ()(..),             -- Not legal Haskell 98
    (->),               -- ... available through built-in syntax
#endif
#ifdef __HUGS__
    (:),                -- Not legal Haskell 98
#endif

    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,

    -- ** Monads and functors
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),

    -- ** Miscellaneous functions
    id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!),

    -- * List operations
    map, (++), filter,
    head, last, tail, init, null, length, (!!),
    reverse,
    -- ** Reducing lists (folds)
    foldl, foldl1, foldr, foldr1,
    -- *** Special folds
    and, or, any, all,
    sum, product,
    concat, concatMap,
    maximum, minimum,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    -- ** Searching lists
    elem, notElem, lookup,
    -- ** Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError, catch

  ) where

#ifndef __HUGS__
import Control.Monad
import System.IO
import Data.List
import Data.Either
import Data.Maybe
import Data.Tuple
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import Text.Read
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show
import GHC.Err   ( error, undefined )
#endif

#ifndef __HUGS__
import qualified Control.Exception.Base as New (catch)
#endif

#ifdef __HUGS__
import Hugs.Prelude
#endif

#ifndef __HUGS__
infixr 0 $!

-- -----------------------------------------------------------------------------
-- Miscellaneous functions

-- | Strict (call-by-value) application, defined in terms of 'seq'.
($!)    :: (a -> b) -> a -> b
f $! x  = x `seq` f x
#endif

#ifdef __HADDOCK__
-- | The value of @'seq' a b@ is bottom if @a@ is bottom, and otherwise
-- equal to @b@.  'seq' is usually introduced to improve performance by
-- avoiding unneeded laziness.
seq :: a -> b -> b
seq _ y = y
#endif

#ifndef __HUGS__
-- | The 'catch' function establishes a handler that receives any 'IOError'
-- raised in the action protected by 'catch'.  An 'IOError' is caught by
-- the most recent handler established by 'catch'.  These handlers are
-- not selective: all 'IOError's are caught.  Exception propagation
-- must be explicitly provided in a handler by re-raising any unwanted
-- exceptions.  For example, in
--
-- > f = catch g (\e -> if IO.isEOFError e then return [] else ioError e)
--
-- the function @f@ returns @[]@ when an end-of-file exception
-- (cf. 'System.IO.Error.isEOFError') occurs in @g@; otherwise, the
-- exception is propagated to the next outer handler.
--
-- When an exception propagates outside the main program, the Haskell
-- system prints the associated 'IOError' value and exits the program.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.catch' from "Control.Exception".
catch :: IO a -> (IOError -> IO a) -> IO a
catch = New.catch
#endif /* !__HUGS__ */
