{-# LANGUAGE BangPatterns, NoImplicitPrelude, PackageImports, Trustworthy #-}

-- |
-- The Haskell 98 Prelude: a standard module imported by default
-- into all Haskell modules.  For more documentation, see the Haskell 98
-- Report <http://www.haskell.org/onlinereport/>.

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

import qualified "base" Control.Exception.Base as New (catch)
import "base" Control.Monad
import "base" System.IO
import "base" System.IO.Error (IOError, ioError, userError)
import "base" Data.OldList hiding ( splitAt )
import "base" Data.Either
import "base" Data.Maybe
import "base" Data.Tuple

import GHC.Base (($), ($!), (&&), (.), (||), Bool(..), Char, Eq(..), Int,
                 Ord(..), Ordering(..), String, asTypeOf, const, error, flip,
                 id, not, otherwise, seq, undefined, until)
import Text.Read
import GHC.Enum
import GHC.Num
import GHC.Real hiding ( gcd )
import qualified GHC.Real ( gcd )
import GHC.Float
import GHC.Show

-- -----------------------------------------------------------------------------
-- Miscellaneous functions

-- | The 'catch' function establishes a handler that receives any
-- 'IOError' raised in the action protected by 'catch'.
-- An 'IOError' is caught by
-- the most recent handler established by one of the exception handling
-- functions.  These handlers are
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

-- | @'gcd' x y@ is the greatest (positive) integer that divides both @x@
-- and @y@; for example @'gcd' (-3) 6@ = @3@, @'gcd' (-3) (-6)@ = @3@,
-- @'gcd' 0 4@ = @4@.  @'gcd' 0 0@ raises a runtime error.
gcd             :: (Integral a) => a -> a -> a
gcd 0 0         =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         = GHC.Real.gcd x y

-- The GHC's version of 'splitAt' is too strict in 'n' compared to
-- Haskell98/2010 version. Ticket #1182.

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@.
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.
splitAt                :: Int -> [a] -> ([a],[a])
splitAt n xs           =  (take n xs, drop n xs)
