{-
 WARNING: This file is an integral part of the Hugs source code.  Changes to
 the definitions in this file without corresponding modifications in other
 parts of the program may cause the interpreter to fail unexpectedly.  Under
 normal circumstances, you should not attempt to modify this file in any way!
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
    IO, IOResult(..), Addr, StablePtr,
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
    Fractional((/), recip, fromRational, fromDouble),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero),
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac, atan2,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)
	-- Now we have the extra (non standard) thing.
  ) where

import PrelPrim

