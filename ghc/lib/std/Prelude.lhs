We add the option -fno-implicit-prelude here to tell the reader that
special names such as () and -> shouldn't be resolved to Prelude.()
and Prelude.-> (as they are normally). -- SDM 8/10/97

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Prelude (

	-- Everything corresponding to the Report's PreludeList
    module PrelList, 
    lines, words, unlines, unwords,
    sum, product,

        -- Everything corresponding to the Report's PreludeText
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, showList, show),
    reads, shows, read, lex, 
    showChar, showString, readParen, showParen,
    
        -- Everything corresponding to the Report's PreludeIO
    FilePath, IOError,
    ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,

    Bool(..),
    Maybe(..),
    Either(..),
    Ordering(..), 
    Char, String, Int, Integer, Float, Double, IO,
    Rational,
    []((:), []),
    
    module PrelTup,
        -- Includes tuple types + fst, snd, curry, uncurry
    ()(..),		-- The unit type
    (->),		-- functions
    
    Eq(..),
    Ord(..), 
    Enum(..),
    Bounded(..), 
    Num((+), (-), (*), negate, abs, signum, fromInteger),
	-- The fromInt method is exposed only by GlaExts
    Real(..),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
	-- The toInt method is exposed only by GlaExts
    Fractional(..),
    Floating(..),
    RealFrac(..),
    RealFloat(..),

	-- From Monad
    Monad(..),
    Functor(..), 
    mapM, mapM_, sequence, sequence_, (=<<),

    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac,
    --exported by PrelTup: fst, snd, curry, uncurry,
    id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)

  ) where

import PrelBase
import PrelList
#ifndef USE_REPORT_PRELUDE
     hiding ( takeUInt_append )
#endif
import PrelRead
import PrelEnum
import PrelNum
import PrelReal
import PrelFloat
import PrelTup
import PrelMaybe
import PrelShow
import PrelConc
import Monad
import Maybe
import PrelErr   ( error )
import IO

infixr 0 $!

($!)    :: (a -> b) -> a -> b
f $! x  = x `seq` f x

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"
\end{code}


%*********************************************************
%*							*
\subsection{List sum and product}
%*							*
%*********************************************************

List sum and product are defined here because PrelList is too far
down the compilation chain to "see" the Num class.

\begin{code}
-- sum and product compute the sum or product of a finite list of numbers.
{-# SPECIALISE sum     :: [Int] -> Int #-}
{-# SPECIALISE sum     :: [Integer] -> Integer #-}
{-# SPECIALISE product :: [Int] -> Int #-}
{-# SPECIALISE product :: [Integer] -> Integer #-}
sum, product            :: (Num a) => [a] -> a
#ifdef USE_REPORT_PRELUDE
sum                     =  foldl (+) 0  
product                 =  foldl (*) 1
#else
sum	l	= sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)
product	l	= prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)
#endif
\end{code}


%*********************************************************
%*							*
\subsection{Coercions}
%*							*
%*********************************************************

\begin{code}
{-# SPECIALIZE fromIntegral ::
    Int		-> Rational,
    Integer	-> Rational,
    Int  	-> Int,
    Int 	-> Integer,
    Int		-> Float,
    Int		-> Double,
    Integer  	-> Int,
    Integer 	-> Integer,
    Integer	-> Float,
    Integer	-> Double #-}
fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

{-# SPECIALIZE realToFrac ::
    Double	-> Rational, 
    Rational	-> Double,
    Float	-> Rational,
    Rational	-> Float,
    Rational	-> Rational,
    Double	-> Double,
    Double	-> Float,
    Float	-> Float,
    Float	-> Double #-}
realToFrac	:: (Real a, Fractional b) => a -> b
realToFrac	=  fromRational . toRational
\end{code}
