We add the option -fno-implicit-prelude here to tell the reader that
special names such as () and -> shouldn't be resolved to Prelude.()
and Prelude.-> (as they are normally). -- SDM 8/10/97

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Prelude (

	-- Everything from these modules
    module PrelList,
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
    Ratio, Rational, 
    []((:), []),
    
    module PrelTup,
        -- Includes tuple types + fst, snd, curry, uncurry
    ()(..),		-- The unit type
    (->),		-- functions
    
    Eq(..),
    Ord(..), 
    Enum(..),
    Bounded(..), 
    Num((+), (-), (*), negate, abs, signum, fromInteger, fromInt{-glaExt-}),
    Real(..),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger, toInt{-partain-}),
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

infixr 0 $!

($!)    :: (a -> b) -> a -> b
f $! x  = x `seq` f x

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"
\end{code}



