{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Odds and ends, mostly functions for reading and showing
-- RealFloat-like kind of values.
--
-----------------------------------------------------------------------------

module Numeric (

        fromRat,          -- :: (RealFloat a) => Rational -> a
	showSigned,       -- :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
	readSigned,       -- :: (Real a) => ReadS a -> ReadS a

	readInt,          -- :: (Integral a) => a -> (Char -> Bool)
			  --         -> (Char -> Int) -> ReadS a
	readDec,          -- :: (Integral a) => ReadS a
	readOct,          -- :: (Integral a) => ReadS a
	readHex,          -- :: (Integral a) => ReadS a

	showInt,          -- :: Integral a => a -> ShowS
        showIntAtBase,    -- :: Integral a => a -> (a -> Char) -> a -> ShowS
        showHex,          -- :: Integral a => a -> ShowS
        showOct,          -- :: Integral a => a -> ShowS

	showEFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showFFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showGFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showFloat,        -- :: (RealFloat a) => a -> ShowS
	readFloat,        -- :: (RealFloat a) => ReadS a
	
	floatToDigits,    -- :: (RealFloat a) => Integer -> a -> ([Int], Int)
	lexDigits,        -- :: ReadS String

	) where

import Data.Char

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Read
import GHC.Real
import GHC.Float
import GHC.Num
import GHC.Show
import Data.Maybe
import Text.ParserCombinators.ReadP( ReadP, readP_to_S, pfail )
import qualified Text.Read.Lex as L
#endif

#ifdef __HUGS__
import Hugs.Prelude
import Hugs.Numeric
#endif

#ifdef __GLASGOW_HASKELL__
-- -----------------------------------------------------------------------------
-- Reading

readInt :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt base isDigit valDigit = readP_to_S (L.readIntP base isDigit valDigit)

readOct, readDec, readHex :: Num a => ReadS a
readOct = readP_to_S L.readOctP
readDec = readP_to_S L.readDecP
readHex = readP_to_S L.readHexP 

readFloat :: RealFrac a => ReadS a
readFloat = readP_to_S readFloatP

readFloatP :: RealFrac a => ReadP a
readFloatP =
  do tok <- L.lex
     case tok of
       L.Rat y  -> return (fromRational y)
       L.Int i  -> return (fromInteger i)
       other    -> pfail

-- It's turgid to have readSigned work using list comprehensions,
-- but it's specified as a ReadS to ReadS transformer
-- With a bit of luck no one will use it.
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      (do
				        ("-",s) <- lex r
					(x,t)   <- read'' s
					return (-x,t))
			   read'' r = do
			       (str,s) <- lex r
		      	       (n,"")  <- readPos str
			       return (n,s)

-- -----------------------------------------------------------------------------
-- Showing

showInt :: Integral a => a -> ShowS
showInt n cs
    | n < 0     = error "Numeric.showInt: can't show negative numbers"
    | otherwise = go n cs
    where
    go n cs
        | n < 10    = case unsafeChr (ord '0' + fromIntegral n) of
            c@(C# _) -> c:cs
        | otherwise = case unsafeChr (ord '0' + fromIntegral r) of
            c@(C# _) -> go q (c:cs)
        where
        (q,r) = n `quotRem` 10

-- Controlling the format and precision of floats. The code that
-- implements the formatting itself is in @PrelNum@ to avoid
-- mutual module deps.

{-# SPECIALIZE showEFloat ::
	Maybe Int -> Float  -> ShowS,
	Maybe Int -> Double -> ShowS #-}
{-# SPECIALIZE showFFloat ::
	Maybe Int -> Float  -> ShowS,
	Maybe Int -> Double -> ShowS #-}
{-# SPECIALIZE showGFloat ::
	Maybe Int -> Float  -> ShowS,
	Maybe Int -> Double -> ShowS #-}

showEFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS

showEFloat d x =  showString (formatRealFloat FFExponent d x)
showFFloat d x =  showString (formatRealFloat FFFixed d x)
showGFloat d x =  showString (formatRealFloat FFGeneric d x)
#endif  /* __GLASGOW_HASKELL__ */

-- ---------------------------------------------------------------------------
-- Integer printing functions

showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n r
  | n < 0  = error ("Numeric.showIntAtBase: applied to negative number " ++ show n)
  | otherwise = 
    case quotRem n base of { (n', d) ->
    let c = toChr (fromIntegral d) in
    seq c $ -- stricter than necessary
    let
	r' = c : r
    in
    if n' == 0 then r' else showIntAtBase base toChr n' r'
    }

showHex, showOct :: Integral a => a -> ShowS
showHex = showIntAtBase 16 intToDigit
showOct = showIntAtBase 8  intToDigit
