%
% (c) The AQUA Project, Glasgow University, 1997-99
%
\section[Numeric]{Numeric interface}

Odds and ends, mostly functions for reading and showing
\tr{RealFloat}-like kind of values.


\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module Numeric

        ( fromRat          -- :: (RealFloat a) => Rational -> a
	, showSigned       -- :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
	, readSigned       -- :: (Real a) => ReadS a -> ReadS a
	, showInt          -- :: Integral a => a -> ShowS
	, readInt          -- :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
	
	, readDec          -- :: (Integral a) => ReadS a
	, readOct          -- :: (Integral a) => ReadS a
	, readHex          -- :: (Integral a) => ReadS a

	, showEFloat       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	, showFFloat       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	, showGFloat       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	, showFloat        -- :: (RealFloat a) => a -> ShowS
	, readFloat        -- :: (RealFloat a) => ReadS a
	
	 
	, floatToDigits    -- :: (RealFloat a) => Integer -> a -> ([Int], Int)
	, lexDigits        -- :: ReadS String

          -- Implementation checked wrt. Haskell 98 lib report, 1/99.
	) where

import PrelBase
import PrelMaybe
import PrelShow
import PrelArr
import PrelNum
import PrelNumExtra
import PrelRead
import PrelErr ( error )

\end{code}

\begin{code}
showInt :: Integral a => a -> ShowS
showInt i rs
  | i < 0     = error "Numeric.showInt: can't show negative numbers"
  | otherwise = go i rs
    where
     go n r = 
      case quotRem n 10 of                 { (n', d) ->
      case chr (ord_0 + fromIntegral d) of { C# c# -> -- stricter than necessary
      let
	r' = C# c# : r
      in
      if n' == 0 then r' else go n' r'
      }}
\end{code}

Controlling the format and precision of floats. The code that
implements the formatting itself is in @PrelNum@ to avoid
mutual module deps.

\begin{code}
showEFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS

showEFloat d x =  showString (formatRealFloat FFExponent d x)
showFFloat d x =  showString (formatRealFloat FFFixed d x)
showGFloat d x =  showString (formatRealFloat FFGeneric d x)

\end{code}
