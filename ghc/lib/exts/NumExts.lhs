%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[NumExts]{Misc numeric bits}

\begin{code}
module NumExts

       (
         doubleToFloat   -- :: Double -> Float
       , floatToDouble   -- :: Double -> Float

       , showHex         -- :: Integral a => a -> ShowS
       , showOct         -- :: Integral a => a -> ShowS
       , showBin         -- :: Integral a => a -> ShowS

	 -- general purpose number->string converter.
       , showIntAtBase   -- :: Integral a 
			 -- => a		-- base
			 -- -> (a -> Char)      -- digit to char
			 -- -> a                -- number to show.
			 -- -> ShowS
       , showListWith    -- :: (a -> ShowS)
			 -- -> [a]
			 -- -> ShowS
       ) where

import Char (ord, chr)
#ifdef __HUGS__
ord_0 = ord '0'
#else
import PrelNum ( ord_0 )
import PrelShow( showList__ )
import GlaExts
#endif
\end{code}

\begin{code}
doubleToFloat :: Double -> Float
floatToDouble :: Float -> Double

#ifdef __HUGS__
doubleToFloat = primDoubleToFloat
floatToDouble = primFloatToDouble
#else
doubleToFloat (D# d#) = F# (double2Float# d#)
floatToDouble (F# f#) = D# (float2Double# f#)
#endif

#ifdef __HUGS__
showIntAtBase :: Integral a => a -> (a -> Char) -> a -> ShowS
showIntAtBase base toChr n r
  | n < 0  = error ("NumExts.showIntAtBase: applied to negative number " ++ show n)
  | otherwise = 
    case quotRem n base of { (n', d) ->
    let c = toChr d in
    seq c $ -- stricter than necessary
    let
	r' = c : r
    in
    if n' == 0 then r' else showIntAtBase base toChr n' r'
    }
#else
showIntAtBase :: Integral a => a -> (a -> Char) -> a -> ShowS
showIntAtBase base toChr n r
  | n < 0  = error ("NumExts.showIntAtBase: applied to negative number " ++ show n)
  | otherwise = 
    case quotRem n base of { (n', d) ->
    case toChr d        of { C# c# -> -- stricter than necessary
    let
	r' = C# c# : r
    in
    if n' == 0 then r' else showIntAtBase base toChr n' r'
    }}
#endif

showHex :: Integral a => a -> ShowS
showHex n r = 
 showString "0x" $
 showIntAtBase 16 (toChrHex) n r
 where  
  toChrHex d
    | d < 10    = chr (ord_0   + fromIntegral d)
    | otherwise = chr (ord 'a' + fromIntegral (d - 10))

showOct :: Integral a => a -> ShowS
showOct n r = 
 showString "0o" $
 showIntAtBase 8 (toChrOct) n r
 where toChrOct d = chr (ord_0   + fromIntegral d)

showBin :: Integral a => a -> ShowS
showBin n r = 
 showString "0b" $
 showIntAtBase 2 (toChrOct) n r
 where toChrOct d = chr (ord_0 + fromIntegral d)
\end{code}

Easy enough to define by the user, but since it's
occasionally useful (when, say, printing out a 
list of hex values), we define and export it
from @NumExts@.

\begin{code}
showListWith :: (a -> ShowS) -> [a] -> ShowS 
showListWith = showList__
#ifdef __HUGS__
showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)
#endif
\end{code}

