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
       , showIntAtBase   -- :: Integral a => a -> (a -> Char) -> a -> ShowS
       ) where

import Char (ord, chr)
import PrelBase (ord_0)
import GlaExts
\end{code}

\begin{code}
doubleToFloat :: Double -> Float
doubleToFloat (D# d#) = F# (double2Float# d#)

floatToDouble :: Float -> Double
floatToDouble (F# f#) = D# (float2Double# f#)

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
\end{code}
