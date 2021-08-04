{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Numeric.Compat (
  module Base
, showFFloatAlt
, showGFloatAlt
, showHFloat
) where

import Numeric as Base

#if !(MIN_VERSION_base(4,7,0))
import Data.Char (intToDigit)
import GHC.Float
#endif

#if !(MIN_VERSION_base(4,11,0))
import Prelude
#endif

#if !(MIN_VERSION_base(4,7,0))
-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- This behaves as 'showFFloat', except that a decimal point
-- is always guaranteed, even if not needed.
--
-- /Since: 4.7.0.0/
showFFloatAlt    :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloatAlt d x = showString (formatRealFloatAlt FFFixed d True x)

-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- This behaves as 'showFFloat', except that a decimal point
-- is always guaranteed, even if not needed.
--
-- /Since: 4.7.0.0/
showGFloatAlt :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloatAlt d x = showString (formatRealFloatAlt FFGeneric d True x)

formatRealFloatAlt :: (RealFloat a) => FFFormat -> Maybe Int -> Bool -> a
                 -> String
formatRealFloatAlt fmt decs alt x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt fmt (floatToDigits (toInteger base) (-x))
   | otherwise                 = doFmt fmt (floatToDigits (toInteger base) x)
 where
  base = 10

  doFmt format (is, e) =
    let ds = map intToDigit is in
    case format of
     FFGeneric ->
      doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
            (is,e)
     FFExponent ->
      case decs of
       Nothing ->
        let show_e' = show (e-1) in
        case ds of
          "0"     -> "0.0e0"
          [d]     -> d : ".0e" ++ show_e'
          (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
          []      -> error "formatRealFloat/doFmt/FFExponent: []"
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
         _ ->
          let
           (ei,is') = roundTo base (dec'+1) is
           (d:ds') = map intToDigit (if ei > 0 then init is' else is')
          in
          d:'.':ds' ++ 'e':show (e-1+ei)
     FFFixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> ls}
      in
      case decs of
       Nothing
          | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                f n s    ""  = f (n-1) ('0':s) ""
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e "" ds
       Just dec ->
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo base (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map intToDigit is')
         in
         mk0 ls ++ (if null rs && not alt then "" else '.':rs)
        else
         let
          (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' && not alt then "" else '.':ds')
#endif

#if !(MIN_VERSION_base(4,11,0))
{- | Show a floating-point value in the hexadecimal format,
similar to the @%a@ specifier in C's printf.

  >>> showHFloat (212.21 :: Double) ""
  "0x1.a86b851eb851fp7"
  >>> showHFloat (-12.76 :: Float) ""
  "-0x1.9851ecp3"
  >>> showHFloat (-0 :: Double) ""
  "-0x0p+0"
-}
showHFloat :: RealFloat a => a -> ShowS
showHFloat = showString . fmt
  where
  fmt x
    | isNaN x                   = "NaN"
    | isInfinite x              = (if x < 0 then "-" else "") ++ "Infinity"
    | x < 0 || isNegativeZero x = '-' : cvt (-x)
    | otherwise                 = cvt x

  cvt x
    | x == 0 = "0x0p+0"
    | otherwise =
      case floatToDigits 2 x of
        r@([], _) -> error $ "Impossible happened: showHFloat: " ++ show r
        (d:ds, e) -> "0x" ++ show d ++ frac ds ++ "p" ++ show (e-1)

  -- Given binary digits, convert them to hex in blocks of 4
  -- Special case: If all 0's, just drop it.
  frac digits
    | allZ digits = ""
    | otherwise   = "." ++ hex digits
    where
    hex ds =
      case ds of
        []                -> ""
        [a]               -> hexDigit a 0 0 0 ""
        [a,b]             -> hexDigit a b 0 0 ""
        [a,b,c]           -> hexDigit a b c 0 ""
        a : b : c : d : r -> hexDigit a b c d (hex r)

  hexDigit a b c d = showHex (8*a + 4*b + 2*c + d)

  allZ xs = case xs of
              x : more -> x == 0 && allZ more
              []       -> True
#endif
