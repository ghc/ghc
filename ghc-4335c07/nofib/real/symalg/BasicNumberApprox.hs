module BasicNumberApprox (equ, lt, gt, lte, gte, ne, rabs, rsignum, 
                          rtoRational, basicNumber2str) where

import RealM
import BasicNumber
import Data.Ratio
import Data.List(genericDrop, genericTake)

-- This module contains a set of routines which need a precision
-- argument to work for reals. For example, two real numbers can
-- be compared for equality only to a cerain precision.

-- Compare a and b for equality to a precision n.
equ :: BasicNumber -> BasicNumber -> Integer -> Bool
equ (BasRealC a) b n     = if (diff <= 2) then True
                                          else False
                           where
                             diff = abs ((evalReal a n) - (evalReal c n))
                             (BasRealC c) = makeReal b
equ a (b@(BasRealC _)) n = equ b a n
equ a b _                = a == b
-------------------------------------------------------------------------------

-- Check if a < b to a precision n.
lt :: BasicNumber -> BasicNumber -> Integer -> Bool
lt (BasRealC a) b n     = if (diff < -2) then True
                                         else False
                          where
                            diff = ((evalReal a n)  - (evalReal c n))
                            (BasRealC c) = makeReal b
lt a (b@(BasRealC _)) n = gt b a n
lt a b _                = a < b
-------------------------------------------------------------------------------

-- Check if a > b to a precision n.
gt :: BasicNumber -> BasicNumber -> Integer -> Bool
gt (BasRealC a) b n     = if (diff > 2) then True
                                        else False
                          where
                            diff = ((evalReal a n) - (evalReal c n))
                            (BasRealC c) = makeReal b
gt a (b@(BasRealC _)) n = lt b a n
gt a b _                = b < a
-------------------------------------------------------------------------------

-- Check if a <= b to a precision n.
lte :: BasicNumber -> BasicNumber -> Integer -> Bool
lte a b n = not (gt a b n)
-------------------------------------------------------------------------------

-- Check if a >= b to a precision n.
gte :: BasicNumber -> BasicNumber -> Integer -> Bool
gte a b n = not (lt a b n)
-------------------------------------------------------------------------------

-- Check if a /= b to a precision n.
ne :: BasicNumber -> BasicNumber -> Integer -> Bool
ne a b n = not (equ a b n)
-------------------------------------------------------------------------------

-- Get the absolute value of a.
rabs :: BasicNumber -> Integer -> BasicNumber
rabs (a@(BasRealC ar)) n = if (evalReal ar n) < 0 then (fromInteger(-1))*a
                                                  else a
rabs a n = abs a
-------------------------------------------------------------------------------

-- Get the sign of a number.
rsignum :: BasicNumber -> Integer -> BasicNumber
rsignum (a@(BasRealC ar)) n = if ev_ar < 0 
                              then fromInteger (-1)
                              else if ev_ar == 0 
                                   then fromInteger 0
                                   else fromInteger 1
                            where
                              ev_ar = evalReal ar n
rsignum a n = signum a
-------------------------------------------------------------------------------

-- Convert a BasicNumber to a rational with precision n.
rtoRational :: BasicNumber -> Integer -> BasicNumber
rtoRational (BasRealC a) n = if n <= 0 
                             then (BasRationalC ((evalReal a n) % (10^(-n))))
                             else (BasRationalC (((evalReal a n)*(10^n)) % 1))
rtoRational a _            = makeRational a
-------------------------------------------------------------------------------

-- Convert a BasicNumber to a string with precision n.
basicNumber2str :: BasicNumber -> Integer -> String
basicNumber2str (BasRealC x) prec =
                                intPart ++ "." ++ fracPart
                                where
                                  evalX = show (evalReal x prec)
                                  lenBeforeDecimal = (toInteger (length evalX))
                                                     + prec
                                  intPart = if lenBeforeDecimal <= 0
                                            then "0"
                                            else genericTake lenBeforeDecimal
                                                      evalX
                                  fracPart = if lenBeforeDecimal < 0
                                             then (pad (- lenBeforeDecimal)
                                                       '0') ++
                                                  evalX
                                             else genericDrop lenBeforeDecimal
                                                       evalX
                                  pad 0 a     = []
                                  --WAS:pad (n+1) a = a:(pad n a)
                                  pad n a = a:(pad (n-1) a)
basicNumber2str (BasRationalC x) _ = show x
basicNumber2str (BasIntegerC x) _ = show x
-------------------------------------------------------------------------------
