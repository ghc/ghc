
{-# OPTIONS_GHC -fno-implicit-prelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer
-- Copyright   :  (c) Ian Lnyagh 2007-2008
-- License     :  BSD3
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- An simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Integer (
    Integer,
    smallInteger, wordToInteger, integerToWord, toInt#,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, word64ToInteger,
    integerToInt64, int64ToInteger,
#endif
    plusInteger, minusInteger, timesInteger, negateInteger,
    eqInteger, neqInteger, absInteger, signumInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    divModInteger, quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, decodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    -- gcdInteger, lcmInteger, -- XXX
    andInteger, orInteger, xorInteger, complementInteger,
    hashInteger,
 ) where

import GHC.Bool
import GHC.Ordering
import GHC.Prim
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

#if !defined(__HADDOCK__)

errorInteger :: Integer
errorInteger = Positive errorPositive

errorPositive :: Positive
errorPositive = Some 47## None -- Random number

data Integer = Positive Positive | Negative Positive | Naught

smallInteger :: Int# -> Integer
smallInteger i = if i >=# 0# then wordToInteger (int2Word# i)
                 else -- XXX is this right for -minBound?
                      negateInteger (wordToInteger (int2Word# (negateInt# i)))

wordToInteger :: Word# -> Integer
wordToInteger w = if w `eqWord#` 0##
                  then Naught
                  else Positive (Some w None)

integerToWord :: Integer -> Word#
integerToWord (Positive (Some w _)) = w
integerToWord (Negative (Some w _)) = 0## `minusWord#` w
-- Must be Naught by the invariant:
integerToWord _ = 0##

toInt# :: Integer -> Int#
toInt# i = word2Int# (integerToWord i)

#if WORD_SIZE_IN_BITS == 64
-- Nothing
#elif WORD_SIZE_IN_BITS == 32
integerToWord64 :: Integer -> Word64#
integerToWord64 i = int64ToWord64# (integerToInt64 i)

word64ToInteger:: Word64# -> Integer
word64ToInteger w = if w `eqWord64#` wordToWord64# 0##
                    then Naught
                    else Positive (word64ToPositive w)

integerToInt64 :: Integer -> Int64#
integerToInt64 Naught = intToInt64# 0#
integerToInt64 (Positive p) = word64ToInt64# (positiveToWord64 p)
integerToInt64 (Negative p)
    = negateInt64# (word64ToInt64# (positiveToWord64 p))

int64ToInteger :: Int64# -> Integer
int64ToInteger i
 = if i `eqInt64#` intToInt64# 0#
   then Naught
   else if i `gtInt64#` intToInt64# 0#
   then Positive (word64ToPositive (int64ToWord64# i))
   else Negative (word64ToPositive (int64ToWord64# (negateInt64# i)))
#else
#error WORD_SIZE_IN_BITS not supported
#endif

oneInteger :: Integer
oneInteger = Positive onePositive

negativeOneInteger :: Integer
negativeOneInteger = Negative onePositive

twoToTheThirtytwoInteger :: Integer
twoToTheThirtytwoInteger = Positive twoToTheThirtytwoPositive

encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger i j
 = encodeDouble# (toInt# (i `quotInteger` twoToTheThirtytwoInteger))
                 (toInt# i)
                 j

foreign import ccall unsafe "__2Int_encodeDouble"
        encodeDouble# :: Int# -> Int# -> Int# -> Double#

encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger i j = encodeFloat# (toInt# i) j

foreign import ccall unsafe "__int_encodeFloat"
    encodeFloat# :: Int# -> Int# -> Float#

decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger f = case decodeFloat_Int# f of
                       (# mant, exp #) -> (# smallInteger mant, exp #)

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d
 = case decodeDouble_2Int# d of
   (# mant_high#, mant_low#, exp# #) ->
       (# (smallInteger mant_high# `timesInteger` twoToTheThirtytwoInteger)
              `plusInteger` wordToInteger (int2Word# mant_low#),
          exp# #)

doubleFromInteger :: Integer -> Double#
doubleFromInteger Naught = 0.0##
doubleFromInteger (Positive p) = doubleFromPositive p
doubleFromInteger (Negative p) = negateDouble# (doubleFromPositive p)

floatFromInteger :: Integer -> Float#
floatFromInteger Naught = 0.0#
floatFromInteger (Positive p) = floatFromPositive p
floatFromInteger (Negative p) = negateFloat# (floatFromPositive p)

andInteger :: Integer -> Integer -> Integer
Naught     `andInteger` _          = Naught
_          `andInteger` Naught     = Naught
Positive x `andInteger` Positive y = digitsToInteger (x `andDigits` y)
{-
To calculate x & -y we need to calculate
    x & twosComplement y
The (imaginary) sign bits are 0 and 1, so &ing them give 0, i.e. positive.
Note that
    twosComplement y
has infinitely many 1s, but x has a finite number of digits, so andDigits
will return a finite result.
-}
Positive x `andInteger` Negative y = let y' = twosComplementPositive y
                                         z = x `andDigits` y'
                                     in digitsToInteger z
Negative x `andInteger` Positive y = Positive y `andInteger` Negative x
{-
To calculate -x & -y, naively we need to calculate
    twosComplement (twosComplement x & twosComplement y)
but
    twosComplement x & twosComplement y
has infinitely many 1s, so this won't work. Thus we use de Morgan's law
to get
    -x & -y = !(!(-x) | !(-y))
            = !(!(twosComplement x) | !(twosComplement y))
            = !(!(!x + 1) | (!y + 1))
            = !((x - 1) | (y - 1))
but the result is negative, so we need to take the two's complement of
this in order to get the magnitude of the result.
    twosComplement !((x - 1) | (y - 1))
            = !(!((x - 1) | (y - 1))) + 1
            = ((x - 1) | (y - 1)) + 1
-}
-- We don't know that x and y are /strictly/ greater than 1, but
-- minusPositive gives us the required answer anyway.
Negative x `andInteger` Negative y = let x' = x `minusPositive` onePositive
                                         y' = y `minusPositive` onePositive
                                         z = x' `orDigits` y'
                                         -- XXX Cheating the precondition:
                                         z' = succPositive z
                                     in digitsToNegativeInteger z'

orInteger :: Integer -> Integer -> Integer
Naught     `orInteger` i          = i
i          `orInteger` Naught     = i
Positive x `orInteger` Positive y = Positive (x `orDigits` y)
{-
x | -y = - (twosComplement (x | twosComplement y))
       = - (twosComplement !(!x & !(twosComplement y)))
       = - (twosComplement !(!x & !(!y + 1)))
       = - (twosComplement !(!x & (y - 1)))
       = - ((!x & (y - 1)) + 1)
-}
Positive x `orInteger` Negative y = let x' = flipBits x
                                        y' = y `minusPositive` onePositive
                                        z = x' `andDigits` y'
                                        z' = succPositive z
                                    in digitsToNegativeInteger z'
Negative x `orInteger` Positive y = Positive y `orInteger` Negative x
{-
-x | -y = - (twosComplement (twosComplement x | twosComplement y))
        = - (twosComplement !(!(twosComplement x) & !(twosComplement y)))
        = - (twosComplement !(!(!x + 1) & !(!y + 1)))
        = - (twosComplement !((x - 1) & (y - 1)))
        = - (((x - 1) & (y - 1)) + 1)
-}
Negative x `orInteger` Negative y = let x' = x `minusPositive` onePositive
                                        y' = y `minusPositive` onePositive
                                        z = x' `andDigits` y'
                                        z' = succPositive z
                                    in digitsToNegativeInteger z'

xorInteger :: Integer -> Integer -> Integer
Naught     `xorInteger` i          = i
i          `xorInteger` Naught     = i
Positive x `xorInteger` Positive y = digitsToInteger (x `xorDigits` y)
{-
x ^ -y = - (twosComplement (x ^ twosComplement y))
       = - (twosComplement !(x ^ !(twosComplement y)))
       = - (twosComplement !(x ^ !(!y + 1)))
       = - (twosComplement !(x ^ (y - 1)))
       = - ((x ^ (y - 1)) + 1)
-}
Positive x `xorInteger` Negative y = let y' = y `minusPositive` onePositive
                                         z = x `xorDigits` y'
                                         z' = succPositive z
                                     in digitsToNegativeInteger z'
Negative x `xorInteger` Positive y = Positive y `xorInteger` Negative x
{-
-x ^ -y = twosComplement x ^ twosComplement y
        = (!x + 1) ^ (!y + 1)
        = (!x + 1) ^ (!y + 1)
        = !(!x + 1) ^ !(!y + 1)
        = (x - 1) ^ (y - 1)
-}
Negative x `xorInteger` Negative y = let x' = x `minusPositive` onePositive
                                         y' = y `minusPositive` onePositive
                                         z = x' `xorDigits` y'
                                     in digitsToInteger z

complementInteger :: Integer -> Integer
complementInteger x = negativeOneInteger `minusInteger` x

-- Requires a finite Positive.
-- Returns an infinite Positive.
twosComplementPositive :: Positive -> Positive
twosComplementPositive p = succPositive (flipBits p)

-- Requires a finite Positive.
-- Returns an infinite Positive.
flipBits :: Positive -> Positive
flipBits None = let ones = Some (fullBound Unit) ones
         in ones
flipBits (Some w ws) = Some (not# w) (flipBits ws)

negateInteger :: Integer -> Integer
negateInteger (Positive p) = Negative p
negateInteger (Negative p) = Positive p
negateInteger Naught       = Naught

plusInteger :: Integer -> Integer -> Integer
Positive p1 `plusInteger` Positive p2 = Positive (p1 `plusPositive` p2)
Negative p1 `plusInteger` Negative p2 = Negative (p1 `plusPositive` p2)
Positive p1 `plusInteger` Negative p2 = case p1 `comparePositive` p2 of
                                        GT -> Positive (p1 `minusPositive` p2)
                                        EQ -> Naught
                                        LT -> Negative (p2 `minusPositive` p1)
Negative p1 `plusInteger` Positive p2 = Positive p2 `plusInteger` Negative p1
Naught        `plusInteger` i           = i
i           `plusInteger` Naught        = i

minusInteger :: Integer -> Integer -> Integer
i1 `minusInteger` i2 = i1 `plusInteger` negateInteger i2

timesInteger :: Integer -> Integer -> Integer
Positive p1 `timesInteger` Positive p2 = Positive (p1 `timesPositive` p2)
Negative p1 `timesInteger` Negative p2 = Positive (p1 `timesPositive` p2)
Positive p1 `timesInteger` Negative p2 = Negative (p1 `timesPositive` p2)
Negative p1 `timesInteger` Positive p2 = Negative (p1 `timesPositive` p2)
_           `timesInteger` _           = Naught

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
n `divModInteger` d =
    case n `quotRemInteger` d of
        (# q, r #) ->
            if signumInteger r `eqInteger`
               negateInteger (signumInteger d)
            then (# q `minusInteger` oneInteger, r `plusInteger` d #)
            else (# q, r #)

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
Naught      `quotRemInteger` _           = (# Naught, Naught #)
_           `quotRemInteger` Naught
    = (# errorInteger, errorInteger #) -- XXX Can't happen
-- XXX _            `quotRemInteger` Naught     = error "Division by zero"
Positive p1 `quotRemInteger` Positive p2 = p1 `quotRemPositive` p2
Negative p1 `quotRemInteger` Positive p2 = case p1 `quotRemPositive` p2 of
                                           (# q, r #) ->
                                               (# negateInteger q,
                                                  negateInteger r #)
Positive p1 `quotRemInteger` Negative p2 = case p1 `quotRemPositive` p2 of
                                           (# q, r #) ->
                                               (# negateInteger q, r #)
Negative p1 `quotRemInteger` Negative p2 = case p1 `quotRemPositive` p2 of
                                           (# q, r #) ->
                                               (# q, negateInteger r #)

quotInteger :: Integer -> Integer -> Integer
x `quotInteger` y = case x `quotRemInteger` y of
                    (# q, _ #) -> q

remInteger :: Integer -> Integer -> Integer
x `remInteger` y = case x `quotRemInteger` y of
                   (# _, r #) -> r

compareInteger :: Integer -> Integer -> Ordering
Positive x `compareInteger` Positive y = x `comparePositive` y
Positive _ `compareInteger` _          = GT
Naught     `compareInteger` Naught     = EQ
Naught     `compareInteger` Negative _ = GT
Negative x `compareInteger` Negative y = y `comparePositive` x
_          `compareInteger` _          = LT

eqInteger :: Integer -> Integer -> Bool
x `eqInteger` y = case x `compareInteger` y of
                  EQ -> True
                  _ -> False

neqInteger :: Integer -> Integer -> Bool
x `neqInteger` y = case x `compareInteger` y of
                   EQ -> False
                   _ -> True

ltInteger :: Integer -> Integer -> Bool
x `ltInteger` y = case x `compareInteger` y of
                  LT -> True
                  _ -> False

gtInteger :: Integer -> Integer -> Bool
x `gtInteger` y = case x `compareInteger` y of
                  GT -> True
                  _ -> False

leInteger :: Integer -> Integer -> Bool
x `leInteger` y = case x `compareInteger` y of
                  GT -> False
                  _ -> True

geInteger :: Integer -> Integer -> Bool
x `geInteger` y = case x `compareInteger` y of
                  LT -> False
                  _ -> True

absInteger :: Integer -> Integer
absInteger (Negative x) = Positive x
absInteger x = x

signumInteger :: Integer -> Integer
signumInteger (Negative _) = negativeOneInteger
signumInteger Naught       = Naught
signumInteger (Positive _) = oneInteger

-- XXX This isn't a great hash function
hashInteger :: Integer -> Int#
hashInteger _ = 42#

-------------------------------------------------------------------
-- The hard work is done on positive numbers

-- Least significant bit is first

-- Positive's have the property that they contain at least one Bit,
-- and their last Bit is One.
type Positive = Digits
type Positives = List Positive

data Digits = Some Digit Digits
            | None
type Digit = Word#

-- XXX Could move () above us
data Unit = Unit

-- XXX Could move [] above us
data List a = Nil | Cons a (List a)

onePositive :: Positive
onePositive = Some 1## None

halfBoundUp, fullBound :: Unit -> Digit
lowHalfMask :: Unit -> Digit
highHalfShift :: Unit -> Int#
twoToTheThirtytwoPositive :: Positive
#if WORD_SIZE_IN_BITS == 64
halfBoundUp   Unit = 0x8000000000000000##
fullBound     Unit = 0xFFFFFFFFFFFFFFFF##
lowHalfMask   Unit = 0xFFFFFFFF##
highHalfShift Unit = 32#
twoToTheThirtytwoPositive = Some 0x100000000## None
#elif WORD_SIZE_IN_BITS == 32
halfBoundUp   Unit = 0x80000000##
fullBound     Unit = 0xFFFFFFFF##
lowHalfMask   Unit = 0xFFFF##
highHalfShift Unit = 16#
twoToTheThirtytwoPositive = Some 0## (Some 1## None)
#else
#error Unhandled WORD_SIZE_IN_BITS
#endif

digitsMaybeZeroToInteger :: Digits -> Integer
digitsMaybeZeroToInteger None = Naught
digitsMaybeZeroToInteger ds = Positive ds

digitsToInteger :: Digits -> Integer
digitsToInteger ds = case removeZeroTails ds of
                     None -> Naught
                     ds' -> Positive ds'

digitsToNegativeInteger :: Digits -> Integer
digitsToNegativeInteger ds = case removeZeroTails ds of
                             None -> Naught
                             ds' -> Negative ds'

removeZeroTails :: Digits -> Digits
removeZeroTails (Some w ds) = if w `eqWord#` 0##
                              then case removeZeroTails ds of
                                   None -> None
                                   ds' -> Some w ds'
                              else Some w (removeZeroTails ds)
removeZeroTails None = None

#if WORD_SIZE_IN_BITS < 64
word64ToPositive :: Word64# -> Positive
word64ToPositive w
 = if w `eqWord64#` wordToWord64# 0##
   then None
   else Some (word64ToWord# w) (word64ToPositive (w `uncheckedShiftRL64#` 32#))

positiveToWord64 :: Positive -> Word64#
positiveToWord64 None = wordToWord64# 0## -- XXX Can't happen
positiveToWord64 (Some w None) = wordToWord64# w
positiveToWord64 (Some low (Some high _))
    = wordToWord64# low `or64#` (wordToWord64# high `uncheckedShiftL64#` 32#)
#endif

comparePositive :: Positive -> Positive -> Ordering
Some x xs `comparePositive` Some y ys = case xs `comparePositive` ys of
                                        EQ ->      if x `ltWord#` y then LT
                                              else if x `gtWord#` y then GT
                                              else                       EQ
                                        res -> res
None      `comparePositive` None      = EQ
_         `comparePositive` None      = GT
None      `comparePositive` _         = LT

plusPositive :: Positive -> Positive -> Positive
plusPositive = addWithCarry 0##
 where -- digit `elem` [0, 1]
       addWithCarry :: Digit -> Positive -> Positive -> Positive
       addWithCarry c xs None = addOnCarry c xs
       addWithCarry c None ys = addOnCarry c ys
       addWithCarry c xs@(Some x xs') ys@(Some y ys')
        = if x `ltWord#` y then addWithCarry c ys xs
          -- Now x >= y
          else if y `geWord#` halfBoundUp Unit
               -- So they are both at least halfBoundUp, so we subtract
               -- halfBoundUp from each and thus carry 1
               then case x `minusWord#` halfBoundUp Unit of
                    x' ->
                     case y `minusWord#` halfBoundUp Unit of
                     y' ->
                      case x' `plusWord#` y' `plusWord#` c of
                      this ->
                       Some this withCarry
          else if x `geWord#` halfBoundUp Unit
               then case x `minusWord#` halfBoundUp Unit of
                    x' ->
                     case x' `plusWord#` y `plusWord#` c of
                     z ->
                      -- We've taken off halfBoundUp, so now we need to
                      -- add it back on
                      if z `ltWord#` halfBoundUp Unit
                       then Some (z `plusWord#`  halfBoundUp Unit) withoutCarry
                       else Some (z `minusWord#` halfBoundUp Unit) withCarry
          else Some (x `plusWord#` y `plusWord#` c) withoutCarry
           where withCarry    = addWithCarry 1## xs' ys'
                 withoutCarry = addWithCarry 0## xs' ys'

       -- digit `elem` [0, 1]
       addOnCarry :: Digit -> Positive -> Positive
       addOnCarry c ws = if c `eqWord#` 0##
                         then ws
                         else succPositive ws

-- digit `elem` [0, 1]
succPositive :: Positive -> Positive
succPositive None = Some 1## None
succPositive (Some w ws) = if w `eqWord#` fullBound Unit
                           then Some 0## (succPositive ws)
                           else Some (w `plusWord#` 1##) ws

-- Requires x > y
-- In recursive calls, x >= y and x == y => result is None
minusPositive :: Positive -> Positive -> Positive
Some x xs `minusPositive` Some y ys
 = if x `eqWord#` y
   then case xs `minusPositive` ys of
        None -> None
        s -> Some 0## s
   else if x `gtWord#` y then
        Some (x `minusWord#` y) (xs `minusPositive` ys)
   else case (fullBound Unit `minusWord#` y) `plusWord#` 1## of
        z -> -- z = 2^n - y, calculated without overflow
         case z `plusWord#` x of
         z' -> -- z = 2^n + (x - y), calculated without overflow
          Some z' ((xs `minusPositive` ys) `minusPositive` onePositive)
xs   `minusPositive` None = xs
None `minusPositive` _ = errorPositive -- XXX Can't happen
-- XXX None `minusPositive` _ = error "minusPositive: Requirement x > y not met"

timesPositive :: Positive -> Positive -> Positive
-- XXX None's can't happen here:
None             `timesPositive` _           = errorPositive
_                `timesPositive` None        = errorPositive
-- x and y are the last digits in Positive numbers, so are not 0:
Some x None      `timesPositive` Some y None = x `timesDigit` y
xs@(Some _ None) `timesPositive` ys          = ys `timesPositive` xs
-- y is the last digit in a Positive number, so is not 0:
Some x xs'       `timesPositive` ys@(Some y None)
    = -- We could actually skip this test, and everything would
      -- turn out OK. We already play tricks like that in timesPositive.
      let zs = Some 0## (xs' `timesPositive` ys)
      in if x `eqWord#` 0##
         then zs
         else (x `timesDigit` y) `plusPositive` zs
Some x xs' `timesPositive` ys@(Some _ _)
    = (Some x None `timesPositive` ys) `plusPositive`
      Some 0## (xs' `timesPositive` ys)

{-
-- Requires arguments /= 0
Suppose we have 2n bits in a Word. Then
    x = 2^n xh + xl
    y = 2^n yh + yl
    x * y = (2^n xh + xl) * (2^n yh + yl)
          = 2^(2n) (xh yh)
          + 2^n    (xh yl)
          + 2^n    (xl yh)
          +        (xl yl)
                   ~~~~~~~ - all fit in 2n bits
-}
timesDigit :: Digit -> Digit -> Positive
timesDigit x y
 = case splitHalves x of
   (# xh, xl #) ->
    case splitHalves y of
    (# yh, yl #) ->
     case xh `timesWord#` yh of
     xhyh ->
      case splitHalves (xh `timesWord#` yl) of
      (# xhylh, xhyll #) ->
       case xhyll `uncheckedShiftL#` highHalfShift Unit of
       xhyll' ->
        case splitHalves (xl `timesWord#` yh) of
        (# xlyhh, xlyhl #) ->
         case xlyhl `uncheckedShiftL#` highHalfShift Unit of
         xlyhl' ->
          case xl `timesWord#` yl of
          xlyl ->
           -- Add up all the high word results. As the result fits in
           -- 4n bits this can't overflow.
           case xhyh `plusWord#` xhylh `plusWord#` xlyhh of
           high ->
           -- low: xhyll<<n + xlyhl<<n + xlyl
            -- From this point we might make (Some 0 None), but we know
            -- that the final result will be positive and the addition
            -- will work out OK, so everything will work out in the end.
            -- One thing we do need to be careful of is avoiding returning
            -- Some 0 (Some 0 None) + Some n None, as this will result in
            -- Some n (Some 0 None) instead of Some n None.
            let low = Some xhyll' None `plusPositive`
                      Some xlyhl' None `plusPositive`
                      Some xlyl   None
            in if high `eqWord#` 0##
               then low
               else Some 0## (Some high None) `plusPositive` low

splitHalves :: Digit -> (# {- High -} Digit, {- Low -} Digit #)
splitHalves x = (# x `uncheckedShiftRL#` highHalfShift Unit,
                   x `and#` lowHalfMask Unit #)

-- Assumes 0 <= i <= 31
shiftLPositive :: Positive -> Int# -> Positive
shiftLPositive None _ = None -- XXX Can't happen
shiftLPositive p i =
    case WORD_SIZE_IN_BITS# -# i of
    j -> let f carry None = if carry `eqWord#` 0##
                            then None
                            else Some carry None
             f carry (Some w ws) = case w `uncheckedShiftRL#` j of
                                   carry' ->
                                    case w `uncheckedShiftL#` i of
                                    me ->
                                     Some (me `or#` carry) (f carry' ws)
         in f 0## p

-- Long division
quotRemPositive :: Positive -> Positive -> (# Integer, Integer #)
xs `quotRemPositive` ys
    = case f xs of
      (# d, m #) -> (# digitsMaybeZeroToInteger d,
                       digitsMaybeZeroToInteger m #)
    where
          subtractors :: Positives
          subtractors = mkSubtractors (WORD_SIZE_IN_BITS# -# 1#)

          mkSubtractors n = if n ==# 0#
                            then Cons ys Nil
                            else Cons (ys `shiftLPositive` n)
                                      (mkSubtractors (n -# 1#))

          -- The main function. Go the the end of xs, then walk
          -- back trying to divide the number we accumulate by ys.
          f :: Positive -> (# Digits, Digits #)
          f None = (# None, None #)
          f (Some z zs)
              = case f zs of
                (# ds, m #) ->
                    let -- We need to avoid making (Some Zero None) here
                        m' = some z m
                    in case g 0## subtractors m' of
                       (# d, m'' #) ->
                        (# some d ds, m'' #)

          g :: Digit -> Positives -> Digits -> (# Digit, Digits #)
          g d Nil m = (# d, m #)
          g d (Cons sub subs) m
              = case d `uncheckedShiftL#` 1# of
                d' ->
                 case m `comparePositive` sub of
                 LT -> g d' subs m
                 _  -> g (d' `plusWord#` 1##)
                         subs
                         (m `minusPositive` sub)

some :: Digit -> Digits -> Digits
some w None = if w `eqWord#` 0## then None else Some w None
some w ws = Some w ws

andDigits :: Digits -> Digits -> Digits
andDigits _             None          = None
andDigits None          _             = None
andDigits (Some w1 ws1) (Some w2 ws2) = Some (w1 `and#` w2) (andDigits ws1 ws2)

orDigits :: Digits -> Digits -> Digits
orDigits None          ds            = ds
orDigits ds            None          = ds
orDigits (Some w1 ds1) (Some w2 ds2) = Some (w1 `or#` w2) (orDigits ds1 ds2)

xorDigits :: Digits -> Digits -> Digits
xorDigits None          ds            = ds
xorDigits ds            None          = ds
xorDigits (Some w1 ds1) (Some w2 ds2) = Some (w1 `xor#` w2) (xorDigits ds1 ds2)

-- XXX We'd really like word2Double# for this
doubleFromPositive :: Positive -> Double#
doubleFromPositive None = 0.0##
doubleFromPositive (Some w ds)
    = case splitHalves w of
      (# h, l #) ->
       (doubleFromPositive ds *## (2.0## **## WORD_SIZE_IN_BITS.0##))
       +## (int2Double# (word2Int# h) *##
              (2.0## **## int2Double# (highHalfShift Unit)))
       +## int2Double# (word2Int# l)

-- XXX We'd really like word2Float# for this
floatFromPositive :: Positive -> Float#
floatFromPositive None = 0.0#
floatFromPositive (Some w ds)
    = case splitHalves w of
      (# h, l #) ->
       (floatFromPositive ds `timesFloat#` (2.0# `powerFloat#` WORD_SIZE_IN_BITS.0#))
       `plusFloat#` (int2Float# (word2Int# h) `timesFloat#`
             (2.0# `powerFloat#` int2Float# (highHalfShift Unit)))
       `plusFloat#` int2Float# (word2Int# l)

#endif

