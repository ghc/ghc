{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:      Data.Aeson.Encoding.Builder
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2013 Simon Meier <iridcode@gmail.com>
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value using the UTF-8 encoding.

module Data.Aeson.Encoding.Builder
    (
      encodeToBuilder
    , null_
    , bool
    , array
    , emptyArray_
    , emptyObject_
    , object
    , text
    , string
    , unquoted
    , quote
    , scientific
    , day
    , month
    , quarter
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    , ascii2
    , ascii4
    , ascii5
    ) where

import Prelude.Compat

import Data.Aeson.Internal.Time
import Data.Aeson.Types.Internal (Value (..))
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as BP
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Char (chr, ord)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text.Encoding (encodeUtf8BuilderEscaped)
import Data.Time (UTCTime(..))
import Data.Time.Calendar (Day(..), toGregorian)
import Data.Time.Calendar.Month.Compat (Month, toYearMonth)
import Data.Time.Calendar.Quarter.Compat (Quarter, toYearQuarter, QuarterOfYear (..))
import Data.Time.LocalTime
import Data.Word (Word8)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Encode a JSON value to a "Data.ByteString" 'B.Builder'.
--
-- Use this function if you are encoding over the wire, or need to
-- prepend or append further bytes to the encoded JSON value.
encodeToBuilder :: Value -> Builder
encodeToBuilder Null       = null_
encodeToBuilder (Bool b)   = bool b
encodeToBuilder (Number n) = scientific n
encodeToBuilder (String s) = text s
encodeToBuilder (Array v)  = array v
encodeToBuilder (Object m) = object m

-- | Encode a JSON null.
null_ :: Builder
null_ = BP.primBounded (ascii4 ('n',('u',('l','l')))) ()

-- | Encode a JSON boolean.
bool :: Bool -> Builder
bool = BP.primBounded (BP.condB id (ascii4 ('t',('r',('u','e'))))
                                   (ascii5 ('f',('a',('l',('s','e'))))))

-- | Encode a JSON array.
array :: V.Vector Value -> Builder
array v
  | V.null v  = emptyArray_
  | otherwise = B.char8 '[' <>
                encodeToBuilder (V.unsafeHead v) <>
                V.foldr withComma (B.char8 ']') (V.unsafeTail v)
  where
    withComma a z = B.char8 ',' <> encodeToBuilder a <> z

-- Encode a JSON object.
object :: HMS.HashMap T.Text Value -> Builder
object m = case HMS.toList m of
    (x:xs) -> B.char8 '{' <> one x <> foldr withComma (B.char8 '}') xs
    _      -> emptyObject_
  where
    withComma a z = B.char8 ',' <> one a <> z
    one (k,v)     = text k <> B.char8 ':' <> encodeToBuilder v

-- | Encode a JSON string.
text :: T.Text -> Builder
text t = B.char8 '"' <> unquoted t <> B.char8 '"'

-- | Encode a JSON string, without enclosing quotes.
unquoted :: T.Text -> Builder
unquoted = encodeUtf8BuilderEscaped escapeAscii

-- | Add quotes surrounding a builder
quote :: Builder -> Builder
quote b = B.char8 '"' <> b <> B.char8 '"'

-- | Encode a JSON string.
string :: String -> Builder
string t = B.char8 '"' <> BP.primMapListBounded go t <> B.char8 '"'
  where go = BP.condB (> '\x7f') BP.charUtf8 (c2w >$< escapeAscii)

escapeAscii :: BP.BoundedPrim Word8
escapeAscii =
    BP.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
    BP.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
    BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
    BP.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
    BP.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
    BP.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
    BP.liftFixedToBounded hexEscape -- fallback for chars < 0x20
  where
    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 >*< BP.char8 >*< BP.word16HexFixed
{-# INLINE escapeAscii #-}

c2w :: Char -> Word8
c2w c = fromIntegral (ord c)

-- | Encode a JSON number.
scientific :: Scientific -> Builder
scientific s
    | e < 0 || e > 1024 = scientificBuilder s
    | otherwise = B.integerDec (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

emptyArray_ :: Builder
emptyArray_ = BP.primBounded (ascii2 ('[',']')) ()

emptyObject_ :: Builder
emptyObject_ = BP.primBounded (ascii2 ('{','}')) ()

ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ const cs BP.>$< BP.char7 >*< BP.char7
{-# INLINE ascii2 #-}

ascii3 :: (Char, (Char, Char)) -> BP.BoundedPrim a
ascii3 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii3 #-}

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii5 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}

ascii8 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char)))))))
       -> BP.BoundedPrim a
ascii8 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii8 #-}

day :: Day -> Builder
day dd = encodeYear yr <>
         BP.primBounded (ascii6 ('-',(mh,(ml,('-',(dh,dl)))))) ()
  where (yr,m,d)    = toGregorian dd
        !(T mh ml)  = twoDigits m
        !(T dh dl)  = twoDigits d
{-# INLINE day #-}

month :: Month -> Builder
month mm = encodeYear yr <>
           BP.primBounded (ascii3 ('-',(mh,ml))) ()
  where (yr,m) = toYearMonth mm
        !(T mh ml) = twoDigits m
{-# INLINE month #-}

quarter :: Quarter -> Builder
quarter qq = encodeYear yr <>
             BP.primBounded (ascii3 ('-',('q',qd))) ()
  where (yr,q) = toYearQuarter qq
        qd = case q of
            Q1 -> '1'
            Q2 -> '2'
            Q3 -> '3'
            Q4 -> '4'
{-# INLINE quarter #-}

-- | Used in encoding day, month, quarter
encodeYear :: Integer -> Builder
encodeYear y
    | y >= 1000 = B.integerDec y
    | y >= 0    = BP.primBounded (ascii4 (padYear y)) ()
    | y >= -999 = BP.primBounded (ascii5 ('-',padYear (- y))) ()
    | otherwise = B.integerDec y
  where
    padYear y' =
        let (ab,c) = fromIntegral y' `quotRem` 10
            (a,b)  = ab `quotRem` 10
        in ('0',(digit a,(digit b,digit c)))
{-# INLINE encodeYear #-}

timeOfDay :: TimeOfDay -> Builder
timeOfDay t = timeOfDay64 (toTimeOfDay64 t)
{-# INLINE timeOfDay #-}

timeOfDay64 :: TimeOfDay64 -> Builder
timeOfDay64 (TOD h m s)
  | frac == 0 = hhmmss -- omit subseconds if 0
  | otherwise = hhmmss <> BP.primBounded showFrac frac
  where
    hhmmss  = BP.primBounded (ascii8 (hh,(hl,(':',(mh,(ml,(':',(sh,sl)))))))) ()
    !(T hh hl)  = twoDigits h
    !(T mh ml)  = twoDigits m
    !(T sh sl)  = twoDigits (fromIntegral real)
    (real,frac) = s `quotRem` pico
    showFrac = ('.',) >$< (BP.liftFixedToBounded BP.char7 >*< trunc12)
    trunc12 = (`quotRem` micro) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc6) (digits6 >*< trunc6)
    digits6 = ((`quotRem` milli) . fromIntegral) >$< (digits3 >*< digits3)
    trunc6  = ((`quotRem` milli) . fromIntegral) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc3) (digits3 >*< trunc3)
    digits3 = (`quotRem` 10) >$< (digits2 >*< digits1)
    digits2 = (`quotRem` 10) >$< (digits1 >*< digits1)
    digits1 = BP.liftFixedToBounded (digit >$< BP.char7)
    trunc3  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 100) >$< (digits1 >*< trunc2)
    trunc2  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 10)  >$< (digits1 >*< trunc1)
    trunc1  = BP.condB (== 0) BP.emptyB digits1

    pico       = 1000000000000 -- number of picoseconds  in 1 second
    micro      =       1000000 -- number of microseconds in 1 second
    milli      =          1000 -- number of milliseconds in 1 second

timeZone :: TimeZone -> Builder
timeZone (TimeZone off _ _)
  | off == 0  = B.char7 'Z'
  | otherwise = BP.primBounded (ascii6 (s,(hh,(hl,(':',(mh,ml)))))) ()
  where !s         = if off < 0 then '-' else '+'
        !(T hh hl) = twoDigits h
        !(T mh ml) = twoDigits m
        (h,m)      = abs off `quotRem` 60
{-# INLINE timeZone #-}

dayTime :: Day -> TimeOfDay64 -> Builder
dayTime d t = day d <> B.char7 'T' <> timeOfDay64 t
{-# INLINE dayTime #-}

utcTime :: UTCTime -> B.Builder
utcTime (UTCTime d s) = dayTime d (diffTimeOfDay64 s) <> B.char7 'Z'
{-# INLINE utcTime #-}

localTime :: LocalTime -> Builder
localTime (LocalTime d t) = dayTime d (toTimeOfDay64 t)
{-# INLINE localTime #-}

zonedTime :: ZonedTime -> Builder
zonedTime (ZonedTime t z) = localTime t <> timeZone z
{-# INLINE zonedTime #-}

data T = T {-# UNPACK #-} !Char {-# UNPACK #-} !Char

twoDigits :: Int -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10

digit :: Int -> Char
digit x = chr (x + 48)
