{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Data.Aeson.Parser.Time
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Parsers for parsing dates and times.

module Data.Attoparsec.Time
    (
      day
    , localTime
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    , month
    , quarter
    ) where

import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Attoparsec.Text as A
import Data.Attoparsec.Time.Internal (toPico)
import Data.Bits ((.&.))
import Data.Char (isDigit, ord)
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..), fromYearQuarter)
import Data.Time.Calendar.Month.Compat (Month, fromYearMonthValid)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Local

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
day :: Parser Day
day = do
  absOrNeg <- negate <$ char '-' <|> id <$ char '+' <|> pure id
  y <- (decimal <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  m <- (twoDigits <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  d <- twoDigits <|> fail "date must be of form [+,-]YYYY-MM-DD"
  maybe (fail "invalid date") return (fromGregorianValid (absOrNeg y) m d)

-- | Parse a month of the form @[+,-]YYYY-MM@.
month :: Parser Month
month = do
  absOrNeg <- negate <$ char '-' <|> id <$ char '+' <|> pure id
  y <- (decimal <* char '-') <|> fail "month must be of form [+,-]YYYY-MM"
  m <- twoDigits <|> fail "month must be of form [+,-]YYYY-MM"
  maybe (fail "invalid month") return (fromYearMonthValid (absOrNeg y) m)

-- | Parse a quarter of the form @[+,-]YYYY-QN@.
quarter :: Parser Quarter
quarter = do
  absOrNeg <- negate <$ char '-' <|> id <$ char '+' <|> pure id
  y <- (decimal <* char '-') <|> fail "month must be of form [+,-]YYYY-MM"
  _ <- char 'q' <|> char 'Q'
  q <- parseQ
  return $! fromYearQuarter (absOrNeg y) q
  where
    parseQ = Q1 <$ char '1'
      <|> Q2 <$ char '2'
      <|> Q3 <$ char '3'
      <|> Q4 <$ char '4'

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
  h <- twoDigits
  m <- char ':' *> twoDigits
  s <- option 0 (char ':' *> seconds)
  if h < 24 && m < 60 && s < 61
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit
      return $! parsePicos real t
    _ -> return $! fromIntegral real
 where
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where T n t'  = T.foldl' step (T 12 (fromIntegral a0)) t
          step ma@(T m a) c
              | m <= 0    = ma
              | otherwise = T (m-1) (10 * a + fromIntegral (ord c) .&. 15)

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe Local.TimeZone)
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      mm <- peekChar
      m <- case mm of
             Just ':'           -> anyChar *> twoDigits
             Just d | isDigit d -> twoDigits
             _                  -> return 0
      let off | ch == '-' = negate off0
              | otherwise = off0
          off0 = h * 60 + m
      case undefined of
        _   | off == 0 ->
              return Nothing
            | off < -720 || off > 840 || m > 59 ->
              fail "invalid time zone offset"
            | otherwise ->
              let !tz = Local.minutesToTimeZone off
              in return (Just tz)

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@.  The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Parser Local.LocalTime
localTime = Local.LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = satisfy (\c -> c == 'T' || c == ' ')

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
utcTime :: Parser UTCTime
utcTime = do
  lt@(Local.LocalTime d t) <- localTime
  mtz <- timeZone
  case mtz of
    Nothing -> let !tt = Local.timeOfDayToTime t
               in return (UTCTime d tt)
    Just tz -> return $! Local.localTimeToUTC tz lt

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Parser Local.ZonedTime
zonedTime = Local.ZonedTime <$> localTime <*> (fromMaybe utc <$> timeZone)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""
