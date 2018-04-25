{-# OPTIONS -fno-warn-orphans #-}

-- #hide
module Data.Time.Calendar.Gregorian
(
    -- * Gregorian calendar
    toGregorian,fromGregorian,fromGregorianValid,showGregorian,gregorianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addGregorianMonthsClip,addGregorianMonthsRollOver,
    addGregorianYearsClip,addGregorianYearsRollOver,

    -- re-exported from OrdinalDate
    isLeapYear
) where

import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

-- | Convert to proleptic Gregorian calendar. First element of result is year, second month number (1-12), third day (1-31).
toGregorian :: Day -> (Integer,Int,Int)
toGregorian date = (year,month,day) where
    (year,yd) = toOrdinalDate date
    (month,day) = dayOfYearToMonthAndDay (isLeapYear year) yd

-- | Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will be clipped to the correct range, month first, then day.
fromGregorian :: Integer -> Int -> Int -> Day
fromGregorian year month day = fromOrdinalDate year (monthAndDayToDayOfYear (isLeapYear year) month day)

-- | Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will return Nothing
fromGregorianValid :: Integer -> Int -> Int -> Maybe Day
fromGregorianValid year month day = do
    doy <- monthAndDayToDayOfYearValid (isLeapYear year) month day
    fromOrdinalDateValid year doy

-- | Show in ISO 8601 format (yyyy-mm-dd)
showGregorian :: Day -> String
showGregorian date = (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d) where
    (y,m,d) = toGregorian date

-- | The number of days in a given month according to the proleptic Gregorian calendar. First argument is year, second is month.
gregorianMonthLength :: Integer -> Int -> Int
gregorianMonthLength year = monthLength (isLeapYear year)

rolloverMonths :: (Integer,Integer) -> (Integer,Int)
rolloverMonths (y,m) = (y + (div (m - 1) 12),fromIntegral (mod (m - 1) 12) + 1)

addGregorianMonths :: Integer -> Day -> (Integer,Int,Int)
addGregorianMonths n day = (y',m',d) where
    (y,m,d) = toGregorian day
    (y',m') = rolloverMonths (y,fromIntegral m + n)

-- | Add months, with days past the last day of the month clipped to the last day.
-- For instance, 2005-01-30 + 1 month = 2005-02-28.
addGregorianMonthsClip :: Integer -> Day -> Day
addGregorianMonthsClip n day = fromGregorian y m d where
    (y,m,d) = addGregorianMonths n day

-- | Add months, with days past the last day of the month rolling over to the next month.
-- For instance, 2005-01-30 + 1 month = 2005-03-02.
addGregorianMonthsRollOver :: Integer -> Day -> Day
addGregorianMonthsRollOver n day = addDays (fromIntegral d - 1) (fromGregorian y m 1) where
    (y,m,d) = addGregorianMonths n day

-- | Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
-- For instance, 2004-02-29 + 2 years = 2006-02-28.
addGregorianYearsClip :: Integer -> Day -> Day
addGregorianYearsClip n = addGregorianMonthsClip (n * 12)

-- | Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
-- For instance, 2004-02-29 + 2 years = 2006-03-01.
addGregorianYearsRollOver :: Integer -> Day -> Day
addGregorianYearsRollOver n = addGregorianMonthsRollOver (n * 12)

-- orphan instance
instance Show Day where
    show = showGregorian
