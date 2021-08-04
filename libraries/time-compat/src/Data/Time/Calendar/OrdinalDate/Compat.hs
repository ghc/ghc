{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif
module Data.Time.Calendar.OrdinalDate.Compat (
    Day, Year, DayOfYear, WeekOfYear,
    toOrdinalDate,
    fromOrdinalDate,
#if __GLASGOW_HASKELL__ >= 710
    pattern YearDay,
#endif
    fromOrdinalDateValid,
    showOrdinalDate,
    isLeapYear,
    mondayStartWeek,
    sundayStartWeek,
    fromMondayStartWeek,
    fromMondayStartWeekValid,
    fromSundayStartWeek,
    fromSundayStartWeekValid,
) where

import Data.Time.Orphans ()

import Data.Time.Calendar.OrdinalDate hiding (fromSundayStartWeekValid)
#if MIN_VERSION_time(1,6,0)
import Data.Time.Calendar.OrdinalDate (fromSundayStartWeekValid)
#else
import Data.Time.Calendar.Private
#endif

#if !MIN_VERSION_time(1,11,0)
import Data.Time.Calendar
import Data.Time.Calendar.Types
#endif

#if !MIN_VERSION_time(1,11,0)

#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor for ISO 8601 Ordinal Date format.
-- Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
pattern YearDay :: Year -> DayOfYear -> Day
pattern YearDay y d <- (toOrdinalDate -> (y,d)) where
    YearDay y d = fromOrdinalDate y d

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearDay #-}
#endif
#endif

#endif

#if !MIN_VERSION_time(1,6,0)
fromSundayStartWeekValid ::
       Year -- ^ Year.
    -> WeekOfYear -- ^ Sunday-starting week number (as @%U@ in 'Data.Time.Format.formatTime').
    -> Int -- ^ Day of week.
                               -- Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
    -> Maybe Day
fromSundayStartWeekValid year w d = do
    d' <- clipValid 0 6 d
    let
        -- first day of the year
        firstDay = fromOrdinalDate year 1
        -- 0-based week of year
        zbFirstSunday = (4 - toModifiedJulianDay firstDay) `mod` 7
        -- 0-based week number
        zbWeek = w - 1
        -- 0-based day of week
        zbDay = d'
        -- 0-based day in year
        zbYearDay = zbFirstSunday + 7 * toInteger zbWeek + toInteger zbDay
    zbYearDay' <-
        clipValid
            0
            (if isLeapYear year
                 then 365
                 else 364)
            zbYearDay
    return $ addDays zbYearDay' firstDay
#endif
