{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
#endif
module Data.Time.Calendar.Compat (
    -- * Days
    Day(..),addDays,diffDays,

    -- * CalendarDiffTime
    CalendarDiffDays (..),
    calendarDay,calendarWeek,calendarMonth,calendarYear,scaleCalendarDiffDays,

    -- * Gregorian calendar
    toGregorian,fromGregorian,fromGregorianValid,showGregorian,gregorianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addGregorianMonthsClip,addGregorianMonthsRollOver,
    addGregorianYearsClip,addGregorianYearsRollOver,
    addGregorianDurationClip,addGregorianDurationRollOver,
    diffGregorianDurationClip,diffGregorianDurationRollOver,

    -- re-exported from OrdinalDate
    isLeapYear ,

      -- * Week
    DayOfWeek(..), dayOfWeek,
    dayOfWeekDiff, firstDayOfWeekOnAfter,

    -- * Type aliases
    DayOfMonth, MonthOfYear, Year,
#if __GLASGOW_HASKELL__ >= 710
    pattern YearMonthDay,
#endif
    ) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Orphans ()

#if !MIN_VERSION_time(1,11,0)
import Data.Time.Calendar.Types
#endif

#if !MIN_VERSION_time(1,9,0)
import Data.Time.Calendar.WeekDate.Compat
#endif

#if !MIN_VERSION_time(1,5,0)
import System.Locale (TimeLocale (..))
#endif

import Control.DeepSeq (NFData (..))
import Data.Data       (Data, Typeable)
import Data.Monoid     (Monoid (..))
import Data.Semigroup  (Semigroup (..))

-------------------------------------------------------------------------------
-- CalendarDiffTime
-------------------------------------------------------------------------------

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,9,2)
deriving instance Typeable CalendarDiffDays
deriving instance Data CalendarDiffDays
#endif

#if !MIN_VERSION_time(1,9,0)

data CalendarDiffDays = CalendarDiffDays
    { cdMonths :: Integer
    , cdDays :: Integer
    } deriving (Eq,
    Data
#if __GLASGOW_HASKELL__ >= 802
#endif
    ,Typeable
#if __GLASGOW_HASKELL__ >= 802
#endif
    )

-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)

-- | Additive
instance Monoid CalendarDiffDays where
    mempty  = CalendarDiffDays 0 0
    mappend = (<>)

instance Show CalendarDiffDays where
    show (CalendarDiffDays m d) = "P" ++ show m ++ "M" ++ show d ++ "D"

instance NFData CalendarDiffDays where
    rnf (CalendarDiffDays x y) = rnf x `seq` rnf y

calendarDay :: CalendarDiffDays
calendarDay = CalendarDiffDays 0 1

calendarWeek :: CalendarDiffDays
calendarWeek = CalendarDiffDays 0 7

calendarMonth :: CalendarDiffDays
calendarMonth = CalendarDiffDays 1 0

calendarYear :: CalendarDiffDays
calendarYear = CalendarDiffDays 12 0

-- | Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (CalendarDiffDays m d) = CalendarDiffDays (k * m) (k * d)

#endif

-------------------------------------------------------------------------------
-- Gregorian
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,9,0)

-- | Add months (clipped to last day), then add days
addGregorianDurationClip :: CalendarDiffDays -> Day -> Day
addGregorianDurationClip (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addGregorianDurationRollOver :: CalendarDiffDays -> Day -> Day
addGregorianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffGregorianDurationClip :: Day -> Day -> CalendarDiffDays
diffGregorianDurationClip day2 day1 = let
    (y1,m1,d1) = toGregorian day1
    (y2,m2,d2) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addGregorianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

-- | Calendrical difference, with as many whole months as possible.
-- Same as 'diffGregorianDurationClip' for positive durations.
diffGregorianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffGregorianDurationRollOver day2 day1 = let
    (y1,m1,d1) = toGregorian day1
    (y2,m2,d2) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addGregorianDurationRollOver (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

#endif

#if !MIN_VERSION_time(1,11,0)
#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor for the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range, month first, then day.
pattern YearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern YearMonthDay y m d <- (toGregorian -> (y,m,d)) where
    YearMonthDay y m d = fromGregorian y m d

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearMonthDay #-}
#endif
#endif
#endif

-------------------------------------------------------------------------------
-- DayOfWeek
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,11,0)
-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d
#endif
