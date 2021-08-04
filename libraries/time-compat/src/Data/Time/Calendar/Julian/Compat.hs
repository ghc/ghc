{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
#endif
module Data.Time.Calendar.Julian.Compat (
    Year, MonthOfYear, DayOfMonth, DayOfYear,

    -- JulianYearDay
    toJulianYearAndDay,
    fromJulianYearAndDay,
    fromJulianYearAndDayValid,
    showJulianYearAndDay,
    isJulianLeapYear,

    toJulian,fromJulian,
#if __GLASGOW_HASKELL__ >= 710
    pattern JulianYearMonthDay,
#endif
    fromJulianValid,showJulian,julianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addJulianMonthsClip,addJulianMonthsRollOver,
    addJulianYearsClip,addJulianYearsRollOver,
    addJulianDurationClip,addJulianDurationRollOver,
    diffJulianDurationClip,diffJulianDurationRollOver,
) where

import Data.Time.Orphans ()

import Data.Time.Calendar.Julian
import Data.Time.Calendar.Compat

#if !MIN_VERSION_time(1,11,0)
import Data.Time.Calendar.Types
#endif

#if !MIN_VERSION_time(1,9,0)

-- | Add months (clipped to last day), then add days
addJulianDurationClip :: CalendarDiffDays -> Day -> Day
addJulianDurationClip (CalendarDiffDays m d) day = addDays d $ addJulianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addJulianDurationRollOver :: CalendarDiffDays -> Day -> Day
addJulianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addJulianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffJulianDurationClip :: Day -> Day -> CalendarDiffDays
diffJulianDurationClip day2 day1 = let
    (y1,m1,d1) = toJulian day1
    (y2,m2,d2) = toJulian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addJulianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

-- | Calendrical difference, with as many whole months as possible.
-- Same as 'diffJulianDurationClip' for positive durations.
diffJulianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffJulianDurationRollOver day2 day1 = let
    (y1,m1,d1) = toJulian day1
    (y2,m2,d2) = toJulian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addJulianDurationRollOver (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

#endif

#if !MIN_VERSION_time(1,11,0)
#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor for the proleptic Julian calendar.
-- Invalid values will be clipped to the correct range, month first, then day.
pattern JulianYearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern JulianYearMonthDay y m d <- (toJulian -> (y,m,d)) where
    JulianYearMonthDay y m d = fromJulian y m d

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE JulianYearMonthDay #-}
#endif
#endif
#endif
