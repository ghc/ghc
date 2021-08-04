{-# LANGUAGE CPP                #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
#endif
module Data.Time.Calendar.WeekDate.Compat (
    Year, WeekOfYear, DayOfWeek(..), dayOfWeek,
    FirstWeekType (..),
    toWeekCalendar,
    fromWeekCalendar,
    fromWeekCalendarValid,
    

    -- * ISO 8601 Week Date format
    toWeekDate,
    fromWeekDate,
#if __GLASGOW_HASKELL__ >= 710
    pattern YearWeekDay,
#endif
    fromWeekDateValid,
    showWeekDate,
) where

import Data.Time.Orphans ()

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

#if !MIN_VERSION_time(1,9,0)
import Data.Time.Format
#endif

#if !MIN_VERSION_time(1,11,0)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Time.Calendar.Types
import Data.Time.Calendar.Private
import Data.Time.Calendar.OrdinalDate
#endif

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))


#if !MIN_VERSION_time(1,11,0)
data FirstWeekType
    = FirstWholeWeek
    -- ^ first week is the first whole week of the year
    | FirstMostWeek
    -- ^ first week is the first week with four days in the year
    deriving (Eq, Typeable)

firstDayOfWeekCalendar :: FirstWeekType -> DayOfWeek -> Year -> Day
firstDayOfWeekCalendar wt dow year = let
    jan1st = fromOrdinalDate year 1
    in case wt of
        FirstWholeWeek -> firstDayOfWeekOnAfter dow jan1st
        FirstMostWeek -> firstDayOfWeekOnAfter dow $ addDays (-3) jan1st

-- Note that the year number matches the weeks, and so is not always the same as the Gregorian year number.
toWeekCalendar ::
    FirstWeekType
    -- ^ how to reckon the first week of the year
    -> DayOfWeek
    -- ^ the first day of each week
    -> Day
    -> (Year, WeekOfYear, DayOfWeek)
toWeekCalendar wt ws d = let
    dw = dayOfWeek d
    (y0,_) = toOrdinalDate d
    j1p = firstDayOfWeekCalendar wt ws $ pred y0
    j1 = firstDayOfWeekCalendar wt ws y0
    j1s = firstDayOfWeekCalendar wt ws $ succ y0
    in if d < j1
        then (pred y0,succ $ div (fromInteger $ diffDays d j1p) 7,dw)
        else if d < j1s then (y0,succ $ div (fromInteger $ diffDays d j1) 7,dw)
        else (succ y0,succ $ div (fromInteger $ diffDays d j1s) 7,dw)

-- | Convert from the given kind of "week calendar".
-- Invalid week and day values will be clipped to the correct range.
fromWeekCalendar ::
    FirstWeekType
    -- ^ how to reckon the first week of the year
    -> DayOfWeek
    -- ^ the first day of each week
    -> Year -> WeekOfYear -> DayOfWeek -> Day
fromWeekCalendar wt ws y wy dw = let
    d1 :: Day
    d1 = firstDayOfWeekCalendar wt ws y
    wy' = clip 1 53 wy
    getday :: WeekOfYear -> Day
    getday wy'' = addDays (toInteger $ (pred wy'' * 7) + (dayOfWeekDiff dw ws)) d1
    d1s = firstDayOfWeekCalendar wt ws $ succ y
    day = getday wy'
    in if wy' == 53 then if day >= d1s then getday 52 else day else day

-- | Convert from the given kind of "week calendar".
-- Invalid week and day values will return Nothing.
fromWeekCalendarValid ::
     FirstWeekType
    -- ^ how to reckon the first week of the year
    -> DayOfWeek
    -- ^ the first day of each week
    -> Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromWeekCalendarValid wt ws y wy dw = let
    d = fromWeekCalendar wt ws y wy dw
    in if toWeekCalendar wt ws d == (y,wy,dw) then Just d else Nothing

#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor for ISO 8601 Week Date format.
-- Invalid week values will be clipped to the correct range.
pattern YearWeekDay :: Year -> WeekOfYear -> DayOfWeek -> Day
pattern YearWeekDay y wy dw <- (toWeekDate -> (y,wy,toEnum -> dw)) where
    YearWeekDay y wy dw = fromWeekDate y wy (fromEnum dw)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearWeekDay #-}
#endif
#endif

#endif

#if !MIN_VERSION_time(1,9,0)

data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Ord, Show, Read, Typeable, Data)

instance NFData DayOfWeek where
    rnf !_ = ()

instance Hashable DayOfWeek where
    hashWithSalt salt = hashWithSalt salt . fromEnum

-- | \"Circular\", so for example @[Tuesday ..]@ gives an endless sequence.
-- Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday], and 'toEnum' performs mod 7 to give a cycle of days.
instance Enum DayOfWeek where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday = 1
    fromEnum Tuesday = 2
    fromEnum Wednesday = 3
    fromEnum Thursday = 4
    fromEnum Friday = 5
    fromEnum Saturday = 6
    fromEnum Sunday = 7
    enumFromTo wd1 wd2
        | wd1 == wd2 = [wd1]
    enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
    enumFromThenTo wd1 wd2 wd3
        | wd2 == wd3 = [wd1, wd2]
    enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3



-------------------------------------------------------------------------------
-- FormatTime DayOfWeek
-------------------------------------------------------------------------------

toSomeDay :: DayOfWeek -> Day
toSomeDay d = ModifiedJulianDay (fromIntegral $ fromEnum d + 4)

#if MIN_VERSION_time(1,8,0)
#define FORMAT_OPTS tl mpo i
#else
#define FORMAT_OPTS tl mpo
#endif

instance FormatTime DayOfWeek where
    formatCharacter 'u' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter 'u')
    formatCharacter 'w' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter 'w')
    formatCharacter 'a' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter 'a')
    formatCharacter 'A' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter 'A')
    formatCharacter _  = Nothing

#endif

#if !MIN_VERSION_time(1,11,0)
-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d
#endif
