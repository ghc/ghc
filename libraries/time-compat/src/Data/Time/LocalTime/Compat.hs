{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.Time.LocalTime.Compat (
    -- * Time zones
    TimeZone(..),timeZoneOffsetString,timeZoneOffsetString',minutesToTimeZone,hoursToTimeZone,utc,

    -- getting the locale time zone
    getTimeZone,getCurrentTimeZone,

    -- * Time of day
    TimeOfDay(..),midnight,midday,makeTimeOfDayValid,
    timeToDaysAndTimeOfDay,daysAndTimeOfDayToTime,
    utcToLocalTimeOfDay,localToUTCTimeOfDay,
    timeToTimeOfDay,timeOfDayToTime,
    dayFractionToTimeOfDay,timeOfDayToDayFraction,
    pastMidnight, sinceMidnight,

    -- * CalendarDiffTime
    CalendarDiffTime (..),
    calendarTimeDays, calendarTimeTime, scaleCalendarDiffTime,

    -- * Local Time
    LocalTime(..),

    addLocalTime,diffLocalTime,

    -- converting UTC and UT1 times to LocalTime
    utcToLocalTime,localTimeToUTC,ut1ToLocalTime,localTimeToUT1,

    -- * Zoned Time
    ZonedTime(..),utcToZonedTime,zonedTimeToUTC,getZonedTime,utcToLocalZonedTime,
    ) where

import Data.Time.Orphans ()

import Data.Time.LocalTime
import Data.Time.Clock.Compat
import Data.Time.Calendar.Compat

import Data.Fixed (Pico (..), showFixed, divMod')
import Data.Monoid (Monoid (..))
import Data.Data (Data, Typeable)
import Data.Semigroup (Semigroup (..))

import Control.DeepSeq (NFData (..))

-------------------------------------------------------------------------------
-- TimeOfDay
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,9,0)

-- | Convert a period of time into a count of days and a time of day since midnight.
-- The time of day will never have a leap second.
timeToDaysAndTimeOfDay :: NominalDiffTime -> (Integer,TimeOfDay)
timeToDaysAndTimeOfDay dt = let
    s = realToFrac dt
    (m,ms) = divMod' s 60
    (h,hm) = divMod' m 60
    (d,dh) = divMod' h 24
    in (d,TimeOfDay dh hm ms)

-- | Convert a count of days and a time of day since midnight into a period of time.
daysAndTimeOfDayToTime :: Integer -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d (TimeOfDay dh hm ms) = (+) (realToFrac ms) $ (*) 60 $ (+) (realToFrac hm) $ (*) 60 $ (+) (realToFrac dh) $ (*) 24 $ realToFrac d

#endif

#if !MIN_VERSION_time(1,10,0)
-- | Same as 'timeToTimeOfDay'.
pastMidnight :: DiffTime -> TimeOfDay
pastMidnight = timeToTimeOfDay

-- | Same as 'timeOfDayToTime'.
sinceMidnight :: TimeOfDay -> DiffTime
sinceMidnight = timeOfDayToTime
#endif

-------------------------------------------------------------------------------
-- CalendarDiffTime
-------------------------------------------------------------------------------

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,9,2)
deriving instance Typeable CalendarDiffTime
deriving instance Data CalendarDiffTime
#endif

#if !MIN_VERSION_time(1,9,2)

data CalendarDiffTime = CalendarDiffTime
    { ctMonths :: Integer
    , ctTime :: NominalDiffTime
    } deriving (Eq,
    Data
    ,Typeable
    )

-- | Additive
instance Semigroup CalendarDiffTime where
    CalendarDiffTime m1 d1 <> CalendarDiffTime m2 d2 = CalendarDiffTime (m1 + m2) (d1 + d2)

instance Monoid CalendarDiffTime where
    mempty = CalendarDiffTime 0 0
    mappend = (<>)

instance NFData CalendarDiffTime where
    rnf (CalendarDiffTime x y) = rnf x `seq` rnf y

instance Show CalendarDiffTime where
    show (CalendarDiffTime m t) = "P" ++ show m ++ "MT" ++ showFixed True (realToFrac t :: Pico) ++ "S"

calendarTimeDays :: CalendarDiffDays -> CalendarDiffTime
calendarTimeDays (CalendarDiffDays m d) = CalendarDiffTime m $ fromInteger d * nominalDay

calendarTimeTime :: NominalDiffTime -> CalendarDiffTime
calendarTimeTime dt = CalendarDiffTime 0 dt

-- | Scale by a factor. Note that @scaleCalendarDiffTime (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffTime :: Integer -> CalendarDiffTime -> CalendarDiffTime
scaleCalendarDiffTime k (CalendarDiffTime m d) = CalendarDiffTime (k * m) (fromInteger k * d)
#endif

-------------------------------------------------------------------------------
-- LocalTime
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,9,0)

-- | addLocalTime a b = a + b
addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

-- | diffLocalTime a b = a - b
diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

#endif
