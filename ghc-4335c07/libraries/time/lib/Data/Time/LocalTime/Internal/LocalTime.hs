{-# OPTIONS -fno-warn-orphans #-}
#include "HsConfigure.h"

-- #hide
module Data.Time.LocalTime.Internal.LocalTime
(
    -- * Local Time
    LocalTime(..),

    -- converting UTC and UT1 times to LocalTime
    utcToLocalTime,localTimeToUTC,ut1ToLocalTime,localTimeToUT1,
) where



import Control.DeepSeq
import Data.Typeable
#if LANGUAGE_Rank2Types
import Data.Data
#endif
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.TimeZone


-- | A simple day and time aggregate, where the day is of the specified parameter,
-- and the time is a TimeOfDay.
-- Conversion of this (as local civil time) to UTC depends on the time zone.
-- Conversion of this (as local mean time) to UT1 depends on the longitude.
data LocalTime = LocalTime {
    localDay    :: Day,
    localTimeOfDay   :: TimeOfDay
} deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    ,Data, Typeable
#endif
#endif
#endif
    )

instance NFData LocalTime where
    rnf (LocalTime d t) = rnf d `seq` rnf t `seq` ()

instance Show LocalTime where
    show (LocalTime d t) = (showGregorian d) ++ " " ++ (show t)

-- | Get the local time of a UTC time in a time zone.
utcToLocalTime :: TimeZone -> UTCTime -> LocalTime
utcToLocalTime tz (UTCTime day dt) = LocalTime (addDays i day) tod where
    (i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

-- | Get the UTC time of a local time in a time zone.
localTimeToUTC :: TimeZone -> LocalTime -> UTCTime
localTimeToUTC tz (LocalTime day tod) = UTCTime (addDays i day) (timeOfDayToTime todUTC) where
    (i,todUTC) = localToUTCTimeOfDay tz tod

-- | Get the local time of a UT1 time on a particular meridian (in degrees, positive is East).
ut1ToLocalTime :: Rational -> UniversalTime -> LocalTime
ut1ToLocalTime long (ModJulianDate date) = LocalTime (ModifiedJulianDay localMJD) (dayFractionToTimeOfDay localToDOffset) where
    localTime = date + long / 360 :: Rational
    localMJD = floor localTime
    localToDOffset = localTime - (fromIntegral localMJD)

-- | Get the UT1 time of a local time on a particular meridian (in degrees, positive is East).
localTimeToUT1 :: Rational -> LocalTime -> UniversalTime
localTimeToUT1 long (LocalTime (ModifiedJulianDay localMJD) tod) = ModJulianDate ((fromIntegral localMJD) + (timeOfDayToDayFraction tod) - (long / 360))

-- orphan instance
instance Show UniversalTime where
    show t = show (ut1ToLocalTime 0 t)
