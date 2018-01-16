{-# OPTIONS -fno-warn-unused-imports #-}
#include "HsConfigure.h"
-- #hide
module Data.Time.LocalTime.Internal.TimeOfDay
(
    -- * Time of day
    TimeOfDay(..),midnight,midday,makeTimeOfDayValid,
    utcToLocalTimeOfDay,localToUTCTimeOfDay,
    timeToTimeOfDay,timeOfDayToTime,
    dayFractionToTimeOfDay,timeOfDayToDayFraction
) where

import Control.DeepSeq
import Data.Typeable
import Data.Fixed
#if LANGUAGE_Rank2Types
import Data.Data
#endif
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Calendar.Private
import Data.Time.LocalTime.Internal.TimeZone


-- | Time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day.
data TimeOfDay = TimeOfDay {
    -- | range 0 - 23
    todHour    :: Int,
    -- | range 0 - 59
    todMin     :: Int,
    -- | Note that 0 <= 'todSec' < 61, accomodating leap seconds.
    -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
    todSec     :: Pico
} deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    ,Data, Typeable
#endif
#endif
#endif
    )

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = rnf h `seq` rnf m `seq` s `seq` () -- FIXME: Data.Fixed had no NFData instances yet at time of writing

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

-- | Hour twelve
midday :: TimeOfDay
midday = TimeOfDay 12 0 0

instance Show TimeOfDay where
    show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

makeTimeOfDayValid :: Int -> Int -> Pico -> Maybe TimeOfDay
makeTimeOfDayValid h m s = do
    _ <- clipValid 0 23 h
    _ <- clipValid 0 59 m
    _ <- clipValid 0 60.999999999999 s
    return (TimeOfDay h m s)

-- | Convert a time of day in UTC to a time of day in some timezone, together with a day adjustment.
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
    m' = m + timeZoneMinutes zone
    h' = h + (div m' 60)

-- | Convert a time of day in some timezone to a time of day in UTC, together with a day adjustment.
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate (timeZoneMinutes zone)))

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400

-- | Get the time of day given a time since midnight.
-- Time more than 24h will be converted to leap-seconds.
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt | dt >= posixDayLength = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDayLength)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s where
    s' = realToFrac dt
    s = mod' s' 60
    m' = div' s' 60
    m = mod' m' 60
    h = div' m' 60

-- | Get the time since midnight for a given time of day.
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | Get the time of day given the fraction of a day since midnight.
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

-- | Get the fraction of a day since midnight given a time of day.
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod) / realToFrac posixDayLength
