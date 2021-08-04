{-# LANGUAGE CPP #-}
module Data.Time.Clock.Compat (
    -- * Universal Time
    -- | Time as measured by the Earth.
    UniversalTime(..),

    -- * Absolute intervals, DiffTime
    DiffTime,
    secondsToDiffTime,
    picosecondsToDiffTime,
    diffTimeToPicoseconds,

    -- * UTCTime
    UTCTime (..),

    -- * NominalDiffTime
    NominalDiffTime,
    secondsToNominalDiffTime,
    nominalDiffTimeToSeconds,
    nominalDay,

    -- * UTC differences
    addUTCTime,
    diffUTCTime,
  
    -- * Current time
    getCurrentTime,
    getTime_resolution,

    -- * Type aliases
    Year,
    MonthOfYear,
    DayOfMonth,
    ) where

import Data.Time.Orphans ()
import Data.Time.Calendar.Types

import Data.Time.Clock
import Data.Fixed (Pico)

#if !MIN_VERSION_time(1,9,1)

-- | Create a 'NominalDiffTime' from a number of seconds.
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

-- | Get the seconds in a 'NominalDiffTime'.
nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac

#endif

#if !MIN_VERSION_time(1,8,0)
-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400
#endif

#if !MIN_VERSION_time(1,8,0)
-- | The resolution of 'getSystemTime', 'getCurrentTime', 'getPOSIXTime'
getTime_resolution :: DiffTime
getTime_resolution = 1E-6 -- microsecond
#endif

#if !MIN_VERSION_time(1,6,0)
-- | Get the number of picoseconds in a 'DiffTime'.
diffTimeToPicoseconds :: DiffTime -> Integer
diffTimeToPicoseconds = truncate . (1000000000000 *)
#endif
