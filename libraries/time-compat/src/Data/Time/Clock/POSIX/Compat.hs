{-# LANGUAGE CPP #-}
module Data.Time.Clock.POSIX.Compat (
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime,
    systemToPOSIXTime,
    ) where

import Data.Time.Orphans ()

import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.System.Compat

#if !MIN_VERSION_time(1,8,0)
systemToPOSIXTime :: SystemTime -> POSIXTime
systemToPOSIXTime (MkSystemTime s ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9
#endif
