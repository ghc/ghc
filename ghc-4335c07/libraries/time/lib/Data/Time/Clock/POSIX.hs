-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime,
    systemToPOSIXTime,
) where

import Data.Time.Clock.Internal.POSIXTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.System
import Data.Time.Calendar.Days
import Data.Fixed

posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime i = let
    (d,t) = divMod' i posixDayLength
 in UTCTime (addDays d systemEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d systemEpochDay) * posixDayLength) + min posixDayLength (realToFrac t)

systemToPOSIXTime :: SystemTime -> POSIXTime
systemToPOSIXTime (MkSystemTime s ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9

-- | Get the current POSIX time from the system clock.
getPOSIXTime :: IO POSIXTime
getPOSIXTime = fmap systemToPOSIXTime getSystemTime

-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = systemToUTCTime `fmap` getSystemTime
