{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Time.Clock.System.Compat (
    systemEpochDay,
    SystemTime(..),
    truncateSystemTimeLeapSecond,
    getSystemTime,
    systemToUTCTime,
    utcToSystemTime,
    systemToTAITime,
    ) where

import Data.Time.Orphans ()

#if MIN_VERSION_time(1,8,0)
import Data.Time.Clock.System
#else

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Word (Word32)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Data.Time.Clock.TAI.Compat
import Data.Time.Clock.POSIX
import Data.Time.Compat

-- | 'SystemTime' is time returned by system clock functions.
-- Its semantics depends on the clock function, but the epoch is typically the beginning of 1970.
-- Note that 'systemNanoseconds' of 1E9 to 2E9-1 can be used to represent leap seconds.
data SystemTime = MkSystemTime
    { systemSeconds ::     {-# UNPACK #-} !Int64
    , systemNanoseconds :: {-# UNPACK #-} !Word32
    } deriving (Eq,Ord,Show,Typeable,Data)

instance NFData SystemTime where
    rnf a = a `seq` ()

-- | Get the system time, epoch start of 1970 UTC, leap-seconds ignored.
-- 'getSystemTime' is typically much faster than 'getCurrentTime'.
getSystemTime :: IO SystemTime


-- Use gettimeofday
getSystemTime = do
    t <- getPOSIXTime
    let secs = truncate t
    let nsecs = truncate $ 1000000000 * (t - fromIntegral secs)
    return (MkSystemTime secs nsecs)

-- | Map leap-second values to the start of the following second.
-- The resulting 'systemNanoseconds' will always be in the range 0 to 1E9-1.
truncateSystemTimeLeapSecond :: SystemTime -> SystemTime
truncateSystemTimeLeapSecond (MkSystemTime seconds nanoseconds) | nanoseconds >= 1000000000 = MkSystemTime (succ seconds) 0
truncateSystemTimeLeapSecond t = t

-- | Convert 'SystemTime' to 'UTCTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
systemToUTCTime :: SystemTime -> UTCTime
systemToUTCTime (MkSystemTime seconds nanoseconds) = let
    days :: Int64
    timeSeconds :: Int64
    (days, timeSeconds) = seconds `divMod` 86400

    day :: Day
    day = addDays (fromIntegral days) systemEpochDay

    timeNanoseconds :: Int64
    timeNanoseconds = timeSeconds * 1000000000 + (fromIntegral nanoseconds)

    timePicoseconds :: Int64
    timePicoseconds = timeNanoseconds * 1000

    time :: DiffTime
    time = picosecondsToDiffTime $ fromIntegral timePicoseconds
    in UTCTime day time

-- | Convert 'UTCTime' to 'SystemTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
utcToSystemTime :: UTCTime -> SystemTime
utcToSystemTime (UTCTime day time) = let
    days :: Int64
    days = fromIntegral $ diffDays day systemEpochDay

    timePicoseconds :: Int64
    timePicoseconds = fromIntegral $ diffTimeToPicoseconds time

    timeNanoseconds :: Int64
    timeNanoseconds = timePicoseconds `div` 1000

    timeSeconds :: Int64
    nanoseconds :: Int64
    (timeSeconds,nanoseconds) = if timeNanoseconds >= 86400000000000 then (86399,timeNanoseconds - 86399000000000) else timeNanoseconds `divMod` 1000000000

    seconds :: Int64
    seconds = days * 86400 + timeSeconds

    in MkSystemTime seconds $ fromIntegral nanoseconds

systemEpochAbsolute :: AbsoluteTime
systemEpochAbsolute = taiNominalDayStart systemEpochDay

-- | Convert 'SystemTime' to 'AbsoluteTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' TAI.
systemToTAITime :: SystemTime -> AbsoluteTime
systemToTAITime (MkSystemTime s ns) = let
    diff :: DiffTime
    diff = (fromIntegral s) + (fromIntegral ns) * 1E-9
    in addAbsoluteTime diff systemEpochAbsolute

-- | The day of the epoch of 'SystemTime', 1970-01-01
systemEpochDay :: Day
systemEpochDay = ModifiedJulianDay 40587

#endif
