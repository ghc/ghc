{-# OPTIONS -fno-warn-orphans #-}
#include "HsConfigure.h"

-- #hide
module Data.Time.LocalTime.Internal.ZonedTime
(
    ZonedTime(..),utcToZonedTime,zonedTimeToUTC,getZonedTime,utcToLocalZonedTime
) where

import Control.DeepSeq
import Data.Typeable
#if LANGUAGE_Rank2Types
import Data.Data
#endif

import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.LocalTime


-- | A local time together with a time zone.
data ZonedTime = ZonedTime {
    zonedTimeToLocalTime :: LocalTime,
    zonedTimeZone :: TimeZone
}
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    deriving (Data, Typeable)
#endif
#endif
#endif

instance NFData ZonedTime where
    rnf (ZonedTime lt z) = rnf lt `seq` rnf z `seq` ()

utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime zone time = ZonedTime (utcToLocalTime zone time) zone

zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC (ZonedTime t zone) = localTimeToUTC zone t

instance Show ZonedTime where
    show (ZonedTime t zone) = show t ++ " " ++ show zone

-- orphan instance
instance Show UTCTime where
    show t = show (utcToZonedTime utc t)

getZonedTime :: IO ZonedTime
getZonedTime = do
    t <- getCurrentTime
    zone <- getTimeZone t
    return (utcToZonedTime zone t)

-- |
utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime t = do
    zone <- getTimeZone t
    return (utcToZonedTime zone t)
