{-# OPTIONS -fno-warn-orphans #-}
#include "HsConfigure.h"
-- | TAI and leap-second maps for converting to UTC: most people won't need this module.
module Data.Time.Clock.TAI
(
    -- TAI arithmetic
    module Data.Time.Clock.Internal.AbsoluteTime,

    -- leap-second map type
    LeapSecondMap,

    -- conversion between UTC and TAI with map
    utcDayLength,utcToTAITime,taiToUTCTime,

    taiClock,
) where

import Data.Time.Clock.Internal.AbsoluteTime
import Data.Time.LocalTime
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.SystemTime
import Data.Time.Clock.System
import Data.Time.Clock
import Data.Maybe
import Data.Fixed

instance Show AbsoluteTime where
    show t = show (utcToLocalTime utc (fromJust (taiToUTCTime (const (Just 0)) t))) ++ " TAI" -- ugly, but standard apparently

-- | TAI - UTC during this day.
-- No table is provided, as any program compiled with it would become
-- out of date in six months.
type LeapSecondMap = Day -> Maybe Int

utcDayLength :: LeapSecondMap -> Day -> Maybe DiffTime
utcDayLength lsmap day = do
    i0 <- lsmap day
    i1 <- lsmap $ addDays 1 day
    return $ realToFrac (86400 + i1 - i0)

dayStart :: LeapSecondMap -> Day -> Maybe AbsoluteTime
dayStart lsmap day = do
    i <- lsmap day
    return $ addAbsoluteTime (realToFrac $ (toModifiedJulianDay day) * 86400 + toInteger i) taiEpoch

utcToTAITime :: LeapSecondMap -> UTCTime -> Maybe AbsoluteTime
utcToTAITime lsmap (UTCTime day dtime) = do
    t <- dayStart lsmap day
    return $ addAbsoluteTime dtime t

taiToUTCTime :: LeapSecondMap -> AbsoluteTime -> Maybe UTCTime
taiToUTCTime lsmap abstime = let
    stable day = do
        dayt <- dayStart lsmap day
        len <- utcDayLength lsmap day
        let
            dtime = diffAbsoluteTime abstime dayt
            day' = addDays (div' dtime len) day
        if day == day' then return (UTCTime day dtime) else stable day'
    in stable $ ModifiedJulianDay $ div' (diffAbsoluteTime abstime taiEpoch) 86400

-- | TAI clock, if it exists. Note that it is unlikely to be set correctly, without due care and attention.
taiClock :: Maybe (DiffTime,IO AbsoluteTime)
taiClock = fmap (fmap (fmap systemToTAITime)) getTAISystemTime
