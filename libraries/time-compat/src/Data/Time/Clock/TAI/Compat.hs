{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.Time.Clock.TAI.Compat (
    -- * TAI arithmetic
    AbsoluteTime,taiEpoch,addAbsoluteTime,diffAbsoluteTime,
    taiNominalDayStart,

    -- * leap-second map type
    LeapSecondMap,

    -- * conversion between UTC and TAI with map
#if MIN_VERSION_time(1,7,0)
    T.utcDayLength,T.utcToTAITime,T.taiToUTCTime,
#else
    utcDayLength,utcToTAITime,taiToUTCTime,
#endif

    taiClock,
    ) where

import Data.Time.Orphans ()

import Data.Time.Compat
import Data.Time.Clock.TAI hiding (utcDayLength,utcToTAITime,taiToUTCTime)
import qualified Data.Time.Clock.TAI as T

import Data.Fixed (div')

-- | This type is either 'LeapSecondMap' or 'LeapSecondTable', depending
-- on the version of @time@ (changed in @time-1.7.0@).
#if !(MIN_VERSION_time(1,7,0))
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
#endif

#if !(MIN_VERSION_time(1,8,0))
taiNominalDayStart :: Day -> AbsoluteTime
taiNominalDayStart (ModifiedJulianDay ds) =
   addAbsoluteTime (secondsToDiffTime (ds * 86400)) taiEpoch

-- | TAI clock, if it exists. Note that it is unlikely to be set correctly, without due care and attention.
taiClock :: Maybe (DiffTime,IO AbsoluteTime)
taiClock = Nothing
#endif
