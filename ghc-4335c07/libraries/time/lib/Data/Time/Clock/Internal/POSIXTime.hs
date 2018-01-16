module Data.Time.Clock.Internal.POSIXTime where

import Data.Time.Clock.Internal.NominalDiffTime


-- | 86400 nominal seconds in every day
posixDayLength :: NominalDiffTime
posixDayLength = nominalDay

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC
--
-- To convert from a 'Foreign.C.CTime' or 'System.Posix.EpochTime', use 'realToFrac'.
--
type POSIXTime = NominalDiffTime
