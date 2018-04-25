-- | Types and functions for UTC and UT1
module Data.Time.Clock
(
    module Data.Time.Clock.Internal.UniversalTime,
    module Data.Time.Clock.Internal.DiffTime,
    module Data.Time.Clock.Internal.UTCTime,
    module Data.Time.Clock.Internal.NominalDiffTime,
    module Data.Time.Clock.Internal.UTCDiff,
    getCurrentTime,
    getTime_resolution
) where

import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.SystemTime
import Data.Time.Clock.Internal.UTCDiff
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX
import Data.Time.Format.Parse()
import Data.Time.LocalTime()
