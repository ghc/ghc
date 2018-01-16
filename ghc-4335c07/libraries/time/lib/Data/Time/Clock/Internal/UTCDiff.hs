-- #hide
module Data.Time.Clock.Internal.UTCDiff where

import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX

-- | addUTCTime a b = a + b
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixSecondsToUTCTime (x + (utcTimeToPOSIXSeconds t))

-- | diffUTCTime a b = a - b
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime a b = (utcTimeToPOSIXSeconds a) - (utcTimeToPOSIXSeconds b)
