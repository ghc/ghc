#include "HsConfigure.h"
module Data.Time.Clock.Internal.UTCTime
(
    -- * UTC
    -- | UTC is time as measured by a clock, corrected to keep pace with the earth by adding or removing
    -- occasional seconds, known as \"leap seconds\".
    -- These corrections are not predictable and are announced with six month's notice.
    -- No table of these corrections is provided, as any program compiled with it would become
    -- out of date in six months.
    --
    -- If you don't care about leap seconds, use 'UTCTime' and 'NominalDiffTime' for your clock calculations,
    -- and you'll be fine.
    UTCTime(..),
) where

import Data.Typeable
#if LANGUAGE_Rank2Types
import Data.Data
#endif
import Control.DeepSeq
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.DiffTime


-- | This is the simplest representation of UTC.
-- It consists of the day number, and a time offset from midnight.
-- Note that if a day has a leap second added to it, it will have 86401 seconds.
data UTCTime = UTCTime {
    -- | the day
    utctDay :: Day,
    -- | the time from midnight, 0 <= t < 86401s (because of leap-seconds)
    utctDayTime :: DiffTime
}
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    deriving (Data, Typeable)
#endif
#endif
#endif

instance NFData UTCTime where
    rnf (UTCTime d t) = rnf d `seq` rnf t `seq` ()

instance Eq UTCTime where
    (UTCTime da ta) == (UTCTime db tb) = (da == db) && (ta == tb)

instance Ord UTCTime where
    compare (UTCTime da ta) (UTCTime db tb) = case (compare da db) of
        EQ -> compare ta tb
        cmp -> cmp
