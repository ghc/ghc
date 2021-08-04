module Test.Types(CheckInstances) where

import Data.Data
import Data.Time.Compat
import Data.Time.Clock.System.Compat
import Data.Time.Clock.TAI.Compat

class (Typeable t, Data t) => CheckDataInstances t
class (Typeable t, Data t, Eq t) => CheckInstances t

instance CheckInstances UTCTime
instance CheckInstances NominalDiffTime

instance CheckInstances Day
instance CheckInstances DayOfWeek
instance CheckInstances TimeOfDay
instance CheckInstances LocalTime
instance CheckInstances TimeZone
instance CheckDataInstances ZonedTime
instance CheckInstances CalendarDiffDays
instance CheckInstances CalendarDiffTime

instance CheckInstances SystemTime

instance CheckInstances AbsoluteTime
instance CheckInstances UniversalTime
