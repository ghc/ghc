module Main where

import Control.DeepSeq (NFData (rnf), force)
import Data.Hashable (Hashable)

import Data.Time.Calendar.Compat
import Data.Time.Calendar.Month.Compat
import Data.Time.Calendar.Quarter.Compat
import Data.Time.Clock.System.Compat
import Data.Time.Clock.TAI.Compat
import Data.Time.Compat
import Data.Time.Format.Compat
import Test.HUnit.Base               ((@?=))

main :: IO ()
main = do
    utc <- getCurrentTime

    -- UTCTime
    putStrLn $ formatTime defaultTimeLocale rfc822DateFormat (force utc)

    -- ZonedTime
    zt <- getZonedTime
    putStrLn $ formatTime defaultTimeLocale rfc822DateFormat (force zt)

    -- SystemTime
    st <- getSystemTime
    print $ force st

    -- FormatTime DayOfWeek
    formatTime defaultTimeLocale "%u %w %a %A" Monday @?= "1 1 Mon Monday"

    -- TAI taiNominalDayStart
    show (taiNominalDayStart (ModifiedJulianDay 123)) @?= "1859-03-20 00:00:00 TAI"

_ParseTimeInstances :: [()]
_ParseTimeInstances =
    [ () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , () -- test (undefined :: DiffTime)
    , () -- test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    ]
  where
    test :: ParseTime t => t -> ()
    test _ = ()

_FormatTimeInstances :: [()]
_FormatTimeInstances =
    [ () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , () -- test (undefined :: DiffTime)
    , () -- test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Month)
    ]
  where
    test :: FormatTime t => t -> ()
    test _ = ()

_NFDataInstances :: [()]
_NFDataInstances =
    [ test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , test (undefined :: DiffTime)
    , test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , test (undefined :: CalendarDiffTime)
    , test (undefined :: CalendarDiffDays)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Month)
    , test (undefined :: Quarter)
    , test (undefined :: QuarterOfYear)
    ]
  where
    test :: NFData t => t -> ()
    test = rnf 

_EnumInstances :: [()]
_EnumInstances =
    [ test (undefined :: Day)
    , test (undefined :: Month)
    , test (undefined :: Quarter)
    , test (undefined :: QuarterOfYear)
    ]
  where
    test :: Enum t => t -> ()
    test _ = ()

_HashableInstances :: [()]
_HashableInstances =
    [ test (undefined :: TimeLocale)
    , test (undefined :: LocalTime)
    , test (undefined :: TimeOfDay)
    , test (undefined :: TimeZone)
    , test (undefined :: UniversalTime)
    , test (undefined :: UTCTime)
    , test (undefined :: NominalDiffTime)
    , test (undefined :: DiffTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Day)
    , test (undefined :: QuarterOfYear)
    , test (undefined :: Quarter)
    , test (undefined :: Month)
    ]
  where
    test :: Hashable t => t -> ()
    test _ = ()
