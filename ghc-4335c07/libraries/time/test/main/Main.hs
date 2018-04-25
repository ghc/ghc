module Main where

import Test.Tasty
import Test.Calendar.AddDays
import Test.Calendar.Calendars
import Test.Calendar.ClipDates
import Test.Calendar.ConvertBack
import Test.Calendar.Easter
import Test.Calendar.LongWeekYears
import Test.Calendar.MonthDay
import Test.Calendar.Valid
import Test.Clock.Conversion
import Test.Clock.Resolution
import Test.Clock.TAI
import Test.Format.Format
import Test.Format.ParseTime
import Test.LocalTime.Time


tests :: TestTree
tests = testGroup "Time" [
    testGroup "Calendar" [
        addDaysTest,
        testCalendars,
        clipDates,
        convertBack,
        longWeekYears,
        testMonthDay,
        testEaster,
        testValid
        ],
    testGroup "Clock" [
        testClockConversion,
        testResolutions,
        testTAI
        ],
    testGroup "Format" [
        testFormat,
        testParseTime
        ],
    testGroup "LocalTime" [
        testTime
        ]
    ]

main :: IO ()
main = defaultMain tests
