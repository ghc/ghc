{-# OPTIONS -fno-warn-orphans #-}

module Test.Format.ISO8601
    ( testISO8601
    ) where

import Data.Ratio
import Data.Time.Compat
import Data.Time.Format.ISO8601.Compat
import Test.Arbitrary ()
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (reason)
import Test.TestUtil

deriving instance Eq ZonedTime

readShowProperty :: (Eq a, Show a) => Format a -> a -> Property
readShowProperty fmt val =
    case formatShowM fmt val of
        Nothing -> property Discard
        Just str -> let
            found = formatParseM fmt str
            expected = Just val
            in property $
               if expected == found
                   then succeeded
                   else failed {reason = show str ++ ": expected " ++ (show expected) ++ ", found " ++ (show found)}

readBoth :: NameTest t => (FormatExtension -> t) -> [TestTree]
readBoth fmts = [nameTest "extended" $ fmts ExtendedFormat, nameTest "basic" $ fmts BasicFormat]

readShowProperties :: (Eq a, Show a, Arbitrary a) => (FormatExtension -> Format a) -> [TestTree]
readShowProperties fmts = readBoth $ \fe -> readShowProperty $ fmts fe

newtype Durational t =
    MkDurational t

instance Show t => Show (Durational t) where
    show (MkDurational t) = show t

instance Arbitrary (Durational CalendarDiffDays) where
    arbitrary = do
        mm <- choose (-10000, 10000)
        dd <- choose (-40, 40)
        return $ MkDurational $ CalendarDiffDays mm dd

instance Arbitrary (Durational CalendarDiffTime) where
    arbitrary = let
        limit = 40 * 86400
        picofactor = 10 ^ (12 :: Int)
        in do
               mm <- choose (-10000, 10000)
               ss <- choose (negate limit * picofactor, limit * picofactor)
               return $ MkDurational $ CalendarDiffTime mm $ fromRational $ ss % picofactor

testReadShowFormat :: TestTree
testReadShowFormat =
    nameTest
        "read-show format"
        [ nameTest "calendarFormat" $ readShowProperties $ calendarFormat
        , nameTest "yearMonthFormat" $ readShowProperty $ yearMonthFormat
        , nameTest "yearFormat" $ readShowProperty $ yearFormat
        , nameTest "centuryFormat" $ readShowProperty $ centuryFormat
        , nameTest "expandedCalendarFormat" $ readShowProperties $ expandedCalendarFormat 6
        , nameTest "expandedYearMonthFormat" $ readShowProperty $ expandedYearMonthFormat 6
        , nameTest "expandedYearFormat" $ readShowProperty $ expandedYearFormat 6
        , nameTest "expandedCenturyFormat" $ readShowProperty $ expandedCenturyFormat 4
        , nameTest "ordinalDateFormat" $ readShowProperties $ ordinalDateFormat
        , nameTest "expandedOrdinalDateFormat" $ readShowProperties $ expandedOrdinalDateFormat 6
        , nameTest "weekDateFormat" $ readShowProperties $ weekDateFormat
        , nameTest "yearWeekFormat" $ readShowProperties $ yearWeekFormat
        , nameTest "expandedWeekDateFormat" $ readShowProperties $ expandedWeekDateFormat 6
        , nameTest "expandedYearWeekFormat" $ readShowProperties $ expandedYearWeekFormat 6
        , nameTest "timeOfDayFormat" $ readShowProperties $ timeOfDayFormat
        , nameTest "hourMinuteFormat" $ readShowProperties $ hourMinuteFormat
        , nameTest "hourFormat" $ readShowProperty $ hourFormat
        , nameTest "withTimeDesignator" $ readShowProperties $ \fe -> withTimeDesignator $ timeOfDayFormat fe
        , nameTest "withUTCDesignator" $ readShowProperties $ \fe -> withUTCDesignator $ timeOfDayFormat fe
        , nameTest "timeOffsetFormat" $ readShowProperties $ timeOffsetFormat
        , nameTest "timeOfDayAndOffsetFormat" $ readShowProperties $ timeOfDayAndOffsetFormat
        , nameTest "localTimeFormat" $
          readShowProperties $ \fe -> localTimeFormat (calendarFormat fe) (timeOfDayFormat fe)
        , nameTest "zonedTimeFormat" $
          readShowProperties $ \fe -> zonedTimeFormat (calendarFormat fe) (timeOfDayFormat fe) fe
        , nameTest "utcTimeFormat" $ readShowProperties $ \fe -> utcTimeFormat (calendarFormat fe) (timeOfDayFormat fe)
        , nameTest "dayAndTimeFormat" $
          readShowProperties $ \fe -> dayAndTimeFormat (calendarFormat fe) (timeOfDayFormat fe)
        , nameTest "timeAndOffsetFormat" $ readShowProperties $ \fe -> timeAndOffsetFormat (timeOfDayFormat fe) fe
        , nameTest "durationDaysFormat" $ readShowProperty $ durationDaysFormat
        , nameTest "durationTimeFormat" $ readShowProperty $ durationTimeFormat
        , nameTest "alternativeDurationDaysFormat" $
          readBoth $ \fe (MkDurational t) -> readShowProperty (alternativeDurationDaysFormat fe) t
        , nameTest "alternativeDurationTimeFormat" $
          readBoth $ \fe (MkDurational t) -> readShowProperty (alternativeDurationTimeFormat fe) t
        , nameTest "intervalFormat" $
          readShowProperties $ \fe ->
              intervalFormat (localTimeFormat (calendarFormat fe) (timeOfDayFormat fe)) durationTimeFormat
        , nameTest "recurringIntervalFormat" $
          readShowProperties $ \fe ->
              recurringIntervalFormat (localTimeFormat (calendarFormat fe) (timeOfDayFormat fe)) durationTimeFormat
        ]

testShowFormat :: String -> Format t -> String -> t -> TestTree
testShowFormat name fmt str t = nameTest (name ++ ": " ++ str) $ assertEqual "" (Just str) $ formatShowM fmt t

testShowFormats :: TestTree
testShowFormats =
    nameTest
        "show format"
        [ testShowFormat "durationDaysFormat" durationDaysFormat "P0D" $ CalendarDiffDays 0 0
        , testShowFormat "durationDaysFormat" durationDaysFormat "P4Y" $ CalendarDiffDays 48 0
        , testShowFormat "durationDaysFormat" durationDaysFormat "P7M" $ CalendarDiffDays 7 0
        , testShowFormat "durationDaysFormat" durationDaysFormat "P5D" $ CalendarDiffDays 0 5
        , testShowFormat "durationDaysFormat" durationDaysFormat "P2Y3M81D" $ CalendarDiffDays 27 81
        , testShowFormat "durationTimeFormat" durationTimeFormat "P0D" $ CalendarDiffTime 0 0
        , testShowFormat "durationTimeFormat" durationTimeFormat "P4Y" $ CalendarDiffTime 48 0
        , testShowFormat "durationTimeFormat" durationTimeFormat "P7M" $ CalendarDiffTime 7 0
        , testShowFormat "durationTimeFormat" durationTimeFormat "P5D" $ CalendarDiffTime 0 $ 5 * nominalDay
        , testShowFormat "durationTimeFormat" durationTimeFormat "P2Y3M81D" $ CalendarDiffTime 27 $ 81 * nominalDay
        , testShowFormat "durationTimeFormat" durationTimeFormat "PT2H" $ CalendarDiffTime 0 $ 7200
        , testShowFormat "durationTimeFormat" durationTimeFormat "PT3M" $ CalendarDiffTime 0 $ 180
        , testShowFormat "durationTimeFormat" durationTimeFormat "PT12S" $ CalendarDiffTime 0 $ 12
        , testShowFormat "durationTimeFormat" durationTimeFormat "PT1M18.77634S" $ CalendarDiffTime 0 $ 78.77634
        , testShowFormat "durationTimeFormat" durationTimeFormat "PT2H1M18.77634S" $ CalendarDiffTime 0 $ 7278.77634
        , testShowFormat "durationTimeFormat" durationTimeFormat "P5DT2H1M18.77634S" $
          CalendarDiffTime 0 $ 5 * nominalDay + 7278.77634
        , testShowFormat "durationTimeFormat" durationTimeFormat "P7Y10M5DT2H1M18.77634S" $
          CalendarDiffTime 94 $ 5 * nominalDay + 7278.77634
        , testShowFormat "durationTimeFormat" durationTimeFormat "P7Y10MT2H1M18.77634S" $
          CalendarDiffTime 94 $ 7278.77634
        , testShowFormat "durationTimeFormat" durationTimeFormat "P8YT2H1M18.77634S" $ CalendarDiffTime 96 $ 7278.77634
        , testShowFormat "alternativeDurationDaysFormat" (alternativeDurationDaysFormat ExtendedFormat) "P0001-00-00" $
          CalendarDiffDays 12 0
        , testShowFormat "alternativeDurationDaysFormat" (alternativeDurationDaysFormat ExtendedFormat) "P0002-03-29" $
          CalendarDiffDays 27 29
        , testShowFormat "alternativeDurationDaysFormat" (alternativeDurationDaysFormat ExtendedFormat) "P0561-08-29" $
          CalendarDiffDays (561 * 12 + 8) 29
        , testShowFormat
              "alternativeDurationTimeFormat"
              (alternativeDurationTimeFormat ExtendedFormat)
              "P0000-00-01T00:00:00" $
          CalendarDiffTime 0 86400
        , testShowFormat
              "alternativeDurationTimeFormat"
              (alternativeDurationTimeFormat ExtendedFormat)
              "P0007-10-05T02:01:18.77634" $
          CalendarDiffTime 94 $ 5 * nominalDay + 7278.77634
        , testShowFormat
              "alternativeDurationTimeFormat"
              (alternativeDurationTimeFormat ExtendedFormat)
              "P4271-10-05T02:01:18.77634" $
          CalendarDiffTime (12 * 4271 + 10) $ 5 * nominalDay + 7278.77634
        , testShowFormat "centuryFormat" centuryFormat "02" 2
        , testShowFormat "centuryFormat" centuryFormat "21" 21
        , testShowFormat
              "intervalFormat etc."
              (intervalFormat
                   (localTimeFormat (calendarFormat ExtendedFormat) (timeOfDayFormat ExtendedFormat))
                   durationTimeFormat)
              "2015-06-13T21:13:56/P1Y2M7DT5H33M2.34S"
              ( LocalTime (fromGregorian 2015 6 13) (TimeOfDay 21 13 56)
              , CalendarDiffTime 14 $ 7 * nominalDay + 5 * 3600 + 33 * 60 + 2.34)
        , testShowFormat
              "recurringIntervalFormat etc."
              (recurringIntervalFormat
                   (localTimeFormat (calendarFormat ExtendedFormat) (timeOfDayFormat ExtendedFormat))
                   durationTimeFormat)
              "R74/2015-06-13T21:13:56/P1Y2M7DT5H33M2.34S"
              ( 74
              , LocalTime (fromGregorian 2015 6 13) (TimeOfDay 21 13 56)
              , CalendarDiffTime 14 $ 7 * nominalDay + 5 * 3600 + 33 * 60 + 2.34)
        , testShowFormat
              "recurringIntervalFormat etc."
              (recurringIntervalFormat (calendarFormat ExtendedFormat) durationDaysFormat)
              "R74/2015-06-13/P1Y2M7D"
              (74, fromGregorian 2015 6 13, CalendarDiffDays 14 7)
        , testShowFormat "timeOffsetFormat" iso8601Format "-06:30" (minutesToTimeZone (-390))
        , testShowFormat "timeOffsetFormat" iso8601Format "+00:00" (minutesToTimeZone 0)
        , testShowFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+0000" (minutesToTimeZone 0)
        , testShowFormat "timeOffsetFormat" iso8601Format "+00:10" (minutesToTimeZone 10)
        , testShowFormat "timeOffsetFormat" iso8601Format "-00:10" (minutesToTimeZone (-10))
        , testShowFormat "timeOffsetFormat" iso8601Format "+01:35" (minutesToTimeZone 95)
        , testShowFormat "timeOffsetFormat" iso8601Format "-01:35" (minutesToTimeZone (-95))
        , testShowFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+0135" (minutesToTimeZone 95)
        , testShowFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "-0135" (minutesToTimeZone (-95))
        , testShowFormat
              "timeOffsetFormat"
              (timeOffsetFormat BasicFormat)
              "-1100"
              (minutesToTimeZone $ negate $ 11 * 60)
        , testShowFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+1015" (minutesToTimeZone $ 615)
        , testShowFormat
              "zonedTimeFormat"
              iso8601Format
              "2024-07-06T08:45:56.553-06:30"
              (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone (-390)))
        , testShowFormat
              "zonedTimeFormat"
              iso8601Format
              "2024-07-06T08:45:56.553+06:30"
              (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone 390))
        , testShowFormat
              "utcTimeFormat"
              iso8601Format
              "2024-07-06T08:45:56.553Z"
              (UTCTime (fromGregorian 2024 07 06) (timeOfDayToTime $ TimeOfDay 8 45 56.553))
        , testShowFormat
              "utcTimeFormat"
              iso8601Format
              "2028-12-31T23:59:60.9Z"
              (UTCTime (fromGregorian 2028 12 31) (timeOfDayToTime $ TimeOfDay 23 59 60.9))
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1994-W52-7" (fromGregorian 1995 1 1)
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1995-W01-1" (fromGregorian 1995 1 2)
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1996-W52-7" (fromGregorian 1996 12 29)
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1997-W01-2" (fromGregorian 1996 12 31)
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1997-W01-3" (fromGregorian 1997 1 1)
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1974-W32-6" (fromGregorian 1974 8 10)
        , testShowFormat "weekDateFormat" (weekDateFormat BasicFormat) "1974W326" (fromGregorian 1974 8 10)
        , testShowFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1995-W05-6" (fromGregorian 1995 2 4)
        , testShowFormat "weekDateFormat" (weekDateFormat BasicFormat) "1995W056" (fromGregorian 1995 2 4)
        , testShowFormat
              "weekDateFormat"
              (expandedWeekDateFormat 6 ExtendedFormat)
              "+001995-W05-6"
              (fromGregorian 1995 2 4)
        , testShowFormat "weekDateFormat" (expandedWeekDateFormat 6 BasicFormat) "+001995W056" (fromGregorian 1995 2 4)
        , testShowFormat "ordinalDateFormat" (ordinalDateFormat ExtendedFormat) "1846-235" (fromGregorian 1846 8 23)
        , testShowFormat "ordinalDateFormat" (ordinalDateFormat BasicFormat) "1844236" (fromGregorian 1844 8 23)
        , testShowFormat
              "ordinalDateFormat"
              (expandedOrdinalDateFormat 5 ExtendedFormat)
              "+01846-235"
              (fromGregorian 1846 8 23)
        , testShowFormat "hourMinuteFormat" (hourMinuteFormat ExtendedFormat) "13:17.25" (TimeOfDay 13 17 15)
        , testShowFormat "hourMinuteFormat" (hourMinuteFormat ExtendedFormat) "01:12.4" (TimeOfDay 1 12 24)
        , testShowFormat "hourMinuteFormat" (hourMinuteFormat BasicFormat) "1317.25" (TimeOfDay 13 17 15)
        , testShowFormat "hourMinuteFormat" (hourMinuteFormat BasicFormat) "0112.4" (TimeOfDay 1 12 24)
        , testShowFormat "hourFormat" hourFormat "22" (TimeOfDay 22 0 0)
        , testShowFormat "hourFormat" hourFormat "06" (TimeOfDay 6 0 0)
        , testShowFormat "hourFormat" hourFormat "18.9475" (TimeOfDay 18 56 51)
        ]

testISO8601 :: TestTree
testISO8601 = nameTest "ISO8601" [testShowFormats, testReadShowFormat]
