module Test.LocalTime.Time(testTime) where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import Test.LocalTime.TimeRef

showCal :: Integer -> String
showCal mjd
  = let date    = ModifiedJulianDay mjd
        (y,m,d) = toGregorian date
        date' = fromGregorian y m d
    in concat [ show mjd ++ "="
                 ++ showGregorian date ++ "="
                 ++ showOrdinalDate date ++ "="
                 ++ showWeekDate date
                 ++ "\n"

               , if date == date'
                   then ""
                   else "=" ++ (show $ toModifiedJulianDay date') ++ "!" ]

testCal :: String
testCal
  = concat
        -- days around 1 BCE/1 CE
      [ concatMap showCal [-678950 .. -678930]

        -- days around 1000 CE
      , concatMap showCal [-313710 .. -313690]

        -- days around MJD zero
      , concatMap showCal [-30..30]
      , showCal 40000
      , showCal 50000

        -- 1900 not a leap year
      , showCal 15078
      , showCal 15079

        -- 1980 is a leap year
      , showCal 44297
      , showCal 44298
      , showCal 44299

        -- 1990 not a leap year
      , showCal 47950
      , showCal 47951

        -- 2000 is a leap year
      , showCal 51602
      , showCal 51603
      , showCal 51604

        -- years 2000 and 2001, plus some slop
      , concatMap showCal [51540..52280] ]

showUTCTime :: UTCTime -> String
showUTCTime (UTCTime d t) =  show (toModifiedJulianDay d) ++ "," ++ show t

myzone :: TimeZone
myzone = hoursToTimeZone (- 8)

leapSec1998Cal :: LocalTime
leapSec1998Cal = LocalTime (fromGregorian 1998 12 31) (TimeOfDay 23 59 60.5)

leapSec1998 :: UTCTime
leapSec1998 = localTimeToUTC utc leapSec1998Cal

testUTC :: String
testUTC
  = let lsMineCal = utcToLocalTime myzone leapSec1998
        lsMine = localTimeToUTC myzone lsMineCal
    in unlines [ showCal 51178
           , show leapSec1998Cal
           , showUTCTime leapSec1998
           , show lsMineCal
           , showUTCTime lsMine ]

neglong :: Rational
neglong = -120

poslong :: Rational
poslong = 120

testUT1 :: String
testUT1
  = unlines [ show $ ut1ToLocalTime 0 $ ModJulianDate 51604.0
            , show $ ut1ToLocalTime 0 $ ModJulianDate 51604.5
            , show $ ut1ToLocalTime neglong $ ModJulianDate 51604.0
            , show $ ut1ToLocalTime neglong $ ModJulianDate 51604.5
            , show $ ut1ToLocalTime poslong $ ModJulianDate 51604.0
            , show $ ut1ToLocalTime poslong $ ModJulianDate 51604.5 ]

testTimeOfDayToDayFraction :: String
testTimeOfDayToDayFraction
  = let f = dayFractionToTimeOfDay . timeOfDayToDayFraction
    in unlines [ show $ f $ TimeOfDay 12 34 56.789
               , show $ f $ TimeOfDay 12 34 56.789123
               , show $ f $ TimeOfDay 12 34 56.789123456
               , show $ f $ TimeOfDay 12 34 56.789123456789 ]

testTime :: TestTree
testTime = testCase "testTime" $
    assertEqual "times" testTimeRef $ unlines [testCal, testUTC, testUT1, testTimeOfDayToDayFraction]
