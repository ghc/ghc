module Test.Calendar.ClipDates(clipDates) where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar
import Test.Tasty
import Test.Tasty.HUnit
import Test.Calendar.ClipDatesRef

yearAndDay :: (Integer,Int) -> String
yearAndDay (y,d) = (show y) ++ "-" ++ (show d) ++ " = " ++ (showOrdinalDate (fromOrdinalDate y d))

gregorian :: (Integer,Int,Int) -> String
gregorian (y,m,d) = (show y) ++ "-" ++ (show m) ++ "-" ++ (show d) ++ " = " ++ (showGregorian (fromGregorian y m d))

iSOWeekDay :: (Integer,Int,Int) -> String
iSOWeekDay (y,w,d) = (show y) ++ "-W" ++ (show w) ++ "-" ++ (show d) ++ " = " ++ (showWeekDate (fromWeekDate y w d))

--

tupleUp2 :: [a] -> [b] -> [(a, b)]
tupleUp2 l1 l2 = concatMap (\e -> map (e,) l2) l1

tupleUp3 :: [a] -> [b] -> [c] -> [(a, b, c)]
tupleUp3 l1 l2 l3
  = let ts = tupleUp2 l2 l3
    in concatMap (\e -> map (\(f, g) -> (e, f, g)) ts) l1

--

clipDates :: TestTree
clipDates = testCase "clipDates" $
    let
        yad  = unlines $ map yearAndDay $
            tupleUp2 [1968,1969,1971] [-4,0,1,200,364,365,366,367,700]


        greg = unlines $ map gregorian $
            tupleUp3 [1968,1969,1971] [-20,-1,0,1,2,12,13,17] [-7,-1,0,1,2,27,28,29,30,31,32,40]

        iso  = unlines $ map iSOWeekDay $
            tupleUp3 [1968,1969,2004] [-20,-1,0,1,20,51,52,53,54] [-2,-1,0,1,4,6,7,8,9]

    in assertEqual "" clipDatesRef $
        concat [ "YearAndDay\n", yad, "Gregorian\n", greg, "ISOWeekDay\n", iso ]
