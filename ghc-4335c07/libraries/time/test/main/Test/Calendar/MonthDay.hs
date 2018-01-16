module Test.Calendar.MonthDay(testMonthDay) where

import Data.Time.Calendar.MonthDay
import Test.Tasty
import Test.Tasty.HUnit
import Test.Calendar.MonthDayRef

showCompare :: (Eq a,Show a) => a -> String -> a -> String
showCompare a1 b a2 | a1 == a2 = (show a1) ++ " == " ++ b
showCompare a1 b a2 = "DIFF: " ++ (show a1) ++ " -> " ++ b ++ " -> " ++ (show a2)

testMonthDay :: TestTree
testMonthDay = testCase "testMonthDay" $
    assertEqual "" testMonthDayRef $ concat $ map (\isL -> unlines (leap isL : yearDays isL)) [False,True]
    where
        leap isLeap = if isLeap then "Leap:" else "Regular:"
        yearDays isLeap =
            map (\yd -> let
                (m,d)  = dayOfYearToMonthAndDay isLeap yd
                yd'    = monthAndDayToDayOfYear isLeap m d
                mdtext = show m ++ "-" ++ show d
            in showCompare yd mdtext yd')
            [-2..369]
