module Test.LocalTime.TimeOfDay
    ( testTimeOfDay
    ) where

import Data.Time.LocalTime.Compat
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (reason)

testTimeOfDay :: TestTree
testTimeOfDay =
    testGroup
        "TimeOfDay"
        [ testProperty "daysAndTimeOfDayToTime . timeToDaysAndTimeOfDay" $ \ndt -> let
              (d, tod) = timeToDaysAndTimeOfDay ndt
              ndt' = daysAndTimeOfDayToTime d tod
              in ndt' == ndt
        , testProperty "timeOfDayToTime . timeToTimeOfDay" $ \dt -> let
              tod = timeToTimeOfDay dt
              dt' = timeOfDayToTime tod
              in dt' == dt
        ]
