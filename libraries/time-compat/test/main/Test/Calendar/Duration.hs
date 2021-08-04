module Test.Calendar.Duration
    ( testDuration
    ) where

import Data.Time.Calendar.Compat
import Data.Time.Calendar.Julian.Compat
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (reason)

testAddDiff :: TestTree
testAddDiff =
    testGroup
        "add diff"
        [ testProperty "add diff GregorianDurationClip" $ \day1 day2 ->
              addGregorianDurationClip (diffGregorianDurationClip day2 day1) day1 == day2
        , testProperty "add diff GregorianDurationRollOver" $ \day1 day2 ->
              addGregorianDurationRollOver (diffGregorianDurationRollOver day2 day1) day1 == day2
        , testProperty "add diff JulianDurationClip" $ \day1 day2 ->
              addJulianDurationClip (diffJulianDurationClip day2 day1) day1 == day2
        , testProperty "add diff JulianDurationRollOver" $ \day1 day2 ->
              addJulianDurationRollOver (diffJulianDurationRollOver day2 day1) day1 == day2
        ]

testClip :: (Integer, Int, Int) -> (Integer, Int, Int) -> (Integer, Integer) -> TestTree
testClip (y1, m1, d1) (y2, m2, d2) (em, ed) = let
    day1 = fromGregorian y1 m1 d1
    day2 = fromGregorian y2 m2 d2
    expected = CalendarDiffDays em ed
    found = diffGregorianDurationClip day1 day2
    in testCase (show day1 ++ " - " ++ show day2) $ assertEqual "" expected found

testDiffs :: TestTree
testDiffs =
    testGroup
        "diffs"
        [ testClip (2017, 04, 07) (2017, 04, 07) (0, 0)
        , testClip (2017, 04, 07) (2017, 04, 01) (0, 6)
        , testClip (2017, 04, 01) (2017, 04, 07) (0, -6)
        , testClip (2017, 04, 07) (2017, 02, 01) (2, 6)
        , testClip (2017, 02, 01) (2017, 04, 07) (-2, -6)
        ]

testDuration :: TestTree
testDuration = testGroup "CalendarDiffDays" [testAddDiff, testDiffs]
