module Test.Clock.TAI(testTAI) where

import Data.Time
import Data.Time.Clock.TAI
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil


sampleLeapSecondMap :: LeapSecondMap
sampleLeapSecondMap d | d < fromGregorian 1972 1 1 = Nothing
sampleLeapSecondMap d | d < fromGregorian 1972 7 1 = Just 10
sampleLeapSecondMap d | d < fromGregorian 1975 1 1 = Just 11
sampleLeapSecondMap _ = Nothing

testTAI :: TestTree;
testTAI = testGroup "leap second transition" $ let
    dayA = fromGregorian 1972 6 30
    dayB = fromGregorian 1972 7 1

    utcTime1 = UTCTime dayA 86399
    utcTime2 = UTCTime dayA 86400
    utcTime3 = UTCTime dayB 0

    mAbsTime1 = utcToTAITime sampleLeapSecondMap utcTime1
    mAbsTime2 = utcToTAITime sampleLeapSecondMap utcTime2
    mAbsTime3 = utcToTAITime sampleLeapSecondMap utcTime3
    in
    [
        testCase "mapping" $ do
            assertEqual "dayA" (Just 10) $ sampleLeapSecondMap dayA
            assertEqual "dayB" (Just 11) $ sampleLeapSecondMap dayB
        ,
        testCase "day length" $ do
            assertEqual "dayA" (Just 86401) $ utcDayLength sampleLeapSecondMap dayA
            assertEqual "dayB" (Just 86400) $ utcDayLength sampleLeapSecondMap dayB
        ,
        testCase "differences" $ do
            absTime1 <- assertJust mAbsTime1
            absTime2 <- assertJust mAbsTime2
            absTime3 <- assertJust mAbsTime3
            assertEqual "absTime2 - absTime1" 1 $ diffAbsoluteTime absTime2 absTime1
            assertEqual "absTime3 - absTime2" 1 $ diffAbsoluteTime absTime3 absTime2
        ,
        testGroup "round-trip"
        [
            testCase "1" $ do
                absTime <- assertJust mAbsTime1
                utcTime <- assertJust $ taiToUTCTime sampleLeapSecondMap absTime
                assertEqual "round-trip" utcTime1 utcTime
            ,
            testCase "2" $ do
                absTime <- assertJust mAbsTime2
                utcTime <- assertJust $ taiToUTCTime sampleLeapSecondMap absTime
                assertEqual "round-trip" utcTime2 utcTime
            ,
            testCase "3" $ do
                absTime <- assertJust mAbsTime3
                utcTime <- assertJust $ taiToUTCTime sampleLeapSecondMap absTime
                assertEqual "round-trip" utcTime3 utcTime
        ]
    ]
