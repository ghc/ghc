module UnitTests.Distribution.Client.IndexUtils.Timestamp (tests) where

import Distribution.Text
import Data.Time
import Data.Time.Clock.POSIX

import Distribution.Client.IndexUtils.Timestamp

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testProperty "Timestamp1" prop_timestamp1
    , testProperty "Timestamp2" prop_timestamp2
    , testProperty "Timestamp3" prop_timestamp3
    , testProperty "Timestamp4" prop_timestamp4
    , testProperty "Timestamp5" prop_timestamp5
    ]

-- test unixtime format parsing
prop_timestamp1 :: Int -> Bool
prop_timestamp1 t0 = Just t == simpleParse ('@':show t0)
  where
    t = toEnum t0 :: Timestamp

-- test display/simpleParse roundtrip
prop_timestamp2 :: Int -> Bool
prop_timestamp2 t0
  | t /= nullTimestamp  = simpleParse (display t) == Just t
  | otherwise           = display t == ""
  where
    t = toEnum t0 :: Timestamp

-- test display against reference impl
prop_timestamp3 :: Int -> Bool
prop_timestamp3 t0
  | t /= nullTimestamp  = refDisp t == display t
  | otherwise           = display t == ""
  where
    t = toEnum t0 :: Timestamp

    refDisp = maybe undefined (formatTime undefined "%FT%TZ")
              . timestampToUTCTime

-- test utcTimeToTimestamp/timestampToUTCTime roundtrip
prop_timestamp4 :: Int -> Bool
prop_timestamp4 t0
  | t /= nullTimestamp  = (utcTimeToTimestamp =<< timestampToUTCTime t) == Just t
  | otherwise           = timestampToUTCTime t == Nothing
  where
    t = toEnum t0 :: Timestamp

prop_timestamp5 :: Int -> Bool
prop_timestamp5 t0
  | t /= nullTimestamp = timestampToUTCTime t == Just ut
  | otherwise          = timestampToUTCTime t == Nothing
  where
    t = toEnum t0 :: Timestamp
    ut = posixSecondsToUTCTime (fromIntegral t0)
