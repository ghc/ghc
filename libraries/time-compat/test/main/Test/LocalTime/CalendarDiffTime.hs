module Test.LocalTime.CalendarDiffTime
    ( testCalendarDiffTime
    ) where

--import Data.Time.LocalTime
import Test.Arbitrary ()
import Test.Tasty

--import Test.Tasty.QuickCheck hiding (reason)
--testReadShow :: TestTree
--testReadShow = testProperty "read . show" $ \(t :: CalendarDiffTime) -> read (show t) == t
testCalendarDiffTime :: TestTree
testCalendarDiffTime =
    testGroup
        "CalendarDiffTime"
          --testReadShow
        []
