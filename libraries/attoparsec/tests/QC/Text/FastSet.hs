module QC.Text.FastSet where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.Text.FastSet as FastSet

membershipCorrect :: String -> String -> Property
membershipCorrect members others =
    let fs = FastSet.fromList members
        correct c = (c `FastSet.member` fs) == (c `elem` members)
    in property $ all correct (members ++ others)

tests :: [TestTree]
tests = [ testProperty "membership is correct" membershipCorrect ]
